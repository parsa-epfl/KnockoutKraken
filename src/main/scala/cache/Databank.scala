package armflex.cache

import chisel3._
import chisel3.util._

import chisel3.stage.ChiselStage

import armflex.util.Diverter
import armflex.util.BRAMConfig
import armflex.util.BRAMPort
import armflex.util.BRAM
import armflex.util.FlushQueue
//import firrtl.PrimOps.Mul

import scala.collection.mutable
import treadle.executable.DataType
import scala.xml.dtd.impl.Base

class FrontendRequestPacket(
  addressWidth: Int,
  threadIDWidth: Int,
  blockSize: Int
) extends Bundle{
  val addr = UInt(addressWidth.W) // access address.
  val thread_id = UInt(threadIDWidth.W)
  val groupedFlag = Bool() //! Used by the Adapter and should be propagated.
  val w_v = Bool()// write valid?
  val wData = UInt(blockSize.W)
  val wMask = UInt(blockSize.W)
  val wpermission = Bool() // 0 is read-only and 1 is read-write.

  // overload constructor to quickly build a frontend port from the cache parameter.
  def this(param: CacheParameter) = this(
    param.addressWidth,
    param.threadIDWidth(),
    param.blockBit,
  )

  override def cloneType(): this.type = new FrontendRequestPacket(addressWidth, threadIDWidth, blockSize).asInstanceOf[this.type]
}

class FrontendReplyPacket(param: CacheParameter) extends Bundle{
  val data = UInt(param.blockBit.W)
  val thread_id = UInt(param.threadIDWidth().W)
  val hit = Bool()

  override def cloneType(): this.type = new FrontendReplyPacket(param).asInstanceOf[this.type]
}

/**
 * Packet sent to backend for notifying a miss.
 * @param param the Cache parameter
 */ 
class MissRequestPacket(param: CacheParameter) extends Bundle{
  val addr = UInt(param.addressWidth.W)
  val thread_id = UInt(param.threadIDWidth().W)
  val lru = UInt(param.wayWidth().W)

  val grouped_v = Bool()
  val not_sync_with_data_v = Bool()

  override def cloneType: this.type = new MissRequestPacket(param).asInstanceOf[this.type]
}

/**
 * Packet of the reply from the backend to resolve a miss
 * @param t the chisel type of the entry.
 * @param param the Cache parameter.
 */ 
class MissResolveReplyPacket(t: Entry, param: CacheParameter) extends Bundle{
  val set_number = UInt(param.setWidth().W)
  val thread_id = UInt(param.threadIDWidth().W)
  val lru = UInt(param.wayWidth().W)
  
  val grouped_v = Bool()
  val not_sync_with_data_v = Bool()

  val data = t.cloneType

  override def cloneType: this.type = new MissResolveReplyPacket(t, param).asInstanceOf[this.type]
}

/**
 * Packet of the request to the backend for cache writing back when there is a miss.
 */ 
class WriteBackRequestPacket(param: CacheParameter) extends Bundle{
  val addr = UInt(param.addressWidth.W)
  val data = UInt(param.blockBit.W)

  override def cloneType: this.type = new WriteBackRequestPacket(param).asInstanceOf[this.type]
}

/**
 * The frontend manager.
 * @param t the Chisel type of the entry
 * @param param the Cache parameter
 */ 
class DataBankFrontend(
  t: Entry,
  param: CacheParameter
) extends MultiIOModule{
  // Ports to frontend
  val frontend_request_i = IO(Flipped(Decoupled(new FrontendRequestPacket(param))))
  val frontend_reply_o = IO(Valid(new FrontendReplyPacket(param)))

  // Ports to Bank RAM (Read port)
  val setType = Vec(param.associativity, t.cloneType)

  val bank_ram_request_addr_o = IO(Decoupled(UInt(param.setWidth().W)))
  val bank_ram_reply_data_i = IO(Flipped(Decoupled(setType.cloneType)))

  val bank_ram_write_request_o = IO(Decoupled(new BankWriteRequestPacket(t, param)))

  // Port to Backend (Read Request)
  val miss_request_o = IO(Decoupled(new MissRequestPacket(param)))
  val writeback_request_o = IO(Decoupled(new WriteBackRequestPacket(param)))
  val refill_request_i = IO(Flipped(Decoupled(new MissResolveReplyPacket(t, param))))

  // Port to LRU
  val lru_addr_o = IO(ValidIO(UInt(param.setWidth.W)))
  val lru_index_o = IO(ValidIO(UInt(param.wayWidth().W)))
  val lru_which_i = IO(Input(UInt(param.wayWidth().W)))

  val pipeline_state_ready = Wire(Vec(2, Bool())) // this variables keeps all the back-pressure caused by the pipeline stall

  class DataArrivalPacket extends Bundle{
    val thread_id = UInt(param.threadIDWidth().W)
    val grouped_v = Bool()
  }

  val packet_arrive_o = IO(Output(Valid(new DataArrivalPacket)))

  frontend_request_i.ready := pipeline_state_ready(0)

  // Store request
  val s1_frontend_request_n = Wire(Decoupled(new FrontendRequestPacket(param)))
  s1_frontend_request_n.bits := frontend_request_i.bits
  s1_frontend_request_n.valid := frontend_request_i.fire()
  val s1_frontend_request_r = (if(param.implementedWithRegister) FlushQueue(s1_frontend_request_n, 0, true)("s1_frontend_request_r")
  else FlushQueue(s1_frontend_request_n, 1, true)("s1_frontend_request_r"))
  s1_frontend_request_r.ready := pipeline_state_ready(1)

  // pass to the bram
  bank_ram_request_addr_o.bits := frontend_request_i.bits.addr(param.setWidth-1, 0)
  bank_ram_request_addr_o.valid := frontend_request_i.fire()

  // pass to the LRU
  lru_addr_o.bits := frontend_request_i.bits.addr(param.setWidth-1, 0)
  lru_addr_o.valid := bank_ram_request_addr_o.fire()

  // fetch data from the bram
  bank_ram_reply_data_i.ready := s1_frontend_request_r.valid // If transaction is valid, then we accept the result.
  
  val match_bits = bank_ram_reply_data_i.bits.map({ x=>
    x.checkHit(s1_frontend_request_r.bits.addr, s1_frontend_request_r.bits.thread_id) && x.v
  }) // get all the comparison results
  assert(PopCount(match_bits) === 1.U || PopCount(match_bits) === 0.U, "It's impossible to hit multiple entries.")

  val full_writing_v = s1_frontend_request_r.bits.w_v && s1_frontend_request_r.bits.wMask.andR()

  val match_which = OHToUInt(match_bits) // encode from the comparison

  /**
   * This context records the information to writing to the bank.
   */ 
  class writing_context_t extends Bundle{
    val addr = UInt(param.addressWidth.W)
    val thread_id = UInt(param.threadIDWidth().W)
    val dataBlock = UInt(param.backendPortSize.W)
    val pending_v = Bool()
    def toEntry(): Entry = {
      Mux(pending_v, t.makePending(addr, thread_id), t.refill(addr, thread_id, dataBlock))
    }
  }

  val s2_bank_writing_n = Wire(Valid(new writing_context_t))
  val s2_bank_writing_r = RegNext(s2_bank_writing_n)

  val s2_writing_matched = s2_bank_writing_r.valid && s2_bank_writing_r.bits.addr === s1_frontend_request_r.bits.addr

  val hit_entry = Mux(
    s2_writing_matched,
    s2_bank_writing_r.bits.toEntry(),
    bank_ram_reply_data_i.bits(match_which)
  )

  val hit_v: Bool = Mux(
    s2_writing_matched,
    !s2_bank_writing_r.bits.pending_v,
    PopCount(match_bits) === 1.U && hit_entry.p === false.B
  )

  val entry_is_pending_v: Bool = Mux(
    s2_writing_matched,
    s2_bank_writing_r.bits.pending_v,
    PopCount(match_bits) === 1.U && hit_entry.p
  )

  s2_bank_writing_n.bits.addr := s1_frontend_request_r.bits.addr
  s2_bank_writing_n.bits.thread_id := s1_frontend_request_r.bits.thread_id
  s2_bank_writing_n.bits.pending_v := entry_is_pending_v || (!hit_v)
  s2_bank_writing_n.valid := s1_frontend_request_r.valid

  frontend_reply_o.valid := s1_frontend_request_r.fire() // Also return if miss
  frontend_reply_o.bits.thread_id := s1_frontend_request_r.bits.thread_id
  frontend_reply_o.bits.hit := hit_v || full_writing_v // this term is related to the wake up. A full-writing should be viewed as a hit to the frontend and a miss to the LRU and eviction.
  frontend_reply_o.bits.data := Mux(hit_v, hit_entry.read(), s1_frontend_request_r.bits.wData)

  lru_index_o.bits := match_which
  lru_index_o.valid := hit_v

  val frontend_write_to_bank = Wire(Decoupled(new BankWriteRequestPacket(t, param))) // normal writing
  val updated_entry = Mux(s1_frontend_request_r.bits.w_v, 
    hit_entry.write(
      s1_frontend_request_r.bits.wData, 
      s1_frontend_request_r.bits.wMask
    ),
    hit_entry
  )

  frontend_write_to_bank.bits.data := updated_entry
  frontend_write_to_bank.bits.addr := s1_frontend_request_r.bits.addr(param.setWidth()-1, 0)
  frontend_write_to_bank.bits.which := match_which
  frontend_write_to_bank.valid := s1_frontend_request_r.valid && hit_v && s1_frontend_request_r.bits.w_v

  s2_bank_writing_n.bits.dataBlock := updated_entry.read() //! Problem: writing a pending position

  val frontend_pending_miss_entry = Wire(Decoupled(new BankWriteRequestPacket(t, param))) // make the entry pending.
  frontend_pending_miss_entry.bits.addr := s1_frontend_request_r.bits.addr(param.setWidth()-1, 0)
  frontend_pending_miss_entry.bits.data := t.makePending(s1_frontend_request_r.bits.addr, s1_frontend_request_r.bits.thread_id)
  frontend_pending_miss_entry.bits.which := lru_which_i
  frontend_pending_miss_entry.valid := s1_frontend_request_r.valid && !entry_is_pending_v && (!hit_v && !full_writing_v)

  val full_writing_request = Wire(Decoupled(new BankWriteRequestPacket(t, param))) // A full writing, which means a complete override to the block, so no original data needed.
  full_writing_request.bits.addr := s1_frontend_request_r.bits.addr(param.setWidth()-1, 0)
  full_writing_request.bits.data := t.refill(s1_frontend_request_r.bits.addr, s1_frontend_request_r.bits.thread_id, s1_frontend_request_r.bits.wData)
  full_writing_request.bits.which := lru_which_i
  full_writing_request.valid := s1_frontend_request_r.valid && !entry_is_pending_v && (!hit_v && full_writing_v)
  

  val backend_update_bank_request = Wire(Decoupled(new BankWriteRequestPacket(t, param))) // refilling.
  backend_update_bank_request.bits.addr := refill_request_i.bits.set_number
  backend_update_bank_request.bits.data := refill_request_i.bits.data
  backend_update_bank_request.bits.which := refill_request_i.bits.lru
  backend_update_bank_request.valid := refill_request_i.valid
  refill_request_i.ready := backend_update_bank_request.ready || (refill_request_i.bits.not_sync_with_data_v && refill_request_i.valid)


  val u_bank_writing_arb = Module(new Arbiter(new BankWriteRequestPacket(t, param), 4))
  u_bank_writing_arb.io.in(0) <> full_writing_request
  u_bank_writing_arb.io.in(1) <> frontend_pending_miss_entry
  u_bank_writing_arb.io.in(2) <> frontend_write_to_bank
  u_bank_writing_arb.io.in(3) <> backend_update_bank_request
  bank_ram_write_request_o <> u_bank_writing_arb.io.out

  assert(
    PopCount(full_writing_request.valid ## frontend_pending_miss_entry.valid ## frontend_write_to_bank.valid) <= 1.U,
    "It's impossible that a request is 'missing' and 'writing' at the same time."
  )


  val s2_miss_request_n = Wire(Decoupled(new MissRequestPacket(param)))
  s2_miss_request_n.bits.addr := s1_frontend_request_r.bits.addr
  s2_miss_request_n.bits.thread_id := s1_frontend_request_r.bits.thread_id
  s2_miss_request_n.bits.grouped_v := s1_frontend_request_r.bits.groupedFlag
  s2_miss_request_n.bits.lru := lru_which_i
  s2_miss_request_n.bits.not_sync_with_data_v := entry_is_pending_v
  s2_miss_request_n.valid := !hit_v && s1_frontend_request_r.fire() && 
    !full_writing_v // full writing is not a miss

  val s2_miss_request_r = FlushQueue(s2_miss_request_n, 2, true)("s2_miss_request_r")
  miss_request_o <> s2_miss_request_r


  val replaced_entry = bank_ram_reply_data_i.bits(lru_which_i)

  val s2_writeback_request_n = Wire(Decoupled(new WriteBackRequestPacket(param))) // write back the evicted block
  s2_writeback_request_n.bits.addr := replaced_entry.address(s1_frontend_request_r.bits.addr(param.setWidth-1, 0))
  s2_writeback_request_n.bits.data := replaced_entry.read()
  s2_writeback_request_n.valid := 
    replaced_entry.d && // evicted entry is dirty
    !hit_v && // miss occurs
    s1_frontend_request_r.fire()

  val s2_writeback_request_r = FlushQueue(s2_writeback_request_n, 2, true)("s2_writeback_request_r")
  writeback_request_o <> s2_writeback_request_r

  //packet_arrive_o
  packet_arrive_o.valid := refill_request_i.fire()
  packet_arrive_o.bits.grouped_v := refill_request_i.bits.grouped_v
  packet_arrive_o.bits.thread_id := refill_request_i.bits.thread_id

  
  pipeline_state_ready(1) := true.B && //s2_miss_n.ready &&
  bank_ram_reply_data_i.valid === s1_frontend_request_r.valid &&  // BRAM response arrives.
  s2_miss_request_n.ready && s2_writeback_request_n.ready // Both miss and writeback will be accepted.
  pipeline_state_ready(0) := s1_frontend_request_n.ready && bank_ram_request_addr_o.ready
}

