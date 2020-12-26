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

class DataBankFrontendRequestPacket(
  addressWidth: Int,
  threadIDWidth: Int,
  blockSize: Int
) extends Bundle{
  val addr = UInt(addressWidth.W) // access address.
  val thread_id = UInt(threadIDWidth.W)
  val w_v = Bool()// write valid?
  val wData = UInt(blockSize.W)
  val wMask = UInt(blockSize.W)

  val flush_v = Bool() // Flushing signal
  val refill_v = Bool() // Is the request a refilling?

  // overload constructor to quickly build a frontend port from the cache parameter.
  def this(param: CacheParameter) = this(
    param.addressWidth,
    param.threadIDWidth(),
    param.blockBit,
  )

  override def cloneType(): this.type = new DataBankFrontendRequestPacket(addressWidth, threadIDWidth, blockSize).asInstanceOf[this.type]
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

  val not_sync_with_data_v = Bool()

  override def cloneType: this.type = new MissRequestPacket(param).asInstanceOf[this.type]
}

/**
 * Packet of the reply from the backend to resolve a miss
 * @param param the Cache parameter.
 */ 
class MissResolveReplyPacket(param: CacheParameter) extends Bundle{
  val addr = UInt(param.addressWidth.W)
  val thread_id = UInt(param.threadIDWidth().W)
  
  val not_sync_with_data_v = Bool()

  val data = UInt(param.blockBit.W)

  override def cloneType: this.type = new MissResolveReplyPacket(param).asInstanceOf[this.type]
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
 * @param param the Cache parameter
 * @param writableFunction a function that returns whether to perform writing. The two parameters are original value and new value.
 */ 
class DataBankManager(
  param: CacheParameter,
  writableFunction: (CacheEntry, CacheEntry) => Bool // (CacheParameter: UInt, CacheParameter: UInt) => Bool
) extends MultiIOModule {
  // Ports to frontend
  val frontend_request_i = IO(Flipped(Decoupled(new DataBankFrontendRequestPacket(param))))
  val frontend_reply_o = IO(Valid(new FrontendReplyPacket(param)))

  // Ports to Bank RAM (Read port)
  val setType = Vec(param.associativity, new CacheEntry(param))

  val bank_ram_request_addr_o = IO(Decoupled(UInt(param.setWidth().W)))
  val bank_ram_reply_data_i = IO(Flipped(Decoupled(setType.cloneType)))

  val bank_ram_write_request_o = IO(Decoupled(new BankWriteRequestPacket(param)))

  // Port to Backend (Read Request)
  val miss_request_o = IO(Decoupled(new MissRequestPacket(param)))
  val writeback_request_o = IO(Decoupled(new WriteBackRequestPacket(param)))

  // Port to LRU
  val lru_addr_o = IO(Output(UInt(param.setWidth.W)))
  val lru_index_o = IO(ValidIO(UInt(param.wayWidth().W)))
  val lru_which_i = IO(Input(UInt(param.wayWidth().W)))

  val pipeline_state_ready = Wire(Vec(2, Bool())) // this variables keeps all the back-pressure caused by the pipeline stall

  frontend_request_i.ready := pipeline_state_ready(0)

  // Store request
  val s1_frontend_request_n = Wire(Decoupled(new DataBankFrontendRequestPacket(param)))
  s1_frontend_request_n.bits := frontend_request_i.bits

  if(param.implementedWithRegister) 
    s1_frontend_request_n.valid := frontend_request_i.valid // why valid instead of fire()? There will be a combinational loop of frontend_request.
  else 
    s1_frontend_request_n.valid := frontend_request_i.fire()

  val s1_frontend_request_r = (if(param.implementedWithRegister) FlushQueue(s1_frontend_request_n, 0, true)("s1_frontend_request_r")
  else FlushQueue(s1_frontend_request_n, 1, true)("s1_frontend_request_r"))
  s1_frontend_request_r.ready := pipeline_state_ready(1)

  // pass to the bram
  if(param.setWidth() > 1)
    bank_ram_request_addr_o.bits := frontend_request_i.bits.addr(param.setWidth-1, 0)
  else
    bank_ram_request_addr_o.bits := 0.U
  
  if(param.implementedWithRegister)
    bank_ram_request_addr_o.valid := frontend_request_i.valid
  else
    bank_ram_request_addr_o.valid := frontend_request_i.fire() //! Always remember to check the handshake signal when eliminating one level pipeline stage!!!

  // pass to the LRU
  if(param.setWidth() > 1)
    lru_addr_o := frontend_request_i.bits.addr(param.setWidth-1, 0)
  else
    lru_addr_o := 0.U 

  // fetch data from the bram
  bank_ram_reply_data_i.ready := s1_frontend_request_r.valid // If transaction is valid, then we accept the result.
  
  val match_bits = bank_ram_reply_data_i.bits.map({ x=>
    x.checkHit(s1_frontend_request_r.bits.addr, s1_frontend_request_r.bits.thread_id) && x.v
  }) // get all the comparison results
  assert(PopCount(match_bits) === 1.U || PopCount(match_bits) === 0.U, "It's impossible to hit multiple entries.")

  val full_writing_v = s1_frontend_request_r.bits.w_v && s1_frontend_request_r.bits.wMask.andR()
  when(s1_frontend_request_r.bits.refill_v && s1_frontend_request_r.valid){
    assert(full_writing_v, "Refill must be a full writing. (Full mask!)")
  }
    

  val match_which = OHToUInt(match_bits) // encode from the comparison

  /**
   * This context records the information to writing to the bank.
   */ 
  class writing_context_t extends Bundle{
    val addr = UInt(param.addressWidth.W)
    val thread_id = UInt(param.threadIDWidth().W)
    val dataBlock = UInt(param.blockBit.W)
    val flush_v = Bool()
    def toEntry(): CacheEntry = {
      val res = Wire(new CacheEntry(param))
      res.v := Mux(flush_v, false.B, true.B)
      res.refill(this.addr, this.thread_id, this.dataBlock)
      res
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
    !s2_bank_writing_r.bits.flush_v,
    PopCount(match_bits) === 1.U
  )

  s2_bank_writing_n.bits.addr := s1_frontend_request_r.bits.addr
  s2_bank_writing_n.bits.thread_id := s1_frontend_request_r.bits.thread_id
  s2_bank_writing_n.bits.flush_v := s1_frontend_request_r.bits.flush_v
  s2_bank_writing_n.valid := s1_frontend_request_r.valid && s1_frontend_request_r.bits.w_v && (hit_v || full_writing_v) //! No fire() required here because this register is in a branch instead of the main stream.

  frontend_reply_o.valid := s1_frontend_request_r.fire() // Also return if miss
  frontend_reply_o.bits.thread_id := s1_frontend_request_r.bits.thread_id
  frontend_reply_o.bits.hit := hit_v || full_writing_v // this term is related to the wake up. A full-writing should be viewed as a hit to the frontend and a miss to the LRU and eviction.
  frontend_reply_o.bits.data := hit_entry.read()

  lru_index_o.bits := Mux(hit_v, match_which, lru_which_i)
  // Hit, refill, and full writing will update the LRU bits. But flush won't update it.
  lru_index_o.valid := s1_frontend_request_r.valid && (hit_v || full_writing_v) && !s1_frontend_request_r.bits.flush_v

  val frontend_write_to_bank = Wire(Decoupled(new BankWriteRequestPacket(param))) // normal writing

  val updated_entry = Mux(s1_frontend_request_r.bits.w_v,
    // how to determine the updated entry?
    // - if normal write, update
    // - if refill, keep original value if hit
    // - otherwise, keep original value
    hit_entry.write(
      s1_frontend_request_r.bits.wData, 
      s1_frontend_request_r.bits.wMask,
      s1_frontend_request_r.bits.refill_v
    ),
    hit_entry
  )

  frontend_write_to_bank.bits.data := updated_entry
  frontend_write_to_bank.bits.data.v := Mux(s1_frontend_request_r.bits.flush_v, false.B, true.B)
  if(param.setWidth() > 1)
    frontend_write_to_bank.bits.addr := s1_frontend_request_r.bits.addr(param.setWidth()-1, 0)
  else
    frontend_write_to_bank.bits.addr := 0.U
  frontend_write_to_bank.bits.which := match_which
  frontend_write_to_bank.valid := s1_frontend_request_r.valid && hit_v && !s1_frontend_request_r.bits.refill_v && s1_frontend_request_r.bits.w_v && writableFunction(hit_entry,updated_entry) // When flushing, write to bank.

  s2_bank_writing_n.bits.dataBlock := updated_entry.read()

  val full_writing_request = Wire(Decoupled(new BankWriteRequestPacket(param))) // A full writing, which means a complete override to the block, so no original data needed. (refill should follow this path)
  if(param.setWidth() > 1)
    full_writing_request.bits.addr := s1_frontend_request_r.bits.addr(param.setWidth()-1, 0)
  else
    full_writing_request.bits.addr := 0.U
  full_writing_request.bits.data.refill(s1_frontend_request_r.bits.addr, s1_frontend_request_r.bits.thread_id, s1_frontend_request_r.bits.wData)
  full_writing_request.bits.which := lru_which_i
  full_writing_request.valid := s1_frontend_request_r.valid && (!hit_v && full_writing_v)



  val u_bank_writing_arb = Module(new Arbiter(new BankWriteRequestPacket(param), 2))
  u_bank_writing_arb.io.in(0) <> full_writing_request
  u_bank_writing_arb.io.in(1) <> frontend_write_to_bank
  bank_ram_write_request_o <> u_bank_writing_arb.io.out

  assert(
    PopCount(full_writing_request.valid ## frontend_write_to_bank.valid) <= 1.U,
    "It's impossible that a request is 'missing' and 'writing' at the same time."
  )


  val s2_miss_request_n = Wire(Decoupled(new MissRequestPacket(param)))
  s2_miss_request_n.bits.addr := s1_frontend_request_r.bits.addr
  s2_miss_request_n.bits.thread_id := s1_frontend_request_r.bits.thread_id
  s2_miss_request_n.bits.not_sync_with_data_v := false.B
  s2_miss_request_n.valid := !hit_v && s1_frontend_request_r.fire() && 
    !full_writing_v &&  // full writing is not a miss
    !s1_frontend_request_r.bits.flush_v // flush is not a miss

  val s2_miss_request_r = FlushQueue(s2_miss_request_n, 2, true)("s2_miss_request_r")
  miss_request_o <> s2_miss_request_r


  val replaced_entry = bank_ram_reply_data_i.bits(lru_which_i)
  val eviction_wb_req = Wire(Decoupled(new WriteBackRequestPacket(param))) // write back due to the eviction.
  if(param.setWidth() > 1)
    eviction_wb_req.bits.addr := replaced_entry.address(s1_frontend_request_r.bits.addr(param.setWidth-1, 0))
  else
    eviction_wb_req.bits.addr := replaced_entry.tag
  eviction_wb_req.bits.data := replaced_entry.read()
  eviction_wb_req.valid := 
    replaced_entry.d && // evicted entry is dirty
    !hit_v && // miss occurs
    s1_frontend_request_r.fire()

  val flush_wb_req = Wire(Decoupled(new WriteBackRequestPacket(param))) // write back due to the flushing
  flush_wb_req.bits.addr := s1_frontend_request_r.bits.addr
  flush_wb_req.bits.data := hit_entry.read()
  flush_wb_req.valid :=
    hit_entry.d && // flushed is dirty
    hit_v && s1_frontend_request_r.bits.flush_v && // flush confirmed.
    s1_frontend_request_r.fire()

  assert(!(flush_wb_req.valid && eviction_wb_req.valid),
    "It's impossible to write back due to eviction and miss at the same time!"
  )

  val u_wb_req_arb = Module(new Arbiter(new WriteBackRequestPacket(param), 2))
  u_wb_req_arb.io.in(0) <> eviction_wb_req
  u_wb_req_arb.io.in(1) <> flush_wb_req

  val s2_writeback_request_n = u_wb_req_arb.io.out
  val s2_writeback_request_r = FlushQueue(s2_writeback_request_n, 2, true)("s2_writeback_request_r")
  writeback_request_o <> s2_writeback_request_r
  
  pipeline_state_ready(1) := true.B && //s2_miss_n.ready &&
  bank_ram_reply_data_i.valid === s1_frontend_request_r.valid &&  // BRAM response arrives.
  s2_miss_request_n.ready && s2_writeback_request_n.ready // Both miss and writeback will be accepted.
  pipeline_state_ready(0) := s1_frontend_request_n.ready && bank_ram_request_addr_o.ready
}

