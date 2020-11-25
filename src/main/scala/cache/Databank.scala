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
  val threadID = UInt(threadIDWidth.W)
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
  val threadID = UInt(param.threadIDWidth().W)
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
  val frontendRequest_i = IO(Flipped(Decoupled(new FrontendRequestPacket(param))))
  val frontendReply_o = IO(Valid(new FrontendReplyPacket(param)))

  // Ports to Bank RAM (Read port)
  val set_t = Vec(param.associativity, t.cloneType)

  val bankRamRequestAddr_o = IO(Decoupled(UInt(param.setWidth().W)))
  val bankRamReplyData_i = IO(Flipped(Decoupled(set_t.cloneType)))

  val bankRamWriteRequest_o = IO(Decoupled(new BankWriteRequestPacket(t, param)))

  // Port to Backend (Read Request)
  val miss_request_o = IO(Decoupled(new MissRequestPacket(param)))
  val writeback_request_o = IO(Decoupled(new WriteBackRequestPacket(param)))
  val refill_request_i = IO(Flipped(Decoupled(new MissResolveReplyPacket(t, param))))

  // Port to LRU
  val lruPort = IO(new Bundle{
    val addr_o = Output(UInt(param.addressWidth.W))
    val addr_vo = Output(Bool())

    val index_o = Output(UInt(param.wayWidth().W))
    val index_vo = Output(Bool())

    val lru_i = Input(UInt(param.wayWidth().W))
  })

  val pipelineStateReady = Wire(Vec(2, Bool()))

  class DataArrivalPacket extends Bundle{
    val thread_id = UInt(param.threadIDWidth().W)
    val grouped_v = Bool()
  }

  val packetArrive_o = IO(Output(Valid(new DataArrivalPacket)))

  frontendRequest_i.ready := pipelineStateReady(0)

  // Store request
  val s1_frontendRequest_n = Wire(Decoupled(new FrontendRequestPacket(param)))
  s1_frontendRequest_n.bits := frontendRequest_i.bits
  s1_frontendRequest_n.valid := frontendRequest_i.fire()
  val s1_frontendRequest_r = (if(param.implementedWithRegister) FlushQueue(s1_frontendRequest_n, 0, true)("s1_frontendRequest_r")
  else FlushQueue(s1_frontendRequest_n, 1, true)("s1_frontendRequest_r"))
  s1_frontendRequest_r.ready := pipelineStateReady(1)

  // pass to the bram
  bankRamRequestAddr_o.bits := frontendRequest_i.bits.addr(param.setWidth-1, 0)
  bankRamRequestAddr_o.valid := frontendRequest_i.fire()

  // pass to the LRU
  lruPort.addr_o := frontendRequest_i.bits.addr(param.setWidth-1, 0)
  lruPort.addr_vo := bankRamRequestAddr_o.fire()

  // fetch data from the bram
  bankRamReplyData_i.ready := s1_frontendRequest_r.valid // If transaction is valid, then we accept the result.
  
  val matchBits = bankRamReplyData_i.bits.map({ x=>
    x.checkHit(s1_frontendRequest_r.bits.addr, s1_frontendRequest_r.bits.threadID) && x.v
  }) // get all the comparison results
  assert(PopCount(matchBits) === 1.U || PopCount(matchBits) === 0.U, "It's impossible to hit multiple entries.")
  val mapWhich = OHToUInt(matchBits) // encode from the comparison

  /**
   * This context records the information to writing to the bank.
   */ 
  class writing_context_t extends Bundle{
    val addr = UInt(param.addressWidth.W)
    val threadID = UInt(param.threadIDWidth().W)
    val dataBlock = UInt(param.backendPortSize.W)
    val pending_v = Bool()
    def toEntry(): Entry = {
      Mux(pending_v, t.makePending(addr, threadID), t.refill(addr, threadID, dataBlock))
    }
  }

  val s2_bank_writing_n = Wire(Valid(new writing_context_t))
  val s2_bank_writing_r = RegNext(s2_bank_writing_n)

  val s2_writing_matched = s2_bank_writing_r.valid && s2_bank_writing_r.bits.addr === s1_frontendRequest_r.bits.addr

  val hitEntry = Mux(
    s2_writing_matched,
    s2_bank_writing_r.bits.toEntry(),
    bankRamReplyData_i.bits(mapWhich)
  )

  val isHit: Bool = Mux(
    s2_writing_matched,
    !s2_bank_writing_r.bits.pending_v,
    PopCount(matchBits) === 1.U && hitEntry.p === false.B
  )

  val isPending: Bool = Mux(
    s2_writing_matched,
    s2_bank_writing_r.bits.pending_v,
    PopCount(matchBits) === 1.U && hitEntry.p
  )

  s2_bank_writing_n.bits.addr := s1_frontendRequest_r.bits.addr
  s2_bank_writing_n.bits.threadID := s1_frontendRequest_r.bits.threadID
  s2_bank_writing_n.bits.pending_v := isPending || (!isHit)
  s2_bank_writing_n.valid := s1_frontendRequest_r.valid

  frontendReply_o.valid := s1_frontendRequest_r.fire()
  frontendReply_o.bits.threadID := s1_frontendRequest_r.bits.threadID
  frontendReply_o.bits.hit := isHit // if not head, also return.
  frontendReply_o.bits.data := hitEntry.read()

  lruPort.index_o := mapWhich
  lruPort.index_vo := isHit

  val frontendWriteToBank = Wire(Decoupled(new BankWriteRequestPacket(t, param)))
  val updatedEntry = Mux(s1_frontendRequest_r.bits.w_v, 
    hitEntry.write(
      s1_frontendRequest_r.bits.wData, 
      s1_frontendRequest_r.bits.wMask
    ),
    hitEntry
  )

  frontendWriteToBank.bits.data := updatedEntry
  frontendWriteToBank.bits.addr := s1_frontendRequest_r.bits.addr(param.setWidth()-1, 0)
  frontendWriteToBank.bits.which := mapWhich
  frontendWriteToBank.valid := s1_frontendRequest_r.valid && isHit && s1_frontendRequest_r.bits.w_v

  s2_bank_writing_n.bits.dataBlock := updatedEntry.read() //! Problem: writing a pending position

  val frontendPendingMissEntry = Wire(Decoupled(new BankWriteRequestPacket(t, param)))
  frontendPendingMissEntry.bits.addr := s1_frontendRequest_r.bits.addr(param.setWidth()-1, 0)
  frontendPendingMissEntry.bits.data := t.makePending(s1_frontendRequest_r.bits.addr, s1_frontendRequest_r.bits.threadID)
  frontendPendingMissEntry.bits.which := lruPort.lru_i
  frontendPendingMissEntry.valid := s1_frontendRequest_r.valid && !isPending && !isHit
  assert(!(frontendPendingMissEntry.valid && frontendWriteToBank.valid), "It's impossible that a request is 'missing' and 'writing' at the same time.")

  val backendUpdateBankRequest = Wire(Decoupled(new BankWriteRequestPacket(t, param)))
  backendUpdateBankRequest.bits.addr := refill_request_i.bits.set_number
  backendUpdateBankRequest.bits.data := refill_request_i.bits.data
  backendUpdateBankRequest.bits.which := refill_request_i.bits.lru
  backendUpdateBankRequest.valid := refill_request_i.valid
  refill_request_i.ready := backendUpdateBankRequest.ready || (refill_request_i.bits.not_sync_with_data_v && refill_request_i.valid)


  val u_writeToBankArb = Module(new Arbiter(new BankWriteRequestPacket(t, param), 3))
  u_writeToBankArb.io.in(0) <> frontendPendingMissEntry
  u_writeToBankArb.io.in(1) <> frontendWriteToBank
  u_writeToBankArb.io.in(2) <> backendUpdateBankRequest
  bankRamWriteRequest_o <> u_writeToBankArb.io.out


  val s2_miss_request_n = Wire(Decoupled(new MissRequestPacket(param)))
  s2_miss_request_n.bits.addr := s1_frontendRequest_r.bits.addr
  s2_miss_request_n.bits.thread_id := s1_frontendRequest_r.bits.threadID
  s2_miss_request_n.bits.grouped_v := s1_frontendRequest_r.bits.groupedFlag
  s2_miss_request_n.bits.lru := lruPort.lru_i
  s2_miss_request_n.bits.not_sync_with_data_v := isPending
  s2_miss_request_n.valid := !isHit && s1_frontendRequest_r.fire()

  val s2_miss_request_r = FlushQueue(s2_miss_request_n, 2, true)("s2_miss_request_r")
  miss_request_o <> s2_miss_request_r


  val replaced_entry = bankRamReplyData_i.bits(lruPort.lru_i)

  val s2_writeback_request_n = Wire(Decoupled(new WriteBackRequestPacket(param)))
  s2_writeback_request_n.bits.addr := replaced_entry.address(s1_frontendRequest_r.bits.addr(param.setWidth-1, 0))
  s2_writeback_request_n.bits.data := replaced_entry.read()
  s2_writeback_request_n.valid := 
    replaced_entry.d && // evicted entry is dirty
    !isHit && // miss occurs
    s1_frontendRequest_r.fire()

  val s2_writeback_request_r = FlushQueue(s2_writeback_request_n, 2, true)("s2_writeback_request_r")
  writeback_request_o <> s2_writeback_request_r

  //packetArrive_o
  packetArrive_o.valid := refill_request_i.fire()
  packetArrive_o.bits.grouped_v := refill_request_i.bits.grouped_v
  packetArrive_o.bits.thread_id := refill_request_i.bits.thread_id

  
  pipelineStateReady(1) := true.B && //s2_miss_n.ready &&
  bankRamReplyData_i.valid === s1_frontendRequest_r.valid &&  // BRAM response arrives.
  s2_miss_request_n.ready && s2_writeback_request_n.ready // Both miss and writeback will be accepted.
  pipelineStateReady(0) := s1_frontendRequest_n.ready && bankRamRequestAddr_o.ready
}

