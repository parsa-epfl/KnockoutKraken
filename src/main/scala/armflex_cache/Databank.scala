package armflex_cache

import chisel3._
import chisel3.util._

import chisel3.stage.ChiselStage

import armflex.util.Diverter
import armflex.util.BRAMConfig
import armflex.util.BRAMPort
import armflex.util.BRAM
import armflex.util.FlushQueue

case class DatabankParameter(
  // Cache properties
  setNumber: Int = 1024,
  associativity: Int = 4,
  blockBit: Int = 512,
  // Address
  addressWidth: Int = 55, // address to access the whole block instead of one byte
  // Address space ID
  asidWidth: Int = 15,
  // thread Number
  threadNumber: Int = 32,

  // the data bank is implemented in register?
  implementedWithRegister: Boolean = false
){
  assert (isPow2(setNumber))
  assert (isPow2(associativity))
  assert (isPow2(threadNumber))

  def tagWidth(): Int = {
    addressWidth - log2Ceil(setNumber)
  }

  def wayWidth(): Int = {
    log2Ceil(associativity)
  }

  def setWidth(): Int = {
    log2Ceil(setNumber)
  }

  def threadIDWidth(): Int = {
    log2Ceil(threadNumber)
  }

}

class DataBankFrontendRequestPacket(
  addressWidth: Int,
  asidWidth: Int,
  blockSize: Int,
  threadIDWidth: Int
) extends Bundle{
  val addr = UInt(addressWidth.W) // access address.
  val asid = UInt(asidWidth.W)
  val tid = UInt(threadIDWidth.W) // only for wakeup
  val permission = UInt(2.W)
  val wData = UInt(blockSize.W)
  val wMask = UInt(blockSize.W)

  val flush_v = Bool() // Flushing signal

  val refill_v = Bool() // Is the request a refilling?
  val refillData = UInt(blockSize.W) // the data of refilling.
  // A refilling must contains a potential pending operation.
  // - If a refill cause a miss, the writing result would be refill result + pending operation.
  // - If a refill cause a hit, the writing result would be hit + pending operation

  // overload constructor to quickly build a frontend port from the cache parameter.
  def this(param: DatabankParameter) = this(
    param.addressWidth,
    param.asidWidth,
    param.blockBit,
    log2Ceil(param.threadNumber)
  )

  override def cloneType(): this.type = new DataBankFrontendRequestPacket(addressWidth, asidWidth, blockSize, threadIDWidth).asInstanceOf[this.type]
}

class DataBankFrontendReplyPacket(
  asidWidth: Int,
  blockSize: Int,
  threadIDWidth: Int
) extends Bundle {
  // data
  val rData = UInt(blockSize.W) // data read from the cache if hit. (Before apply writing)
  val wData = UInt(blockSize.W) // data applied writing request if any. (The data that will be written into cache)
  // thread id / asid
  val tid = UInt(threadIDWidth.W)
  val asid = UInt(asidWidth.W)
  // properties
  val hit = Bool() // does this reply hit the cache?
  val refill = Bool() // is this reply a refill request?
  val flush = Bool() // is this reply a flush request?


  def this(param: DatabankParameter) = this(
    param.asidWidth,
    param.blockBit,
    log2Ceil(param.threadNumber)
  )
}

/**
 * Packet sent to backend for notifying a miss.
 * @param param the Cache parameter
 */ 
class MissRequestPacket(param: DatabankParameter) extends Bundle{
  val addr = UInt(param.addressWidth.W)
  val asid = UInt(param.asidWidth.W)
  val tid = UInt(log2Ceil(param.threadNumber).W) // only for wake up
  val permission = UInt(2.W)

  // write request is also included here.
  val wData = UInt(param.blockBit.W)
  val wMask = UInt(param.blockBit.W)

  val not_sync_with_data_v = Bool() 

  override def cloneType: this.type = new MissRequestPacket(param).asInstanceOf[this.type]
}

/**
 * Packet of the request to the backend for cache writing back when there is a miss.
 */ 
class WriteBackRequestPacket(param: DatabankParameter) extends Bundle{
  val addr = UInt(param.addressWidth.W)
  val data = UInt(param.blockBit.W)

  val flush_v = Bool() // True if this eviction is caused by flush

  override def cloneType: this.type = new WriteBackRequestPacket(param).asInstanceOf[this.type]
}

/**
 * The frontend manager.
 * @param param the Cache parameter
 * @param updateFunction specify how to override the hit entry with given request and hit entry. Return the updated hit entry that will be written to.
 * 
 * @note updateFunction will only be considered when it's a neither flushing nor refilling request, and return the old entry will not trigger a writing.
 */ 
class DataBankManager(
  param: DatabankParameter,
  updateFunction: (DataBankFrontendRequestPacket, CacheEntry) => CacheEntry // (req: DataBankFrontendRequestPacket, entryToUpdate: CacheEntry) => result
) extends MultiIOModule {
  // Ports to frontend
  val frontend_request_i = IO(Flipped(Decoupled(new DataBankFrontendRequestPacket(param))))
  val frontend_reply_o = IO(Valid(new DataBankFrontendReplyPacket(param)))

  // Ports to Bank RAM (Read port)
  val setType = Vec(param.associativity, new CacheEntry(param))

  val bank_ram_request_addr_o = IO(Decoupled(UInt(param.setWidth().W)))
  val bank_ram_reply_data_i = IO(Flipped(Decoupled(setType.cloneType)))

  val bank_ram_write_request_o = IO(Decoupled(new BankWriteRequestPacket(param)))

  // Port to Backend (Read Request)
  val miss_request_o = IO(Decoupled(new MissRequestPacket(param)))
  val writeback_request_o = IO(Decoupled(new WriteBackRequestPacket(param)))

  // Port to LRU
  val lru_addr_o = IO(Output(UInt(param.setWidth().W)))
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

  val s1_frontend_request_r = (if(param.implementedWithRegister) FlushQueue(s1_frontend_request_n, 0, true)
  else FlushQueue(s1_frontend_request_n, 1, true))
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
    x.checkHit(s1_frontend_request_r.bits.addr, s1_frontend_request_r.bits.asid) && x.v
  }) // get all the comparison results
  assert(PopCount(match_bits) === 1.U || PopCount(match_bits) === 0.U, "It's impossible to hit multiple entries.")

  val full_writing_v = s1_frontend_request_r.bits.wMask.andR() || s1_frontend_request_r.bits.refill_v
  when(s1_frontend_request_r.bits.refill_v && s1_frontend_request_r.valid){
    assert(full_writing_v, "Refill must be a full writing. (Full mask!)")
  }
    

  val match_which = OHToUInt(match_bits) // encode from the comparison

  /**
   * This context records the information to writing to the bank.
   */ 
  class writing_context_t extends Bundle{
    val addr = UInt(param.addressWidth.W)
    val asid = UInt(param.asidWidth.W)
    val dataBlock = UInt(param.blockBit.W)
    val flush_v = Bool() // The operation in the second stage (write) is a flush.
    def toEntry(): CacheEntry = {
      val res = Wire(new CacheEntry(param))
      res.v := Mux(flush_v, false.B, true.B)
      res.refill(this.addr, this.asid, this.dataBlock)
      res.d := true.B
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
  s2_bank_writing_n.bits.asid := s1_frontend_request_r.bits.asid
  s2_bank_writing_n.bits.flush_v := s1_frontend_request_r.bits.flush_v
  s2_bank_writing_n.valid := 
    s1_frontend_request_r.valid && // Request is valid
    (s1_frontend_request_r.bits.wMask.orR() || s1_frontend_request_r.bits.flush_v || s1_frontend_request_r.bits.refill_v) && // write, flush, or refill.
    (hit_v || full_writing_v) //! No fire() required here because this register is in a branch instead of the main stream.

  lru_index_o.bits := Mux(hit_v, match_which, lru_which_i)
  // Hit, refill, and full writing will update the LRU bits. But flush won't update it.
  lru_index_o.valid := s1_frontend_request_r.valid && (hit_v || full_writing_v) && !s1_frontend_request_r.bits.flush_v

  val frontend_write_to_bank = Wire(Decoupled(new BankWriteRequestPacket(param))) // normal writing (writing & flushing)
  if(param.setWidth() > 1)
    frontend_write_to_bank.bits.addr := s1_frontend_request_r.bits.addr(param.setWidth()-1, 0)
  else
    frontend_write_to_bank.bits.addr := 0.U

  val full_writing_request = Wire(Decoupled(new BankWriteRequestPacket(param))) // A full writing, which means a complete override to the block, so no original data needed. (refill should follow this path)
  if(param.setWidth() > 1)
    full_writing_request.bits.addr := s1_frontend_request_r.bits.addr(param.setWidth()-1, 0)
  else
    full_writing_request.bits.addr := 0.U

  val flushEntry = Wire(new CacheEntry(param))
  flushEntry.d := false.B
  flushEntry.data := DontCare
  flushEntry.tag := DontCare
  flushEntry.v := false.B

  val updatedEntry = Mux( // this one will be written into cache if the request type is a flushing or a normal write.
    s1_frontend_request_r.bits.flush_v,  // Flush?
    flushEntry,  // Flushed entry
    updateFunction(s1_frontend_request_r.bits, hit_entry) // Apply write operation
  )


  val refillNewEntry = Wire(new CacheEntry(param))
  refillNewEntry.refill(
    s1_frontend_request_r.bits.addr,
    s1_frontend_request_r.bits.asid,
    s1_frontend_request_r.bits.refillData
  )

  val refillEntry = Mux( // This one will be written into cache if the request is a refill
    hit_v,
    updateFunction(s1_frontend_request_r.bits, hit_entry),
    updateFunction(s1_frontend_request_r.bits, refillNewEntry)
  )

  frontend_reply_o.valid := s1_frontend_request_r.fire() // Also return if miss
  frontend_reply_o.bits.rData := hit_entry.read()
  frontend_reply_o.bits.wData := Mux(
    full_writing_v,
    refillEntry.read(),
    updatedEntry.read()
  )
  frontend_reply_o.bits.tid := s1_frontend_request_r.bits.tid
  frontend_reply_o.bits.asid := s1_frontend_request_r.bits.asid
  frontend_reply_o.bits.hit := hit_v || full_writing_v // full writing should be recognized as hit.
  frontend_reply_o.bits.flush := s1_frontend_request_r.bits.flush_v
  frontend_reply_o.bits.refill := s1_frontend_request_r.bits.refill_v

  frontend_write_to_bank.bits.data := updatedEntry
  frontend_write_to_bank.bits.which := match_which
  frontend_write_to_bank.valid :=
    s1_frontend_request_r.valid &&
    hit_v &&
    !s1_frontend_request_r.bits.refill_v &&
    updatedEntry.asUInt =/= hit_entry.asUInt

  s2_bank_writing_n.bits.dataBlock := frontend_reply_o.bits.wData


  full_writing_request.bits.data := refillEntry
  full_writing_request.bits.which := lru_which_i
  full_writing_request.valid := s1_frontend_request_r.valid && full_writing_v

  bank_ram_write_request_o.bits := Mux(
    full_writing_request.valid,
    full_writing_request.bits,
    frontend_write_to_bank.bits
  )

  bank_ram_write_request_o.valid := full_writing_request.valid || frontend_write_to_bank.valid
  full_writing_request.ready := bank_ram_write_request_o.ready
  frontend_write_to_bank.ready := bank_ram_write_request_o.ready

  assert(
    PopCount(full_writing_request.valid ## frontend_write_to_bank.valid) <= 1.U,
    "It's impossible that a request is 'missing' and 'writing' at the same time."
  )

  val s2_miss_request_n = Wire(Decoupled(new MissRequestPacket(param)))
  s2_miss_request_n.bits.addr := s1_frontend_request_r.bits.addr
  s2_miss_request_n.bits.asid := s1_frontend_request_r.bits.asid
  s2_miss_request_n.bits.tid := s1_frontend_request_r.bits.tid
  s2_miss_request_n.bits.not_sync_with_data_v := false.B
  s2_miss_request_n.bits.permission := s1_frontend_request_r.bits.permission
  s2_miss_request_n.bits.wMask := s1_frontend_request_r.bits.wMask
  s2_miss_request_n.bits.wData := s1_frontend_request_r.bits.wData
  s2_miss_request_n.valid := !hit_v && s1_frontend_request_r.fire() && 
    !full_writing_v &&  // full writing is not a miss
    !s1_frontend_request_r.bits.flush_v // flush is not a miss

  val s2_miss_request_r = FlushQueue(s2_miss_request_n, 2, pipe = true)
  miss_request_o <> s2_miss_request_r


  val replaced_entry = bank_ram_reply_data_i.bits(lru_which_i)
  val eviction_wb_req = Wire(Decoupled(new WriteBackRequestPacket(param))) // write back due to the eviction.

  if(param.setWidth() > 1)
    eviction_wb_req.bits.addr := replaced_entry.address(s1_frontend_request_r.bits.addr(param.setWidth-1, 0))
  else
    eviction_wb_req.bits.addr := replaced_entry.tag
    
  eviction_wb_req.bits.data := replaced_entry.read()
  eviction_wb_req.bits.flush_v := false.B
  eviction_wb_req.valid :=
    replaced_entry.d && // evicted entry is dirty
    !hit_v && // miss occurs
    s1_frontend_request_r.bits.refill_v && // a refilling request.
    s1_frontend_request_r.fire()
  

  val flush_wb_req = Wire(Decoupled(new WriteBackRequestPacket(param))) // write back due to the flushing
  flush_wb_req.bits.addr := s1_frontend_request_r.bits.addr
  flush_wb_req.bits.data := hit_entry.read()
  flush_wb_req.bits.flush_v := true.B
  flush_wb_req.valid :=
    hit_entry.d && // flushed is dirty
    hit_v && s1_frontend_request_r.bits.flush_v && // flush confirmed.
    s1_frontend_request_r.fire()

  assert(!(flush_wb_req.valid && eviction_wb_req.valid),
    "It's impossible to write back due to eviction and miss at the same time!"
  )

  val s2_writeback_request_n = Wire(Decoupled(new WriteBackRequestPacket(param)))
  s2_writeback_request_n.bits := Mux(
    eviction_wb_req.valid,
    eviction_wb_req.bits,
    flush_wb_req.bits
  )
  s2_writeback_request_n.valid := eviction_wb_req.valid || flush_wb_req.valid
  eviction_wb_req.ready := s2_writeback_request_n.ready
  flush_wb_req.ready := s2_writeback_request_n.ready

  val s2_writeback_request_r = FlushQueue(s2_writeback_request_n, 2, true)
  writeback_request_o <> s2_writeback_request_r
  
  pipeline_state_ready(1) := true.B && //s2_miss_n.ready &&
  bank_ram_reply_data_i.valid === s1_frontend_request_r.valid &&  // BRAM response arrives.
  s2_miss_request_n.ready && s2_writeback_request_n.ready // Both miss and writeback will be accepted.
  pipeline_state_ready(0) := s1_frontend_request_n.ready && bank_ram_request_addr_o.ready
}

