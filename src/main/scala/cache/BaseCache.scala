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
import chisel3.SpecifiedDirection.Flip

case class CacheParameter(
  // Cache properties
  setNumber: Int = 1024,
  associativity: Int = 4,
  blockBit: Int = 512,  
  // Address
  addressWidth: Int = 55, // address to access the whole block instead of one byte
  // Thread id
  threadNumber: Int = 4,
  // the data bank is implemented in register?
  implementedWithRegister: Boolean = false
){
  assert (isPow2(setNumber))
  assert (isPow2(associativity))

  def tagWidth(): Int = {
    addressWidth - log2Ceil(setNumber)
  }

  def threadIDWidth(): Int = {
    if(threadNumber > 0)
      log2Ceil(threadNumber)
    else
      0
  }

  def wayWidth(): Int = {
    log2Ceil(associativity)
  }

  def setWidth(): Int = {
    log2Ceil(setNumber)
  }
  
}

/**
 * Request packet from the pipeline to the Cache.
 * @param addressWidth the width of the address to index the cache
 * @param threadIDWidth the width of the thread
 * @param blockSize the size of the data part
 */ 
class CacheFrontendRequestPacket(
  addressWidth: Int,
  threadIDWidth: Int,
  blockSize: Int
) extends Bundle {
  val addr = UInt(addressWidth.W) // access address.
  val thread_id = UInt(threadIDWidth.W)
  val w_v = Bool()// write valid?
  val wData = UInt(blockSize.W)
  val wMask = UInt(blockSize.W)

  def this(param: CacheParameter) = this(
    param.addressWidth,
    param.threadIDWidth(),
    param.blockBit,
  )

  override def cloneType(): this.type = new CacheFrontendRequestPacket(addressWidth, threadIDWidth, blockSize).asInstanceOf[this.type]

  def toInternalRequestPacket(): DataBankFrontendRequestPacket = {
    val res = Wire(new DataBankFrontendRequestPacket(addressWidth, threadIDWidth, blockSize))
    res.addr := this.addr
    res.flush_v := false.B
    res.refill_v := false.B

    res.thread_id := this.thread_id
    res.wData := this.wData
    res.wMask := this.wMask
    res.w_v := this.w_v

    res
  }
}

/**
 * Request packet from the Page Manager to Cache to flush one item
 * @param addressWidth the width of the address to index the cache
 * @param threadIDWidth the width of the thread
 */ 

class CacheFrontendFlushRequest(
  addressWidth: Int,
  threadIDWidth: Int
) extends Bundle {
  val addr = UInt(addressWidth.W) // access address.
  val thread_id = UInt(threadIDWidth.W)

  def this(param: CacheParameter) = this(
    param.addressWidth,
    param.threadIDWidth()
  )

  override def cloneType(): this.type = new CacheFrontendFlushRequest(addressWidth, threadIDWidth).asInstanceOf[this.type]

  def toInternalRequestPacket(blockSize: Int): DataBankFrontendRequestPacket = {
    val res = Wire(new DataBankFrontendRequestPacket(addressWidth, threadIDWidth, blockSize))

    res.addr := this.addr
    res.flush_v := true.B
    res.refill_v := false.B
    res.thread_id := this.thread_id
    res.wData := DontCare
    res.wMask := 0.U
    res.w_v := true.B

    res
  }
}



/**
 * Queue that stores the miss requests orderly. These requests are necessary when generating refilling packet.
 * @param param the Cache Parameter.
 */ 
class RefillingQueue(param: CacheParameter) extends MultiIOModule{
  val miss_request_i = IO(Flipped(Decoupled(new MissRequestPacket(param))))
  val backend_reply_i = IO(Flipped(Decoupled(UInt(param.blockBit.W))))
  val refill_o = IO(Decoupled(new MissResolveReplyPacket(param)))
  // there is a queue for the miss request

  val miss_request_q = Queue(miss_request_i, param.threadNumber * 2)
  assert(miss_request_i.ready)

  refill_o.bits.thread_id := miss_request_q.bits.thread_id
  refill_o.bits.not_sync_with_data_v := miss_request_q.bits.not_sync_with_data_v
  refill_o.bits.addr := miss_request_q.bits.addr
  refill_o.bits.data := backend_reply_i.bits

  refill_o.valid := miss_request_q.valid && Mux(miss_request_q.bits.not_sync_with_data_v, true.B, backend_reply_i.valid)
  miss_request_q.ready := refill_o.ready && Mux(miss_request_q.bits.not_sync_with_data_v, true.B, backend_reply_i.valid)
  backend_reply_i.ready := refill_o.ready && !(miss_request_q.bits.not_sync_with_data_v && backend_reply_i.valid)
}


class MergedBackendRequestPacket(param: CacheParameter) extends Bundle{
  val addr = UInt(param.addressWidth.W)
  val thread_id = UInt(param.threadIDWidth().W)
  val w_v = Bool()
  val data = UInt(param.blockBit.W)

  override def cloneType: this.type = new MergedBackendRequestPacket(param).asInstanceOf[this.type]
}

class BackendRequestMerger(param: CacheParameter) extends MultiIOModule{
  val read_request_i = IO(Flipped(Decoupled(new MissRequestPacket(param))))
  val write_request_i = IO(Flipped(Decoupled(new WriteBackRequestPacket(param))))

  val backend_request_o = IO(Decoupled(new MergedBackendRequestPacket(param)))

  val read_request_converted = Wire(Decoupled(new MergedBackendRequestPacket(param)))
  read_request_converted.bits.addr := read_request_i.bits.addr
  read_request_converted.bits.data := DontCare
  read_request_converted.bits.thread_id := read_request_i.bits.thread_id
  read_request_converted.bits.w_v := false.B
  read_request_converted.valid := read_request_i.valid
  read_request_i.ready := read_request_converted.ready

  val write_request_converted = Wire(Decoupled(new MergedBackendRequestPacket(param)))
  write_request_converted.bits.addr := write_request_i.bits.addr
  write_request_converted.bits.data := write_request_i.bits.data
  write_request_converted.bits.thread_id := DontCare
  write_request_converted.bits.w_v := true.B
  write_request_converted.valid := write_request_i.valid
  write_request_i.ready := write_request_converted.ready

  val merger_rr = Module(new RRArbiter(new MergedBackendRequestPacket(param), 2))
  merger_rr.io.in(0) <> read_request_converted
  merger_rr.io.in(1) <> write_request_converted
  backend_request_o <> merger_rr.io.out
}

/**
 *  base class of LRU module.
 *  @param lruCore the LRU updating logic
 *  @param param Cache parameters
 */ 
class LRU[T <: LRUCore](
  param: CacheParameter,
  lruCore: () => T
) extends MultiIOModule{
  val addr_i = IO(Input(UInt(param.setWidth.W)))
  val index_i = IO(Flipped(ValidIO(UInt(param.wayWidth().W))))
  val lru_o = IO(Output(UInt(param.wayWidth().W)))
  // add an extra stage to store the addr. They will be used for the LRU bits writing back.
  val addr_s1_r = if(param.implementedWithRegister) addr_i else RegNext(addr_i)

  // Connected to the LRU Core
  val core = Module(lruCore())

  implicit val bramConfig = new BRAMConfig(
    1,
    core.encodingWidth(),
    param.setNumber
  )

  val bram = Module(new BRAMorRegister(param.implementedWithRegister))

  bram.portA.EN := true.B
  bram.portA.ADDR := addr_i
  bram.portA.WE := false.B
  bram.portA.DI := 0.U


  core.io.encoding_i := bram.portA.DO
  core.io.lru_i := index_i.bits
  lru_o := core.io.lru_o

  // write back
  bram.portB.EN := index_i.valid
  bram.portB.ADDR := addr_s1_r
  bram.portB.WE := index_i.valid
  bram.portB.DI := core.io.encoding_o
}

/**
 * The generator for BRAM-based cache.
 * 
 * The size of all operands is `param.blockBit`, which is typically 512bit
 * for a cache. If you hope to access the cache by 32bit, please partition 
 * the block manually. 
 * 
 * @param param the parameters of the cache
 * @param lruCore an generator of the LRU updating logic. See LRUCore.scala for all options.
* @param writableFunction a function that returns whether to perform writing. The two parameters are original value and new value.
 * 
 */ 
class BaseCache(
  val param: CacheParameter,
  lruCore: () => LRUCore,
  writableFunction: (CacheEntry, CacheEntry) => Bool // (CacheParameter: UInt, CacheParameter: UInt) => Bool
) extends MultiIOModule{
  val u_bank_frontend = Module(new DataBankManager(param, writableFunction))
  val u_bram_adapter = Module(new BRAMPortAdapter(param))

  implicit val cfg = u_bram_adapter.bramCfg
  val bram = Module(new BRAMorRegister(param.implementedWithRegister))
  u_bram_adapter.bram_ports(0) <> bram.portA
  u_bram_adapter.bram_ports(1) <> bram.portB

  u_bram_adapter.frontend_read_reply_data_o <> u_bank_frontend.bank_ram_reply_data_i
  u_bram_adapter.frontend_read_request_i <> u_bank_frontend.bank_ram_request_addr_o
  u_bram_adapter.frontend_write_request_i <> u_bank_frontend.bank_ram_write_request_o

  val u_lruCore = Module(new LRU(param, lruCore))
  u_lruCore.addr_i <> u_bank_frontend.lru_addr_o
  u_lruCore.index_i <> u_bank_frontend.lru_index_o
  u_bank_frontend.lru_which_i := u_lruCore.lru_o

  // Connect to the Refill Queue
  //val u_refill_queue = Module(new RefillingQueue(param))
  //u_refill_queue.miss_request_i <> u_bank_frontend.miss_request_o
  // u_refill_queue.refill_o <> u_bank_frontend.refill_request_i
  //val backendReadReply_i = IO(Flipped(u_refill_queue.backend_reply_i.cloneType)) //! When clone a Flipped Decoupled type, remember to use Flipped to make it correct direction
  //backendReadReply_i <> u_refill_queue.backend_reply_i

  val frontend_request_i = IO(Flipped(Decoupled(new CacheFrontendRequestPacket(param))))
  val flush_request_i = IO(Flipped(Decoupled(new CacheFrontendFlushRequest(param))))
  val refill_request_i = IO(Flipped(Decoupled(new MissResolveReplyPacket(param))))

  //val refill_request = Wire(u_bank_frontend.frontend_request_i.cloneType)
  //refill_request.valid := u_refill_queue.refill_o.valid
  //refill_request.bits.addr := u_refill_queue.refill_o.bits.addr
  //refill_request.bits.flush_v := false.B
  //refill_request.bits.thread_id := u_refill_queue.refill_o.bits.thread_id
  //refill_request.bits.wData := u_refill_queue.refill_o.bits.data
  //refill_request.bits.wMask := Fill(param.blockBit, true.B)
  //refill_request.bits.w_v := true.B
  //refill_request.bits.refill_v := true.B

  //u_refill_queue.refill_o.ready := refill_request.ready

  val refill_request_internal = Wire(u_bank_frontend.frontend_request_i.cloneType)
  refill_request_internal.valid := refill_request_i.valid
  refill_request_internal.bits.addr := refill_request_i.bits.addr
  refill_request_internal.bits.flush_v := false.B
  refill_request_internal.bits.thread_id := refill_request_i.bits.thread_id
  refill_request_internal.bits.wData := refill_request_i.bits.data
  refill_request_internal.bits.wMask := Fill(param.blockBit, true.B)
  refill_request_internal.bits.w_v := true.B
  refill_request_internal.bits.refill_v := true.B

  refill_request_i.ready := refill_request_internal.ready


  val packet_arrive_o = IO(Valid(new Bundle{
    val thread_id = UInt(param.threadIDWidth().W)
  }))

  packet_arrive_o.valid := refill_request_i.fire()
  packet_arrive_o.bits.thread_id := refill_request_i.bits.thread_id

  //val flush_ack_o = IO(Output(Bool()))

  val u_frontend_arb = Module(new Arbiter(u_bank_frontend.frontend_request_i.bits.cloneType(), 3))
  // the order:
  // - 0: Flush
  // - 1: Refill
  // - 2: Request

  // The normal request from the pipeline
  u_frontend_arb.io.in(2).valid := frontend_request_i.valid
  u_frontend_arb.io.in(2).bits := frontend_request_i.bits.toInternalRequestPacket()
  frontend_request_i.ready := !flush_request_i.valid && !refill_request_i.valid
  // The flush request from other place
  u_frontend_arb.io.in(0).valid := flush_request_i.valid
  u_frontend_arb.io.in(0).bits := flush_request_i.bits.toInternalRequestPacket(param.blockBit)
  flush_request_i.ready := u_frontend_arb.io.in(0).ready
  // The refilling request from the backend of the cache
  u_frontend_arb.io.in(1) <> refill_request_internal

  u_frontend_arb.io.out <> u_bank_frontend.frontend_request_i


  // frontend_reply_o: Response to the R/W request from the pipeline
  val frontend_reply_o = IO(u_bank_frontend.frontend_reply_o.cloneType)
  frontend_reply_o.bits <> u_bank_frontend.frontend_reply_o.bits
  frontend_reply_o.valid := u_bank_frontend.frontend_reply_o.valid && RegNext(frontend_request_i.fire())
  // val packet_arrive_o = IO(u_bank_frontend.packet_arrive_o.cloneType)
  // packet_arrive_o <> u_bank_frontend.packet_arrive_o

  // Connect to the backend merger
  val u_backend_merger = Module(new BackendRequestMerger(param))
  u_backend_merger.read_request_i <> u_bank_frontend.miss_request_o
  u_backend_merger.write_request_i <> u_bank_frontend.writeback_request_o
  
  // Backend Queue
  val backend_request_o = IO(u_backend_merger.backend_request_o.cloneType)
  backend_request_o <> u_backend_merger.backend_request_o
}

object BaseCache {
  def generateCache(param: CacheParameter, lruCore: () => LRUCore): BaseCache = {
    new BaseCache(param, lruCore, (a, b) => true.B)
  }
}