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
//import scala.xml.dtd.impl.Base
import chisel3.SpecifiedDirection.Flip

case class CacheParameter(
  // Cache properties
  setNumber: Int = 1024,
  associativity: Int = 4,
  blockBit: Int = 512,  
  // Address
  addressWidth: Int = 55, // address to access the whole block instead of one byte
  // Address space ID
  asidWidth: Int = 15,
  
  // the data bank is implemented in register?
  implementedWithRegister: Boolean = false
){
  assert (isPow2(setNumber))
  assert (isPow2(associativity))

  def tagWidth(): Int = {
    addressWidth - log2Ceil(setNumber)
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
 * @param asidWidth the width of the address space ID.
 * @param blockSize the size of the data part
 */ 
class CacheFrontendRequestPacket(
  addressWidth: Int,
  asidWidth: Int,
  blockSize: Int
) extends Bundle {
  val addr = UInt(addressWidth.W) // access address.
  val asid = UInt(asidWidth.W)
  val permission = UInt(2.W)
  val wData = UInt(blockSize.W)
  val wMask = UInt(blockSize.W)

  def this(param: CacheParameter) = this(
    param.addressWidth,
    param.asidWidth,
    param.blockBit,
  )

  override def cloneType(): this.type = new CacheFrontendRequestPacket(addressWidth, asidWidth, blockSize).asInstanceOf[this.type]

  def toInternalRequestPacket(): DataBankFrontendRequestPacket = {
    val res = Wire(new DataBankFrontendRequestPacket(addressWidth, asidWidth, blockSize))
    res.addr := this.addr
    res.flush_v := false.B
    res.refill_v := false.B

    res.asid := this.asid
    res.wData := this.wData
    res.wMask := this.wMask

    res.permission := this.permission

    res
  }
}

/**
 * Request packet from the Page Manager to Cache to flush one item
 * @param addressWidth the width of the address to index the cache
 * @param asidWidth the width of the address space id.
 */ 

class CacheFrontendFlushRequest(
  addressWidth: Int,
  asidWidth: Int
) extends Bundle {
  val addr = UInt(addressWidth.W) // access address.
  val asid = UInt(asidWidth.W)

  def this(param: CacheParameter) = this(
    param.addressWidth,
    param.asidWidth
  )

  override def cloneType(): this.type = new CacheFrontendFlushRequest(addressWidth, asidWidth).asInstanceOf[this.type]

  def toInternalRequestPacket(blockSize: Int): DataBankFrontendRequestPacket = {
    val res = Wire(new DataBankFrontendRequestPacket(addressWidth, asidWidth, blockSize))

    res.addr := this.addr
    res.flush_v := true.B
    res.refill_v := false.B
    res.asid := this.asid
    res.wData := DontCare
    res.wMask := DontCare

    res.permission := DontCare // Flush request doesn't consider the permission.

    res
  }
}

class MergedBackendRequestPacket(param: CacheParameter) extends Bundle{
  val addr = UInt(param.addressWidth.W)
  val asid = UInt(param.asidWidth.W)
  val permission = UInt(2.W)
  val w_v = Bool()
  val flush_v = Bool() // this request is caused by flush
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
  read_request_converted.bits.asid := read_request_i.bits.asid
  read_request_converted.bits.w_v := false.B
  read_request_converted.bits.flush_v := false.B
  read_request_converted.bits.permission := read_request_i.bits.permission
  read_request_converted.valid := read_request_i.valid
  read_request_i.ready := read_request_converted.ready

  val write_request_converted = Wire(Decoupled(new MergedBackendRequestPacket(param)))
  write_request_converted.bits.addr := write_request_i.bits.addr
  write_request_converted.bits.data := write_request_i.bits.data
  write_request_converted.bits.asid := DontCare
  write_request_converted.bits.w_v := true.B
  write_request_converted.bits.flush_v := write_request_i.bits.flush_v
  write_request_converted.bits.permission := 1.U // The permission for writing is always 1.
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
 * @param updateFunction specify how to override the hit entry with given request and hit entry. Return the updated hit entry that will be written to.
 * 
 * @note updateFunction will only be considered when it's a neither flushing nor refilling request, and return the old entry will not trigger a writing.
 */ 
class BaseCache(
  val param: CacheParameter,
  lruCore: () => LRUCore,
  updateFunction: (DataBankFrontendRequestPacket, CacheEntry) => CacheEntry // (req: DataBankFrontendRequestPacket, entryToUpdate: CacheEntry) => result
) extends MultiIOModule{
  val u_bank_frontend = Module(new DataBankManager(param, updateFunction))
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
  val stall_request_vi = IO(Input(Bool()))

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
  refill_request_internal.bits.asid := refill_request_i.bits.asid
  refill_request_internal.bits.wData := refill_request_i.bits.data
  refill_request_internal.bits.wMask := Fill(param.blockBit, true.B)
  refill_request_internal.bits.refill_v := true.B
  refill_request_internal.bits.permission := DontCare

  refill_request_i.ready := refill_request_internal.ready


  val packet_arrive_o = IO(Valid(new Bundle{
    val asid = UInt(param.asidWidth.W)
  }))

  packet_arrive_o.valid := refill_request_i.fire()
  packet_arrive_o.bits.asid := refill_request_i.bits.asid

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

  u_bank_frontend.frontend_request_i.bits := u_frontend_arb.io.out.bits
  u_bank_frontend.frontend_request_i.valid := !stall_request_vi && u_frontend_arb.io.out.valid
  u_frontend_arb.io.out.ready := !stall_request_vi && u_bank_frontend.frontend_request_i.ready


  // frontend_reply_o: Response to the R/W request from the pipeline
  val frontend_reply_o = IO(u_bank_frontend.frontend_reply_o.cloneType)
  frontend_reply_o.bits <> u_bank_frontend.frontend_reply_o.bits
  frontend_reply_o.valid := u_bank_frontend.frontend_reply_o.valid && (if(param.implementedWithRegister) frontend_request_i.fire() else RegNext(frontend_request_i.fire()))
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
    def cacheUpdateFunction(req: DataBankFrontendRequestPacket, oldEntry: CacheEntry): CacheEntry = {
      oldEntry.write(req.wData, req.wMask, false.B, true.B)
    }
    new BaseCache(param, lruCore, cacheUpdateFunction)
  }
}