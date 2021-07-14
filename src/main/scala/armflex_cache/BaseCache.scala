package armflex_cache

import armflex.PipeCache
import chisel3._
import chisel3.util._
import armflex.util.BRAMConfig

case class CacheParameter(
  pAddrWidth: Int = 36,
  setNumber: Int = 1024,
  associativity: Int = 4,
  blockSizeInBit: Int = 512,
  refillQueueSize: Int = 16
){
  def databankParameter: DatabankParameter = {
    return new DatabankParameter(
      setNumber,
      associativity,
      blockSizeInBit,
      pAddrWidth - log2Ceil(blockSizeInBit / 8),
      0,
      1,
      false
    )
  }

  def blockAddressWidth = databankParameter.addressWidth
}

/**
 * Request packet from the pipeline to the Cache.
 * @param param the parameter of cache
 */ 
class CacheFrontendRequestPacket(
  param: CacheParameter
) extends Bundle {
  val addr = UInt(param.pAddrWidth.W) // access address.
  val wData = UInt(param.blockSizeInBit.W)
  val wMask = UInt(param.blockSizeInBit.W)

  def toInternalRequestPacket: DataBankFrontendRequestPacket = {
    val res = Wire(new DataBankFrontendRequestPacket(param.databankParameter))
    res.addr := this.addr >> log2Ceil(param.blockSizeInBit / 8)
    res.flush_v := false.B

    res.refillData := DontCare
    res.refill_v := false.B

    res.asid := DontCare
    res.wData := this.wData
    res.wMask := this.wMask
    res.tid := DontCare

    res.permission := DontCare


    res
  }
}

/**
 * Request packet from the Page Manager to Cache to flush one item
 * @param param the parameter of cache
 */ 

class CacheFlushRequest(
  param: CacheParameter
) extends Bundle {
  val addr = UInt(param.pAddrWidth.W) // access address.


  def toInternalRequestPacket: DataBankFrontendRequestPacket = {
    val res = Wire(new DataBankFrontendRequestPacket(param.databankParameter))

    res.addr := this.addr >> log2Ceil(param.blockSizeInBit / 8)
    res.flush_v := true.B

    res.refillData := DontCare
    res.refill_v := false.B

    res.asid := DontCare
    res.wData := DontCare
    res.wMask := DontCare
    res.tid := DontCare

    res.permission := DontCare // Flush request doesn't consider the permission.

    res
  }
}

/**
 * Refill request from the DRAM.
 * @param param the parameter of cache
 */
class CacheRefillRequest(
  param: CacheParameter
) extends Bundle {
  val addr = UInt(param.blockAddressWidth.W)
  // old request
  val wData = UInt(param.blockSizeInBit.W)
  val wMask = UInt(param.blockSizeInBit.W)

  // refill data
  val refillData = UInt(param.blockSizeInBit.W)

  def toInternalRequestPacket: DataBankFrontendRequestPacket = {
    val res = Wire(new DataBankFrontendRequestPacket(param.databankParameter))

    res.addr := this.addr
    res.flush_v := false.B

    res.refillData := refillData
    res.refill_v := true.B

    res.asid := DontCare
    res.wData := this.wData
    res.wMask := this.wMask
    res.tid := DontCare

    res.permission := DontCare
    res
  }
}

class MergedBackendRequestPacket(param: DatabankParameter) extends Bundle{
  val addr = UInt(param.addressWidth.W)
  val asid = UInt(param.asidWidth.W)
  val tid = UInt(log2Ceil(param.threadNumber).W)
  val permission = UInt(2.W)
  val w_v = Bool()
  val flush_v = Bool() // this request is caused by flush
  val data = UInt(param.blockBit.W)

  override def cloneType: this.type = new MergedBackendRequestPacket(param).asInstanceOf[this.type]
}

class BackendRequestMerger(param: DatabankParameter) extends MultiIOModule{
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
  read_request_converted.bits.tid := read_request_i.bits.tid
  read_request_converted.valid := read_request_i.valid
  read_request_i.ready := read_request_converted.ready

  val write_request_converted = Wire(Decoupled(new MergedBackendRequestPacket(param)))
  write_request_converted.bits.addr := write_request_i.bits.addr
  write_request_converted.bits.data := write_request_i.bits.data
  write_request_converted.bits.asid := DontCare
  write_request_converted.bits.w_v := true.B
  write_request_converted.bits.flush_v := write_request_i.bits.flush_v
  write_request_converted.bits.permission := 1.U // The permission for writing is always 1.
  write_request_converted.bits.tid := DontCare
  write_request_converted.valid := write_request_i.valid
  write_request_i.ready := write_request_converted.ready

  val merger_rr = Module(new RRArbiter(new MergedBackendRequestPacket(param), 2))
  merger_rr.io.in(0) <> read_request_converted
  merger_rr.io.in(1) <> write_request_converted
  backend_request_o <> merger_rr.io.out
}

/**
 *  Base class of LRU module.
 *  @param lruCore the LRU updating logic
 *  @param param Databank parameters
 */
class LRU[T <: LRUCore](
  param: DatabankParameter,
  lruCore: () => T
) extends MultiIOModule{
  val addr_i = IO(Input(UInt(param.setWidth().W)))
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

class RefillQueue (
  param: CacheParameter,
) extends MultiIOModule {
  val missRequest_i = IO(Flipped(Decoupled(new MissRequestPacket(param.databankParameter)))) // Input of the miss request
  val dramRefillData_i = IO(Flipped(Decoupled(UInt(param.blockSizeInBit.W)))) // the reply from the DRAM about cache miss
  val refillRequest_o = IO(Decoupled(new CacheRefillRequest(param)))

  val missRequest_qo = Queue(missRequest_i, param.refillQueueSize)
  refillRequest_o.valid := missRequest_qo.valid && dramRefillData_i.valid
  refillRequest_o.bits.refillData := dramRefillData_i.bits
  refillRequest_o.bits.addr := missRequest_qo.bits.addr
  refillRequest_o.bits.wData := missRequest_qo.bits.wData
  refillRequest_o.bits.wMask := missRequest_qo.bits.wMask

  missRequest_qo.ready := refillRequest_o.ready && dramRefillData_i.valid
  dramRefillData_i.ready := missRequest_qo.valid && refillRequest_o.ready
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
  val u_bank_frontend = Module(new DataBankManager(param.databankParameter, updateFunction))
  val u_bram_adapter = Module(new BRAMPortAdapter(param.databankParameter))

  implicit val cfg = u_bram_adapter.bramCfg
  val bram = Module(new BRAMorRegister(false))
  u_bram_adapter.bram_ports(0) <> bram.portA
  u_bram_adapter.bram_ports(1) <> bram.portB

  u_bram_adapter.frontend_read_reply_data_o <> u_bank_frontend.bank_ram_reply_data_i
  u_bram_adapter.frontend_read_request_i <> u_bank_frontend.bank_ram_request_addr_o
  u_bram_adapter.frontend_write_request_i <> u_bank_frontend.bank_ram_write_request_o

  val u_lruCore = Module(new LRU(param.databankParameter, lruCore))
  u_lruCore.addr_i <> u_bank_frontend.lru_addr_o
  u_lruCore.index_i <> u_bank_frontend.lru_index_o
  u_bank_frontend.lru_which_i := u_lruCore.lru_o

  val u_refill_queue = Module(new RefillQueue(param))

  val pipeline_request_i = IO(Flipped(Decoupled(new PipeCache.CacheRequest(
    param.pAddrWidth, param.blockSizeInBit
  ))))

  val flush_request_i = IO(Flipped(Decoupled(new CacheFlushRequest(param))))
  val reject_request_vi = IO(Input(Bool())) // When this signal is raised, no new requests will be accepted.

  val refill_data_i = IO(Flipped(Decoupled(UInt(param.blockSizeInBit.W))))
  u_refill_queue.dramRefillData_i <> refill_data_i

  val u_frontend_arb = Module(new Arbiter(u_bank_frontend.frontend_request_i.bits.cloneType(), 3))
  // the order:
  // - 0: Flush
  // - 1: Refill
  // - 2: Pipeline

  // The normal request from the pipeline
  u_frontend_arb.io.in(2).valid := pipeline_request_i.valid
  u_frontend_arb.io.in(2).bits.tid := DontCare
  u_frontend_arb.io.in(2).bits.addr := pipeline_request_i.bits.addr >> log2Ceil(param.blockSizeInBit / 8)
  u_frontend_arb.io.in(2).bits.asid := DontCare
  u_frontend_arb.io.in(2).bits.flush_v := false.B
  u_frontend_arb.io.in(2).bits.permission := DontCare
  u_frontend_arb.io.in(2).bits.refillData := DontCare
  u_frontend_arb.io.in(2).bits.refill_v := false.B
  u_frontend_arb.io.in(2).bits.wData := pipeline_request_i.bits.data
  u_frontend_arb.io.in(2).bits.wMask := Cat(pipeline_request_i.bits.w_en.asBools().map(Fill(8, _)).reverse)
  pipeline_request_i.ready := !flush_request_i.valid && !u_refill_queue.refillRequest_o.valid && !reject_request_vi
  // The flush request from other place
  u_frontend_arb.io.in(0).valid := flush_request_i.valid
  u_frontend_arb.io.in(0).bits := flush_request_i.bits.toInternalRequestPacket
  flush_request_i.ready := u_frontend_arb.io.in(0).ready
  // The refilling request from the backend of the cache
  u_frontend_arb.io.in(1).valid := u_refill_queue.refillRequest_o.valid
  u_frontend_arb.io.in(1).bits := u_refill_queue.refillRequest_o.bits.toInternalRequestPacket
  u_refill_queue.refillRequest_o.ready := !flush_request_i.valid && !reject_request_vi

  u_bank_frontend.frontend_request_i.bits := u_frontend_arb.io.out.bits
  u_bank_frontend.frontend_request_i.valid := !reject_request_vi && u_frontend_arb.io.out.valid
  u_frontend_arb.io.out.ready := !reject_request_vi && u_bank_frontend.frontend_request_i.ready

  // reply
  val pipeline_response_o = IO(Valid(new PipeCache.CacheResponse(param.blockSizeInBit)))
  pipeline_response_o.valid := u_bank_frontend.frontend_reply_o.valid && !u_bank_frontend.frontend_reply_o.bits.flush
  pipeline_response_o.bits.data := Mux(
    u_bank_frontend.frontend_reply_o.bits.refill,
    u_bank_frontend.frontend_reply_o.bits.wData, // the data being written into BRAM will return in case it's a fetch
    u_bank_frontend.frontend_reply_o.bits.rData
  )
  pipeline_response_o.bits.hit := u_bank_frontend.frontend_reply_o.bits.hit
  pipeline_response_o.bits.miss2hit := u_bank_frontend.frontend_reply_o.bits.refill
  pipeline_response_o.bits.miss := !pipeline_response_o.bits.hit

  // Connect to the backend merger
  val u_backend_merger = Module(new BackendRequestMerger(param.databankParameter))
  u_backend_merger.write_request_i <> u_bank_frontend.writeback_request_o
  
  // Backend Queue
  val backend_request_o = IO(u_backend_merger.backend_request_o.cloneType)
  backend_request_o <> u_backend_merger.backend_request_o

  u_refill_queue.missRequest_i.bits := u_bank_frontend.miss_request_o.bits
  u_backend_merger.read_request_i.bits := u_bank_frontend.miss_request_o.bits

  u_bank_frontend.miss_request_o.ready := u_refill_queue.missRequest_i.ready && u_backend_merger.read_request_i.ready
  u_refill_queue.missRequest_i.valid := u_bank_frontend.miss_request_o.valid && u_backend_merger.read_request_i.ready
  u_backend_merger.read_request_i.valid := u_bank_frontend.miss_request_o.valid && u_refill_queue.missRequest_i.ready

}

object BaseCache {
  def generateCache(
    param: CacheParameter,
    lruCore: () => LRUCore
 ): BaseCache = {
    def cacheUpdateFunction(req: DataBankFrontendRequestPacket, oldEntry: CacheEntry): CacheEntry = {
      oldEntry.write(req.wData, req.wMask, false.B, true.B)
    }
    return new BaseCache(
      param,
      lruCore,
      cacheUpdateFunction
    )
  }
}