package armflex_cache

import armflex.PipeCache
import chisel3._
import chisel3.util._
import armflex.util.BRAMParams

case class CacheParams(
  pAddrWidth: Int = 36,
  setNumber: Int = 1024,
  associativity: Int = 4,
  blockSize: Int = 512,
  refillQueueSize: Int = 16
) {
  val databankParameter: DatabankParams = new DatabankParams(
    setNumber,
    associativity,
    blockSize,
    pAddrWidth - log2Ceil(blockSize / 8),
    0,
    1,
    false
    )
}

/**
 * Request packet from the pipeline to the Cache.
 * @params params the parameter of cache
 */
class CacheFrontendRequestPacket(
  params: CacheParams
) extends Bundle {
  val addr = UInt(params.pAddrWidth.W) // access address.
  val wData = UInt(params.blockSize.W)
  val wMask = UInt(params.blockSize.W)

  def toInternalRequestPacket: DataBankFrontendRequestPacket = {
    val res = Wire(new DataBankFrontendRequestPacket(params.databankParameter))
    res.addr := this.addr >> log2Ceil(params.blockSize / 8)
    res.flush_v := false.B

    res.refillData := DontCare
    res.refill_v := false.B

    res.asid := DontCare
    res.wData := this.wData
    res.wMask := this.wMask
    res.thid := DontCare

    res.perm := DontCare


    res
  }
}

/**
 * Request packet from the Page Manager to Cache to flush one item
 * @params params the parameter of cache
 */

class CacheFlushRequest(
  params: CacheParams
) extends Bundle {
  val addr = UInt(params.pAddrWidth.W) // access address.


  def toInternalRequestPacket: DataBankFrontendRequestPacket = {
    val res = Wire(new DataBankFrontendRequestPacket(params.databankParameter))

    res.addr := this.addr >> log2Ceil(params.blockSize / 8)
    res.flush_v := true.B

    res.refillData := DontCare
    res.refill_v := false.B

    res.asid := DontCare
    res.wData := DontCare
    res.wMask := DontCare
    res.thid := DontCare

    res.perm := DontCare // Flush request doesn't consider the perm.

    res
  }
}

/**
 * Refill request from the DRAM.
 * @params params the parameter of cache
 */
class CacheRefillRequest(
  params: CacheParams
) extends Bundle {
  val addr = UInt(params.pAddrWidth.W)
  // old request
  val wData = UInt(params.blockSize.W)
  val wMask = UInt(params.blockSize.W)

  // refill data
  val refillData = UInt(params.blockSize.W)

  def toInternalRequestPacket: DataBankFrontendRequestPacket = {
    val res = Wire(new DataBankFrontendRequestPacket(params.databankParameter))

    res.addr := this.addr
    res.flush_v := false.B

    res.refillData := refillData
    res.refill_v := true.B

    res.asid := DontCare
    res.wData := this.wData
    res.wMask := this.wMask
    res.thid := DontCare

    res.perm := DontCare
    res
  }
}

class MergedBackendRequestPacket(params: DatabankParams) extends Bundle{
  val addr = UInt(params.addrW.W)
  val asid = UInt(params.asidW.W)
  val thid = UInt(log2Ceil(params.thidN).W)
  val perm = UInt(2.W)
  val w_v = Bool()
  val flush_v = Bool() // this request is caused by flush
  val data = UInt(params.blockBit.W)

  override def cloneType: this.type = new MergedBackendRequestPacket(params).asInstanceOf[this.type]
}

class BackendRequestMerger(params: DatabankParams) extends MultiIOModule{
  val read_request_i = IO(Flipped(Decoupled(new MissRequestPacket(params))))
  val write_request_i = IO(Flipped(Decoupled(new WriteBackRequestPacket(params))))

  val backend_request_o = IO(Decoupled(new MergedBackendRequestPacket(params)))

  val read_request_converted = Wire(Decoupled(new MergedBackendRequestPacket(params)))
  read_request_converted.bits.addr := read_request_i.bits.addr
  read_request_converted.bits.data := DontCare
  read_request_converted.bits.asid := read_request_i.bits.asid
  read_request_converted.bits.w_v := false.B
  read_request_converted.bits.flush_v := false.B
  read_request_converted.bits.perm := read_request_i.bits.perm
  read_request_converted.bits.thid := read_request_i.bits.thid
  read_request_converted.valid := read_request_i.valid
  read_request_i.ready := read_request_converted.ready

  val write_request_converted = Wire(Decoupled(new MergedBackendRequestPacket(params)))
  write_request_converted.bits.addr := write_request_i.bits.addr
  write_request_converted.bits.data := write_request_i.bits.data
  write_request_converted.bits.asid := DontCare
  write_request_converted.bits.w_v := true.B
  write_request_converted.bits.flush_v := write_request_i.bits.flush_v
  write_request_converted.bits.perm := 1.U // The perm for writing is always 1.
  write_request_converted.bits.thid := DontCare
  write_request_converted.valid := write_request_i.valid
  write_request_i.ready := write_request_converted.ready

  val merger_rr = Module(new RRArbiter(new MergedBackendRequestPacket(params), 2))
  merger_rr.io.in(0) <> read_request_converted
  merger_rr.io.in(1) <> write_request_converted
  backend_request_o <> merger_rr.io.out
}

/**
 *  Base class of LRU module.
 *  @params lruCore the LRU updating logic
 *  @params params Databank parameters
 */
class LRU[T <: LRUCore](
  params: DatabankParams,
  lruCore: () => T
) extends MultiIOModule{
  val addr_i = IO(Input(UInt(params.setWidth().W)))
  val index_i = IO(Flipped(ValidIO(UInt(params.wayWidth().W))))
  val lru_o = IO(Output(UInt(params.wayWidth().W)))
  // add an extra stage to store the addr. They will be used for the LRU bits writing back.
  val addr_s1_r = if(params.implementedWithRegister) addr_i else RegNext(addr_i)

  // Connected to the LRU Core
  val core = Module(lruCore())

  implicit val bramParams = new BRAMParams(
    1,
    core.encodingWidth(),
    params.setNumber
    )

  val bram = Module(new BRAMorRegister(params.implementedWithRegister))

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
  param: CacheParams,
) extends MultiIOModule {
  val missRequest_i = IO(Flipped(Decoupled(new MissRequestPacket(param.databankParameter)))) // Input of the miss request
  val dramRefillData_i = IO(Flipped(Decoupled(UInt(param.blockSize.W)))) // the reply from the DRAM about cache miss
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
 * The size of all operands is `params.blockBit`, which is typically 512bit
 * for a cache. If you hope to access the cache by 32bit, please partition
 * the block manually.
 *
 * @params params the parameters of the cache
 * @params lruCore an generator of the LRU updating logic. See LRUCore.scala for all options.
 * @params updateFunction specify how to override the hit entry with given request and hit entry. Return the updated hit entry that will be written to.
 *
 * @note updateFunction will only be considered when it's a neither flushing nor refilling request, and return the old entry will not trigger a writing.
 */
class BaseCache(
  val param: CacheParams,
  lruCore: () => LRUCore,
  updateFunction: (DataBankFrontendRequestPacket, CacheEntry) => CacheEntry // (req: DataBankFrontendRequestPacket, entryToUpdate: CacheEntry) => result
) extends MultiIOModule{
  val u_bank_frontend = Module(new DataBankManager(param.databankParameter, updateFunction))
  val u_bram_adapter = Module(new BRAMPortAdapter(param.databankParameter))

  implicit val cfg = u_bram_adapter.bramParams
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
    param.pAddrWidth, param.blockSize
  ))))

  val flush_request_i = IO(Flipped(Decoupled(new CacheFlushRequest(param))))
  val reject_request_vi = IO(Input(Bool())) // When this signal is raised, no new requests will be accepted.

  val refill_data_i = IO(Flipped(Decoupled(UInt(param.blockSize.W))))
  u_refill_queue.dramRefillData_i <> refill_data_i

  val u_frontend_arb = Module(new Arbiter(u_bank_frontend.frontend_request_i.bits.cloneType(), 3))
  // the order:
  // - 0: Flush
  // - 1: Refill
  // - 2: Pipeline

  // The normal request from the pipeline
  u_frontend_arb.io.in(2).valid := pipeline_request_i.valid
  u_frontend_arb.io.in(2).bits.thid := DontCare
  u_frontend_arb.io.in(2).bits.addr := pipeline_request_i.bits.addr >> log2Ceil(param.blockSize / 8)
  u_frontend_arb.io.in(2).bits.asid := DontCare
  u_frontend_arb.io.in(2).bits.flush_v := false.B
  u_frontend_arb.io.in(2).bits.perm := DontCare
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
  val pipeline_response_o = IO(Valid(new PipeCache.CacheResponse(param.blockSize)))
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
    param: CacheParams,
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