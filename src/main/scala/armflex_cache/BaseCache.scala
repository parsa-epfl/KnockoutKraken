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
class CachePipelineReq(params: CacheParams) extends Bundle {
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

class CacheFlushRequest(params: CacheParams) extends Bundle {
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
class CacheRefillRequest(params: CacheParams) extends Bundle {
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
  val data = UInt(params.blockSize.W)

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

class CacheMMUIO(params: CacheParams) extends Bundle {
  val flushReq = Flipped(Decoupled(new CacheFlushRequest(params)))
  val stallReq = Input(Bool()) // When this signal is raised, no new requests will be accepted.
}

class CacheAxiMemoryIO(params: DatabankParams) extends Bundle {
  val req = Decoupled(new MergedBackendRequestPacket(params))
  val resp = Flipped(Decoupled(UInt(params.blockSize.W)))
}

/**
 * The generator for BRAM-based cache.
 *
 * The size of all operands is `params.blockSize`, sel is typically 512bit
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
  val params: CacheParams,
  lruCore: () => LRUCore,
  updateFunction: (DataBankFrontendRequestPacket, CacheEntry) => CacheEntry // (req: DataBankFrontendRequestPacket, entryToUpdate: CacheEntry) => result
) extends MultiIOModule{
  val pipeline_io = IO(Flipped(new PipeCache.PipeCacheIO(params.pAddrWidth, params.blockSize)))
  val mmu_i = IO(new CacheMMUIO(params))
  val axiMem_io = IO(new CacheAxiMemoryIO(params.databankParameter))

  private val u_bramPortsAdapter = Module(new BRAMPortAdapter(params.databankParameter))
  private val u_dataBankManager = Module(new DataBankManager(params.databankParameter, updateFunction))
  private val bram = Module(new BRAMorRegister(false)(u_bramPortsAdapter.bramParams))
  private val u_lruCore = Module(new LRU(params.databankParameter, lruCore))
  private val u_refillQueue = Module(new RefillQueue(params))
  private val u_backendMerger = Module(new BackendRequestMerger(params.databankParameter))

  // Manage ports for DataBank -----
  private val u_3wayArbiter = Module(new Arbiter(u_dataBankManager.frontend_request_i.bits.cloneType(), 3))
  private val arbFlushPort = u_3wayArbiter.io.in(0)
  private val arbRefillPort = u_3wayArbiter.io.in(1)
  private val arbPipelinePort = u_3wayArbiter.io.in(2)

  // Manage BRAM ports -----
  u_bramPortsAdapter.bram_ports(0) <> bram.portA
  u_bramPortsAdapter.bram_ports(1) <> bram.portB

  u_bramPortsAdapter.frontend_read_reply_data_o <> u_dataBankManager.bank_ram_reply_data_i
  u_bramPortsAdapter.frontend_read_request_i <> u_dataBankManager.bank_ram_request_addr_o
  u_bramPortsAdapter.frontend_write_request_i <> u_dataBankManager.bank_ram_write_request_o

  // Connect LRU Policy -----
  u_lruCore.addr_i <> u_dataBankManager.lru_addr_o
  u_lruCore.index_i <> u_dataBankManager.lru_index_o
  u_dataBankManager.lru_which_i := u_lruCore.lru_o

  u_refillQueue.dramRefillData_i <> axiMem_io.resp

  // The normal request from the pipeline
  arbPipelinePort.valid := pipeline_io.req.valid
  arbPipelinePort.bits.thid := DontCare
  arbPipelinePort.bits.addr := pipeline_io.req.bits.addr >> log2Ceil(params.blockSize / 8)
  arbPipelinePort.bits.asid := DontCare
  arbPipelinePort.bits.flush_v := false.B
  arbPipelinePort.bits.perm := DontCare
  arbPipelinePort.bits.refillData := DontCare
  arbPipelinePort.bits.refill_v := false.B
  arbPipelinePort.bits.wData := pipeline_io.req.bits.data
  arbPipelinePort.bits.wMask := Cat(pipeline_io.req.bits.w_en.asBools().map(Fill(8, _)).reverse)
  pipeline_io.req.ready := !mmu_i.flushReq.valid && !u_refillQueue.refillRequest_o.valid && !mmu_i.stallReq

  // The flush request from the mmu
  arbFlushPort.valid := mmu_i.flushReq.valid
  arbFlushPort.bits := mmu_i.flushReq.bits.toInternalRequestPacket
  mmu_i.flushReq.ready := arbFlushPort.ready

  // The refilling/new block arrival request from DRAM
  arbRefillPort.valid := u_refillQueue.refillRequest_o.valid
  arbRefillPort.bits := u_refillQueue.refillRequest_o.bits.toInternalRequestPacket
  u_refillQueue.refillRequest_o.ready := !mmu_i.flushReq.valid && !mmu_i.stallReq

  u_dataBankManager.frontend_request_i.bits := u_3wayArbiter.io.out.bits
  u_dataBankManager.frontend_request_i.valid := !mmu_i.stallReq && u_3wayArbiter.io.out.valid
  u_3wayArbiter.io.out.ready := !mmu_i.stallReq && u_dataBankManager.frontend_request_i.ready

  // Reply to pipeline ------
  pipeline_io.resp.valid := u_dataBankManager.frontend_reply_o.valid && !u_dataBankManager.frontend_reply_o.bits.flush
  pipeline_io.resp.bits.data := Mux(
    u_dataBankManager.frontend_reply_o.bits.refill,
    u_dataBankManager.frontend_reply_o.bits.wData, // the data being written into BRAM will return in case it's a fetch
    u_dataBankManager.frontend_reply_o.bits.rData
  )
  pipeline_io.resp.bits.hit := u_dataBankManager.frontend_reply_o.bits.hit
  pipeline_io.resp.bits.miss2hit := u_dataBankManager.frontend_reply_o.bits.refill
  pipeline_io.resp.bits.miss := !pipeline_io.resp.bits.hit

  // Connect to the backend merger ------
  u_backendMerger.write_request_i <> u_dataBankManager.writeback_request_o

  // Backend Queue
  axiMem_io.req <> u_backendMerger.backend_request_o

  u_refillQueue.missRequest_i.bits := u_dataBankManager.miss_request_o.bits
  u_backendMerger.read_request_i.bits := u_dataBankManager.miss_request_o.bits

  u_dataBankManager.miss_request_o.ready := u_refillQueue.missRequest_i.ready && u_backendMerger.read_request_i.ready
  u_refillQueue.missRequest_i.valid := u_dataBankManager.miss_request_o.valid && u_backendMerger.read_request_i.ready
  u_backendMerger.read_request_i.valid := u_dataBankManager.miss_request_o.valid && u_refillQueue.missRequest_i.ready
}

object BaseCache {
  def apply(
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