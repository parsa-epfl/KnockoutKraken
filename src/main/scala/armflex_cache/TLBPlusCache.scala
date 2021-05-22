package armflex_cache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import armflex.ProcConfig
import armflex.MInst
import armflex.util._

/** Parameter structure for the whole memory system
  */
case class MemorySystemParameter(
  vAddressWidth:  Int = 64, // byte address
  pAddressWidth:  Int = 36, // 64GB
  pageSize:       Int = 4096, // page size
  tlbSetNumber:   Int = 16,
  asidWidth: Int = 15,
  tlbWayNumber:   Int = 4,
  cacheSetNumber: Int = 1024,
  cacheWayNumber: Int = 4,
  cacheBlockSize: Int = 512 // the size
) {

  def vPageNumberWidth(): Int = vAddressWidth - log2Ceil(pageSize)
  def pPageNumberWidth(): Int = pAddressWidth - log2Ceil(pageSize)
  def blockBiasWidth():   Int = log2Ceil(cacheBlockSize / 8)

  def toTLBParameter(): TLBParameter = {
    return new TLBParameter(
      vPageNumberWidth(),
      pPageNumberWidth(),
      2,
      asidWidth,
      tlbSetNumber,
      tlbWayNumber,
      true
    )
  }

  def toCacheParameter(): CacheParameter = {
    return new CacheParameter(
      cacheSetNumber,
      cacheWayNumber,
      cacheBlockSize,
      pAddressWidth - blockBiasWidth(),
      asidWidth,
      false
    )
  }
}

/** Suggested by the name. :)
  */
class TLBPlusCache(
  param:        MemorySystemParameter,
  cacheLRUCore: () => LRUCore,
  tlbLRUCore:   () => LRUCore)
    extends MultiIOModule {

  val cacheParam = param.toCacheParameter()
  val tlbParam = param.toTLBParameter()

  val u_cache = Module(BaseCache.generateCache(cacheParam, cacheLRUCore))
  val u_tlb = Module(new BaseTLB(tlbParam, tlbLRUCore))

  val frontend_request_i = IO(Flipped(Decoupled(new CacheFrontendRequestPacket(
    param.vAddressWidth - param.blockBiasWidth(), // virtual address with index rid
    param.asidWidth,
    param.cacheBlockSize
  ))))

  u_tlb.frontend_request_i.bits.tag.asid := frontend_request_i.bits.asid
  u_tlb.frontend_request_i.bits.tag.vpn := frontend_request_i.bits.addr(
    param.vAddressWidth - param.blockBiasWidth() - 1,
    log2Ceil(param.pageSize) - param.blockBiasWidth()
  )
  u_tlb.frontend_request_i.bits.permission := frontend_request_i.bits.permission
  u_tlb.frontend_request_i.valid := frontend_request_i.valid

  // val tlb_frontend_reply = u_tlb.frontend_reply_o
  u_cache.frontend_request_i.bits.addr := Cat(
    u_tlb.frontend_reply_o.bits.entry.ppn,
    frontend_request_i.bits.addr(
      log2Ceil(param.pageSize) - param.blockBiasWidth() - 1,
      0
    )
  )

  u_cache.frontend_request_i.bits.asid := frontend_request_i.bits.asid
  u_cache.frontend_request_i.bits.wData := frontend_request_i.bits.wData
  u_cache.frontend_request_i.bits.wMask := frontend_request_i.bits.wMask
  u_cache.frontend_request_i.bits.permission := frontend_request_i.bits.permission
  u_cache.frontend_request_i.valid := u_tlb.frontend_reply_o.valid &&
    u_tlb.frontend_reply_o.bits.hit && // TLB hit
    !u_tlb.frontend_reply_o.bits.violation // No violation

  frontend_request_i.ready := u_tlb.frontend_request_i.ready && u_cache.frontend_request_i.ready

  class tlb_cache_frontend_reply_t extends Bundle {
    val tlb_hit_v = Bool()
    val cache_hit_v = Bool()
    val data = UInt(cacheParam.blockBit.W)
    val asid = UInt(cacheParam.asidWidth.W)
  }

  // FIXME: The reply logic is not compatible with BRAM-based TLB. Need refactoring here.
  val frontend_reply_o = IO(Valid(new FrontendReplyPacket(param.toCacheParameter())))
  frontend_reply_o.valid := u_cache.frontend_reply_o.valid || RegNext(frontend_request_i.fire()) // tempo solution.
  frontend_reply_o.bits.hit := u_cache.frontend_reply_o.bits.hit && RegNext(
    u_tlb.frontend_reply_o.bits.hit && !u_tlb.frontend_reply_o.bits.violation
  )
  frontend_reply_o.bits.data := u_cache.frontend_reply_o.bits.data
  frontend_reply_o.bits.asid := u_cache.frontend_reply_o.bits.asid
  frontend_reply_o.bits.dirty := u_cache.frontend_reply_o.bits.dirty
  //frontend_reply_o.bits.tlb_hit_v := && RegNext(u_tlb.frontend_reply_o.bits.hit && !u_tlb.frontend_reply_o.bits.violation)

  // Flush
  // cache_flush_request_i
  val cache_flush_request_i = IO(Flipped(u_cache.flush_request_i.cloneType))
  cache_flush_request_i <> u_cache.flush_request_i
  // tlb_flush_request_i
  val tlb_flush_request_i = IO(Flipped(u_tlb.flush_request_i.cloneType))
  tlb_flush_request_i <> u_tlb.flush_request_i

  // Stall
  val stall_request_i = IO(Input(Bool()))
  u_cache.stall_request_vi := stall_request_i

  val tlb_flush_reply_o = IO(Output(u_tlb.frontend_reply_o.cloneType))
  tlb_flush_reply_o.bits := u_tlb.frontend_reply_o.bits
  // TODO: When the TLB is not a register file, this logic won't hold. Find a solution to it.
  tlb_flush_reply_o.valid := u_tlb.frontend_reply_o.valid && tlb_flush_request_i.fire()

  // Backend
  // tlb_backend_request_o
  val tlb_backend_request_o = IO(u_tlb.backend_request_o.cloneType)
  tlb_backend_request_o <> u_tlb.backend_request_o
  // tlb_backend_reply_i
  val tlb_backend_reply_i = IO(Flipped(u_tlb.backend_reply_i.cloneType))
  tlb_backend_reply_i <> u_tlb.backend_reply_i
  // cache_backend_request_o
  val cache_backend_request_o = IO(u_cache.backend_request_o.cloneType)
  cache_backend_request_o <> u_cache.backend_request_o
  // cache_backend_reply_i
  val cache_backend_reply_i = IO(Flipped(u_cache.refill_request_i.cloneType))
  cache_backend_reply_i <> u_cache.refill_request_i

  // Notify
  // tlb_packet_arrive_o
  val tlb_packet_arrive_o = IO(u_tlb.packet_arrive_o.cloneType)
  tlb_packet_arrive_o <> u_tlb.packet_arrive_o
  // tlb_violation_o
  val tlb_violation_o = IO(u_tlb.violation_o.cloneType)
  tlb_violation_o <> u_tlb.violation_o
  // cache packet arrive_o
  val cache_packet_arrive_o = IO(u_cache.packet_arrive_o.cloneType)
  cache_packet_arrive_o <> u_cache.packet_arrive_o
}



object CacheBackendToAXIInterface {

  /** Queue that stores the miss requests orderly. These requests are necessary when generating refilling packet.
    * @param param the Cache Parameter.
    */
  class RefillQueue(param: CacheParameter, queueDepth: Int) extends MultiIOModule {
    val miss_request_i = IO(Flipped(Decoupled(new MissRequestPacket(param))))
    val backend_reply_i = IO(Flipped(Decoupled(UInt(param.blockBit.W))))
    val refill_o = IO(Decoupled(new MissResolveReplyPacket(param)))
    // there is a queue for the miss request

    val miss_request_q = Queue(miss_request_i, queueDepth)
    assert(miss_request_i.ready)

    refill_o.bits.asid := miss_request_q.bits.asid
    refill_o.bits.not_sync_with_data_v := miss_request_q.bits.not_sync_with_data_v
    refill_o.bits.addr := miss_request_q.bits.addr
    refill_o.bits.data := backend_reply_i.bits

    refill_o.valid := miss_request_q.valid && Mux(
      miss_request_q.bits.not_sync_with_data_v,
      true.B,
      backend_reply_i.valid
    )
    miss_request_q.ready := refill_o.ready && Mux(
      miss_request_q.bits.not_sync_with_data_v,
      true.B,
      backend_reply_i.valid
    )
    backend_reply_i.ready := refill_o.ready && !(miss_request_q.bits.not_sync_with_data_v && backend_reply_i.valid)
  }

  class CacheBackendAXIAdaptors(param: MemorySystemParameter, queueSize: Int) extends MultiIOModule {
    assert(param.cacheBlockSize == 512, "Only 512bit AXI transactions is supported.")
    //assert(param.pAddressWidth == 36, "Only 36bit memory addres is supported.")

    val cache_backend_request_i = IO(Flipped(Decoupled(new MergedBackendRequestPacket(param.toCacheParameter()))))

    val q_cache_backend_request = Queue(cache_backend_request_i, queueSize)
    val pending_queue_empty_o = IO(Output(Bool()))
    pending_queue_empty_o := !q_cache_backend_request.valid

    val cache_backend_reply_o = IO(Decoupled(new MissResolveReplyPacket(param.toCacheParameter())))

    // AXI Read request
    val M_DMA_R = IO(new AXIReadMasterIF(
      param.pAddressWidth, 
      param.cacheBlockSize
    ))
    M_DMA_R.req.bits.length := 1.U
    M_DMA_R.req.bits.address := Cat(
      q_cache_backend_request.bits.addr,
      Fill(param.blockBiasWidth(), 0.U)
    )
    M_DMA_R.req.valid := !q_cache_backend_request.bits.w_v && q_cache_backend_request.valid

    cache_backend_reply_o.bits.addr := q_cache_backend_request.bits.addr
    cache_backend_reply_o.bits.data := M_DMA_R.data.bits
    cache_backend_reply_o.bits.not_sync_with_data_v := false.B
    cache_backend_reply_o.bits.asid := q_cache_backend_request.bits.asid
    cache_backend_reply_o.valid := M_DMA_R.data.valid
    M_DMA_R.data.ready := cache_backend_reply_o.ready

    // AXI Write Request
    val M_DMA_W = IO(new AXIWriteMasterIF(
      param.pAddressWidth,
      param.cacheBlockSize
    ))

    M_DMA_W.req.bits.address := Cat(
      q_cache_backend_request.bits.addr,
      Fill(param.blockBiasWidth(), 0.U)
    )
    M_DMA_W.req.bits.length := 1.U
    M_DMA_W.req.valid := q_cache_backend_request.bits.w_v && q_cache_backend_request.valid

    M_DMA_W.data.bits := q_cache_backend_request.bits.data
    // WARN: data valid should keep high until the done signal is received.
    M_DMA_W.data.valid := M_DMA_W.req.valid

    q_cache_backend_request.ready := Mux(
      q_cache_backend_request.bits.w_v,
      M_DMA_W.done,
      M_DMA_R.done
    )
  }

}
