package armflex.cache

import chisel3._
import chisel3.util._
import chisel3.experimental._


/**
 * Parameter structure for the whole memory system
 */ 
case class MemorySystemParameter(
  vAddressWidth: Int = 64, // byte address
  pAddressWidth: Int = 36, // 64GB
  pageSize: Int = 4096, // page size

  threadNumber: Int = 4,

  tlbSetNumber: Int = 8,
  tlbWayNumber: Int = 16,

  cacheSetNumber: Int = 1024,
  cacheWayNumber: Int = 4,
  cacheBlockSize: Int = 512, // the size
){

def vPageNumberWidth(): Int = vAddressWidth - log2Ceil(pageSize)
def pPageNumberWidth(): Int = pAddressWidth - log2Ceil(pageSize)

def toTLBParameter(): TLBParameter = {
  return new TLBParameter(
    vPageNumberWidth(),
    pPageNumberWidth(),
    threadNumber,
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
    pAddressWidth - log2Ceil(cacheBlockSize),
    threadNumber,
    false
  )
}
}



/**
 * Suggested by the name. :)
 * 
 */ 
class TLBPlusCache (
  param: MemorySystemParameter,
  cacheLRUCore: () => LRUCore,
  tlbLRUCore: () => LRUCore,
) extends MultiIOModule {

  val cacheParam = param.toCacheParameter()
  val tlbParam = param.toTLBParameter()

  val u_cache = Module(BaseCache.generateCache(cacheParam, cacheLRUCore))
  val u_tlb = Module(new BaseTLB(tlbParam, tlbLRUCore))

  val frontend_request_i = IO(Flipped(Decoupled(new CacheFrontendRequestPacket(
    param.vAddressWidth - log2Ceil(param.cacheBlockSize), // virtual address with index rid
    log2Ceil(param.threadNumber),
    param.cacheBlockSize
  ))))

  u_tlb.frontend_request_i.bits.tag.thread_id := frontend_request_i.bits.thread_id
  u_tlb.frontend_request_i.bits.tag.vpage := frontend_request_i.bits.addr(
    param.vAddressWidth - log2Ceil(param.cacheBlockSize) - 1,
    log2Ceil(param.pageSize) - log2Ceil(param.cacheBlockSize)
  )
  u_tlb.frontend_request_i.bits.w_v := frontend_request_i.bits.w_v
  u_tlb.frontend_request_i.valid := frontend_request_i.valid

  // val tlb_frontend_reply = u_tlb.frontend_reply_o
  u_cache.frontend_request_i.bits.addr := Cat(
    u_tlb.frontend_reply_o.bits.pp,
    frontend_request_i.bits.addr(
      log2Ceil(param.pageSize) - log2Ceil(param.cacheBlockSize)-1,
      0
    )
  )

  u_cache.frontend_request_i.bits.thread_id := frontend_request_i.bits.thread_id
  u_cache.frontend_request_i.bits.wData := frontend_request_i.bits.wData
  u_cache.frontend_request_i.bits.wMask := frontend_request_i.bits.wMask
  u_cache.frontend_request_i.bits.w_v := frontend_request_i.bits.w_v
  u_cache.frontend_request_i.valid := u_tlb.frontend_reply_o.valid && 
    u_tlb.frontend_reply_o.bits.hit && // TLB hit
    !u_tlb.frontend_reply_o.bits.violation // No violation

  frontend_request_i.ready := u_tlb.frontend_request_i.ready && u_cache.frontend_request_i.ready

  class tlb_cache_frontend_reply_t extends Bundle {
    val tlb_hit_v = Bool()
    val cache_hit_v = Bool()
    val data = UInt(cacheParam.blockBit.W)
    val thread_id = UInt(cacheParam.threadIDWidth().W)
  }

  val frontend_reply_o = IO(Valid(new tlb_cache_frontend_reply_t))
  frontend_reply_o.valid := u_cache.frontend_reply_o.valid
  frontend_reply_o.bits.cache_hit_v := u_cache.frontend_reply_o.bits.hit
  frontend_reply_o.bits.data := u_cache.frontend_reply_o.bits.data
  frontend_reply_o.bits.thread_id := u_cache.frontend_reply_o.bits.thread_id
  frontend_reply_o.bits.tlb_hit_v := RegNext(u_tlb.frontend_reply_o.bits.hit && !u_tlb.frontend_reply_o.bits.violation)

  // Flush
  // cache_flush_request_i
  val cache_flush_request_i = IO(Flipped(u_cache.flush_request_i.cloneType))
  cache_flush_request_i <> u_cache.flush_request_i
  // tlb_flush_request_i
  val tlb_flush_request_i = IO(Flipped(u_tlb.flush_request_i.cloneType))
  tlb_flush_request_i <> u_tlb.flush_request_i

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

object TLBPlusCacheRunner extends App {
  import chisel3.stage.ChiselStage
  val param = new MemorySystemParameter(
    
  )
  println(
    (new ChiselStage).emitVerilog(new TLBPlusCache(
      param,
      () => new PseudoTreeLRUCore(param.cacheWayNumber),
      () => new PseudoTreeLRUCore(param.tlbWayNumber)  
    ))
  )
}
