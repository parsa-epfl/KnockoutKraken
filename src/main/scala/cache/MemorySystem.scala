package armflex.cache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import armflex.ProcConfig
import armflex.MInst
import armflex.util.FlushQueue
import arm.PROCESSOR_TYPES
import arm.DECODE_CONTROL_SIGNALS._

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
  def blockBiasWidth(): Int = log2Ceil(cacheBlockSize / 8)

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
      pAddressWidth - blockBiasWidth(),
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
    param.vAddressWidth - param.blockBiasWidth(), // virtual address with index rid
    log2Ceil(param.threadNumber),
    param.cacheBlockSize
  ))))

  u_tlb.frontend_request_i.bits.tag.thread_id := frontend_request_i.bits.thread_id
  u_tlb.frontend_request_i.bits.tag.vpage := frontend_request_i.bits.addr(
    param.vAddressWidth - param.blockBiasWidth() - 1,
    log2Ceil(param.pageSize) - param.blockBiasWidth()
  )
  u_tlb.frontend_request_i.bits.w_v := frontend_request_i.bits.w_v
  u_tlb.frontend_request_i.valid := frontend_request_i.valid

  // val tlb_frontend_reply = u_tlb.frontend_reply_o
  u_cache.frontend_request_i.bits.addr := Cat(
    u_tlb.frontend_reply_o.bits.pp,
    frontend_request_i.bits.addr(
      log2Ceil(param.pageSize) - param.blockBiasWidth() - 1,
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

object CacheInterfaceAdaptors {

/**
  * Generate Cache block and writing mask according to the data size and address bias
  * @param blockSize the size of a cache block. For instance, 512 mean 512 bit
  * @param data the data from the register file. 64bit with effective data starting from 0. For instance 0x000000000000000F for a byte
  * @param size the size of the operand in the @param data. Mask will be generated according to this parameter. For instance SIZEB means a byte
  * @param addrBias the address to shift the @param data to the correct position. Usually the lower 6 bits (512-bit cache block) of the address
  * @return the Pair (shifted data, mask)
  */ 
private def shiftData(blockSize: Int, data: UInt, size: UInt, addrBias: UInt): (UInt, UInt) = { // (data, mask)
  val accessMaskingInByte = MuxLookup(size, 0.U, Array(
    SIZEB -> 1.U((PROCESSOR_TYPES.DATA_SZ / 8).W),
    SIZEH -> 3.U((PROCESSOR_TYPES.DATA_SZ / 8).W),
    SIZE32 -> 15.U((PROCESSOR_TYPES.DATA_SZ / 8).W),
    SIZE64 -> Fill((PROCESSOR_TYPES.DATA_SZ / 8), true.B)
  ))
  val accessMaskingInBit = VecInit(accessMaskingInByte.asBools().map(Fill(8, _))).asUInt()
  assert(data.getWidth == PROCESSOR_TYPES.DATA_SZ)
  val res = WireInit(UInt(blockSize.W), data) // apply mask before logic extending to 512bit.
  return (res << (addrBias << 3), accessMaskingInBit << (addrBias << 3))
}

/**
 * Recover the data from a cache block (512bit) and shift it in the lowest position.
 * @param blockSize the size of a cache block (512bit)
 * @param data a cache line
 * @param size the mask of required operand 
 * @param addrBias the lower 6 bits of the address. It defines data's position in the cache block as well as determines how many bits to shift in order to recover the data.
 * @return the recovered data
 */ 
private def recoverData(blockSize: Int, data: UInt, size: UInt, addrBias: UInt): UInt = {
  val accessMaskingInByte = MuxLookup(size, 0.U, Array(
    SIZEB -> 1.U((PROCESSOR_TYPES.DATA_SZ / 8).W),
    SIZEH -> 3.U((PROCESSOR_TYPES.DATA_SZ / 8).W),
    SIZE32 -> 15.U((PROCESSOR_TYPES.DATA_SZ / 8).W),
    SIZE64 -> Fill((PROCESSOR_TYPES.DATA_SZ / 8), true.B)
  ))
  val accessMaskingInBit = VecInit(accessMaskingInByte.asBools().map(Fill(8, _))).asUInt()
  assert(data.getWidth == blockSize)
  return (data >> (addrBias << 3)) & accessMaskingInBit
}

/**
 * The handshake packet between request adaptor and reply adaptor.
 * @param param Cache Parameter
 */ 
class CacheInterfaceHandshakePacket(param: CacheParameter) extends Bundle {
  val bias_addr = UInt(log2Ceil(param.blockBit / 8).W)
  val size = UInt(2.W)
  val thread_id = UInt(param.threadIDWidth.W)
  val pair_v = Bool()
  val order = Bool() // 0: 1:
}

/**
 * Adaptor converting the Pipeline request to the TLB/Cache request.
 * Add 1 cycle latency to the data path
 * @param param the parameter of the Memory system
 * implicit @param cfg the configuration of ARMFlex pipeline
 */ 
class CacheRequestAdaptor (
  val param: MemorySystemParameter
)(
  implicit cfg: ProcConfig
) extends MultiIOModule {
  val i = IO(Flipped(Decoupled(new MInst)))
  val o = IO(Decoupled(new CacheFrontendRequestPacket(param.toCacheParameter())))

  class internal_memory_request_t extends Bundle {
    val size = UInt(2.W)
    val vaddr = UInt(param.vAddressWidth.W)
    val thread_id = UInt(log2Ceil(param.threadNumber).W)
    val w_v = Bool()
    val w_data = PROCESSOR_TYPES.DATA_T
    val pair_order = UInt(1.W)
  }

  // TODO: Complete the connection between from_pipeline and i
  val from_pipeline = Decoupled(new internal_memory_request_t)
  from_pipeline.bits.size := i.bits.size
  from_pipeline.bits.vaddr := i.bits.memReq(0).addr
  // TODO: where is the thread id?
  from_pipeline.bits.w_data := i.bits.memReq(0).data
  from_pipeline.bits.pair_order := 0.U // the first request
  from_pipeline.valid := i.valid
  i.ready := from_pipeline.ready

  val s1_pair_context_n = Decoupled(new internal_memory_request_t)
  s1_pair_context_n.bits.size := i.bits.size
  // TODO: where is the thread id?
  // pair_context_r.bits.thread_id := 0
  s1_pair_context_n.bits.vaddr := i.bits.memReq(1).addr
  s1_pair_context_n.bits.w_data := i.bits.memReq(1).addr
  s1_pair_context_n.bits.pair_order := 1.U // the second request
  s1_pair_context_n.valid := from_pipeline.fire() && i.bits.isPair
  val s1_pair_context_r = FlushQueue(s1_pair_context_n, 1, false)("s1_pair_context_r")

  val u_arb = Module(new Arbiter(new internal_memory_request_t, 2))
  u_arb.io.in(0) <> s1_pair_context_r
  u_arb.io.in(1) <> from_pipeline
  // shift shift shift!

  val shifted = shiftData(
    param.cacheBlockSize,
    u_arb.io.out.bits.w_data,
    u_arb.io.out.bits.size,
    u_arb.io.out.bits.vaddr(
      param.blockBiasWidth() - 1,
      0
    )
  )

  o.bits.addr := u_arb.io.out.bits.vaddr(
    param.vAddressWidth-1,
    param.blockBiasWidth()
  )
  o.bits.thread_id := u_arb.io.out.bits.thread_id
  o.bits.w_v := u_arb.io.out.bits.w_v
  o.bits.wData := shifted._1
  o.bits.wMask := shifted._2

  o.valid := u_arb.io.out.valid
  u_arb.io.out.ready := o.ready

  // a message to the reply arbiter for synchronization
  val sync_message = Decoupled(new CacheInterfaceHandshakePacket(param.toCacheParameter()))
  sync_message.bits.size := u_arb.io.out.bits.size
  sync_message.bits.bias_addr := u_arb.io.out.bits.vaddr(
    param.blockBiasWidth()-1,
    0
  )
  sync_message.bits.order := u_arb.io.out.bits.pair_order
  sync_message.bits.thread_id := u_arb.io.out.bits.thread_id
  sync_message.valid := o.fire()
  assert(sync_message.ready, "It's impossible that the FIFO of sync_message is full")

  val sync_message_o = IO(Decoupled(new CacheInterfaceHandshakePacket(param.toCacheParameter())))
  sync_message_o := Queue(sync_message, 2 * param.threadNumber)
}

class CacheReplyAdaptor (
  val param: MemorySystemParameter
)(
  implicit cfg: ProcConfig
) extends MultiIOModule {
  val cache_reply_i = IO(Flipped(Valid(new FrontendReplyPacket(param.toCacheParameter()))))
  val data_o = IO(Valid(Vec(2, PROCESSOR_TYPES.DATA_T)))
  val sync_message_i = IO(Flipped(Decoupled(new CacheInterfaceHandshakePacket(param.toCacheParameter()))))
  // sync_message_i should align with cache_reply_i
  assert(cache_reply_i.valid === sync_message_i.valid, "sync_message_i should align with cache_reply_i.")
  
  when(cache_reply_i.valid){
    assert(cache_reply_i.bits.thread_id === sync_message_i.bits.thread_id)
  }

  val recovered_data = recoverData(param.cacheBlockSize, cache_reply_i.bits.data, sync_message_i.bits.size, sync_message_i.bits.bias_addr)

  // For normal case, just return is fine
  val single_transaction_result = Valid(Vec(2, PROCESSOR_TYPES.DATA_T))
  single_transaction_result.bits(0) := recovered_data
  single_transaction_result.bits(1) := DontCare
  single_transaction_result.valid := sync_message_i.valid && !sync_message_i.bits.pair_v && cache_reply_i.bits.hit

  // store context
  class store_context_t extends Bundle {
    val data = PROCESSOR_TYPES.DATA_T
    val v = Bool()
  }

  val s1_store_context_r = Reg(new store_context_t)
  s1_store_context_r.data := recovered_data
  s1_store_context_r.v := sync_message_i.valid && sync_message_i.bits.pair_v && sync_message_i.bits.order === 0.U && cache_reply_i.bits.hit
  // for pair instruction, result generated here.
  val pair_transaction_result = Valid(Vec(2, PROCESSOR_TYPES.DATA_T))
  pair_transaction_result.bits(0) := s1_store_context_r.data
  pair_transaction_result.bits(1) := recovered_data
  pair_transaction_result.valid := s1_store_context_r.v && sync_message_i.valid && sync_message_i.bits.pair_v && sync_message_i.bits.order === 1.U && cache_reply_i.bits.hit

  // select data_o from two options
  assert(!(single_transaction_result.valid && pair_transaction_result.valid), "It's impossible to collect result in both single and pair way!")
  data_o.valid := pair_transaction_result.valid || single_transaction_result.valid
  data_o.bits := Mux(single_transaction_result.valid, single_transaction_result.bits, pair_transaction_result.bits)
}

}

