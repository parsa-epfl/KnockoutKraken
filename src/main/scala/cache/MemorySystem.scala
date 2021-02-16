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

  tlbSetNumber: Int = 16,
  tlbWayNumber: Int = 8,

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
    u_tlb.frontend_reply_o.bits.entry.pp,
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

  // val frontend_reply_o = IO(Valid(new tlb_cache_frontend_reply_t))
  val frontend_reply_o = IO(Valid(new FrontendReplyPacket(param.toCacheParameter())))
  frontend_reply_o.valid := u_cache.frontend_reply_o.valid
  frontend_reply_o.bits.hit := u_cache.frontend_reply_o.bits.hit && RegNext(u_tlb.frontend_reply_o.bits.hit && !u_tlb.frontend_reply_o.bits.violation)
  frontend_reply_o.bits.data := u_cache.frontend_reply_o.bits.data
  frontend_reply_o.bits.thread_id := u_cache.frontend_reply_o.bits.thread_id
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
  val order = UInt(1.W) // 0: 1:

  override def cloneType: this.type = new CacheInterfaceHandshakePacket(param).asInstanceOf[this.type]
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
  //implicit cfg: ProcConfig
) extends MultiIOModule {
  // val i = IO(Flipped(Decoupled(new MInst)))
  class request_from_pipeline_t extends Bundle {
    val size = UInt(2.W)
    val memReq = Vec(2, new Bundle{
      val addr = UInt(param.vAddressWidth.W)
      val data = PROCESSOR_TYPES.DATA_T
    })
    val isLoad = Bool()
    val isPair = Bool()
    val threadID = UInt(log2Ceil(param.threadNumber).W)
  }
  // TODO: Replace the request_from_pipelie_t with the MInst
  val i = IO(Flipped(Decoupled(new request_from_pipeline_t)))

  val o = IO(Decoupled(new CacheFrontendRequestPacket(param.toCacheParameter())))

  class internal_memory_request_t extends Bundle {
    val size = UInt(2.W)
    val vaddr = UInt(param.vAddressWidth.W)
    val thread_id = UInt(log2Ceil(param.threadNumber).W)
    val w_v = Bool()
    val w_data = PROCESSOR_TYPES.DATA_T
    val pair_order = UInt(1.W)
    val pair_v = Bool()
  }

  val from_pipeline = Wire(Decoupled(new internal_memory_request_t))
  from_pipeline.bits.size := i.bits.size
  from_pipeline.bits.vaddr := i.bits.memReq(0).addr
  from_pipeline.bits.thread_id := i.bits.threadID
  from_pipeline.bits.w_v := !i.bits.isLoad
  from_pipeline.bits.w_data := i.bits.memReq(0).data
  from_pipeline.bits.pair_order := 0.U // the first request
  from_pipeline.bits.pair_v := i.bits.isPair
  from_pipeline.valid := i.valid
  i.ready := from_pipeline.ready

  val s1_pair_context_n = Wire(Decoupled(new internal_memory_request_t))
  s1_pair_context_n.bits.size := i.bits.size
  s1_pair_context_n.bits.thread_id := i.bits.threadID
  s1_pair_context_n.bits.vaddr := i.bits.memReq(1).addr
  s1_pair_context_n.bits.w_v := !i.bits.isLoad
  s1_pair_context_n.bits.w_data := i.bits.memReq(1).addr
  s1_pair_context_n.bits.pair_order := 1.U // the second request
  s1_pair_context_n.bits.pair_v := true.B
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
  val sync_message = Wire(Decoupled(new CacheInterfaceHandshakePacket(param.toCacheParameter())))
  sync_message.bits.size := u_arb.io.out.bits.size
  sync_message.bits.bias_addr := u_arb.io.out.bits.vaddr(
    param.blockBiasWidth()-1,
    0
  )
  sync_message.bits.order := u_arb.io.out.bits.pair_order
  sync_message.bits.pair_v := u_arb.io.out.bits.pair_v
  sync_message.bits.thread_id := u_arb.io.out.bits.thread_id
  sync_message.valid := o.fire()
  assert(sync_message.ready, "It's impossible that the FIFO of sync_message is full")

  val sync_message_o = IO(Decoupled(new CacheInterfaceHandshakePacket(param.toCacheParameter())))
  sync_message_o <> Queue(sync_message, 2 * param.threadNumber)
}

// The purpose of this module is to apply masking and shifting to the data so that it's in the correct position.
class CacheReplyAdaptor (
  val param: MemorySystemParameter
)(
  //implicit cfg: ProcConfig
) extends MultiIOModule {
  val cache_reply_i = IO(Flipped(Valid(new FrontendReplyPacket(param.toCacheParameter()))))
  // val data_o = IO(Valid(Vec(2, PROCESSOR_TYPES.DATA_T))) 
  val data_o = IO(Valid(new FrontendReplyPacket(param.toCacheParameter())))
  val sync_message_i = IO(Flipped(Decoupled(new CacheInterfaceHandshakePacket(param.toCacheParameter()))))
  // sync_message_i should align with cache_reply_i
  assert(cache_reply_i.valid === sync_message_i.valid, "sync_message_i should align with cache_reply_i.")
  sync_message_i.ready := true.B //?

  when(cache_reply_i.valid){
    assert(cache_reply_i.bits.thread_id === sync_message_i.bits.thread_id)
  }

  val recovered_data = recoverData(param.cacheBlockSize, cache_reply_i.bits.data, sync_message_i.bits.size, sync_message_i.bits.bias_addr)

  data_o.bits.data := recovered_data
  data_o.bits.dirty := cache_reply_i.bits.dirty
  data_o.bits.hit := cache_reply_i.bits.hit
  data_o.bits.thread_id := cache_reply_i.bits.thread_id
  data_o.valid := cache_reply_i.valid
}

}

object CacheBackendToAXIInterface{

/**
 * Queue that stores the miss requests orderly. These requests are necessary when generating refilling packet.
 * @param param the Cache Parameter.
 */ 
class RefillQueue(param: CacheParameter) extends MultiIOModule{
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

import DMAController.Bus._
import DMAController.Worker.XferDescBundle
import DMAController.Frontend.{AXI4Reader, AXI4Writer}

class CacheBackendAXIAdaptors(param: MemorySystemParameter) extends MultiIOModule {
  val M_AXI = IO(new AXI4(
    param.pAddressWidth, param.cacheBlockSize
  ))

  val cache_backend_request_i = IO(Flipped(Decoupled(new MergedBackendRequestPacket(param.toCacheParameter()))))

  val q_cache_backend_request = Queue(cache_backend_request_i, param.threadNumber * 2)
  val pending_queue_empty_o = IO(Output(Bool()))
  pending_queue_empty_o := !q_cache_backend_request.valid

  val cache_backend_reply_o = IO(Decoupled(new MissResolveReplyPacket(param.toCacheParameter())))

  val u_axi_read_engine = Module(new AXI4Reader(param.pAddressWidth, param.cacheBlockSize))

  M_AXI.ar <> u_axi_read_engine.io.bus.ar
  M_AXI.r <> u_axi_read_engine.io.bus.r
  u_axi_read_engine.io.bus.b <> AXI4B.stub()
  u_axi_read_engine.io.bus.aw <> AXI4AW.stub(param.pAddressWidth)
  u_axi_read_engine.io.bus.w <> AXI4W.stub(param.cacheBlockSize)

  u_axi_read_engine.io.xfer.length := 1.U
  u_axi_read_engine.io.xfer.address := Cat(q_cache_backend_request.bits.addr, Fill(log2Ceil(param.cacheBlockSize), 0.U))
  u_axi_read_engine.io.xfer.valid := !q_cache_backend_request.bits.w_v && q_cache_backend_request.valid

  cache_backend_reply_o.bits.addr := q_cache_backend_request.bits.addr
  cache_backend_reply_o.bits.data := u_axi_read_engine.io.dataOut.bits // FIXME: Add FIFO here to shrink the critical path?
  cache_backend_reply_o.bits.not_sync_with_data_v := false.B
  cache_backend_reply_o.bits.thread_id := q_cache_backend_request.bits.thread_id
  cache_backend_reply_o.valid := u_axi_read_engine.io.dataOut.valid
  u_axi_read_engine.io.dataOut.ready := cache_backend_reply_o.ready

  val u_axi_write_engine = Module(new AXI4Writer(param.pAddressWidth, param.cacheBlockSize))

  u_axi_write_engine.io.xfer.address := Cat(q_cache_backend_request.bits.addr, Fill(log2Ceil(param.cacheBlockSize), 0.U))
  u_axi_write_engine.io.xfer.length := 1.U
  u_axi_write_engine.io.xfer.valid := q_cache_backend_request.bits.w_v && q_cache_backend_request.valid

  u_axi_write_engine.io.dataIn.bits := q_cache_backend_request.bits.data
  u_axi_write_engine.io.dataIn.valid := u_axi_write_engine.io.xfer.valid

  q_cache_backend_request.ready := Mux(
    q_cache_backend_request.bits.w_v,
    u_axi_write_engine.io.xfer.done,
    u_axi_read_engine.io.xfer.done
  )

  M_AXI.aw <> u_axi_write_engine.io.bus.aw
  M_AXI.w <> u_axi_write_engine.io.bus.w
  M_AXI.b <> u_axi_write_engine.io.bus.b
  u_axi_write_engine.io.bus.ar <> AXI4AR.stub(param.pAddressWidth)
  u_axi_write_engine.io.bus.r <> AXI4R.stub(param.cacheBlockSize)
}

}

class InstructionMemorySystem(
  param: MemorySystemParameter
) extends MultiIOModule {
  val u_tlb_plus_cache = Module(new TLBPlusCache(
    param, 
    () => new MatrixLRUCore(param.cacheWayNumber),
    () => new PseudoTreeLRUCore(param.tlbWayNumber)
  ))

  val frontend_request_i = IO(Flipped(u_tlb_plus_cache.frontend_request_i.cloneType))
  frontend_request_i <> u_tlb_plus_cache.frontend_request_i

  val frontend_reply_o = IO(u_tlb_plus_cache.frontend_reply_o.cloneType)
  frontend_reply_o <> u_tlb_plus_cache.frontend_reply_o

  val cache_flush_request_i = IO(Flipped(u_tlb_plus_cache.cache_flush_request_i.cloneType))
  cache_flush_request_i <> u_tlb_plus_cache.cache_flush_request_i

  val tlb_flush_request_i = IO(Flipped(u_tlb_plus_cache.tlb_flush_request_i.cloneType))
  tlb_flush_request_i <> u_tlb_plus_cache.tlb_flush_request_i

  val tlb_flush_reply_o = IO(u_tlb_plus_cache.tlb_flush_reply_o.cloneType)
  tlb_flush_reply_o <> u_tlb_plus_cache.tlb_flush_reply_o

  val stall_request_i = IO(Input(Bool()))
  u_tlb_plus_cache.stall_request_i := stall_request_i

  val tlb_backend_request_o = IO(u_tlb_plus_cache.tlb_backend_request_o.cloneType)
  tlb_backend_request_o <> u_tlb_plus_cache.tlb_backend_request_o

  val tlb_backend_reply_i = IO(Flipped(u_tlb_plus_cache.tlb_backend_reply_i.cloneType))
  tlb_backend_reply_i <> u_tlb_plus_cache.tlb_backend_reply_i

  val cache_backend_request_o = IO(u_tlb_plus_cache.cache_backend_request_o.cloneType)
  cache_backend_request_o <> u_tlb_plus_cache.cache_backend_request_o

  val cache_backend_reply_i = IO(Flipped(u_tlb_plus_cache.cache_backend_reply_i.cloneType))
  cache_backend_reply_i <> u_tlb_plus_cache.cache_backend_reply_i

  val tlb_packet_arrive_o = IO(u_tlb_plus_cache.tlb_packet_arrive_o.cloneType)
  tlb_packet_arrive_o <> u_tlb_plus_cache.tlb_packet_arrive_o

  val tlb_violation_o = IO(u_tlb_plus_cache.tlb_violation_o.cloneType)
  tlb_violation_o <> u_tlb_plus_cache.tlb_violation_o

  val cache_packet_arrive_o = IO(u_tlb_plus_cache.cache_packet_arrive_o.cloneType)
  cache_packet_arrive_o <> u_tlb_plus_cache.cache_packet_arrive_o
}

class DataMemorySystem(
  param: MemorySystemParameter
) extends MultiIOModule {
  val u_tlb_plus_cache = Module(new TLBPlusCache(
    param, 
    () => new MatrixLRUCore(param.cacheWayNumber),
    () => new PseudoTreeLRUCore(param.tlbWayNumber)
  ))

  val u_request_adaptor = Module(new CacheInterfaceAdaptors.CacheRequestAdaptor(param)())
  val u_reply_adaptor = Module(new CacheInterfaceAdaptors.CacheReplyAdaptor(param)())

  u_reply_adaptor.sync_message_i <> u_request_adaptor.sync_message_o
  
  val frontend_request_i = IO(Flipped(u_request_adaptor.i.cloneType))
  frontend_request_i <> u_request_adaptor.i
  u_request_adaptor.o <> u_tlb_plus_cache.frontend_request_i

  val frontend_reply_o = IO(u_reply_adaptor.data_o.cloneType)
  frontend_reply_o <> u_reply_adaptor.data_o
  u_reply_adaptor.cache_reply_i <> u_tlb_plus_cache.frontend_reply_o

  val cache_flush_request_i = IO(Flipped(u_tlb_plus_cache.cache_flush_request_i.cloneType))
  cache_flush_request_i <> u_tlb_plus_cache.cache_flush_request_i

  val tlb_flush_request_i = IO(Flipped(u_tlb_plus_cache.tlb_flush_request_i.cloneType))
  tlb_flush_request_i <> u_tlb_plus_cache.tlb_flush_request_i

  val tlb_flush_reply_o = IO(u_tlb_plus_cache.tlb_flush_reply_o.cloneType)
  tlb_flush_reply_o <> u_tlb_plus_cache.tlb_flush_reply_o

  val stall_request_i = IO(Input(Bool()))
  u_tlb_plus_cache.stall_request_i := stall_request_i

  val tlb_backend_request_o = IO(u_tlb_plus_cache.tlb_backend_request_o.cloneType)
  tlb_backend_request_o <> u_tlb_plus_cache.tlb_backend_request_o

  val tlb_backend_reply_i = IO(Flipped(u_tlb_plus_cache.tlb_backend_reply_i.cloneType))
  tlb_backend_reply_i <> u_tlb_plus_cache.tlb_backend_reply_i

  val cache_backend_request_o = IO(u_tlb_plus_cache.cache_backend_request_o.cloneType)
  cache_backend_request_o <> u_tlb_plus_cache.cache_backend_request_o

  val cache_backend_reply_i = IO(Flipped(u_tlb_plus_cache.cache_backend_reply_i.cloneType))
  cache_backend_reply_i <> u_tlb_plus_cache.cache_backend_reply_i

  val tlb_packet_arrive_o = IO(u_tlb_plus_cache.tlb_packet_arrive_o.cloneType)
  tlb_packet_arrive_o <> u_tlb_plus_cache.tlb_packet_arrive_o

  val tlb_violation_o = IO(u_tlb_plus_cache.tlb_violation_o.cloneType)
  tlb_violation_o <> u_tlb_plus_cache.tlb_violation_o

  val cache_packet_arrive_o = IO(u_tlb_plus_cache.cache_packet_arrive_o.cloneType)
  cache_packet_arrive_o <> u_tlb_plus_cache.cache_packet_arrive_o
}

/**
 * This module combines the Instruction path and Data path together. 
 * Nothing special, only a wrapper :)
 */ 
class NearPipelineMemorySystem(
  param: MemorySystemParameter
) extends MultiIOModule {
  val u_inst = Module(new InstructionMemorySystem(param))
  // ports of instruction path
  val icache_backend_reply_i = IO(Flipped(u_inst.cache_backend_reply_i.cloneType))
  u_inst.cache_backend_reply_i <> icache_backend_reply_i
  val icache_backend_request_o = IO(u_inst.cache_backend_request_o.cloneType)
  u_inst.cache_backend_request_o <> icache_backend_request_o
  val icache_flush_request_i = IO(u_inst.cache_flush_request_i.cloneType)
  u_inst.cache_flush_request_i <> icache_flush_request_i
  val icache_packet_arrive_o = IO(u_inst.cache_packet_arrive_o.cloneType)
  u_inst.cache_packet_arrive_o <> icache_packet_arrive_o
  val ifrontend_reply_o = IO(u_inst.frontend_reply_o.cloneType)
  u_inst.frontend_reply_o <> ifrontend_reply_o
  val ifrontend_request_i = IO(Flipped(u_inst.frontend_request_i.cloneType))
  u_inst.frontend_request_i <> ifrontend_request_i
  u_inst.stall_request_i := false.B
  val itlb_backend_reply_i = IO(Flipped(u_inst.tlb_backend_reply_i.cloneType))
  u_inst.tlb_backend_reply_i <> itlb_backend_reply_i
  val itlb_backend_request_o = IO(u_inst.tlb_backend_request_o.cloneType)
  u_inst.tlb_backend_request_o <> itlb_backend_request_o
  val itlb_flush_request_i = IO(Flipped(u_inst.tlb_flush_request_i.cloneType))
  u_inst.tlb_flush_request_i <> itlb_flush_request_i
  val itlb_packet_arrive_o = IO(u_inst.tlb_packet_arrive_o.cloneType)
  u_inst.tlb_packet_arrive_o <> itlb_packet_arrive_o
  val itlb_violation_o = IO(u_inst.tlb_violation_o.cloneType)
  u_inst.tlb_violation_o <> itlb_violation_o

  val u_data = Module(new DataMemorySystem(param))
  // ports of the data path
  val dcache_backend_reply_i = IO(Flipped(u_data.cache_backend_reply_i.cloneType))
  u_data.cache_backend_reply_i <> dcache_backend_reply_i
  val dcache_backend_request_o = IO(u_data.cache_backend_request_o.cloneType)
  u_data.cache_backend_request_o <> dcache_backend_request_o
  val dcache_flush_request_i = IO(Flipped(u_data.cache_flush_request_i.cloneType))
  u_data.cache_flush_request_i <> dcache_flush_request_i
  val dcache_packet_arrive_o = IO(u_data.cache_packet_arrive_o.cloneType)
  u_data.cache_packet_arrive_o <> dcache_packet_arrive_o
  val dfrontend_reply_o = IO(u_data.frontend_reply_o.cloneType)
  u_data.frontend_reply_o <> dfrontend_reply_o
  val dfrontend_request_i = IO(Flipped(u_data.frontend_request_i.cloneType))
  u_data.frontend_request_i <> dfrontend_request_i
  u_data.stall_request_i := false.B
  val dtlb_backend_reply_i = IO(Flipped(u_data.tlb_backend_reply_i.cloneType))
  u_data.tlb_backend_reply_i <> dtlb_backend_reply_i
  val dtlb_backend_request_o = IO(u_data.tlb_backend_request_o.cloneType)
  u_data.tlb_backend_request_o <> dtlb_backend_request_o
  val dtlb_flush_request_i = IO(Flipped(u_data.tlb_flush_request_i.cloneType))
  u_data.tlb_flush_request_i <> dtlb_flush_request_i
  val dtlb_packet_arrive_o = IO(u_data.tlb_packet_arrive_o.cloneType)
  u_data.tlb_packet_arrive_o <> dtlb_packet_arrive_o
  val dtlb_violation_o = IO(u_data.tlb_violation_o.cloneType)
  u_data.tlb_violation_o <> dtlb_violation_o
}
