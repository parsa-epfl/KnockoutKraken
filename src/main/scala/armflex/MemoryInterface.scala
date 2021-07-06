package armflex

import chisel3._
import chisel3.util._

import arm._
import arm.DECODE_CONTROL_SIGNALS._

import armflex.util._



object CacheInterfaceAdaptors {

  /** Generate Cache block and writing mask according to the data size and address bias
    * @param blockSize the size of a cache block. For instance, 512 mean 512 bit
    * @param data the data from the register file. 64bit with effective data starting from 0. For instance 0x000000000000000F for a byte
    * @param size the size of the operand in the @param data. Mask will be generated according to this parameter. For instance SIZEB means a byte
    * @param addrBias the address to shift the @param data to the correct position. Usually the lower 6 bits (512-bit cache block) of the address
    * @return the Pair (shifted data, mask)
    */
  private def shiftData(blockSize: Int, data: UInt, size: UInt, addrBias: UInt): (UInt, UInt) = { // (data, mask)
    val accessMaskingInByte = MuxLookup(
      size,
      0.U,
      Array(
        SIZEB -> 1.U((PROCESSOR_TYPES.DATA_SZ / 8).W),
        SIZEH -> 3.U((PROCESSOR_TYPES.DATA_SZ / 8).W),
        SIZE32 -> 15.U((PROCESSOR_TYPES.DATA_SZ / 8).W),
        SIZE64 -> Fill((PROCESSOR_TYPES.DATA_SZ / 8), true.B)
      )
    )
    val accessMaskingInBit = VecInit(accessMaskingInByte.asBools().map(Fill(8, _))).asUInt()
    assert(data.getWidth == PROCESSOR_TYPES.DATA_SZ)
    val res = WireInit(UInt(blockSize.W), data) // apply mask before logic extending to 512bit.
    return (
      (res << (addrBias << 3).asUInt()).asUInt(),
      (accessMaskingInBit << (addrBias << 3).asUInt()).asUInt()
    )
  }

  /** Recover the data from a cache block (512bit) and shift it in the lowest position.
    * @param blockSize the size of a cache block (512bit)
    * @param data a cache line
    * @param size the mask of required operand
    * @param addrBias the lower 6 bits of the address. It defines data's position in the cache block as well as determines how many bits to shift in order to recover the data.
    * @return the recovered data
    */
  private def recoverData(blockSize: Int, data: UInt, size: UInt, addrBias: UInt): UInt = {
    val accessMaskingInByte = MuxLookup(
      size,
      0.U,
      Array(
        SIZEB -> 1.U((PROCESSOR_TYPES.DATA_SZ / 8).W),
        SIZEH -> 3.U((PROCESSOR_TYPES.DATA_SZ / 8).W),
        SIZE32 -> 15.U((PROCESSOR_TYPES.DATA_SZ / 8).W),
        SIZE64 -> Fill((PROCESSOR_TYPES.DATA_SZ / 8), true.B)
      )
    )
    val accessMaskingInBit = VecInit(accessMaskingInByte.asBools().map(Fill(8, _))).asUInt()
    assert(data.getWidth == blockSize)
    return (data >> (addrBias << 3).asUInt).asUInt() & accessMaskingInBit
  }

  /** The handshake packet between request adaptor and reply adaptor.
    * @param blockSizeInBit number of bits for a cache block
    * @param threadIDWidth width of thread ID
    */
  class CacheInterfaceHandshakePacket(
    blockSizeInBit: Int,
    threadIDWidth: Int
  ) extends Bundle {
    val bias_addr = UInt(log2Ceil(blockSizeInBit / 8).W)
    val size = UInt(2.W)
    val threadID = UInt(threadIDWidth.W)
    val pair_v = Bool()
    val order = UInt(1.W) // 0: 1:

    override def cloneType: this.type = new CacheInterfaceHandshakePacket(blockSizeInBit, threadIDWidth).asInstanceOf[this.type]
  }

  /**
   * Adaptor converting the Pipeline request to the TLB/Cache request.
   * Add 1 cycle latency to the data path
   * @param threadIDWidth: the width of bits to represent thread. (e.g. 2 for threads)
   * @param vAddrW: the width of the virtual address (e.g. 64bit)
   * @param blockSizeInBit the block size in bits. (e.g. 512bit)
  */
  class CacheRequestAdaptor(
    threadIDWidth: Int,
    vAddrW: Int,
    blockSizeInBit: Int
  ) extends MultiIOModule {

    import armflex.MInstTag
    // val i = IO(Flipped(Decoupled(new MInst)))

    val i = IO(Flipped(Decoupled(new MInstTag(UInt(threadIDWidth.W)))))
    val o = IO(Decoupled(new PipelineMemoryRequestPacket(
      vAddrW,
      threadIDWidth,
      blockSizeInBit
    )))

    class internal_memory_request_t extends Bundle {
      val size = UInt(2.W)
      val vaddr = UInt(vAddrW.W)
      val threadID = UInt(threadIDWidth.W)
      val w_v = Bool()
      val w_data = PROCESSOR_TYPES.DATA_T
      val pair_order = UInt(1.W)
      val pair_v = Bool()
    }

    val from_pipeline = Wire(Decoupled(new internal_memory_request_t))
    from_pipeline.bits.size := i.bits.size
    from_pipeline.bits.vaddr := i.bits.memReq(0).addr
    from_pipeline.bits.threadID := i.bits.tag
    from_pipeline.bits.w_v := !i.bits.isLoad
    from_pipeline.bits.w_data := i.bits.memReq(0).data
    from_pipeline.bits.pair_order := 0.U // the first request
    from_pipeline.bits.pair_v := i.bits.isPair
    from_pipeline.valid := i.valid
    i.ready := from_pipeline.ready

    val s1_pair_context_n = Wire(Decoupled(new internal_memory_request_t))
    s1_pair_context_n.bits.size := i.bits.size
    s1_pair_context_n.bits.threadID := i.bits.tag
    s1_pair_context_n.bits.vaddr := i.bits.memReq(1).addr
    s1_pair_context_n.bits.w_v := !i.bits.isLoad
    s1_pair_context_n.bits.w_data := i.bits.memReq(1).addr
    s1_pair_context_n.bits.pair_order := 1.U // the second request
    s1_pair_context_n.bits.pair_v := true.B
    s1_pair_context_n.valid := from_pipeline.fire() && i.bits.isPair
    val s1_pair_context_r = FlushQueue(s1_pair_context_n, 1, false)

    val u_arb = Module(new Arbiter(new internal_memory_request_t, 2))
    u_arb.io.in(0) <> s1_pair_context_r
    u_arb.io.in(1) <> from_pipeline
    // shift shift shift!

    val shifted = shiftData(
      blockSizeInBit,
      u_arb.io.out.bits.w_data,
      u_arb.io.out.bits.size,
      u_arb.io.out.bits.vaddr(
        log2Ceil(blockSizeInBit / 8) - 1,
        0
      )
    )

    o.bits.addr := u_arb.io.out.bits.vaddr
    o.bits.thread_id := u_arb.io.out.bits.threadID
    o.bits.wData := shifted._1
    o.bits.wMask := shifted._2
    o.bits.permission := Mux(u_arb.io.out.bits.w_v, 1.U, 0.U)

    o.valid := u_arb.io.out.valid
    u_arb.io.out.ready := o.ready

    // a message to the reply arbiter for synchronization
    val sync_message = Wire(Decoupled(new CacheInterfaceHandshakePacket(blockSizeInBit, threadIDWidth)))
    sync_message.bits.size := u_arb.io.out.bits.size
    sync_message.bits.bias_addr := u_arb.io.out.bits.vaddr(
      log2Ceil(blockSizeInBit / 8) - 1,
      0
    )
    sync_message.bits.order := u_arb.io.out.bits.pair_order
    sync_message.bits.pair_v := u_arb.io.out.bits.pair_v
    sync_message.bits.threadID := u_arb.io.out.bits.threadID
    sync_message.valid := o.fire()
    assert(sync_message.ready, "It's impossible that the FIFO of sync_message is full")

    val sync_message_o = IO(Decoupled(new CacheInterfaceHandshakePacket(blockSizeInBit, threadIDWidth)))
    sync_message_o <> sync_message
  }

// The purpose of this module is to apply masking and shifting to the data so that it's in the correct position.
  class CacheReplyAdaptor(
    threadIDWidth: Int,
    vAddrW: Int,
    blockSizeInBit: Int
  ) extends MultiIOModule {
    val cache_reply_i = IO(Flipped(Valid(new PipelineMemoryReplyPacket(blockSizeInBit, threadIDWidth))))
    // val data_o = IO(Valid(Vec(2, PROCESSOR_TYPES.DATA_T)))
    val data_o = IO(Valid(new PipelineMemoryReplyPacket(blockSizeInBit, threadIDWidth)))
    val sync_message_i = IO(Flipped(Decoupled(new CacheInterfaceHandshakePacket(blockSizeInBit, threadIDWidth))))
    // sync_message_i should align with cache_reply_i
    when(cache_reply_i.valid){
      assert(sync_message_i.valid, "sync_message_i should align with cache_reply_i.")
    }
    sync_message_i.ready := true.B //?

    when(cache_reply_i.valid) {
      assert(cache_reply_i.bits.thread_id === sync_message_i.bits.threadID)
    }

    val recovered_data = recoverData(
      blockSizeInBit,
      cache_reply_i.bits.data,
      sync_message_i.bits.size,
      sync_message_i.bits.bias_addr
    )

    data_o.bits.data := recovered_data
    // data_o.bits.dirty := cache_reply_i.bits.dirty
    data_o.bits.hit := cache_reply_i.bits.hit
    data_o.bits.thread_id := cache_reply_i.bits.thread_id
    data_o.valid := cache_reply_i.valid
  }
}

object PipeTLB {
  class PipeTLBRequest(
    val vaddrWidth: Int,
    val thidWidth: Int,
    val asidWidth: Int
  ) extends Bundle {
    val addr = UInt(vaddrWidth.W)
    val thid = UInt(thidWidth.W)
    val asid = UInt(asidWidth.W)
    val perm = UInt(2.W)
  }

  class PipeTLBResponse(
    val paddrWidth: Int
  ) extends Bundle {
    val addr = UInt(paddrWidth.W)
    val hit = Bool()
    val miss = Bool()
    val permFault = Bool()
  }

  class PipeTLBIO(
    val vaddrWidth: Int,
    val paddrWidth: Int,
    val thidWidth: Int,
    val asidWidth: Int
  ) extends Bundle {
    val req = Decoupled(new PipeTLBRequest(vaddrWidth, thidWidth, asidWidth))
    val resp = Flipped(Decoupled(new PipeTLBRequest(paddrWidth, thidWidth, asidWidth)))
  }
}

object PipeCache {
  class CacheRequest(
    val paddrWidth: Int,
    val blockWidth: Int
  ) extends Bundle {
    val addr = UInt(paddrWidth.W)
    val data = UInt(blockWidth.W)
    val w_en = UInt((blockWidth/8).W)
  }

  class CacheResponse(
    val blockWidth: Int
  ) extends Bundle {
    val data = UInt(blockWidth.W)
    val hit = Bool()
    val miss = Bool()
    val miss2hit = Bool()
  }

  class PipeCacheIO[MetaT <: Data](
    val paddrWidth: Int,
    val blockSize: Int
  ) extends Bundle {
    val req = Decoupled(new CacheRequest(paddrWidth, blockSize))
    val resp = Flipped(Decoupled(new CacheResponse(blockSize)))
  }

  class PipeCacheRequest[MetaT <: Data](
    val gen: MetaT,
    val paddrWidth: Int,
    val blockWidth: Int
  ) extends Bundle {
    val port = Decoupled(new CacheRequest(paddrWidth, blockWidth))
    val meta = Input(gen.cloneType)
  }

  class PipeCacheResponse[MetaT <: Data](
    val gen: MetaT,
    val blockSize: Int
  ) extends Bundle {
    val port = Flipped(Decoupled(new CacheResponse(blockSize)))
    val meta = Output(gen.cloneType)
  }

  class PipeCacheInterfaceIO[MetaT <: Data](
    val genMeta: MetaT,
    val paddrWidth: Int,
    val blockSize: Int
  ) extends Bundle {
    val req = new PipeCacheRequest(genMeta, paddrWidth, blockSize)
    val resp = new PipeCacheResponse(genMeta, blockSize)
  }
}

class PipeMemPortIO(
  val vaddrWidth: Int,
  val paddrWidth: Int,
  val thidWidth: Int,
  val asidWidth: Int,
  val blockSize: Int
) extends Bundle {
  val tlb = new PipeTLB.PipeTLBIO(vaddrWidth, paddrWidth, thidWidth, asidWidth)
  val cache = new PipeCache.PipeCacheIO(paddrWidth, blockSize)
}

