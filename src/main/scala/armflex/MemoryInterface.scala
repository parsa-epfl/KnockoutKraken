package armflex

import chisel3._
import chisel3.util._

import armflex.util.{DoubleLatencyQueue}

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
    val resp = Flipped(Decoupled(new PipeTLBResponse(paddrWidth)))
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
    gen: MetaT,
    val paddrWidth: Int,
    val blockWidth: Int
  ) extends Bundle {
    val port = Flipped(Decoupled(new CacheRequest(paddrWidth, blockWidth)))
    val meta = Input(gen)
    override def cloneType: this.type = new PipeCacheRequest[MetaT](gen, paddrWidth, blockWidth).asInstanceOf[this.type]
  }

  class PipeCacheResponse[MetaT <: Data](
    gen: MetaT,
    val blockSize: Int
  ) extends Bundle {
    val port = Decoupled(new CacheResponse(blockSize))
    val meta = Output(gen)
    override def cloneType: this.type = new PipeCacheResponse[MetaT](gen, blockSize).asInstanceOf[this.type]
  }

  class PipeCacheInterfaceIO[MetaT <: Data](
    genMeta: MetaT,
    val paddrWidth: Int,
    val blockSize: Int
  ) extends Bundle {
    val req = new PipeCacheRequest(genMeta.cloneType, paddrWidth, blockSize)
    val resp = new PipeCacheResponse(genMeta.cloneType, blockSize)
    override def cloneType: this.type = new PipeCacheInterfaceIO[MetaT](genMeta, paddrWidth, blockSize).asInstanceOf[this.type]
  }

  /* For Cache (not TLB), this module handles miss delays by using a double Queueing system
   * All transactions should be certified to complete
   */
  // TODO: Short path should allocate less entries, close to the latency of a hit request
  class CacheInterface[MetaT <: Data](
    genMeta: MetaT,
    val maxInstsInFlight: Int,
    val paddrWidth: Int,
    val blockSize: Int
  ) extends MultiIOModule {
    val pipe_io = IO(new PipeCacheInterfaceIO(genMeta, paddrWidth, blockSize))
    val cache_io = IO(new PipeCacheIO(paddrWidth, blockSize))

    pipe_io.req.port <> cache_io.req
    pipe_io.resp.port <> cache_io.resp

    private val metaQ = Module(new DoubleLatencyQueue(genMeta, maxInstsInFlight))
    private val pipeMetaResp = Mux(metaQ.resp_o.base.valid, metaQ.resp_o.base.bits, metaQ.resp_o.long.bits)
    metaQ.req_i.bits := pipe_io.req.meta
    pipe_io.resp.meta := pipeMetaResp

    private val hit = WireInit(cache_io.resp.bits.hit)
    private val miss = WireInit(cache_io.resp.bits.miss)
    private val miss2hit = WireInit(cache_io.resp.bits.miss2hit)
    metaQ.req_i.valid := pipe_io.req.port.fire
    metaQ.ctrl_i := 0.U.asTypeOf(metaQ.ctrl_i)
    when(cache_io.resp.fire) {
      metaQ.ctrl_i.drop := false.B // All transactions are certified to complete
      metaQ.ctrl_i.done := hit && !miss2hit
      metaQ.ctrl_i.long := miss
      metaQ.ctrl_i.longDone := miss2hit
    }

    // If metaQ is full, wait for misses to complete
    when(!metaQ.req_i.ready) {
      pipe_io.req.port.ready := false.B
      cache_io.req.valid := false.B
    }

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

