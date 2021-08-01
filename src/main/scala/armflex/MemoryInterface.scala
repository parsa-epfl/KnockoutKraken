package armflex

import chisel3._
import chisel3.util._

import armflex.util.{DoubleLatencyQueue}

object PipeTLB {
  class PipeTLBRequest(
    val vAddrW: Int,
    val thidW: Int,
    val asidW: Int
  ) extends Bundle {
    val addr = UInt(vAddrW.W)
    val thid = UInt(thidW.W)
    val asid = UInt(asidW.W)
    val perm = UInt(2.W) // See ProcTypes.scala
  }

  class PipeTLBResponse(
    val pAddrW: Int
  ) extends Bundle {
    val addr = UInt(pAddrW.W)
    val hit = Bool()
    val miss = Bool()
    val violation = Bool()
  }

  class PipeTLBIO(
    val vAddrW: Int,
    val pAddrW: Int,
    val thidW: Int,
    val asidW: Int
  ) extends Bundle {
    val req = Decoupled(new PipeTLBRequest(vAddrW, thidW, asidW))
    val resp = Flipped(Decoupled(new PipeTLBResponse(pAddrW)))
  }
}

object PipeCache {
  class CacheRequest(
    val pAddrW: Int,
    val blockW: Int
  ) extends Bundle {
    val addr = UInt(pAddrW.W)
    val data = UInt(blockW.W)
    val w_en = UInt((blockW/8).W)
  }

  class CacheResponse(
    val blockW: Int
  ) extends Bundle {
    val data = UInt(blockW.W)
    val hit = Bool()
    val miss = Bool()
    val miss2hit = Bool()
  }

  class PipeCacheIO[MetaT <: Data](
    val pAddrW: Int,
    val blockSize: Int
  ) extends Bundle {
    val req = Decoupled(new CacheRequest(pAddrW, blockSize))
    val resp = Flipped(Decoupled(new CacheResponse(blockSize)))
  }

  class PipeCacheRequest[MetaT <: Data](
    gen: MetaT,
    val pAddrW: Int,
    val blockW: Int
  ) extends Bundle {
    val port = Flipped(Decoupled(new CacheRequest(pAddrW, blockW)))
    val meta = Input(gen)
    override def cloneType: this.type = new PipeCacheRequest[MetaT](gen, pAddrW, blockW).asInstanceOf[this.type]
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
    val pAddrW: Int,
    val blockSize: Int
  ) extends Bundle {
    val req = new PipeCacheRequest(genMeta.cloneType, pAddrW, blockSize)
    val resp = new PipeCacheResponse(genMeta.cloneType, blockSize)
    override def cloneType: this.type = new PipeCacheInterfaceIO[MetaT](genMeta, pAddrW, blockSize).asInstanceOf[this.type]
  }

  /* For Cache (not TLB), this module handles miss delays by using a double Queueing system
   * All transactions should be certified to complete
   */
  // TODO: Short path should allocate less entries, close to the latency of a hit request
  class CacheInterface[MetaT <: Data](
    genMeta: MetaT,
    val maxInstsInFlight: Int,
    val pAddrW: Int,
    val blockSize: Int
  ) extends MultiIOModule {
    val pipe_io = IO(new PipeCacheInterfaceIO(genMeta, pAddrW, blockSize))
    val cache_io = IO(new PipeCacheIO(pAddrW, blockSize))
    val pending = IO(Output(UInt(log2Ceil(maxInstsInFlight).W)))

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

    // ---- Track Pending Requests ----
    pending := metaQ.pending

  }

  /**
   * This module ensures that all pending cache requests are completed before allowing for flushing
   */
  class CacheFlushingController extends MultiIOModule {
    val mmu_io = IO(new PipeMMUPortIO)
    val ctrl = IO(new Bundle {
      val hasPendingWork = Input(Bool())
      val stopTransactions = Output(Bool())
      val waitingForMMU = Output(Bool())
    })
    private val sFlush_idle :: sFlush_completePending :: sFlush_waitComplete :: Nil = Enum(3)
    private val flushState = RegInit(sFlush_idle)
    mmu_io.flushPermReq.ready := false.B
    mmu_io.flushCompled.ready := false.B
    switch(flushState) {
      is(sFlush_idle) {
        when(mmu_io.flushPermReq.valid) {
          flushState := sFlush_completePending
        }
      }
      is(sFlush_completePending) {
        // Stop translating new requests
        when(!ctrl.hasPendingWork) {
          mmu_io.flushPermReq.ready := true.B
          when(mmu_io.flushPermReq.fire) {
            flushState := sFlush_waitComplete
          }
        }
      }
      is(sFlush_waitComplete) {
        // Stop translating new requests
        mmu_io.flushCompled.ready := true.B
        when(mmu_io.flushCompled.fire) {
          flushState := sFlush_idle
        }
      }
    }

    ctrl.waitingForMMU := flushState === sFlush_waitComplete
    ctrl.stopTransactions := flushState === sFlush_completePending || flushState === sFlush_waitComplete
  }
}

class PipeMemPortIO(
  val vAddrW: Int,
  val pAddrW: Int,
  val thidW: Int,
  val asidW: Int,
  val blockSize: Int
) extends Bundle {
  val tlb = new PipeTLB.PipeTLBIO(vAddrW, pAddrW, thidW, asidW)
  val cache = new PipeCache.PipeCacheIO(pAddrW, blockSize)
}

class PipeMMUPortIO extends Bundle {
  val flushPermReq = Flipped(Decoupled())
  val flushCompled = Flipped(Decoupled())
}

class PipeMMUIO extends Bundle {
  val data = new PipeMMUPortIO
  val inst = new PipeMMUPortIO
}
