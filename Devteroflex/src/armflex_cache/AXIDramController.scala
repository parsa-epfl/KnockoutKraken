package armflex_cache

import armflex.util.{AXIReadMasterIF, AXIWriteMasterIF}
import chisel3._
import chisel3.util._

import armflex_pmu.CycleCountingPort
import armflex.util.ReadPort
import armflex.util.WritePort

class Cache2AXIAdaptor(params: DatabankParams, queueSize: Int) extends Module {
  assert(params.blockSize == 512, "Only 512bit AXI transactions is supported.")
  //assert(params.pAddressWidth == 36, "Only 36bit memory addres is supported.")

  // AXI AWS Shell DMA
  val M_DMA_R = IO(new AXIReadMasterIF(params.addrW + log2Ceil(params.blockSize / 8), params.blockSize))
  val M_DMA_W = IO(new AXIWriteMasterIF(params.addrW + log2Ceil(params.blockSize / 8), params.blockSize))

  // Cache
  val cache_io = IO(Flipped(new CacheAxiMemoryIO(params)))
  val mmu_io_pendingQueueEmpty = IO(Output(Bool()))

  val q_cache_backend_request = Queue(cache_io.req, queueSize)
  mmu_io_pendingQueueEmpty := !q_cache_backend_request.valid

  M_DMA_R.req.bits.length := 1.U
  M_DMA_R.req.bits.address := Cat(q_cache_backend_request.bits.addr, Fill(log2Ceil(params.blockSize / 8), 0.U))
  M_DMA_R.req.valid := !q_cache_backend_request.bits.w_v && q_cache_backend_request.valid

  cache_io.resp.bits := M_DMA_R.data.bits
  cache_io.resp.valid := M_DMA_R.data.valid
  M_DMA_R.data.ready := cache_io.resp.ready


  M_DMA_W.req.bits.address := Cat(q_cache_backend_request.bits.addr, Fill(log2Ceil(params.blockSize / 8), 0.U))
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

  // The port to start a counter to record the latency to resolve a cache miss.
  val oPMUReq = IO(Output(new CycleCountingPort(params.thidN)))
  oPMUReq.start.bits := q_cache_backend_request.bits.thid
  oPMUReq.start.valid := q_cache_backend_request.valid
  oPMUReq.stop.valid := q_cache_backend_request.fire
  oPMUReq.stop.bits := q_cache_backend_request.bits.thid
}

class Cache2DMAAdaptor(params: DatabankParams, queueSize: Int) extends Module {
  assert(params.blockSize == 512, "Only 512bit AXI transactions is supported.")
  //assert(params.pAddressWidth == 36, "Only 36bit memory addres is supported.")

  // AXI AWS Shell DMA
  val M_DMA = IO(new Bundle {
    val rd = Flipped(new ReadPort(params.addrW + log2Ceil(params.blockSize / 8), params.blockSize))
    val wr = Flipped(new WritePort(params.addrW + log2Ceil(params.blockSize / 8), params.blockSize))
  })

  // Cache
  val cache_io = IO(Flipped(new CacheAxiMemoryIO(params)))
  val mmu_io_pendingQueueEmpty = IO(Output(Bool()))

  val sAddr :: sData :: Nil = Enum(2)
  val state_r = RegInit(sAddr)

  val q_cache_backend_request = Queue(cache_io.req, queueSize)
  mmu_io_pendingQueueEmpty := !q_cache_backend_request.valid
  q_cache_backend_request.ready := false.B

  val burstSize = 1 // We only send a single block operation
  val addr_w = WireInit(Cat(q_cache_backend_request.bits.addr, Fill(log2Ceil(params.blockSize / 8), 0.U)))
  val isWr = WireInit(q_cache_backend_request.bits.w_v)
  val isRd = WireInit(!q_cache_backend_request.bits.w_v)

  M_DMA.rd.req.bits.burst := burstSize.U
  M_DMA.rd.req.bits.addr := addr_w
  M_DMA.rd.req.bits.w_en := 0.U
  M_DMA.rd.req.valid := q_cache_backend_request.valid && state_r === sAddr && isRd

  cache_io.resp.bits := M_DMA.rd.data.bits
  cache_io.resp.valid := M_DMA.rd.data.valid
  M_DMA.rd.data.ready := cache_io.resp.ready && state_r === sData

  M_DMA.wr.req.bits.burst := burstSize.U
  M_DMA.wr.req.bits.addr := addr_w
  M_DMA.wr.req.bits.w_en := Fill(M_DMA.wr.req.bits.w_en.getWidth, 1.U)
  M_DMA.wr.req.valid := q_cache_backend_request.valid && state_r === sAddr && isWr

  M_DMA.wr.data.bits := q_cache_backend_request.bits.data
  M_DMA.wr.data.valid := q_cache_backend_request.valid && state_r === sData
  // WARN: data valid should keep high until the packet completed

  switch(state_r) {
    is(sAddr) {
      when(M_DMA.rd.req.fire || M_DMA.wr.req.fire) {
        state_r := sData
      }
    }

    is(sData) {
      when(M_DMA.rd.data.fire || M_DMA.wr.data.fire) {
        state_r := sAddr
        q_cache_backend_request.ready := true.B
      }
    }
  }

  // The port to start a counter to record the latency to resolve a cache miss.
  val oPMUReq = IO(Output(new CycleCountingPort(params.thidN)))
  oPMUReq.start.bits := q_cache_backend_request.bits.thid
  oPMUReq.start.valid := q_cache_backend_request.valid
  oPMUReq.stop.valid := q_cache_backend_request.fire
  oPMUReq.stop.bits := q_cache_backend_request.bits.thid
}