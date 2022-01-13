package armflex_cache

import armflex.util.{AXIReadMasterIF, AXIWriteMasterIF}
import chisel3._
import chisel3.util._

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
}