import chisel3._
import chisel3.util._
import armflex._
import armflex_cache._
import armflex_mmu.{MMU, MMUParameter}
import armflex.util._

import antmicro.Bus._

class ARMFlexTop(
  forRTLSimulator: Boolean = true
) extends MultiIOModule {
  implicit val pipelineCfg = new ProcConfig(NB_THREADS = 8)
  val memoryParameter = new MemorySystemParameter(
    // FIXME: Remember to make it into 36 before actually exporting to the hardware for FPGA
    pAddressWidth = if(forRTLSimulator) 24 else 34, // For scaling down
    threadNumber = pipelineCfg.NB_THREADS,
    tlbWayNumber = 2
  )
  val pdParam = new MMUParameter(memoryParameter)

  val u_pipeline = Module(new PipelineAxi)

  // val S_AXIL_TRANSPLANT = IO(Flipped(u_pipeline.transplantIO.ctl.cloneType))
  // S_AXIL_TRANSPLANT <> u_pipeline.transplantIO.ctl

  val u_inst_path = Module(new TLBPlusCache(
    memoryParameter,
    () => new MatrixLRUCore(memoryParameter.cacheWayNumber),
    () => new PseudoTreeLRUCore(memoryParameter.tlbWayNumber)
  ))

//  u_pipeline.mem.inst.req <> u_inst_path.frontend_request_i
//  u_pipeline.mem.inst.resp <> u_inst_path.frontend_reply_o
//
//  u_pipeline.mem.wake(0).bits := u_inst_path.tlb_packet_arrive_o.bits
//  u_pipeline.mem.wake(0).valid := u_inst_path.tlb_packet_arrive_o.valid
//
//  u_pipeline.mem.wake(1).bits := u_inst_path.cache_packet_arrive_o.bits
//  u_pipeline.mem.wake(1).valid := u_inst_path.cache_packet_arrive_o.valid
//
//  u_pipeline.mem.instFault.bits := u_inst_path.tlb_violation_o.bits
//  u_pipeline.mem.instFault.valid := u_inst_path.tlb_violation_o.valid

  // val u_inst_axi = Module(new CacheBackendToAXIInterface.CacheBackendAXIAdaptors(memoryParameter, pipelineCfg.NB_THREADS))
  
  // u_inst_axi.cache_backend_reply_o <> u_inst_path.cache_backend_reply_i
  // u_inst_axi.cache_backend_request_i <> u_inst_path.cache_backend_request_o // TODO: Add FIFO here to buffer the backend request if timing is not good then.

  val u_data_path = Module(new TLBPlusCache(
    memoryParameter,
    () => new MatrixLRUCore(memoryParameter.cacheWayNumber),
    () => new PseudoTreeLRUCore(memoryParameter.tlbWayNumber)
  ))

//  u_pipeline.mem.data.req <> u_data_path.frontend_request_i
//  u_pipeline.mem.data.resp <> u_data_path.frontend_reply_o
//
//  u_pipeline.mem.wake(2).bits := u_data_path.tlb_packet_arrive_o.bits
//  u_pipeline.mem.wake(2).valid := u_data_path.tlb_packet_arrive_o.valid
//
//  u_pipeline.mem.wake(3).bits := u_data_path.cache_packet_arrive_o.bits
//  u_pipeline.mem.wake(3).valid := u_data_path.cache_packet_arrive_o.valid
//
//  u_pipeline.mem.dataFault := u_data_path.tlb_violation_o
//  u_pipeline.mem.dataFault.valid := u_data_path.tlb_violation_o.valid

  // val u_data_axi = Module(new CacheBackendToAXIInterface.CacheBackendAXIAdaptors(memoryParameter, pipelineCfg.NB_THREADS * 2))
  
  // u_data_axi.cache_backend_reply_o <> u_data_path.cache_backend_reply_i
  // u_data_axi.cache_backend_request_i <> u_data_path.cache_backend_request_o

  val u_pd = Module(new MMU(pdParam, 2))
  // ports of the page demander
  val S_AXI = IO(Flipped(u_pd.S_AXI.cloneType))
  u_pd.S_AXI <> S_AXI
  
  //val S_AXIL_TT = IO(u_pd.S_AXIL_TT.cloneType)
  // u_pd.S_AXIL_TT <> S_AXIL_TT
  // val S_AXIL_QEMU_MQ = IO(u_pd.S_AXIL_QEMU_MQ.cloneType)
  // S_AXIL_QEMU_MQ <> u_pd.S_AXIL_QEMU_MQ

//  // u_pd.dcache_flush_request_o
//  u_pd.dcache_flush_request_o <> u_data_path.cache_flush_request_i
//  // u_pd.dcache_stall_request_vo
//  u_pd.dcache_stall_request_vo <> u_data_path.stall_request_i
//  // u_pd.dcache_wb_queue_empty_i
//  u_pd.dcache_wb_queue_empty_i <> u_data_axi.pending_queue_empty_o
//
//  // u_pd.dtlb_backend_reply_o
//  u_pd.dtlb_backend_reply_o <> u_data_path.tlb_backend_reply_i
//  // u_pd.dtlb_backend_request_i
//  u_pd.dtlb_backend_request_i <> u_data_path.tlb_backend_request_o
//  // u_pd.dtlb_flush_reply_i
//  u_pd.dtlb_flush_reply_i := u_data_path.tlb_flush_reply_o
//  // u_pd.dtlb_flush_request_o
//  u_pd.dtlb_flush_request_o <> u_data_path.tlb_flush_request_i
//
//  // u_pd.icache_flush_request_o
//  u_pd.icache_flush_request_o <> u_inst_path.cache_flush_request_i
//  // u_pd.icache_stall_request_vo
//  u_pd.icache_stall_request_vo <> u_inst_path.stall_request_i
//  // u_pd.icache_wb_queue_empty_i
//  u_pd.icache_wb_queue_empty_i <> u_inst_axi.pending_queue_empty_o
//
//  // u_pd.itlb_backend_reply_o
//  u_pd.itlb_backend_reply_o <> u_inst_path.tlb_backend_reply_i
//  // u_pd.itlb_backend_request_i
//  u_pd.itlb_backend_request_i <> u_inst_path.tlb_backend_request_o
//  // u_pd.itlb_flush_reply_i
//  u_pd.itlb_flush_reply_i <> u_inst_path.tlb_flush_reply_o
//  // u_pd.itlb_flush_request_o
//  u_pd.itlb_flush_request_o <> u_inst_path.tlb_flush_request_i

  val M_AXI = IO(new AXI4(
    pdParam.dramAddrWidth, 
    pdParam.dramDataWidth
  ))

  val u_axi_read = Module(new AXIReadMultiplexer(
    pdParam.dramAddrWidth,
    pdParam.dramDataWidth,
    8
  ))

  for(i <- 0 until 6) u_axi_read.S_IF(i) <> u_pd.M_DMA_R(i)
//  u_axi_read.S_IF(6) <> u_inst_axi.M_DMA_R
//  u_axi_read.S_IF(7) <> u_data_axi.M_DMA_R

  M_AXI.ar <> u_axi_read.M_AXI.ar
  M_AXI.r <> u_axi_read.M_AXI.r
  u_axi_read.M_AXI.aw <> AXI4AW.stub(pdParam.dramAddrWidth)
  u_axi_read.M_AXI.w <> AXI4W.stub(pdParam.dramDataWidth)
  u_axi_read.M_AXI.b <> AXI4B.stub()

  val u_axi_write = Module(new AXIWriteMultiplexer(
    pdParam.dramAddrWidth,
    pdParam.dramDataWidth,
    7
  ))

  for(i <- 0 until 5) u_axi_write.S_IF(i) <> u_pd.M_DMA_W(i)
//  u_axi_write.S_IF(5) <> u_inst_axi.M_DMA_W
//  u_axi_write.S_IF(6) <> u_data_axi.M_DMA_W

  M_AXI.aw <> u_axi_write.M_AXI.aw
  M_AXI.w <> u_axi_write.M_AXI.w
  M_AXI.b <> u_axi_write.M_AXI.b

  u_axi_write.M_AXI.ar <> AXI4AR.stub(pdParam.dramAddrWidth)
  u_axi_write.M_AXI.r <> AXI4R.stub(pdParam.dramDataWidth)

  val u_axil_inter = Module(new AXILInterconnector(
    Seq(0x0000, 0x8000), Seq(0x8000, 0x8000), 32, 32
  ))

  val S_AXIL = IO(Flipped(u_axil_inter.S_AXIL.cloneType))
  S_AXIL <> u_axil_inter.S_AXIL

  u_axil_inter.M_AXIL(0) <> u_pipeline.S_AXI
  u_axil_inter.M_AXIL(1) <> u_pd.S_AXIL_QEMU_MQ
}

object ARMFlexTopVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  import java.io._
  val fr = new FileWriter(new File("ARMFlexTop_AWS.v"))
  fr.write(c.emitVerilog(new ARMFlexTop(false)))
  fr.close()
}

// This module is wraps all Pipeline ports with a AxiLite register
class PipelineAxiHacked(implicit val cfg: ProcConfig) extends MultiIOModule {
  import antmicro.CSR._
  import antmicro.Bus._
  val axiDataWidth = 32
  val regCount = 2
  val pipeline = Module(new PipelineWithTransplant)
  val axiLiteCSR = Module(new AXI4LiteCSR(axiDataWidth, 40))
  val csr = Module(new CSR(axiDataWidth, 40))
  csr.io.bus <> axiLiteCSR.io.bus

  val S_AXIL_TRANSPLANT = IO(Flipped(axiLiteCSR.io.ctl.cloneType))
  S_AXIL_TRANSPLANT <> axiLiteCSR.io.ctl

  val transplant_ram = IO(Flipped(pipeline.hostIO.port.cloneType))
  transplant_ram <> pipeline.hostIO.port

  val trans2host = WireInit(Mux(pipeline.hostIO.trans2host.done.valid, 1.U << pipeline.hostIO.trans2host.done.tag, 0.U))
  val host2transClear = WireInit(Mux(pipeline.hostIO.trans2host.clear.valid, 1.U << pipeline.hostIO.trans2host.clear.tag, 0.U))

  SetCSR(trans2host.asUInt(), csr.io.csr(0), axiDataWidth)
  val pendingHostTrans = ClearCSR(host2transClear.asUInt(), csr.io.csr(1), axiDataWidth)
  pipeline.hostIO.host2trans.pending := pendingHostTrans

  // Hacked port
  pipeline.mem.dataFault := SimpleCSR(csr.io.csr(2), axiDataWidth).asTypeOf(pipeline.mem.dataFault)
  pipeline.mem.instFault := SimpleCSR(csr.io.csr(3), axiDataWidth).asTypeOf(pipeline.mem.instFault)
  val handshakeReg = WireInit(SimpleCSR(csr.io.csr(4), axiDataWidth))
  pipeline.mem.inst.req.ready := handshakeReg(0)
  pipeline.mem.inst.resp.valid := handshakeReg(1)
  pipeline.mem.data.req.ready := handshakeReg(2)
  pipeline.mem.data.resp.valid := handshakeReg(3)
  pipeline.mem.wake.zipWithIndex.foreach {
    case (bits, idx) => 
      bits.valid := handshakeReg(3+idx)
      bits.tag := SimpleCSR(csr.io.csr(4+idx), axiDataWidth)
  }

  val currIdx = 4 + pipeline.mem.wake.length
  val regsPerBlock = cfg.BLOCK_SIZE/axiDataWidth
  pipeline.mem.inst.resp.bits := Cat(for (idx <- 0 until regsPerBlock) yield SimpleCSR(csr.io.csr(currIdx+idx), axiDataWidth)).asTypeOf(pipeline.mem.inst.resp.bits)
  pipeline.mem.data.resp.bits := Cat(for (idx <- 0 until regsPerBlock) yield SimpleCSR(csr.io.csr(currIdx+regsPerBlock+idx), axiDataWidth)).asTypeOf(pipeline.mem.data.resp.bits)
 
}

object PipelineFakeTopVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  import java.io._
  val fr = new FileWriter(new File("Pipeline.v"))
  val proc = new ProcConfig(NB_THREADS = 16)
  fr.write(c.emitVerilog(new PipelineAxiHacked()(proc)))
  fr.close()
}
