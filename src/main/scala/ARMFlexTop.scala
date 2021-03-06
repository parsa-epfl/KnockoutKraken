package armflex

import chisel3._
import chisel3.util._
import armflex.cache.TLBPlusCache
import armflex.cache.MemorySystemParameter
import armflex.cache.MatrixLRUCore
import armflex.cache.PseudoTreeLRUCore
import armflex.cache.CacheBackendToAXIInterface
import armflex.demander.PageDemander
import armflex.util._
import armflex.demander.software_bundle.ParameterConstants

import DMAController.Bus._

class ARMFlexTop extends MultiIOModule {
  implicit val pipelineCfg = new ProcConfig(NB_THREADS = 8)
  val memoryParameter = new MemorySystemParameter(threadNumber = 8)

  val u_pipeline = Module(new PipelineAxi)

  val S_AXIL_TRANSPLANT = IO(Flipped(u_pipeline.transplantIO.ctl.cloneType))
  S_AXIL_TRANSPLANT <> u_pipeline.transplantIO.ctl

  val S_AXI_TRANSPlANT = IO(Flipped(u_pipeline.transplantIO.port.cloneType))
  S_AXI_TRANSPlANT <> u_pipeline.transplantIO.port

  val u_inst_path = Module(new TLBPlusCache(
    memoryParameter,
    () => new MatrixLRUCore(memoryParameter.cacheWayNumber),
    () => new PseudoTreeLRUCore(memoryParameter.tlbWayNumber)
  ))

  u_pipeline.mem.inst.req <> u_inst_path.frontend_request_i
  u_pipeline.mem.inst.resp <> u_inst_path.frontend_reply_o

  u_pipeline.mem.wake(0).tag := u_inst_path.tlb_packet_arrive_o.bits
  u_pipeline.mem.wake(0).valid := u_inst_path.tlb_packet_arrive_o.valid

  u_pipeline.mem.wake(1).tag := u_inst_path.cache_packet_arrive_o.bits.thread_id
  u_pipeline.mem.wake(1).valid := u_inst_path.cache_packet_arrive_o.valid

  u_pipeline.mem.instFault.tag := u_inst_path.tlb_violation_o.bits
  u_pipeline.mem.instFault.valid := u_inst_path.tlb_violation_o.valid

  val u_inst_axi = Module(new CacheBackendToAXIInterface.CacheBackendAXIAdaptors(memoryParameter))
  
  u_inst_axi.cache_backend_reply_o <> u_inst_path.cache_backend_reply_i
  u_inst_axi.cache_backend_request_i <> u_inst_path.cache_backend_request_o // TODO: Add FIFO here to buffer the backend request if timing is not good then.

  val u_data_path = Module(new TLBPlusCache(
    memoryParameter,
    () => new MatrixLRUCore(memoryParameter.cacheWayNumber),
    () => new PseudoTreeLRUCore(memoryParameter.tlbWayNumber)
  ))

  u_pipeline.mem.data.req <> u_data_path.frontend_request_i
  u_pipeline.mem.data.resp <> u_data_path.frontend_reply_o

  u_pipeline.mem.wake(2).tag := u_data_path.tlb_packet_arrive_o.bits
  u_pipeline.mem.wake(2).valid := u_data_path.tlb_packet_arrive_o.valid

  u_pipeline.mem.wake(3).tag := u_data_path.cache_packet_arrive_o.bits.thread_id
  u_pipeline.mem.wake(3).valid := u_data_path.cache_packet_arrive_o.valid

  u_pipeline.mem.dataFault.tag := u_data_path.tlb_violation_o.bits
  u_pipeline.mem.dataFault.valid := u_data_path.tlb_violation_o.valid

  val u_data_axi = Module(new CacheBackendToAXIInterface.CacheBackendAXIAdaptors(memoryParameter))
  
  u_data_axi.cache_backend_reply_o <> u_data_path.cache_backend_reply_i
  u_data_axi.cache_backend_request_i <> u_data_path.cache_backend_request_o

  val u_pd = Module(new PageDemander(memoryParameter, 2, false))
  // ports of the page demander
  val S_AXI_PAGE = IO(Flipped(u_pd.S_AXI_PAGE.cloneType))
  u_pd.S_AXI_PAGE <> S_AXI_PAGE
  val S_AXIL_TT = IO(u_pd.S_AXI_TT.cloneType)
  u_pd.S_AXI_TT <> S_AXIL_TT
  val S_AXIL_QEMU_MQ = IO(u_pd.S_AXIL_QEMU_MQ.cloneType)
  S_AXIL_QEMU_MQ <> u_pd.S_AXIL_QEMU_MQ
  val S_AXI_QEMU_MQ = IO(Flipped(u_pd.S_AXI_QEMU_MQ.cloneType))
  u_pd.S_AXI_QEMU_MQ <> S_AXI_QEMU_MQ

  // u_pd.dcache_flush_request_o
  u_pd.dcache_flush_request_o <> u_data_path.cache_flush_request_i
  // u_pd.dcache_stall_request_vo
  u_pd.dcache_stall_request_vo <> u_data_path.stall_request_i
  // u_pd.dcache_wb_queue_empty_i
  u_pd.dcache_wb_queue_empty_i <> u_data_axi.pending_queue_empty_o

  // u_pd.dtlb_backend_reply_o
  u_pd.dtlb_backend_reply_o <> u_data_path.tlb_backend_reply_i
  // u_pd.dtlb_backend_request_i
  u_pd.dtlb_backend_request_i <> u_data_path.tlb_backend_request_o
  // u_pd.dtlb_flush_reply_i
  u_pd.dtlb_flush_reply_i := u_data_path.tlb_flush_reply_o
  // u_pd.dtlb_flush_request_o
  u_pd.dtlb_flush_request_o <> u_data_path.tlb_flush_request_i

  // u_pd.icache_flush_request_o
  u_pd.icache_flush_request_o <> u_inst_path.cache_flush_request_i
  // u_pd.icache_stall_request_vo
  u_pd.icache_stall_request_vo <> u_inst_path.stall_request_i
  // u_pd.icache_wb_queue_empty_i
  u_pd.icache_wb_queue_empty_i <> u_inst_axi.pending_queue_empty_o

  // u_pd.itlb_backend_reply_o
  u_pd.itlb_backend_reply_o <> u_inst_path.tlb_backend_reply_i
  // u_pd.itlb_backend_request_i
  u_pd.itlb_backend_request_i <> u_inst_path.tlb_backend_request_o
  // u_pd.itlb_flush_reply_i
  u_pd.itlb_flush_reply_i <> u_inst_path.tlb_flush_reply_o
  // u_pd.itlb_flush_request_o
  u_pd.itlb_flush_request_o <> u_inst_path.tlb_flush_request_i

  val M_AXI = IO(new AXI4(
    ParameterConstants.dram_addr_width, 
    ParameterConstants.dram_data_width
  ))

  val u_axi_read = Module(new AXIReadMultiplexer(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width,
    8
  ))

  for(i <- 0 until 6) u_axi_read.S_IF(i) <> u_pd.M_DMA_R(i)
  u_axi_read.S_IF(6) <> u_inst_axi.M_DMA_R
  u_axi_read.S_IF(7) <> u_data_axi.M_DMA_R

  M_AXI.ar <> u_axi_read.M_AXI.ar
  M_AXI.r <> u_axi_read.M_AXI.r
  u_axi_read.M_AXI.aw <> AXI4AW.stub(ParameterConstants.dram_addr_width)
  u_axi_read.M_AXI.w <> AXI4W.stub(ParameterConstants.dram_data_width)
  u_axi_read.M_AXI.b <> AXI4B.stub()

  val u_axi_write = Module(new AXIWriteMultiplexer(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width,
    6
  ))

  for(i <- 0 until 4) u_axi_write.S_IF(i) <> u_pd.M_DMA_W(i)
  u_axi_write.S_IF(4) <> u_inst_axi.M_DMA_W
  u_axi_write.S_IF(5) <> u_data_axi.M_DMA_W

  M_AXI.aw <> u_axi_write.M_AXI.aw
  M_AXI.w <> u_axi_write.M_AXI.w
  M_AXI.b <> u_axi_write.M_AXI.b

  u_axi_write.M_AXI.ar <> AXI4AR.stub(ParameterConstants.dram_addr_width)
  u_axi_write.M_AXI.r <> AXI4R.stub(ParameterConstants.dram_data_width)
}



object ARMFlexTopVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  import java.io._
  val fr = new FileWriter(new File("ARMFlexTop.v"))
  fr.write(c.emitVerilog(new ARMFlexTop))
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

  SetCSR(trans2host, csr.io.csr(0), axiDataWidth)
  val pendingHostTrans = ClearCSR(host2transClear, csr.io.csr(1), axiDataWidth)
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
