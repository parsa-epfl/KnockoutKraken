import antmicro.Bus.{AXI4, AXI4AR, AXI4AW, AXI4B, AXI4R, AXI4W}
import antmicro.CSR._
import chisel3._
import chisel3.util._
import armflex._
import armflex.util.{ AXILInterconnector, AXI4MasterDMACtrl}
import armflex_cache._
import armflex_mmu.{MMU, MemoryHierarchyParams}
import armflex.util.ExtraUtils._
import firrtl.options.TargetDirAnnotation
import instrumentation.{ TraceDump, TraceDumpParams }
import antmicro.Bus.AXI4Lite
import armflex.util._

class ARMFlexTopInstrumented(
  paramsPipeline: PipelineParams,
  paramsMemoryHierarchy: MemoryHierarchyParams
) extends Module {
  import armflex.util.AXIReadMultiplexer
  import armflex.util.AXIWriteMultiplexer
  private val axilMulti = Module(new AXILInterconnectorNonOptimized(Seq(0x00000, 0x30000), 32, 32))
  private val devteroFlexTop = Module(new ARMFlexTop(paramsPipeline, paramsMemoryHierarchy))
  axilMulti.M_AXIL(0) <> devteroFlexTop.S_AXIL
  private val dmaMasterCtrl = Module(new AXI4MasterDMACtrl(3, 4, paramsMemoryHierarchy.dramAddrW, paramsMemoryHierarchy.dramDataW))
  val S_AXI = IO(Flipped(devteroFlexTop.S_AXI.cloneType))
  val S_AXIL = IO(Flipped(devteroFlexTop.S_AXIL.cloneType))
  val M_AXI = IO(new AXI4(paramsMemoryHierarchy.dramAddrW, paramsMemoryHierarchy.dramDataW))

  S_AXI <> devteroFlexTop.S_AXI
  S_AXIL <> axilMulti.S_AXIL
  
  M_AXI <> dmaMasterCtrl.M_AXI
  dmaMasterCtrl.req.wrPorts(0) <> devteroFlexTop.AXI_MEM.AXI_MMU.M_DMA_WR // PageTableAccess
  dmaMasterCtrl.req.rdPorts(0) <> devteroFlexTop.AXI_MEM.AXI_MMU.M_DMA_RD // PageTableAccess
  dmaMasterCtrl.req.wrPorts(1) <> devteroFlexTop.AXI_MEM.M_AXI_DMA_icache.wr
  dmaMasterCtrl.req.rdPorts(1) <> devteroFlexTop.AXI_MEM.M_AXI_DMA_icache.rd
  dmaMasterCtrl.req.wrPorts(2) <> devteroFlexTop.AXI_MEM.M_AXI_DMA_dcache.wr
  dmaMasterCtrl.req.rdPorts(2) <> devteroFlexTop.AXI_MEM.M_AXI_DMA_dcache.rd


  // Instrumentation to store PC
  val traceWrapper = Module(new TraceWrapper(
    paramsMemoryHierarchy.dramAddrW, axilMulti.S_AXIL.addrWidth, 0x1F000))
  traceWrapper.commit.bits <> devteroFlexTop.instrument.commit.bits.pc
  traceWrapper.commit.handshake(devteroFlexTop.instrument.commit)
  axilMulti.M_AXIL(1) <> traceWrapper.S_AXIL
  dmaMasterCtrl.req.wrPorts(3) <> traceWrapper.M_AXI_W
}

class TraceWrapper(val dramAddrW: Int, val axilAddrW: Int, val axilBaseAddr: Int) extends Module {
  val S_AXIL = IO(Flipped(new AXI4Lite(axilAddrW, 32))) 
  val M_AXI_W = IO(new WritePort(dramAddrW, 512, 32))
  val commit = IO(Flipped(Decoupled(UInt(64.W))))

  private val traceDumper = Module(new TraceDump(new TraceDumpParams(dramAddrW, 512, 64, 1024, 256, 32)))
  private val nCSR = 6
  private val uAxilToCSR = Module(new AXI4LiteCSR(32, nCSR))
  private val cfgBusCSR = new CSRBusSlaveConfig(0, nCSR)
  private val uCSR = Module(new CSR(32, nCSR))
  uAxilToCSR.io.ctl <> S_AXIL
  uCSR.io.bus <> uAxilToCSR.io.bus

  private val clearToggle = WireInit(VecInit(0.U(32.W).asBools))
  private val baseAddrCSR = SimpleCSR(uCSR.io.csr(0), 32)
  private val start = ClearCSR(clearToggle.asUInt, uCSR.io.csr(1), 32)

  traceDumper.init_addr.bits := baseAddrCSR
  traceDumper.init_addr.valid := start(0)
  clearToggle(0) := traceDumper.init_addr.ready

  traceDumper.trace_data.bits := commit.bits
  traceDumper.trace_data.handshake(commit)
  when(traceDumper.init_addr.ready) {
    commit.ready := true.B // Hasn't been enabled yet, let it run
  }

  private val cntExecute = Module(new PerfCounter(32))
  private val cntStalls = Module(new PerfCounter(32))
  StatusCSR(cntExecute.io.count, uCSR.io.csr(2), 32)
  StatusCSR(cntStalls.io.count, uCSR.io.csr(3), 32)
  private val enableCntCSR = SimpleCSR(uCSR.io.csr(4), 32)
  clearToggle(1) := true.B // Always clear toggle start signal
  cntExecute.io.reset := start(1)
  cntStalls.io.reset := start(1)
  cntExecute.io.incr := traceDumper.trace_data.fire && enableCntCSR(0)
  cntStalls.io.incr := !traceDumper.trace_data.fire && enableCntCSR(0)

  private val cntBursts = Module(new PerfCounter(32))
  StatusCSR(cntBursts.io.count, uCSR.io.csr(5), 32)
  cntBursts.io.reset := start(1)
  cntBursts.io.incr := traceDumper.dram_write_port.req.fire

  M_AXI_W <> traceDumper.dram_write_port
}

object ARMFlexInstrumentedVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  import java.io._
  val fr = new FileWriter(new File("regression/cl_devteroflex/design/ARMFlexTop.v"))
  fr.write(c.emitVerilog(
    new ARMFlexTopInstrumented(
      new PipelineParams(thidN = 8, pAddrW =  24),
      new MemoryHierarchyParams(thidN = 8, pAddrW = 24)
      ), annotations = Seq(TargetDirAnnotation("regression/cl_devteroflex/design/"))))
  fr.close()
}
