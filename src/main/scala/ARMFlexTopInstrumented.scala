import antmicro.Bus.{AXI4, AXI4AR, AXI4AW, AXI4B, AXI4R, AXI4W}
import antmicro.CSR._
import chisel3._
import chisel3.util._
import armflex._
import armflex.util.{ AXILInterconnector, AXIWriteMasterIF}
import armflex_cache._
import armflex_mmu.{MMU, MemoryHierarchyParams}
import armflex.util.ExtraUtils._
import firrtl.options.TargetDirAnnotation
import armflex.util.{ DRAMWrapperWrite, DRAMPortParams }
import instrumentation.{ TraceDump, TraceDumpParams }
import antmicro.Bus.AXI4Lite
import armflex.util.PerfCounter
import armflex.util.AXILInterconnectorNonOptimized

class ARMFlexTopInstrumented(
  paramsPipeline: PipelineParams,
  paramsMemoryHierarchy: MemoryHierarchyParams
) extends MultiIOModule {
  import armflex.util.AXIReadMultiplexer
  import armflex.util.AXIWriteMultiplexer
  private val devteroFlexTop = Module(new ARMFlexTop(paramsPipeline, paramsMemoryHierarchy))
  private val axiMulti_R = Module(new AXIReadMultiplexer(paramsMemoryHierarchy.dramAddrW, 512, 6))
  private val axiMulti_W = Module(new AXIWriteMultiplexer(paramsMemoryHierarchy.dramAddrW, 512, 6))
  //private val axilMulti = Module(new AXILInterconnector(Seq(0x00000, 0x10000, 0x1F000), Seq(0x08000, 0x10000, 0x1F000), 32, 32))
  private val axilMulti = Module(new AXILInterconnectorNonOptimized(Seq(0x00000, 0x10000, 0x1F000), 32, 32))
  val S_AXI = IO(Flipped(devteroFlexTop.AXI_MEM.AXI_MMU.S_AXI.cloneType))
  val S_AXIL = IO(Flipped(axilMulti.S_AXIL.cloneType))
  S_AXI <> devteroFlexTop.AXI_MEM.AXI_MMU.S_AXI
  S_AXIL <> axilMulti.S_AXIL
  axilMulti.M_AXIL(0) <> devteroFlexTop.S_AXIL_TRANSPLANT
  axilMulti.M_AXIL(1) <> devteroFlexTop.AXI_MEM.AXI_MMU.S_AXIL_QEMU_MQ
  for(i <- 0 until devteroFlexTop.AXI_MEM.AXI_MMU.M_DMA_R.length)
    axiMulti_R.S_IF(i) <> devteroFlexTop.AXI_MEM.AXI_MMU.M_DMA_R(i)
  for(i <- 0 until devteroFlexTop.AXI_MEM.AXI_MMU.M_DMA_W.length)
    axiMulti_W.S_IF(i) <> devteroFlexTop.AXI_MEM.AXI_MMU.M_DMA_W(i)
  var W_IDX = devteroFlexTop.AXI_MEM.AXI_MMU.M_DMA_W.length
  var R_IDX = devteroFlexTop.AXI_MEM.AXI_MMU.M_DMA_R.length
  axiMulti_W.S_IF(W_IDX+0) <> devteroFlexTop.AXI_MEM.M_AXI_DMA_icacheW
  axiMulti_W.S_IF(W_IDX+1) <> devteroFlexTop.AXI_MEM.M_AXI_DMA_dcacheW
  axiMulti_R.S_IF(R_IDX+0) <> devteroFlexTop.AXI_MEM.M_AXI_DMA_icacheR
  axiMulti_R.S_IF(R_IDX+1) <> devteroFlexTop.AXI_MEM.M_AXI_DMA_dcacheR

  val M_AXI = IO(new AXI4(paramsMemoryHierarchy.dramAddrW, paramsMemoryHierarchy.cacheBlockSize))
  // Interconnect Read ports
  M_AXI.ar <> axiMulti_R.M_AXI.ar
  M_AXI.r <> axiMulti_R.M_AXI.r
  // Disable Write ports
  axiMulti_R.M_AXI.aw <> AXI4AW.stub(paramsMemoryHierarchy.dramAddrW)
  axiMulti_R.M_AXI.w <> AXI4W.stub(paramsMemoryHierarchy.cacheBlockSize)
  axiMulti_R.M_AXI.b <> AXI4B.stub()
  // Interconnect Write ports
  M_AXI.aw <> axiMulti_W.M_AXI.aw
  M_AXI.w <> axiMulti_W.M_AXI.w
  M_AXI.b <> axiMulti_W.M_AXI.b
  // Disable Read ports
  axiMulti_W.M_AXI.ar <> AXI4AR.stub(paramsMemoryHierarchy.dramAddrW)
  axiMulti_W.M_AXI.r <> AXI4R.stub(paramsMemoryHierarchy.cacheBlockSize)

  // Instrumentation to store PC
  val traceWrapper = Module(new TraceWrapper(
    paramsMemoryHierarchy.dramAddrW, axilMulti.S_AXIL.addrWidth, 0x1F000))
  traceWrapper.commit.bits <> devteroFlexTop.instrument.commit.bits.pc
  traceWrapper.commit.handshake(devteroFlexTop.instrument.commit)
  axilMulti.M_AXIL(2) <> traceWrapper.S_AXIL
  axiMulti_W.S_IF(W_IDX+2) <> traceWrapper.M_AXI_W
}

class TraceWrapper(val dramAddrW: Int, val axilAddrW: Int, val axilBaseAddr: Int) extends MultiIOModule {
  val S_AXIL = IO(Flipped(new AXI4Lite(axilAddrW, 32))) 
  val M_AXI_W = IO(new AXIWriteMasterIF(dramAddrW, 512))
  val commit = IO(Flipped(Decoupled(UInt(64.W))))

  private val traceDumper = Module(new TraceDump(new TraceDumpParams(dramAddrW, 512, 64, 1024, 256, 32)))
  private val dramPortTransformer = Module(new DRAMWrapperWrite(new DRAMPortParams(dramAddrW, 512)))
  private val nCSR = 6
  private val uAxilToCSR = Module(new AXI4LiteCSR(32, nCSR))
  private val cfgBusCSR = new CSRBusSlave(0, nCSR)
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

  dramPortTransformer.write <> traceDumper.dram_write_port
  M_AXI_W <> dramPortTransformer.M_AXI_W
}

object ARMFlexInstrumentedVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  import java.io._
  val fr = new FileWriter(new File("regression/rtl/ARMFlexTop.v"))
  fr.write(c.emitVerilog(
    new ARMFlexTopInstrumented(
      new PipelineParams(thidN = 8, pAddrW =  24),
      new MemoryHierarchyParams(thidN = 8, pAddrW = 24)
      ), annotations = Seq(TargetDirAnnotation("regression/rtl/"))))
  fr.close()
}
