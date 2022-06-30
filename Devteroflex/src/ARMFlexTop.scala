import antmicro.Bus.{AXI4, AXI4AR, AXI4AW, AXI4B, AXI4R, AXI4W}
import chisel3._
import chisel3.util._
import armflex._
import armflex.util._
import armflex_cache._
import armflex_mmu._
import armflex.util.ExtraUtils._
import firrtl.options.TargetDirAnnotation
import scala.collection.DebugUtils
import antmicro.CSR.AXI4LiteCSR
import antmicro.CSR.CSRBusMasterToNSlaves
import antmicro.CSR.CSRBusSlaveConfig
import armflex_pmu.PerformanceMonitor
import antmicro.CSR._
import chisel3.experimental.{prefix, noPrefix}

class MemorySystemPipelinePortIO(params: MemoryHierarchyParams) extends Bundle {
  val cache = Flipped(new PipeCache.PipeCacheIO(params.getCacheParams.pAddrWidth, params.getCacheParams.blockSize))
  val tlb = new TLB2PipelineIO(params.getPageTableParams)
}

class MemorySystemPipelineIO(params: MemoryHierarchyParams) extends Bundle {
  val data = new MemorySystemPipelinePortIO(params)
  val inst = new MemorySystemPipelinePortIO(params)
  val mmu = Flipped(new PipeMMUIO)
}

class MemorySystem(params: MemoryHierarchyParams) extends Module {

  private val mmu = Module(new MMU(params))
  private val itlb = Module(new TLB(params.getPageTableParams, () => new PseudoTreeLRUCore(params.tlbWayNumber)))
  private val dtlb = Module(new TLB(params.getPageTableParams, () => new PseudoTreeLRUCore(params.tlbWayNumber)))
  private val icache = Module(BaseCache(params.getCacheParams, () => new MatrixLRUCore(params.cacheWayNumber)))
  private val dcache = Module(BaseCache(params.getCacheParams, () => new MatrixLRUCore(params.cacheWayNumber)))
  private val icacheAdaptor = Module(new Cache2DMAAdaptor(params.getCacheParams.databankParameter, params.thidN + 1))
  private val dcacheAdaptor = Module(new Cache2DMAAdaptor(params.getCacheParams.databankParameter, params.thidN + 1))
  mmu.mmu_tlb_io.inst <> itlb.mmu_io
  mmu.mmu_tlb_io.data <> dtlb.mmu_io
  mmu.mmu_cache_io.inst <> icache.mmu_i
  mmu.mmu_cache_io.data <> dcache.mmu_i
  mmu.mmu_cache_io.inst.wbEmpty := icacheAdaptor.mmu_io_pendingQueueEmpty
  mmu.mmu_cache_io.data.wbEmpty := dcacheAdaptor.mmu_io_pendingQueueEmpty
  icache.axiMem_io <> icacheAdaptor.cache_io
  dcache.axiMem_io <> dcacheAdaptor.cache_io

  val pipeline_io = IO(new MemorySystemPipelineIO(params))
  val axiShell_io = IO(new Bundle {
    val AXI_MMU = mmu.axiShell_io.cloneType
    val M_AXI_DMA_icache = icacheAdaptor.M_DMA.cloneType
    val M_AXI_DMA_dcache = dcacheAdaptor.M_DMA.cloneType
  })

  val S_CSR = IO(Flipped(mmu.S_CSR.cloneType))
  S_CSR <> mmu.S_CSR

  val S_AXI = IO(Flipped(mmu.S_AXI.cloneType))
  S_AXI <> mmu.S_AXI

  pipeline_io.inst.cache <> icache.pipeline_io
  pipeline_io.data.cache <> dcache.pipeline_io
  pipeline_io.inst.tlb <> itlb.pipeline_io
  pipeline_io.data.tlb <> dtlb.pipeline_io
  pipeline_io.mmu <> mmu.mmu_pipe_io
  axiShell_io.AXI_MMU <> mmu.axiShell_io
  axiShell_io.M_AXI_DMA_icache <> icacheAdaptor.M_DMA
  axiShell_io.M_AXI_DMA_dcache <> dcacheAdaptor.M_DMA
  when(icacheAdaptor.M_DMA.rd.req.fire) {
    assert(icacheAdaptor.M_DMA.rd.req.bits.addr >= (1 << (params.pAddrW - 8)).U, "iCache should not read the page table region.")
  }
  when(icacheAdaptor.M_DMA.wr.req.fire) {
    assert(icacheAdaptor.M_DMA.rd.req.bits.addr >= (1 << (params.pAddrW - 8)).U, "iCache should not read the page table region.")
  }

  // PMU
  val oPMUEventTriggers = IO(Output(new Bundle {
    val tlb = dtlb.oPMUCountingReq.cloneType
    val dcache = dcacheAdaptor.oPMUReq.cloneType
    val mm = mmu.oPMUCountingReq.cloneType
  }))

  oPMUEventTriggers.tlb := dtlb.oPMUCountingReq
  oPMUEventTriggers.dcache := dcacheAdaptor.oPMUReq
  oPMUEventTriggers.mm := mmu.oPMUCountingReq

  // TODO: Rewire ILA
  val oILA = IO(new Bundle {
    val dTLBTranslateReq = Output(dtlb.oDebug.translateReq.cloneType)
    val dTLBTranslateResp = Output(dtlb.oDebug.translateResp.cloneType)
    val dTLBRefillResp = Output(dtlb.oDebug.refillResp.cloneType)

    // val pageFaultReq = Output(mmu.oDebug.pageFaultReq.cloneType)
    // val pageFaultReply = Output(mmu.oDebug.pageFaultReply.cloneType)
    // val inFIFOHandshake = Output(mmu.oDebug.inFIFOHandshake.cloneType)
    // val outFIFOHandshake = Output(mmu.oDebug.outFIFOHandshake.cloneType)

    // val pwState = Output(UInt(2.W))
    // val ptAccessReq = Output(mmu.oILA.ptAccessReq.cloneType)
    // val ptes = Output(mmu.oILA.ptes.cloneType)
    // val pteBufferState = Output(mmu.oILA.pteBufferState.cloneType)
    // val pteHitVec = Output(mmu.oILA.pteHitVec.cloneType)
  })

  oILA.dTLBRefillResp := dtlb.oDebug.refillResp
  oILA.dTLBTranslateReq := dtlb.oDebug.translateReq
  oILA.dTLBTranslateResp := dtlb.oDebug.translateResp

  // oILA.pageFaultReq := mmu.oDebug.pageFaultReq
  // oILA.pageFaultReply := mmu.oDebug.pageFaultReply
  // oILA.inFIFOHandshake := mmu.oDebug.inFIFOHandshake
  // oILA.outFIFOHandshake := mmu.oDebug.outFIFOHandshake

  // oILA.pwState := mmu.oILA.pwState
  // oILA.ptAccessReq := mmu.oILA.ptAccessReq
  // oILA.ptes := mmu.oILA.ptes
  // oILA.pteBufferState := mmu.oILA.pteBufferState
  // oILA.pteHitVec := mmu.oILA.pteHitVec
}

object MemorySystemVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  import java.io._
  val fr = new FileWriter(new File("test/genFiles/ArmflexTop/MemorySystem.v"))
  fr.write(c.emitVerilog(new MemorySystem(new MemoryHierarchyParams), annotations = Seq(TargetDirAnnotation("test/genFiles/ArmflexTop"))))
  fr.close()
}

class ARMFlexTop(
  paramsPipeline: PipelineParams,
  paramsMemoryHierarchy: MemoryHierarchyParams
) extends Module {
  // Assert that both systems have same parameters
  assert(paramsPipeline.thidN == paramsMemoryHierarchy.thidN)
  assert(paramsPipeline.asidW == paramsMemoryHierarchy.asidW)
  assert(paramsPipeline.vAddrW == paramsMemoryHierarchy.vAddrW)
  assert(paramsPipeline.pAddrW == paramsMemoryHierarchy.pAddrW)
  assert(paramsPipeline.blockSize == paramsMemoryHierarchy.cacheBlockSize)

  val u_pipeline = Module(new PipelineWithCSR(paramsPipeline))
  private val memory = Module(new MemorySystem(paramsMemoryHierarchy))
  u_pipeline.mmu_io <> memory.pipeline_io.mmu
  // TLB interconnect
  memory.pipeline_io.inst.tlb.translationReq.bits := u_pipeline.mem_io.inst.tlb.req.bits
  memory.pipeline_io.inst.tlb.translationReq.handshake(u_pipeline.mem_io.inst.tlb.req)
  memory.pipeline_io.data.tlb.translationReq.bits := u_pipeline.mem_io.data.tlb.req.bits
  memory.pipeline_io.data.tlb.translationReq.handshake(u_pipeline.mem_io.data.tlb.req)
  // Note, ready singal is stashed
  u_pipeline.mem_io.inst.tlb.resp.bits := memory.pipeline_io.inst.tlb.translationResp.bits.toPipeTLBResponse
  u_pipeline.mem_io.inst.tlb.resp.valid := memory.pipeline_io.inst.tlb.translationResp.valid
  u_pipeline.mem_io.data.tlb.resp.bits := memory.pipeline_io.data.tlb.translationResp.bits.toPipeTLBResponse
  u_pipeline.mem_io.data.tlb.resp.valid := memory.pipeline_io.data.tlb.translationResp.valid
  // Wake up after a miss completed
  u_pipeline.mem_io.wake(0).tag := memory.pipeline_io.inst.tlb.wakeAfterMiss.bits
  u_pipeline.mem_io.wake(0).valid := memory.pipeline_io.inst.tlb.wakeAfterMiss.valid
  u_pipeline.mem_io.wake(1).tag := memory.pipeline_io.data.tlb.wakeAfterMiss.bits
  u_pipeline.mem_io.wake(1).valid := memory.pipeline_io.data.tlb.wakeAfterMiss.valid

  memory.pipeline_io.inst.cache <> u_pipeline.mem_io.inst.cache
  memory.pipeline_io.data.cache <> u_pipeline.mem_io.data.cache

  
  // Wrap S_CSR bus with an AXIL
  val uAXIL2CSR = Module(new AXI4LiteCSR(32, 0x500))
  val S_AXIL = IO(Flipped(uAXIL2CSR.io.ctl.cloneType))
  S_AXIL <> uAXIL2CSR.io.ctl
  val uCSRMux = Module(new CSRBusMasterToNSlaves(32, Seq(
    new CSRBusSlaveConfig(0, 0x100),
    new CSRBusSlaveConfig(0x100, TransplantConsts.TRANS_REG_TOTAL_REGS),
    new CSRBusSlaveConfig(0x200, 4),
    new CSRBusSlaveConfig(0x300, 0x100),
    new CSRBusSlaveConfig(0x400, 0x100)
  ), (0, 0x500)))
  uCSRMux.masterBus <> uAXIL2CSR.io.bus
  uCSRMux.slavesBus(0) <> u_pipeline.S_CSR_ThreadTable
  uCSRMux.slavesBus(1) <> u_pipeline.S_CSR_Pipeline
  uCSRMux.slavesBus(2) <> memory.S_CSR

  def axiAddressMapFunction(addr: UInt, idx: Int): Bool = {
    if(idx == 0)
      return addr < 0x10000.U
    else if(idx == 1)
      return addr < 0x20000.U && addr >= 0x10000.U
    else
      return false.B
  }

  // Aggregate AXI slaves
  val uAXIMux = Module(new AXIInterconnector(2, axiAddressMapFunction, 17, 512))
  
  uAXIMux.M_AXI(0) <> u_pipeline.S_AXI_ArchState // 0x00000 to 0x10000
  uAXIMux.M_AXI(1) <> memory.S_AXI // 0x10000 to 0x20000

  val S_AXI = IO(Flipped(uAXIMux.S_AXI.cloneType))
  S_AXI <> uAXIMux.S_AXI

  val AXI_MEM = IO(memory.axiShell_io.cloneType)
  AXI_MEM <> memory.axiShell_io

  // Instrumentation Interface
  val instrument = IO(u_pipeline.instrument.cloneType)
  instrument <> u_pipeline.instrument

  val dbg = IO(u_pipeline.dbg.cloneType)
  dbg <> u_pipeline.dbg

  // Performance Unit
  val uPMU = Module(new PerformanceMonitor(paramsPipeline.thidN))
  uPMU.iCommittedValid := u_pipeline.oPMUCountingCommit

  uPMU.iCycleCountingReq(0) := memory.oPMUEventTriggers.dcache
  uPMU.iCycleCountingReq(1) := memory.oPMUEventTriggers.tlb
  uPMU.iCycleCountingReq(2) := u_pipeline.oPMUTransplantCycleCountingReq
  uPMU.iCycleCountingReq(3) := memory.oPMUEventTriggers.mm

  uCSRMux.slavesBus(3) <> uPMU.S_CSR

  val assertRegs = Math.ceil(u_pipeline.asserts.asUInt.getWidth/32.0).toInt
  assert(assertRegs >= 1)
  val uCSR = Module(new CSR(32, assertRegs))
  uCSRMux.slavesBus(4) <> uCSR.io.bus
  
  // uCSR[0]: Assertions
  assert(assertRegs == 1)
  val regsAssert = Seq.fill(assertRegs)(prefix("Asserts")(SetClearReg(32)))
  val (rAssertsPending_0, setAssert_0, clearAssert_0) = regsAssert(0)
  clearAssert_0 := PulseCSR(uCSR.io.csr(0), 32)
  StatusCSR(rAssertsPending_0, uCSR.io.csr(0), 32)
  setAssert_0 := u_pipeline.asserts.asUInt

  val regsAssert_first = Seq.fill(assertRegs)(prefix("Asserts_first")(SetClearReg(32)))
  val (rAssertsPending_1, setAssert_1, clearAssert_1) = regsAssert_first(0)
  clearAssert_1 := PulseCSR(uCSR.io.csr(0), 32)
  StatusCSR(rAssertsPending_1, uCSR.io.csr(0), 32)
  when(!rAssertsPending_1.orR) {
    setAssert_1 := u_pipeline.asserts.asUInt
  }


  // This debug signal is different from dbg: It's directly connected to the ILA.
  val oILA = IO(Output(new Bundle {
    val assert = u_pipeline.asserts.asUInt.cloneType
    val mem = memory.oILA.cloneType
  }))
  oILA.mem := memory.oILA
  oILA.assert := u_pipeline.asserts.asUInt
}

class ARMFlexTopSimulator(
  paramsPipeline: PipelineParams,
  paramsMemoryHierarchy: MemoryHierarchyParams
) extends Module {
  import armflex.util.AXIReadMultiplexer
  import armflex.util.AXIWriteMultiplexer
  val devteroFlexTop = Module(new ARMFlexTop(paramsPipeline, paramsMemoryHierarchy))
  private val dmaMasterCtrl = Module(new AXI4MasterDMACtrl(3, 3, paramsMemoryHierarchy.dramAddrW, paramsMemoryHierarchy.dramDataW))
  val S_AXI = IO(Flipped(devteroFlexTop.S_AXI.cloneType))
  val S_AXIL = IO(Flipped(devteroFlexTop.S_AXIL.cloneType))
  val M_AXI = IO(new AXI4(paramsMemoryHierarchy.dramAddrW, paramsMemoryHierarchy.dramDataW))

  S_AXI <> devteroFlexTop.S_AXI
  S_AXIL <> devteroFlexTop.S_AXIL
  
  M_AXI <> dmaMasterCtrl.M_AXI
  dmaMasterCtrl.req.wrPorts(0) <> devteroFlexTop.AXI_MEM.AXI_MMU.M_DMA_WR // PageTableAccess
  dmaMasterCtrl.req.rdPorts(0) <> devteroFlexTop.AXI_MEM.AXI_MMU.M_DMA_RD // PageTableAccess
  dmaMasterCtrl.req.wrPorts(1) <> devteroFlexTop.AXI_MEM.M_AXI_DMA_icache.wr
  dmaMasterCtrl.req.rdPorts(1) <> devteroFlexTop.AXI_MEM.M_AXI_DMA_icache.rd
  dmaMasterCtrl.req.wrPorts(2) <> devteroFlexTop.AXI_MEM.M_AXI_DMA_dcache.wr
  dmaMasterCtrl.req.rdPorts(2) <> devteroFlexTop.AXI_MEM.M_AXI_DMA_dcache.rd

  // No Instrumentation enabled in this build, make sure it has no impact downstream
  devteroFlexTop.instrument.commit.ready := true.B
  val dbg = IO(devteroFlexTop.dbg.cloneType)
  dbg <> devteroFlexTop.dbg

  // The signals observed by ILA
  // val oILA = IO(Output(devteroFlexTop.oILA.cloneType))
  // oILA := devteroFlexTop.oILA
}

object ARMFlexTopSimulatorVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  import java.io._
  val fr = new FileWriter(new File("regression/cl_devteroflex/design/ARMFlexTop.v"))
  fr.write(c.emitVerilog(
    new ARMFlexTopSimulator(
      new PipelineParams(thidN = 32, pAddrW =  24),
      new MemoryHierarchyParams(thidN = 32, pAddrW = 24)
      ), annotations = Seq(TargetDirAnnotation("regression/cl_devteroflex/design/"))))
  fr.close()
}

object ARMFlexTopDebugVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  import java.io._
  val fr = new FileWriter(new File("regression/cl_devteroflex/design/ARMFlexTop.v"))
  fr.write(c.emitVerilog(
    new ARMFlexTopSimulator(
      new PipelineParams(thidN = 32, pAddrW =  24, DebugSignals = true),
      new MemoryHierarchyParams(thidN = 32, pAddrW = 24)
      ), annotations = Seq(TargetDirAnnotation("regression/cl_devteroflex/design/"))))
  fr.close()
}


object ARMFlexTopVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  import java.io._
  val v = c.emitVerilog(
    new ARMFlexTopSimulator(
      new PipelineParams(thidN = 32, pAddrW =  34),
      new MemoryHierarchyParams(thidN = 32, pAddrW = 34)
    ), annotations = Seq(TargetDirAnnotation("fpga/")))
  
  
  // renaming AXI wires
  val processed_v = v.replaceAll("""([MS]_AXI[A-Z_]*_)(aw|w|b|r|ar)_""", "$1")

  val fr = new FileWriter(new File("fpga/ARMFlexTop.v"))
  // fr.write(v)
  fr.write(processed_v)
  fr.close()
}

