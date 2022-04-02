import antmicro.Bus.{AXI4, AXI4AR, AXI4AW, AXI4B, AXI4R, AXI4W}
import chisel3._
import chisel3.util._
import armflex._
import armflex.util._
import armflex_cache._
import armflex_mmu.{MMU, MemoryHierarchyParams}
import armflex.util.ExtraUtils._
import firrtl.options.TargetDirAnnotation
import scala.collection.DebugUtils
import antmicro.CSR.AXI4LiteCSR
import antmicro.CSR.CSRBusMasterToNSlaves
import antmicro.CSR.CSRBusSlaveConfig

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
  private val icacheAdaptor = Module(new Cache2AXIAdaptor(params.getCacheParams.databankParameter, params.thidN + 1))
  private val dcacheAdaptor = Module(new Cache2AXIAdaptor(params.getCacheParams.databankParameter, params.thidN + 1))
  mmu.tlb_io.inst <> itlb.mmu_io
  mmu.tlb_io.data <> dtlb.mmu_io
  mmu.cache_io.inst <> icache.mmu_i
  mmu.cache_io.data <> dcache.mmu_i
  mmu.cacheAxiCtrl_io.icacheWbEmpty := icacheAdaptor.mmu_io_pendingQueueEmpty
  mmu.cacheAxiCtrl_io.dcacheWbEmpty := dcacheAdaptor.mmu_io_pendingQueueEmpty
  icache.axiMem_io <> icacheAdaptor.cache_io
  dcache.axiMem_io <> dcacheAdaptor.cache_io

  val pipeline_io = IO(new MemorySystemPipelineIO(params))
  val axiShell_io = IO(new Bundle {
    val AXI_MMU = mmu.axiShell_io.cloneType
    val M_AXI_DMA_icacheR = icacheAdaptor.M_DMA_R.cloneType
    val M_AXI_DMA_dcacheR = dcacheAdaptor.M_DMA_R.cloneType
    val M_AXI_DMA_icacheW = icacheAdaptor.M_DMA_W.cloneType
    val M_AXI_DMA_dcacheW = dcacheAdaptor.M_DMA_W.cloneType
  })

  val S_CSR = IO(Flipped(mmu.S_CSR.cloneType))
  S_CSR <> mmu.S_CSR

  val S_AXI = IO(Flipped(mmu.S_AXI.cloneType))
  S_AXI <> mmu.S_AXI

  pipeline_io.inst.cache <> icache.pipeline_io
  pipeline_io.data.cache <> dcache.pipeline_io
  pipeline_io.inst.tlb <> itlb.pipeline_io
  pipeline_io.data.tlb <> dtlb.pipeline_io
  pipeline_io.mmu <> mmu.pipeline_io
  axiShell_io.AXI_MMU <> mmu.axiShell_io
  axiShell_io.M_AXI_DMA_icacheR <> icacheAdaptor.M_DMA_R
  when(icacheAdaptor.M_DMA_R.req.fire) {
    assert(icacheAdaptor.M_DMA_R.req.bits.address >= (1 << (params.pAddrW - 8)).U, "iCache should not read the page table region.")
  }
  axiShell_io.M_AXI_DMA_dcacheR <> dcacheAdaptor.M_DMA_R
  when(dcacheAdaptor.M_DMA_R.req.fire) {
    assert(dcacheAdaptor.M_DMA_R.req.bits.address >= (1 << (params.pAddrW - 8)).U, "dCache should not read the page table region.")
  }
  axiShell_io.M_AXI_DMA_icacheW <> icacheAdaptor.M_DMA_W
  when(icacheAdaptor.M_DMA_W.req.fire) {
    assert(icacheAdaptor.M_DMA_W.req.bits.address >= (1 << (params.pAddrW - 8)).U, "iCache should not write the page table region.")
  }
  axiShell_io.M_AXI_DMA_dcacheW <> dcacheAdaptor.M_DMA_W
  when(dcacheAdaptor.M_DMA_W.req.fire) {
    assert(dcacheAdaptor.M_DMA_W.req.bits.address >= (1 << (params.pAddrW - 8)).U, "dCache should not write the page table region.")
  }
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
  val uAXIL2CSR = Module(new AXI4LiteCSR(32, 0x300))
  val S_AXIL = IO(Flipped(uAXIL2CSR.io.ctl.cloneType))
  S_AXIL <> uAXIL2CSR.io.ctl
  val uCSRMux = Module(new CSRBusMasterToNSlaves(32, Seq(
    new CSRBusSlaveConfig(0, 0x100),
    new CSRBusSlaveConfig(0x100, 4),
    new CSRBusSlaveConfig(0x200, 4)
  ), (0, 0x300)))
  uCSRMux.masterBus <> uAXIL2CSR.io.bus
  uCSRMux.slavesBus(0) <> u_pipeline.S_CSR_TreadTable
  uCSRMux.slavesBus(1) <> u_pipeline.S_CSR_Pipeline
  uCSRMux.slavesBus(2) <> memory.S_CSR

  // Aggregate AXI slaves
  val uAXIMux = Module(new AXIInterconnector(
    Seq(0x00000, 0x10000),
    Seq(0x10000, 0x10000),
    17,
    512
  ))
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
}

class ARMFlexTopSimulator(
  paramsPipeline: PipelineParams,
  paramsMemoryHierarchy: MemoryHierarchyParams
) extends Module {
  import armflex.util.AXIReadMultiplexer
  import armflex.util.AXIWriteMultiplexer
  val devteroFlexTop = Module(new ARMFlexTop(paramsPipeline, paramsMemoryHierarchy))
  private val axiMulti_R = Module(new AXIReadMultiplexer(paramsMemoryHierarchy.dramAddrW, 512, 6))
  private val axiMulti_W = Module(new AXIWriteMultiplexer(paramsMemoryHierarchy.dramAddrW, 512, 5))
  val S_AXI = IO(Flipped(devteroFlexTop.S_AXI.cloneType))
  val S_AXIL = IO(Flipped(devteroFlexTop.S_AXIL.cloneType))
  S_AXI <> devteroFlexTop.S_AXI
  S_AXIL <> devteroFlexTop.S_AXIL
  
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

  // No Instrumentation enabled in this build, make sure it has no impact downstream
  devteroFlexTop.instrument.commit.ready := true.B
  val dbg = IO(devteroFlexTop.dbg.cloneType)
  dbg <> devteroFlexTop.dbg
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
  fr.write(processed_v)
  fr.close()
}

/**
 * This module is wraps all Pipeline ports with a AxiLite register, it is not meant to be functionally correct but
 * actually just to get synthesis/area numbers without getting optimized.
 * It can also be used to check for Chisel RTL generation faults.
 * 
 * @param params
 */
class PipelineAxiHacked(params: PipelineParams) extends Module {
  import antmicro.CSR._
  val axidataW = 32
  val regCount = 2
  val pipeline = Module(new PipelineWithTransplant(params))
  val axiLiteCSR = Module(new AXI4LiteCSR(axidataW, 40))
  val AXILiteRealCSR = Module(new AXI4LiteCSR(axidataW, 0x200))
  val csr = Module(new CSR(axidataW, 40))
  csr.io.bus <> axiLiteCSR.io.bus

  val S_AXIL_TRANSPLANT = IO(Flipped(axiLiteCSR.io.ctl.cloneType))
  S_AXIL_TRANSPLANT <> axiLiteCSR.io.ctl

  axiLiteCSR.io.bus <> pipeline.hostIO.S_CSR

  val S_AXI_ARCHSTATE = IO(Flipped(pipeline.hostIO.S_AXI.cloneType))
  S_AXI_ARCHSTATE <> pipeline.hostIO.S_AXI

  // Hacked port
  pipeline.mem_io.wake := SimpleCSR(csr.io.csr(2), axidataW).asTypeOf(pipeline.mem_io.wake)
  val handshakeReg = WireInit(SimpleCSR(csr.io.csr(4), axidataW))
  pipeline.mem_io.inst.tlb.req.ready := handshakeReg(0)
  pipeline.mem_io.inst.tlb.resp.valid := handshakeReg(1)
  pipeline.mem_io.inst.cache.req.ready := handshakeReg(0)
  pipeline.mem_io.inst.cache.resp.valid := handshakeReg(1)
  pipeline.mem_io.data.tlb.req.ready := handshakeReg(0)
  pipeline.mem_io.data.tlb.resp.valid := handshakeReg(1)
  pipeline.mem_io.data.cache.req.ready := handshakeReg(0)
  pipeline.mem_io.data.cache.resp.valid := handshakeReg(1)
  pipeline.mem_io.wake.zipWithIndex.foreach {
    case (bits, idx) =>
      bits.valid := handshakeReg(3+idx)
      bits.tag := SimpleCSR(csr.io.csr(4+idx), axidataW)
  }

  pipeline.mmu_io.data.flushCompled.valid := handshakeReg(2)
  pipeline.mmu_io.data.flushPermReq.valid := handshakeReg(3)
  pipeline.mmu_io.inst.flushCompled.valid := handshakeReg(2)
  pipeline.mmu_io.inst.flushPermReq.valid := handshakeReg(3)

  val currIdx = 4 + pipeline.mem_io.wake.length
  val regsPerBlock = params.blockSize/axidataW
  pipeline.mem_io.inst.tlb.resp.bits := Cat(for (idx <- 0 until regsPerBlock) yield SimpleCSR(csr.io.csr(currIdx+idx), axidataW)).asTypeOf(pipeline.mem_io.inst.tlb.resp.bits)
  pipeline.mem_io.inst.cache.resp.bits := Cat(for (idx <- 0 until regsPerBlock) yield SimpleCSR(csr.io.csr(currIdx+idx), axidataW)).asTypeOf(pipeline.mem_io.inst.cache.resp.bits)
  pipeline.mem_io.data.tlb.resp.bits := Cat(for (idx <- 0 until regsPerBlock) yield SimpleCSR(csr.io.csr(currIdx+regsPerBlock+idx), axidataW)).asTypeOf(pipeline.mem_io.data.tlb.resp.bits)
  pipeline.mem_io.data.cache.resp.bits := Cat(for (idx <- 0 until regsPerBlock) yield SimpleCSR(csr.io.csr(currIdx+regsPerBlock+idx), axidataW)).asTypeOf(pipeline.mem_io.data.cache.resp.bits)

  csr.io.csr(38) <> 0.U.asTypeOf(csr.io.csr(0))
  csr.io.csr(3) <> 0.U.asTypeOf(csr.io.csr(0))
  csr.io.csr(39) <> 0.U.asTypeOf(csr.io.csr(0))
}

object PipelineFakeTopVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  import java.io._
  val fr = new FileWriter(new File("test/genFiles/Pipeline/Pipeline.v"))
  val proc = new PipelineParams(thidN = 16)
  fr.write(c.emitVerilog(new PipelineAxiHacked(proc), annotations = Seq(TargetDirAnnotation("test/genFiles/Pipeline"))))
  fr.close()
}
