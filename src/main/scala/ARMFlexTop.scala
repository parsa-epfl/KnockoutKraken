import antmicro.Bus.{AXI4, AXI4AR, AXI4AW, AXI4B, AXI4R, AXI4W}
import chisel3._
import chisel3.util._
import armflex._
import armflex.util.AXILInterconnector
import armflex_cache._
import armflex_mmu.{MMU, MemoryHierarchyParams}
import armflex.util.ExtraUtils._
import firrtl.options.TargetDirAnnotation

class MemorySystemPipelinePortIO(params: MemoryHierarchyParams) extends Bundle {
  val cache = Flipped(new PipeCache.PipeCacheIO(params.getCacheParams.pAddrWidth, params.getCacheParams.blockSize))
  val tlb = new TLB2PipelineIO(params.getPageTableParams)
}

class MemorySystemPipelineIO(params: MemoryHierarchyParams) extends Bundle {
  val data = new MemorySystemPipelinePortIO(params)
  val inst = new MemorySystemPipelinePortIO(params)
  val mmu = Flipped(new PipeMMUIO)
}

class MemorySystem(params: MemoryHierarchyParams) extends MultiIOModule {

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

  pipeline_io.inst.cache <> icache.pipeline_io
  pipeline_io.data.cache <> dcache.pipeline_io
  pipeline_io.inst.tlb <> itlb.pipeline_io
  pipeline_io.data.tlb <> dtlb.pipeline_io
  pipeline_io.mmu <> mmu.pipeline_io
  axiShell_io.AXI_MMU <> mmu.axiShell_io
  axiShell_io.M_AXI_DMA_icacheR <> icacheAdaptor.M_DMA_R
  axiShell_io.M_AXI_DMA_dcacheR <> dcacheAdaptor.M_DMA_R
  axiShell_io.M_AXI_DMA_icacheW <> icacheAdaptor.M_DMA_W
  axiShell_io.M_AXI_DMA_dcacheW <> dcacheAdaptor.M_DMA_W
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
) extends MultiIOModule {
  // Assert that both systems have same parameters
  assert(paramsPipeline.thidN == paramsMemoryHierarchy.thidN)
  assert(paramsPipeline.asidW == paramsMemoryHierarchy.asidW)
  assert(paramsPipeline.vAddrW == paramsMemoryHierarchy.vAddrW)
  assert(paramsPipeline.pAddrW == paramsMemoryHierarchy.pAddrW)
  assert(paramsPipeline.blockSize == paramsMemoryHierarchy.cacheBlockSize)

  private val u_pipeline = Module(new PipelineAxi(paramsPipeline))
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

  val S_AXIL_TRANSPLANT = IO(Flipped(u_pipeline.S_AXIL.cloneType))
  val AXI_MEM = IO(memory.axiShell_io.cloneType)
  S_AXIL_TRANSPLANT <> u_pipeline.S_AXIL
  AXI_MEM <> memory.axiShell_io
}

class ARMFlexTopSimulator(
  paramsPipeline: PipelineParams,
  paramsMemoryHierarchy: MemoryHierarchyParams
) extends MultiIOModule {
  import armflex.util.AXIReadMultiplexer
  import armflex.util.AXIWriteMultiplexer
  private val devteroFlexTop = Module(new ARMFlexTop(paramsPipeline, paramsMemoryHierarchy))
  private val axiMulti_R = Module(new AXIReadMultiplexer(paramsMemoryHierarchy.dramAddrW, 512, 6))
  private val axiMulti_W = Module(new AXIWriteMultiplexer(paramsMemoryHierarchy.dramAddrW, 512, 5))
  private val axilMulti = Module(new AXILInterconnector(Seq(0x00000, 0x10000), Seq(0x08000,0x10000), 32, 32))
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
}

object ARMFlexTopSimulatorVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  import java.io._
  val fr = new FileWriter(new File("test/genFiles/ArmflexTopSim/ARMFlexTop_SIM.v"))
  fr.write(c.emitVerilog(
    new ARMFlexTopSimulator(
      new PipelineParams(thidN = 4, pAddrW =  24),
      new MemoryHierarchyParams(thidN = 4, pAddrW = 24)
      ), annotations = Seq(TargetDirAnnotation("test/genFiles/ArmflexTopSim"))))
  fr.close()
}

object ARMFlexTopVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  import java.io._
  val fr = new FileWriter(new File("test/genFiles/ArmflexTop/ARMFlexTop_AWS.v"))
  fr.write(c.emitVerilog(
    new ARMFlexTop(
      new PipelineParams(thidN = 2),
      new MemoryHierarchyParams(thidN = 2)
      ), annotations = Seq(TargetDirAnnotation("test/genFiles/ArmflexTop"))))
  fr.close()
}

/**
 * This module is wraps all Pipeline ports with a AxiLite register, it is not meant to be functionally correct but
 * actually just to get synthesis/area numbers without getting optimized.
 * It can also be used to check for Chisel RTL generation faults.
 * @params params
 */
class PipelineAxiHacked(params: PipelineParams) extends MultiIOModule {
  import antmicro.CSR._
  val axidataW = 32
  val regCount = 2
  val pipeline = Module(new PipelineWithTransplant(params))
  val axiLiteCSR = Module(new AXI4LiteCSR(axidataW, 40))
  val csr = Module(new CSR(axidataW, 40))
  csr.io.bus <> axiLiteCSR.io.bus

  val S_AXIL_TRANSPLANT = IO(Flipped(axiLiteCSR.io.ctl.cloneType))
  S_AXIL_TRANSPLANT <> axiLiteCSR.io.ctl

  val transplant_ram = IO(pipeline.hostIO.port.cloneType)
  transplant_ram <> pipeline.hostIO.port

  val trans2host = WireInit(Mux(pipeline.hostIO.trans2host.done.valid, 1.U << pipeline.hostIO.trans2host.done.tag, 0.U))
  val host2transClear = WireInit(Mux(pipeline.hostIO.trans2host.clear.valid, 1.U << pipeline.hostIO.trans2host.clear.tag, 0.U))

  SetCSR(trans2host.asUInt, csr.io.csr(0), axidataW)
  val pendingHostTrans = ClearCSR(host2transClear.asUInt, csr.io.csr(1), axidataW)
  pipeline.hostIO.host2trans.pending := pendingHostTrans

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
