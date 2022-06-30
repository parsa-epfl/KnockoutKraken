// See LICENSE.txt for license details.
package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._
import armflex.util._
import antmicro.CSR._
import chisel3.aop.Select
import armflex_pmu.CycleCountingPort

class PipelineParams(
  val asidW:    Int = 15,
  val thidN:    Int = 32,
  val pAddrW:   Int = 36,
  val vAddrW:   Int = 64,
  val blockSize:Int = 512,
  // Simulation settings
  val assertEnabled: Boolean = false, // TODO use this variable for assertions
  val DebugSignals: Boolean = false,
  val rtlVerbose:   Boolean = false,
  val simVerbose:   Boolean = false,
  val ilaEnabled:   Boolean = false
) {
  assert(vAddrW == 64 || vAddrW == 32)
  val thidW = log2Ceil(thidN)
  def thidT = UInt(thidW.W)

  val axiDataW = 32

  def getBlockAddrUpperBits(addr: UInt) = addr(12, log2Ceil(blockSize / 8))
  def getBlockAddrBits(addr: UInt) = addr(log2Ceil(blockSize / 8) - 1, 0)

  def simLog(str: Printable): Unit = {
    if (simVerbose) {
      printf(str)
    }
  }
}

import antmicro.Bus.AXI4Lite

class PipelineWithCSR(params: PipelineParams) extends Module {
  // TODO: Not used anyways as we rely on another mechanism, we could remove this
  private val uCSRVecAsid = Module(new StatusVecCSR(params.thidN, params.axiDataW))
  val S_CSR_ThreadTable = IO(Flipped(uCSRVecAsid.io.bus.cloneType))
  S_CSR_ThreadTable <> uCSRVecAsid.io.bus
  uCSRVecAsid.io.vec := 0.U.asTypeOf(uCSRVecAsid.io.vec.cloneType)

  val pipelineU = Module(new PipelineWithTransplant(params))
  val S_CSR_Pipeline = IO(Flipped(pipelineU.hostIO.S_CSR.cloneType))
  S_CSR_Pipeline <> pipelineU.hostIO.S_CSR

  // BRAM (Architecture State)
  val S_AXI_ArchState = IO(Flipped(pipelineU.hostIO.S_AXI.cloneType))
  pipelineU.hostIO.S_AXI <> S_AXI_ArchState

  // Memory port.
  val mem_io = IO(pipelineU.mem_io.cloneType)
  val mmu_io = IO(pipelineU.mmu_io.cloneType)
  pipelineU.mem_io <> mem_io
  pipelineU.mmu_io <> mmu_io

  // Instrumentation Interface
  val instrument = IO(pipelineU.instrument.cloneType)
  instrument <> pipelineU.instrument

  val dbg = IO(pipelineU.dbg.cloneType)
  dbg <> pipelineU.dbg

  val asserts = IO(pipelineU.asserts.cloneType)
  asserts <> pipelineU.asserts

  // To PerformanceMonitor
  val oPMUCountingCommit = IO(Output(Bool()))
  oPMUCountingCommit := pipelineU.oPMUCountingCommit
  val oPMUTransplantCycleCountingReq = IO(Output(new CycleCountingPort(params.thidN)))
  oPMUTransplantCycleCountingReq := pipelineU.oPMUTransplantCycleCountingReq
}

class PipelineWithTransplant(params: PipelineParams) extends Module {

  // Pipeline
  val pipelineU = Module(new Pipeline(params))

  // State
  // Memory
  val mem_io = IO(pipelineU.mem_io.cloneType)
  val mmu_io = IO(pipelineU.mmu_io.cloneType)
  mem_io <> pipelineU.mem_io
  mmu_io <> pipelineU.mmu_io
  val archstateU = Module(new ArchState(params.thidN, params.DebugSignals))
  archstateU.pstateIO.mem(0).thid := mem_io.inst.tlb.req.bits.thid
  archstateU.pstateIO.mem(1).thid := mem_io.data.tlb.req.bits.thid
  mem_io.inst.tlb.req.bits.asid := archstateU.pstateIO.mem(0).asid
  mem_io.data.tlb.req.bits.asid := archstateU.pstateIO.mem(1).asid

  // -------- Pipeline ---------
  // Get state from Issue
  pipelineU.archstate.issue.ready := true.B
  pipelineU.archstate.issue.sel.tag <> archstateU.pstateIO.issue.thid
  pipelineU.archstate.issue.regs.curr <> archstateU.pstateIO.issue.pstate
  pipelineU.archstate.issue.rd <> archstateU.rfile_rd

  // Writeback state from commit
  
  // -------- Stats ------
  // TODO Performance counter stats

  // -------- Transplant ---------
  val transplantU = Module(new TransplantUnit(params.thidN))
  class HostIO extends Bundle {
    val S_AXI = Flipped(transplantU.S_AXI.cloneType)
    val S_CSR = Flipped(transplantU.S_CSR.cloneType)
  }
  val hostIO = IO(new HostIO)

  // Shanqing sincerely to @Rafael: PLEASE!!! Only update one signal from one place, not multiple places? Otherwise when debugging you have to search all possible sources and this is a disaster!!!!
  // Doing this can bring some benefit when writing, but it's a bomb buried deep for future debugging!
  archstateU.pstateIO.commit <> pipelineU.archstate.commit

  // Update State - Highjack commit ports from pipeline
  
  pipelineU.archstate.commit.ready := archstateU.pstateIO.commit.ready && 
                                    !transplantU.trans2cpu.stallPipeline && 
                                    !transplantU.cpu2trans.stallPipeline
  
  // Multiplex the archstate write port.
  when(transplantU.trans2cpu.rfile_wr.en){
    // transplant unit always has the highest priority to write into the register file.
    archstateU.rfile_wr <> transplantU.trans2cpu.rfile_wr
    // Don't worry, the pipeline should be stalled.
    assert(pipelineU.archstate.commit.ready === false.B)
  }.otherwise {
    archstateU.rfile_wr <> pipelineU.archstate.commit.wr
  }

  // By default, the Pstate is from the CPU.
  // @Rafael you have update to the archstate.pstateIO.commit in another place.
  when(transplantU.trans2cpu.pstate.valid) {
    // PState is from the pipeline.
    archstateU.pstateIO.commit.fire := true.B
    archstateU.pstateIO.commit.tag := transplantU.trans2cpu.thid
    archstateU.pstateIO.commit.pstate.next := transplantU.trans2cpu.pstate.bits
    archstateU.pstateIO.commit.isTransplantUnit := true.B
    archstateU.pstateIO.commit.isCommitUnit := false.B
    transplantU.cpu2trans.pstate := archstateU.pstateIO.commit.pstate.curr
    assert(pipelineU.archstate.commit.ready === false.B)
  }.otherwise {
    // Read State for Host
    transplantU.cpu2trans.pstate := archstateU.pstateIO.transplant.pstate
  }

  transplantU.cpu2trans.rfile_wr <> pipelineU.archstate.commit.wr
  transplantU.cpu2trans.doneCPU.bits := pipelineU.transplantIO.done.tag
  transplantU.cpu2trans.doneCPU.valid := pipelineU.transplantIO.done.valid

  archstateU.pstateIO.transplant.thid := transplantU.trans2cpu.thid
  // Wait for the PC to be available in archstate
  pipelineU.transplantIO.start.valid := transplantU.trans2cpu.start.valid
  pipelineU.transplantIO.start.tag := transplantU.trans2cpu.start.bits
  pipelineU.transplantIO.start.bits.get := archstateU.pstateIO.transplant.pstate.PC

  // Transplant from Host
  transplantU.S_CSR <> hostIO.S_CSR
  transplantU.S_AXI <> hostIO.S_AXI
  pipelineU.transplantIO.stopCPU := transplantU.cpu2trans.stopCPU
  archstateU.pstateIO.forceTransplant := transplantU.cpu2trans.forceTransplant
  pipelineU.transplantIO.status <> transplantU.status

  // Instrumentation interface
  val instrument = IO(pipelineU.instrument.cloneType)
  instrument <> pipelineU.instrument

  // ----------- Asserts 
  val asserts = IO(Output(new Bundle {
    val transplant = transplantU.asserts.cloneType
    val pipeline = pipelineU.asserts.cloneType
  }))
  asserts.transplant <> transplantU.asserts
  asserts.pipeline <> pipelineU.asserts

  if(false) { // TODO Conditional assertions and printing
    when(archstateU.pstateIO.commit.fire) {
      printf(p"Pipeline:Commit:THID[${archstateU.pstateIO.commit.tag}]:PC[0x${Hexadecimal(pipelineU.archstate.commit.pstate.next.PC)}]->PC[0x${Hexadecimal(pipelineU.archstate.commit.pstate.next.PC)}]\n")
    }
    when(pipelineU.transplantIO.done.valid) {
      printf(p"Pipeline:Transplant:THID[${archstateU.pstateIO.commit.tag}]:PC[0x${Hexadecimal(archstateU.pstateIO.commit.pstate.curr.PC)}]->Transplant\n")
    }
  }

  //* DBG
  class DebugBundle extends Bundle {
    val fetch = ValidTag(params.thidN)
    val issue = ValidTag(params.thidN)
    val issuingMem = Output(Bool())
    val issuingTransplant = Output(Bool())
    val commit = ValidTag(params.thidN)
    val commitIsTransplant = Output(Bool())
    val transplant = Output(ValidTag(params.thidN))
    val stateVec = archstateU.dbg.vecState.get.cloneType
  }

  val dbg = IO(new Bundle {
    val bits = if (params.DebugSignals) Some(Output(new DebugBundle)) else None
  })
  if(params.DebugSignals) {
    dbg.bits.get.stateVec := archstateU.dbg.vecState.get
    dbg.bits.get.fetch.valid := pipelineU.mem_io.inst.tlb.req.valid
    dbg.bits.get.fetch.tag := pipelineU.mem_io.inst.tlb.req.bits.thid

    dbg.bits.get.issue.tag := pipelineU.dbg.issue.thid
    dbg.bits.get.issuingMem := pipelineU.dbg.issue.mem
    dbg.bits.get.issuingTransplant := pipelineU.dbg.issue.transplant
    dbg.bits.get.issue.valid := pipelineU.dbg.issue.valid

    dbg.bits.get.commit.tag := RegNext(pipelineU.archstate.commit.tag)
    dbg.bits.get.commit.valid := RegNext(pipelineU.archstate.commit.fire)
    dbg.bits.get.commitIsTransplant := RegNext(pipelineU.transplantIO.done.valid)
    dbg.bits.get.transplant.valid := transplantU.trans2cpu.start.valid
    dbg.bits.get.transplant.tag := transplantU.trans2cpu.start.bits
  }
  // */

  // PMU Event
  val oPMUCountingCommit = IO(Output(Bool()))
  oPMUCountingCommit := archstateU.pstateIO.commit.fire && 
    archstateU.pstateIO.commit.ready && 
    archstateU.pstateIO.commit.isCommitUnit
  
  val oPMUTransplantCycleCountingReq = IO(Output(new CycleCountingPort(params.thidN)))
  oPMUTransplantCycleCountingReq.start.bits := transplantU.cpu2trans.doneCPU.bits
  oPMUTransplantCycleCountingReq.start.valid := transplantU.cpu2trans.doneCPU.valid
  oPMUTransplantCycleCountingReq.stop.bits := transplantU.trans2cpu.start.bits
  oPMUTransplantCycleCountingReq.stop.valid := transplantU.trans2cpu.start.valid
}
