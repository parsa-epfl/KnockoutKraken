// See LICENSE.txt for license details.
package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._
import armflex.util._
import antmicro.CSR._
import chisel3.aop.Select

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
  val simVerbose:   Boolean = false
) {
  assert(vAddrW == 64 || vAddrW == 32)
  val thidW = log2Ceil(thidN)
  def thidT = UInt(thidW.W)

  val axiDataW = 32

  def getBlockAddrUpperBits(addr: UInt) = addr(11, log2Ceil(blockSize / 8) - 1)
  def getBlockAddrBits(addr: UInt) = addr(log2Ceil(blockSize / 8) - 1, 0)

  def simLog(str: Printable): Unit = {
    if (simVerbose) {
      printf(str)
    }
  }
}

import antmicro.Bus.AXI4Lite

class PipelineWithCSR(params: PipelineParams) extends Module {
  private val uCSRthid2asid = Module(new CSR_thid2asid(params.thidN, params.asidW, thid2asidPortsN = 2))
  val S_CSR_TreadTable = IO(Flipped(uCSRthid2asid.bus.cloneType))
  S_CSR_TreadTable <> uCSRthid2asid.bus

  val pipeline = Module(new PipelineWithTransplant(params))
  val S_CSR_Pipeline = IO(Flipped(pipeline.hostIO.S_CSR.cloneType))
  S_CSR_Pipeline <> pipeline.hostIO.S_CSR

  // BRAM (Architecture State)
  val S_AXI_ArchState = IO(Flipped(pipeline.hostIO.S_AXI.cloneType))
  pipeline.hostIO.S_AXI <> S_AXI_ArchState

  // Memory port.
  val mem_io = IO(pipeline.mem_io.cloneType)
  val mmu_io = IO(pipeline.mmu_io.cloneType)
  pipeline.mem_io <> mem_io
  pipeline.mmu_io <> mmu_io

  // mem.inst.req
  uCSRthid2asid.thid_i(0) := pipeline.mem_io.inst.tlb.req.bits.thid
  mem_io.inst.tlb.req.bits.asid := uCSRthid2asid.asid_o(0).bits

  uCSRthid2asid.thid_i(1) := pipeline.mem_io.data.tlb.req.bits.thid
  mem_io.data.tlb.req.bits.asid := uCSRthid2asid.asid_o(1).bits

  // Instrumentation Interface
  val instrument = IO(pipeline.instrument.cloneType)
  instrument <> pipeline.instrument

  val dbg = IO(pipeline.dbg.cloneType)
  dbg <> pipeline.dbg

  if(true) {// TODO conditional assertions
    when(pipeline.mem_io.data.tlb.req.valid){
      assert(uCSRthid2asid.asid_o(1).valid, "No memory request is allowed if the hardware thread is not registed.")
    }
    when(pipeline.mem_io.inst.tlb.req.valid) {
      assert(uCSRthid2asid.asid_o(0).valid, "No memory request is allowed if the hardware thread is not registed.")
    }
  }
}

class PipelineWithTransplant(params: PipelineParams) extends Module {

  // Pipeline
  val pipeline = Module(new Pipeline(params))
  // State
  // Memory
  val mem_io = IO(pipeline.mem_io.cloneType)
  val mmu_io = IO(pipeline.mmu_io.cloneType)
  mem_io <> pipeline.mem_io
  mmu_io <> pipeline.mmu_io
  val archstate = Module(new ArchState(params.thidN, params.DebugSignals))

  // -------- Pipeline ---------
  // Get state from Issue
  pipeline.archstate.issue.ready := true.B
  pipeline.archstate.issue.sel.tag <> archstate.pstateIO.issue.thread
  pipeline.archstate.issue.regs.curr <> archstate.pstateIO.issue.pstate
  pipeline.archstate.issue.rd <> archstate.rfile_rd

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
  // Mem Fault - Transplant
  // TODO: Add page Fault support
  transplantU.mem2trans.instFault := 0.U.asTypeOf(transplantU.mem2trans.instFault) // pipeline.mem_io.instFault
  transplantU.mem2trans.dataFault := 0.U.asTypeOf(transplantU.mem2trans.dataFault) // pipeline.mem_io.dataFault

  // Shanqing sincerely to @Rafael: PLEASE!!! Only update one signal from one place, not multiple places? Otherwise when debugging you have to search all possible sources and this is a disaster!!!!
  // Doing this can bring some benefit when writing, but it's a bomb buried deep for future debugging!
  archstate.pstateIO.commit <> pipeline.archstate.commit

  // Update State - Highjack commit ports from pipeline
  archstate.pstateIO.transplant.thread := transplantU.trans2cpu.thread
  
  pipeline.archstate.commit.ready := archstate.pstateIO.commit.ready && 
                                    !transplantU.trans2cpu.stallPipeline && 
                                    !transplantU.cpu2trans.stallPipeline
  
  // Multiplex the archstate write port.
  when(transplantU.trans2cpu.rfile_wr.en){
    // transplant unit always has the highest priority to write into the register file.
    archstate.rfile_wr <> transplantU.trans2cpu.rfile_wr
    // Don't worry, the pipeline should be stalled.
    assert(pipeline.archstate.commit.ready === false.B)
  }.otherwise {
    archstate.rfile_wr <> pipeline.archstate.commit.wr
  }

  
  // By default, the Pstate is from the CPU.
  // @Rafael you have update to the archstate.pstateIO.commit in another place.
  when(transplantU.trans2cpu.pstate.valid) {
    // PState is from the pipeline.
    archstate.pstateIO.commit.fire := true.B
    archstate.pstateIO.commit.tag := transplantU.trans2cpu.thread
    archstate.pstateIO.commit.pstate.next := transplantU.trans2cpu.pstate.bits
    archstate.pstateIO.commit.isTransplantUnit := true.B
    archstate.pstateIO.commit.isCommitUnit := false.B
    transplantU.cpu2trans.pstate := archstate.pstateIO.commit.pstate.curr
    assert(pipeline.archstate.commit.ready === false.B)
  }.otherwise {
    // Read State for Host
    transplantU.cpu2trans.pstate := archstate.pstateIO.transplant.pstate
  }

  transplantU.cpu2trans.rfile_wr <> pipeline.archstate.commit.wr
  transplantU.cpu2trans.doneCPU := pipeline.transplantIO.done
  pipeline.transplantIO.start.valid := transplantU.trans2cpu.start
  pipeline.transplantIO.start.tag := transplantU.trans2cpu.thread
  pipeline.transplantIO.start.bits.get := transplantU.trans2cpu.pstate.bits.PC
  // Transplant from Host
  transplantU.S_CSR <> hostIO.S_CSR
  transplantU.S_AXI <> hostIO.S_AXI
  pipeline.transplantIO.stopCPU := transplantU.cpu2trans.stopCPU
  archstate.pstateIO.forceTransplant := transplantU.cpu2trans.forceTransplant

  // Instrumentation interface
  val instrument = IO(pipeline.instrument.cloneType)
  instrument <> pipeline.instrument

  if(false) { // TODO Conditional assertions and printing
    when(archstate.pstateIO.commit.fire) {
      printf(p"Pipeline:Commit:THID[${archstate.pstateIO.commit.tag}]:PC[0x${Hexadecimal(pipeline.archstate.commit.pstate.next.PC)}]->PC[0x${Hexadecimal(pipeline.archstate.commit.pstate.next.PC)}]\n")
    }
    when(pipeline.transplantIO.done.valid) {
      printf(p"Pipeline:Transplant:THID[${archstate.pstateIO.commit.tag}]:PC[0x${Hexadecimal(archstate.pstateIO.commit.pstate.curr.PC)}]->Transplant\n")
    }
  }

  class DebugBundle extends Bundle {
    val fetch = ValidTag(params.thidN)
    val issue = ValidTag(params.thidN)
    val issuingMem = Output(Bool())
    val issuingTransplant = Output(Bool())
    val commit = ValidTag(params.thidN)
    val commitIsTransplant = Output(Bool())
    val transplant = Output(ValidTag(params.thidN))
    val stateVec = archstate.dbg.vecState.get.cloneType
  }

  //* DBG
  val dbg = IO(new Bundle {
    val bits = if (params.DebugSignals) Some(Output(new DebugBundle)) else None
  })

  if(params.DebugSignals) {
    dbg.bits.get.stateVec := archstate.dbg.vecState.get
    dbg.bits.get.fetch.valid := pipeline.mem_io.inst.tlb.req.valid
    dbg.bits.get.fetch.tag := pipeline.mem_io.inst.tlb.req.bits.thid

    dbg.bits.get.issue.tag := pipeline.dbg.issue.thread
    dbg.bits.get.issuingMem := pipeline.dbg.issue.mem
    dbg.bits.get.issuingTransplant := pipeline.dbg.issue.transplant
    dbg.bits.get.issue.valid := pipeline.dbg.issue.valid

    dbg.bits.get.commit.tag := RegNext(pipeline.archstate.commit.tag)
    dbg.bits.get.commit.valid := RegNext(pipeline.archstate.commit.fire)
    dbg.bits.get.commitIsTransplant := RegNext(pipeline.transplantIO.done.valid)
    dbg.bits.get.transplant.valid := transplantU.trans2cpu.start
    dbg.bits.get.transplant.tag := transplantU.trans2cpu.thread
  }
  // */
}
