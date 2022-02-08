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

class PipelineAxi(params: PipelineParams) extends Module {
  private val axiAddr_range = (0, 0xA000)
  private val csrAddr_range = (axiAddr_range._1 >> 2, axiAddr_range._2 >> 2)

  val pipeline = Module(new PipelineWithTransplant(params))

  private val uAxilToCSR = Module(new AXI4LiteCSR(params.axiDataW, csrAddr_range._2 - csrAddr_range._1))

  private val transplantCtrl_regCount = 4
  private val cfgBusCSR_archstate = new CSRBusSlave(0, 0x8000 >> 2)
  private val cfgBusCSR_thid2asid = new CSRBusSlave(0x8000 >> 2, params.thidN) // Address is 32b word addressed
  private val cfgBusCSR_transplant = new CSRBusSlave(0x9000 >> 2, transplantCtrl_regCount) // Address is 32b word addressed

  private val uCSRToArchState = Module(new CSR2BRAM(pipeline.transplantU.hostBRAMParams))
  private val uCSRthid2asid = Module(new CSR_thid2asid(params.thidN, params.asidW, thid2asidPortsN = 2))
  private val uCSR2ToTransplant = Module(new CSR(params.axiDataW, transplantCtrl_regCount))

  private val uCSRmux = Module(new CSRBusMasterToNSlaves(params.axiDataW, Seq(
                              cfgBusCSR_archstate, cfgBusCSR_thid2asid, cfgBusCSR_transplant), csrAddr_range))
  uCSRmux.masterBus <> uAxilToCSR.io.bus
  uCSRmux.slavesBus(0) <> uCSRToArchState.io.bus
  uCSRmux.slavesBus(1) <> uCSRthid2asid.bus
  uCSRmux.slavesBus(2) <> uCSR2ToTransplant.io.bus

  val S_AXIL = IO(Flipped(uAxilToCSR.io.ctl.cloneType))
  S_AXIL <> uAxilToCSR.io.ctl

  val doneCPU = WireInit(Mux(pipeline.hostIO.trans2host.doneCPU.valid, 1.U << pipeline.hostIO.trans2host.doneCPU.tag, 0.U))
  val doneTrans = WireInit(Mux(pipeline.hostIO.trans2host.doneTrans.valid, 1.U << pipeline.hostIO.trans2host.doneTrans.tag, 0.U))
  val host2transClear = WireInit(Mux(pipeline.hostIO.trans2host.clear.valid, 1.U << pipeline.hostIO.trans2host.clear.tag, 0.U))

  SetCSR(doneTrans.asUInt, uCSR2ToTransplant.io.csr(0), params.axiDataW)
  val pendingHostTrans = ClearCSR(host2transClear.asUInt, uCSR2ToTransplant.io.csr(1), params.axiDataW)
  val host2transStopCPU = ClearCSR(doneCPU.asUInt, uCSR2ToTransplant.io.csr(2), params.axiDataW)
  val host2transForceTransplant = ClearCSR((-1).S(params.axiDataW).asUInt, uCSR2ToTransplant.io.csr(3), params.axiDataW)

  pipeline.hostIO.host2trans.pending := pendingHostTrans
  pipeline.hostIO.host2trans.stopCPU := host2transStopCPU
  pipeline.hostIO.host2trans.forceTransplant := host2transForceTransplant

  // BRAM (Architecture State)
  pipeline.hostIO.port <> uCSRToArchState.io.port

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
  archstate.pstateIO.commit <> pipeline.archstate.commit
  archstate.rfile_wr <> pipeline.archstate.commit.wr

  // -------- Stats ------
  // TODO Performance counter stats

  // -------- Transplant ---------
  val transplantU = Module(new TransplantUnit(params.thidN))
  class HostIO extends Bundle {
    val port = transplantU.hostBramPort.cloneType
    val trans2host = transplantU.trans2host.cloneType
    val host2trans = transplantU.host2trans.cloneType
  }
  val hostIO = IO(new HostIO)
  // Mem Fault - Transplant
  // TODO: Add page Fault support
  transplantU.mem2trans.instFault := 0.U.asTypeOf(transplantU.mem2trans.instFault) // pipeline.mem_io.instFault
  transplantU.mem2trans.dataFault := 0.U.asTypeOf(transplantU.mem2trans.dataFault) // pipeline.mem_io.dataFault

  // Update State - Highjack commit ports from pipeline
  archstate.pstateIO.transplant.thread := transplantU.trans2cpu.thread
  pipeline.archstate.commit.ready := archstate.pstateIO.commit.ready && !transplantU.trans2cpu.updatingPState
  transplantU.cpu2trans.rfile_wr <> pipeline.archstate.commit.wr
  transplantU.cpu2trans.doneCPU := pipeline.transplantIO.done
  when(transplantU.trans2cpu.updatingPState) {
    archstate.rfile_wr <> transplantU.trans2cpu.rfile_wr
    archstate.pstateIO.commit.fire := true.B
    archstate.pstateIO.commit.tag := transplantU.trans2cpu.thread
    archstate.pstateIO.commit.pstate.next := transplantU.trans2cpu.pstate
    archstate.pstateIO.commit.isTransplantUnit := true.B
    archstate.pstateIO.commit.isCommitUnit := false.B
    transplantU.cpu2trans.pstate := archstate.pstateIO.commit.pstate.curr
  }.otherwise {
    // Read State for Host
    transplantU.cpu2trans.pstate := archstate.pstateIO.transplant.pstate
  }
  pipeline.transplantIO.start.valid := transplantU.trans2cpu.start
  pipeline.transplantIO.start.tag := transplantU.trans2cpu.thread
  pipeline.transplantIO.start.bits.get := transplantU.trans2cpu.pstate.PC
  // Transplant from Host
  transplantU.host2trans <> hostIO.host2trans
  transplantU.trans2host <> hostIO.trans2host
  transplantU.hostBramPort <> hostIO.port
  pipeline.transplantIO.stopCPU := transplantU.cpu2trans.stopCPU

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
