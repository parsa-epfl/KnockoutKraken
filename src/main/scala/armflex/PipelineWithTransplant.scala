// See LICENSE.txt for license details.
package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._
import armflex.util._
import antmicro.CSR._

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

  def getBlockAddrBits(addr: UInt) = addr(log2Ceil(blockSize / 8) - 1, 0)

  def simLog(str: Printable) {
    if (simVerbose) {
      printf(str)
    }
  }
}

import antmicro.Bus.AXI4Lite

class PipelineAxi(params: PipelineParams) extends MultiIOModule {
  private val axiAddr_range = (0, 0xA000)
  private val csrAddr_range = (axiAddr_range._1 >> 2, axiAddr_range._2 >> 2)

  val pipeline = Module(new PipelineWithTransplant(params))

  private val uAxilToCSR = Module(new AXI4LiteCSR(params.axiDataW, csrAddr_range._2 - csrAddr_range._1))

  private val transplantCtrl_regCount = 3
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

  val trans2host = WireInit(Mux(pipeline.hostIO.trans2host.done.valid, 1.U << pipeline.hostIO.trans2host.done.tag, 0.U))
  val host2transClear = WireInit(Mux(pipeline.hostIO.trans2host.clear.valid, 1.U << pipeline.hostIO.trans2host.clear.tag, 0.U))

  SetCSR(trans2host.asUInt, uCSR2ToTransplant.io.csr(0), params.axiDataW)
  val pendingHostTrans = ClearCSR(host2transClear.asUInt, uCSR2ToTransplant.io.csr(1), params.axiDataW)
  val host2transStopCPU = ClearCSR(trans2host.asUInt, uCSR2ToTransplant.io.csr(2), params.axiDataW)

  pipeline.hostIO.host2trans.pending := pendingHostTrans
  pipeline.hostIO.host2trans.stopCPU := host2transStopCPU

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

  if(true) {// TODO conditional assertions
    when(pipeline.mem_io.data.tlb.req.valid){
      assert(uCSRthid2asid.asid_o(1).valid, "No memory request is allowed if the hardware thread is not registed.")
    }
    when(pipeline.mem_io.inst.tlb.req.valid) {
      assert(uCSRthid2asid.asid_o(0).valid, "No memory request is allowed if the hardware thread is not registed.")
    }
  }
}

class PipelineWithTransplant(params: PipelineParams) extends MultiIOModule {

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
  pipeline.archstate.issue.sel.tag <> archstate.pstate.issue.thread
  pipeline.archstate.issue.regs.curr <> archstate.pstate.issue.pregs
  pipeline.archstate.issue.rd <> archstate.rfile_rd

  // Writeback state from commit
  archstate.pstate.commit.curr <> pipeline.archstate.commit.regs.curr
  archstate.pstate.commit.next.valid := pipeline.archstate.commit.sel.valid
  archstate.pstate.commit.next.tag := pipeline.archstate.commit.sel.tag
  archstate.pstate.commit.next.bits.get := pipeline.archstate.commit.regs.next
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
  archstate.pstate.transplant.thread := transplantU.trans2cpu.thread
  pipeline.archstate.commit.ready := !transplantU.trans2cpu.updatingPState
  transplantU.cpu2trans.rfile_wr <> pipeline.archstate.commit.wr
  transplantU.cpu2trans.done := pipeline.transplantIO.done
  when(transplantU.trans2cpu.updatingPState) {
    archstate.rfile_wr <> transplantU.trans2cpu.rfile_wr
    archstate.pstate.commit.next.valid := true.B
    archstate.pstate.commit.next.tag := transplantU.trans2cpu.thread
    archstate.pstate.commit.next.bits.get := transplantU.trans2cpu.pregs
    transplantU.cpu2trans.pregs := archstate.pstate.commit.curr
  }.otherwise {
    // Read State for Host
    transplantU.cpu2trans.pregs := archstate.pstate.transplant.pregs
  }
  pipeline.transplantIO.start.valid := transplantU.trans2cpu.start
  pipeline.transplantIO.start.tag := transplantU.trans2cpu.thread
  pipeline.transplantIO.start.bits.get := transplantU.trans2cpu.pregs.PC
  // Transplant from Host
  transplantU.host2trans <> hostIO.host2trans
  transplantU.trans2host <> hostIO.trans2host
  transplantU.hostBramPort <> hostIO.port
  pipeline.transplantIO.stopCPU := transplantU.cpu2trans.stopCPU

  // Instrumentation interface
  val instrument = IO(pipeline.instrument.cloneType)
  instrument <> pipeline.instrument

  if(false) { // TODO Conditional assertions and printing
    when(archstate.pstate.commit.next.valid) {
      printf(p"Pipeline:Commit:THID[${archstate.pstate.commit.next.tag}]:PC[0x${Hexadecimal(pipeline.archstate.commit.regs.next.PC)}]->PC[0x${Hexadecimal(pipeline.archstate.commit.regs.next.PC)}]\n")
    }
    when(pipeline.transplantIO.done.valid) {
      printf(p"Pipeline:Transplant:THID[${archstate.pstate.commit.next.tag}]:PC[0x${Hexadecimal(archstate.pstate.commit.curr.PC)}]->Transplant\n")
    }
  }

  //* DBG
  val dbg = IO(new Bundle {
    val bits =
      if (params.DebugSignals) Some(Output(new Bundle {
        val fetch = ValidTag(params.thidN, new FullStateBundle)
        val issue = ValidTag(params.thidN, new FullStateBundle)
        val issuingMem = Output(Bool())
        val issuingTransplant = Output(Bool())
        val commit = ValidTag(params.thidN, new FullStateBundle)
        val commitTransplant = Output(Valid(INST_T))
        val stateVec = archstate.dbg.vecState.get.cloneType
      }))
      else None
  })

  if(params.DebugSignals) {
    dbg.bits.get.stateVec := archstate.dbg.vecState.get
    dbg.bits.get.fetch.valid := pipeline.mem_io.inst.tlb.req.valid
    dbg.bits.get.fetch.tag := pipeline.mem_io.inst.tlb.req.bits.thid
    dbg.bits.get.fetch.bits.get := archstate.dbg.vecState.get(dbg.bits.get.fetch.tag)

    dbg.bits.get.issue.tag := pipeline.dbg.issue.thread
    dbg.bits.get.issue.bits.get := archstate.dbg.vecState.get(dbg.bits.get.issue.tag)
    dbg.bits.get.issuingMem := pipeline.dbg.issue.mem
    dbg.bits.get.issuingTransplant := pipeline.dbg.issue.transplant
    dbg.bits.get.issue.valid := pipeline.dbg.issue.valid

    dbg.bits.get.commit.tag := pipeline.archstate.commit.sel.tag
    dbg.bits.get.commit.valid := pipeline.archstate.commit.sel.valid
    dbg.bits.get.commit.bits.get := archstate.dbg.vecState.get(dbg.bits.get.commit.tag)
    dbg.bits.get.commitTransplant.valid := pipeline.transplantIO.done.valid
    dbg.bits.get.commitTransplant.bits := pipeline.transplantIO.done.bits.get
  }
  // */
}
