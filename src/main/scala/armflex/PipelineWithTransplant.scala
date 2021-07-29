// See LICENSE.txt for license details.
package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._
import arm.DECODE_CONTROL_SIGNALS._

import armflex_cache._
import armflex.util._
import armflex.util.ExtraUtils._

import antmicro.CSR._
import firrtl.PrimOps.Mul

class ProcConfig(
  // Address Space ID
  val ASID_WIDTH:    Int = 15,
  // Chisel Generator configs
  val NB_THREADS:    Int = 2,
  val BLOCK_SIZE:    Int = 512,
  val pAddressWidth: Int = 36,
  // Simulation settings
  val DebugSignals: Boolean = false,
  val rtlVerbose:   Boolean = false,
  val simVerbose:   Boolean = false) {
  // Threads
  // val NB_THREADS
  val NB_THREADS_W = log2Ceil(NB_THREADS) // 4 Threads
  def TAG_T = UInt(NB_THREADS_W.W)
  val TAG_X = 0.U(NB_THREADS_W.W)
  val TAG_VEC_X = 0.U(NB_THREADS.W)
  def TAG_VEC_T = UInt(NB_THREADS.W)

  // Memory
  val cacheLatency = 1

  // BRAM Generator configs
  val bramConfigState = new BRAMConfig(8, 8, 1024, "", false, false)

  def simLog(str: Printable) {
    if (simVerbose) {
      printf(str)
    }
  }
}

import antmicro.Bus.AXI4Lite

class PipelineAxi(implicit val cfg: ProcConfig) extends MultiIOModule {
  val axiDataWidth = 32
  val regCount = 2
  val bramRegCount = 2*1024

  val pipeline = Module(new PipelineWithTransplant)
  val uThreadTable = Module(new ThreadTable(
    cfg.NB_THREADS, cfg.ASID_WIDTH, 2, 0
  ))

  val uAxilToCSR = Module(new AXI4LiteCSR(axiDataWidth, 2*bramRegCount))

  val uCSR2ToTransplant = Module(new CSR(axiDataWidth, regCount))
  val uCSRToArchState = Module(new CSR2BRAM(pipeline.transplantU.hostBRAMConfig))
  uCSRToArchState.io.bus <> 0.U.asTypeOf(uCSRToArchState.io.bus.cloneType)
  uCSR2ToTransplant.io.bus <> 0.U.asTypeOf(uCSR2ToTransplant.io.bus)
  uThreadTable.S_BUS <> 0.U.asTypeOf(uThreadTable.S_BUS)

  // TODO: Replace this with a AXI arbiter. 
  when((uAxilToCSR.io.bus.addr >> log2Ceil(bramRegCount)).asUInt() === 0.U) { // 0-2K
    uCSRToArchState.io.bus <> uAxilToCSR.io.bus
  }.elsewhen((uAxilToCSR.io.bus.addr >> (log2Ceil(bramRegCount) - 1)).asUInt() === 2.U) { // 2k - 3k
    uThreadTable.S_BUS <> uAxilToCSR.io.bus
  }.otherwise { // 3k - 4k
    uCSR2ToTransplant.io.bus <> uAxilToCSR.io.bus
  }

  val S_AXI = IO(Flipped(new AXI4Lite(log2Ceil(bramRegCount*2) + log2Ceil(axiDataWidth / 8), axiDataWidth)))
  S_AXI <> uAxilToCSR.io.ctl

  val trans2host = WireInit(Mux(pipeline.hostIO.trans2host.done.valid, 1.U << pipeline.hostIO.trans2host.done.tag, 0.U))
  val host2transClear = WireInit(Mux(pipeline.hostIO.trans2host.clear.valid, 1.U << pipeline.hostIO.trans2host.clear.tag, 0.U))

  SetCSR(trans2host.asUInt(), uCSR2ToTransplant.io.csr(0), axiDataWidth)
  val pendingHostTrans = ClearCSR(host2transClear.asUInt(), uCSR2ToTransplant.io.csr(1), axiDataWidth)
  pipeline.hostIO.host2trans.pending := pendingHostTrans
  
  // BRAM (Architecture State)
  pipeline.hostIO.port <> uCSRToArchState.io.port

  // Memory port.
  val mem_io = IO(pipeline.mem_io.cloneType)
  pipeline.mem_io <> mem_io

  // mem.inst.req
  uThreadTable.tid_i(0) := pipeline.mem_io.inst.tlb.req.bits.thid
  mem_io.inst.tlb.req.bits.asid := uThreadTable.pid_o(0).bits
  when(pipeline.mem_io.inst.tlb.req.valid) {
    assert(uThreadTable.pid_o(0).valid, "No instruction request is allowed if the hardware thread is not registed.")
  }

  uThreadTable.tid_i(1) := pipeline.mem_io.data.tlb.req.bits.thid
  mem_io.data.tlb.req.bits.asid := uThreadTable.pid_o(1).bits
  when(pipeline.mem_io.data.tlb.req.valid){
    assert(uThreadTable.pid_o(1).valid, "No instruction request is allowed if the hardware thread is not registed.")
  }
}

class PipelineWithTransplant(implicit val cfg: ProcConfig) extends MultiIOModule {

  // Pipeline
  val pipeline = Module(new Pipeline)
  // State
  // Memory
  val mem_io = IO(pipeline.mem_io.cloneType)
  val mmu_io = IO(pipeline.mmu_io.cloneType)
  mem_io <> pipeline.mem_io
  mmu_io <> pipeline.mmu_io
  val archstate = Module(new ArchState(cfg.NB_THREADS, cfg.DebugSignals))

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
  val transplantU = Module(new TransplantUnit(cfg.NB_THREADS))
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

  //* DBG
  val dbg = IO(new Bundle {
    val bits =
      if (cfg.DebugSignals) Some(Output(new Bundle {
        val fetch = ValidTag(cfg.NB_THREADS, new FullStateBundle)
        val issue = ValidTag(cfg.NB_THREADS, new FullStateBundle)
        val issuingMem = Output(Bool())
        val issuingTransplant = Output(Bool())
        val commit = ValidTag(cfg.NB_THREADS, new FullStateBundle)
        val commitTransplant = Output(Valid(INST_T))
        val stateVec = archstate.dbg.vecState.get.cloneType
      }))
      else None
  })

  if(cfg.DebugSignals) {
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
