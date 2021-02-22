// See LICENSE.txt for license details.
package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._
import arm.DECODE_CONTROL_SIGNALS._

import armflex.cache._
import armflex.util._
import armflex.util.ExtraUtils._
import Chisel.debug

class ProcConfig(
  // Chisel Generator configs
  val NB_THREADS:   Int = 2,
  val BLOCK_SIZE:   Int = 512,
  val pAddressWidth: Int = 36,
  // Simulation settings
  val DebugSignals: Boolean = false,
  val rtlVerbose:   Boolean = false,
  val simVerbose:   Boolean = false
  ) {
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
}

class PipelineWithTransplant(implicit val cfg: ProcConfig) extends MultiIOModule {

  // Pipeline
  val pipeline = Module(new Pipeline)
  // State
  // Memory
  val mem = IO(pipeline.mem.cloneType)
  mem <> pipeline.mem
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
  val transplantIO = IO(new Bundle {
    val port = transplantU.hostBramPort.cloneType
    val done = Input(ValidTag(cfg.NB_THREADS))
    val transOut = Output(ValidTag(cfg.NB_THREADS))
  })
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
  transplantIO.port <> transplantU.hostBramPort
  transplantU.host2trans.done := transplantIO.done
  transplantIO.transOut := transplantU.trans2host.done

  //* DBG
  val dbg = IO(Output(new Bundle {
    val fetch = ValidTag(cfg.NB_THREADS, new FullStateBundle)
    val issue = ValidTag(cfg.NB_THREADS, new FullStateBundle)
    val issuingMem = Output(Bool())
    val commit = ValidTag(cfg.NB_THREADS, new FullStateBundle)
    val commitTransplant = Output(Valid(INST_T))
  }))

  dbg.fetch.valid := pipeline.mem.inst.req.valid
  dbg.fetch.tag := pipeline.mem.inst.req.bits.thread_id
  dbg.fetch.bits.get := archstate.dbg.vecState.get(dbg.fetch.tag)

  dbg.issue.tag := pipeline.archstate.issue.sel.tag
  dbg.issue.valid := pipeline.archstate.issue.sel.valid
  dbg.issue.bits.get := archstate.dbg.vecState.get(dbg.issue.tag)
  dbg.issuingMem := pipeline.dbg.issuingMem

  dbg.commit.tag := pipeline.archstate.commit.sel.tag
  dbg.commit.valid := pipeline.archstate.commit.sel.valid
  dbg.commit.bits.get := archstate.dbg.vecState.get(dbg.commit.tag)
  dbg.commitTransplant.valid := pipeline.transplantIO.done.valid
  dbg.commitTransplant.bits := pipeline.transplantIO.done.bits.get
  // */
}