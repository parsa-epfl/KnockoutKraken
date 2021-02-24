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

import antmicro.CSR._
import firrtl.PrimOps.Mul

class ProcConfig(
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
  val pipeline = Module(new PipelineWithTransplant)
  val axiLiteCSR = Module(new AXI4LiteCSR(axiDataWidth, regCount))
  val csr = Module(new CSR(axiDataWidth, regCount))
  csr.io.bus <> axiLiteCSR.io.bus

  val mem = IO(pipeline.mem.cloneType)
  val transplantIO = IO(new Bundle {
    val port = pipeline.hostIO.port.cloneType
    val ctl = Flipped(new AXI4Lite(4, axiDataWidth))
  })
  transplantIO.ctl <> axiLiteCSR.io.ctl
  pipeline.mem <> mem
  pipeline.hostIO.port <> transplantIO.port

  val trans2host = WireInit(Mux(pipeline.hostIO.trans2host.done.valid, 1.U << pipeline.hostIO.trans2host.done.tag, 0.U))
  val host2transClear = WireInit(Mux(pipeline.hostIO.trans2host.clear.valid, 1.U << pipeline.hostIO.trans2host.clear.tag, 0.U))

  SetCSR(trans2host, csr.io.csr(0), axiDataWidth)
  val pendingHostTrans = ClearCSR(host2transClear, csr.io.csr(1), axiDataWidth)
  pipeline.hostIO.host2trans.pending := pendingHostTrans
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
  val hostIO = IO(new Bundle {
    val port = transplantU.hostBramPort.cloneType
    val trans2host = transplantU.trans2host.cloneType
    val host2trans = transplantU.host2trans.cloneType
  })
  // Mem Fault - Transplant
  transplantU.mem2trans.instFault := mem.instFault
  transplantU.mem2trans.dataFault := mem.dataFault
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
      }))
      else None
  })

  if(cfg.DebugSignals) {
    dbg.bits.get.fetch.valid := pipeline.mem.inst.req.valid
    dbg.bits.get.fetch.tag := pipeline.mem.inst.req.bits.thread_id
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
