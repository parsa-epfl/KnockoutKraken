// See LICENSE.txt for license details.
package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._
import arm.DECODE_CONTROL_SIGNALS._

import armflex.cache._
import armflex.util._
import armflex.util.DecoupledTools._
import Chisel.debug

class ProcConfig(
  // Chisel Generator configs
  val NB_THREADS:   Int = 2,
  val BLOCK_SIZE:   Int = 512,
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

  // BRAM Generator configs
  val bramConfigState = new BRAMConfig(8, 8, 1024, "", false, false)
}

class PipelineWithTransplant(implicit val cfg: ProcConfig) extends MultiIOModule {
  val pipeline = Module(new Pipeline)

  // Memory
  val mem = IO(pipeline.mem.cloneType)
  mem <> pipeline.mem
  // Transplants
  val transplantIO = IO(new Bundle {
    val state = new BRAMPort()(cfg.bramConfigState)
    val ctrl = new TransplantUnitHostIO
  })

  val rfile = Module(new RFileSingle(cfg.NB_THREADS, cfg.TAG_T, cfg.DebugSignals))
  val pregsVec = RegInit(VecInit(Seq.fill(cfg.NB_THREADS)(PStateRegs())))

  // -------- Pipeline ---------
  // Get state from Issue
  pipeline.archstate.issue.regs.curr <> pregsVec(pipeline.archstate.issue.sel.tag)
  pipeline.archstate.issue.rd <> rfile.rd

  // Writeback state from commit
  pipeline.archstate.commit.regs.curr <> pregsVec(pipeline.archstate.commit.sel.tag)
  pipeline.archstate.commit.regs.next <> pregsVec(pipeline.archstate.commit.sel.tag)
  pipeline.archstate.commit.wr <> rfile.wr

  // -------- Stats ------
  //pipeline.io.resetStats := 0.U

  // -------- Transplant ---------
  val stateBRAM = Module(new BRAM()(cfg.bramConfigState))
  val transplant = Module(new TransplantUnit)
  transplantIO.state <> stateBRAM.portA
  transplant.io.host2tpu <> transplantIO.ctrl
  transplant.io.stateBRAM <> stateBRAM.portB
  // Default Inputs
  transplant.io.rfile := DontCare
  transplant.io.tpu2cpu.done := pipeline.transplantIO.done

  pipeline.transplantIO.start.valid := transplant.io.tpu2cpu.fire.valid
  pipeline.transplantIO.start.tag := transplant.io.tpu2cpu.fire.tag
  pipeline.transplantIO.start.bits.get := pregsVec(transplant.io.tpu2cpu.fire.tag).PC
  // When working
  transplant.io.cpu2tpuState := pregsVec(transplant.io.tpu2cpu.freeze.tag)
  // TODO When HOST -> FPGA, commit unready
  // TODO When FPGA -> HOST, issue unready
  pipeline.archstate.commit.ready := !transplant.io.tpu2cpu.freeze.valid
  pipeline.archstate.issue.ready := !transplant.io.tpu2cpu.freeze.valid
  when(transplant.io.tpu2cpu.freeze.valid) {
    // Redirect freezed cpu to transplant
    transplant.io.tpu2cpu.freeze.tag <> rfile.rd.tag
    transplant.io.rfile.rs1_addr <> rfile.rd.port(0).addr
    transplant.io.rfile.rs2_addr <> rfile.rd.port(1).addr
    transplant.io.rfile.rs1_data <> rfile.rd.port(0).data
    transplant.io.rfile.rs2_data <> rfile.rd.port(1).data

    transplant.io.tpu2cpu.freeze.tag <> rfile.wr.tag
    transplant.io.rfile.w1_addr <> rfile.wr.addr
    transplant.io.rfile.w1_data <> rfile.wr.data
    transplant.io.rfile.w1_en <> rfile.wr.en

    // Freeze PSTATE, When not written by TPU
    pregsVec(transplant.io.tpu2cpu.freeze.tag) := pregsVec(transplant.io.tpu2cpu.freeze.tag)
    when(transplant.io.tpu2cpuStateReg.valid) {
      when(transplant.io.tpu2cpuStateReg.bits === TPU2STATE.r_PC) {
        pregsVec(transplant.io.tpu2cpu.freeze.tag).PC := transplant.io.tpu2cpuState.PC
      }.elsewhen(transplant.io.tpu2cpuStateReg.bits === TPU2STATE.r_SP) {
        pregsVec(transplant.io.tpu2cpu.freeze.tag).SP := transplant.io.tpu2cpuState.SP
      }.elsewhen(transplant.io.tpu2cpuStateReg.bits === TPU2STATE.r_NZCV) {
        pregsVec(transplant.io.tpu2cpu.freeze.tag).NZCV := transplant.io.tpu2cpuState.NZCV
      }
    }
  }

  //* DBG
  val dbg = IO(Output(new Bundle {
    val fetch = ValidTag(cfg.TAG_T, new FullStateBundle)
    val issue = ValidTag(cfg.TAG_T, new FullStateBundle)
    val issuingMem = Output(Bool())
    val commit = ValidTag(cfg.TAG_T, new FullStateBundle)
    val commitTransplant = Output(Valid(INST_T))
  }))

  dbg.fetch.valid := pipeline.mem.inst.req.valid
  dbg.fetch.tag := pipeline.mem.inst.req.bits.thread_id
  dbg.fetch.bits.get.rfile := rfile.dbg.rfileVec.get(dbg.fetch.tag)
  dbg.fetch.bits.get.regs := pregsVec(dbg.fetch.tag)

  dbg.issue.tag := pipeline.archstate.issue.sel.tag
  dbg.issue.valid := pipeline.archstate.issue.sel.valid
  dbg.issue.bits.get.rfile := rfile.dbg.rfileVec.get(dbg.issue.tag)
  dbg.issue.bits.get.regs := pregsVec(dbg.issue.tag)
  dbg.issuingMem := pipeline.dbg.issuingMem

  dbg.commit.tag := pipeline.archstate.commit.sel.tag
  dbg.commit.valid := pipeline.archstate.commit.sel.valid
  dbg.commit.bits.get.rfile := rfile.dbg.rfileVec.get(dbg.commit.tag)
  dbg.commit.bits.get.regs := pregsVec(dbg.commit.tag)
  dbg.commitTransplant.valid := pipeline.transplantIO.done.valid
  dbg.commitTransplant.bits := pipeline.transplantIO.done.bits.get
 // */
}