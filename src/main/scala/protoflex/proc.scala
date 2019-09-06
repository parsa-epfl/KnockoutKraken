// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.core.withReset
import chisel3.util.{DeqIO, Queue, RegEnable, Valid, log2Ceil}

import common.DECODE_CONTROL_SIGNALS.I_X
import common.PROCESSOR_TYPES._
import common.constBRAM.TDPBRAM36ParamDict
import common.{BRAM, BRAMConfig, BRAMPortAXI, DECODE_CONTROL_SIGNALS, FReg}

case class ProcConfig(val NB_THREADS : Int = 4, val DebugSignals : Boolean = false, EntriesTLB: Int = 32) {
  val ppageBRAMc = new BRAMConfig(Seq(TDPBRAM36ParamDict(36), TDPBRAM36ParamDict(36)))
  val stateBRAMc = new BRAMConfig(Seq(TDPBRAM36ParamDict(36), TDPBRAM36ParamDict(36)))

  // Threads
  val NB_THREAD_W = log2Ceil(NB_THREADS) // 4 Threads
  def TAG_T = UInt(NB_THREAD_W.W)
  val TAG_X = 0.U(NB_THREAD_W.W)
  val TAG_VEC_X = 0.U(NB_THREADS.W)
  def TAG_VEC_T = UInt(NB_THREADS.W)

  // Memory
  val TLB_NB_ENTRY = EntriesTLB
  val TLB_NB_ENTRY_W = log2Ceil(TLB_NB_ENTRY)
}

class ProcStateDBG(implicit val cfg : ProcConfig) extends Bundle
{
  val fetchReg = Output(DeqIO(new FInst))
  val decReg   = Output(DeqIO(new DInst))
  val issueReg = Output(DeqIO(new DInst))
  val commitReg = Output(DeqIO(new CommitInst))

  val vecPRegs = Output(Vec(cfg.NB_THREADS, new PStateRegs))
  val vecRFiles = Output(Vec(cfg.NB_THREADS, Vec(REG_N, DATA_T)))

  val tuWorking = Output(Bool())
  val tuWorkingTag = Output(cfg.TAG_T)

  val comited = Output(Bool())
}

/** Processor
  *
  */
class Proc(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new Bundle {
    // memory interface TODO
    // val mem_req = Output(Valid(new MemRes))
    // val mem_res = Input(Valid(new MemRes))

    // BRAM Host Ports
    val ppageBRAM = new BRAMPortAXI(0)(cfg.ppageBRAMc)
    val stateBRAM = new BRAMPortAXI(0)(cfg.stateBRAMc)

    // AXI Host Communication
    val host2tpu = new TransplantUnitHostIO

    // Debug
    val procStateDBG = if(cfg.DebugSignals) Some(new ProcStateDBG) else None
  })

  // System modules -bits----------------------------------------

  // Host modules
  // BRAM Program page
  val ppage = Module(new BRAM()(cfg.ppageBRAMc))
  // BRAM PC State
  val state = Module(new BRAM()(cfg.stateBRAMc))
  // Transplant Unit
  val tpu = Module(new TransplantUnit())

  // Internal State -----------------------------------------
  // PState
  val emptyPStateRegs = WireInit(PStateRegs())
  val vec_rfile = VecInit(Seq.fill(cfg.NB_THREADS)(Module(new RFile).io))
  val vec_pregs = RegInit(VecInit(Seq.fill(cfg.NB_THREADS)(emptyPStateRegs)))

  // Pipeline -----------------------------------------------
  val specPCReg = RegInit(false.B)
  // Fetch
  val fetch = Module(new FetchUnit())
  // fetchReg in Fetch Unit
  // Decodeqde
  val decoder = Module(new DecodeUnit())
  val decReg = Module(new FReg(new DInst))
  // Issue
  val issuer = Module(new IssueUnit())
  // issueReg in Issue Unit
  // Commitement stage
  // |         |        |            |
  // | Execute |        |            |
  // |         | Branch |            |
  // |         |        | Load Store |
  // |         |        |            |
  val executer = Module(new ExecuteUnit())
  val brancher = Module(new BranchUnit())
  val ldstU = Module(new LoadStoreUnit())
  val commitReg = Module(new FReg(new CommitInst))

  // Extra Regs and wires
  val fetch_en = RegInit(VecInit(Seq.fill(cfg.NB_THREADS)(false.B)))

  // Interconnect -------------------------------------------

  //io HOST <> TP
  io.ppageBRAM <> ppage.io.getPort(0)
  io.stateBRAM <> state.io.getPort(0)

  io.host2tpu <> tpu.io.host2tpu

  // Transplant Unit TP <> CPU
  tpu.io.stateBRAM <> state.io.getPort(1)
  tpu.io.tpu2cpu.done := false.B
  tpu.io.tpu2cpu.doneTag := 0.U

  // PState + Branching -> Fetch
  val fetch_tag = WireInit(0.U) // TODO, better arbiter for the tag to fetch
  for (i <- 0 until cfg.NB_THREADS) {
    when(fetch_en(i)) {
      fetch_tag := i.U
    }
  }

  fetch.io.ppageBRAM <> ppage.io.getPort(1)
  fetch.io.vecPC zip vec_pregs foreach {case (fPC, pPC) => fPC := pPC.PC}

  fetch.io.tagIn := fetch_tag
  fetch.io.en := fetch_en(fetch_tag)
  fetch.io.commitReg.bits := commitReg.io.deq.bits
  fetch.io.commitReg.valid := commitReg.io.deq.valid
  fetch.io.fire.valid := tpu.io.tpu2cpu.fire
  fetch.io.fire.bits := tpu.io.tpu2cpu.fireTag

  // Fetch -> Decode
  decoder.io.finst := fetch.io.deq.bits
  decReg.io.enq.bits := decoder.io.dinst

  fetch.io.deq.ready := decReg.io.enq.ready
  decReg.io.enq.valid := fetch.io.deq.valid

  // Decode -> Issue
  issuer.io.enq <> decReg.io.deq

  // Execute : Issue -> Execute
  val issued_dinst = issuer.io.deq.bits
  val issued_tag = issued_dinst.tag
  issuer.io.deq.ready := commitReg.io.enq.ready
  issuer.io.commitReg.bits := commitReg.io.deq.bits
  issuer.io.commitReg.valid := commitReg.io.deq.valid
 
  /** Execute */
  // connect rfile read(address) interface
  vec_rfile map { case rfile =>
    rfile.rs1_addr := issued_dinst.rs1.bits
    rfile.rs2_addr := issued_dinst.rs2.bits
  }
  // Read register data from rfile
  val rVal1 = vec_rfile(issued_dinst.tag).rs1_data
  val rVal2 = vec_rfile(issued_dinst.tag).rs2_data

  // connect executeUnit interface
  executer.io.dinst := issued_dinst
  executer.io.rVal1 := rVal1
  executer.io.rVal2 := rVal2

  // connect BranchUnit interface
  brancher.io.dinst := issued_dinst
  brancher.io.nzcv := vec_pregs(issued_dinst.tag).NZCV

  // connect LDSTUnit interface
  ldstU.io.dinst.bits := issuer.io.deq.bits
  ldstU.io.dinst.valid := issuer.io.deq.valid
  ldstU.io.rVal1 := rVal1
  ldstU.io.rVal2 := rVal2
  // io.mem_req := ldstU.io.memReq
  ldstU.io.memRes.valid := false.B // TODO
  ldstU.io.memRes.bits.data := 0.U // TODO

  // testing only
  ldstU.io.write_tlb_vaddr := DontCare
  ldstU.io.write_tlb_entry := DontCare

  // CommitReg
  commitReg.io.enq.bits.exe := executer.io.einst
  commitReg.io.enq.bits.br := brancher.io.binst
  commitReg.io.enq.bits.mem := ldstU.io.minst
  commitReg.io.enq.bits.undef := issued_dinst.itype === I_X
  commitReg.io.enq.bits.tag := issued_tag
  commitReg.io.enq.valid := issuer.io.deq.valid

  commitReg.io.deq.ready := true.B

  // ---- Commit STATE ----
  // Writeback : Execute -> PState

  // connect RFile's write interface
  val commitExec = commitReg.io.deq.bits.exe
  val commitMem = commitReg.io.deq.bits.mem
  val commitBr = commitReg.io.deq.bits.br
  val commitTag = commitReg.io.deq.bits.tag
  for(cpu <- 0 until cfg.NB_THREADS) {
    when(commitExec.valid) {
      vec_rfile(cpu).waddr := commitExec.bits.rd.bits
      vec_rfile(cpu).wdata := commitExec.bits.res
    }.elsewhen(commitMem.valid) {
      vec_rfile(cpu).waddr := commitMem.bits.rd.bits
      vec_rfile(cpu).wdata := commitMem.bits.res
    }.otherwise {
      vec_rfile(cpu).waddr := commitExec.bits.rd.bits
      vec_rfile(cpu).wdata := commitExec.bits.res
    }
    vec_rfile(cpu).wen := false.B
  }

  when (commitReg.io.deq.valid) {
    vec_rfile(commitTag).wen :=
      (commitExec.valid && commitExec.bits.rd.valid) ||
      (commitMem.valid && commitMem.bits.rd.valid)

    when(commitExec.valid && commitExec.bits.nzcv.valid) {
      vec_pregs(commitTag).NZCV := commitExec.bits.nzcv.bits
    }

    when(commitBr.valid) {
      vec_pregs(commitTag).PC := (vec_pregs(commitTag).PC.zext + commitBr.bits.offset.asSInt).asUInt()
    }.otherwise {
      vec_pregs(commitTag).PC := (vec_pregs(commitTag).PC.zext + 4.S).asUInt()
    }
  }

  // Start and stop ---------------
  when(tpu.io.tpu2cpu.fire) {
    fetch_en(tpu.io.tpu2cpu.fireTag) := true.B
  }
  // Cycle delay for commitement
  tpu.io.tpu2cpu.done := commitReg.io.deq.valid && commitReg.io.deq.bits.undef
  tpu.io.tpu2cpu.doneTag := commitReg.io.deq.bits.tag
  when(issuer.io.deq.valid && issued_dinst.itype === I_X) {
    fetch_en(tpu.io.tpu2cpu.flushTag) := false.B
  }


  // Flushing ----------------------------------------------------------------
  issuer.io.flushTag := tpu.io.tpu2cpu.flushTag
  when(tpu.io.tpu2cpu.flush) {
    fetch.io.flush := fetch.io.deq.bits.tag === tpu.io.tpu2cpu.flushTag
    issuer.io.flush := tpu.io.tpu2cpu.flush
    decReg.io.flush := decReg.io.deq.bits.tag === tpu.io.tpu2cpu.flushTag
    commitReg.io.flush := commitReg.io.deq.bits.tag === tpu.io.tpu2cpu.flushTag
  }.otherwise {
    fetch.io.flush := false.B
    issuer.io.flush := false.B
    decReg.io.flush := false.B
    commitReg.io.flush := false.B
  }

  // Transplant Activated ----------------------------------------------------
  // Default Inputs
  tpu.io.cpu2tpuState := vec_pregs(tpu.io.tpu2cpu.freezeTag)
  tpu.io.rfile.rs1_data := vec_rfile(0).rs1_data
  tpu.io.rfile.rs2_data := vec_rfile(0).rs2_data
  // When working
  when(tpu.io.tpu2cpu.freeze) {
    // Redirect freezed cpu to tpu
    vec_rfile(tpu.io.tpu2cpu.freezeTag) <> tpu.io.rfile
    // Freeze PSTATE, When not written by TPU
    vec_pregs(tpu.io.tpu2cpu.freezeTag) := vec_pregs(tpu.io.tpu2cpu.freezeTag)
    when(tpu.io.tpu2cpuStateReg.valid) {
      when(tpu.io.tpu2cpuStateReg.bits === TPU2STATE.r_PC) {
        vec_pregs(tpu.io.tpu2cpu.freezeTag).PC := tpu.io.tpu2cpuState.PC
      }.elsewhen(tpu.io.tpu2cpuStateReg.bits === TPU2STATE.r_SP_EL_NZCV){
        vec_pregs(tpu.io.tpu2cpu.freezeTag).SP := tpu.io.tpu2cpuState.SP
        vec_pregs(tpu.io.tpu2cpu.freezeTag).EL := tpu.io.tpu2cpuState.EL
        vec_pregs(tpu.io.tpu2cpu.freezeTag).NZCV := tpu.io.tpu2cpuState.NZCV
      }
    }
  }

  // DEBUG Signals ------------------------------------------------------------
  if(cfg.DebugSignals) {
    val procStateDBG = io.procStateDBG.get
    procStateDBG.fetchReg.ready := decReg.io.enq.ready
    procStateDBG.fetchReg.valid := fetch.io.deq.valid
    procStateDBG.fetchReg.bits  := fetch.io.deq.bits
    procStateDBG.decReg.ready   := issuer.io.enq.ready
    procStateDBG.decReg.valid   := decReg.io.deq.valid
    procStateDBG.decReg.bits    := decReg.io.deq.bits
    procStateDBG.issueReg.ready := commitReg.io.enq.ready
    procStateDBG.issueReg.valid := issuer.io.deq.valid
    procStateDBG.issueReg.bits  := issuer.io.deq.bits
    procStateDBG.commitReg.ready   := true.B
    procStateDBG.commitReg.valid   := commitReg.io.deq.valid
    procStateDBG.commitReg.bits    := commitReg.io.deq.bits

    // Processor State (XREGS + PSTATE)
    val vecRFiles = Wire(Vec(cfg.NB_THREADS, Vec(REG_N, DATA_T)))
    for(cpu <- 0 until cfg.NB_THREADS) {
      vecRFiles(cpu) := vec_rfile(cpu).vecRFile.get
    }
    tpu.io.rfile.vecRFile.get := vecRFiles(0) // Give a default assignement
    procStateDBG.vecRFiles := vecRFiles
    procStateDBG.vecPRegs := vec_pregs
    procStateDBG.tuWorking := tpu.io.tpu2cpu.freeze
    procStateDBG.tuWorkingTag := tpu.io.tpu2cpu.freezeTag

    val comited = RegNext(commitReg.io.deq.valid)
    procStateDBG.comited := comited
  }


}

class CommitInst(implicit val cfg : ProcConfig) extends Bundle {
  val exe = Valid(new EInst)
  val br = Valid(new BInst)
  val mem = Valid(new MInst)
  val undef = Output(Bool())
  val tag = Output(cfg.TAG_T)
}

