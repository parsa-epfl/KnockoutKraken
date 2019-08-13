// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.core.withReset
import chisel3.util.{DeqIO, Queue, RegEnable, Valid, log2Ceil}

import common.PROCESSOR_TYPES._
import common.constBRAM.TDPBRAM36ParamDict
import common.{BRAM, BRAMConfig, BRAMPort, DECODE_CONTROL_SIGNALS, FReg}

case class ProcConfig(val NB_THREADS : Int = 4, val DebugSignals : Boolean = false) {
  val ppageBRAMc = new BRAMConfig(Seq(TDPBRAM36ParamDict(36), TDPBRAM36ParamDict(36)))
  val stateBRAMc = new BRAMConfig(Seq(TDPBRAM36ParamDict(36), TDPBRAM36ParamDict(36)))

  val NB_THREAD_W = log2Ceil(NB_THREADS) // 4 Threads
  def TAG_T = UInt(NB_THREAD_W.W)
  val TAG_X = 0.U(NB_THREAD_W.W)
  val TAG_VEC_X = 0.U(NB_THREADS.W)
  def TAG_VEC_T = UInt(NB_THREADS.W)
}

class ProcStateDBG(implicit val cfg : ProcConfig) extends Bundle
{
  val fetchReg = Output(DeqIO(new FInst))
  val decReg   = Output(DeqIO(new DInst))
  val issueReg = Output(DeqIO(new DInst))
  val exeReg   = Output(DeqIO(new EInst))
  val brReg    = Output(DeqIO(new BInst))
  val ldstUReg = Output(DeqIO(new MInst))

  val curr_PC  = Output(DATA_T)
  val next_PC  = Output(DATA_T)
}

/** Processor
  *
  */
class Proc(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new Bundle {
    // memory interface
    val mem_req = Output(Valid(new MemRes))
    val mem_res = Input(Valid(new MemRes))

    // BRAM Host Ports
    val ppageBRAM = new BRAMPort(0)(cfg.ppageBRAMc)
    val stateBRAM = new BRAMPort(0)(cfg.stateBRAMc)

    // AXI Host Communication
    val host2tpu = new TransplantUnitHostIO

    // Debug
    val procStateDBG = if(cfg.DebugSignals) Some(new ProcStateDBG) else None
  })

  // System modules -----------------------------------------

  // Host modules
  // BRAM Program page
  val ppage = Module(new BRAM()(cfg.ppageBRAMc))
  // BRAM PC State
  val state = Module(new BRAM()(cfg.stateBRAMc))
  // Transplant Unit
  val tpu = Module(new TransplantUnit())

  // Internal State -----------------------------------------
  // PState
  val emptyPStateRegs = Wire(new PStateRegs()).empty
  val vec_rfile = VecInit(Seq.fill(cfg.NB_THREADS)(Module(new RFile).io))
  val vec_pregs = RegInit(VecInit(Seq.fill(cfg.NB_THREADS)(emptyPStateRegs)))

  // Pipeline -----------------------------------------------
  val specPCReg = RegInit(false.B)
  // Fetch
  val fetch = Module(new FetchUnit())
  // fetchReg in Fetch Unit
  // Decode
  val decoder = Module(new DecodeUnit())
  val decReg = Module(new FReg(new DInst))
  // Issue
  val issuer = Module(new IssueUnit())
  // issueReg in Issue Unit
  // | Execute |        |            |
  val executer = Module(new ExecuteUnit())
  val exeReg = Module(new FReg(new EInst))
  // |         | Branch |            |
  val brancher = Module(new BranchUnit())
  val brReg = Module(new FReg(new BInst))
  // |         |        | Load Store |
  val ldstU = Module(new LoadStoreUnit())
  val ldstUReg = Module(new FReg(new MInst))

  // Extra Regs and wires
  val fake_PC = RegInit(0.U)
  val fetch_en = RegInit(VecInit(Seq.fill(cfg.NB_THREADS)(false.B)))

  // Interconnect -------------------------------------------

  // HOST <> TP
  io.ppageBRAM <> ppage.io.getPort(0)
  io.stateBRAM <> state.io.getPort(0)

  io.host2tpu <> tpu.io.host2tpu

  // Transplant Unit TP <> CPU
  tpu.io.stateBRAM <> state.io.getPort(1)
  tpu.io.tpu2cpu.done := false.B
  tpu.io.tpu2cpu.doneTag := 0.U

  // PState -> Fetch
  fetch.io.ppageBRAM <> ppage.io.getPort(1)

  val fetch_tag = WireInit(0.U)
  for (i <- 0 until cfg.NB_THREADS) {
    when(fetch_en(i)) {
      fetch_tag := i.U
    }
  }
  fetch.io.tagIn := fetch_tag
  fetch.io.en := fetch_en(fetch_tag)
  fetch.io.PC := fake_PC
  when(fetch.io.incr) {
    fake_PC := fake_PC + 4.U
  }

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
  issuer.io.deq.ready := exeReg.io.enq.ready || brReg.io.enq.ready
  // Check for front pressure
  issuer.io.exeReg.bits := exeReg.io.deq.bits
  issuer.io.exeReg.valid :=  exeReg.io.deq.valid

  /** Execute */
  // connect rfile read(address) interface
  vec_rfile map { case rfile =>
    rfile.rs1_addr := issued_dinst.rs1
    rfile.rs2_addr := issued_dinst.rs2
  }
  // Read register data from rfile
  val rVal1 = vec_rfile(issued_dinst.tag).rs1_data
  val rVal2 = vec_rfile(issued_dinst.tag).rs2_data

  // connect executeUnit interface
  executer.io.dinst := issued_dinst
  executer.io.rVal1 := rVal1
  executer.io.rVal2 := rVal2
  // Register ExecuteUnit output
  exeReg.io.enq.valid := executer.io.einst.valid && issuer.io.deq.valid
  exeReg.io.enq.bits  := executer.io.einst.bits

  // connect BranchUnit interface
  brancher.io.dinst := issued_dinst
  brancher.io.nzcv := vec_pregs(issued_dinst.tag).NZCV

  // Register BranchUnit output
  brReg.io.enq.valid := issuer.io.deq.valid && brancher.io.binst.valid
  brReg.io.enq.bits.offset := brancher.io.binst.bits.offset
  brReg.io.enq.bits.tag := brancher.io.binst.bits.tag

  // connect LDSTUnit interface
  ldstU.io.dinst.bits := issuer.io.deq.bits
  ldstU.io.dinst.valid := issuer.io.deq.valid
  ldstU.io.rVal1 := rVal1
  ldstU.io.rVal2 := rVal2
  ldstU.io.pc    := vec_pregs(issued_dinst.tag).PC
  io.mem_req := ldstU.io.memReq
  ldstU.io.memRes := io.mem_res

  // testing only
  ldstU.io.write_tlb_vaddr := DontCare
  ldstU.io.write_tlb_entry := DontCare

  // Register LDSTUnit output MInst
  ldstUReg.io.enq.valid := ldstU.io.minst.valid && issuer.io.deq.valid // TODO: check condition
  ldstUReg.io.enq.bits := ldstU.io.minst.bits

  // ---- Commit STATE ----
  // Writeback : Execute -> PState
  // NOTE: Always dequeue executions stage, because always ready for writeback
  exeReg.io.deq.ready := true.B
  brReg.io.deq.ready := true.B
  ldstUReg.io.deq.ready := true.B

  // connect RFile's write interface
  for(cpu <- 0 until cfg.NB_THREADS) {
    when(exeReg.io.deq.valid) {
      vec_rfile(cpu).waddr := exeReg.io.deq.bits.rd
      vec_rfile(cpu).wdata := exeReg.io.deq.bits.res
    }.elsewhen(ldstUReg.io.deq.valid) {
      vec_rfile(cpu).waddr := ldstUReg.io.deq.bits.rd
      vec_rfile(cpu).wdata := ldstUReg.io.deq.bits.res
    }.otherwise{
      vec_rfile(cpu).waddr := exeReg.io.deq.bits.rd
      vec_rfile(cpu).wdata := exeReg.io.deq.bits.res
    }
    vec_rfile(cpu).wen := false.B
  }
  when(exeReg.io.deq.valid) {
    vec_rfile(exeReg.io.deq.bits.tag).wen := exeReg.io.deq.bits.rd_en
  }.elsewhen(ldstUReg.io.deq.valid) {
    vec_rfile(ldstUReg.io.deq.bits.tag).wen := ldstUReg.io.deq.bits.rd_en
  }
 
  // PState Regs
  when (exeReg.io.deq.valid && exeReg.io.deq.bits.nzcv_en) {
    vec_pregs(exeReg.io.deq.bits.tag).NZCV := exeReg.io.deq.bits.nzcv
  }

  val last_thread = Reg(cfg.TAG_T)
  // do not update PC when an instruction(branch/execute) instruction didn't executed
  // update PC
  when(brReg.io.deq.valid) {
    fake_PC := (vec_pregs(brReg.io.deq.bits.tag).PC.zext + brReg.io.deq.bits.offset.asSInt).asUInt()
    vec_pregs(brReg.io.deq.bits.tag).PC := (vec_pregs(brReg.io.deq.bits.tag).PC.zext + brReg.io.deq.bits.offset.asSInt).asUInt()
    last_thread := brReg.io.deq.bits.tag
  }.elsewhen(exeReg.io.deq.valid) {
    vec_pregs(exeReg.io.deq.bits.tag).PC := vec_pregs(exeReg.io.deq.bits.tag).PC + 4.U
    last_thread := exeReg.io.deq.bits.tag
  }.elsewhen(ldstUReg.io.deq.valid) {
    vec_pregs(ldstUReg.io.deq.bits.tag).PC := vec_pregs(ldstUReg.io.deq.bits.tag).PC + 4.U
    last_thread := exeReg.io.deq.bits.tag
  }


  // Flushing ----------------------------------------------------------------
  issuer.io.flushTag := tpu.io.tpu2cpu.flushTag
  when(tpu.io.tpu2cpu.flush) {
    fetch.io.flush := tpu.io.tpu2cpu.flush
    issuer.io.flush := tpu.io.tpu2cpu.flush
    decReg.io.flush := decReg.io.deq.bits.tag === tpu.io.tpu2cpu.flushTag
    brReg.io.flush := brReg.io.deq.bits.tag === tpu.io.tpu2cpu.flushTag
    exeReg.io.flush := exeReg.io.deq.bits.tag === tpu.io.tpu2cpu.flushTag
    ldstUReg.io.flush := ldstUReg.io.deq.bits.tag === tpu.io.tpu2cpu.flushTag
  }.otherwise {
    fetch.io.flush := false.B
    issuer.io.flush := false.B
    decReg.io.flush := false.B
    brReg.io.flush := false.B
    exeReg.io.flush := false.B
    ldstUReg.io.flush := false.B
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

  // Start and stop
  when(tpu.io.tpu2cpu.fire) {
    fake_PC := vec_pregs(tpu.io.tpu2cpu.fireTag).PC
    fetch_en(tpu.io.tpu2cpu.fireTag) := true.B
  }
  tpu.io.tpu2cpu.done := issuer.io.deq.valid && issued_dinst.itype === DECODE_CONTROL_SIGNALS.I_X
  tpu.io.tpu2cpu.doneTag := issued_dinst.tag
  when(issuer.io.deq.valid && issued_dinst.itype === DECODE_CONTROL_SIGNALS.I_X) {
    fetch_en(tpu.io.tpu2cpu.flushTag) := false.B
  }

  // DEBUG Signals ------------------------------------------------------------
  if(cfg.DebugSignals) {
    io.procStateDBG.get.fetchReg <> fetch.io.deq
    io.procStateDBG.get.decReg   <> decReg.io.deq
    io.procStateDBG.get.issueReg <> issuer.io.deq
    io.procStateDBG.get.exeReg   <> exeReg.io.deq
    io.procStateDBG.get.brReg    <> brReg.io.deq
    io.procStateDBG.get.ldstUReg <> ldstUReg.io.deq

    io.procStateDBG.get.curr_PC := vec_pregs(last_thread).PC
    io.procStateDBG.get.next_PC := vec_pregs(last_thread).PC
  }


}

