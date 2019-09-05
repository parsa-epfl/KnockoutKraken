// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.core.withReset
import chisel3.util.{DeqIO, Queue, RegEnable, Valid, log2Ceil}

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
  val exeReg   = Output(DeqIO(new EInst))
  val brReg    = Output(DeqIO(new BInst))
  val ldstUReg = Output(DeqIO(new MInst))

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
  val emptyPStateRegs = WireInit(PStateRegs())
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
  // Commitement stage
  // | Execute |        |            |
  val executer = Module(new ExecuteUnit())
  val exeReg = Module(new FReg(new EInst))
  // |         | Branch |            |
  val brancher = Module(new BranchUnit())
  val brReg = Module(new FReg(new BInst))
  // |         |        | Load Store |
  val ldstU = Module(new LoadStoreUnit())
  val ldstUReg = Module(new FReg(new MInst))
  // |  UNDEF  |        |            |
  val undefINSN = Wire(Valid(cfg.TAG_T))

  // Extra Regs and wires
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
  fetch.io.branch.valid := brReg.io.deq.valid
  fetch.io.branch.bits := brReg.io.deq.bits
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
  issuer.io.deq.ready := exeReg.io.enq.ready || brReg.io.enq.ready
  // Check for front pressure
  issuer.io.exeReg.bits  := exeReg.io.deq.bits
  issuer.io.exeReg.valid := exeReg.io.deq.valid

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
  // Register ExecuteUnit output
  exeReg.io.enq.valid := executer.io.einst.valid && issuer.io.deq.valid && exeReg.io.enq.ready
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
  // io.mem_req := ldstU.io.memReq
  ldstU.io.memRes.valid := false.B // TODO
  ldstU.io.memRes.bits.data := 0.U // TODO

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
      vec_rfile(cpu).waddr := exeReg.io.deq.bits.rd.bits
      vec_rfile(cpu).wdata := exeReg.io.deq.bits.res
    }.elsewhen(ldstUReg.io.deq.valid) {
      vec_rfile(cpu).waddr := ldstUReg.io.deq.bits.rd.bits
      vec_rfile(cpu).wdata := ldstUReg.io.deq.bits.res
    }.otherwise{
      vec_rfile(cpu).waddr := exeReg.io.deq.bits.rd.bits
      vec_rfile(cpu).wdata := exeReg.io.deq.bits.res
    }
    vec_rfile(cpu).wen := false.B
  }
  when(exeReg.io.deq.valid) {
    vec_rfile(exeReg.io.deq.bits.tag).wen := exeReg.io.deq.bits.rd.valid
  }.elsewhen(ldstUReg.io.deq.valid) {
    vec_rfile(ldstUReg.io.deq.bits.tag).wen := ldstUReg.io.deq.bits.rd.valid
  }
 
  // PState Regs
  when (exeReg.io.deq.valid && exeReg.io.deq.bits.nzcv.valid) {
    vec_pregs(exeReg.io.deq.bits.tag).NZCV := exeReg.io.deq.bits.nzcv.bits
  }

  val last_thread = Reg(cfg.TAG_T)
  // do not update PC when an instruction(branch/execute) instruction didn't executed
  // update PC
  when(brReg.io.deq.valid) {
    vec_pregs(brReg.io.deq.bits.tag).PC := (vec_pregs(brReg.io.deq.bits.tag).PC.zext + brReg.io.deq.bits.offset.asSInt).asUInt()
    last_thread := brReg.io.deq.bits.tag
  }.elsewhen(exeReg.io.deq.valid) {
    vec_pregs(exeReg.io.deq.bits.tag).PC := vec_pregs(exeReg.io.deq.bits.tag).PC + 4.U
    last_thread := exeReg.io.deq.bits.tag
  }.elsewhen(ldstUReg.io.deq.valid) {
    vec_pregs(ldstUReg.io.deq.bits.tag).PC := vec_pregs(ldstUReg.io.deq.bits.tag).PC + 4.U
    last_thread := exeReg.io.deq.bits.tag
  }

  // Start and stop ---------------
  when(tpu.io.tpu2cpu.fire) {
    fetch_en(tpu.io.tpu2cpu.fireTag) := true.B
  }
  // Cycle delay for commitement
  undefINSN.bits := RegNext(issued_dinst.tag)
  undefINSN.valid := RegNext(issuer.io.deq.valid && issued_dinst.itype === DECODE_CONTROL_SIGNALS.I_X)
  tpu.io.tpu2cpu.done := undefINSN.valid
  tpu.io.tpu2cpu.doneTag := undefINSN.bits
  when(issuer.io.deq.valid && issued_dinst.itype === DECODE_CONTROL_SIGNALS.I_X) {
    fetch_en(tpu.io.tpu2cpu.flushTag) := false.B
  }


  // Flushing ----------------------------------------------------------------
  issuer.io.flushTag := tpu.io.tpu2cpu.flushTag
  when(tpu.io.tpu2cpu.flush) {
    fetch.io.flush := fetch.io.deq.bits.tag === tpu.io.tpu2cpu.flushTag
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

  // DEBUG Signals ------------------------------------------------------------
  if(cfg.DebugSignals) {
    val procStateDBG = io.procStateDBG.get
    procStateDBG.fetchReg.ready := decReg.io.enq.ready
    procStateDBG.fetchReg.valid := fetch.io.deq.valid
    procStateDBG.fetchReg.bits  := fetch.io.deq.bits
    procStateDBG.decReg.ready   := issuer.io.enq.ready
    procStateDBG.decReg.valid   := decReg.io.deq.valid
    procStateDBG.decReg.bits    := decReg.io.deq.bits
    procStateDBG.issueReg.ready := exeReg.io.enq.ready || brReg.io.enq.ready
    procStateDBG.issueReg.valid := issuer.io.deq.valid
    procStateDBG.issueReg.bits  := issuer.io.deq.bits
    procStateDBG.exeReg.ready   := true.B
    procStateDBG.exeReg.valid   := exeReg.io.deq.valid
    procStateDBG.exeReg.bits    := exeReg.io.deq.bits
    procStateDBG.brReg.ready    := true.B
    procStateDBG.brReg.valid    := brReg.io.deq.valid
    procStateDBG.brReg.bits     := brReg.io.deq.bits
    procStateDBG.ldstUReg.ready := true.B
    procStateDBG.ldstUReg.valid := ldstUReg.io.deq.valid
    procStateDBG.ldstUReg.bits  := ldstUReg.io.deq.bits

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

    val comited = RegNext((exeReg.io.deq.valid || brReg.io.deq.valid || ldstUReg.io.deq.valid))
    procStateDBG.comited := comited
  }


}

