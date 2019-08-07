// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.core.withReset
import chisel3.util.{DeqIO, Queue, RegEnable, Valid, log2Ceil}

import common.PROCESSOR_TYPES._
import common.constBRAM.TDPBRAM36ParamDict
import common.{BRAM, BRAMConfig, BRAMPort, DECODE_CONTROL_SIGNALS}

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
  val inst_in  = Output(new DInst)

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
    val host2tp = new TransplantUnitHostIO

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
  val tp = Module(new TransplantUnit())

  // Reset | Clear thread state
  val flush = (tp.io.flush || reset.toBool)

  // Pipeline -----------------------------------------------
  // Fetch
  val fetch = Module(new FetchUnit())
  // Decode
  val decoder = Module(new DecodeUnit())
  val decReg = withReset(flush){Module(new Queue(new DInst, 1, pipe = true, flow = false))}
  // Issue
  val issuer = Module(new IssueUnit())
  // issueReg in unit
  // | Execute |        |            |
  val executer = Module(new ExecuteUnit())
  val exeReg = withReset(flush){Module(new Queue(new EInst, 1, pipe = true, flow = false))}
  // |         | Branch |            |
  val brancher = Module(new BranchUnit())
  val brReg   = withReset(flush){Module(new Queue(new BInst, 1, pipe = true, flow = false))}
  // |         |        | Load Store |
  val ldstU = Module(new LoadStoreUnit())
  val ldstUReg = withReset(flush){Module(new Queue(new MInst, 1, pipe = true, flow = false))}

  // Internal State -----------------------------------------
  // PState
  val vec_rfile = VecInit(Seq.fill(cfg.NB_THREADS)(Module(new RFile).io))
  val vec_pregs = RegInit(VecInit(Seq.fill(cfg.NB_THREADS)(Wire(new PStateRegs()).empty)))
  // PCs
  val curr_PC = Wire(DATA_T)
  val next_PC = Wire(DATA_T)

  // Interconnect -------------------------------------------

  // HOST <> TP
  io.ppageBRAM <> ppage.io.getPort(0)
  io.stateBRAM <> state.io.getPort(0)

  io.host2tp <> tp.io.host2tp

  // Transplant Unit TP <> CPU
  tp.io.tp2cpu.done := false.B
  tp.io.tp2cpu.doneTag := 0.U

  // fetch <> ppage
  ppage.io.getPort(1).dataIn.get := 0.U
  ppage.io.getPort(1).addr := fetch.io.addr
  ppage.io.getPort(1).en := fetch.io.rd_en
  ppage.io.getPort(1).writeEn.get := false.B
  fetch.io.data := ppage.io.getPort(1).dataOut.get

  // transplant unit <> pstate
  state.io.getPort(1) <> tp.io.bram_port

  val fetch_en = withReset(flush){RegInit(false.B)}
  when(tp.io.fetch_start) { fetch_en := true.B }
  tp.io.tp_req := Mux(RegNext(fetch_en.toBool()), decoder.io.tp_req, false.B)

  // IRAM(ppage)-> Fetch TODO
  fetch.io.en := fetch_en
  fetch.io.PC := next_PC
  fetch.io.inst.ready := true.B // TODO: for now always ready ( change decoder to wait for branch instruction)
  fetch.io.tag_in := 0.U // TODO

  // Fetch -> Decode TODO
  decoder.io.inst := Mux(fetch.io.inst.valid, fetch.io.inst.bits, INST_X)
  decoder.io.tag := fetch.io.tag_out

  // Register instruction (DInst) // TODO
  // consumeFetch := decReg.io.enq.ready TODO
  decReg.io.enq.valid := decoder.io.dinst.itype =/= DECODE_CONTROL_SIGNALS.I_X
  decReg.io.enq.bits  := decoder.io.dinst

  // Decode -> Issue
  issuer.io.enq <> decReg.io.deq
  issuer.io.flush := flush

  // Execute : Issue -> Execute
  val issued_dinst = issuer.io.deq.bits
  val issued_tag = issued_dinst.tag
  val tag_select = Mux(tp.io.tp_en, tp.io.tp_tag, issued_tag)
  issuer.io.deq.ready := exeReg.io.enq.ready || brReg.io.enq.ready
  // Check for front pressure
  issuer.io.exeReg.bits := exeReg.io.deq.bits
  issuer.io.exeReg.valid :=  exeReg.io.deq.valid

  /** Execute */
  val rs1_addr = Mux(tp.io.tp_en, tp.io.tp_reg_raddr, issued_dinst.rs1)
  val rs2_addr = Mux(tp.io.tp_en, tp.io.tp_reg_raddr, issued_dinst.rs2)
  // connect rfile read(address) interface
  vec_rfile map { case rfile =>
    rfile.rs1_addr := rs1_addr
    rfile.rs2_addr := rs2_addr
  }
  // Read register data from rfile
  val rVal1 = vec_rfile(tag_select).rs1_data
  val rVal2 = vec_rfile(tag_select).rs2_data

  // reading register: transplant
  tp.io.tp_reg_rdata := rVal1
  tp.io.tp_reg_rdata := rVal2

  // connect executeUnit interface
  executer.io.rVal1 := rVal1
  executer.io.rVal2 := rVal2
  executer.io.dinst := issued_dinst

  // Register ExecuteUnit output
  exeReg.io.enq.valid := executer.io.einst.valid && issuer.io.deq.valid
  exeReg.io.enq.bits  := executer.io.einst.bits

  // connect BranchUnit interface
  brancher.io.nzcv := vec_pregs(issued_dinst.tag).NZCV
  brancher.io.dinst := issued_dinst

  // Register BranchUnit output
  brReg.io.enq.valid := issuer.io.deq.valid && brancher.io.binst.valid
  brReg.io.enq.bits.offset := brancher.io.binst.bits.offset
  brReg.io.enq.bits.tag := brancher.io.binst.bits.tag

  // connect LDSTUnit interface
  ldstU.io.dinst := issued_dinst
  ldstU.io.rVal1 := rVal1
  ldstU.io.rVal2 := rVal2
  ldstU.io.pc    := curr_PC
  io.mem_req := ldstU.io.memReq
  ldstU.io.memRes := io.mem_res

  // testing only
  ldstU.io.write_tlb_vaddr := DontCare
  ldstU.io.write_tlb_entry := DontCare

  // Register LDSTUnit output MInst
  ldstUReg.io.enq.valid := ldstU.io.minst.valid && issuer.io.deq.valid // TODO: check condition
  ldstUReg.io.enq.bits := ldstU.io.minst.bits
  ldstUReg.io.deq.ready := true.B // This register is just for debugging, flush continously

  // Writeback : Execute -> PState
  // NOTE: Always dequeue executions stage
  exeReg.io.deq.ready := true.B
  brReg.io.deq.ready := true.B

  // connect RFile's write interface
  val waddr = Mux(tp.io.tp_en, tp.io.tp_reg_waddr, exeReg.io.deq.bits.rd)
  val wdata = Mux(tp.io.tp_en, tp.io.tp_reg_wdata, exeReg.io.deq.bits.res)
  vec_rfile map { case rfile =>
    rfile.waddr := waddr
    rfile.wdata := wdata
    rfile.wen := false.B
  }
  val wen = Mux(tp.io.tp_en, tp.io.tp_reg_wen, exeReg.io.deq.bits.rd_en && exeReg.io.deq.valid)
  vec_rfile(tag_select).wen := wen

  // PState Regs
  when (exeReg.io.deq.valid && exeReg.io.deq.bits.nzcv_en) {
    vec_pregs(exeReg.io.deq.bits.tag).NZCV := exeReg.io.deq.bits.nzcv
  }

  val last_thread = Reg(cfg.TAG_T)
  // do not update PC when an instruction(branch/execute) instruction didn't executed
  curr_PC := vec_pregs(last_thread).PC
  next_PC := vec_pregs(last_thread).PC

  // pstate in tp mode
  tp.io.tp_pstate_in.PC := vec_pregs(tp.io.tp_tag).PC
  tp.io.tp_pstate_in.SP := vec_pregs(tp.io.tp_tag).SP
  tp.io.tp_pstate_in.EL := vec_pregs(tp.io.tp_tag).EL
  tp.io.tp_pstate_in.NZCV := vec_pregs(tp.io.tp_tag).NZCV
  when(tp.io.tp_en && tp.io.tp_pstate_wen){
    vec_pregs(tp.io.tp_tag).PC := tp.io.tp_pstate_out.PC
    vec_pregs(tp.io.tp_tag).SP := tp.io.tp_pstate_out.SP
    vec_pregs(tp.io.tp_tag).EL := tp.io.tp_pstate_out.EL
    vec_pregs(tp.io.tp_tag).NZCV := tp.io.tp_pstate_out.NZCV
  }
  // update PC
  when(brReg.io.deq.valid) {
    vec_pregs(brReg.io.deq.bits.tag).PC := (vec_pregs(brReg.io.deq.bits.tag).PC.zext + brReg.io.deq.bits.offset.asSInt).asUInt()
    curr_PC := vec_pregs(brReg.io.deq.bits.tag).PC
    next_PC := (vec_pregs(brReg.io.deq.bits.tag).PC.zext + brReg.io.deq.bits.offset.asSInt).asUInt()
    last_thread := brReg.io.deq.bits.tag
  }.elsewhen(exeReg.io.deq.valid) {
    vec_pregs(exeReg.io.deq.bits.tag).PC := vec_pregs(exeReg.io.deq.bits.tag).PC + 1.U
    curr_PC := vec_pregs(exeReg.io.deq.bits.tag).PC
    next_PC := vec_pregs(exeReg.io.deq.bits.tag).PC + 1.U
    last_thread := exeReg.io.deq.bits.tag
  }

  // DEBUG Signals
  if(cfg.DebugSignals) {
    io.procStateDBG.get.inst_in   := decoder.io.dinst

    io.procStateDBG.get.decReg   <> decReg.io.deq
    io.procStateDBG.get.issueReg <> issuer.io.deq
    io.procStateDBG.get.exeReg   <> exeReg.io.deq
    io.procStateDBG.get.brReg    <> brReg.io.deq
    io.procStateDBG.get.ldstUReg   <> ldstUReg.io.deq

    io.procStateDBG.get.curr_PC := curr_PC
    io.procStateDBG.get.next_PC := next_PC
  }


}

