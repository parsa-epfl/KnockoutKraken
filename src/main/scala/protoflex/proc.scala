// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.core.withReset
import chisel3.util.{DeqIO, Queue, RegEnable, Valid}
import common.PROCESSOR_TYPES._
import common.constBRAM.TDPBRAM36ParamDict
import common.{BRAM, BRAMConfig, BRAMPort}

case class ProcConfig(widthExample: Int) {
   val ppageBRAMc = new BRAMConfig(Seq(TDPBRAM36ParamDict(36), TDPBRAM36ParamDict(36)))
   val stateBRAMc = new BRAMConfig(Seq(TDPBRAM36ParamDict(36), TDPBRAM36ParamDict(36)))
  // Move from object describing types to trait implementing methods
  // From argument passed to the ProcConfig ?
  def makeExUInt = UInt(widthExample.W)
}

/** Processor
  *
  */
class Proc(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new Bundle {
    // TODO: 3 signals
    val tag  = Input(TAG_T)
    val valid = Input(Bool())
    val ready = Output(Bool())

    // State
    val inst_in   = Output(new DInst)
    val dec_reg   = Flipped(DeqIO(new DInst))
    val issue_reg = Flipped(DeqIO(new DInst))
    val exe_reg   = Flipped(DeqIO(new EInst))
    val br_reg    = Flipped(DeqIO(new BInst))
    val lsu_reg   = Flipped(DeqIO(new MInst))
    val curr_PC   = Output(DATA_T)
    val next_PC   = Output(DATA_T)
    val wen       = Output(Bool())

    // memory interface
    val mem_req = Output(Valid(new MemRes))
    val mem_res = Input(Valid(new MemRes))

    // BRAM Ports
    val ppage_bram = new BRAMPort(0)(cfg.ppageBRAMc)
    val state_bram = new BRAMPort(0)(cfg.stateBRAMc)

    // AXI Host Communication
    val tp_start = Input(Bool())
    val tp_done = Output(Bool())
  })

  // Program page BRAM
  val ppage = Module(new BRAM()(cfg.ppageBRAMc))
  ppage.io.getPort(0) <> io.ppage_bram

  // State BRAM
  val state = Module(new BRAM()(cfg.stateBRAMc))
  state.io.getPort(0) <> io.state_bram

  val stateRead = state.io.getPort(1).dataOut.get

  // Transplant Unit
  val tp = Module(new TransplantUnit())
  tp.io.start := io.tp_start
  io.tp_done := tp.io.done
  val flush = tp.io.flush || reset.toBool()

  // PCs
  val curr_PC = Wire(DATA_T)
  val next_PC = Wire(DATA_T)

  // Pipeline
  // Fetch
  val fetch = Module(new FetchUnit())

  // Decode
  val decoder = Module(new DecodeUnit())
  val dec_reg = withReset(flush){Module(new Queue(new DInst, 1, pipe = true, flow = false))}

  // Issue
  val issuer = Module(new IssueUnit())
  // issue_reg in unit

  // | Execute |        |            |
  val executer = Module(new ExecuteUnit())
  val exe_reg = withReset(flush){Module(new Queue(new EInst, 1, pipe = true, flow = false))}

  // |         | Branch |            |
  val brancher = Module(new BranchUnit())
  val br_reg = withReset(flush){Module(new Queue(new BInst, 1, pipe = true, flow = false))}

  // |         |        | Load Store |
  val lsu = Module(new LoadStoreUnit())
  val lsu_reg = withReset(flush){Module(new Queue(new MInst, 1, pipe = true, flow = false))}


  // PState
  // Writeback
  val vec_rfile = VecInit(Seq.fill(NUM_THREADS)(Module(new RFile).io))
  val vec_pregs = RegInit(VecInit(Seq.fill(NUM_THREADS)(Wire(new PStateRegs()).empty)))

  // For debug and state testing
  io.inst_in := decoder.io.dinst
  io.dec_reg   <> dec_reg.io.deq
  io.issue_reg <> issuer.io.deq
  io.exe_reg   <> exe_reg.io.deq
  io.br_reg    <> br_reg.io.deq
  io.lsu_reg   <> lsu_reg.io.deq

  // fetch <> ppage
  ppage.io.getPort(1).dataIn.get := 0.U
  ppage.io.getPort(1).addr := fetch.io.addr
  ppage.io.getPort(1).en := fetch.io.rd_en
  ppage.io.getPort(1).writeEn.get := false.B
  fetch.io.data := ppage.io.getPort(1).dataOut.get

  // transplant unit <> pstate
  state.io.getPort(1) <> tp.io.bram_port

  val fetch_en = withReset(flush){RegInit(0.U(1.W))}
  when(tp.io.fetch_start){fetch_en := true.B}
  tp.io.tp_req := Mux(RegNext(fetch_en.toBool()), decoder.io.tp_req, false.B)

  // IRAM(ppage)-> Fetch
  fetch.io.en := fetch_en.toBool()
  fetch.io.PC := next_PC
  fetch.io.inst.ready := true.B // TODO: for now always ready ( change decoder to wait for branch instruction)
  fetch.io.tag_in := io.tag

  // Fetch -> Decode
  decoder.io.inst := Mux(fetch.io.inst.valid, fetch.io.inst.bits, INST_X)
  decoder.io.tag := fetch.io.tag_out

  // Register instruction (DInst)
  io.ready := dec_reg.io.enq.ready
  dec_reg.io.enq.valid := io.valid && decoder.io.dinst.itype =/= 0.U
  dec_reg.io.enq.bits  := decoder.io.dinst

  // Decode -> Issue
  issuer.io.enq <> dec_reg.io.deq
  issuer.io.flush := flush

  // Execute : Issue -> Execute
  val issued_dinst = issuer.io.deq.bits
  val issued_tag = issued_dinst.tag
  val tag_select = Mux(tp.io.tp_en, tp.io.tp_tag, issued_tag)
  issuer.io.deq.ready := exe_reg.io.enq.ready || br_reg.io.enq.ready
  // Check for front pressure
  issuer.io.exe_reg.bits := exe_reg.io.deq.bits
  issuer.io.exe_reg.valid :=  exe_reg.io.deq.valid

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
  exe_reg.io.enq.valid := executer.io.einst.valid && issuer.io.deq.valid
  exe_reg.io.enq.bits  := executer.io.einst.bits

  // connect BranchUnit interface
  brancher.io.nzcv := vec_pregs(issued_dinst.tag).NZCV
  brancher.io.dinst := issued_dinst

  // Register BranchUnit output
  br_reg.io.enq.valid := issuer.io.deq.valid && brancher.io.binst.valid
  br_reg.io.enq.bits.offset := brancher.io.binst.bits.offset
  br_reg.io.enq.bits.tag := brancher.io.binst.bits.tag

  // connect LSUnit interface
  lsu.io.dinst := issued_dinst
  lsu.io.rVal1 := rVal1
  lsu.io.rVal2 := rVal2
  lsu.io.pc    := curr_PC
  io.mem_req := lsu.io.memReq
  lsu.io.memRes := io.mem_res

  // testing only
  lsu.io.write_tlb_vaddr := DontCare
  lsu.io.write_tlb_entry := DontCare

  // Register LSUnit output MInst
  lsu_reg.io.enq.valid  := lsu.io.minst.valid && issuer.io.deq.valid // TODO: check condition
  lsu_reg.io.enq.bits := lsu.io.minst.bits

  // Writeback : Execute -> PState
  // NOTE: Always dequeue executions stage
  exe_reg.io.deq.ready := true.B
  br_reg.io.deq.ready := true.B

  // connect RFile's write interface
  val waddr = Mux(tp.io.tp_en, tp.io.tp_reg_waddr, exe_reg.io.deq.bits.rd)
  val wdata = Mux(tp.io.tp_en, tp.io.tp_reg_wdata, exe_reg.io.deq.bits.res)
  vec_rfile map { case rfile =>
    rfile.waddr := waddr
    rfile.wdata := wdata
    rfile.wen := false.B
  }
  val wen = Mux(tp.io.tp_en, tp.io.tp_reg_wen, exe_reg.io.deq.bits.rd_en && exe_reg.io.deq.valid)
  vec_rfile(tag_select).wen := wen
  io.wen := exe_reg.io.deq.bits.rd_en && exe_reg.io.deq.valid


  // PState Regs
  when (exe_reg.io.deq.valid && exe_reg.io.deq.bits.nzcv_en) {
    vec_pregs(exe_reg.io.deq.bits.tag).NZCV := exe_reg.io.deq.bits.nzcv
  }

  val last_thread = Reg(TAG_T)
  // do not update PC when an instruction(branch/execute) instruction didn't executed
  curr_PC := vec_pregs(last_thread).PC
  next_PC := vec_pregs(last_thread).PC
  // output PCs
  io.curr_PC := curr_PC
  io.next_PC := next_PC

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
  when(br_reg.io.deq.valid) {
    vec_pregs(br_reg.io.deq.bits.tag).PC := (vec_pregs(br_reg.io.deq.bits.tag).PC.zext + br_reg.io.deq.bits.offset.asSInt).asUInt()
    curr_PC := vec_pregs(br_reg.io.deq.bits.tag).PC
    next_PC := (vec_pregs(br_reg.io.deq.bits.tag).PC.zext + br_reg.io.deq.bits.offset.asSInt).asUInt()
    last_thread := br_reg.io.deq.bits.tag
  }.elsewhen(exe_reg.io.deq.valid) {
    vec_pregs(exe_reg.io.deq.bits.tag).PC := vec_pregs(exe_reg.io.deq.bits.tag).PC + 1.U
    curr_PC := vec_pregs(exe_reg.io.deq.bits.tag).PC
    next_PC := vec_pregs(exe_reg.io.deq.bits.tag).PC + 1.U
    last_thread := exe_reg.io.deq.bits.tag
  }
}

