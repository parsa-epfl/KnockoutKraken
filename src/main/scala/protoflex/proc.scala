// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.core.withReset
import chisel3.util.{DeqIO, Queue, Valid}
import common.PROCESSOR_TYPES._

/** Processor
  *
  */
class Proc extends Module
{
  val io = IO(new Bundle {
    // Fetche simulator
    val tag  = Input(TAG_T)
    val inst = Input(INST_T)
    val valid = Input(Bool())
    val ready = Output(Bool())

    // State
    val inst_in = Output(new DInst)
    val dec_reg = Flipped(DeqIO(new DInst))
    val issue_reg = Flipped(DeqIO(new DInst))
    val exe_reg = Flipped(DeqIO(new EInst))
    val br_reg = Flipped(DeqIO(new BInst))
    val lsu_reg = Flipped(DeqIO(new MInst))
    val curr_PC = Output(DATA_T)
    val next_PC = Output(DATA_T)
    val wen = Output(Bool())

    // memory interface
    val mem_req = Output(Valid(new MemRes))
    val mem_res = Input(Valid(new MemRes))

    // initialization qemu->armflex
    val tp_mode = Input(Bool())
    val tp_reg_waddr = Input(REG_T)
    val tp_reg_wdata = Input(DATA_T)
    val tp_reg_wen = Input(DATA_T)
    val tp_tag = Input(TAG_T)
    val tp_rs1 = Output(DATA_T)
    val tp_rs2 = Output(DATA_T)
    // pstate
    val tp_pstate_wen = Input(Bool())
    val tp_PC = Input(DATA_T)
    val tp_SP = Input(DATA_T)
    val tp_EL = Input(DATA_T)
    val tp_NZCV = Input(NZCV_T)
    val tp_PC_o = Output(DATA_T)
    val tp_SP_o = Output(DATA_T)
    val tp_EL_o = Output(DATA_T)
    val tp_NZCV_o = Output(NZCV_T)

    // transplant.scala armflex->qemu
    val tp_reg_rs1_addr = Input(REG_T)
    val tp_reg_rs2_addr = Input(REG_T)

    val flush = Input(Bool())
  })
  // PCs
  val curr_PC = Wire(REG_T)
  val next_PC = Wire(REG_T)

  // Pipeline
  val flush = io.flush || reset.toBool()

  // Decode
  val decoder = Module(new DecodeUnit())
  val dec_reg = withReset(flush){Module(new Queue(new DInst, 1, pipe = true, flow = false))}

  // Issue
  val issuer = Module(new IssueUnit())
  // issue_reg in unit

  // Execute
  val executer = Module(new ExecuteUnit())
  val exe_reg = withReset(flush){Module(new Queue(new EInst, 1, pipe = true, flow = false))}

  // Branch
  val brancher = Module(new BranchUnit())
  val br_reg = withReset(flush){Module(new Queue(new BInst, 1, pipe = true, flow = false))}

  // Load Store
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

  // Fetch -> Decode
  decoder.io.inst := io.inst
  decoder.io.tag := io.tag

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
  val tag_select = Mux(io.tp_mode, io.tp_tag, issued_tag)
  issuer.io.deq.ready := exe_reg.io.enq.ready || br_reg.io.enq.ready
  // Check for front pressure
  issuer.io.exe_reg.bits := exe_reg.io.deq.bits
  issuer.io.exe_reg.valid :=  exe_reg.io.deq.valid

  /** Execute */
  val rs1_addr = Mux(io.tp_mode, io.tp_reg_rs1_addr, issued_dinst.rs1)
  val rs2_addr = Mux(io.tp_mode, io.tp_reg_rs2_addr, issued_dinst.rs2)
  // connect rfile read(address) interface
  vec_rfile map { case rfile =>
    rfile.rs1_addr := rs1_addr
    rfile.rs2_addr := rs2_addr
  }
  // Read register data from rfile
  val rVal1 = vec_rfile(tag_select).rs1_data
  val rVal2 = vec_rfile(tag_select).rs2_data

  // reading register: transplant.scala
  io.tp_rs1 := rVal1
  io.tp_rs2 := rVal2

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
  val waddr = Mux(io.tp_mode, io.tp_reg_waddr, exe_reg.io.deq.bits.rd)
  val wdata = Mux(io.tp_mode, io.tp_reg_wdata, exe_reg.io.deq.bits.res)
  vec_rfile map { case rfile =>
    rfile.waddr := waddr
    rfile.wdata := wdata
    rfile.wen := false.B
  }
  val wen = Mux(io.tp_mode, io.tp_reg_wen, exe_reg.io.deq.bits.rd_en && exe_reg.io.deq.valid)
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
  io.tp_PC_o := vec_pregs(io.tp_tag).PC
  io.tp_SP_o := vec_pregs(io.tp_tag).SP
  io.tp_EL_o := vec_pregs(io.tp_tag).EL
  io.tp_NZCV_o := vec_pregs(io.tp_tag).NZCV
  when(io.tp_mode && io.tp_pstate_wen){
    vec_pregs(io.tp_tag).PC := io.tp_PC
    vec_pregs(io.tp_tag).SP := io.tp_SP
    vec_pregs(io.tp_tag).EL := io.tp_EL
    vec_pregs(io.tp_tag).NZCV := io.tp_NZCV
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

