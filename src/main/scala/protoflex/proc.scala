// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.{Queue, Valid, DeqIO}

import common.PROCESSOR_TYPES._

/** Processor
  *
  */
class Proc extends Module
{
  val io = IO(new Bundle {
                // Fetche simulator
                val inst = Input(INST_T)
                val tag  = Input(TAG_T)
                val valid = Input(Bool())
                val ready = Output(Bool())

                // State
                val inst_in = Output(new DInst)
                val dec_reg = Flipped(DeqIO(new DInst))
                val issue_reg = Flipped(DeqIO(new DInst))
                val exe_reg = Flipped(DeqIO(new EInst))
                val br_reg = Flipped(DeqIO(new BInst))
                val curr_PC = Output(DATA_T)
                val next_PC = Output(DATA_T)
                val wen = Output(Bool())
              })

  // Pipeline
  // Decode
  val decoder = Module(new DecodeUnit())
  val dec_reg = Module(new Queue(new DInst, 1, pipe = true, flow = false))

  // Issue
  val issuer = Module(new IssueUnit())
  // issue_reg in unit

  // Execute
  val executer = Module(new ExecuteUnit())
  val exe_reg = Module(new Queue(new EInst, 1, pipe = true, flow = false))

  val brancher = Module(new BranchUnit())
  val br_reg = Module(new Queue(new BInst, 1, pipe = true, flow = false))

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

  // Fetch -> Decode
  decoder.io.inst := io.inst
  decoder.io.tag := io.tag

  // Save into register
  io.ready := dec_reg.io.enq.ready
  dec_reg.io.enq.valid := io.valid && decoder.io.dinst.itype =/= 0.U
  dec_reg.io.enq.bits  := decoder.io.dinst

  // Decode -> Issue
  issuer.io.enq <> dec_reg.io.deq

  // Execute : Issue -> Execute
  val issued_dinst = issuer.io.deq.bits
  val issued_tag = issued_dinst.tag
  issuer.io.deq.ready := exe_reg.io.enq.ready || br_reg.io.enq.ready
  // Check for front pressure
  issuer.io.exe_reg.bits := exe_reg.io.deq.bits
  issuer.io.exe_reg.valid :=  exe_reg.io.deq.valid

  // Execute
  vec_rfile map { case rfile =>
    rfile.rs1_addr := issued_dinst.rs1
    rfile.rs2_addr := issued_dinst.rs2
  }
  executer.io.rVal1 := vec_rfile(issued_tag).rs1_data
  executer.io.rVal2 := vec_rfile(issued_tag).rs2_data
  executer.io.dinst := issued_dinst
  // Execute register
  exe_reg.io.enq.valid := executer.io.einst.valid && issuer.io.deq.valid
  exe_reg.io.enq.bits  := executer.io.einst.bits

  // Branch
  brancher.io.nzcv := vec_pregs(issued_dinst.tag).NZCV
  brancher.io.dinst := issued_dinst
  // Branch register
  br_reg.io.enq.valid := issuer.io.deq.valid && brancher.io.binst.valid
  br_reg.io.enq.bits.offset := brancher.io.binst.bits.offset
  br_reg.io.enq.bits.tag := brancher.io.binst.bits.tag

  // Writeback : Execute -> PState
  // Always dequeue executions stage
  exe_reg.io.deq.ready := true.B
  br_reg.io.deq.ready := true.B
  // RFile
  vec_rfile map { case rfile =>
    rfile.waddr := exe_reg.io.deq.bits.rd
    rfile.wdata := exe_reg.io.deq.bits.res
    rfile.wen := false.B
  }
  vec_rfile(issued_tag).wen := exe_reg.io.deq.bits.rd_en && exe_reg.io.deq.valid
  io.wen := exe_reg.io.deq.bits.rd_en && exe_reg.io.deq.valid


  // PState Regs
  when (exe_reg.io.deq.valid && exe_reg.io.deq.bits.nzcv_en) {
    vec_pregs(exe_reg.io.deq.bits.tag).NZCV := exe_reg.io.deq.bits.nzcv
  }

  val last_thread = Reg(TAG_T)
  io.curr_PC := vec_pregs(last_thread).PC
  io.next_PC := vec_pregs(last_thread).PC
  when(br_reg.io.deq.valid) {
    vec_pregs(br_reg.io.deq.bits.tag).PC := (vec_pregs(br_reg.io.deq.bits.tag).PC.zext + br_reg.io.deq.bits.offset.asSInt).asUInt()
    io.curr_PC := vec_pregs(br_reg.io.deq.bits.tag).PC
    io.next_PC := (vec_pregs(br_reg.io.deq.bits.tag).PC.zext + br_reg.io.deq.bits.offset.asSInt).asUInt()
    last_thread := br_reg.io.deq.bits.tag
  }.elsewhen(exe_reg.io.deq.valid) {
    vec_pregs(exe_reg.io.deq.bits.tag).PC := vec_pregs(exe_reg.io.deq.bits.tag).PC + 1.U
    io.curr_PC := vec_pregs(exe_reg.io.deq.bits.tag).PC
    io.next_PC := vec_pregs(exe_reg.io.deq.bits.tag).PC + 1.U
    last_thread := exe_reg.io.deq.bits.tag
  }
}

