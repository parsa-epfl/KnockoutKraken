package protoflex

import chisel3._
import chisel3.util.{BitPat, ListLookup, MuxLookup, EnqIO, DeqIO, Cat}

import common.PROCESSOR_TYPES._
import common.DECODE_CONTROL_SIGNALS._
import common.DECODE_MATCHING_TABLES._
import common.INSTRUCTIONS._

class DInst extends Bundle
{
  // Data
  val rd    = REG_T
  val rs1   = REG_T
  val rs2   = REG_T
  val imm   = IMM_T
  val shift = SHIFT_T

  // Control
  val aluOp = OP_ALU_T

  // Enables
  val rd_en    = Bool()
  val rs1_en   = Bool()
  val rs2_en   = Bool()
  val imm_en   = Bool()
  val shift_en = Bool()

  // Instruction is Valid
  val inst_en = Bool()

  val tag = TAG_T

  def decode(inst : UInt, tag : UInt) = {
    val decoder = ListLookup(inst, decode_default, decode_table)

    // Data
    val dtype = decoder.head
    rd    := MuxLookup(dtype,   REG_X, Array( I_LogSR -> inst( 4, 0) ))
    rs1   := MuxLookup(dtype,   REG_X, Array( I_LogSR -> inst( 9, 5) ))
    rs2   := MuxLookup(dtype,   REG_X, Array( I_LogSR -> inst(20,16) ))
    imm   := MuxLookup(dtype,   IMM_X, Array( I_LogSR -> inst(15,10) ))
    shift := MuxLookup(dtype, SHIFT_X, Array( I_LogSR -> inst(23,22) ))

    // Control
    val cdecoder = decoder.tail
    val csignals = Seq(aluOp, rd_en, rs1_en, rs2_en, imm_en, shift_en, inst_en)
    csignals zip cdecoder map { case (s, d) => s:= d }

    this.tag := tag

    this
  }

  def empty() = {

    // Data
    rd    := REG_X
    rs1   := REG_X
    rs2   := REG_X
    imm   := IMM_X
    shift := SHIFT_X

    // Control
    aluOp := OP_ALU_X

    // Enables
    rd_en    := N
    rs1_en   := N
    rs2_en   := N
    imm_en   := N
    shift_en := N

    // Instruction is Valid
    inst_en := N

    tag := TAG_X

    this
  }
}

class DecodeUnitIO extends Bundle
{
  // Fetch - Decode
  val in_inst = DeqIO(INST_T)
  val in_tag  = Input(TAG_T)

  // Decode - Issue
  val out_dinst = EnqIO(new DInst)
}

/** Decode unit
  * Fowards the decoded instruction to the issue stage
  */
class DecodeUnit extends Module
{
  val io = IO(new DecodeUnitIO)
  val inst = Cat(1.U,io.in_inst.bits(30,0))// Hack to force 64 bits operations

//    Cat(
//    io.in_inst.bits(31, 24), io.in_inst.bits(23, 16), io.in_inst.bits(15, 8), io.in_inst.bits( 7,  0),
//    1.U, io.in_inst.bits( 6, 0), io.in_inst.bits(15, 8), io.in_inst.bits(23,16), io.in_inst.bits(31,24)
//  )

  val dinst = Wire(new DInst).decode(inst, io.in_tag)

  io.out_dinst.bits     := dinst
  io.out_dinst.bits.tag := io.in_tag

  io.out_dinst.valid := io.in_inst.valid
  io.in_inst.ready   := io.out_dinst.ready
}
