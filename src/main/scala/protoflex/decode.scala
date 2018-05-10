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
  val cond  = COND_T

  // Control
  val itype = I_T
  val op = OP_T

  // Enables
  val rd_en    = Bool()
  val rs1_en   = Bool()
  val rs2_en   = Bool()
  val imm_en   = Bool()
  val shift_en = Bool()
  val cond_en  = Bool()

  // Instruction is Valid
  val inst_en = Bool()

  val tag = TAG_T

  def decode(inst : UInt, tag : UInt) = {
    val decoder = ListLookup(inst, decode_default, decode_table)

    // Data
    val itype = decoder.head
    rd    := MuxLookup(itype,   REG_X, Array( I_LogSR -> inst( 4, 0) ))
    rs1   := MuxLookup(itype,   REG_X, Array( I_LogSR -> inst( 9, 5) ))
    rs2   := MuxLookup(itype,   REG_X, Array( I_LogSR -> inst(20,16) ))
    imm   := MuxLookup(itype,   IMM_X, Array( I_LogSR -> inst(15,10),
                                              I_BImm  -> inst(25, 0),
                                              I_BCImm -> inst(23, 5)))
    shift := MuxLookup(itype, SHIFT_X, Array( I_LogSR -> inst(23,22) ))
    cond  := MuxLookup(itype,  COND_X, Array( I_BCImm -> inst( 3, 0) ))

    // Control
    val cdecoder = decoder.tail
    val csignals = Seq(op, rd_en, rs1_en, rs2_en, imm_en, shift_en, cond_en, inst_en)
    csignals zip cdecoder map { case (s, d) => s:= d }

    this.tag := tag
    this.itype := itype

    this
  }

  def empty() = {

    // Data
    rd    := REG_X
    rs1   := REG_X
    rs2   := REG_X
    imm   := IMM_X
    shift := SHIFT_X
    cond  := COND_X

    // Control
    op := OP_T
    itype := I_X

    // Enables
    rd_en    := N
    rs1_en   := N
    rs2_en   := N
    imm_en   := N
    shift_en := N
    cond_en  := N

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

  val dinst = Wire(new DInst).decode(io.in_inst.bits, io.in_tag)

  io.out_dinst.bits     := dinst
  io.out_dinst.bits.tag := io.in_tag

  io.out_dinst.valid := io.in_inst.valid
  io.in_inst.ready   := io.out_dinst.ready
}
