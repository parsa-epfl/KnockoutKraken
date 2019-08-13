package protoflex

import chisel3._
import chisel3.util.{BitPat, ListLookup, MuxLookup, EnqIO, DeqIO, Cat}

import common.PROCESSOR_TYPES._
import common.DECODE_CONTROL_SIGNALS._
import common.DECODE_MATCHING_TABLES._

class DInst(implicit val cfg: ProcConfig) extends Bundle
{
  // Data
  val rd    = REG_T
  val rs1   = REG_T
  val rs2   = REG_T
  val imm   = IMM_T
  val shift_val = SHIFT_VAL_T
  val shift_type = SHIFT_TYPE_T
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
  val nzcv_en  = Bool()

  // Instruction is Valid
  val inst_en = Bool()

  val tag = cfg.TAG_T

  def decode(inst : UInt, tag : UInt): DInst = {
    val decoder = ListLookup(inst, decode_default, decode_table)

    // Data
    val itype = decoder.head
    rd    := MuxLookup(itype,   REG_X, Array( I_LogSR -> inst( 4, 0),
                                              I_ASImm -> inst( 4, 0),
                                              I_LSImm -> inst(4,0)))
    rs1   := MuxLookup(itype,   REG_X, Array( I_LogSR -> inst( 9, 5),
                                              I_ASImm -> inst( 9, 5)))
    rs2   := MuxLookup(itype,   REG_X, Array( I_LogSR -> inst(20,16) ))
    imm   := MuxLookup(itype,   IMM_X, Array( I_LogSR -> inst(15,10),
                                              I_BImm  -> inst(25, 0),
                                              I_BCImm -> inst(23, 5),
                                              I_ASImm -> inst(21,10),
                                              I_LSImm -> inst(23,5)))
    shift_val := MuxLookup(itype, SHIFT_VAL_X, Array(I_ASImm -> 12.U,
                                                      I_LogSR -> imm))
    shift_type := MuxLookup(itype, SHIFT_TYPE_X, Array( I_LogSR -> inst(23,22),
                                              I_ASImm -> inst(23,22)))
    cond  := MuxLookup(itype,  COND_X, Array( I_BCImm -> inst( 3, 0) ))

    // Control
    val cdecoder = decoder.tail
    val csignals = Seq(op, rd_en, rs1_en, rs2_en, imm_en, shift_en, cond_en, nzcv_en, inst_en)
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
    shift_val := SHIFT_VAL_X
    shift_type := SHIFT_TYPE_X
    cond  := COND_X

    // Control
    op := OP_X
    itype := I_X

    // Enables
    rd_en    := N
    rs1_en   := N
    rs2_en   := N
    imm_en   := N
    shift_en := N
    cond_en  := N
    nzcv_en  := N

    // Instruction is Valid
    inst_en := N

    tag := cfg.TAG_X

    this
  }
}

class DecodeUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  // Fetch - Decode
  val finst = Input(new FInst)
  val tp_req = Output(Bool())

  // Decode - Issue
  val dinst = Output(new DInst)
}

/** Decode unit
  * Fowards the decoded instruction to the issue stage
  */
class DecodeUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new DecodeUnitIO)
  val inst = io.finst.inst
  //val instLE = WireInit(Cat(inst(7,0), inst(15,8), inst(23,16), inst(31,24)))
  val dinst = Wire(new DInst).decode(inst, io.finst.tag)
  io.dinst := dinst
  io.tp_req := dinst.inst_en
}
