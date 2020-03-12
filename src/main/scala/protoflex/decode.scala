package protoflex

import chisel3._
import chisel3.util.{BitPat, ListLookup, MuxLookup, EnqIO, DeqIO, Cat, Valid}

import common.PROCESSOR_TYPES._
import common.DECODE_CONTROL_SIGNALS._
import common.DECODE_MATCHING_TABLES._

class DInst(implicit val cfg: ProcConfig) extends Bundle
{
  // Data
  val rd = Valid(REG_T)
  val rs1 = Output(REG_T)
  val rs2 = Output(REG_T)
  val imm = Output(IMM_T)
  val shift_val = Valid(SHIFT_VAL_T)
  val shift_type = Output(SHIFT_TYPE_T)
  val cond  = Valid(COND_T)

  // Control
  val is32bit = Output(Bool())
  val itype = Output(I_T)
  val op = Output(OP_T)

  // Enables
  val nzcv  = Valid(NZCV_T)

  val tag = Output(cfg.TAG_T)
  val inst32 = Valid(INST_T)

  val pc = Output(DATA_T)

  def decode(inst : UInt, tag_ : UInt): DInst = {
    val decoder = ListLookup(inst, decode_default, decode_table)

    // Data
    val itype = decoder.head
    rd.bits := MuxLookup(itype,  inst(4, 0), Array(
      I_PCRel -> inst( 4, 0),
      I_BitF  -> inst( 4, 0),
      I_DP1S  -> inst( 4, 0),
      I_DP2S  -> inst( 4, 0),
      I_LogSR -> inst( 4, 0),
      I_LogI  -> inst( 4, 0),
      I_MovI  -> inst( 4, 0),
      I_ASImm -> inst( 4, 0),
      I_ASSR  -> inst( 4, 0),
      I_ASSR  -> inst( 4, 0),
      I_CSel  -> inst( 4, 0),
      I_LSPoReg -> inst( 4, 0),
      I_LSPrReg -> inst( 4, 0),
      I_LSPReg-> inst( 4, 0),
      I_LSRReg-> inst( 4, 0),
      I_LSUImm-> inst( 4, 0)
    ))

    rs1 := MuxLookup(itype, inst( 9, 5), Array(
      I_BReg  -> inst( 9, 5),
      I_BitF  -> inst( 9, 5),
      I_DP1S  -> inst( 9, 5),
      I_DP2S  -> inst( 9, 5),
      I_LogSR -> inst( 9, 5),
      I_LogI  -> inst( 9, 5),
      I_ASSR  -> inst( 9, 5),
      I_ASImm -> inst( 9, 5),
      I_CCImm -> inst( 9, 5),
      I_CCReg -> inst( 9, 5),
      I_CSel  -> inst( 9, 5),
      I_LSPoReg -> inst( 9, 5),
      I_LSPrReg -> inst( 9, 5),
      I_LSUReg-> inst( 9, 5),
      I_LSPReg-> inst( 9, 5),
      I_LSRReg-> inst( 9, 5),
      I_LSUImm-> inst( 9, 5)
    ))

    // rs2 = inst(4,0) => Rs2 = Rt
    rs2 := MuxLookup(itype, inst(20,16), Array(
      I_LogSR -> inst(20,16),
      I_ASSR  -> inst(20,16),
      I_CSel  -> inst(20,16),
      I_CCReg -> inst(20,16),
      I_TBImm -> inst( 4, 0),
      I_BitF  -> inst( 4, 0),
      I_DP2S  -> inst(20,16),
      I_MovI  -> inst( 4, 0),
      I_CBImm -> inst( 4, 0),
      I_LSPReg-> inst(14,10), // Rt2 as Rd2
      I_LSRReg-> inst(20,16),
      I_LSUImm-> inst( 4, 0)
    ))

    imm := MuxLookup(itype, inst(23,5), Array(
      I_BitF  -> inst(22,10), // N(22), immr(21:16), imms(15:10)
      I_LogI  -> inst(22,10), // N(22), immr(21:16), imms(15:10)
      I_MovI  -> inst(22, 5), // hw(22,21), imm16(20:5)
      I_TBImm -> inst(23, 5), // b40(23,19), imm14(18,5)
      I_CCImm -> inst(21,16), // zero(21), imm5(20,16)

      I_BImm  -> inst(25, 0),
      I_PCRel -> Cat(inst(23,5), inst(30,29)), // immlo(30,29), immhi(23,5)
      I_BCImm -> inst(23,5),
      I_CBImm -> inst(23,5),

      I_ASImm -> inst(21,10),

      I_LSPoReg -> inst(20,12),
      I_LSPrReg -> inst(20,12),
      I_LSUReg-> inst(21,12), // zero(21), imm9(20,12)
      I_LSPReg-> inst(21,15), // imm7(21,15)
      I_LSUImm-> inst(21,10)
    ))

    shift_val.bits := MuxLookup(itype, inst(15,10), Array(
      I_LogSR -> inst(15,10),
      I_ASSR  -> inst(15,10),
      I_ASImm -> Mux(inst(22), 12.U, 0.U),
      I_BitF  -> inst(21,16) ))

    shift_type := MuxLookup(itype, inst(23,22), Array(
      I_DP2S  -> inst(11,10),
      I_LogSR -> inst(23,22),
      I_ASSR  -> inst(23,22),
      I_ASImm -> LSL,
      I_BitF  -> ROR))

    cond.bits := MuxLookup(itype,  inst(15,12), Array(
      I_CSel  -> inst(15,12),
      I_CCImm -> inst(15,12),
      I_CCReg -> inst(15,12),
      I_BCImm -> inst( 3, 0)
    ))
    nzcv.bits := MuxLookup(itype, inst( 3, 0), Array (
      I_CCImm -> inst( 3, 0),
      I_CCReg -> inst( 3, 0)
    ))

    // Control
    val cdecoder = decoder.tail
    val csignals = Seq(op, rd.valid, shift_val.valid, cond.valid, nzcv.valid, is32bit)
    csignals zip cdecoder map { case (s, d) => s := d }

    tag := tag_
    inst32.valid := (itype =/= I_X)
    inst32.bits := inst

    this.itype := itype

    // Special cases
    when(itype === I_LSRReg) {
      shift_val.valid := inst(12)   // S
      shift_val.bits := inst(15,13) // option
    }

    this
  }

}

object DInst {
  def genTypeUInt()(implicit cfg: ProcConfig): UInt = WireInit(new DInst, DontCare).asUInt.cloneType
  def apply()(implicit cfg: ProcConfig): DInst = {
    val dinst = Wire(new DInst)

    // Data
    dinst.rd.bits  := REG_X
    dinst.rs1      := REG_X
    dinst.rs2      := REG_X
    dinst.imm      := IMM_X
    dinst.shift_val.bits := SHIFT_VAL_X
    dinst.shift_type := SHIFT_TYPE_X
    dinst.cond.bits := COND_X
    dinst.nzcv.bits := NZCV_X

    // Control
    dinst.op := OP_X
    dinst.itype := I_X
    dinst.is32bit := N

    // Enables
    dinst.rd.valid := N
    dinst.shift_val.valid := N
    dinst.cond.valid := N
    dinst.nzcv.valid := N

    // Instruction
    dinst.inst32.bits := INST_X
    dinst.inst32.valid := N
    dinst.tag := cfg.TAG_X
    dinst.pc := DATA_X
    dinst
  }
}

class DecodeUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  // Fetch - Decode
  val finst = Input(new FInst)
  // Decode - Issue
  val dinst = Output(new DInst)
}

/** Decode unit
  */
class DecodeUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new DecodeUnitIO)
  val inst = io.finst.inst
  val dinst = WireInit(DInst())
  dinst.decode(inst, io.finst.tag)
  dinst.pc := io.finst.pc
  io.dinst := dinst
}
