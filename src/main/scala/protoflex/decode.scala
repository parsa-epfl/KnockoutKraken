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
  val rs1 = Valid(REG_T)
  val rs2 = Valid(REG_T)
  val imm = Valid(IMM_T)
  val shift_val = Valid(SHIFT_VAL_T)
  val shift_type = Output(SHIFT_TYPE_T)
  val cond  = Valid(COND_T)

  // Control
  val itype = Output(I_T)
  val op = Output(OP_T)

  // Enables
  val nzcv_en  = Output(Bool())

  val tag = Output(cfg.TAG_T)
  val inst32 = Valid(INST_T)

  val pc = Output(DATA_T)

  def decode(inst : UInt, tag_ : UInt): DInst = {
    val decoder = ListLookup(inst, decode_default, decode_table)

    // Data
    val itype = decoder.head
    rd.bits := MuxLookup(itype,  REG_X, Array(
                           I_LogSR -> inst( 4, 0),
                           I_LogI  -> inst( 4, 0),
                           I_ASImm -> inst( 4, 0),
                           I_ASSR  -> inst( 4, 0),
                           I_LSImm -> inst( 4, 0),
                           I_ASSR  -> inst( 4, 0)))

    rs1.bits := MuxLookup(itype, REG_X, Array(
                            I_LogSR -> inst( 9, 5),
                            I_LogI  -> inst( 9, 5),
                            I_ASSR  -> inst( 9, 5),
                            I_ASImm -> inst( 9, 5)
                          ))

    rs2.bits := MuxLookup(itype, REG_X, Array(
                            I_LogSR -> inst(20,16),
                            I_ASSR  -> inst(20,16)))

    imm.bits := MuxLookup(itype, IMM_X, Array(
                            I_LogI  -> inst(21,10), // NOTE: immr(21:16), imms(15:10)
                            I_BImm  -> inst(25, 0),
                            I_BCImm -> inst(23, 5),
                            I_ASImm -> inst(21,10),
                            I_LSImm -> inst(23, 5)))

    shift_val.bits := MuxLookup(itype, SHIFT_VAL_X, Array(
                                  I_LogSR -> inst(15,10),
                                  I_ASSR  -> inst(15,10),
                                  I_ASImm -> Mux(inst(22), 12.U, 0.U)))

    shift_type := MuxLookup(itype, SHIFT_TYPE_X, Array(
                              I_LogSR -> inst(23,22),
                              I_ASSR  -> inst(23,22),
                              I_ASImm -> LSL))

    cond.bits := MuxLookup(itype,  COND_X, Array(
                             I_BCImm -> inst( 3, 0) ))

    // Control
    val cdecoder = decoder.tail
    val csignals = Seq(op, rd.valid, rs1.valid, rs2.valid, imm.valid, shift_val.valid, cond.valid, nzcv_en)
    csignals zip cdecoder map { case (s, d) => s := d }


    tag := tag_
    inst32.valid := (itype =/= I_X)
    inst32.bits := inst

    this.itype := itype

    this
  }

}

object DInst {
  def apply()(implicit cfg: ProcConfig): DInst = {
    val dinst = Wire(new DInst)

    // Data
    dinst.rd.bits  := REG_X
    dinst.rs1.bits := REG_X
    dinst.rs2.bits := REG_X
    dinst.imm.bits := IMM_X
    dinst.shift_val.bits := SHIFT_VAL_X
    dinst.shift_type := SHIFT_TYPE_X
    dinst.cond.bits := COND_X

    // Control
    dinst.op := OP_X
    dinst.itype := I_X

    // Enables
    dinst.rd.valid := N
    dinst.rs1.valid := N
    dinst.rs2.valid := N
    dinst.imm.valid := N
    dinst.shift_val.valid := N
    dinst.cond.valid := N
    dinst.nzcv_en := N

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
  val instBE = WireInit(Cat(inst(7,0), inst(15,8), inst(23,16), inst(31,24))) // Little Endian processor
  val dinst = WireInit(DInst())
  dinst.decode(instBE, io.finst.tag)
  dinst.pc := io.finst.pc
  io.dinst := dinst
}
