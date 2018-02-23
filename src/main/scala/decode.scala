package protoflex

import chisel3._
import chisel3.util.BitPat
import chisel3.util.Enum

object ALU
{
  val SZ_FN = 3
  def ALU_X = BitPat.dontCare(SZ_FN)
  def AND  = 0.U(SZ_FN)
  def BIC  = 1.U(SZ_FN)
  def ORR  = 2.U(SZ_FN)
  def ORN  = 3.U(SZ_FN)
  def EOR  = 4.U(SZ_FN)
  def EON  = 5.U(SZ_FN)
  def ADD  = 6.U(SZ_FN)
  def SUB  = 7.U(SZ_FN)
}

object SHIFT
{
  val SZ_FN = 2
  def SHIFT_X = BitPat.dontCare(SZ_FN)
  def LSL  = 0.U(SZ_FN)
  def LSR  = 1.U(SZ_FN)
  def ASR  = 2.U(SZ_FN)
  def ROR  = 3.U(SZ_FN)
}

class DInstIo extends Bundle
{
  val rd  = UInt(5.W)
  val rs1 = UInt(5.W)
  val rs2 = UInt(5.W)

  val imm = UInt(26.W)

  val aluOp = UInt(ALU.SZ_FN)

  val shift = UInt(SHIFT.SZ_FN)
  val shift_v = Bool()

  val valid = Bool(false)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])] ) = 


class DecodeUnitIo extends Bundle
{
  val inst = Input(UInt(32.W))
  val dinst = Output(new DInstIo) // Reciver -> Flipped(new DInstIoOut)
}

class DecodeUnit extends Module
{
  val io = new DecodeUnitIo
}

abstract trait DecodeConstants
{
// scalastyle:off
  def decode_default: List[BitPat] =
    //
    //
    //   RD
    //   |                   RS1
    //   |                   |                   RS2
    //   |                   |                   |                   IMM                                 SHIFT        INSTR
    //   |                   |                   |                   |                   ALUOP           VALID        VALID
    //   |                   |                   |                   |                   |      SHIFT    |            |
    //   |                   |                   |                   |                   |      OP       |            |
    //   |                   |                   |                   |                   |      |        |            |
    List(BitPat.dontCare(5), BitPat.dontCare(5), BitPat.dontCare(5), BitPat.dontCare(5), ALU_X, SHIFT_X, BitPat(0.U), BitPat(0.U))
  val table: Array[(BitPat, List[BitPat])]
// scalastyle:on
}

object Decode extends DecodeConstants


object Instructions {
/* Logical (shifted register) */
/* 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant */
/* sf |  opc  |  0  1  0  1  0 | shift |  N |      Rm        |       imm6        |       Rn       |       Rd      |                  |         */
  def LogiSR              = BitPat("b???01010????????????????????????")
/*  1 |  0  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | AND              | 64-bit  */
/*  1 |  0  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | BIC              | 64-bit  */
/*  1 |  0  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ORR              | 64-bit  */
/*  1 |  0  1 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | ORN              | 64-bit  */
/*  1 |  1  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | EOR              | 64-bit  */
/*  1 |  1  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | EON              | 64-bit  */
/*  1 |  1  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ANDS             | 64-bit  */
/*  1 |  1  1 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | BICS             | 64-bit  */
  def AND                = BitPat("b100???????0?????????????????????")
  def BIC                = BitPat("b100???????1?????????????????????")
  def ORR                = BitPat("b101???????0?????????????????????")
  def ORN                = BitPat("b101???????1?????????????????????")
  def EOR                = BitPat("b110???????0?????????????????????")
  def EON                = BitPat("b110???????1?????????????????????")
  def ANDS               = BitPat("b111???????0?????????????????????")
  def BICS               = BitPat("b111???????1?????????????????????")
}
