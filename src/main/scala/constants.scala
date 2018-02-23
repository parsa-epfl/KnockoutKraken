package protoflex

import chisel3._
import chisel3.util.BitPat

object INSTR_TYPES
{
  private val SZ_REG = 5
  private val SZ_MAX_IMM = 26
  def REG_X = BitPat.dontCare(SZ_REG)
  def IMM_X = BitPat.dontCare(SZ_MAX_IMM)
}

object OP_ALU
{
  private val SZ = 3
  def OP_ALU_X = BitPat.dontCare(SZ)
  def OP_AND  = 0.U(SZ)
  def OP_BIC  = 1.U(SZ)
  def OP_ORR  = 2.U(SZ)
  def OP_ORN  = 3.U(SZ)
  def OP_EOR  = 4.U(SZ)
  def OP_EON  = 5.U(SZ)
  def OP_ADD  = 6.U(SZ)
  def OP_SUB  = 7.U(SZ)
}

object SHIFT_OP
{
  private val SZ = 2
  def SHIFT_X = BitPat.dontCare(SZ)
  def LSL  = 0.U(SZ)
  def LSR  = 1.U(SZ)
  def ASR  = 2.U(SZ)
  def ROR  = 3.U(SZ)
}

object CONSTANTS
{
  def N = false.B
  def Y = true.B
}

object INSTRUCTIONS {
/* Logical (shifted register) */
/* 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant */
/* sf |  opc  |  0  1  0  1  0 | shift |  N |      Rm        |       imm6        |       Rn       |       Rd      |                  |         */
/*  1 |  0  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | AND              | 64-bit  */
/*  1 |  0  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | BIC              | 64-bit  */
/*  1 |  0  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ORR              | 64-bit  */
/*  1 |  0  1 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | ORN              | 64-bit  */
/*  1 |  1  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | EOR              | 64-bit  */
/*  1 |  1  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | EON              | 64-bit  */
/*  1 |  1  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ANDS             | 64-bit  */
/*  1 |  1  1 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | BICS             | 64-bit  */
  def AND     = BitPat("b10001010??0?????????????????????")
  def BIC     = BitPat("b10001010??1?????????????????????")
  def ORR     = BitPat("b10101010??0?????????????????????")
  def ORN     = BitPat("b10101010??1?????????????????????")
  def EOR     = BitPat("b11001010??0?????????????????????")
  def EON     = BitPat("b11001010??1?????????????????????")
  def ANDS    = BitPat("b11101010??0?????????????????????")
  def BICS    = BitPat("b11101010??1?????????????????????")
  def INST_X  = BitPat.dontCare(32)
}
