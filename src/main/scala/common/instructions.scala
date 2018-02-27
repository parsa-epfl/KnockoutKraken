package common

import chisel3._
import chisel3.util.BitPat

/*
 * Bit Patterns for Instructions
 *
 */
object INSTRUCTIONS
{
  // Logical (shifted register)
  // 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant
  // sf |  opc  |  0  1  0  1  0 | shift |  N |      Rm        |       imm6        |       Rn       |       Rd      |                  |
  //  1 |  0  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | AND              | 64-bit
  //  1 |  0  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | BIC              | 64-bit
  //  1 |  0  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ORR              | 64-bit
  //  1 |  0  1 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | ORN              | 64-bit
  //  1 |  1  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | EOR              | 64-bit
  //  1 |  1  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | EON              | 64-bit
  def AND     = BitPat("b10001010??0?????????????????????")
  def BIC     = BitPat("b10001010??1?????????????????????")
  def ORR     = BitPat("b10101010??0?????????????????????")
  def ORN     = BitPat("b10101010??1?????????????????????")
  def EOR     = BitPat("b11001010??0?????????????????????")
  def EON     = BitPat("b11001010??1?????????????????????")
  def INST_X  = BitPat.dontCare(32)

  // Types
  private def W_TYPE = 2
  val I_X     = 0.U(W_TYPE)
  // Logical (shifted register)
  // 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant
  // sf |  opc  |  0  1  0  1  0 | shift |  N |      Rm        |       imm6        |       Rn       |       Rd      |                  |
  val I_LogSR = 1.U(W_TYPE)
}

