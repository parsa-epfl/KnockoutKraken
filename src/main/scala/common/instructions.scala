package common

import chisel3._
import chisel3.util.BitPat

/*
 * Bit Patterns for Instructions
 *
 */
object INSTRUCTIONS
{
  val TYPE_W = DEC_LITS.TYPE_W.W

  def INST_X  = BitPat.dontCare(32)
  def I_T = UInt(TYPE_W)
  val I_X = DEC_LITS.I_X.U(TYPE_W)

  // Branches immediate
  // Unconditional branch (immediate)
  val I_BImm = DEC_LITS.I_BImm.U(TYPE_W)
  // 31 | 30 29 28 27 26 | 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00 | Instruction Page | Variant
  // op |  0  0  1  0  1 |                                   imm26                                       |                  |    -   
  //  0 |  0  0  1  0  1 |                                   imm26                                       | B                |    -   
  //  1 |  0  0  1  0  1 |                                   imm26                                       | BL               |    -   
  def B  = BitPat("b000101??????????????????????????")
  def BL = BitPat("b100101??????????????????????????")

  // Compare & branch (immediate)
  val I_BCImm = DEC_LITS.I_BCImm.U(TYPE_W)
  // 31 30 29 28 27 26 25 | 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 | 03 02 01 00 | Instruction Page | Variant
  //  0  1  0  1  0  1  0 | o1 |                             imm19                        | o0 |    cond     |                  |
  //  0  1  0  1  0  1  0 |  0 |                             imm19                        |  0 |    cond     | B.cond           |    -
  def BCond = BitPat("b01010100???????????????????0????")

  // Logical (shifted register)
  val I_LogSR = DEC_LITS.I_LogSR.U(TYPE_W) // Type
  // 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant
  // sf |  opc  |  0  1  0  1  0 | shift |  N |      Rm        |       imm6        |       Rn       |       Rd      |                  |
  //  1 |  0  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | AND              | 64-bit
  //  1 |  0  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | BIC              | 64-bit
  //  1 |  0  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ORR              | 64-bit
  //  1 |  0  1 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | ORN              | 64-bit
  //  1 |  1  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | EOR              | 64-bit
  //  1 |  1  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | EON              | 64-bit
  //  1 |  1  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ANDS             | 64-bit
  //  1 |  1  1 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | BICS             | 64-bit
  def AND = BitPat("b10001010??0?????????????????????")
  def BIC = BitPat("b10001010??1?????????????????????")
  def ORR = BitPat("b10101010??0?????????????????????")
  def ORN = BitPat("b10101010??1?????????????????????")
  def EOR = BitPat("b11001010??0?????????????????????")
  def EON = BitPat("b11001010??1?????????????????????")
  def ANDS= BitPat("b11101010??0?????????????????????")
  def BICS= BitPat("b11101010??1?????????????????????")

}
