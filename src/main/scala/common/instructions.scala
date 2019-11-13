package common

import chisel3._
import chisel3.util.BitPat

/*
 * Bit Patterns for Instructions
 *
 */
object INSTRUCTIONS
{
  def INST_X  = BitPat.dontCare(32)

  // Branches immediate
  // Unconditional branch (immediate)
  // I_BImm
  // 31 | 30 29 28 27 26 | 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00 | Instruction Page | Variant
  // op |  0  0  1  0  1 |                                   imm26                                       |                  |    -
  //  0 |  0  0  1  0  1 |                                   imm26                                       | B                |    -
  //  1 |  0  0  1  0  1 |                                   imm26                                       | BL               |    -
  def BImm = BitPat("b?00101??????????????????????????")
  def BImm_B  = BitPat("b000101??????????????????????????")
  def BImm_BL = BitPat("b100101??????????????????????????")

  // Conditional branch (immediate)
  // I_BCImm
  // 31 30 29 28 27 26 25 | 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 | 03 02 01 00 | Instruction Page | Variant
  //  0  1  0  1  0  1  0 | o1 |                             imm19                        | o0 |    cond     |                  |
  //  0  1  0  1  0  1  0 |  0 |                             imm19                        |  0 |    cond     | B.cond           |    -
  def BCImm = BitPat("b01010100???????????????????0????")
  def BCond = BitPat("b01010100???????????????????0????")

  // Logical (shifted register)
  // I_LogSR
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
  def LogSR = BitPat("b1??01010??0?????????????????????")
  def LogSR_AND  = BitPat("b10001010??0?????????????????????")
  def LogSR_BIC  = BitPat("b10001010??1?????????????????????")
  def LogSR_ORR  = BitPat("b10101010??0?????????????????????")
  def LogSR_ORN  = BitPat("b10101010??1?????????????????????")
  def LogSR_EOR  = BitPat("b11001010??0?????????????????????")
  def LogSR_EON  = BitPat("b11001010??1?????????????????????")
  def LogSR_ANDS = BitPat("b11101010??0?????????????????????")
  def LogSR_BICS = BitPat("b11101010??1?????????????????????")

  // Logical (immediate)
  // 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf |  opc  |  1  0  0  1  0  0 |  N |       immr        |      imms         |      Rn        |        Rd      |                  |
  //  1 |  0  0 |  1  0  0  1  0  0 |  - |       immr        |      imms         |      Rn        |        Rd      | AND              | 64-bit
  //  1 |  0  1 |  1  0  0  1  0  0 |  - |       immr        |      imms         |      Rn        |        Rd      | ORR              | 64-bit
  //  1 |  1  0 |  1  0  0  1  0  0 |  - |       immr        |      imms         |      Rn        |        Rd      | EOR              | 64-bit
  //  1 |  1  1 |  1  0  0  1  0  0 |  - |       immr        |      imms         |      Rn        |        Rd      | ANDS             | 64-bit
  def LogI = BitPat("b1??100100???????????????????????")
  def LogI_undef= BitPat("b1??1001000??????????????????????") // sf = 1 && N != 1
  def LogI_AND  = BitPat("b100100100???????????????????????")
  def LogI_ORR  = BitPat("b101100100???????????????????????")
  def LogI_EOR  = BitPat("b110100100???????????????????????")
  def LogI_ANDS = BitPat("b111100100???????????????????????")
  // DecodeBitMask : immediate && (imms & levels) == levels : levels = 0b111111 => imms == 0b111111
  def DecBitMask_LogI_undef
                = BitPat("b1??1001000??????111111??????????")

  // Bitfield
  // 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf |  opc  |  1  0  0  1  1  0 |  N |       immr        |      imms         |      Rn        |        Rd      |                  |
  //  1 |  0  0 |  1  0  0  1  1  0 |  1 |       immr        |      imms         |      Rn        |        Rd      | SBFM             | 64-bit
  //  1 |  0  1 |  1  0  0  1  1  0 |  1 |       immr        |      imms         |      Rn        |        Rd      | BFM              | 64-bit
  //  1 |  1  0 |  1  0  0  1  1  0 |  1 |       immr        |      imms         |      Rn        |        Rd      | UBFM             | 64-bit
  def BitF = BitPat("b1??1001101??????????????????????")
  def BitF_undef= BitPat("b1??10011010?????????????????????") // sf = 1 && N != 1
  def BitF_SBFM = BitPat("b1001001101??????????????????????")
  def BitF_BFM  = BitPat("b1011001101??????????????????????")
  def BitF_UBFM = BitPat("b1101001101??????????????????????")

  // Add/subtract (shifted register) */
  // 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant
  // sf | op |  S |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      |                  |
  //  1 |  0 |  0 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ADD              | 64-bit  */
  //  1 |  0 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ADDS             | 64-bit  */
  //  1 |  1 |  0 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | SUB              | 64-bit  */
  //  1 |  1 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | SUBS             | 64-bit  */
  def ASSR = BitPat("b1??01011??0?????????????????????")
  def ASSR_ADD  = BitPat("b10001011??0?????????????????????")
  def ASSR_ADDS = BitPat("b10101011??0?????????????????????")
  def ASSR_SUB  = BitPat("b11001011??0?????????????????????")
  def ASSR_SUBS = BitPat("b11101011??0?????????????????????")

  // Add/subtract (immediate)
  // I_ASImm
  // 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf | op |  S |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | ADD              | 64-bit
  //  1 |  0 |  0 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | ADD              | 64-bit
  //  1 |  0 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | ADDS             | 64-bit
  //  1 |  1 |  0 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | SUB              | 64-bit
  //  1 |  1 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | SUBS             | 64-bit
  // Note on shift : 01 -> LSL #12, nothing else possible
  def ASImm =  BitPat("b1??100010???????????????????????")
  def ASImm_ADD_I  = BitPat("b100100010???????????????????????")
  def ASImm_ADDS_I = BitPat("b101100010???????????????????????")
  def ASImm_SUB_I  = BitPat("b110100010???????????????????????")
  def ASImm_SUBS_I = BitPat("b111100010???????????????????????")

  //  Load register (literal) */
  //  V = 1 bit is for SIMD, we ignore
  //  31 30 | 29 28 27 | 26 | 25 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  //  opc  |  0  1  1 |  V |  0  0 |                           imm19                          |       Rt       |                  |
  //  0  0 |  0  1  1 |  0 |  0  0 |                           imm19                          |       Rt       | LDR              | 32-bit
  //  0  1 |  0  1  1 |  0 |  0  0 |                           imm19                          |       Rt       | LDR              | 64-bit
  //  1  0 |  0  1  1 |  0 |  0  0 |                           imm19                          |       Rt       | LDRSW            |
  //  1  1 |  0  1  1 |  0 |  0  0 |                           imm19                          |       Rt       | PRFM             |
  def LDR_I = BitPat("b01011000????????????????????????")

}
