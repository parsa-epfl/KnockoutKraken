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


  // PC-rel. addressing
  // 31 | 30 29 | 28 27 26 25 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // op | immlo |  1  0  0  0  0 |                                 immhi                    |        Rd      |                  |
  //  0 | immlo |  1  0  0  0  0 |                                 immhi                    |        Rd      | ADR              |    -
  //  1 | immlo |  1  0  0  0  0 |                                 immhi                    |        Rd      | ADRP             |    -
  def PCRel = BitPat("b???10000????????????????????????")
  def PCRel_ADR  = BitPat("b0??10000????????????????????????")
  def PCRel_ADRP = BitPat("b1??10000????????????????????????")

  // Unconditional branch (immediate)
  // 31 | 30 29 28 27 26 | 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00 | Instruction Page | Variant
  // op |  0  0  1  0  1 |                                   imm26                                       |                  |    -
  //  0 |  0  0  1  0  1 |                                   imm26                                       | B                |    -
  //  1 |  0  0  1  0  1 |                                   imm26                                       | BL               |    -
  def BImm = BitPat("b?00101??????????????????????????")
  def BImm_B  = BitPat("b000101??????????????????????????")
  def BImm_BL = BitPat("b100101??????????????????????????")

  // Conditional branch (immediate)
  // 31 30 29 28 27 26 25 | 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 | 03 02 01 00 | Instruction Page | Variant
  //  0  1  0  1  0  1  0 | o1 |                             imm19                        | o0 |    cond     |                  |
  //  0  1  0  1  0  1  0 |  0 |                             imm19                        |  0 |    cond     | B.cond           |    -
  def BCImm = BitPat("b01010100???????????????????0????")
  def BCond = BitPat("b01010100???????????????????0????")

  // Compare and branch (immediate)          Bits[31:24]
  // 31 | 30 29 28 27 26 25 | 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf |  0  1  1  0  1  0 | op |                             imm19                        |      Rt        |                  |
  //  1 |  0  1  1  0  1  0 |  0 |                             imm19                        |      Rt        | CBZ              | 64-bit
  //  1 |  0  1  1  0  1  0 |  1 |                             imm19                        |      Rt        | CBNZ             | 64-bit
  def CBImm = BitPat("b?011010?????????????????????????")
  def CBImm_CBZ  = BitPat("b10110100????????????????????????")
  def CBImm_CBNZ = BitPat("b10110101????????????????????????")

  // Logical (shifted register)
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
  def LogI_AND  = BitPat("b1001001001??????????????????????")
  def LogI_ORR  = BitPat("b1011001001??????????????????????")
  def LogI_EOR  = BitPat("b1101001001??????????????????????")
  def LogI_ANDS = BitPat("b1111001001??????????????????????")

  // Move wide (immediate)
  // 31 | 30 29 | 28 27 26 25 24 23 | 22 21 | 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf |  opc  |  1  0  0  1  0  1 |  hw   |                     imm16                       |        Rd      |                  |
  //  1 |  0  0 |  1  0  0  1  0  1 |   -   |                     imm16                       |        Rd      | MOVN             | 64-bit
  //  1 |  1  0 |  1  0  0  1  0  1 |   -   |                     imm16                       |        Rd      | MOVZ             | 64-bit
  //  1 |  1  1 |  1  0  0  1  0  1 |   -   |                     imm16                       |        Rd      | MOVK             | 64-bit
  def MovI = BitPat("b1??100101???????????????????????")
  def MovI_MOVN = BitPat("b100100101???????????????????????")
  def MovI_MOVZ = BitPat("b110100101???????????????????????")
  def MovI_MOVK = BitPat("b111100101???????????????????????")

  // Bitfield
  // 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf |  opc  |  1  0  0  1  1  0 |  N |       immr        |      imms         |      Rn        |        Rd      |                  |
  //  1 |  0  0 |  1  0  0  1  1  0 |  1 |       immr        |      imms         |      Rn        |        Rd      | SBFM             | 64-bit
  //  1 |  0  1 |  1  0  0  1  1  0 |  1 |       immr        |      imms         |      Rn        |        Rd      | BFM              | 64-bit
  //  1 |  1  0 |  1  0  0  1  1  0 |  1 |       immr        |      imms         |      Rn        |        Rd      | UBFM             | 64-bit
  def BitF = BitPat("b1??1001101??????????????????????")
  def BitF_SBFM = BitPat("b1001001101??????????????????????")
  def BitF_BFM  = BitPat("b1011001101??????????????????????")
  def BitF_UBFM = BitPat("b1101001101??????????????????????")

  // Add/subtract (shifted register) */
  // 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf | op |  S |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd       |                  |
  //  1 |  1 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |  1  1  1  1  1 | CMP              | 64-bit  */
  //  1 |  0 |  0 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd       | ADD              | 64-bit  */
  //  1 |  0 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd       | ADDS             | 64-bit  */
  //  1 |  1 |  0 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd       | SUB              | 64-bit  */
  //  1 |  1 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd       | SUBS             | 64-bit  */
  def ASSR = BitPat("b1??01011??0?????????????????????")
  def ASSR_CMP  = BitPat("b11101011??0????????????????11111")
  def ASSR_ADD  = BitPat("b10001011??0?????????????????????")
  def ASSR_ADDS = BitPat("b10101011??0?????????????????????")
  def ASSR_SUB  = BitPat("b11001011??0?????????????????????")
  def ASSR_SUBS = BitPat("b11101011??0?????????????????????")

  // Conditional select
  // 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant
  // sf | op |  S |  1  1  0  1  0  1  0  0 |       Rm       |      cond   |  op2  |       Rn       |       Rd      |                  |
  //  1 |  0 |  0 |  1  1  0  1  0  1  0  0 |       Rm       |      cond   |  0  0 |       Rn       |       Rd      | CSEL             | 64-bit
  //  1 |  0 |  0 |  1  1  0  1  0  1  0  0 |       Rm       |      cond   |  0  1 |       Rn       |       Rd      | CSINC            | 64-bit
  //  1 |  1 |  0 |  1  1  0  1  0  1  0  0 |       Rm       |      cond   |  0  0 |       Rn       |       Rd      | CSINV            | 64-bit
  //  1 |  1 |  0 |  1  1  0  1  0  1  0  0 |       Rm       |      cond   |  0  1 |       Rn       |       Rd      | CSNEG            | 64-bit
  def CSel = BitPat("b1?011010100?????????0???????????")
  def CSel_CSEL  = BitPat("b10011010100?????????00??????????")
  def CSel_CSINC = BitPat("b10011010100?????????01??????????")
  def CSel_CSINV = BitPat("b11011010100?????????00??????????")
  def CSel_CSNEG = BitPat("b11011010100?????????01??????????")

  // Conditional compare (immediate)
  // 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 | 09 08 07 06 05 | 04 | 03 02 01 00| Instruction Page | Variant
  // sf | op |  S |  1  1  0  1  0  0  1  0 |     imm5       |      cond   |  1 | o2 |       Rn       | o3 |   nzcv     |                  |
  //  1 |  0 |  1 |  1  1  0  1  0  0  1  0 |     imm5       |      cond   |  1 |  0 |       Rn       |  0 |   nzcv     | CCMN             | 64-bit
  //  1 |  1 |  1 |  1  1  0  1  0  0  1  0 |     imm5       |      cond   |  1 |  0 |       Rn       |  0 |   nzcv     | CCMP             | 64-bit
  //  0 |  1 |  1 |  1  1  0  1  0  0  1  0 |     imm5       |      cond   |  1 |  0 |       Rn       |  0 |   nzcv     | CCMP             | 32-bit
  def CCImm = BitPat("b1?111010010?????????10?????0????")
  def CCImm_CCMN = BitPat("b10111010010?????????10?????0????")
  def CCImm_CCMP = BitPat("b11111010010?????????10?????0????")
  def CCImm_CCMP32 = BitPat("b01111010010?????????10?????0????")

  // Conditional compare (register)
  // 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 | 09 08 07 06 05 | 04 | 03 02 01 00| Instruction Page | Variant
  // sf | op |  S |  1  1  0  1  0  0  1  0 |      Rm        |      cond   |  0 | o2 |       Rn       | o3 |   nzcv     |                  |
  //  1 |  0 |  1 |  1  1  0  1  0  0  1  0 |      Rm        |      cond   |  0 |  0 |       Rn       |  0 |   nzcv     | CCMN             | 64-bit
  //  1 |  1 |  1 |  1  1  0  1  0  0  1  0 |      Rm        |      cond   |  0 |  0 |       Rn       |  0 |   nzcv     | CCMP             | 64-bit
  //  0 |  1 |  1 |  1  1  0  1  0  0  1  0 |      Rm        |      cond   |  0 |  0 |       Rn       |  0 |   nzcv     | CCMP             | 32-bit
  def CCReg = BitPat("b1?111010010?????????00?????0????")
  def CCReg_CCMN = BitPat("b10111010010?????????00?????0????")
  def CCReg_CCMP = BitPat("b11111010010?????????00?????0????")
  def CCReg_CCMP32 = BitPat("b01111010010?????????00?????0????")

  // Add/subtract (immediate)
  // 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf | op |  S |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | ADD              | 64-bit
  //  0 |  1 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |  1  1  1  1  1 | CMP              | 32-bit
  //  1 |  1 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |  1  1  1  1  1 | CMP              | 64-bit
  //  1 |  0 |  0 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | ADD              | 64-bit
  //  1 |  0 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | ADDS             | 64-bit
  //  1 |  1 |  0 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | SUB              | 64-bit
  //  1 |  1 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | SUBS             | 64-bit
  // Note on shift : 01 -> LSL #12, nothing else possible
  def ASImm =  BitPat("b1??100010???????????????????????")
  def ASImm_CMP32 = BitPat("b011100010??????????????????11111")
  def ASImm_CMP   = BitPat("b111100010??????????????????11111")
  def ASImm_ADD   = BitPat("b100100010???????????????????????")
  def ASImm_ADDS  = BitPat("b101100010???????????????????????")
  def ASImm_SUB   = BitPat("b110100010???????????????????????")
  def ASImm_SUBS  = BitPat("b111100010???????????????????????")

  //  Load register (literal) */
  //  V = 1 bit is for SIMD, we ignore
  //  31 30 | 29 28 27 | 26 | 25 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  //  opc  |  0  1  1 |  V |  0  0 |                           imm19                          |       Rt       |                  |
  //  0  0 |  0  1  1 |  0 |  0  0 |                           imm19                          |       Rt       | LDR              | 32-bit
  //  0  1 |  0  1  1 |  0 |  0  0 |                           imm19                          |       Rt       | LDR              | 64-bit
  //  1  0 |  0  1  1 |  0 |  0  0 |                           imm19                          |       Rt       | LDRSW            |
  //  1  1 |  0  1  1 |  0 |  0  0 |                           imm19                          |       Rt       | PRFM             |
  def LDR_I = BitPat("b01011000????????????????????????")

  // Load/store register (unsigned immediate) */
  // 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  //  size |  1  1  1 |  V |  0  1 |  opc  |              imm12                  |       Rn       |       Rt       |                  |
  //  0  0 |  1  1  1 |  0 |  0  1 |  0  0 |              imm12                  |       Rn       |       Rt       | STRB             | unsigned offset
  //  0  1 |  1  1  1 |  0 |  0  1 |  0  0 |              imm12                  |       Rn       |       Rt       | STRH             | unsigned offset
  //  1  0 |  1  1  1 |  0 |  0  1 |  0  0 |              imm12                  |       Rn       |       Rt       | STR              | 32-bit
  //  1  1 |  1  1  1 |  0 |  0  1 |  0  0 |              imm12                  |       Rn       |       Rt       | STR              | 64-bit
  //  0  0 |  1  1  1 |  0 |  0  1 |  0  1 |              imm12                  |       Rn       |       Rt       | LDRB            | unsigned offset
  //  0  1 |  1  1  1 |  0 |  0  1 |  0  1 |              imm12                  |       Rn       |       Rt       | LDRH            | unsigned offset
  //  1  0 |  1  1  1 |  0 |  0  1 |  0  1 |              imm12                  |       Rn       |       Rt       | LDR             | 32-bit
  //  1  1 |  1  1  1 |  0 |  0  1 |  0  1 |              imm12                  |       Rn       |       Rt       | LDR             | 64-bit
  def LSUImm = BitPat("b??1110010???????????????????????")
  def LSUImm_STR8  = BitPat("b0011100100??????????????????????")
  def LSUImm_STR16 = BitPat("b0111100100??????????????????????")
  def LSUImm_STR32 = BitPat("b1011100100??????????????????????")
  def LSUImm_STR64 = BitPat("b1111100100??????????????????????")
  def LSUImm_LDR8  = BitPat("b0011100101??????????????????????")
  def LSUImm_LDR16 = BitPat("b0111100101??????????????????????")
  def LSUImm_LDR32 = BitPat("b1011100101??????????????????????")
  def LSUImm_LDR64 = BitPat("b1111100101??????????????????????")
  //  0  0 |  1  1  1 |  0 |  0  1 |  1  0 |                imm12                |       Rn       |       Rt       | LDRSB            | 64-bit
  //  0  1 |  1  1  1 |  0 |  0  1 |  1  0 |                imm12                |       Rn       |       Rt       | LDRSH            | 64-bit
  //  1  0 |  1  1  1 |  0 |  0  1 |  1  0 |                imm12                |       Rn       |       Rt       | LDRSW            | unsigned offset
  //  1  1 |  1  1  1 |  0 |  0  1 |  1  0 |                imm12                |       Rn       |       Rt       | PRFUM            | unsigned offset
  //  0  0 |  1  1  1 |  0 |  0  1 |  1  1 |                imm12                |       Rn       |       Rt       | LDRSB            | 32-bit
  //  0  1 |  1  1  1 |  0 |  0  1 |  1  1 |                imm12                |       Rn       |       Rt       | LDRSH            | 32-bit

  // Load/store pair register (signed offset)
  // 31 30 | 29 28 27 | 26 | 25 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  //  opc  |  1  0  1 |  V |  0  1  0 |  L |         imm7         |       Rt2      |       Rn       |       Rt       |                  |
  //  1  0 |  1  0  1 |  0 |  0  1  0 |  0 |         imm7         |       Rt2      |       Rn       |       Rt       | STP              | 64-bit
  //  1  0 |  1  0  1 |  0 |  0  1  0 |  1 |         imm7         |       Rt2      |       Rn       |       Rt       | LDP              | 64-bit
  def LSPReg = BitPat("b101010010???????????????????????")
  def LSPReg_STP64 = BitPat("b1010100100??????????????????????")
  def LSPReg_LDP64 = BitPat("b1010100101??????????????????????")

  // Load/store register (register offset)
  // 31 30 | 29 28 27 | 26 | 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 | 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  //  opc  |  1  1  1 |  V |  0  0 |  0 |  L |  1 |       Rm       |  option  |  S |  1  0 |       Rn       |       Rt       |                  |
  //  0  0 |  1  1  1 |  0 |  0  0 |  0 |  0 |  1 |       Rm       |  x  1  x |  S |  1  0 |       Rn       |       Rt       |  STRB            | register
  //  0  1 |  1  1  1 |  0 |  0  0 |  0 |  0 |  1 |       Rm       |  x  1  x |  S |  1  0 |       Rn       |       Rt       |  STRH            | register
  //  1  0 |  1  1  1 |  0 |  0  0 |  0 |  0 |  1 |       Rm       |  x  0  x |  S |  1  0 |       Rn       |       Rt       |  STR             | 32-bit
  //  1  1 |  1  1  1 |  0 |  0  0 |  0 |  0 |  1 |       Rm       |  x  0  x |  S |  1  0 |       Rn       |       Rt       |  STR             | 64-bit
  //  0  0 |  1  1  1 |  0 |  0  0 |  0 |  1 |  1 |       Rm       |  x  1  x |  S |  1  0 |       Rn       |       Rt       |  LDRRB           | register
  //  0  1 |  1  1  1 |  0 |  0  0 |  0 |  1 |  1 |       Rm       |  x  1  x |  S |  1  0 |       Rn       |       Rt       |  LDRRH           | register
  //  1  0 |  1  1  1 |  0 |  0  0 |  0 |  1 |  1 |       Rm       |  x  0  x |  S |  1  0 |       Rn       |       Rt       |  LDR             | 32-bit
  //  1  1 |  1  1  1 |  0 |  0  0 |  0 |  1 |  1 |       Rm       |  x  0  x |  S |  1  0 |       Rn       |       Rt       |  LDR             | 64-bit
  def LSRReg = BitPat("b??111000001?????????10??????????")
  def LSRReg_STR8  = BitPat("b00111000001??????1??10??????????")
  def LSRReg_STR16 = BitPat("b01111000001??????1??10??????????")
  def LSRReg_STR32 = BitPat("b10111000001??????0??10??????????")
  def LSRReg_STR64 = BitPat("b11111000001??????0??10??????????")
  def LSRReg_LDR8  = BitPat("b00111000001??????1??10??????????")
  def LSRReg_LDR16 = BitPat("b01111000001??????1??10??????????")
  def LSRReg_LDR32 = BitPat("b10111000001??????0??10??????????")
  def LSRReg_LDR64 = BitPat("b11111000001??????0??10??????????")

  // Load/store (immediate post-indexed)
  // Load/store (immediate pre-indexed)
  // Load/store (unscaled immediate)
  //
  // 31 30 29   27  26 25 24 23 22 21  20    12 11 10 9    5 4    0
  // +----+-------+---+-----+-----+---+--------+-----+------+------+
  // |size| 1 1 1 | V | 0 0 | opc | 0 |  imm9  | idx |  Rn  |  Rt  |
  // +----+-------+---+-----+-----+---+--------+-----+------+------+
  //
  // idx = 01 -> post-indexed, 11 pre-indexed, 00 unscaled imm. (no writeback)
  //       10 -> unprivileged
  // V = 0 -> non-vector
  // size: 00 -> 8 bit, 01 -> 16 bit, 10 -> 32 bit, 11 -> 64bit
  // opc: 00 -> store, 01 -> loadu, 10 -> loads 64, 11 -> loads 32
  //
}
