package arm

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

  // Test and branch (immediate)
  // 31 | 30 29 28 27 26 25 | 24 | 23 22 21 20 19 | 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // b5 |  0  1  1  0  1  1 | op |      b40       |                  imm14                    |        Rt      |                  |
  // b5 |  0  1  1  0  1  1 |  0 |      b40       |                  imm14                    |        Rt      | TBZ              |
  // b5 |  0  1  1  0  1  1 |  1 |      b40       |                  imm14                    |        Rt      | TBNZ             |
  def TBImm = BitPat("b?011011?????????????????????????")
  def TBImm_TBZ32  = BitPat("b00110110????????????????????????")
  def TBImm_TBNZ32 = BitPat("b00110111????????????????????????")
  def TBImm_TBZ    = BitPat("b10110110????????????????????????")
  def TBImm_TBNZ   = BitPat("b10110111????????????????????????")

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

  // Unconditional branch (register)
  // 31 30 29 28 27 26 25 | 24 | 23 | 22  21 | 20 19 18 17 16 | 15 14 13 12 | 11 | 10 | 09 08 07 06 05 | 04 | 03 02 01 00 | Instruction Page | Variant
  //  1  1  0  1  0  1  1 |  0 |  0 |  0   0 |  1  1  1  1  1 |  0  0  0  0 |  0 |  0 |      Rn        |  0 |  0  0  0  0 | BR               |
  //  1  1  0  1  0  1  1 |  0 |  0 |  0   1 |  1  1  1  1  1 |  0  0  0  0 |  0 |  0 |      Rn        |  0 |  0  0  0  0 | BLR              |
  //  1  1  0  1  0  1  1 |  0 |  0 |  1   0 |  1  1  1  1  1 |  0  0  0  0 |  0 |  0 |      Rn        |  0 |  0  0  0  0 | RET              |
  def BReg = BitPat("b110101100??11111000000?????00000")
  def BReg_BR   = BitPat("b1101011000011111000000?????00000")
  def BReg_BLR  = BitPat("b1101011000111111000000?????00000")
  def BReg_RET  = BitPat("b1101011001011111000000?????00000")

  // Compare and branch (immediate)          Bits[31:24]
  // 31 | 30 29 28 27 26 25 | 24 | 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf |  0  1  1  0  1  0 | op |                             imm19                        |      Rt        |                  |
  //  0 |  0  1  1  0  1  0 |  0 |                             imm19                        |      Rt        | CBZ              | 32-bit
  //  0 |  0  1  1  0  1  0 |  1 |                             imm19                        |      Rt        | CBNZ             | 32-bit
  //  1 |  0  1  1  0  1  0 |  0 |                             imm19                        |      Rt        | CBZ              | 64-bit
  //  1 |  0  1  1  0  1  0 |  1 |                             imm19                        |      Rt        | CBNZ             | 64-bit
  def CBImm = BitPat("b?011010?????????????????????????")
  def CBImm_CBZ32 = BitPat("b00110100????????????????????????")
  def CBImm_CBNZ32= BitPat("b00110101????????????????????????")
  def CBImm_CBZ  = BitPat("b10110100????????????????????????")
  def CBImm_CBNZ = BitPat("b10110101????????????????????????")

  // Logical (shifted register)
  // 31 | 30 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant
  // sf |  opc  |  0  1  0  1  0 | shift |  N |      Rm        |       imm6        |       Rn       |       Rd      |                  |
  //  0 |  1  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | TST              | 32-bit
  //  0 |  0  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | AND              | 32-bit
  //  0 |  0  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | BIC              | 32-bit
  //  0 |  0  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ORR              | 32-bit
  //  0 |  0  1 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | ORN              | 32-bit
  //  0 |  1  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | EOR              | 32-bit
  //  0 |  1  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | EON              | 32-bit
  //  0 |  1  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ANDS             | 32-bit
  //  0 |  1  1 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | BICS             | 32-bit
  //  1 |  1  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | TST              | 64-bit
  //  1 |  0  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | AND              | 64-bit
  //  1 |  0  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | BIC              | 64-bit
  //  1 |  0  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ORR              | 64-bit
  //  1 |  0  1 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | ORN              | 64-bit
  //  1 |  1  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | EOR              | 64-bit
  //  1 |  1  0 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | EON              | 64-bit
  //  1 |  1  1 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      | ANDS             | 64-bit
  //  1 |  1  1 |  0  1  0  1  0 | shift |  1 |      Rm        |       imm6        |       Rn       |       Rd      | BICS             | 64-bit
  def LogSR = BitPat("b1??01010??0?????????????????????")
  def LogSR_TST32  = BitPat("b01101010??0????????????????11111")
  def LogSR_AND32  = BitPat("b00001010??0?????????????????????")
  def LogSR_BIC32  = BitPat("b00001010??1?????????????????????")
  def LogSR_ORR32  = BitPat("b00101010??0?????????????????????")
  def LogSR_ORN32  = BitPat("b00101010??1?????????????????????")
  def LogSR_EOR32  = BitPat("b01001010??0?????????????????????")
  def LogSR_EON32  = BitPat("b01001010??1?????????????????????")
  def LogSR_ANDS32 = BitPat("b01101010??0?????????????????????")
  def LogSR_BICS32 = BitPat("b01101010??1?????????????????????")
  def LogSR_TST  = BitPat("b11101010??0????????????????11111")
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
  //  0 |  1  1 |  1  0  0  1  0  0 |  0 |       immr        |      imms         |      Rn        |  1  1  1  1  1 | TST              | 32-bit
  //  0 |  0  0 |  1  0  0  1  0  0 |  0 |       immr        |      imms         |      Rn        |        Rd      | AND              | 32-bit
  //  0 |  0  1 |  1  0  0  1  0  0 |  0 |       immr        |      imms         |      Rn        |        Rd      | ORR              | 32-bit
  //  0 |  1  0 |  1  0  0  1  0  0 |  0 |       immr        |      imms         |      Rn        |        Rd      | EOR              | 32-bit
  //  0 |  1  1 |  1  0  0  1  0  0 |  0 |       immr        |      imms         |      Rn        |        Rd      | ANDS             | 32-bit
  //  1 |  1  1 |  1  0  0  1  0  0 |  - |       immr        |      imms         |      Rn        |  1  1  1  1  1 | TST              | 64-bit
  //  1 |  0  0 |  1  0  0  1  0  0 |  - |       immr        |      imms         |      Rn        |        Rd      | AND              | 64-bit
  //  1 |  0  1 |  1  0  0  1  0  0 |  - |       immr        |      imms         |      Rn        |        Rd      | ORR              | 64-bit
  //  1 |  1  0 |  1  0  0  1  0  0 |  - |       immr        |      imms         |      Rn        |        Rd      | EOR              | 64-bit
  //  1 |  1  1 |  1  0  0  1  0  0 |  - |       immr        |      imms         |      Rn        |        Rd      | ANDS             | 64-bit
  def LogI = BitPat("b1??100100???????????????????????")
  def LogI_TST32 = BitPat("b0111001000?????????????????11111")
  def LogI_AND32 = BitPat("b0001001000??????????????????????")
  def LogI_ORR32 = BitPat("b0011001000??????????????????????")
  def LogI_EOR32 = BitPat("b0101001000??????????????????????")
  def LogI_ANDS32= BitPat("b0111001000??????????????????????")
  def LogI_TST   = BitPat("b111100100??????????????????11111")
  def LogI_AND   = BitPat("b100100100???????????????????????")
  def LogI_ORR   = BitPat("b101100100???????????????????????")
  def LogI_EOR   = BitPat("b110100100???????????????????????")
  def LogI_ANDS  = BitPat("b111100100???????????????????????")

  // Move wide (immediate)
  // 31 | 30 29 | 28 27 26 25 24 23 | 22 21 | 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf |  opc  |  1  0  0  1  0  1 |  hw   |                     imm16                       |        Rd      |                  |
  //  0 |  0  0 |  1  0  0  1  0  1 |  0  - |                     imm16                       |        Rd      | MOVN             | 32-bit
  //  0 |  1  0 |  1  0  0  1  0  1 |  0  - |                     imm16                       |        Rd      | MOVZ             | 32-bit
  //  0 |  1  1 |  1  0  0  1  0  1 |  0  - |                     imm16                       |        Rd      | MOVK             | 32-bit
  //  1 |  0  0 |  1  0  0  1  0  1 |   -   |                     imm16                       |        Rd      | MOVN             | 64-bit
  //  1 |  1  0 |  1  0  0  1  0  1 |   -   |                     imm16                       |        Rd      | MOVZ             | 64-bit
  //  1 |  1  1 |  1  0  0  1  0  1 |   -   |                     imm16                       |        Rd      | MOVK             | 64-bit
  def MovI = BitPat("b1??100101???????????????????????")
  def MovI_MOVN32 = BitPat("b0001001010??????????????????????")
  def MovI_MOVZ32 = BitPat("b0101001010??????????????????????")
  def MovI_MOVK32 = BitPat("b0111001010??????????????????????")
  def MovI_MOVN = BitPat("b100100101???????????????????????")
  def MovI_MOVZ = BitPat("b110100101???????????????????????")
  def MovI_MOVK = BitPat("b111100101???????????????????????")

  // Bitfield
  // 31 | 30 29 | 28 27 26 25 24 23 | 22 | 21 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf |  opc  |  1  0  0  1  1  0 |  N |       immr        |      imms         |      Rn        |        Rd      |                  |
  //  0 |  0  0 |  1  0  0  1  1  0 |  0 |       immr        |      imms         |      Rn        |        Rd      | SBFM             | 32-bit
  //  0 |  0  1 |  1  0  0  1  1  0 |  0 |       immr        |      imms         |      Rn        |        Rd      | BFM              | 32-bit
  //  0 |  1  0 |  1  0  0  1  1  0 |  0 |       immr        |      imms         |      Rn        |        Rd      | UBFM             | 32-bit
  //  1 |  0  0 |  1  0  0  1  1  0 |  1 |       immr        |      imms         |      Rn        |        Rd      | SBFM             | 64-bit
  //  1 |  0  1 |  1  0  0  1  1  0 |  1 |       immr        |      imms         |      Rn        |        Rd      | BFM              | 64-bit
  //  1 |  1  0 |  1  0  0  1  1  0 |  1 |       immr        |      imms         |      Rn        |        Rd      | UBFM             | 64-bit
  def BitF = BitPat("b1??1001101??????????????????????")
  def BitF_SBFM32 = BitPat("b0001001100??????????????????????")
  def BitF_BFM32  = BitPat("b0011001100??????????????????????")
  def BitF_UBFM32 = BitPat("b0101001100??????????????????????")
  def BitF_SBFM   = BitPat("b1001001101??????????????????????")
  def BitF_BFM    = BitPat("b1011001101??????????????????????")
  def BitF_UBFM   = BitPat("b1101001101??????????????????????")

  // Data-processing (1 source)
  // 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf |  1 |  S |  1  1  0  1  0  1  1  0 |    opcode2     |      opcode       |      Rn        |        Rd      |                  |
  //  0 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  0  0  0 |      Rn        |        Rd      | RBIT             | 32-bit
  //  0 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  0  0  1 |      Rn        |        Rd      | REV16            | 32-bit
  //  0 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  0  1  0 |      Rn        |        Rd      | REV              | 32-bit
  //  0 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  1  0  0 |      Rn        |        Rd      | CLZ              | 32-bit
  //  0 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  1  0  1 |      Rn        |        Rd      | CLS              | 32-bit
  //  1 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  0  0  0 |      Rn        |        Rd      | RBIT             | 64-bit
  //  1 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  0  0  1 |      Rn        |        Rd      | REV16            | 64-bit
  //  1 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  0  1  0 |      Rn        |        Rd      | REV32            | 64-bit
  //  1 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  0  1  1 |      Rn        |        Rd      | REV              | 64-bit
  //  1 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  1  0  0 |      Rn        |        Rd      | CLZ              | 64-bit
  //  1 |  1 |  0 |  1  1  0  1  0  1  1  0 |  0  0  0  0  0 |  0  0  0  1  0  1 |      Rn        |        Rd      | CLS              | 64-bit
  def DP1S_RBIT32  = BitPat("b0101101011000000000000??????????")
  def DP1S_32REV16 = BitPat("b0101101011000000000001??????????")
  def DP1S_32REV32 = BitPat("b0101101011000000000010??????????")
  def DP1S_CLZ32   = BitPat("b0101101011000000000100??????????")
  def DP1S_CLS32   = BitPat("b0101101011000000000101??????????")
  def DP1S_RBIT    = BitPat("b1101101011000000000000??????????")
  def DP1S_REV16   = BitPat("b1101101011000000000001??????????")
  def DP1S_REV32   = BitPat("b1101101011000000000010??????????")
  def DP1S_REV     = BitPat("b1101101011000000000011??????????")
  def DP1S_CLZ     = BitPat("b1101101011000000000100??????????")
  def DP1S_CLS     = BitPat("b1101101011000000000101??????????")

  // Data-processing (2 sources)
  // Don't support Division
  // 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf |  0 |  S |  1  1  0  1  0  1  1  0 |       Rm       |      opcode       |      Rn        |        Rd      |                  |
  //  0 |  0 |  S |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  0  0 |      Rn        |        Rd      | LSLV             | 32-bit
  //  0 |  0 |  S |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  0  1 |      Rn        |        Rd      | LSRV             | 32-bit
  //  0 |  0 |  S |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  1  0 |      Rn        |        Rd      | ASRV             | 32-bit
  //  0 |  0 |  S |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  1  1 |      Rn        |        Rd      | RORV             | 32-bit
  //  1 |  0 |  S |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  0  0 |      Rn        |        Rd      | LSLV             | 64-bit
  //  1 |  0 |  S |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  0  1 |      Rn        |        Rd      | LSRV             | 64-bit
  //  1 |  0 |  S |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  1  0 |      Rn        |        Rd      | ASRV             | 64-bit
  //  1 |  0 |  S |  1  1  0  1  0  1  1  0 |       Rm       |  0  0  1  0  1  1 |      Rn        |        Rd      | RORV             | 64-bit
  def DP2S_LSLV32 = BitPat("b00011010110?????001000??????????")
  def DP2S_LSRV32 = BitPat("b00011010110?????001001??????????")
  def DP2S_ASRV32 = BitPat("b00011010110?????001010??????????")
  def DP2S_RORV32 = BitPat("b00011010110?????001011??????????")
  def DP2S_LSLV = BitPat("b10011010110?????001000??????????")
  def DP2S_LSRV = BitPat("b10011010110?????001001??????????")
  def DP2S_ASRV = BitPat("b10011010110?????001010??????????")
  def DP2S_RORV = BitPat("b10011010110?????001011??????????")

  // Add/subtract (shifted register)
  // 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf | op |  S |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd       |                  |
  //  1 |  1 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |  1  1  1  1  1 | CMP              | 32-bit  */
  //  1 |  0 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |  1  1  1  1  1 | CMN              | 32-bit  */
  //  1 |  0 |  0 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd       | ADD              | 32-bit  */
  //  1 |  0 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd       | ADDS             | 32-bit  */
  //  1 |  1 |  0 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd       | SUB              | 32-bit  */
  //  1 |  1 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd       | SUBS             | 32-bit  */
  //  1 |  1 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |  1  1  1  1  1 | CMP              | 64-bit  */
  //  1 |  0 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |  1  1  1  1  1 | CMN              | 64-bit  */
  //  1 |  0 |  0 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd       | ADD              | 64-bit  */
  //  1 |  0 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd       | ADDS             | 64-bit  */
  //  1 |  1 |  0 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd       | SUB              | 64-bit  */
  //  1 |  1 |  1 |  0  1  0  1  1 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd       | SUBS             | 64-bit  */
  def ASSR = BitPat("b1??01011??0?????????????????????")
  def ASSR_CMP32 = BitPat("b01101011??0????????????????11111")
  def ASSR_CMN32 = BitPat("b00101011??0????????????????11111")
  def ASSR_ADD32 = BitPat("b00001011??0?????????????????????")
  def ASSR_ADDS32= BitPat("b00101011??0?????????????????????")
  def ASSR_SUB32 = BitPat("b01001011??0?????????????????????")
  def ASSR_SUBS32= BitPat("b01101011??0?????????????????????")
  def ASSR_CMP  = BitPat("b11101011??0????????????????11111")
  def ASSR_CMN  = BitPat("b10101011??0????????????????11111")
  def ASSR_ADD  = BitPat("b10001011??0?????????????????????")
  def ASSR_ADDS = BitPat("b10101011??0?????????????????????")
  def ASSR_SUB  = BitPat("b11001011??0?????????????????????")
  def ASSR_SUBS = BitPat("b11101011??0?????????????????????")

  // Add/subtract (extended register)
  // 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf | op |  S |  0  1  0  1  1 |  opt  |  1 |      Rm        |  option  |   imm3   |       Rn       |       Rd       |                  |
  //  1 |  0 |  0 |  0  1  0  1  1 |  0  0 |  1 |      Rm        |  option  |   imm3   |       Rn       |  1  1  1  1  1 | CMP              | 32-bit  */
  def ASER = BitPat("b1??01011??1?????????????????????")
  def ASER_ADD = BitPat("b10001011??1?????????????????????")

  // Add/subtract (immediate)
  // 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf | op |  S |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |                |                  |
  //  0 |  1 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |  1  1  1  1  1 | CMP              | 32-bit
  //  0 |  0 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |  1  1  1  1  1 | CMN              | 32-bit
  //  0 |  0 |  0 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | ADD              | 32-bit
  //  0 |  0 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | ADDS             | 32-bit
  //  0 |  1 |  0 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | SUB              | 32-bit
  //  0 |  1 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | SUBS             | 32-bit
  //  1 |  1 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |  1  1  1  1  1 | CMP              | 64-bit
  //  1 |  0 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |  1  1  1  1  1 | CMN              | 64-bit
  //  1 |  0 |  0 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | ADD              | 64-bit
  //  1 |  0 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | ADDS             | 64-bit
  //  1 |  1 |  0 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | SUB              | 64-bit
  //  1 |  1 |  1 |  1  0  0  0  1 | 0  sh |               imm12                 |      Rn        |        Rd      | SUBS             | 64-bit
  // Note on shift : 01 -> LSL #12, nothing else possible
  def ASImm =  BitPat("b1??100010???????????????????????")
  def ASImm_CMP32 = BitPat("b011100010??????????????????11111")
  def ASImm_CMN32 = BitPat("b001100010??????????????????11111")
  def ASImm_ADD32 = BitPat("b000100010???????????????????????")
  def ASImm_ADDS32= BitPat("b001100010???????????????????????")
  def ASImm_SUB32 = BitPat("b010100010???????????????????????")
  def ASImm_SUBS32= BitPat("b011100010???????????????????????")
  def ASImm_CMP   = BitPat("b111100010??????????????????11111")
  def ASImm_CMN   = BitPat("b101100010??????????????????11111")
  def ASImm_ADD   = BitPat("b100100010???????????????????????")
  def ASImm_ADDS  = BitPat("b101100010???????????????????????")
  def ASImm_SUB   = BitPat("b110100010???????????????????????")
  def ASImm_SUBS  = BitPat("b111100010???????????????????????")

  // Add/subtract (extended register) */
  // 31 | 30 | 29 | 28 27 26 25 24 | 23 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  // sf | op |  S |  0  1  0  1  1 |  opt  |  1 |      Rm        |  option  |   imm3   |       Rn       |       Rd       |                  |

  // Conditional select
  // 31 | 30 | 29 | 28 27 26 25 24 23 22 21 | 20 19 18 17 16 | 15 14 13 12 | 11 10 | 09 08 07 06 05 | 04 03 02 01 00| Instruction Page | Variant
  // sf | op |  S |  1  1  0  1  0  1  0  0 |       Rm       |      cond   |  op2  |       Rn       |       Rd      |                  |
  //  0 |  0 |  0 |  1  1  0  1  0  1  0  0 |       Rm       |      cond   |  0  0 |       Rn       |       Rd      | CSEL             | 32-bit
  //  0 |  0 |  0 |  1  1  0  1  0  1  0  0 |       Rm       |      cond   |  0  1 |       Rn       |       Rd      | CSINC            | 32-bit
  //  0 |  1 |  0 |  1  1  0  1  0  1  0  0 |       Rm       |      cond   |  0  0 |       Rn       |       Rd      | CSINV            | 32-bit
  //  0 |  1 |  0 |  1  1  0  1  0  1  0  0 |       Rm       |      cond   |  0  1 |       Rn       |       Rd      | CSNEG            | 32-bit
  //  1 |  0 |  0 |  1  1  0  1  0  1  0  0 |       Rm       |      cond   |  0  0 |       Rn       |       Rd      | CSEL             | 64-bit
  //  1 |  0 |  0 |  1  1  0  1  0  1  0  0 |       Rm       |      cond   |  0  1 |       Rn       |       Rd      | CSINC            | 64-bit
  //  1 |  1 |  0 |  1  1  0  1  0  1  0  0 |       Rm       |      cond   |  0  0 |       Rn       |       Rd      | CSINV            | 64-bit
  //  1 |  1 |  0 |  1  1  0  1  0  1  0  0 |       Rm       |      cond   |  0  1 |       Rn       |       Rd      | CSNEG            | 64-bit
  def CSel = BitPat("b1?011010100?????????0???????????")
  def CSel_CSEL32  = BitPat("b00011010100?????????00??????????")
  def CSel_CSINC32 = BitPat("b00011010100?????????01??????????")
  def CSel_CSINV32 = BitPat("b01011010100?????????00??????????")
  def CSel_CSNEG32 = BitPat("b01011010100?????????01??????????")
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

  // Load/store register (unsigned immediate)
  // 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 20 19 18 17 16 15 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  //  size |  1  1  1 |  V |  0  1 |  opc  |              imm12                  |       Rn       |       Rt       |                  |
  //  0  0 |  1  1  1 |  0 |  0  1 |  0  0 |              imm12                  |       Rn       |       Rt       | STRB             | unsigned offset
  //  0  1 |  1  1  1 |  0 |  0  1 |  0  0 |              imm12                  |       Rn       |       Rt       | STRH             | unsigned offset
  //  1  0 |  1  1  1 |  0 |  0  1 |  0  0 |              imm12                  |       Rn       |       Rt       | STR              | 32-bit
  //  1  1 |  1  1  1 |  0 |  0  1 |  0  0 |              imm12                  |       Rn       |       Rt       | STR              | 64-bit
  //  0  0 |  1  1  1 |  0 |  0  1 |  0  1 |              imm12                  |       Rn       |       Rt       | LDRB             | unsigned offset
  //  0  1 |  1  1  1 |  0 |  0  1 |  0  1 |              imm12                  |       Rn       |       Rt       | LDRH             | unsigned offset
  //  1  0 |  1  1  1 |  0 |  0  1 |  0  1 |              imm12                  |       Rn       |       Rt       | LDR              | 32-bit
  //  1  1 |  1  1  1 |  0 |  0  1 |  0  1 |              imm12                  |       Rn       |       Rt       | LDR              | 64-bit
  def LSUImm = BitPat("b??1110010???????????????????????")
  def LSUImm_STRB  = BitPat("b0011100100??????????????????????")
  def LSUImm_STRH  = BitPat("b0111100100??????????????????????")
  def LSUImm_STR32 = BitPat("b1011100100??????????????????????")
  def LSUImm_STR64 = BitPat("b1111100100??????????????????????")
  def LSUImm_LDRB  = BitPat("b0011100101??????????????????????")
  def LSUImm_LDRH  = BitPat("b0111100101??????????????????????")
  def LSUImm_LDR32 = BitPat("b1011100101??????????????????????")
  def LSUImm_LDR64 = BitPat("b1111100101??????????????????????")
  //  0  0 |  1  1  1 |  0 |  0  1 |  1  0 |                imm12                |       Rn       |       Rt       | LDRSB            | 64-bit
  //  0  0 |  1  1  1 |  0 |  0  1 |  1  1 |                imm12                |       Rn       |       Rt       | LDRSB            | 32-bit
  //  0  1 |  1  1  1 |  0 |  0  1 |  1  0 |                imm12                |       Rn       |       Rt       | LDRSH            | 64-bit
  //  0  1 |  1  1  1 |  0 |  0  1 |  1  1 |                imm12                |       Rn       |       Rt       | LDRSH            | 32-bit
  //  1  0 |  1  1  1 |  0 |  0  1 |  1  0 |                imm12                |       Rn       |       Rt       | LDRSW            | unsigned offset
  //  1  1 |  1  1  1 |  0 |  0  1 |  1  0 |                imm12                |       Rn       |       Rt       | PRFUM            | unsigned offset
  def LSUImm_LDRSB64 = BitPat("b0011100110??????????????????????")
  def LSUImm_LDRSB32 = BitPat("b0011100111??????????????????????")
  def LSUImm_LDRSH64 = BitPat("b0111100110??????????????????????")
  def LSUImm_LDRSH32 = BitPat("b0111100111??????????????????????")
  def LSUImm_LDRSW   = BitPat("b1011100110??????????????????????")
  //def LSUImm_PRFUM = BitPat("b1111100110??????????????????????")

  // Load/store pair register (post-indexed)
  // 31 30 | 29 28 27 | 26 | 25 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  //  opc  |  1  0  1 |  V |  0  0  1 |  L |         imm7         |       Rt2      |       Rn       |       Rt       |                  |
  //  0  0 |  1  0  1 |  0 |  0  0  1 |  0 |         imm7         |       Rt2      |       Rn       |       Rt       | STP              | 32-bit
  //  1  0 |  1  0  1 |  0 |  0  0  1 |  0 |         imm7         |       Rt2      |       Rn       |       Rt       | STP              | 64-bit
  //  0  0 |  1  0  1 |  0 |  0  0  1 |  1 |         imm7         |       Rt2      |       Rn       |       Rt       | LDP              | 32-bit
  //  1  0 |  1  0  1 |  0 |  0  0  1 |  1 |         imm7         |       Rt2      |       Rn       |       Rt       | LDP              | 64-bit
  def LSPairPo = BitPat("b101010010???????????????????????")
  def LSPairPo_STP32 = BitPat("b0010100010??????????????????????")
  def LSPairPo_STP64 = BitPat("b1010100010??????????????????????")
  def LSPairPo_LDP32 = BitPat("b0010100011??????????????????????")
  def LSPairPo_LDP64 = BitPat("b1010100011??????????????????????")


  // Load/store pair register (signed offset)
  // 31 30 | 29 28 27 | 26 | 25 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  //  opc  |  1  0  1 |  V |  0  1  0 |  L |         imm7         |       Rt2      |       Rn       |       Rt       |                  |
  //  0  0 |  1  0  1 |  0 |  0  1  0 |  0 |         imm7         |       Rt2      |       Rn       |       Rt       | STP              | 32-bit
  //  1  0 |  1  0  1 |  0 |  0  1  0 |  0 |         imm7         |       Rt2      |       Rn       |       Rt       | STP              | 64-bit
  //  0  0 |  1  0  1 |  0 |  0  1  0 |  1 |         imm7         |       Rt2      |       Rn       |       Rt       | LDP              | 32-bit
  //  1  0 |  1  0  1 |  0 |  0  1  0 |  1 |         imm7         |       Rt2      |       Rn       |       Rt       | LDP              | 64-bit
  def LSPair = BitPat("b101010010???????????????????????")
  def LSPair_STP32 = BitPat("b0010100100??????????????????????")
  def LSPair_STP64 = BitPat("b1010100100??????????????????????")
  def LSPair_LDP32 = BitPat("b0010100101??????????????????????")
  def LSPair_LDP64 = BitPat("b1010100101??????????????????????")

  // Load/store pair register (pre-indexed)
  // 31 30 | 29 28 27 | 26 | 25 24 23 | 22 | 21 20 19 18 17 16 15 | 14 13 12 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  //  opc  |  1  0  1 |  V |  0  1  1 |  L |         imm7         |       Rt2      |       Rn       |       Rt       |                  |
  //  0  0 |  1  0  1 |  0 |  0  1  1 |  0 |         imm7         |       Rt2      |       Rn       |       Rt       | STP              | 32-bit
  //  1  0 |  1  0  1 |  0 |  0  1  1 |  0 |         imm7         |       Rt2      |       Rn       |       Rt       | STP              | 64-bit
  //  0  0 |  1  0  1 |  0 |  0  1  1 |  1 |         imm7         |       Rt2      |       Rn       |       Rt       | LDP              | 32-bit
  //  1  0 |  1  0  1 |  0 |  0  1  1 |  1 |         imm7         |       Rt2      |       Rn       |       Rt       | LDP              | 64-bit
  def LSPairPr = BitPat("b101010010???????????????????????")
  def LSPairPr_STP32 = BitPat("b0010100110??????????????????????")
  def LSPairPr_STP64 = BitPat("b1010100110??????????????????????")
  def LSPairPr_LDP32 = BitPat("b0010100111??????????????????????")
  def LSPairPr_LDP64 = BitPat("b1010100111??????????????????????")

  // Load/store register (register offset)
  // 31 30 | 29 28 27 | 26 | 25 24 | 23 | 22 | 21 | 20 19 18 17 16 | 15 14 13 | 12 | 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  //  size |  1  1  1 |  V |  0  0 |   opc   |  1 |       Rm       |  option  |  S |  1  0 |       Rn       |       Rt       |                  |
  //  size |  1  1  1 |  V |  0  0 |  0 |  L |  1 |       Rm       |  option  |  S |  1  0 |       Rn       |       Rt       |                  |
  //  0  0 |  1  1  1 |  0 |  0  0 |  0 |  0 |  1 |       Rm       |  x  1  x |  S |  1  0 |       Rn       |       Rt       |  STRB            | register
  //  0  1 |  1  1  1 |  0 |  0  0 |  0 |  0 |  1 |       Rm       |  x  1  x |  S |  1  0 |       Rn       |       Rt       |  STRH            | register
  //  1  0 |  1  1  1 |  0 |  0  0 |  0 |  0 |  1 |       Rm       |  x  1  x |  S |  1  0 |       Rn       |       Rt       |  STR             | 32-bit
  //  1  1 |  1  1  1 |  0 |  0  0 |  0 |  0 |  1 |       Rm       |  x  1  x |  S |  1  0 |       Rn       |       Rt       |  STR             | 64-bit
  //  0  0 |  1  1  1 |  0 |  0  0 |  0 |  1 |  1 |       Rm       |  x  1  x |  S |  1  0 |       Rn       |       Rt       |  LDRB            | register
  //  0  1 |  1  1  1 |  0 |  0  0 |  0 |  1 |  1 |       Rm       |  x  1  x |  S |  1  0 |       Rn       |       Rt       |  LDRH            | register
  //  1  0 |  1  1  1 |  0 |  0  0 |  0 |  1 |  1 |       Rm       |  x  1  x |  S |  1  0 |       Rn       |       Rt       |  LDR             | 32-bit
  //  1  1 |  1  1  1 |  0 |  0  0 |  0 |  1 |  1 |       Rm       |  x  1  x |  S |  1  0 |       Rn       |       Rt       |  LDR             | 64-bit
  def LSReg = BitPat("b??1110000?1??????1??10??????????")
  def LSReg_STRB  = BitPat("b00111000001??????1??10??????????")
  def LSReg_STRH  = BitPat("b01111000001??????1??10??????????")
  def LSReg_STR32 = BitPat("b10111000001??????1??10??????????")
  def LSReg_STR64 = BitPat("b11111000001??????1??10??????????")
  def LSReg_LDRB  = BitPat("b00111000011??????1??10??????????")
  def LSReg_LDRH  = BitPat("b01111000011??????1??10??????????")
  def LSReg_LDR32 = BitPat("b10111000011??????1??10??????????")
  def LSReg_LDR64 = BitPat("b11111000011??????1??10??????????")

  // Load/store register (post-indexed)
  // 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  //  size |  1  1  1 |  V |  0  0 |  opc  |  0 |           imm9             |  0  1 |       Rn       |       Rt       |                  |
  //  0  0 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |           imm9             |  0  1 |       Rn       |       Rt       |  STRB            |
  //  0  0 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |           imm9             |  0  1 |       Rn       |       Rt       |  LDRB            |
  //  0  0 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |           imm9             |  0  1 |       Rn       |       Rt       |  LDRSB           | 64-bit
  //  0  0 |  1  1  1 |  0 |  0  0 |  1  1 |  0 |           imm9             |  0  1 |       Rn       |       Rt       |  LDRSB           | 32-bit
  //  0  1 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |           imm9             |  0  1 |       Rn       |       Rt       |  STRH            |
  //  0  1 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |           imm9             |  0  1 |       Rn       |       Rt       |  LDRH            |
  //  0  1 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |           imm9             |  0  1 |       Rn       |       Rt       |  LDRSH           | 64-bit
  //  0  1 |  1  1  1 |  0 |  0  0 |  1  1 |  0 |           imm9             |  0  1 |       Rn       |       Rt       |  LDRSH           | 32-bit
  //  1  0 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |           imm9             |  0  1 |       Rn       |       Rt       |  STR             | 32-bit
  //  1  0 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |           imm9             |  0  1 |       Rn       |       Rt       |  LDR             | 32-bit
  //  1  0 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |           imm9             |  0  1 |       Rn       |       Rt       |  LDRSW           |
  //  1  1 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |           imm9             |  0  1 |       Rn       |       Rt       |  STR             | 64-bit
  //  1  1 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |           imm9             |  0  1 |       Rn       |       Rt       |  LDR             | 64-bit
  //  1  1 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |           imm9             |  0  1 |       Rn       |       Rt       |  PRFUM           |
  def LSRegPo        = BitPat("b??111000??0?????????01??????????")
  def LSRegPo_STRB   = BitPat("b00111000000?????????01??????????")
  def LSRegPo_LDRB   = BitPat("b00111000010?????????01??????????")
  def LSRegPo_LDRSB  = BitPat("b00111000100?????????01??????????")
  def LSRegPo_LDRSB32= BitPat("b00111000110?????????01??????????")
  def LSRegPo_STRH   = BitPat("b01111000000?????????01??????????")
  def LSRegPo_LDRH   = BitPat("b01111000010?????????01??????????")
  def LSRegPo_LDRSH  = BitPat("b01111000100?????????01??????????")
  def LSRegPo_LDRSH32= BitPat("b01111000110?????????01??????????")
  def LSRegPo_STR32  = BitPat("b10111000000?????????01??????????")
  def LSRegPo_LDR32  = BitPat("b10111000010?????????01??????????")
  def LSRegPo_LDRSW  = BitPat("b10111000100?????????01??????????")
  def LSRegPo_STR    = BitPat("b11111000000?????????01??????????")
  def LSRegPo_LDR    = BitPat("b11111000010?????????01??????????")

  // Load/store register (pre-indexed)
  // 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  //  size |  1  1  1 |  V |  0  0 |  opc  |  0 |           imm9             |  1  1 |       Rn       |       Rt       |                  |
  //  0  0 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |           imm9             |  1  1 |       Rn       |       Rt       |  STURB           |
  //  0  0 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |           imm9             |  1  1 |       Rn       |       Rt       |  LDURB           |
  //  0  0 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |           imm9             |  1  1 |       Rn       |       Rt       |  LDURSB          | 64-bit
  //  0  0 |  1  1  1 |  0 |  0  0 |  1  1 |  0 |           imm9             |  1  1 |       Rn       |       Rt       |  LDURSB          | 32-bit
  //  0  1 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |           imm9             |  1  1 |       Rn       |       Rt       |  STURH           |
  //  0  1 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |           imm9             |  1  1 |       Rn       |       Rt       |  LDURH           |
  //  0  1 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |           imm9             |  1  1 |       Rn       |       Rt       |  LDURSH          | 64-bit
  //  0  1 |  1  1  1 |  0 |  0  0 |  1  1 |  0 |           imm9             |  1  1 |       Rn       |       Rt       |  LDURSH          | 32-bit
  //  1  0 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |           imm9             |  1  1 |       Rn       |       Rt       |  STUR            | 32-bit
  //  1  0 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |           imm9             |  1  1 |       Rn       |       Rt       |  LDUR            | 32-bit
  //  1  0 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |           imm9             |  1  1 |       Rn       |       Rt       |  LDURSW          |
  //  1  1 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |           imm9             |  1  1 |       Rn       |       Rt       |  STUR            | 64-bit
  //  1  1 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |           imm9             |  1  1 |       Rn       |       Rt       |  LDUR            | 64-bit
  //  1  1 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |           imm9             |  1  1 |       Rn       |       Rt       |  PRFUM           |
  def LSRegPr        = BitPat("b??111000??0?????????11??????????")
  def LSRegPr_STRB   = BitPat("b00111000000?????????11??????????")
  def LSRegPr_LDRB   = BitPat("b00111000010?????????11??????????")
  def LSRegPr_LDRSB  = BitPat("b00111000100?????????11??????????")
  def LSRegPr_LDRSB32= BitPat("b00111000110?????????11??????????")
  def LSRegPr_STRH   = BitPat("b01111000000?????????11??????????")
  def LSRegPr_LDRH   = BitPat("b01111000010?????????11??????????")
  def LSRegPr_LDRSH  = BitPat("b01111000100?????????11??????????")
  def LSRegPr_LDRSH32= BitPat("b01111000110?????????11??????????")
  def LSRegPr_STR32  = BitPat("b10111000000?????????11??????????")
  def LSRegPr_LDR32  = BitPat("b10111000010?????????11??????????")
  def LSRegPr_LDRSW  = BitPat("b10111000100?????????11??????????")
  def LSRegPr_STR    = BitPat("b11111000000?????????11??????????")
  def LSRegPr_LDR    = BitPat("b11111000010?????????11??????????")

  // Load/store register (unscaled immediate)
  // 31 30 | 29 28 27 | 26 | 25 24 | 23 22 | 21 | 20 19 18 17 16 15 14 13 12 | 11 10 | 09 08 07 06 05 | 04 03 02 01 00 | Instruction Page | Variant
  //  size |  1  1  1 |  V |  0  0 |  opc  |  0 |           imm9             |  0  0 |       Rn       |       Rt       |                  |
  //  0  0 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |           imm9             |  0  0 |       Rn       |       Rt       |  STURB           |
  //  0  0 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |           imm9             |  0  0 |       Rn       |       Rt       |  LDURB           |
  //  0  0 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |           imm9             |  0  0 |       Rn       |       Rt       |  LDURSB          | 64-bit
  //  0  0 |  1  1  1 |  0 |  0  0 |  1  1 |  0 |           imm9             |  0  0 |       Rn       |       Rt       |  LDURSB          | 32-bit
  //  0  1 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |           imm9             |  0  0 |       Rn       |       Rt       |  STURH           |
  //  0  1 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |           imm9             |  0  0 |       Rn       |       Rt       |  LDURH           |
  //  0  1 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |           imm9             |  0  0 |       Rn       |       Rt       |  LDURSH          | 64-bit
  //  0  1 |  1  1  1 |  0 |  0  0 |  1  1 |  0 |           imm9             |  0  0 |       Rn       |       Rt       |  LDURSH          | 32-bit
  //  1  0 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |           imm9             |  0  0 |       Rn       |       Rt       |  STUR            | 32-bit
  //  1  0 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |           imm9             |  0  0 |       Rn       |       Rt       |  LDUR            | 32-bit
  //  1  0 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |           imm9             |  0  0 |       Rn       |       Rt       |  LDURSW          |
  //  1  1 |  1  1  1 |  0 |  0  0 |  0  0 |  0 |           imm9             |  0  0 |       Rn       |       Rt       |  STUR            | 64-bit
  //  1  1 |  1  1  1 |  0 |  0  0 |  0  1 |  0 |           imm9             |  0  0 |       Rn       |       Rt       |  LDUR            | 64-bit
  //  1  1 |  1  1  1 |  0 |  0  0 |  1  0 |  0 |           imm9             |  0  0 |       Rn       |       Rt       |  PRFUM           |
  def LSUReg         = BitPat("b??111000??0?????????00??????????")
  def LSUReg_STURB   = BitPat("b00111000000?????????00??????????")
  def LSUReg_LDURB   = BitPat("b00111000010?????????00??????????")
  def LSUReg_LDURSB  = BitPat("b00111000100?????????00??????????")
  def LSUReg_LDURSB32= BitPat("b00111000110?????????00??????????")
  def LSUReg_STURH   = BitPat("b01111000000?????????00??????????")
  def LSUReg_LDURH   = BitPat("b01111000010?????????00??????????")
  def LSUReg_LDURSH  = BitPat("b01111000100?????????00??????????")
  def LSUReg_LDURSH32= BitPat("b01111000110?????????00??????????")
  def LSUReg_STUR32  = BitPat("b10111000000?????????00??????????")
  def LSUReg_LDUR32  = BitPat("b10111000010?????????00??????????")
  def LSUReg_LDURSW  = BitPat("b10111000100?????????00??????????")
  def LSUReg_STUR    = BitPat("b11111000000?????????00??????????")
  def LSUReg_LDUR    = BitPat("b11111000010?????????00??????????")
  def LSUReg_PRFUM   = BitPat("b11111000100?????????00??????????")
}
