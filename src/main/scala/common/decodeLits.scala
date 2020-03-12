package common

// Non hardware types for Testing and debbuging
object DEC_LITS {
  // Default Values
  val IMM_W = 26
  val IMM_X = 0
  val REG_X = 0

  // Controls Signals
  val Y = 1
  val N = 0

  // ALU Operation Signals
  val OP_W = 4
  val OP_X = 0
  val OP_ALU_X = 0
  val OP_AND = 0
  val OP_BIC = 1
  val OP_ORR = 2
  val OP_ORN = 3
  val OP_EOR = 4
  val OP_EON = 5

  // PC-rel
  val OP_ADR  = 0
  val OP_ADRP = 1

  // Add/Subtract
  val OP_ADD = 0
  val OP_SUB = 1

  // Unconditonal branch (immediate)
  val OP_B = 0
  val OP_BL = 1

  // Conditional branch (immediate)
  val OP_BCOND = 0

  // Uncondional branch (register)
  val OP_BR = 0
  val OP_BLR = 1
  val OP_RET = 0 // Same as BR, but with different Hint_Branch (not Used)

  // Test and branch (immediate)
  val OP_TBZ  = 0
  val OP_TBNZ = 1

  // Move wide (immediate)
  val OP_MOVN = 0
  val OP_MOVZ = 1
  val OP_MOVK = 2

  // Bitfield
  val OP_SBFM = 0
  val OP_BFM  = 1
  val OP_UBFM = 2

  // Conditional select
  val OP_CSEL  = 0
  val OP_CSINC = 1
  val OP_CSINV = 2
  val OP_CSNEG = 3

  // Conditional compare
  val OP_CCMN = 0
  val OP_CCMP = 1

  // Data-processing (1 source)
  val OP_RBIT  = 0
  val OP_REV16 = 1
  val OP_REV32 = 2
  val OP_REV   = 3
  val OP_CLZ   = 4
  val OP_CLS   = 5

  // Conditional Branch
  val OP_CBZ  = 0
  val OP_CBNZ = 1

  // Shiift Operation Signals
  val SHIFT_VAL_W = 6 // maximum shift for 64 bit
  val SHIFT_VAL_X = 0
  val SHIFT_TYPE_W = 2
  val SHIFT_TYPE_X = 0
  val LSL = 0
  val LSR = 1
  val ASR = 2
  val ROR = 3

  // Cond Operations Signals C1-144
  val COND_W = 4
  val COND_X = 0
  val EQ = 0  // 000 0
  val NE = 1  // 000 1
  val CS = 2  // 001 0
  val HS = 2  // 001 0
  val CC = 3  // 001 1
  val LO = 3  // 001 1
  val MI = 4  // 010 0
  val PL = 5  // 010 1
  val VS = 6  // 011 0
  val VC = 7  // 011 1
  val HI = 8  // 100 0
  val LS = 9  // 100 1
  val GE = 10 // 101 0
  val LT = 11 // 101 1
  val GT = 12 // 110 0
  val LE = 13 // 110 1
  val AL = 14 // 111 0
  val NV = 15 // 111 1

  // Load/store operation
  // op(1,0) = size
  val SIZEB  = 0
  val SIZEH  = 1
  val SIZE32 = 2
  val SIZE64 = 3
  // op(2) = L
  val isLoad = (1 << 2)
  // op(3) = isSigned
  val isSigned = (1 << 3)

  val OP_STRB   = SIZEB
  val OP_STRH   = SIZEH
  val OP_STR32  = SIZE32
  val OP_STR64  = SIZE64
  val OP_LDRB   = SIZEB  + isLoad
  val OP_LDRH   = SIZEH  + isLoad
  val OP_LDR32  = SIZE32 + isLoad
  val OP_LDR64  = SIZE64 + isLoad

  val OP_LDRSB = SIZEB  + isLoad + isSigned
  val OP_LDRSH = SIZEH  + isLoad + isSigned
  val OP_LDRSW = SIZE32 + isLoad + isSigned

  val OP_STURB  = SIZEB
  val OP_STURH  = SIZEH
  val OP_STUR32 = SIZE32
  val OP_STUR64 = SIZE64
  val OP_LDURB  = SIZEB  + isLoad
  val OP_LDURH  = SIZEH  + isLoad
  val OP_LDUR32 = SIZE32 + isLoad
  val OP_LDUR64 = SIZE64 + isLoad

  val OP_LDURSB = SIZEB  + isLoad + isSigned
  val OP_LDURSH = SIZEH  + isLoad + isSigned
  val OP_LDURSW = SIZE32 + isLoad + isSigned

  // Pair register
  val OP_STP32 = SIZE32
  val OP_STP64 = SIZE64
  val OP_LDP32 = SIZE32 + isLoad
  val OP_LDP64 = SIZE64 + isLoad

  // Instruction Types for scala
  val TYPE_W = 5
  val I_X = 0
  val I_LogSR = 1  // Logical (shifted register)
  val I_LogI  = 2  // Logical (immediate)
  val I_BitF  = 3  // Bitfield

  val I_DP1S  = 4  // Data-processing (1 source)
  val I_DP2S  = 5  // Data-processing (2 source)
  val I_CCImm = 6  // Conditional compare (immediate)
  val I_CCReg = 7  // Conditional compare (register)

  val I_ASImm = 8  // Add/Subtract (Immediate)
  val I_ASSR  = 9  // Add/subtract (shifted register)
  val I_MovI  = 10 // Move wide (immediate)
  val I_CSel  = 11 // Conditional select

  val I_BImm  = 12 // Unconditional branch (immediate)
  val I_BCImm = 13 // Conditional branch (immediate)
  val I_BReg  = 14 // Conditional branch (register)
  val I_CBImm = 15 // Branch and Compare (immediate)

  val I_LSRReg  = 16 // Load/store register (register offset)
  val I_LSPoReg = 17 // Load/store register (post-indexed)
  val I_LSPrReg = 18 // Load/store register (pre-indexed)
  val I_LSUReg  = 19 // Load/store register (unscaled immediate)

  val I_LSUImm = 20 // Load/store (unsigned immediate)
  val I_LSPReg = 21 // Load/store pair register (signed offset)

  val I_TBImm  = 24 // Test and branch (immediate)
  val I_PCRel  = 25 // PC-Relative

  // TODO: Remove this list? Never used for something usefull
  //       Dropped AssemblyInstruction based verification for QEMU based
  //                  RD
  //                  EN
  //                  | RS1
  //                  |  EN        COND
  //                  |  |           EN
  //                  |  | RS2  SHIFT| NZCV
  //                  |  |  EN    EN |  EN
  //                  |  |  | IMM |  |  |
  //                  |  |  |  EN |  |  |
  //                  |  |  |  |  |  |  |
  //                  |  |  |  |  |  |  |
  //                  |  |  |  |  |  |  |
  val LI_X      = List(N, N, N, N, N, N, N)
  val LI_LogSR  = List(Y, Y, Y, N, Y, N, N)
  val LI_LogI   = List(Y, Y, N, Y, N, N, N)
  val LI_MovI   = List(Y, N, N, Y, N, N, N)
  val LI_BImm   = List(N, N, N, Y, N, N, N)
  val LI_BCImm  = List(N, N, N, Y, N, Y, N)
  val LI_ASSR   = List(Y, Y, Y, N, Y, N, N)
  val LI_ASImm  = List(Y, Y, N, Y, Y, N, N)
  val LI_BitF   = List(Y, Y, N, Y, N, N, N)
  val LI_CSel   = List(Y, Y, Y, N, N, Y, N)
  val LI_CCImm  = List(N, Y, N, Y, N, Y, Y)
  val LI_CCReg  = List(N, Y, Y, N, N, Y, Y)
  val LI_CBImm  = List(N, Y, N, Y, N, N, N)
  val LI_PCRel  = List(Y, N, N, Y, N, N, N)
  val LI_LSUImm = List(Y, Y, Y, Y, N, N, N)
  val LI_LSRReg = List(Y, Y, Y, Y, N, N, N)
  val LI_LSPReg = List(Y, Y, Y, Y, N, N, N)

  def decode_table(inst_type : Int): List[Int] =
    inst_type match {
      case I_X     => LI_X
      case I_LogSR => LI_LogSR
      case I_LogI  => LI_LogI
      case I_MovI  => LI_MovI
      case I_BImm  => LI_BImm
      case I_BCImm => LI_BCImm
      case I_ASSR  => LI_ASSR
      case I_ASImm => LI_ASImm
      case I_BitF  => LI_BitF
      case I_CSel  => LI_CSel
      case I_CCImm => LI_CCImm
      case I_CCReg => LI_CCReg
      case I_CBImm => LI_BCImm
      case I_PCRel => LI_PCRel
      case I_LSUImm => LI_LSUImm
      case I_LSRReg => LI_LSRReg
      case I_LSPReg => LI_LSPReg
    }
}