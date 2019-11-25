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
  val OP_W = 3
  val OP_X = 0
  val OP_ALU_X = 0
  val OP_AND = 0
  val OP_BIC = 1
  val OP_ORR = 2
  val OP_ORN = 3
  val OP_EOR = 4
  val OP_EON = 5

  // Add/Subtract
  val OP_ADD = 0
  val OP_SUB = 1

  // Branches signals
  val OP_B = 0
  val OP_BL = 1
  val OP_BCOND = 2

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

  // load/store operation signals
  val OP_LDR = 0

  // Instruction Types for scala
  val TYPE_W = 4
  val I_X = 0
  val I_LogSR = 1 // Logical (shifted register)
  val I_LogI  = 2 // Logical (immediate)
  val I_MovI  = 3 // Move wide (immediate)
  val I_BImm  = 4 // Unconditional branch (immediate)
  val I_BCImm = 5 // Conditional branch (immediate)
  val I_ASSR  = 6 // Add/subtract (shifted register)
  val I_ASImm = 7 // Add/Subtract (Immediate)
  val I_LSImm = 8 // Load/Store Immediate
  val I_BitF  = 9 // Logical (shifted register)
  val I_CSel = 10 // Conditional select

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
  val LI_X     = List(N, N, N, N, N, N, N)
  val LI_LogSR = List(Y, Y, Y, N, Y, N, N)
  val LI_LogI  = List(Y, Y, N, Y, N, N, N)
  val LI_MovI  = List(Y, N, N, Y, N, N, N)
  val LI_BImm  = List(N, N, N, Y, N, N, N)
  val LI_BCImm = List(N, N, N, Y, N, Y, N)
  val LI_ASSR  = List(Y, Y, Y, N, Y, N, N)
  val LI_ASImm = List(Y, Y, N, Y, Y, N, N)
  val LI_LSImm = List(Y, N, N, Y, N, N, N)
  val LI_BitF  = List(Y, Y, N, Y, N, N, N)
  val LI_CSel  = List(Y, Y, Y, N, N, Y, N)

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
      case I_LSImm => LI_LSImm
      case I_BitF  => LI_BitF
      case I_CSel  => LI_CSel
    }
}
