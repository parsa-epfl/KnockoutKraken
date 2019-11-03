package common

import chisel3._
import chisel3.util.BitPat
import DECODE_CONTROL_SIGNALS.{N, Y, _}

object DECODE_CONTROL_SIGNALS
{
  // Valault Defues
  val IMM_W = DEC_LITS.IMM_W.W
  def IMM_T = UInt(IMM_W)
  val IMM_X = DEC_LITS.IMM_X.U(IMM_W)

  // Controls Signals
  def C_T = Bool()
  val Y = true.B
  val N = false.B

  // Inst OP's
  val OP_W = DEC_LITS.OP_W.W
  def OP_T = UInt(OP_W)
  val OP_X = DEC_LITS.OP_X.U(OP_W)

  // ALU Operation Signals
  val OP_AND = DEC_LITS.OP_AND.U(OP_W)
  val OP_BIC = DEC_LITS.OP_BIC.U(OP_W)
  val OP_ORR = DEC_LITS.OP_ORR.U(OP_W)
  val OP_ORN = DEC_LITS.OP_ORN.U(OP_W)
  val OP_EOR = DEC_LITS.OP_EOR.U(OP_W)
  val OP_EON = DEC_LITS.OP_EON.U(OP_W)
  val OP_ADD = DEC_LITS.OP_ADD.U(OP_W)
  val OP_SUB = DEC_LITS.OP_SUB.U(OP_W)

  // Branches Signals
  val OP_B = DEC_LITS.OP_B.U(OP_W)
  val OP_BL = DEC_LITS.OP_BL.U(OP_W)
  val OP_BCOND = DEC_LITS.OP_BCOND.U(OP_W)

  // Shiift Operation Signals
  val SHIFT_VAL_W = DEC_LITS.SHIFT_VAL_W.W
  def SHIFT_VAL_T = UInt(SHIFT_VAL_W)
  val SHIFT_VAL_X = DEC_LITS.SHIFT_VAL_X.U(SHIFT_VAL_W)
  val SHIFT_TYPE_W = DEC_LITS.SHIFT_TYPE_W.W
  def SHIFT_TYPE_T = UInt(SHIFT_TYPE_W)
  val SHIFT_TYPE_X = DEC_LITS.SHIFT_TYPE_X.U(SHIFT_TYPE_W)
  val LSL  = DEC_LITS.LSL.U(SHIFT_TYPE_W)
  val LSR  = DEC_LITS.LSR.U(SHIFT_TYPE_W)
  val ASR  = DEC_LITS.ASR.U(SHIFT_TYPE_W)
  val ROR  = DEC_LITS.ROR.U(SHIFT_TYPE_W)

  // Cond Operations Signals
  val COND_W = DEC_LITS.COND_W.W
  def COND_T = UInt(COND_W)
  val COND_X = DEC_LITS.COND_X.U(COND_W)
  // Not needed, already in 4 bit form
  // val EQ = DEC_LITS.EQ.U(COND_W)
  // val NE = DEC_LITS.NE.U(COND_W)
  // val CS = DEC_LITS.CS.U(COND_W)
  // val HS = DEC_LITS.HS.U(COND_W)
  // val CC = DEC_LITS.CC.U(COND_W)
  // val LO = DEC_LITS.LO.U(COND_W)
  // val MI = DEC_LITS.MI.U(COND_W)
  // val PL = DEC_LITS.PL.U(COND_W)
  // val VS = DEC_LITS.VS.U(COND_W)
  // val VC = DEC_LITS.VC.U(COND_W)
  // val HI = DEC_LITS.HI.U(COND_W)
  // val LS = DEC_LITS.LS.U(COND_W)
  // val GE = DEC_LITS.GE.U(COND_W)
  // val LT = DEC_LITS.LT.U(COND_W)
  // val GT = DEC_LITS.GT.U(COND_W)
  // val LE = DEC_LITS.LE.U(COND_W)
  // val AL = DEC_LITS.AL.U(COND_W)
  // val NV = DEC_LITS.NV.U(COND_W)

  // load/store signal
  val OP_LDR = DEC_LITS.OP_LDR.U(OP_W)

  // Instruction Types for chisel
  val TYPE_W = 3.W
  def I_T = UInt(TYPE_W)
  val I_X = DEC_LITS.I_X.U(TYPE_W)
  val I_LogSR = DEC_LITS.I_LogSR.U(TYPE_W)
  val I_BImm  = DEC_LITS.I_BImm.U(TYPE_W)
  val I_BCImm = DEC_LITS.I_BCImm.U(TYPE_W)
  val I_ASSR  = DEC_LITS.I_ASSR.U(TYPE_W)
  val I_ASImm = DEC_LITS.I_ASImm.U(TYPE_W)
  val I_LSImm = DEC_LITS.I_LSImm.U(TYPE_W)
}

object DECODE_MATCHING_TABLES
{
  import INSTRUCTIONS._

  def decode_default: List[UInt] =
    //
    //              RD
    //  IN TR       EN
    //  TYPE        | RS1
    //    |         |  EN        COND
    //    |         |  |           EN
    //    |         |  |           | NZCV
    //    |    INST |  | RS2  SHIFT|  EN
    //    |     OP  |  |  EN    EN |  |
    //    |     |   |  |  | IMM |  |  |
    //    |     |   |  |  |  EN |  |  |
    //    |     |   |  |  |  |  |  |  |
    //    |     |   |  |  |  |  |  |  |
    //    |     |   |  |  |  |  |  |  |
    List(I_X, OP_X, N, N, N, N, N, N, N)

  def decode_table: Array[(BitPat, List[UInt])]  =
      //
      //                                      RD
      //                    IN TR             EN
      //                    TYPE              | RS1
      //                      |               |  EN        COND
      //                      |               |  |           EN
      //                      |               |  |           | NZCV
      //                      |      INST     |  | RS2  SHIFT|  EN
      //                      |       OP      |  |  EN    EN |  |
      //                      |       |       |  |  | IMM |  |  |
      //                      |       |       |  |  |  EN |  |  |
      //                      |       |       |  |  |  |  |  |  |
      //                      |       |       |  |  |  |  |  |  |
      //                      |       |       |  |  |  |  |  |  |
    Array(
      // Logical (shifted register)
      LogSR_AND    -> List(I_LogSR, OP_AND,   Y, Y, Y, N, Y, N, N),
      LogSR_BIC    -> List(I_LogSR, OP_BIC,   Y, Y, Y, N, Y, N, N),
      LogSR_ORR    -> List(I_LogSR, OP_ORR,   Y, Y, Y, N, Y, N, N),
      LogSR_ORN    -> List(I_LogSR, OP_ORN,   Y, Y, Y, N, Y, N, N),
      LogSR_EOR    -> List(I_LogSR, OP_EOR,   Y, Y, Y, N, Y, N, N),
      LogSR_EON    -> List(I_LogSR, OP_EON,   Y, Y, Y, N, Y, N, N),
      LogSR_ANDS   -> List(I_LogSR, OP_AND,   Y, Y, Y, N, Y, N, Y),
      LogSR_BICS   -> List(I_LogSR, OP_BIC,   Y, Y, Y, N, Y, N, Y),
      // Unconditional branch (immediate)
      BImm_B       -> List(I_BImm , OP_B,     N, N, N, Y, N, N, N),
      BImm_BL      -> List(I_BImm , OP_BL,    N, N, N, Y, N, N, N),
      // Conditional branch (immediate)
      BCond        -> List(I_BCImm, OP_BCOND, N, N, N, Y, N, Y, N),
      // Add/subtract (shifted register)
      ASSR_ADD     -> List(I_ASSR, OP_ADD,    Y, Y, Y, N, Y, N, N),
      ASSR_ADDS    -> List(I_ASSR, OP_ADD,    Y, Y, Y, N, Y, N, Y),
      ASSR_SUB     -> List(I_ASSR, OP_SUB,    Y, Y, Y, N, Y, N, N),
      ASSR_SUBS    -> List(I_ASSR, OP_SUB,    Y, Y, Y, N, Y, N, Y),
      // Add/subtract (immediate)
      ASImm_ADD_I  -> List(I_ASImm, OP_ADD,   Y, Y, N, Y, Y, N, N),
      ASImm_ADDS_I -> List(I_ASImm, OP_ADD,   Y, Y, N, Y, Y, N, Y),
      ASImm_SUB_I  -> List(I_ASImm, OP_SUB,   Y, Y, N, Y, Y, N, N),
      ASImm_SUBS_I -> List(I_ASImm, OP_SUB,   Y, Y, N, Y, Y, N, Y),
      // load/store (immediate)
      LDR_I        -> List(I_LSImm, OP_LDR,   Y, N, N, Y, N, N, N),
    )
}

// Non hardware types for Testing and debbuging
object DEC_LITS
{
  // Valault Defues
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
  val OP_ADD = 6
  val OP_SUB = 7

  // Branches singals
  val OP_B = 0
  val OP_BL = 1
  val OP_BCOND = 2

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
  val TYPE_W = 3
  val I_X = 0
  val I_LogSR = 1 // Logical (shifted register)
  val I_BImm  = 2 // Unconditional branch (immediate)
  val I_BCImm = 3 // Conditional branch (immediate)
  val I_ASSR  = 4 // Add/subtract (shifted register)
  val I_ASImm = 5 // ADD/Subdract with Immediate
  val I_LSImm = 6 // Load/Store Immediate

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
  val LI_BImm  = List(N, N, N, Y, N, N, N)
  val LI_BCImm = List(N, N, N, Y, N, Y, N)
  val LI_ASSR  = List(Y, Y, Y, N, Y, N, N)
  val LI_ASImm = List(Y, Y, N, Y, Y, N, N)
  val LI_LSImm = List(Y, N, N, Y, N, N, N)

  def decode_table(inst_type : Int): List[Int] =
    inst_type match {
      case I_X     => LI_X
      case I_LogSR => LI_LogSR
      case I_BImm  => LI_BImm
      case I_BCImm => LI_BCImm
      case I_ASSR  => LI_ASSR
      case I_ASImm => LI_ASImm
      case I_LSImm => LI_LSImm
    }
}
