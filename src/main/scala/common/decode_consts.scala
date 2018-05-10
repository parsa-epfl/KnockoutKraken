package common

import chisel3._
import chisel3.util.BitPat

import INSTRUCTIONS._
import DECODE_CONTROL_SIGNALS._

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
  val OP_BCOND = DEC_LITS.OP_BCOND.U(OP_W)

  // Shiift Operation Signals
  val SHIFT_W = DEC_LITS.SHIFT_W.W
  def SHIFT_T = UInt(SHIFT_W)
  val SHIFT_X = DEC_LITS.SHIFT_X.U(SHIFT_W)
  val LSL  = DEC_LITS.LSL.U(SHIFT_W)
  val LSR  = DEC_LITS.LSR.U(SHIFT_W)
  val ASR  = DEC_LITS.ASR.U(SHIFT_W)
  val ROR  = DEC_LITS.ROR.U(SHIFT_W)

  // Cond Operations Signals
  val COND_W = DEC_LITS.COND_W.W
  def COND_T = UInt(COND_W)
  val COND_X = DEC_LITS.COND_X.U(COND_W)
  // Not needed, already in 4 bit form
  //  val EQ = DEC_LITS.EQ.U(COND_W)
  //  val NE = DEC_LITS.NE.U(COND_W)
  //  val CS = DEC_LITS.CS.U(COND_W)
  //  val HS = DEC_LITS.HS.U(COND_W)
  //  val CC = DEC_LITS.CC.U(COND_W)
  //  val LO = DEC_LITS.LO.U(COND_W)
  //  val MI = DEC_LITS.MI.U(COND_W)
  //  val PL = DEC_LITS.PL.U(COND_W)
  //  val VS = DEC_LITS.VS.U(COND_W)
  //  val VC = DEC_LITS.VC.U(COND_W)
  //  val HI = DEC_LITS.HI.U(COND_W)
  //  val LS = DEC_LITS.LS.U(COND_W)
  //  val GE = DEC_LITS.GE.U(COND_W)
  //  val LT = DEC_LITS.LT.U(COND_W)
  //  val GT = DEC_LITS.GT.U(COND_W)
  //  val LE = DEC_LITS.LE.U(COND_W)
  //  val AL = DEC_LITS.AL.U(COND_W)
  //  val NV = DEC_LITS.NV.U(COND_W)
}

object DECODE_MATCHING_TABLES
{

  def decode_default: List[UInt] =
    //
    //              RD            INSTRUCTION
    //  IN TR       EN              VALID
    //  TYPE        | RS1        COND |
    //    |         |  EN          EN |
    //    |    INST |  | RS2  SHIFT|  |
    //    |     OP  |  |  EN    EN |  |
    //    |     |   |  |  | IMM |  |  |
    //    |     |   |  |  |  EN |  |  |
    //    |     |   |  |  |  |  |  |  |
    //    |     |   |  |  |  |  |  |  |
    //    |     |   |  |  |  |  |  |  |
    List(I_X, OP_X, N, N, N, N, N, N, N)

  def decode_table: Array[(BitPat, List[UInt])]  =
    Array(
      /* Logical (shifted register) 64-bit */
      AND  -> List(I_LogSR, OP_AND  , Y, Y, Y, N, Y, N, Y),
      BIC  -> List(I_LogSR, OP_BIC  , Y, Y, Y, N, Y, N, Y),
      ORR  -> List(I_LogSR, OP_ORR  , Y, Y, Y, N, Y, N, Y),
      ORN  -> List(I_LogSR, OP_ORN  , Y, Y, Y, N, Y, N, Y),
      EOR  -> List(I_LogSR, OP_EOR  , Y, Y, Y, N, Y, N, Y),
      EON  -> List(I_LogSR, OP_EON  , Y, Y, Y, N, Y, N, Y),
      B    -> List(I_BImm , OP_B    , N, N, N, Y, N, N, Y),
      BL   -> List(I_BImm , OP_B    , N, N, N, Y, N, N, Y),
      BCond-> List(I_BCImm, OP_BCOND, N, N, N, Y, N, Y, Y)
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
  val OP_BCOND = 1

  // Shiift Operation Signals
  val SHIFT_W = 2
  val SHIFT_X = 0
  val LSL = 0
  val LSR = 1
  val ASR = 2
  val ROR = 3

  // Cond Operations Signals C1-144
  val COND_W = 4
  val COND_X = 0
  val EQ = 0
  val NE = 1
  val CS = 2
  val HS = 2
  val CC = 3
  val LO = 3
  val MI = 4
  val PL = 5
  val VS = 6
  val VC = 7
  val HI = 8
  val LS = 9
  val GE = 10
  val LT = 11
  val GT = 12
  val LE = 13
  val AL = 14
  val NV = 15

  // Types
  val TYPE_W = 2
  val I_X = 0
  val I_LogSR = 1
  val I_BImm  = 2
  val I_BCImm = 3

  def decode_table(inst_type : Int ): List[BigInt] =
    inst_type match {
      //                   RD            INSTRUCTION
      //                   EN              VALID
      //                   | RS1        COND |
      //                   |  EN          EN |
      //                   |  | RS2  SHIFT|  |
      //                   |  |  EN    EN |  |
      //                   |  |  | IMM |  |  |
      //                   |  |  |  EN |  |  |
      //                   |  |  |  |  |  |  |
      //                   |  |  |  |  |  |  |
      //                   |  |  |  |  |  |  |
      case I_X     => List(N, N, N, N, N, N, N)
      case I_LogSR => List(Y, Y, Y, N, Y, N, Y)
      case I_BImm  => List(N, N, N, Y, N, N, Y)
      case I_BCImm => List(N, N, N, Y, N, Y, Y)
    }

}
