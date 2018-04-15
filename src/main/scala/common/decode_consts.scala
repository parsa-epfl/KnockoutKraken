package common

import chisel3._
import chisel3.util.BitPat

import INSTRUCTIONS._
import DECODE_CONTROL_SIGNALS._

object DECODE_CONTROL_SIGNALS
{
  // Default Values
  private def W_MAX_IMM = 26.W
  def IMM_T = UInt(W_MAX_IMM)
  val IMM_X = 0.U(W_MAX_IMM)

  // Controls Signals
  val Y =   true.B
  val N =   false.B
  def C_T = Bool()

  // ALU Operation Signals
  private def W_ALU = 3.W
  def OP_ALU_T = UInt(W_ALU)
  val OP_AND  = 0.U(W_ALU)
  val OP_BIC  = 1.U(W_ALU)
  val OP_ORR  = 2.U(W_ALU)
  val OP_ORN  = 3.U(W_ALU)
  val OP_EOR  = 4.U(W_ALU)
  val OP_EON  = 5.U(W_ALU)
  val OP_ADD  = 6.U(W_ALU)
  val OP_SUB  = 7.U(W_ALU)
  val OP_ALU_X = 0.U(W_ALU)

  // Shiift Operation Signals
  private def W_SHIFT = 2.W
  def SHIFT_T = UInt(W_SHIFT)
  val LSL  = 0.U(W_SHIFT)
  val LSR  = 1.U(W_SHIFT)
  val ASR  = 2.U(W_SHIFT)
  val ROR  = 3.U(W_SHIFT)
  val SHIFT_X = 0.U(W_SHIFT)
}

object DECODE_MATCHING_TABLES
{

  def decode_default: List[UInt] =
    //
    //          ALU    RD         INSTRUCTION
    //  INSTR    OP    EN           VALID
    //  TYPE     |     | RS1          |
    //    |      |     |  EN          |
    //    |      |     |  | RS2  SHIFT|
    //    |      |     |  |  EN    EN |
    //    |      |     |  |  | IMM |  |
    //    |      |     |  |  |  EN |  |
    //    |      |     |  |  |  |  |  |
    //    |      |     |  |  |  |  |  |
    //    |      |     |  |  |  |  |  |
    List(I_X,OP_ALU_X, N, N, N, N, N, N)

  def decode_table: Array[(BitPat, List[UInt])]  =
    Array(
      /* Logical (shifted register) 64-bit */
      AND  -> List(I_LogSR, OP_AND, Y, Y, Y, N, Y, Y),
      BIC  -> List(I_LogSR, OP_BIC, Y, Y, Y, N, Y, Y),
      ORR  -> List(I_LogSR, OP_ORR, Y, Y, Y, N, Y, Y),
      ORN  -> List(I_LogSR, OP_ORN, Y, Y, Y, N, Y, Y),
      EOR  -> List(I_LogSR, OP_EOR, Y, Y, Y, N, Y, Y),
      EON  -> List(I_LogSR, OP_EON, Y, Y, Y, N, Y, Y)
    )
}

// Non hardware types for Testing and debbuging
object DECODE_INTEGER_LITERALS
{
  // Default Values
  val IMM_X = 0
  val REG_X = 0

  // Controls Signals
  val Y = 1
  val N = 0

  // ALU Operation Signals
  val OP_AND  = 0
  val OP_BIC  = 1
  val OP_ORR  = 2
  val OP_ORN  = 3
  val OP_EOR  = 4
  val OP_EON  = 5
  val OP_ADD  = 6
  val OP_SUB  = 7
  val OP_ALU_X = 0

  // Shiift Operation Signals
  val LSL     = 0
  val LSR     = 1
  val ASR     = 2
  val ROR     = 3
  val SHIFT_X = 0

  // Types
  private def W_TYPE = 2
  val I_X     = 0
  val I_LogSR = 1


  def decode_table(inst_type : Int ): List[BigInt] =

    inst_type match {
      //                   RD         INSTRUCTION
      //                   EN           VALID
      //                   | RS1          |
      //                   |  EN          |
      //                   |  | RS2  SHIFT|
      //                   |  |  EN    EN |
      //                   |  |  | IMM |  |
      //                   |  |  |  EN |  |
      //                   |  |  |  |  |  |
      //                   |  |  |  |  |  |
      //                   |  |  |  |  |  |
      case I_X     => List(N, N, N, N, N, N)
      case I_LogSR => List(Y, Y, Y, N, Y, Y)

  }

}
