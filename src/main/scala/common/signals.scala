package common

import chisel3._
import chisel3.util.BitPat

object SIGNALS
{
  // Default Values
  private def W_REG = 5.W
  private def W_MAX_IMM = 26.W
  def REG_T = UInt(W_REG)
  def IMM_T = UInt(W_MAX_IMM)
  val REG_X = 0.U(W_REG)
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
