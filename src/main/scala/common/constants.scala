package common

import chisel3._
import chisel3.util.BitPat

object SIGNALS
{
  // Default Values
  private val W_REG = 5.W
  private val W_MAX_IMM = 26.W
  val REG_T = UInt(W_REG)
  val IMM_T = UInt(W_MAX_IMM)
  val REG_X = 0.U(W_REG)
  val IMM_X = 0.U(W_MAX_IMM)

  // Controls Signals
  val Y = Bool(true)
  val N = Bool(false)
  val C_T = Bool()

  // ALU Operation Signals
  private val W_ALU = 3.W
  val OP_ALU_T = UInt(W_ALU)
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
  private val W_SHIFT = 2.W
  val SHIFT_T = UInt(W_SHIFT)
  val LSL  = 0.U(W_SHIFT)
  val LSR  = 1.U(W_SHIFT)
  val ASR  = 2.U(W_SHIFT)
  val ROR  = 3.U(W_SHIFT)
  val SHIFT_X = 0.U(W_SHIFT)
}
