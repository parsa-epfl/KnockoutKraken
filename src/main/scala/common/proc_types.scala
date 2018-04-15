package common

import chisel3._
import chisel3.util.BitPat

/*
 * Bit Patterns for Instructions
 *
 */
object PROCESSOR_TYPES
{
  // Number of threads
  def NUM_THREADS = 4
  def W_NUM_THREAD = chisel3.util.log2Ceil(NUM_THREADS).W // 4 Threads
  def TAG_T = UInt(W_NUM_THREAD)
  val TAG_X = 0.U(W_NUM_THREAD)
  val TAG_VEC_X = 0.U(NUM_THREADS.W)

  // Data
  def W_DATA = 64.W
  def DATA_T = UInt(W_DATA)
  val DATA_X = 0.U(W_DATA)

  def W_INST = 32.W
  def INST_T = UInt(W_INST)

  // Regs
  def REG_N = 32
  def W_REG = 5.W
  def REG_T = UInt(W_REG)
  val REG_X = 0.U(W_REG)

}

