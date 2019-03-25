package common

import chisel3._
import chisel3.util.{BitPat,log2Ceil}

/*
 * Bit Patterns for Instructions
 *
 */
object PROCESSOR_TYPES
{
  // Number of threads
  val NUM_THREADS = 4
  val NUM_THREAD_W = log2Ceil(NUM_THREADS).W // 4 Threads
  def TAG_T = UInt(NUM_THREAD_W)
  val TAG_X = 0.U(NUM_THREAD_W)
  val TAG_VEC_X = 0.U(NUM_THREADS.W)

  // Data
  val DATA_W = 64.W
  def DATA_T = UInt(DATA_W)
  val DATA_X = 0.U(DATA_W)

  val INST_W = 32.W
  def INST_T = UInt(INST_W)

  // Regs
  val REG_N = 32
  val REG_W = log2Ceil(REG_N).W
  def REG_T = UInt(REG_W)
  val REG_X = 0.U(REG_W)

  // Special Regs
  val NZCV_W = 4.W
  def NZCV_T = UInt(NZCV_W)
  val NZCV_X = 0.U(NZCV_W)

  // Virtual memory
  val VADDR = 48
  val PADDR = 32

  // TLB
  val PG_OFFSET = 12 // 4K page
  val TLB_ENTRIES = 256
  val TLB_SZ = log2Ceil(TLB_ENTRIES)
  val TLB_TAG =  VADDR - PG_OFFSET - TLB_SZ
  val PPN = PADDR - PG_OFFSET

}

