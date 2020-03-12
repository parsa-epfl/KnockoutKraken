package common

import chisel3._
import chisel3.util.{BitPat,log2Ceil}

/*
 * Bit Patterns for Instructions
 *
 */
object PROCESSOR_TYPES
{
  // Data
  val DATA_SZ = 64
  val DATA_W = DATA_SZ.W
  def DATA_T = UInt(DATA_W)
  val DATA_X = 0.U(DATA_W)

  val INST_W = 32.W
  def INST_T = UInt(INST_W)
  val INST_X = 0.U(INST_W)

  // Regs
  val REG_N = 32
  val REG_W = log2Ceil(REG_N).W
  def REG_T = UInt(REG_W)
  val REG_X = 0.U(REG_W)

  // Special Regs
  val SP_REG_N = 4
  val NZCV_SZ = 4
  val NZCV_W = NZCV_SZ.W
  def NZCV_T = UInt(NZCV_W)
  val NZCV_X = 0.U(NZCV_W)

  // Virtual memory
  val PAGE_SZ = 12 // 4096
  val VADDR = 48
  val PADDR = 32

  // Miss type
  def MISS_T = UInt(2.W)
  val DATA_LOAD  = 0 // FA_QflexCmds.DATA_LOAD
  val DATA_STORE = 1 // FA_QflexCmds.DATA_STORE
  val INST_FETCH = 2 // FA_QflexCmds.INST_FETCH

  // TLB
  val PG_OFFSET = 12 // 4K page
  val TLB_ENTRIES = 256
  val TLB_SZ = log2Ceil(TLB_ENTRIES)
  val TLB_TAG =  VADDR - PG_OFFSET - TLB_SZ
  val PPN = PADDR - PG_OFFSET
}

