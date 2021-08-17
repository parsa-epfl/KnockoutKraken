package arm

import chisel3._
import chisel3.util.{BitPat,log2Ceil}

object PROCESSOR_TYPES
{
  // Data
  val DATA_SZ = 64
  val DATA_W = DATA_SZ.W
  def DATA_T = UInt(DATA_W)
  val DATA_X = 0.U(DATA_W)

  // Inst
  val INST_SZ = 32
  val INST_W = INST_SZ.W
  def INST_T = UInt(INST_W)
  val INST_X = 0.U(INST_W)

  // Regs
  val REG_N = 32
  val REG_SZ = log2Ceil(REG_N)
  val REG_W = REG_SZ.W
  def REG_T = UInt(REG_W)
  val REG_X = 0.U(REG_W)

  // Special Regs
  val SP_REG_N = 4
  val NZCV_SZ = 4
  val NZCV_W = NZCV_SZ.W
  def NZCV_T = UInt(NZCV_W)
  val NZCV_X = 0.U(NZCV_W)

  // Permissions types (See QEMU MMU)
  def PERMISSION_T = UInt(2.W)
  val DATA_LOAD  = 0
  val DATA_STORE = 1
  val INST_FETCH = 2
}

