package protoflex

import chisel3._
import chisel3.util.BitPat
import chisel3.util.ListLookup

import INSTR_TYPES._
import OP_ALU._
import SHIFT_OP._
import CONSTANTS._
import INSTRUCTIONS._

class DInst extends Bundle
{
  val rd  = REG_X
  val rs1 = REG_X
  val rs2 = REG_X

  val imm = IMM_X
  val imm_valid = Bool(false)

  val aluOp = OP_ALU_X

  val shift = SHIFT_X
  val shift_v = Bool(false)

  val valid = Bool(false)

  def decode(inst: UInt) = {
    val dinst_sigs = 
  }
}

class DecodeUnitIo extends Bundle
{
  val inst = Input(UInt(32.W))
  val dinst = Output(new DInst)
}

class DecodeUnit extends Module
{
  val io = new DecodeUnitIo
}

abstract trait DecodeConstants
{
// scalastyle:off
  def decode_default: List[BitPat] =
    //
    //
    //   RD                  IMM
    //   |     RS1            |    ALUOP
    //   |      |     RS2     |      |           SHIFT VALID
    //   |      |      |      |      |  SHIFT OP     |
    //   |      |      |      |      |      |        |  INSTRUCTION
    //   |      |      |      |      |      |        |  VALID
    //   |      |      |      |      |      |        |  |
    //   |      |      |      |      |      |        |  |
    List(REG_X, REG_X, REG_X, IMM_X, ALU_X, SHIFT_X, N, N)
  val table: Array[(BitPat, List[BitPat])]
// scalastyle:on
}

object Decode extends DecodeConstants
{
  val table = Array(
    /* Logical (shifted register) 64-bit */
    AND  -> List(REG_X, REG_X, REG_X, IMM_X, OP_ALU_X, SHIFT_X, N, N),
    BIC  -> List(REG_X, REG_X, REG_X, IMM_X, OP_ALU_X, SHIFT_X, N, N),
    ORR  -> List(REG_X, REG_X, REG_X, IMM_X, OP_ALU_X, SHIFT_X, N, N),
    ORN  -> List(REG_X, REG_X, REG_X, IMM_X, OP_ALU_X, SHIFT_X, N, N),
    EOR  -> List(REG_X, REG_X, REG_X, IMM_X, OP_ALU_X, SHIFT_X, N, N),
    EON  -> List(REG_X, REG_X, REG_X, IMM_X, OP_ALU_X, SHIFT_X, N, N),
    ANDS -> List(REG_X, REG_X, REG_X, IMM_X, OP_ALU_X, SHIFT_X, N, N),
    BICS -> List(REG_X, REG_X, REG_X, IMM_X, OP_ALU_X, SHIFT_X, N, N),
    INST_X -> decode_default
  )
}


