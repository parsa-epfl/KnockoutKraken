package Protoflex

import chisel3._
import chisel3.util.BitPat
import chisel3.util.ListLookup

import constants.SIGNALS._
import common.INSTRUCTIONS._

class DInst extends Bundle
{
  // Data
  val rd  = REG_T
  val rs1 = REG_T
  val rs2 = REG_T
  val imm = IMM_T
  val shift = SHIFT_T

  // Control
  val imm_valid = Bool()
  val aluOp = OP_ALU_T
  val shift_v = Bool()
  val valid = Bool()

  def decode(inst : UInt) = {
    val cdecoder = ListLookup(inst, Decode.decode_control_default, Decode.table_control)
    val csignals = Seq(imm_valid, aluOp, shift_v, valid)
    csignals zip cdecoder map { case (s, d) => s:= d }

    val ddecoder = ListLookup(inst, Decode.decode_data_default, Decode.table_data)

    this
  }
}

object Decode
{
  def decode_control_default: List[UInt] =
    //
    //         ALU   INSTRUCTION
    //          OP     VALID
    //          |        |
    //  IMM     |   SHIFT|
    // VALID    |   VALID|
    //   |      |     |  |
    //   |      |     |  |
    List(N, OP_ALU_X, N, N)

  def table_control: Array[(BitPat, List[UInt])]  =
    Array(
      /* Logical (shifted register) 64-bit */
      AND  -> List(N, OP_AND, Y, Y),
      BIC  -> List(N, OP_BIC, Y, Y),
      ORR  -> List(N, OP_ORR, Y, Y),
      ORN  -> List(N, OP_ORN, Y, Y),
      EOR  -> List(N, OP_EOR, Y, Y),
      EON  -> List(N, OP_EON, Y, Y)
    )

  def decode_data_default: List[UInt] =
    //
    //    RD                  IMM
    //    |      RS1            |    SHIFT
    //    |       |     RS2     |      OP
    //    |       |      |      |      |
    //    |       |      |      |      |
    List(REG_X, REG_X, REG_X, IMM_X, SHIFT_X)
}

class DecodeUnitIo extends Bundle
{
  val inst = Input(UInt(32.W))
  val dinst = Output(new DInst)
}

class DecodeUnit extends Module
{
  val io = new DecodeUnitIo
  io.dinst := Wire(new DInst).decode(io.inst)
}

