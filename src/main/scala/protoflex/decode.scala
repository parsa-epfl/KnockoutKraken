package protoflex

import chisel3._
import chisel3.util.{BitPat, ListLookup, MuxLookup, Cat}

import common.DECODE_CONTROL_SIGNALS._
import common.DECODE_MATCHING_TABLES._
import common.INSTRUCTIONS._

class DInst extends Bundle
{
  // Data
  val rd    = REG_T
  val rs1   = REG_T
  val rs2   = REG_T
  val imm   = IMM_T
  val shift = SHIFT_T

  // Control
  val imm_valid = Bool()
  val aluOp = OP_ALU_T
  val shift_v = Bool()
  val valid = Bool()

  def decode(inst : UInt) = {
    val decoder = ListLookup(inst, decode_default, decode_table)

    // Data
    val dtype = decoder.head
    rd    := MuxLookup(dtype,   REG_X, Array( I_LogSR -> inst(4,0)   ))
    rs1   := MuxLookup(dtype,   REG_X, Array( I_LogSR -> inst(9,5)   ))
    rs2   := MuxLookup(dtype,   REG_X, Array( I_LogSR -> inst(20,16) ))
    imm   := MuxLookup(dtype,   IMM_X, Array( I_LogSR -> inst(15,10) ))
    shift := MuxLookup(dtype, SHIFT_X, Array( I_LogSR -> inst(23,22) ))

    // Control
    val cdecoder = decoder.tail
    val csignals = Seq(imm_valid, aluOp, shift_v, valid)
    csignals zip cdecoder map { case (s, d) => s:= d }

    this
  }
}
class DecodeUnitIO extends Bundle
{
  val inst = Input(UInt(32.W))
  val dinst = Output(new DInst)
}

class DecodeUnit extends Module
{
  val io = IO(new DecodeUnitIO)
  val dinst = Wire(new DInst).decode(io.inst)
  io.dinst := dinst
}
