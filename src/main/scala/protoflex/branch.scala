// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.{MuxLookup, Valid}

import common.DECODE_CONTROL_SIGNALS._
import common.PROCESSOR_TYPES._

class BInst(implicit val cfg: ProcConfig) extends Bundle {
  val offset = DATA_T
}

class BranchUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  val dinst = Input(new DInst)
  val cond = Input(Bool())

  val binst = Output(Valid(new BInst))
}

class BranchUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new BranchUnitIO)

  // Default IO
  io.binst.valid :=  false.B

  // Offset
  val signExtended = Wire(SInt(DATA_W))
  signExtended := io.dinst.imm.bits.asSInt
  io.binst.bits.offset := signExtended.asUInt

  // Condition to branch
  when (io.dinst.itype ===  I_BImm || io.dinst.itype === I_BCImm) {
    when(io.dinst.op === OP_BCOND) {
      io.binst.valid := io.cond
    }.elsewhen(io.dinst.op === OP_B) {
      io.binst.valid := true.B
    }
  }
}
