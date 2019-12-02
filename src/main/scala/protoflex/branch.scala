// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.{MuxLookup, Valid, Cat}

import common.DECODE_CONTROL_SIGNALS._
import common.PROCESSOR_TYPES._

class BInst(implicit val cfg: ProcConfig) extends Bundle {
  val rd = Valid(REG_T)
  val res = Output(DATA_T)
  val pc = Output(DATA_T)
}

class BranchUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  val dinst = Input(new DInst)
  val rVal1 = Input(DATA_T)
  val rVal2 = Input(DATA_T)
  val cond = Input(Bool())
  val pc = Input(DATA_T)

  val binst = Output(Valid(new BInst))
}

class BranchUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new BranchUnitIO)

  val imm = WireInit(Cat(io.dinst.imm.bits, 0.U(2.W)))
  val offst = WireInit(SInt(64.W), imm.asSInt)

  val binst = Wire(new BInst)
  binst.rd.valid := io.dinst.itype === I_BImm && io.dinst.op === OP_BL
  binst.rd.bits := 30.U
  binst.res := io.pc + 4.U

  binst.pc := (io.pc.zext + offst).asUInt

  io.binst.bits := binst
  io.binst.valid :=
    MuxLookup(io.dinst.itype, false.B, Array(
                I_BImm -> true.B,
                I_BCImm -> io.cond,
                I_CBImm -> MuxLookup(io.dinst.op, false.B, Array(
                                       OP_CBZ  -> (io.rVal2 === 0.U),
                                       OP_CBNZ -> (io.rVal2 =/= 0.U)
                                     ))
              ))
}
