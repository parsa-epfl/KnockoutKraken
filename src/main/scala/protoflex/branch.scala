// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.{MuxLookup, Valid}

import common.DECODE_CONTROL_SIGNALS._
import common.PROCESSOR_TYPES._

class BInst(implicit val cfg: ProcConfig) extends Bundle {
  val offset = DATA_T
  val tag = cfg.TAG_T
}

class CondUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO( new Bundle {
                val cond = Input(COND_T)
                val nzcv = Input(NZCV_T)
                val res  = Output(Bool())
              })
  val result = WireInit(false.B)
  /* */when (io.cond(3,1) === "b000".U) {result := (io.nzcv(2) === 1.U);}
  .elsewhen (io.cond(3,1) === "b001".U) {result := (io.nzcv(1) === 1.U);}
  .elsewhen (io.cond(3,1) === "b010".U) {result := (io.nzcv(3) === 1.U);}
  .elsewhen (io.cond(3,1) === "b011".U) {result := (io.nzcv(0) === 1.U);}
  .elsewhen (io.cond(3,1) === "b100".U) {result := (io.nzcv(1) === 1.U);}
  .elsewhen (io.cond(3,1) === "b101".U) {result := (io.nzcv(3) === 1.U);}
  .elsewhen (io.cond(3,1) === "b110".U) {result := (io.nzcv(3) === 1.U);}
  .elsewhen (io.cond(3,1) === "b111".U) {result := true.B}

  when(io.cond(0) === 1.U && io.cond =/= "b1111".U) {
    io.res := !result
  }.otherwise {
    io.res := result
  }
}

class BranchUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  val dinst = Input(new DInst)
  val nzcv  = Input(NZCV_T)

  val binst = Output(Valid(new BInst))
}

class BranchUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new BranchUnitIO)

  // Default IO
  io.binst.valid :=  false.B
  io.binst.bits.tag := io.dinst.tag

  // Offset
  val signExtended = Wire(SInt(DATA_W))
  signExtended := io.dinst.imm.bits.asSInt
  io.binst.bits.offset := signExtended.asUInt

  // Condition to branch
  val cond = Module(new CondUnit())
  cond.io.cond := io.dinst.cond.bits
  cond.io.nzcv := io.nzcv
  when (io.dinst.itype ===  I_BImm || io.dinst.itype === I_BCImm) {
    when(io.dinst.op === OP_BCOND) {
      io.binst.valid := cond.io.res
    }.elsewhen(io.dinst.op === OP_B) {
      io.binst.valid := true.B
    }
  }
}
