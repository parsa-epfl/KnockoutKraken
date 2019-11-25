package protoflex

import chisel3._
import chisel3.util.{Valid, MuxLookup}

import common.DECODE_CONTROL_SIGNALS._
import common.PROCESSOR_TYPES._

class ConditionHolds(implicit val cfg: ProcConfig) extends Module
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

class CInst(implicit val cfg: ProcConfig) extends Bundle {
  val rd = Valid(REG_T)
  val res = Output(DATA_T)
}

class CondUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  val dinst = Input(new DInst)
  val rVal1 = Input(DATA_T)
  val rVal2 = Input(DATA_T)
  val nzcv = Input(NZCV_T)
  val condRes = Output(Bool())
  val cinst = Output(Valid(new CInst))
}

class CondUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new CondUnitIO)

  val condHolds = Module(new ConditionHolds)
  condHolds.io.cond := io.dinst.cond.bits
  condHolds.io.nzcv := io.nzcv
  io.condRes := condHolds.io.res


  val res = Wire(DATA_T)
  when(condHolds.io.res) {
    res := io.rVal1
  }.otherwise {
    res := MuxLookup(io.dinst.op, io.rVal2, Array(
                       OP_CSEL  -> (io.rVal2),
                       OP_CSINC -> (io.rVal2 + 1.U),
                       OP_CSINV -> (~io.rVal2),
                       OP_CSNEG -> ((~io.rVal2) + 1.U)
                     ))
  }

  val cinst = Wire(new CInst)
  cinst.rd := io.dinst.rd
  cinst.res := res

  io.cinst.bits := cinst
  io.cinst.valid :=
    MuxLookup(io.dinst.itype, false.B, Array(
                I_CSel -> true.B
              ))

}

