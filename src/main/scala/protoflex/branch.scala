// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.MuxLookup

import common.DECODE_CONTROL_SIGNALS._
import common.PROCESSOR_TYPES._

class CondUnit extends Module {
  val io = IO( new Bundle {
                val cond = Input(COND_T)
                val nzcv = Input(NZCV_T)
                val res  = Output(Bool())
              })
  val result = WireInit(false.B)
  when (io.nzcv(3,1) === "b000".U) {result := (io.nzcv(2) === "b1".U);}
  .elsewhen (io.nzcv(3,1) === "b001".U) {result := (io.nzcv(1) === "b1".U);}
  .elsewhen (io.nzcv(3,1) === "b010".U) {result := (io.nzcv(3) === "b1".U);}
  .elsewhen (io.nzcv(3,1) === "b011".U) {result := (io.nzcv(0) === "b1".U);}
  .elsewhen (io.nzcv(3,1) === "b100".U) {result := (io.nzcv(1) === "b1".U);}
  .elsewhen (io.nzcv(3,1) === "b101".U) {result := (io.nzcv(3) === "b1".U);}
  .elsewhen (io.nzcv(3,1) === "b110".U) {result := (io.nzcv(3) === "b1".U);}
  .elsewhen (io.nzcv(3,1) === "b111".U) {result := true.B}

  when(io.cond(0) === "b1".U && io.cond =/= "b1111".U) {
    io.res := !result
  }.otherwise {
    io.res := result
  }
}

class BranchUnitIO extends Bundle
{
  val dinst = Input(new DInst)
  val nzcv  = Input(NZCV_T)

  val offset = Output(DATA_T)
  val valid = Output(Bool())
}

class BranchUnit extends Module
{
  val io = IO(new BranchUnitIO)

  // Default IO
  io.valid := false.B

  // Condition to branch
  val cond = Module(new CondUnit())
  //cond.io.cond := io.dinst.cond
  cond.io.nzcv := io.nzcv

  val signExtended = io.dinst.imm.asSInt()
  when(io.dinst.op === OP_BCOND) {
    io.offset := signExtended.asUInt
    io.valid := cond.io.res
  }.otherwise {
    io.offset := 0.U
    io.valid := false.B
  }
}
