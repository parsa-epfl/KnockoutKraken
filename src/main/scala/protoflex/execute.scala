// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.{MuxLookup, log2Ceil}

import common.DECODE_CONTROL_SIGNALS._
import common.PROCESSOR_TYPES._

class EInst extends Bundle {
  val res  = DATA_T
  val rd   = REG_T
  val rd_v = C_T
  val tag  = TAG_T
}

class PStateFlags extends Module {
  val io = IO(new Bundle {
                val res = Input(UInt(DATA_W + 1.W))
                val nzcv = Output(NZCV_T)
              })
  io.nzcv(0) := (io.res.asSInt < 0.S)
  io.nzcv(1) := (io.res === 0.U)
  io.nzcv(2) := (io.res(64) === 1.U)
  io.nzcv(3) := (io.res(64) === 1.U)
}

class BasicALU extends Module {
  val io = IO(new Bundle {
                val a = Input(DATA_T)
                val b = Input(DATA_T)
                val opcode = Input(OP_T)
                val res = Output(UInt(DATA_W + 1.W))
              })

  io.res :=
    MuxLookup(io.opcode, 0.U,
              Array (
                OP_AND -> (io.a  &  io.b),
                OP_BIC -> (io.a  & ~io.b),
                OP_ORR -> (io.a  |  io.b),
                OP_ORN -> (io.a  | ~io.b),
                OP_EOR -> (io.a  ^  io.b),
                OP_EON -> (io.a  ^ ~io.b),
                OP_ADD -> (io.a  +& io.b),
                OP_SUB -> (io.a  -& io.b)
              ))
}

class ShiftALU extends Module {
  val io = IO(new Bundle {
                val word = Input(DATA_T)
                val amount = Input(UInt(log2Ceil(DATA_W.get).W))
                val opcode = Input(SHIFT_T)
                val res = Output(DATA_T)
              })

  /** Taken from Rocket chip arbiter
    */
  def rotateRight[T <: Data](norm: Vec[T], rot: UInt): Vec[T] = {
    val n = norm.size
    VecInit.tabulate(n) { i =>
      Mux(rot < (n - i).U, norm(rot - (n - i).U), norm(i.U + rot))
    }
  }

  io.res :=
    MuxLookup(io.opcode, io.word,
              Array(
                LSL -> (io.word << io.amount),
                LSR -> (io.word >> io.amount),
                ASR -> (io.word.asSInt() >> io.amount).asUInt(),
                ROR -> rotateRight(VecInit(io.word.toBools), io.amount).asUInt
              ))
}

class ExecuteUnitIO extends Bundle
{
  val dinst = Input(new DInst)
  val rVal1 = Input(DATA_T)
  val rVal2 = Input(DATA_T)
  val einst = Output(new EInst)
}

class ExecuteUnit extends Module
{
  val io = IO(new ExecuteUnitIO)

  // Take immediate
  val interVal2 = Mux(io.dinst.rs2_en, io.rVal2, io.dinst.imm)

  // Shift
  val shiftALU = Module(new ShiftALU())
  shiftALU.io.word := interVal2
  shiftALU.io.amount := io.dinst.imm
  shiftALU.io.opcode := io.dinst.shift

  // Choose shifted
  val aluVal2 = Mux(io.dinst.shift_en, shiftALU.io.res, interVal2)

  // Execute in ALU now that we have both inputs ready
  val basicALU = Module(new BasicALU())
  basicALU.io.a := io.rVal1
  basicALU.io.b := aluVal2
  basicALU.io.opcode := io.dinst.op

  // Build executed instrcution
  val einst = Wire(new EInst)
  einst.res := basicALU.io.res
  einst.rd  := io.dinst.rd
  einst.rd_v := true.B
  einst.tag := io.dinst.tag

  // Output
  io.einst := einst
}
