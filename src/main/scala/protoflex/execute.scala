// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.MuxLookup

import common.DECODE_CONTROL_SIGNALS._
import common.PROCESSOR_TYPES._

class BasicALU extends Module {
  val io = IO(new Bundle {
    val a = Input(DATA_T)
    val b = Input(DATA_T)
    val opcode = Input(OP_ALU_T)
    val res = Output(DATA_T)
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
                OP_ADD -> (io.a  +  io.b),
                OP_SUB -> (io.a  -  io.b)
              ))
}

class ShiftALU extends Module {
  val io = IO(new Bundle {
    val word = Input(DATA_T)
    val amount = Input(UInt(6.W))
    val opcode = Input(SHIFT_T)
    val res = Output(DATA_T)
  })

  /** Taken from Rocket chip arbiter
    */
  def rotateRight[T <: Data](norm: Vec[T], rot: UInt): Vec[T] = {
    val n = norm.size
    VecInit.tabulate(n) { i =>
      Mux(rot < UInt(n - i), norm(rot - UInt(n - i)), norm(UInt(i) + rot))
    }
  }

  io.res :=
    MuxLookup(io.opcode, 0.U,
              Array(
                LSL -> (io.word << io.amount),
                LSR -> (io.word >> io.amount),
                ASR -> (io.word.asSInt() >> io.amount),
                ROR -> rotateRight(VecInit(io.word.toBools), io.amount).asUInt
              ))
}

class ExecuteUnitIO extends Bundle
{
  val dinst = Input(new DInst)
  val rVal1 = Input(UInt(64.W))
  val rVal2 = Input(UInt(64.W))

  val res = Output(UInt(64.W))
  val rd  = Output(UInt(5.W))
}

class ExecuteUnit extends Module
{
  val io = IO(new ExecuteUnitIO)


  // Take immediate as second argument if valid
  val interVal2 = io.rVal2
  when (io.dinst.imm_valid) {
    interVal2 := io.dinst.imm
  }
  val aluVal2 = DATA_T

  // Shift if valid
  val shiftALU = Module(new ShiftALU())
  shiftALU.io.word := interVal2
  shiftALU.io.amount := io.dinst.imm
  shiftALU.io.opcode := io.dinst.shift

  aluVal2 := interVal2
  when (io.dinst.shift_v) {
    aluVal2 := shiftALU.io.res
  }

  // Execute in ALU now that we have second input ready
  val basicALU = Module(new BasicALU())
  basicALU.io.a := io.rVal1
  basicALU.io.b := aluVal2
  basicALU.io.opcode := io.dinst.aluOp

  io.res := basicALU.io.res
  io.rd  := io.dinst.rd
}
