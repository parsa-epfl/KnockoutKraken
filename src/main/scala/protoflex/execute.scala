// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.MuxLookup

import common.DECODE_CONTROL_SIGNALS._

class BasicALU extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(64.W))
    val b = Input(UInt(64.W))
    val opcode = Input(OP_ALU_T)
    val res = Output(UInt(64.W))
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
    val word = Input(UInt(64.W))
    val amount = Input(UInt(6.W))
    val opcode = Input(SHIFT_T)
    val res = Output(UInt(64.W))
  })

  // TODO Could be optimized by taking the opcode(0) bit
  // to ensure a 2 way mux
  // need to check the Verilog compiled code
  // or surface area between N-way MuxLookup and Mux2
  val left_amount =
    MuxLookup(io.opcode, 0.U,
              Array(
                LSL -> io.amount,
                ROR -> (64.U - io.amount) // Warps around lsb's
              ))

  val left  = io.word << left_amount // Zero extend
  val right = io.word >> io.amount   // Zero extend

  val ASR_right = io.word.asSInt() >> io.amount // Sign extended with SInt

  io.res :=
    MuxLookup(io.opcode, 0.U,
              Array(
                LSL -> left,
                LSR -> right,
                ASR -> ASR_right,
                ROR -> (left | right)
              ))
}

class ExecutionUnitIO extends Bundle
{
  val dinst = Input(new DInst)
  val rVal1 = Input(UInt(64.W))
  val rVal2 = Input(UInt(64.W))

  val res = Output(UInt(64.W))
  val rd  = Output(UInt(5.W))
}

class ExecutionUnit extends Module
{
  val io = IO(new ExecutionUnitIO)


  // Take immediate as second argument if valid
  val interVal2 = io.rVal2
  when (io.dinst.imm_valid) {
    interVal2 := io.dinst.imm
  }
  val aluVal2 = UInt(64.W)

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
