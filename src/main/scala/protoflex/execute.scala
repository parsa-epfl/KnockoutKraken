// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.MuxLookup

import common.DECODE_CONTROL_SIGNALS._
import common.PROCESSOR_TYPES._

class EInst extends Bundle {
  val rd_v = Bool()
  val rd = REG_T
  val res = DATA_T
  val tag = TAG_T
}

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
      Mux(rot < (n - i).U, norm(rot - (n - i).U), norm(i.U + rot))
    }
  }

  io.res :=
    MuxLookup(io.opcode, 0.U,
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
  basicALU.io.opcode := io.dinst.aluOp

  // Build executed instrcution
  val einst = Wire(new EInst)
  einst.res := basicALU.io.res
  einst.rd  := io.dinst.rd
  einst.tag := io.dinst.tag
  einst.rd_v := true.B

  // Output
  io.einst := einst
}
