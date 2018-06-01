// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.{MuxLookup, log2Ceil}

import common.DECODE_CONTROL_SIGNALS._
import common.PROCESSOR_TYPES._
import common.INSTRUCTIONS._

class EInst extends Bundle {
  val res  = DATA_T
  val rd   = REG_T
  val tag  = TAG_T
}

class BasicALU extends Module {
  val io = IO(new Bundle {
                val a = Input(DATA_T)
                val b = Input(DATA_T)
                val opcode = Input(OP_T)
                val res = Output(UInt(DATA_W))
                val nzcv = Output(NZCV_T)
              })

  val res =
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

  /*
   * NZCV flags
   */
  val nzcv = VecInit(NZCV_X.toBools)
  nzcv(0) := res.asSInt < 0.S
  nzcv(1) := res === 0.U
  nzcv(2) := (io.a +& io.b)(DATA_W.get) === 1.U // Unsigned carry
  // Sign carry (overflow)
  val sign_sum = io.a.asSInt + io.b.asSInt
  val isPos = io.a.asSInt > 0.S & io.b.asSInt > 0.S
  val isNeg = io.a.asSInt < 0.S & io.b.asSInt < 0.S
  when(isPos) {
    nzcv(3) := !(sign_sum > 0.S)
  }.elsewhen(isNeg) {
    nzcv(3) := !(sign_sum < 0.S)
  }.otherwise {
    nzcv(3) := false.B
  }

  io.nzcv := nzcv.asUInt

  io.res := res
}

class ShiftALU extends Module {
  val io = IO(new Bundle {
                val word = Input(DATA_T)
                val amount = Input(UInt(log2Ceil(DATA_W.get).W))
                val opcode = Input(SHIFT_T)
                val res = Output(DATA_T)
                val carry = Output(C_T)
              })

  /** Taken from Rocket chip arbiter
    */
  def rotateRight[T <: Data](norm: Vec[T], rot: UInt): Vec[T] = {
    val n = norm.size
    VecInit.tabulate(n) { i =>
      Mux(rot < (n - i).U, norm(rot - (n - i).U), norm(i.U + rot))
    }
  }

  val res =
    MuxLookup(io.opcode, io.word,
              Array(
                LSL -> (io.word << io.amount),
                LSR -> (io.word >> io.amount),
                ASR -> (io.word.asSInt() >> io.amount).asUInt(),
                ROR -> rotateRight(VecInit(io.word.toBools), io.amount).asUInt
              ))

  io.carry := res(DATA_W.get).toBool()
  io.res := res
}

class ExecuteUnitIO extends Bundle
{
  val dinst = Input(new DInst)
  val rVal1 = Input(DATA_T)
  val rVal2 = Input(DATA_T)

  val einst = Output(new EInst)
  val valid = Output(Bool())
  val nzcv  = Output(NZCV_T)
  val nzcv_en = Output(Bool())
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
  einst.tag := io.dinst.tag

  // Output
  io.einst := einst
  io.valid :=
    MuxLookup(io.dinst.itype, false.B,
              Array(
                I_LogSR -> true.B
              ))

  io.nzcv := basicALU.io.nzcv
  io.nzcv_en := io.dinst.nzcv_en
}
