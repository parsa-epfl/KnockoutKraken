// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.{Valid, MuxLookup, log2Ceil}

import common.DECODE_CONTROL_SIGNALS._
import common.PROCESSOR_TYPES._

class EInst(implicit val cfg: ProcConfig) extends Bundle {
  val rd = Valid(REG_T)
  val nzcv = Valid(NZCV_T)

  val tag = Output(cfg.TAG_T)
  val res = Output(DATA_T)
}

class BasicALU(implicit val cfg: ProcConfig) extends Module {
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
  // Unsigned carry
  nzcv(2) := (io.a +& io.b)(DATA_W.get) === 1.U
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

class ShiftALU(implicit val cfg: ProcConfig) extends Module {
  val io = IO(new Bundle {
                val word = Input(DATA_T)
                val amount = Input(SHIFT_VAL_T)
                val opcode = Input(SHIFT_TYPE_T)
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

class ExecuteUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  val dinst = Input(new DInst)
  val rVal1 = Input(DATA_T)
  val rVal2 = Input(DATA_T)

  val einst = Output(Valid(new EInst))
}

class ExecuteUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new ExecuteUnitIO)

  // Take immediate
  val interVal2 = Mux(io.dinst.rs2.valid, io.rVal2, io.dinst.imm.bits)

  // Shift
  val shiftALU = Module(new ShiftALU())
  shiftALU.io.word := interVal2
  shiftALU.io.amount := io.dinst.shift_val.bits
  shiftALU.io.opcode := io.dinst.shift_type

  // Choose shifted
  val aluVal2 = Mux(io.dinst.shift_val.valid, shiftALU.io.res, interVal2)

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
  einst.nzcv.bits := basicALU.io.nzcv
  einst.nzcv.valid := io.dinst.nzcv_en

  // Output
  io.einst.bits := einst
  // invalid for non-data processing instructions
  io.einst.valid :=
    MuxLookup(io.dinst.itype, false.B,
              Array(
                I_LogSR -> true.B,
                I_ASImm -> true.B
              ))

}
