// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util._

import common.DECODE_CONTROL_SIGNALS._
import common.PROCESSOR_TYPES._

class EInst(implicit val cfg: ProcConfig) extends Bundle {
  val rd = Valid(REG_T)
  val nzcv = Valid(NZCV_T)

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
  val nzcv = VecInit(NZCV_X.asBools)
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

object ALU {
  /** Taken from Rocket chip arbiter
    */
  def rotateRight[T <: Data](norm: Vec[T], rot: UInt): Vec[T] = {
    val n = norm.size
    VecInit.tabulate(n) { i =>
      Mux(rot < (n - i).U, norm(rot - (n - i).U), norm(i.U + rot))
    }
  }

}

class ShiftALU(implicit val cfg: ProcConfig) extends Module {
  val io = IO(new Bundle {
                val word = Input(DATA_T)
                val amount = Input(SHIFT_VAL_T)
                val opcode = Input(SHIFT_TYPE_T)
                val res = Output(DATA_T)
                val carry = Output(C_T)
              })

  val res =
    MuxLookup(io.opcode, io.word,
              Array(
                LSL -> (io.word << io.amount),
                LSR -> (io.word >> io.amount),
                ASR -> (io.word.asSInt() >> io.amount).asUInt(),
                ROR -> ALU.rotateRight(VecInit(io.word.asBools), io.amount).asUInt
              ))

  io.carry := res(DATA_W.get).asBool()
  io.res := res
}

// aarch64/instrs/integer/bitmasks/DecodeBitMasks
// NOTE Only supports 64 bits operations
// (bits(M), bits(M)) DecodeBitMasks(bit immN, bits(6) imms, bits(6) immr, boolean immediate)
// M = 64, immN = 1, immediate = true
class DecodeBitMasks(implicit val cfg: ProcConfig) extends Module
{
  // 64 bits -> immn = 1
  val io = IO(new Bundle {
                val imms = Input(UInt(6.W))
                val immr = Input(UInt(6.W))
                val wmask = Output(DATA_T)
                val tmask = Output(DATA_T) // NOTE: Not needed for Logical (immediate)
                val undef = Output(Bool())
              })

  // The bit patterns we create here are 64 bit patterns which
  // are vectors of identical elements of size e = 2, 4, 8, 16, 32 or
  // 64 bits each. Each element contains the same value: a run
  // of between 1 and e-1 non-zero bits, rotated within the
  // element by between 0 and e-1 bits.
  //
  // The element size and run length are encoded into immn (1 bit)
  // and imms (6 bits) as follows:
  // 64 bit elements: immn = 1, imms = <length of run - 1>
  // 32 bit elements: immn = 0, imms = 0 : <length of run - 1>
  // 16 bit elements: immn = 0, imms = 10 : <length of run - 1>
  //  8 bit elements: immn = 0, imms = 110 : <length of run - 1>
  //  4 bit elements: immn = 0, imms = 1110 : <length of run - 1>
  //  2 bit elements: immn = 0, imms = 11110 : <length of run - 1>
  // Notice that immn = 0, imms = 11111x is the only combination
  // not covered by one of the above options; this is reserved.
  // Further, <length of run - 1> all-ones is a reserved pattern.
  //
  // In all cases the rotation is by immr % e (and immr is 6 bits).

  def BitMask(bits: UInt): UInt = {
    val ones = WireInit(DATA_X)
    for ( i <- 0 until DATA_SZ) {
      when(i.U === bits) {
        val value = WireInit(VecInit(DATA_X.asBools))
        for ( j <- 0 until i) value(j) := 1.U
        ones := value.asUInt
      }
    }
    ones
  }

  val len = 6
  val esize = 1 << len; // 64, because 64 bit instructions
  val levels = (esize - 1).U; // Mask for 64 bit: 0b111111

  val s = io.imms & levels;
  val r = io.immr & levels;
  val diff = s - r;

  val d = diff & levels;
  val welem = BitMask(s + 1.U)
  val wmask = ALU.rotateRight(VecInit(welem.asBools), r).asUInt // ROR(welem, R) and truncate esize
  io.wmask := wmask

  io.tmask := 0.U
  // Undefined cases
  // if immediate && (imms AND levels) == levels then UNDEFINED;
  val immsIsOnes = io.imms === levels
  io.undef := immsIsOnes
}

class ExecuteUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  val dinst = Input(new DInst)
  val rVal1 = Input(DATA_T)
  val rVal2 = Input(DATA_T)

  val einst = Output(Valid(new EInst))
  val undef = Output(Bool())
}

class ExecuteUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new ExecuteUnitIO)


  // Operations
  // Shift
  val shiftALU = Module(new ShiftALU())
  shiftALU.io.word := MuxLookup(io.dinst.itype, io.rVal2,
                                Array(
                                  I_ASSR  -> io.rVal2,
                                  I_LogSR -> io.rVal2,
                                  I_ASImm -> io.dinst.imm.bits,
                                ))
  shiftALU.io.amount := io.dinst.shift_val.bits
  shiftALU.io.opcode := io.dinst.shift_type

  // DecodeBitMasks
  // NOTE Logical (immediate): immr(21:16), imms(15:10)
  val decodeBitMask = Module(new DecodeBitMasks)
  decodeBitMask.io.imms := io.dinst.imm.bits(21,16)
  decodeBitMask.io.immr := io.dinst.imm.bits(15,10)

  val aluVal2 = MuxLookup(io.dinst.itype, shiftALU.io.res,
                          Array(
                            I_ASSR  -> shiftALU.io.res,
                            I_LogSR -> shiftALU.io.res,
                            I_ASImm -> shiftALU.io.res,
                            I_LogI -> decodeBitMask.io.wmask
                          ))

  // Execute in ALU now that we have both inputs ready
  val basicALU = Module(new BasicALU())
  basicALU.io.a := io.rVal1
  basicALU.io.b := aluVal2
  basicALU.io.opcode := io.dinst.op

  // Build executed instruction
  val einst = Wire(new EInst)
  einst.res := basicALU.io.res
  einst.rd  := io.dinst.rd
  einst.nzcv.bits := basicALU.io.nzcv
  einst.nzcv.valid := io.dinst.nzcv_en

  // Output
  io.einst.bits := einst

  // invalid for non-data processing instructions
  io.einst.valid :=
    MuxLookup(io.dinst.itype, false.B,
              Array(
                I_LogSR -> true.B,
                I_LogI  -> true.B,
                I_ASSR  -> true.B,
                I_ASImm -> true.B
              ))

  io.undef := decodeBitMask.io.undef
}
