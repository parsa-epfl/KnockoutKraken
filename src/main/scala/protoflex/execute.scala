// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util._

import common.DECODE_CONTROL_SIGNALS._
import common.PROCESSOR_TYPES._
import treadle.executable.MuxLongs

class EInst(implicit val cfg: ProcConfig) extends Bundle {
  val rd = Valid(REG_T)
  val nzcv = Valid(NZCV_T)

  val res = Output(DATA_T)
}

class LogicALU(implicit val cfg: ProcConfig) extends Module {
  val io = IO(new Bundle {
                val a = Input(DATA_T)
                val b = Input(DATA_T)
                val opcode = Input(OP_T)
                val res = Output(UInt(DATA_W))
              })

  val res =
    MuxLookup(io.opcode, 0.U,
              Array (
                OP_AND -> (io.a  &  io.b),
                OP_BIC -> (io.a  & ~io.b),
                OP_ORR -> (io.a  |  io.b),
                OP_ORN -> (io.a  | ~io.b),
                OP_EOR -> (io.a  ^  io.b),
                OP_EON -> (io.a  ^ ~io.b)
              ))

  io.res := res
}

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

class AddWithCarry(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new Bundle {
                val a = Input(DATA_T)
                val b = Input(DATA_T)
                val carry = Input(UInt(1.W))
                val res = Output(DATA_T)
                val nzcv = Output(NZCV_T)
              })

  val res = Wire(DATA_T)
  val carry = Wire(UInt(1.W))
  val unsigned_sum = WireInit(UInt((DATA_SZ + 1).W), io.a + io.b + io.carry)
  val signed_sum   = WireInit(UInt((DATA_SZ + 1).W), (io.a.asSInt + io.b.asSInt).asUInt + io.carry)

  res := unsigned_sum(DATA_SZ-1, 0)
  carry := unsigned_sum(DATA_SZ)

  // NZCV flags
  val nzcv = VecInit(NZCV_X.asBools)
  nzcv(0) := res(DATA_SZ-1).asBool
  nzcv(1) := res === 0.U
  // Unsigned carry
  nzcv(2) := res =/= unsigned_sum
  nzcv(3) := res =/= signed_sum
  // Sign carry (overflow)

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

class BitfieldALU(implicit val cfg: ProcConfig) extends Module {
  val io = IO(new Bundle {
                val op = Input(OP_T)
                val immr = Input(UInt(6.W))
                val imms = Input(UInt(6.W))
                val src = Input(DATA_T)
                val dst = Input(DATA_T)
                val wmask = Input(DATA_T)
                val tmask = Input(DATA_T)
                val rorSrcR = Input(DATA_T)
                val aluVal1 = Output(DATA_T)
                val aluVal2 = Output(DATA_T)
                val aluOp = Output(OP_T)
              })

  val bot = WireInit(DATA_X)
  val top = WireInit(DATA_X)
  val aluVal1 = WireInit(DATA_X)
  val aluVal2 = WireInit(DATA_X)

  when(io.op === OP_SBFM) {
    bot := io.rorSrcR & io.wmask
    top := Mux(io.src(io.imms), (-1).S(64.W).asUInt, 0.U)

    aluVal1 := top & ~io.tmask
    aluVal2 := bot & io.tmask
  }.elsewhen(io.op === OP_BFM) {
    bot := (io.dst & ~io.wmask) | (io.rorSrcR & io.wmask)
    top := DATA_X

    aluVal1 := (io.dst & ~io.tmask)
    aluVal2 := (bot & io.tmask)
  }.elsewhen(io.op === OP_UBFM) {
    bot := io.rorSrcR & io.wmask
    top := DATA_X

    aluVal1 := bot & io.tmask
    aluVal2 := bot & io.tmask
  }

  io.aluVal1 := aluVal1
  io.aluVal2 := aluVal2
  io.aluOp := OP_ORR
}

// Move (wide immediate)
class Move(implicit val cfg: ProcConfig) extends Module {
  val io = IO(new Bundle {
                val op = Input(UInt(OP_W))
                val hw = Input(UInt(2.W))
                val imm = Input(UInt(16.W))
                val rd = Input(DATA_T)
                val res = Output(DATA_T)
              })

  val pos = Cat(io.hw, 0.U(4.W))
  val result = WireInit(DATA_X)

  when(io.op === OP_MOVZ) {
    result := io.rd
  }
  //result(pos+15.U, pos) := io.imm

  val vecResult = VecInit(result.asBools)
  val vecBools = VecInit(io.imm.asBools)

  for(i <- 0 until DATA_SZ-16) {
    when(i.U === pos) {
      for(j <- 0 until 16) {
        vecResult(i + j) := vecBools(j)
      }
    }
  }

  val res = WireInit(vecResult.asUInt)
  io.res := Mux(io.op === OP_MOVN, ~res, res)
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
// M = 64, immN = 1
class DecodeBitMasks(implicit val cfg: ProcConfig) extends Module
{
  // 64 bits -> immn = 1
  val io = IO(new Bundle {
                val imms = Input(UInt(6.W))
                val immr = Input(UInt(6.W))
                val wmask = Output(DATA_T)
                val tmask = Output(DATA_T) // NOTE: Not needed for Logical (immediate)
                val immediate = Input(Bool())
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

  def Ones(bits: UInt): UInt = {
    val table  = VecInit.tabulate(DATA_SZ + 1) { i => BigInt("0"*(DATA_SZ-i) ++ "1"*i, 2).U }
    val res = Wire(DATA_T)
    res := table(bits)
    res
  }
  val welem = Wire(DATA_T)
  val wmask = Wire(DATA_T)
  val telem = Wire(DATA_T)
  val tmask = Wire(DATA_T)

  val len = 6
  val esize = 1 << len // 64, because 64 bit instructions
  val levels = (esize - 1).U // Mask for 64 bit: 0b111111

  val s = WireInit(io.imms & levels)
  val r = WireInit(io.immr & levels)
  val diff = WireInit(UInt(6.W), s - r)

  val d = WireInit(UInt(7.W), diff & levels)
  val onesS = WireInit(UInt(7.W), s + 1.U) // Add a bit for the 0b1000000 = 64 bit case
  val onesD = WireInit(UInt(7.W), d + 1.U) // Add a bit for the 0b1000000 = 64 bit case

  welem := WireInit(Ones(onesS))
  wmask := ALU.rotateRight(VecInit(welem.asBools), r).asUInt // ROR(welem, R) and truncate esize

  telem := WireInit(Ones(onesD))
  tmask := telem

  io.wmask := wmask
  io.tmask := tmask
}

class ExecuteUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  val dinst = Input(new DInst)
  val rVal1 = Input(DATA_T)
  val rVal2 = Input(DATA_T)
  val nzcv = Input(NZCV_T)

  val condRes = Output(Bool())
  val einst = Output(Valid(new EInst))
}

class ExecuteUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new ExecuteUnitIO)

  // Operations
  // Shift
  val shiftALU = Module(new ShiftALU())
  shiftALU.io.word :=
    MuxLookup(io.dinst.itype, io.rVal2, Array(
                I_ASSR  -> io.rVal2,
                I_ASImm -> io.dinst.imm.bits,
                I_CCImm -> io.dinst.imm.bits, // PASSTHROUGH
                I_CCReg -> io.rVal2, // PASSTHROUGH
                I_CSel  -> io.rVal2, // PASSTHROUGH
                I_LogSR -> io.rVal2,
                I_LogI  -> io.rVal1, // PASSTHROUGH
                I_BitF  -> io.rVal1 // ROR(X(Rn), immr)
              ))
  shiftALU.io.amount := io.dinst.shift_val.bits // when 0 => PASSTHROUGH
  shiftALU.io.opcode := io.dinst.shift_type

  // CondUnit
  val condHolds = Module(new ConditionHolds)
  condHolds.io.cond := io.dinst.cond.bits
  condHolds.io.nzcv := io.nzcv
  io.condRes := condHolds.io.res

  // DecodeBitMasks
  // NOTE Logical (immediate): immr(21:16), imms(15:10)
  val decodeBitMask = Module(new DecodeBitMasks)
  decodeBitMask.io.immr := io.dinst.imm.bits(21-10,16-10)
  decodeBitMask.io.imms := io.dinst.imm.bits(15-10,10-10)
  decodeBitMask.io.immediate := MuxLookup(io.dinst.itype, false.B, Array(
                                            I_LogI -> true.B,
                                            I_BitF -> false.B
                                          ))

  // Bitfield
  // NOTE Logical (immediate): immr(21:16), imms(15:10)
  val bitfield = Module(new BitfieldALU)
  bitfield.io.op := io.dinst.op
  bitfield.io.immr := io.dinst.imm.bits(21-10,16-10)
  bitfield.io.imms := io.dinst.imm.bits(15-10,10-10)
  bitfield.io.src := io.rVal1
  bitfield.io.dst := io.rVal2 // See decoder, binds Rd to rs2
  bitfield.io.wmask := decodeBitMask.io.wmask
  bitfield.io.tmask := decodeBitMask.io.tmask
  bitfield.io.rorSrcR := shiftALU.io.res

  // Move wide (immediate)
  val move = Module(new Move)
  move.io.op := io.dinst.op
  move.io.hw := io.dinst.imm.bits(17,16)
  move.io.imm := io.dinst.imm.bits(15,0)
  move.io.rd := io.rVal2

  val aluVal1 = MuxLookup(io.dinst.itype, io.rVal1, Array(
                            I_ASSR  -> io.rVal1,
                            I_ASImm -> io.rVal1,
                            I_CCImm -> io.rVal1,
                            I_CCReg -> io.rVal1,
                            I_CSel  -> io.rVal1,
                            I_LogI  -> io.rVal1,
                            I_LogSR -> io.rVal1,
                            I_BitF  -> bitfield.io.aluVal1
                          ))
  val aluVal2 = MuxLookup(io.dinst.itype, shiftALU.io.res, Array(
                            I_ASSR  -> shiftALU.io.res,
                            I_ASImm -> shiftALU.io.res,
                            I_CCImm -> shiftALU.io.res, // PASSTHROUGH imm
                            I_CCReg -> shiftALU.io.res, // PASSTHROUGH rs2
                            I_CSel  -> shiftALU.io.res, // PASSTHROUGH rs2
                            I_LogSR -> shiftALU.io.res,
                            I_LogI  -> decodeBitMask.io.wmask,
                            I_BitF  -> bitfield.io.aluVal2
                          ))
  val aluOp = MuxLookup(io.dinst.itype, io.dinst.op, Array(
                          I_LogSR -> io.dinst.op,
                          I_LogI  -> io.dinst.op,
                          I_BitF  -> bitfield.io.aluOp
                        ))

  // Execute in ALU now that we have both inputs ready
  val logicALU = Module(new LogicALU())
  logicALU.io.a := aluVal1
  logicALU.io.b := aluVal2
  logicALU.io.opcode := aluOp


  // NOTE: OP_SUB = 1 and OP_CCMP = 1 => Negative sum
  val addWithCarry = Module(new AddWithCarry)
  // I_ASSR || I_ASImm
  addWithCarry.io.a := aluVal1
  addWithCarry.io.b := Mux(io.dinst.op === OP_SUB, ~aluVal2, aluVal2)
  addWithCarry.io.carry := Mux(io.dinst.op === OP_SUB, 1.U, 0.U)
  when(io.dinst.itype === I_CSel) {
    addWithCarry.io.a := 0.U
    addWithCarry.io.b :=
      Mux(io.dinst.op === OP_CSINV || io.dinst.op === OP_CSNEG, ~io.rVal2, io.rVal1)
    addWithCarry.io.carry :=
      Mux(io.dinst.op === OP_CSINC || io.dinst.op === OP_CSNEG, 1.U, 0.U)
  }.elsewhen(io.dinst.itype === I_CCImm || io.dinst.itype === I_CCReg) {
    addWithCarry.io.a := aluVal1
    addWithCarry.io.b := Mux(io.dinst.op === OP_CCMP, ~aluVal2, aluVal2)
    addWithCarry.io.carry := Mux(io.dinst.op === OP_CCMP, 1.U, 0.U)
  }.elsewhen(io.dinst.itype === I_LogSR) { // Get nzcv flags from addWithCarry
    addWithCarry.io.a := 0.U
    addWithCarry.io.b := logicALU.io.res
    addWithCarry.io.carry := 0.U
  }

  // Build executed instruction
  val einst = Wire(new EInst)
  einst.res := MuxLookup(io.dinst.itype, logicALU.io.res, Array(
                 I_BitF  -> logicALU.io.res,
                 I_LogSR -> logicALU.io.res,
                 I_LogI  -> logicALU.io.res,
                 I_ASSR  -> addWithCarry.io.res,
                 I_ASImm -> addWithCarry.io.res,
                 I_MovI  -> move.io.res,
                 I_CSel  -> Mux(condHolds.io.res, io.rVal1, addWithCarry.io.res)
                 ))
  einst.rd := io.dinst.rd
  einst.nzcv.bits := MuxLookup(io.dinst.itype, addWithCarry.io.nzcv, Array(
                           I_LogSR -> addWithCarry.io.nzcv,
                           I_CCImm -> Mux(condHolds.io.res, addWithCarry.io.nzcv, io.dinst.nzcv.bits),
                           I_CCReg -> Mux(condHolds.io.res, addWithCarry.io.nzcv, io.dinst.nzcv.bits)
                         ))
  einst.nzcv.valid := io.dinst.nzcv.valid

  io.einst.bits := einst
  io.einst.valid :=
    MuxLookup(io.dinst.itype, false.B, Array(
                I_MovI  -> true.B,
                I_LogSR -> true.B,
                I_LogI  -> true.B,
                I_ASSR  -> true.B,
                I_ASImm -> true.B,
                I_BitF  -> true.B,
                I_CCImm -> true.B,
                I_CCReg -> true.B,
                I_CSel  -> true.B
              ))
}
