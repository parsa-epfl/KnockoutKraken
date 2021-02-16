// See LICENSE.txt for license details.
package armflex

import chisel3._
import chisel3.util._

import arm.DECODE_CONTROL_SIGNALS._
import arm.PROCESSOR_TYPES._
import util.MACC

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
    val nzcv = Output(NZCV_T)
    val is32bit = Input(Bool())
  })

  // Already negated on instantiator
  val res = MuxLookup(io.opcode, 0.U, Array(
                OP_AND -> (io.a  &  io.b),
                OP_BIC -> (io.a  &  io.b),
                OP_ORR -> (io.a  |  io.b),
                OP_ORN -> (io.a  |  io.b),
                OP_EOR -> (io.a  ^  io.b),
                OP_EON -> (io.a  ^  io.b)
              ))

  io.res := res
  io.nzcv := Cat(Mux(io.is32bit, res(31), res(63)), (res === 0.U).asUInt, 0.U(2.W))
}

class ConditionHolds(implicit val cfg: ProcConfig) extends Module
{
  val io = IO( new Bundle {
                val cond = Input(COND_T)
                val nzcv = Input(NZCV_T)
                val res  = Output(Bool())
              })
  val result = WireInit(false.B)
  val PSTATE_N = io.nzcv(3)
  val PSTATE_Z = io.nzcv(2)
  val PSTATE_C = io.nzcv(1)
  val PSTATE_V = io.nzcv(0)
  /* */when (io.cond(3,1) === "b000".U) {result := (PSTATE_Z === 1.U);}                          // EQ or NE
  .elsewhen (io.cond(3,1) === "b001".U) {result := (PSTATE_C === 1.U);}                          // CS or CC
  .elsewhen (io.cond(3,1) === "b010".U) {result := (PSTATE_N === 1.U);}                          // MI or PL
  .elsewhen (io.cond(3,1) === "b011".U) {result := (PSTATE_V === 1.U);}                          // VS or VC
  .elsewhen (io.cond(3,1) === "b100".U) {result := (PSTATE_C === 1.U && PSTATE_Z === 0.U);}      // HI or LS
  .elsewhen (io.cond(3,1) === "b101".U) {result := (PSTATE_N === PSTATE_V);}                     // GE or LT
  .elsewhen (io.cond(3,1) === "b110".U) {result := (PSTATE_N === PSTATE_V && PSTATE_Z === 0.U);} // GT or LE
  .elsewhen (io.cond(3,1) === "b111".U) {result := true.B}                                       // AL

  when(io.cond(0) === 1.U && io.cond =/= "b1111".U) {
    io.res := !result
  }.otherwise {
    io.res := result
  }
}

class AddWithCarry(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new Bundle {
                val is32bit = Input(Bool())
                val a = Input(DATA_T)
                val b = Input(DATA_T)
                val carry = Input(UInt(1.W))
                val res = Output(DATA_T)
                val nzcv = Output(NZCV_T)
              })

  val unsigned_sum = WireInit((io.a +& io.b) + io.carry)
  val signed_sum   = WireInit((io.a.asSInt +& io.b.asSInt).asUInt + io.carry)
  val res = WireInit(unsigned_sum(DATA_SZ-1, 0))

  // NZCV flags
  val nzcv = VecInit(NZCV_X.asBools)
  val n = WireInit(res(DATA_SZ-1).asBool)
  val z = WireInit(res === 0.U)
  val c = WireInit(res =/= unsigned_sum)
  val v = WireInit(res.asSInt =/= signed_sum.asSInt)
  nzcv(3) := n
  nzcv(2) := z
  nzcv(1) := c
  nzcv(0) := v

  io.res := res
  io.nzcv := nzcv.asUInt

  when(io.is32bit) {
    unsigned_sum := (io.a(31,0) +& io.b(31,0)) + io.carry
    res          := unsigned_sum(32-1, 0)
    signed_sum   := (io.a(31,0).asSInt +& io.b(31,0).asSInt).asUInt + io.carry
    n := res(32-1).asBool
    z := res(31,0) === 0.U
    c := res(31,0) =/= unsigned_sum(32,0)
    v := res(31,0).asSInt =/= signed_sum(32,0).asSInt
  }
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

  def HighestBitSet(bits: Bits): UInt = {
    val seq: Seq[Bool] = bits.asBools
    // if bits === 0.U then return -1
    PriorityMux(seq.reverse, (seq.size-1 to 0 by -1).map(_.asUInt))
  }

  def CountifyLeadingSignBits(bits: UInt, size: Int): UInt = bits(size-1, 1) ^ bits(size-2, 0)
  def CountLeadingZeroBits(bits: UInt, size: Int): UInt = (size-1).U - HighestBitSet(bits)
  def CountLeadingSignBits(bits: UInt, size: Int): UInt =
    CountLeadingZeroBits(CountifyLeadingSignBits(bits,size), size)

  def getByte(bits: UInt, idx: Int): UInt = bits((idx+1)*8-1, idx*8)
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
                //val aluVal1 = Output(DATA_T)
                //val aluVal2 = Output(DATA_T)
                //val aluOp = Output(OP_T)
                val res = Output(DATA_T)
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

  //io.aluVal1 := aluVal1
  //io.aluVal2 := aluVal2
  //io.aluOp := OP_ORR
  io.res := aluVal1 | aluVal2
}

// Move (wide immediate)
class Move(implicit val cfg: ProcConfig) extends Module {
  val io = IO(new Bundle {
                val op = Input(OP_T)
                val hw = Input(UInt(2.W))
                val imm = Input(UInt(16.W))
                val rd = Input(DATA_T)
                val res = Output(DATA_T)
              })

  val pos = Cat(io.hw, 0.U(4.W))
  val result = WireInit(DATA_X)

  result := MuxLookup(io.op, 0.U, Array(
                        OP_MOVN -> 0.U,
                        OP_MOVZ -> 0.U,
                        OP_MOVK -> io.rd
                      ))

  //result(pos+15.U, pos) := io.imm
  val vecResult = VecInit(result.asBools)
  val vecBools = VecInit(io.imm.asBools)

  for(i <- 0 to DATA_SZ-16) {
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
    val is32bit = Input(Bool())
  })

  val res = MuxLookup(io.opcode, io.word, Array(
    LSL -> (io.word << io.amount),
    LSR -> (io.word >> io.amount),
    ASR -> (io.word.asSInt() >> io.amount).asUInt(),
    ROR -> ALU.rotateRight(VecInit(io.word.asBools), io.amount).asUInt
  ))

  val word32 = io.word(31,0)
  val amount32 = io.amount(4,0)
  val res32 = MuxLookup(io.opcode, word32, Array(
    LSL -> (word32 << amount32),
    LSR -> (word32 >> amount32),
    ASR -> (word32.asSInt() >> amount32).asUInt(),
    ROR -> ALU.rotateRight(VecInit(word32.asBools), amount32).asUInt
  ))

  io.carry := Mux(io.is32bit, res(32).asBool, res(64).asBool)
  io.res := Mux(io.is32bit, res32, res)
}

// aarch64/instrs/integer/bitmasks/DecodeBitMasks
// (bits(M), bits(M)) DecodeBitMasks(bit immN, bits(6) imms, bits(6) immr, boolean immediate)
// M = 64, immN = 1
class DecodeBitMasks(implicit val cfg: ProcConfig) extends Module
{
  // 64 bits -> immn = 1
  val io = IO(new Bundle {
    val immn = Input(UInt(1.W))
    val imms = Input(UInt(6.W))
    val immr = Input(UInt(6.W))
    val wmask = Output(DATA_T)
    val tmask = Output(DATA_T) // NOTE: Not needed for Logical (immediate)
    val is32bit = Input(Bool())
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

  val OnesTable = VecInit.tabulate(DATA_SZ + 1) { i => BigInt("0"*(DATA_SZ-i) ++ "1"*i, 2).U }

  val welem = Wire(DATA_T)
  val wmask = Wire(DATA_T)
  val telem = Wire(DATA_T)
  val tmask = Wire(DATA_T)

  val toBeEncoded = WireInit(UInt(7.W), Cat(io.immn, ~io.imms))
  val len = WireInit(ALU.HighestBitSet(toBeEncoded))
  val levels = WireInit(OnesTable(len)(5,0))

  val s = WireInit(io.imms & levels)
  val r = WireInit(io.immr & levels)
  val diff = WireInit(s -& r)

  val d = WireInit(diff & levels)
  val onesS = WireInit(s +& 1.U)
  val onesD = WireInit(d +& 1.U)

  welem := OnesTable(onesS)
  wmask := ALU.rotateRight(VecInit(welem.asBools), r).asUInt // ROR(welem, R) and truncate esize

  telem := OnesTable(onesD)
  tmask := telem

  io.wmask := wmask
  io.tmask := tmask

  when(io.is32bit) {
    wmask := ALU.rotateRight(VecInit(welem(31,0).asBools), r(4,0)).asUInt.pad(64)
  }
}

class DataProcessing(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new Bundle {
    val a = Input(DATA_T)
    val b = Input(DATA_T)
    val op = Input(OP_T)
    val is32bit = Input(Bool())
    val res = Output(DATA_T)
  })

  val operand = io.a
  val res = Wire(DATA_T)

  val countLeadingBits = Wire(DATA_T)
  countLeadingBits :=
    ALU.CountLeadingZeroBits(Mux(io.op === OP_CLS, ALU.CountifyLeadingSignBits(operand, 64), operand), 64)


  val rev = WireInit(DATA_X)

  def Catify(bits: UInt, size:Int, containers:Int): UInt = {
    if(containers != 1) {
      Cat(Catify(bits(size-1, size/2), size/2, containers/2),
          Catify(bits(size/2-1, 0),    size/2, containers/2))
    } else {
      Cat(for(idx <- size/8-1 to 0 by -1) yield (ALU.getByte(bits, idx)))
    }
  }

  rev := MuxLookup(io.op, operand, Array(
    OP_REV16 -> Catify(operand, 64, 4),
    OP_REV32 -> Catify(operand, 64, 2),
    OP_REV   -> Catify(operand, 64, 1)
    //OP_REV16 -> Cat(
    //  Cat(operand(0,0), operand(0,0)),
    //  Cat(operand(0,0), operand(0,0)),
    //  Cat(operand(0,0), operand(0,0)),
    //  Cat(operand(0,0), operand(0,0))),
    //OP_REV32 -> Cat(
    //  Cat(operand(0,0), operand(0,0), operand(0,0), operand(0,0)),
    //  Cat(operand(0,0), operand(0,0), operand(0,0), operand(0,0))),
    //OP_REV   -> Cat(
    //  operand(0,0), operand(0,0), operand(0,0), operand(0,0),
    //  operand(0,0), operand(0,0), operand(0,0), operand(0,0))
  ))

  res := MuxLookup(io.op, io.a, Array(
    OP_CLS -> countLeadingBits,
    OP_CLZ -> countLeadingBits,
    OP_REV -> rev,
    OP_REV32 -> rev,
    OP_REV16 -> rev
  ))

  io.res := res

  when(io.is32bit) {
    countLeadingBits :=
    ALU.CountLeadingZeroBits(Mux(io.op === OP_CLS,
      ALU.CountifyLeadingSignBits(operand(31,0), 32), operand(31,0)), 32)
    rev := MuxLookup(io.op, operand, Array(
      OP_REV16 -> Catify(operand, 32, 2),
      OP_REV32 -> Catify(operand, 32, 1)))
  }
}

class DataProc3S(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new Bundle {
    val op = Input(OP_T)
    val rVal1 = Input(DATA_T)
    val rVal2 = Input(DATA_T)
    val rVal3 = Input(DATA_T)
    val res = Output(DATA_T)
    val is32bit = Input(Bool())
  })

  // NOTE MACC does Signed operations, but because we only support 32 bit it'll be treated as unsigned
  val macc32 = Module(new MACC(64, true))
  macc32.io.mult1 := Cat(0.U(32.W), io.rVal1(31,0)).asSInt
  macc32.io.mult2 := Cat(0.U(32.W), io.rVal2(31,0)).asSInt
  macc32.io.add := Cat(0.U(32.W), io.rVal3(31,0)).asSInt
  io.res := Mux(io.is32bit, macc32.io.res.asUInt, 0.U) // 64 bit not supported yet
}

class ExecuteUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new Bundle {
    val dinst = Input(new DInst)
    val rVal1 = Input(DATA_T)
    val rVal2 = Input(DATA_T)
    val rVal3 = Input(DATA_T)
    val nzcv = Input(NZCV_T)

    val condRes = Output(Bool())

    val einst = Output(Valid(new EInst))
})

  val rVal1 = WireInit(Mux(io.dinst.rs1 === 31.U, 0.U, io.rVal1))
  val rVal2 = WireInit(Mux(io.dinst.rs2 === 31.U, 0.U, io.rVal2))
  val rVal3 = WireInit(Mux(io.dinst.imm(4,0) === 31.U, 0.U, io.rVal3))
  // R[31] can be SP or Zero depending on instructions
  when(io.dinst.rs1 === 31.U) {
    rVal1 := MuxLookup(io.dinst.itype, 0.U, Array(
      I_ASImm -> io.rVal1,
      I_ASER -> io.rVal1
    ))
  }

  // Operations
  // Sign extend 
  val extendReg = Module(new ExtendReg)
  extendReg.io.value := io.rVal2
  extendReg.io.option := io.dinst.shift_val.bits(5,3)
  extendReg.io.shift := 0.U // No shift here

  // Shift
  val shiftALU = Module(new ShiftALU())
  shiftALU.io.word :=
    MuxLookup(io.dinst.itype, rVal2, Array(
                I_ASSR  -> rVal2,
                I_ASER  -> extendReg.io.res,
                I_PCRel -> io.dinst.imm, // LSL 12
                I_ASImm -> io.dinst.imm,
                I_CCImm -> io.dinst.imm, // PASSTHROUGH
                I_CCReg -> rVal2, // PASSTHROUGH
                I_CSel  -> rVal2, // PASSTHROUGH
                I_LogSR -> rVal2,
                I_DP2S  -> rVal1,
                I_LogI  -> rVal1, // PASSTHROUGH
                I_BitF  -> rVal1 // ROR(X(Rn), immr)
              ))
  // when !shift_val.valid => PASSTHROUGH
  shiftALU.io.amount := Mux(io.dinst.shift_val.valid,
    MuxLookup(io.dinst.itype, io.dinst.shift_val.bits, Array(
      I_DP2S -> io.rVal2(5,0),
      I_ASER -> io.dinst.shift_val.bits(2,0)
    )), 0.U)
  shiftALU.io.opcode := io.dinst.shift_type
  shiftALU.io.is32bit := io.dinst.is32bit

  // CondUnit
  val condHolds = Module(new ConditionHolds)
  condHolds.io.cond := io.dinst.cond.bits
  condHolds.io.nzcv := io.nzcv
  io.condRes := condHolds.io.res

  // DecodeBitMasks
  // NOTE Logical (immediate): N(22), immr(21:16), imms(15:10)
  val decodeBitMask = Module(new DecodeBitMasks)
  decodeBitMask.io.immn := io.dinst.imm(22-10)
  decodeBitMask.io.immr := io.dinst.imm(21-10,16-10)
  decodeBitMask.io.imms := io.dinst.imm(15-10,10-10)
  decodeBitMask.io.is32bit := io.dinst.is32bit

  // Bitfield
  // NOTE Logical (immediate): immr(21:16), imms(15:10)
  val bitfield = Module(new BitfieldALU)
  bitfield.io.op := io.dinst.op
  bitfield.io.immr := io.dinst.imm(21-10,16-10)
  bitfield.io.imms := io.dinst.imm(15-10,10-10)
  bitfield.io.src := rVal1
  bitfield.io.dst := rVal2 // See decoder, binds Rd to rs2
  bitfield.io.wmask := decodeBitMask.io.wmask
  bitfield.io.tmask := decodeBitMask.io.tmask
  bitfield.io.rorSrcR := shiftALU.io.res

  // Move wide (immediate): hw(22,21), imm16(20,5)
  val move = Module(new Move)
  move.io.op := io.dinst.op
  move.io.hw := io.dinst.imm(22-5,21-5)
  move.io.imm := io.dinst.imm(20-5,5-5)
  move.io.rd := rVal2

  val aluVal1 = WireInit(MuxLookup(io.dinst.itype, rVal1, Array(
                            I_ASSR  -> rVal1,
                            I_ASER  -> rVal1,
                            I_ASImm -> rVal1,
                            I_CCImm -> rVal1,
                            I_CCReg -> rVal1,
                            I_CSel  -> rVal1,
                            I_LogI  -> rVal1,
                            I_LogSR -> rVal1,
                          )))
  val aluVal2 = WireInit(MuxLookup(io.dinst.itype, shiftALU.io.res, Array(
                            I_ASSR  -> shiftALU.io.res,
                            I_ASER  -> shiftALU.io.res,
                            I_ASImm -> shiftALU.io.res,
                            I_CCImm -> shiftALU.io.res, // PASSTHROUGH imm
                            I_CCReg -> shiftALU.io.res, // PASSTHROUGH rs2
                            I_CSel  -> shiftALU.io.res, // PASSTHROUGH rs2
                            I_LogSR -> shiftALU.io.res,
                            I_LogI  -> decodeBitMask.io.wmask,
                          )))
  val aluOp = WireInit(MuxLookup(io.dinst.itype, io.dinst.op, Array(
                          I_LogSR -> io.dinst.op,
                          I_LogI  -> io.dinst.op,
                        )))

  // Execute in ALU now that we have both inputs ready
  val logicALU = Module(new LogicALU())
  logicALU.io.a := aluVal1
  logicALU.io.b := Mux(io.dinst.op(0), ~aluVal2, aluVal2)
  logicALU.io.opcode := aluOp
  logicALU.io.is32bit := io.dinst.is32bit

  // Data-Processing
  val dataProcessing = Module(new DataProcessing)
  dataProcessing.io.a := rVal1
  dataProcessing.io.b := rVal2
  dataProcessing.io.op := io.dinst.op
  dataProcessing.io.is32bit := io.dinst.is32bit

  // Data-Processing 3
  val dataProc3S = Module(new DataProc3S)
  dataProc3S.io.op := io.dinst.op
  dataProc3S.io.rVal1 := rVal1
  dataProc3S.io.rVal2 := rVal2
  dataProc3S.io.rVal3 := rVal3
  dataProc3S.io.is32bit := io.dinst.is32bit

  val addWithCarry = Module(new AddWithCarry)
  // I_ASSR || I_ASImm
  addWithCarry.io.is32bit := io.dinst.is32bit
  addWithCarry.io.a := aluVal1
  addWithCarry.io.b := Mux(io.dinst.op(0), ~aluVal2, aluVal2)
  addWithCarry.io.carry := Mux(io.dinst.op === OP_SUB, 1.U, 0.U) // OP_SUB === OP_CCMP
  when(io.dinst.itype === I_CSel) {
    val isNeg = (io.dinst.op === OP_CSINV || io.dinst.op === OP_CSNEG)
    val isCarry = (io.dinst.op === OP_CSINC || io.dinst.op === OP_CSNEG)
    addWithCarry.io.a := 0.U
    addWithCarry.io.b := Mux(isNeg, ~aluVal2, aluVal2)
    addWithCarry.io.carry := Mux(isCarry, 1.U, 0.U)
  }.elsewhen(io.dinst.itype === I_CCImm || io.dinst.itype === I_CCReg) {
    addWithCarry.io.a := aluVal1
    addWithCarry.io.b := Mux(io.dinst.op(0), ~aluVal2, aluVal2)
    addWithCarry.io.carry := Mux(io.dinst.op === OP_CCMP, 1.U, 0.U)
  }

  // Build executed instruction
  val einst = Wire(new EInst)
  val res = Wire(DATA_T)
  res := MuxLookup(io.dinst.itype, logicALU.io.res, Array(
    I_BitF  -> bitfield.io.res,
    I_LogSR -> logicALU.io.res,
    I_LogI  -> logicALU.io.res,
    I_DP1S  -> dataProcessing.io.res,
    I_DP2S  -> shiftALU.io.res,
    I_DP3S  -> dataProc3S.io.res,
    I_ASSR  -> addWithCarry.io.res,
    I_ASER  -> addWithCarry.io.res,
    I_ASImm -> addWithCarry.io.res,
    I_MovI  -> move.io.res,
    I_CSel  -> Mux(condHolds.io.res, rVal1, addWithCarry.io.res)
  ))
  einst.res := Mux(io.dinst.is32bit, Cat(0.U, res(31,0)), res)
  einst.rd := io.dinst.rd
  when(io.dinst.itype === I_LogSR || io.dinst.itype === I_LogI) {
    einst.rd.valid := io.dinst.rd.bits =/= 31.U
  }
  einst.nzcv.bits := MuxLookup(io.dinst.itype, addWithCarry.io.nzcv, Array(
                           I_LogSR -> logicALU.io.nzcv,
                           I_LogI  -> logicALU.io.nzcv,
                           I_CCImm -> Mux(condHolds.io.res, addWithCarry.io.nzcv, io.dinst.nzcv.bits),
                           I_CCReg -> Mux(condHolds.io.res, addWithCarry.io.nzcv, io.dinst.nzcv.bits)
                         ))
  einst.nzcv.valid := io.dinst.nzcv.valid

  io.einst.bits := einst
  io.einst.valid := MuxLookup(io.dinst.itype, false.B, Array(
    I_LogSR -> true.B,
    I_LogI  -> true.B,
    I_BitF  -> true.B,
    I_DP1S  -> true.B,
    I_DP2S  -> true.B,
    I_DP3S  -> true.B,
    I_CCImm -> true.B,
    I_CCReg -> true.B,
    I_ASImm -> true.B,
    I_ASSR  -> true.B,
    I_ASER  -> true.B,
    I_MovI  -> true.B,
    I_CSel  -> true.B
  ))
}
