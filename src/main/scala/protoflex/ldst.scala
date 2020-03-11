// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util._

import common.PROCESSOR_TYPES._
import common.DECODE_CONTROL_SIGNALS._

class MemReq(implicit val cfg: ProcConfig) extends Bundle
{
  val addr = DATA_T
  val data = DATA_T
  val reg = REG_T
}

class MInst(implicit val cfg: ProcConfig) extends Bundle
{

  val size = UInt(2.W)
  val isPair = Output(Bool())
  val isLoad = Bool()
  val is32bit = Bool()
  val isSigned= Bool()
  val memReq = Output(Vec(2, new MemReq))

  // With Write Back
  val rd_res = Output(DATA_T)
  val rd = Output(Valid(REG_T))
  val unalignedExcpSP = Output(Bool())
}

class LDSTUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  val dinst = Input(new DInst)
  val rVal1 = Input(DATA_T) // Rn
  val rVal2 = Input(DATA_T) // Rt in some cases
  val pstate = Input(new PStateRegs)

  val minst = Output(Valid(new MInst))
}

class ExtendReg(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new Bundle {
                val value = Input(DATA_T)
                val option = Input(UInt(3.W))
                val shift = Input(UInt(2.W))
                val res = Output(DATA_T)
              })
  val value = io.value
  val isSigned = io.option(2)
  val size = io.option(1,0) // NOTE: option(1) == 1
  val valSExt = WireInit(MuxLookup(size, value.asSInt, Array(
                                     0.U -> value( 7,0).asSInt.pad(64),
                                     1.U -> value(15,0).asSInt.pad(64),
                                     2.U -> value(31,0).asSInt.pad(64),
                                     3.U -> value.asSInt
                                   )))
  val valUExt = WireInit(MuxLookup(size, value, Array(
                                     0.U -> value( 7,0).pad(64),
                                     1.U -> value(15,0).pad(64),
                                     2.U -> value(31,0).pad(64),
                                     3.U -> value
                                   )))
  val res = WireInit(Mux(isSigned, valSExt.asUInt, valUExt))
  io.res := res << io.shift
}

class LDSTUnit(implicit val cfg: ProcConfig) extends Module
{

  val io = IO(new LDSTUnitIO)

  // Decode All variants
  val size = WireInit(io.dinst.op(1,0))
  val isLoad = WireInit(io.dinst.op(2))
  val isSigned = WireInit(io.dinst.op(3))
  val offstSignExt7 = io.dinst.imm(6,0).asSInt.pad(64).asUInt
  val offstSignExt9 = io.dinst.imm(8,0).asSInt.pad(64).asUInt
  val offst = WireInit(DATA_T, io.dinst.imm)

  // Decode for LD/ST with Post/Pre-index and Signed offset variants
  val wback = WireInit(false.B)
  val postindex = WireInit(false.B)

  // Decode LSRReg variants
  val option = io.dinst.shift_val.bits
  val shift = Mux(io.dinst.shift_val.valid, size, 0.U)
  val extendReg = Module(new ExtendReg)
  extendReg.io.value := io.rVal2
  extendReg.io.option := option
  extendReg.io.shift := shift

  when(io.dinst.itype === I_LSUImm) {
    wback := false.B
    postindex := false.B
    offst := io.dinst.imm(11,0) << size
  }.elsewhen(io.dinst.itype === I_LSUReg) {
    wback := false.B
    postindex := false.B
    offst := offstSignExt9
  }.elsewhen(io.dinst.itype === I_LSPReg) {
    wback := false.B
    postindex := false.B
    offst := offstSignExt7 << size
  }.elsewhen(io.dinst.itype === I_LSRReg) {
    wback := false.B
    postindex := false.B
    offst := extendReg.io.res
  }.elsewhen(io.dinst.itype === I_LSPoReg) {
    wback := true.B
    postindex := true.B
    offst := offstSignExt9
  }.elsewhen(io.dinst.itype === I_LSPrReg) {
    wback := true.B
    postindex := false.B
    offst := offstSignExt9
  }
  //  if n == 31 then
  //     address = SP[]; // SP[] == X[31]
  //  else
  //     address = X[n];
  //
  when(io.dinst.rs1 === 31.U) {
    // CheckSPAAligment(); TODO
  }
  val base_address = WireInit(io.rVal1)


  //  if !postindex then
  //     address = address + offset
  // ==> implicitly
  //  if postindex then
  //     address = address
  //  else
  //     address = address + offset
  val ldst_address = WireInit(base_address + offst)
  when(postindex) {
    ldst_address := base_address
  }

  // integer datasize = 8 << size
  // bits(datasize) data

  // For WRITES
  // if rt_unknown
  //   data = bits(datasize) UNKNOWN
  // else
  //   data = X[t]
  //
  // Mem[address, datasize DIV 8, AccType_NORMAL] = data

  // LD's
  // data = Mem[address, datasize DIV 8, AccType_NORMAL]
  // if signed then
  //   X[t] = SignExtend(data, regsize)
  // else
  //   X[t] = ZeroExtend(data, regsize)

  // ST's
  // data = X[t]
  // Mem[address, datasize DIV 8, AccType_NORMAL] = data

  val data = WireInit(io.rVal2) // data = Rt = rVal2

  // Output
  io.minst.bits.size := size
  io.minst.bits.isSigned := isSigned
  io.minst.bits.is32bit := size =/= SIZE64 && !(io.dinst.itype === I_LSUImm && io.dinst.op === OP_LDRSW)
  io.minst.bits.isLoad := isLoad
  io.minst.valid := MuxLookup(io.dinst.itype, false.B, Array(
    I_LSUReg -> true.B,
    I_LSPReg -> true.B,
    I_LSRReg -> true.B,
    I_LSPrReg -> true.B,
    I_LSPoReg -> true.B,
    I_LSUImm -> true.B
  ))

  io.minst.bits.memReq(0).addr := ldst_address
  io.minst.bits.memReq(0).data := data
  io.minst.bits.memReq(0).reg := io.dinst.rd.bits
  // For Pair LD/ST
  val dbytes = 1.U << size
  io.minst.bits.isPair := io.dinst.itype === I_LSPReg
  io.minst.bits.memReq(1).addr := ldst_address + dbytes
  io.minst.bits.memReq(1).data := DontCare // Read 3 ports from RFile in single cycle Rd;Rt;Rt2
  io.minst.bits.memReq(1).reg := io.dinst.rs2


  // if wback then
  //   if wb_unknown then
  //     address = bits(64) UNKNOWN;
  //   elsif postindex then
  //     address = address + offset; => base_address + offset
  //   if n == 31 then
  //     SP[] = address;
  //   else
  //     X[n] = address;
  val wback_addr = WireInit(base_address + offst)
  io.minst.bits.unalignedExcpSP := io.dinst.rs1 === 31.U && wback_addr(1,0) =/= 0.U
  io.minst.bits.rd_res := wback_addr
  io.minst.bits.rd.bits := io.dinst.rs1
  io.minst.bits.rd.valid := wback

  // TODO
  // boolean tag_checked = memop != MemOp_PREFETCH && (n != 31); // itype == LSUReg
  // boolean tag_checked = wback || (n != 31);                   // itype == LSUImm
  val tag_checked = WireInit(false.B)

  // if HaveMTEExt() then
  //   SetNotTagCheckedInstruction(!tag_checked); // itype == LSUImm
  //   SetNotTagCheckedInstruction(!tag_checked); // itype == LSUReg
  //   SetNotTagCheckedInstruction(FALSE);        // itype == LSRReg

  // NOTE itype == LSUImm
  // Checking for Transplant corner cases
  // if wback && n == t && n != 31 then
  //     c = ConstrainUnpredictable();
  //     assert c IN {Constraint_NONE, Constraint_UNKNOWN, Constraint_UNDEF, Constraint_NOP};
  //     case c of
  //         when Constraint_NONE rt_unknown = FALSE; // value stored is original value
  //         when Constraint_UNKNOWN rt_unknown = TRUE; // value stored is UNKNOWN => Transplant (?)
  //         when Constraint_UNDEF UNDEFINED;        => Transplant
  //         when Constraint_NOP EndOfInstruction(); => NOP

  // NOTE itype == LSPReg
  // if t == t2 then
  //   Constraint c = ConstrainUnpredictable();
  //   assert c IN {Constraint_UNKNOWN, Constraint_UNDEF, Constraint_NOP};
  //   case c of
  //     when Constraint_UNKNOWN rt_unknown = TRUE; // result is UNKNOWN
  //     when Constraint_UNDEF UNDEFINED;
  //     when Constraint_NOP EndOfInstruction();
  val rt_unkown = WireInit(false.B)
}

class DataAlignByte(implicit val cfg: ProcConfig) extends Module {
  val io = IO(new Bundle {
    val currReq = Input(UInt(1.W))
    val minst = Input(new MInst)
    val data = Input(DATA_T)
    val aligned = Output(DATA_T)
    val byteEn = Output(UInt(8.W)) // Byte enable for Memory Store

    val unalignedExcp = Output(Bool())
  })

  val data = WireInit(io.data)
  val minst = WireInit(io.minst)
  val addr = io.minst.memReq(io.currReq).addr
  val dataBytes = VecInit.tabulate(8) { i => data((i+1)*8-1, i*8) }
  // Seq(
  // data(63,56), data(55,48), data(47,40), data(39,32),
  // data(21,24), data(23,16), data(15, 8), data( 7, 0)
  // ).reverse)

  val data2align = MuxLookup(minst.size, data, Array(
    SIZEB  -> dataBytes(addr(2,0)),
    SIZEH  -> Cat(dataBytes(addr(2,0) + 1.U), dataBytes(addr(2,0))),
    SIZE32 -> Mux(addr(2), data(63,32), data(31, 0)),
    SIZE64 -> data
  ))

  val alignedLoad = WireInit(data2align)
  Mux(minst.is32bit,
    MuxLookup(minst.size, data2align(31,0), Array(
      SIZEB -> Mux(minst.isSigned,
        data2align( 7, 0).asSInt.pad(32).asUInt,
        data2align( 7, 0).pad(32)),
      SIZEH -> Mux(minst.isSigned,
        data2align(15, 0).asSInt.pad(32).asUInt,
        data2align(15, 0).pad(32))
    )).pad(64),
    MuxLookup(minst.size, data2align, Array(
      SIZEB  -> Mux(minst.isSigned,
        data2align( 7, 0).asSInt.pad(64).asUInt,
        data2align( 7, 0).pad(64)),
      SIZEH  -> Mux(minst.isSigned,
        data2align(15, 0).asSInt.pad(64).asUInt,
        data2align(15, 0).pad(64)),
      SIZE32 -> Mux(minst.isSigned,
        data2align(31, 0).asSInt.pad(64).asUInt,
        data2align(31, 0).pad(64)),
      SIZE64 -> data2align
    ))
  )

  val alignedStore = WireInit(data << Cat(addr(2,0), 0.U(3.W)))


  // Byte enable for Stores
  val mask = WireInit(MuxLookup(minst.size, 255.U, Array(
    SIZEB  -> "b1".U,
    SIZEH  -> "b11".U,
    SIZE32 -> "b1111".U,
    SIZE64 -> "b11111111".U
  )))
  val byteEn = WireInit(mask << addr(2,0))

  // NOTE: Read Manual section B2.5 for alignment support -> Possible performance improvements
  val isAlignedMem_0 = WireInit(MuxLookup(minst.size, false.B, Array(
    SIZEB  -> false.B,
    SIZEH  -> (minst.memReq(0).addr(0) === 0.U),
    SIZE32 -> (minst.memReq(0).addr(1,0) === 0.U),
    SIZE64 -> (minst.memReq(0).addr(2,0) === 0.U)
  )))

  val isAlignedMem_1 = WireInit(MuxLookup(minst.size, false.B, Array(
    SIZEB  -> false.B,
    SIZEH  -> (minst.memReq(1).addr(0) === 0.U),
    SIZE32 -> (minst.memReq(1).addr(1,0) === 0.U),
    SIZE64 -> (minst.memReq(1).addr(2,0) === 0.U)
  )))


  io.byteEn := Mux(minst.isLoad, 0.U, byteEn)
  io.aligned := Mux(minst.isLoad, alignedLoad, alignedStore)

  io.unalignedExcp := !isAlignedMem_0 || (minst.isPair && !isAlignedMem_1)
}
