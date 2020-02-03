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
  val size = UInt(2.W)
  val wr32bit = Bool()
  val signExtend = Bool()
  val isLoad = Bool()
}

class MInst(implicit val cfg: ProcConfig) extends Bundle
{
  val rd_res = Output(DATA_T)
  val rd = Output(Valid(REG_T))
  val mem = Output(new MemReq)
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
                                     0.U -> value( 7,0).asSInt.pad(DATA_SZ),
                                     1.U -> value(15,0).asSInt.pad(DATA_SZ),
                                     2.U -> value(31,0).asSInt.pad(DATA_SZ),
                                     3.U -> value.asSInt
                                   )))
  val valUExt = WireInit(MuxLookup(size, value, Array(
                                     0.U -> value( 7,0).pad(DATA_SZ),
                                     1.U -> value(15,0).pad(DATA_SZ),
                                     2.U -> value(31,0).pad(DATA_SZ),
                                     3.U -> value
                                   )))
  val res = WireInit(Mux(isSigned, valSExt.asUInt, valUExt))
  io.res := res
}

class LDSTUnit(implicit val cfg: ProcConfig) extends Module
{

  val io = IO(new LDSTUnitIO)

  // Decode All variants
  val offst = WireInit(io.dinst.imm.bits)
  val size = WireInit(io.dinst.op(1,0))
  val isLoad = WireInit(io.dinst.op(2))
  val isSigned = WireInit(io.dinst.op(3))

  // Decode LSUImm variants
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
    offst := (io.dinst.imm.bits << size)
  }.elsewhen(io.dinst.itype === I_LSRReg) {
    offst := extendReg.io.res
  }

  //  if n == 31 then
  //     address = SP[]; // SP[] == X[31]
  //  else
  //     address = X[n];
  //
  val base_address = WireInit(DATA_X)
  when(io.dinst.rs1.bits === 31.U && (io.dinst.itype === I_LSUImm || io.dinst.itype === I_LSRReg)) {
    // CheckSPAAligment(); TODO
    base_address := io.rVal1
  }.otherwise {
    base_address := io.rVal1
  }

  val post_address = WireInit(base_address + offst)
  val ldst_address = WireInit(base_address)

  //  if !postindex then
  //     address = address + offset
  when(!postindex) {
    ldst_address := post_address
  }

  //  integer datasize = 8 << size
  //  bits(datasize) data

  // For WRITES
  // Only LSUImm {
  //   if rt_unknown
  //     data = bits(datasize) UNKNOWN
  //   else
  //     data = X[t]
  //   }
  //
  // Mem[address, datasize DIV 8, AccType_NORMAL] = data

  // For READS Everyone
  // data = Mem[address, datasize DIV 8, AccType_NORMAL]
  // X[t] = ZeroExtend(data, regsize)
  // bits(32) data = Mem[address, 4, AccType_NORMAL]             // I = LDRSW
  // X[t] = SignExtend(data, 64)                                 // I = LDRSW

  val data = WireInit(io.rVal2) // data = Rt = rVal2

  // Output
  io.minst.bits.mem.addr := ldst_address
  io.minst.bits.mem.data := data
  io.minst.bits.mem.reg := io.dinst.rd.bits
  io.minst.bits.mem.size := size
  io.minst.bits.mem.signExtend := isSigned
  io.minst.bits.mem.wr32bit := size =/= 3.U && (io.dinst.itype === I_LSUImm && io.dinst.op =/= OP_LDRSW)
  io.minst.bits.mem.isLoad := isLoad
  io.minst.valid := MuxLookup(io.dinst.itype, false.B, Array(
                                I_LSRReg -> true.B,
                                I_LSUImm -> true.B
                              ))


  // Writeback address to reg // itype === LSUImm
  // if wback then
  //   if n == 31 then
  //     SP[] = address;
  //   else
  //     X[n] = address;
  // NOTE:
  // - address = base_address + offst
  // - Check for n == 31 on proc.scala
  io.minst.bits.rd_res := ldst_address
  io.minst.bits.rd.bits := io.dinst.rs2.bits
  io.minst.bits.rd.valid := wback

  // Tag management
  val tag_checked = RegInit(false.B)
  when(io.dinst.itype === I_LSUImm) {
    tag_checked := wback || !(io.dinst.rs1.bits === 31.U)
  }

  // TODO
  // if HaveMTEExt() then
  //   SetNotTagCheckedInstruction(!tag_checked); // itype == LSUImm
  //   SetNotTagCheckedInstruction(FALSE);        // itype == LSRReg

  // TODO // itype == LSUImm
  // Checking for Transplant corner cases 
  // if wback && n == t && n != 31 then
  //     c = ConstrainUnpredictable();
  //     assert c IN {Constraint_NONE, Constraint_UNKNOWN, Constraint_UNDEF, Constraint_NOP};
  //     case c of
  //         when Constraint_NONE rt_unknown = FALSE; // value stored is original value
  //         when Constraint_UNKNOWN rt_unknown = TRUE; // value stored is UNKNOWN => Transplant (?)
  //         when Constraint_UNDEF UNDEFINED;        => Transplant
  //         when Constraint_NOP EndOfInstruction(); => NOP
  // NOTE:
  val rt_unkown = WireInit(false.B)

}
