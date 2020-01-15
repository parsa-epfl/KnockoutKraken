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
  val is_store = Bool()
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
  val rVal1 = Input(DATA_T)
  val rVal2 = Input(DATA_T) // Rt in some cases
  val pstate = Input(new PStateRegs)

  val minst = Output(Valid(new MInst))
}

class LDSTUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new LDSTUnitIO)

  val wback = Wire(Bool())
  val postindex = Wire(Bool())
  val offst = Wire(DATA_T)

  val size = Wire(UInt(2.W))
  size := MuxLookup(io.dinst.op, 0.U, Array(
                      OP_STRB  -> 0.U,
                      OP_STRH  -> 1.U,
                      OP_STR32 -> 2.U,
                      OP_STR64 -> 3.U,
                      OP_LDRB  -> 0.U,
                      OP_LDRH  -> 1.U,
                      OP_LDR32 -> 2.U,
                      OP_LDR64 -> 3.U
                    ))

  val datasize = WireInit(size << 8.U)

  wback := MuxLookup(io.dinst.op, false.B, Array(
                          OP_STRB  -> true.B,
                          OP_STRH  -> true.B,
                          OP_STR32 -> true.B,
                          OP_STR64 -> true.B
                        ))
  postindex := MuxLookup(io.dinst.itype, false.B, Array(
                           I_LSUImm -> false.B
                         ))

  offst := MuxLookup(io.dinst.itype, io.dinst.imm.bits, Array(
                       I_LSUImm -> (io.dinst.imm.bits << size),
                       ))



  val base_address = WireInit(DATA_X)
  when(io.dinst.rs1.bits === 31.U) {
    // CheckSPAAligment();
    base_address := io.pstate.SP
  }.otherwise {
    base_address := io.rVal1
  }

  val address = WireInit(base_address + offst)
  val ldst_address = WireInit(base_address)
  when(!postindex) {
    ldst_address := address
  }

  val data = WireInit(io.rVal2) // data = Rt

  val is_store = WireInit(OP_STRB  === io.dinst.op ||
                           OP_STRH  === io.dinst.op ||
                           OP_STR32 === io.dinst.op ||
                           OP_STR64 === io.dinst.op)

  val is_load = WireInit(OP_LDRB  === io.dinst.op ||
                           OP_LDRH  === io.dinst.op ||
                           OP_LDR32 === io.dinst.op ||
                           OP_LDR64 === io.dinst.op)

  // Output
  io.minst.bits.mem.addr := ldst_address
  io.minst.bits.mem.data := data
  io.minst.bits.mem.reg := io.dinst.rs1.bits
  io.minst.bits.mem.size := size
  io.minst.bits.mem.is_store := is_store

  io.minst.valid := MuxLookup(io.dinst.itype, false.B, Array(
                              I_LSUImm -> true.B
                            ))
  // Writeback address to reg
  // if wback then
  //   if n == 31 then
  //     SP[] = address;
  //   else
  //     X[n] = address;
  // NOTE:
  // - address = base_address + offst
  // - Check for n == 31 on proc.scala
  io.minst.bits.rd_res := address
  io.minst.bits.rd.bits := io.dinst.rs2.bits
  io.minst.bits.rd.valid := wback

  // Tag management
  val tag_checked = RegInit(false.B)
  when(io.dinst.itype === I_LSUImm){
    when(is_store) {
      tag_checked := true.B
    }.otherwise{ // TODO
      // if HaveMTEExt() then
      //     SetNotTagCheckedInstruction(!tag_checked);
    }
  }

  // Checking for Transplant corner cases
  // if wback && n == t && n != 31 then
  //     c = ConstrainUnpredictable();
  //     assert c IN {Constraint_NONE, Constraint_UNKNOWN, Constraint_UNDEF, Constraint_NOP};
  //     case c of
  //         when Constraint_NONE rt_unknown = FALSE; // value stored is original value
  //         when Constraint_UNKNOWN rt_unknown = TRUE; // value stored is UNKNOWN
  //         when Constraint_UNDEF UNDEFINED;        => Transplant
  //         when Constraint_NOP EndOfInstruction(); => NOP
  // NOTE:
  val rt_unkown = WireInit(false.B)
  when(rt_unkown) {

  }

}
