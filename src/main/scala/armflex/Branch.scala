// See LICENSE.txt for license details.
package armflex

import chisel3._
import chisel3.util.{MuxLookup, Valid, Cat}

import arm.DECODE_CONTROL_SIGNALS._
import arm.PROCESSOR_TYPES._

class BInst extends Bundle {
  val pc = Output(DATA_T)
  val unalignedExcp = Output(Bool())
}

// Writeback to register based on PC value instruction
class PCRel extends Bundle {
  val rd = Output(REG_T)
  val res = Output(DATA_T)
}

class BranchUnitIO extends Bundle
{
  val dinst = Input(new DInst)
  val rVal1 = Input(DATA_T)
  val rVal2 = Input(DATA_T)
  val cond = Input(Bool())
  val pc = Input(DATA_T)

  val binst = Output(Valid(new BInst))
  val pcrel = Output(Valid(new PCRel))
}

class BranchUnit extends Module
{
  val io = IO(new BranchUnitIO)


  val immS2 = WireInit(Cat(io.dinst.imm, 0.U(2.W)))
  val imm14S2 = WireInit(immS2(2+13,0).asSInt.pad(64))
  val imm26S2 = WireInit(immS2(2+25,0).asSInt.pad(64))
  val imm19S2 = WireInit(immS2(2+18,0).asSInt.pad(64))
  val imm21S0 = WireInit(io.dinst.imm(20,0).asSInt.pad(64))
  val imm21S12 = WireInit(Cat(io.dinst.imm(20,0), 0.U(12.W)).asSInt.pad(64))
  val immSExt = MuxLookup(io.dinst.itype, imm21S0, Array(
                        I_BImm  -> imm26S2,
                        I_BCImm -> imm19S2,
                        I_CBImm -> imm19S2,
                        I_TBImm -> imm14S2,
                        I_PCRel -> Mux(io.dinst.op === OP_ADRP, imm21S12, imm21S0)
                ))

  val pc = WireInit(io.pc)
  when(io.dinst.itype === I_PCRel && io.dinst.op === OP_ADRP) {
    pc := Cat(io.pc(63,12), 0.U(12.W))
  }
  val pcadd = WireInit((pc.zext + immSExt).asUInt)

  // TBImm
  val bit_pos = WireInit(Cat(~io.dinst.is32bit.asUInt, io.dinst.imm(23-5,19-5)))

  val binst = Wire(new BInst)
  val bpc = Mux(io.dinst.itype === I_BReg, io.rVal2, pcadd)
  binst.pc := bpc
  binst.unalignedExcp := bpc(1,0) =/= 0.U

  io.binst.bits := binst
  io.binst.valid :=
    MuxLookup(io.dinst.itype, false.B, Array(
      I_BImm -> true.B,
      I_BReg -> true.B,
      I_BCImm -> io.cond,
      I_TBImm -> (io.rVal2(bit_pos) === io.dinst.op),
      I_CBImm -> MuxLookup(io.dinst.op, false.B, Array(
        OP_CBZ  -> Mux(io.dinst.is32bit, io.rVal2(31,0) === 0.U, io.rVal2 === 0.U),
        OP_CBNZ -> Mux(io.dinst.is32bit, io.rVal2(31,0) =/= 0.U, io.rVal2 =/= 0.U)
      ))
    ))

  val pcrel = Wire(new PCRel)
  pcrel.rd := io.dinst.rd.bits
  pcrel.res := pcadd
  io.pcrel.valid := false.B
  when(io.dinst.itype === I_BImm && io.dinst.op === OP_BL ||
    io.dinst.itype === I_BReg && io.dinst.op === OP_BLR) {
    pcrel.rd := 30.U
    pcrel.res := io.pc + 4.U
    io.pcrel.valid := true.B
  }.elsewhen(io.dinst.itype === I_PCRel) {
    pcrel.rd := io.dinst.rd.bits
    pcrel.res := pcadd
    io.pcrel.valid := true.B
  }
  io.pcrel.bits := pcrel

  // TODO: Not necessary? Depending on intruction, setup HINT
  // Hint_Branch(branch_type); // manual : shared/functions/registers/Hint_Branch
}
