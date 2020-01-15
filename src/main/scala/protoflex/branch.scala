// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.{MuxLookup, Valid, Cat}

import common.DECODE_CONTROL_SIGNALS._
import common.PROCESSOR_TYPES._

class BInst(implicit val cfg: ProcConfig) extends Bundle {
  val pc = Output(DATA_T)
}

// Writeback to register based on PC value instruction
class PCRel(implicit val cfg: ProcConfig) extends Bundle {
  val rd = Output(REG_T)
  val res = Output(DATA_T)
}

class BranchUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  val dinst = Input(new DInst)
  val rVal1 = Input(DATA_T)
  val rVal2 = Input(DATA_T)
  val cond = Input(Bool())
  val pc = Input(DATA_T)

  val binst = Output(Valid(new BInst))
  val pcrel = Output(Valid(new PCRel))
}

class BranchUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new BranchUnitIO)


  val imm = io.dinst.imm.bits.asSInt.pad(64)
  val immS2  = Cat(io.dinst.imm.bits, 0.U(2.W)).asSInt.pad(64)
  val immS12 = Cat(io.dinst.imm.bits, 0.U(12.W)).asSInt.pad(64)
  val immSExt = MuxLookup(io.dinst.itype, imm, Array(
                        I_BCImm -> immS2,
                        I_BImm  -> immS2,
                        I_CBImm -> immS2,
                        I_PCRel -> Mux(io.dinst.op === OP_ADRP,
                                       immS12, imm)
                ))

  val pcadd = WireInit((io.pc.zext + immSExt).asUInt)

  val pcrel = Wire(new PCRel)
  pcrel.rd := io.dinst.rd.bits
  pcrel.res := pcadd
  io.pcrel.valid := false.B
  when(io.dinst.itype === I_BImm && io.dinst.op === OP_BL) {
    pcrel.rd := 30.U
    pcrel.res := io.pc + 4.U
    io.pcrel.valid := true.B
  }.elsewhen(io.dinst.itype === I_PCRel) {
    pcrel.rd := io.dinst.rd.bits
    pcrel.res := pcadd
    io.pcrel.valid := true.B
  }
  io.pcrel.bits := pcrel

  val binst = Wire(new BInst)
  binst.pc := pcadd
  io.binst.bits := binst
  io.binst.valid :=
    MuxLookup(io.dinst.itype, false.B, Array(
                I_BImm -> true.B,
                I_BCImm -> io.cond,
                I_CBImm -> MuxLookup(io.dinst.op, false.B, Array(
                                       OP_CBZ  -> (io.rVal2 === 0.U),
                                       OP_CBNZ -> (io.rVal2 =/= 0.U)
                                     ))
              ))
}
