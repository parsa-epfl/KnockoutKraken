package protoflex

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import common.INSTRUCTIONS._


class DecodeTests(c: DecodeUnit) extends PeekPokeTester(c)
{
  val inst_bits =
    //   sf |  opc  |  0  1  0  1  0 | shift |  N |      Rm        |       imm6        |       Rn       |       Rd      |
    //    1 |  0  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      |
    Seq( "1",  "00",     "01010",      "01",   "0",    "01010",          "111000",           "01010",         "00000")
  val inst = BigInt(inst_bits reduce ( _ + _ ), 2) // ORR

  poke(c.io.in_inst.bits, inst)
  poke(c.io.in_inst.valid, true.B)
  poke(c.io.in_tag, 0.U)
  step(1)

  val out_data = Seq(c.io.out_dinst.bits.rd, c.io.out_dinst.bits.rs1, c.io.out_dinst.bits.rs2, c.io.out_dinst.bits.imm, c.io.out_dinst.bits.shift)
  val exp_data = Seq( inst_bits(8),   inst_bits(7),   inst_bits(5),   inst_bits(6),     inst_bits(3)) map (x => BigInt(x, 2))

  val out_ctrl = Seq(c.io.out_dinst.bits.imm_en,            c.io.out_dinst.bits.aluOp, c.io.out_dinst.bits.shift_en, c.io.out_dinst.bits.inst_en)
  val exp_ctrl = Seq(                 "0", inst_bits(1) + inst_bits(4),                "1",              "1") map (x => BigInt(x, 2))

  out_data zip exp_data map ( m => expect(m._1, m._2))
  out_ctrl zip exp_ctrl map ( m => expect(m._1, m._2))
}

class DecodeTester extends ChiselFlatSpec
{
  behavior of "Decoder"

  private val backendNames = Array("firrtl")

  for ( backend <- backendNames ) {
    it should s"decode an instruction (with $backend)" in {
      Driver(() => new DecodeUnit, backend)((c) => new DecodeTests(c)) should be (true)
    }
  }
}
