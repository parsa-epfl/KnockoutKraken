package protoflex

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import common.SIGNALS._
import common.INSTRUCTIONS._


class DecodeTests(c: DecodeUnit) extends PeekPokeTester(c)
{
  val inst_bits =
    //   sf |  opc  |  0  1  0  1  0 | shift |  N |      Rm        |       imm6        |       Rn       |       Rd      |
    //    1 |  0  0 |  0  1  0  1  0 | shift |  0 |      Rm        |       imm6        |       Rn       |       Rd      |
    Seq( "1",  "00",     "01010",      "01",   "0",    "01010",          "111000",           "01010",         "00000")
  val inst = BigInt(inst_bits reduce ( _ + _ ), 2) // ORR

  poke(c.io.inst, inst)

  step(1)

  val out_data = Seq(c.io.dinst.rd, c.io.dinst.rs1, c.io.dinst.rs2, c.io.dinst.imm, c.io.dinst.shift)
  val exp_data = Seq( inst_bits(8),   inst_bits(7),   inst_bits(5),   inst_bits(6),     inst_bits(3)) map (x => BigInt(x, 2))

  val out_ctrl = Seq(c.io.dinst.imm_valid,            c.io.dinst.aluOp, c.io.dinst.shift_v, c.io.dinst.valid)
  val exp_ctrl = Seq(                 "0", inst_bits(1) + inst_bits(4),                "1",              "1") map (x => BigInt(x, 2))

  out_data zip exp_data map ( m => expect(m._1, m._2))
  out_ctrl zip exp_ctrl map ( m => expect(m._1, m._2))
}

class DecodeTester extends ChiselFlatSpec
{
  behavior of "Decoder"

  backends foreach {backend =>
    it should s"decode an instruction" in {
      Driver(() => new DecodeUnit, backend)((c) => new DecodeTests(c)) should be (true)
    }
  }
}
