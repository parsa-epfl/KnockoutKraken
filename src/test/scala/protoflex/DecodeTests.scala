package protoflex

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import common.INSTRUCTIONS._
import utils.AssemblyParser
import utils.AssemblyInstruction


class DecodeTests(c: DecodeUnit) extends PeekPokeTester(c)
{
  val instrs = AssemblyParser.parse("alu.x")

  val dinst_out = Seq(
    c.io.out_dinst.bits.aluOp,
    c.io.out_dinst.bits.rd,
    c.io.out_dinst.bits.rs1,
    c.io.out_dinst.bits.rs2,
    c.io.out_dinst.bits.imm,
    c.io.out_dinst.bits.shift,
    c.io.out_dinst.bits.rd_en,
    c.io.out_dinst.bits.rs1_en,
    c.io.out_dinst.bits.rs2_en,
    c.io.out_dinst.bits.imm_en,
    c.io.out_dinst.bits.shift_en,
    c.io.out_dinst.bits.inst_en
  )

  instrs map {
    case inst if(inst.inst_en != 0) =>
      println(inst.line)
      poke(c.io.in_inst.bits, inst.bitPat)
      poke(c.io.in_inst.valid, 1)
      poke(c.io.out_dinst.ready, 1)
      poke(c.io.in_tag, 1)

      step(1)

      dinst_out zip inst.io map { case (out, in) => expect(out, in)}

      expect(c.io.out_dinst.bits.tag, 1)
    case _ =>
  }

}

class DecodeTester extends ChiselFlatSpec
{
  behavior of "Decoder"

  private val backendNames = Array("firrtl")

  for ( backend <- backendNames ) {
    it should s"decode logical shift register instructions (with $backend)" in {
      Driver(() => new DecodeUnit, backend)((c) => new DecodeTests(c)) should be (true)
    }
  }
}
