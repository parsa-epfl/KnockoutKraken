package protoflex

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import common.INSTRUCTIONS._
import utils.AssemblyParser
import utils.AssemblyInstruction
import utils.DInstExtractor

class DecodeInstructionTest(c: DecodeUnit) extends PeekPokeTester(c)
{
  def decode(c : DecodeUnit, insts : Seq[AssemblyInstruction]) = {
    val dinst_out = DInstExtractor(c)
    insts map {
      case inst =>
        println(inst.line)
        poke(c.io.in_inst.bits, inst.bitPat)
        poke(c.io.in_inst.valid, 1)
        poke(c.io.out_dinst.ready, 1)
        poke(c.io.in_tag, 1)
        step(1)

        if (peek(c.io.out_dinst.bits.inst_en) != 0) {
          dinst_out zip inst.io map { case (out, in) => expect(out, in)}
          expect(c.io.out_dinst.bits.tag, 1)
        } else {
          println("    - Skipped : Instruction not decoded")
        }
    }
  }
}

class DecodeALULog_SR(c: DecodeUnit) extends PeekPokeTester(c)
{
  val insts = AssemblyParser.parse("alu.x")
  val execute = new DecodeInstructionTest(c).decode(c, insts)
}

class DecodeBranches(c: DecodeUnit) extends PeekPokeTester(c)
{
  val insts = AssemblyParser.parse("branch.x")
  val execute = new DecodeInstructionTest(c).decode(c, insts)
}

class DecodeTester extends ChiselFlatSpec
{
  behavior of "Decoder"

  backends foreach {backend =>
    it should s"decode logical shift register instructions (with $backend)" in {
      Driver(() => new DecodeUnit, backend)((c) => new DecodeALULog_SR(c)) should be (true)
    }
  }
}
