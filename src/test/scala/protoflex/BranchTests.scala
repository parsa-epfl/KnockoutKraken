package protoflex

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import utils.AssemblyParser
import utils.AssemblyInstruction
import common.DEC_LITS._

class BranchTest(c: BranchUnit) extends PeekPokeTester(c)
{
  val instrs = AssemblyParser.parse("alu.x")

  /*
  val dinst_in = Seq(
    c.io.dinst.itype,
    c.io.dinst.op,
    c.io.dinst.rd,
    c.io.dinst.rs1,
    c.io.dinst.rs2,
    c.io.dinst.imm,
    c.io.dinst.shift,
    c.io.dinst.cond,
    c.io.dinst.rd_en,
    c.io.dinst.rs1_en,
    c.io.dinst.rs2_en,
    c.io.dinst.imm_en,
    c.io.dinst.shift_en,
    c.io.dinst.cond_en,
    c.io.dinst.inst_en
  )
   */
}

class BranchTester extends ChiselFlatSpec
{
  behavior of "Branching"

  backends foreach { backend =>
    it should s"branch ops (with $backend)" in {
      Driver(() => new BranchUnit, backend)((c) => new BranchTest(c)) should be (true)
    }
  }
}

object BranchRepl extends App {
  iotesters.Driver.executeFirrtlRepl(args, () => new BranchUnit)
}

