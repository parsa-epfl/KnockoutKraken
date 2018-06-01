package protoflex

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import utils.{AssemblyParser, AssemblyInstruction, DInstExtractor}
import common.DEC_LITS._

class BranchTest(c: BranchUnit) extends PeekPokeTester(c)
{
  val imm = 28
  val insts = AssemblyParser.parse("branch.x")
  val dinst_in = DInstExtractor.extract(c.io.dinst)

  val nzcv_all_cases =
    for {
      i <- 0 to 1
      j <- 0 to 1
      k <- 0 to 1
      l <- 0 to 1
    } yield Seq(i,j,k,l)


  def cycle(inst: Int, nzcv : Seq[Int]) : Unit =
  {
    dinst_in zip insts(inst).io map { case (io, v) => poke(io, v) }
    val nzcv_sig = BigInt(nzcv.mkString, 2)
    poke(c.io.nzcv, nzcv_sig)

    val nzcv_h = nzcv.reverse // Indexes are reversed compared to hardware ones
                              // nzcv = Seq(1,0,0,0) -> nzcv(0) = 1
    val cond = insts(inst).cond.toInt
    val result = cond match {
      /*000X*/ case EQ | NE => (nzcv_h(2) == 1)
      /*001X*/ case CS | HS | CC | LO => (nzcv_h(1) == 1)
      /*010X*/ case MI | PL => (nzcv_h(3) == 1)
      /*011X*/ case VS | VC => (nzcv_h(0) == 1)
      /*100X*/ case HI | LS => (nzcv_h(1) == 1)
      /*101X*/ case GE | LT => (nzcv_h(3) == 1)
      /*110X*/ case GT | LE => (nzcv_h(3) == 1)
      /*111X*/ case AL | NV => true
    }
    val res =  cond.toBinaryString.reverse(0) match {
      case '1' if cond != NV => !result
      case _   => result
    }

    //Uncomment in case tests don't pass
    //println("nzcv:" + nzcv.mkString + "|cond:" + insts(inst).cond.toInt.toBinaryString + "|res:" + res)

    insts(inst).op.toInt match {
      case OP_BCOND =>
        expect(c.io.offset, insts(inst).imm)
        expect(c.io.valid, res)
      case OP_B =>
        expect(c.io.offset, insts(inst).imm)
        expect(c.io.valid, true)
      case _ =>
        expect(c.io.offset, 0)
        expect(c.io.valid, false)
    }
  }

  for( i <- 0 until insts.size )
    nzcv_all_cases.map { nzcv => cycle(i, nzcv : Seq[Int])}
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

object BranchReplCondUnit extends App {
  iotesters.Driver.executeFirrtlRepl(args, () => new CondUnit)
}
