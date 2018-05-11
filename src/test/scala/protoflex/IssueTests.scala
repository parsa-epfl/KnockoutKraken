package protoflex

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import utils.AssemblyParser
import utils.AssemblyInstruction
import common.DEC_LITS._
import utils.DInstExtractor

class IssueTestsReadyValid(c: IssueUnit) extends PeekPokeTester(c)
{
  val (dinst_in, dinst_out) = DInstExtractor(c)

  val exe_in = Seq(
    c.io.exe_rd,
    c.io.exe_tag,
    c.io.exe_rd_v
  )
  val mem_in = Seq(
    c.io.mem_rd,
    c.io.mem_tag,
    c.io.mem_rd_v
  )

  // creates n instructions with target and source registers index of i
  val n = 3
  val insts = (0 to n) map { case i => AssemblyInstruction(i: Int, i: Int, i: Int) }

  // Fill the queue, check whenever ready signal goes low
  poke(c.io.enq.valid, 0)
  poke(c.io.deq.ready, 0)
  exe_in map { s => poke(s, 0) }
  mem_in map { s => poke(s, 0) }

  println("Enqueing")
  for (i <- 0 until 3) {
    println("Cycle" + i)
    poke(c.io.enq.valid, 1)
    poke(c.io.enq.bits.tag, 2)
    dinst_in zip insts(i).io map { case (io, in) => poke(io, in)}

    if(i == 3) expect(c.io.enq.ready, 0)
    else       expect(c.io.enq.ready, 1)

    if(i == 0) {
      expect(c.io.deq.valid, 0)
    } else {
      expect(c.io.deq.valid, 1)
      expect(c.io.deq.bits.tag, 2)
      expect(c.io.deq.bits.rd, 0)
    }
    step(1)
  }

  // Dequeing
  println("Dequeing")
  // Confirm queue is full, and others empty
  expect(c.io.enq.ready, 0)
  poke(c.io.enq.bits.tag, 0)
  expect(c.io.enq.ready, 1)
  poke(c.io.enq.bits.tag, 2)
  expect(c.io.enq.ready, 0)
  for (i <- 0 until 3) {
    println("Cycle" + i)
    poke(c.io.deq.ready, 1)
    expect(c.io.enq.ready, 1)

    if(i == 3) {
      expect(c.io.deq.valid, 0)
    } else {
      expect(c.io.deq.valid, 1)
      expect(c.io.deq.bits.tag, 2)
      expect(c.io.deq.bits.rd, i)
    }

    step(1)
  }
}

class IssueTestsPriority(c: IssueUnit) extends PeekPokeTester(c)
{
  val (dinst_in, dinst_out) = DInstExtractor(c)
  val exe_in = Seq(
    c.io.exe_rd,
    c.io.exe_tag,
    c.io.exe_rd_v
  )
  val mem_in = Seq(
    c.io.mem_rd,
    c.io.mem_tag,
    c.io.mem_rd_v
  )

  // creates n instructions with target and source registers index of i
  val n = 16
  val insts = (0 to n) map { case i => AssemblyInstruction(i: Int, i: Int, i: Int) }

  def cycle(cycle : Int, enq_val : Boolean, inst : Int, tag : Int, enq_rdy : Boolean, deq_valid : Boolean, deq_rdy : Boolean, deq_tag : Int, deq_rd : Int) : Unit = {
    poke(c.io.enq.valid, enq_val)
    poke(c.io.enq.bits.tag, tag)
    poke(c.io.deq.ready, deq_rdy)
    dinst_in zip insts(inst).io map { case (io, in) => poke(io,in) }
    expect(c.io.enq.ready, enq_rdy)
    expect(c.io.deq.valid, deq_valid)
    if( deq_rdy ) {
      expect(c.io.deq.bits.tag, deq_tag)
      expect(c.io.deq.bits.rd, deq_rd)
    }
    step(1)
  }


  // Fill the queue, check whenever ready signal goes low
  poke(c.io.enq.valid, 0)
  poke(c.io.deq.ready, 0)
  poke(c.io.enq.bits.tag, 0)
  exe_in map { s => poke(s, 0) }
  mem_in map { s => poke(s, 0) }
  println()
  step(1)
  expect(c.io.enq.ready, 1)
  expect(c.io.deq.valid, 0)
  println("c: cycle; t:thread; 1,0 queue; R: pipe_reg; ~:last_idx; +:incoming; -:outgoing; ->:deq.ready;")
  println("c  t  1  0  R   t  1  0  R  t  1  0  R  t  1  0  R   ")
  println("   3            2           1           0            ");
  println("0 |#|  |  |+0| |#|  |  |  | #|  |  |  | #|  |  |  |~  ");cycle( 0,  true, 0, 3,  true, false, false, 0, 0)
  println("1 |#|  |  | 0| |#|  |  |+1| #|  |  |  | #|  |  |  |~  ");cycle( 1,  true, 1, 2,  true,  true, false, 0, 0)
  println("2 |#|  |  | 0| |#|  |+2| 1| #|  |  |  | #|  |  |  |~  ");cycle( 2,  true, 2, 2,  true,  true, false, 0, 0)
  println("3 |#|  |  | 0| |#|+3| 2| 1| #|  |  |  | #|  |  |  |~  ");cycle( 3,  true, 3, 2,  true,  true, false, 0, 0)
  println("4 |#|  |  | 0| |#| 3| 2| 1| #|  |  |  | #|  |  |  |~  ");cycle( 4,  true,15, 2, false,  true, false, 0, 0)
  println("5 |#|  |  | 0| |#| 3| 2|-1|~#|  |  |+4| #|  |  |  | ->");cycle( 5,  true, 4, 1,  true,  true,  true, 2, 1)
  println("6 |#|  |  |-0|~|#|  | 3| 2| #|  |+5| 4| #|  |  |  | ->");cycle( 6,  true, 5, 1,  true,  true,  true, 3, 0)
  println("7 |#|  |  |  | |#|  | 3| 2| #|+6| 5|-4|~#|  |  |  | ->");cycle( 7,  true, 6, 1,  true,  true,  true, 1, 4)
  println("8 |#|  |  |  | |#|  | 3|-2|~#|  | 6| 5| #|  |  |  | ->");cycle( 8, false, 0, 0,  true,  true,  true, 2, 2)
  println("9 |#|  |  |  | |#|  |  | 3| #|  | 6|-5|~#|  |  |  | ->");cycle( 9, false, 0, 0,  true,  true,  true, 1, 5)
  println("10|#|  |  |  | |#|  |  |-3|~#|  |  | 6| #|  |  |  | ->");cycle(10, false, 0, 0,  true,  true,  true, 2, 3)
  println("11|#|  |  |  | |#|  |  |  | #|  |  |-6|~#|  |  |  | ->");cycle(11, false, 0, 0,  true,  true,  true, 1, 6)
  println("12|#|  |  |  | |#|  |  |  | #|  |  |  | #|  |  |  | ->");cycle(12, false, 0, 0,  true, false,  true, 0, 0)
}

class IssueTester extends ChiselFlatSpec
{
  behavior of "Issuer"

  backends foreach { backend =>
    "IssueTestsReadyValid" should s"test IssueUnit Ready Valid signals for queues (with $backend)" in {
      Driver(() => new IssueUnit, backend)((c) => new IssueTestsReadyValid(c)) should be (true)
    }
  }

  backends foreach { backend =>
    "IssueTestsPriority" should s"test IssueUnit arbiter (with $backend)" in {
      Driver(() => new IssueUnit, backend)((c) => new IssueTestsPriority(c)) should be (true)
    }
  }
}

/**
  * This provides a way to ruin the firrtl-interpreter REPL (or shell)
  * on the lowered firrtl generated by your circuit. You will be placed
  * in an interactive shell. This can be very helpful as a debugging
  * technique. Type help to see a list of commands.
  */
object IssueRepl extends App {
  iotesters.Driver.executeFirrtlRepl(args, () => new RRArbiter(4))
}

object IssueMain extends App {
  chisel3.Driver.execute(args, () => new IssueUnit)
}
