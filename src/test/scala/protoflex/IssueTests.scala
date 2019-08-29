package protoflex

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import utils.AssemblyParser
import utils.AssemblyInstruction
import utils.DInstExtractor
import common.DEC_LITS._

class IssueTestsReadyValid(c: IssueUnit) extends PeekPokeTester(c)
{
  val (dinst_in, dinst_out) = DInstExtractor(c)

  // creates n instructions with target and source registers index of i
  val n = 3
  val insts = (0 to n) map { case i => AssemblyInstruction(i: Int, i: Int, i: Int) }

  // Fill the queue, check whenever ready signal goes low
  poke(c.io.enq.valid, 0)
  poke(c.io.deq.ready, 0)

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
      expect(c.io.deq.bits.rd.bits, 0)
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
      expect(c.io.deq.bits.rd.bits, i)
    }

    step(1)
  }
}

class IssueTestsPriority(c: IssueUnit) extends PeekPokeTester(c)
{
  val (dinst_in, dinst_out) = DInstExtractor(c)

  // creates n instructions with target and source registers index of i
  val n = 16
  val insts = (0 to n) map { case i => AssemblyInstruction(i: Int, i: Int, i: Int) }

  val descrition = List("~:last_idx; +:incoming; -:outgoing; ->:deq.ready;",
  "c: cycle; t:thread; 1,0 queue; R: pipe_reg;")
  val instr_flow = List(
  "c  t  1  0  R   t  1  0  R  t  1  0  R  t  1  0  R   ",
  "   3            2           1           0            ",
  "0 |#|  |  |+0| |#|  |  |  | #|  |  |  | #|  |  |  |~  ",
  "1 |#|  |  | 0| |#|  |  |+1| #|  |  |  | #|  |  |  |~  ",
  "2 |#|  |  | 0| |#|  |+2| 1| #|  |  |  | #|  |  |  |~  ",
  "3 |#|  |  | 0| |#|+3| 2| 1| #|  |  |  | #|  |  |  |~  ",
  "4 |#|  |  | 0| |#| 3| 2| 1| #|  |  |  | #|  |  |  |~  ",
  "5 |#|  |  | 0| |#| 3| 2|-1|~#|  |  |+4| #|  |  |  | ->",
  "6 |#|  |  |-0|~|#|  | 3| 2| #|  |+5| 4| #|  |  |  | ->",
  "7 |#|  |  |  | |#|  | 3| 2| #|+6| 5|-4|~#|  |  |  | ->",
  "8 |#|  |  |  | |#|  | 3|-2|~#|  | 6| 5| #|  |  |  | ->",
  "9 |#|  |  |  | |#|  |  | 3| #|  | 6|-5|~#|  |  |  | ->",
  "10|#|  |  |  | |#|  |  |-3|~#|  |  | 6| #|  |  |  | ->",
  "11|#|  |  |  | |#|  |  |  | #|  |  |-6|~#|  |  |  | ->",
  "12|#|  |  |  | |#|  |  |  | #|  |  |  | #|  |  |  | ->")

  def cycle(cycle : Int,
            enq_val : Boolean,
            inst : Int,
            tag : Int,
            enq_rdy : Boolean,
            deq_valid : Boolean,
            deq_rdy : Boolean,
            deq_tag : Int,
            deq_rd : Int) : Unit = {
    println(instr_flow(cycle))
    poke(c.io.enq.valid, enq_val)
    poke(c.io.enq.bits.tag, tag)
    poke(c.io.deq.ready, deq_rdy)
    dinst_in zip insts(inst).io map { case (io, in) => poke(io,in) }
    expect(c.io.enq.ready, enq_rdy)
    expect(c.io.deq.valid, deq_valid)
    if( deq_rdy ) {
      expect(c.io.deq.bits.tag, deq_tag)
      expect(c.io.deq.bits.rd.bits, deq_rd)
    }
    step(1)
  }


  // Fill the queue, check whenever ready signal goes low
  poke(c.io.enq.valid, 0)
  poke(c.io.deq.ready, 0)
  poke(c.io.enq.bits.tag, 0)

  step(1)
  expect(c.io.enq.ready, 1)
  expect(c.io.deq.valid, 0)
  descrition map (println(_))
  cycle( 0,  true, 0, 3,  true, false, false, 0, 0)
  cycle( 1,  true, 1, 2,  true,  true, false, 0, 0)
  cycle( 2,  true, 2, 2,  true,  true, false, 0, 0)
  cycle( 3,  true, 3, 2,  true,  true, false, 0, 0)
  cycle( 4,  true,15, 2, false,  true, false, 0, 0)
  cycle( 5,  true, 4, 1,  true,  true,  true, 2, 1)
  cycle( 6,  true, 5, 1,  true,  true,  true, 3, 0)
  cycle( 7,  true, 6, 1,  true,  true,  true, 1, 4)
  cycle( 8, false, 0, 0,  true,  true,  true, 2, 2)
  cycle( 9, false, 0, 0,  true,  true,  true, 1, 5)
  cycle(10, false, 0, 0,  true,  true,  true, 2, 3)
  cycle(11, false, 0, 0,  true,  true,  true, 1, 6)
  cycle(12, false, 0, 0,  true, false,  true, 0, 0)
}

class IssueTester extends ChiselFlatSpec with ArmflexBaseFlatSpec
{
  behavior of "IssueUnit"

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
object IssueRepl extends App with ArmflexBaseFlatSpec {
  iotesters.Driver.executeFirrtlRepl(args, () => new RRArbiter(4))
}

object IssueMain extends App with ArmflexBaseFlatSpec {
  chisel3.Driver.execute(args, () => new IssueUnit)
}
