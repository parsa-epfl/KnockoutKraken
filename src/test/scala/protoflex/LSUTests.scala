package protoflex

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import utils.{AssemblyParser, AssemblyInstruction, DInstExtractor}
import common.DEC_LITS._
import utils.SoftwareStructs


class LSUTests(c: LoadStoreUnit) extends PeekPokeTester(c)
{
  val imm = 28
  val insts = AssemblyParser.parse("ldr.x") // TODO add more instruncti on forms
  val dinst_in = DInstExtractor.extract(c.io.dinst)

  val rVal1 = 0
  val rVal2 = 0
  var pc = 0x000000101001

  poke(c.io.rVal1, rVal1)
  poke(c.io.rVal2, rVal2)

  /* write tlb entries for testing
    *  | tag|set| offset|
    *  | 28 | 8 | 12    |
    */
  def write_entry(tag:Int, set:Int, offset:Int, d:Int){
    poke(c.io.write_tlb_vaddr.bits.tag, tag)
    poke(c.io.write_tlb_vaddr.bits.set, set)
    poke(c.io.write_tlb_vaddr.bits.offset, offset)
    poke(c.io.write_tlb_vaddr.valid, true)
    poke(c.io.write_tlb_entry, d)
    step(1)
    poke(c.io.write_tlb_vaddr.valid, false)
    poke(c.io.write_tlb_entry, 0)
    step(1)
  }

  def cycle(inst: Int) : Unit =
  {
    pc = pc + 0
    dinst_in zip insts(inst).io map { case (io, v) => poke(io, v) }
    print(SoftwareStructs.dinst(peek(c.io.dinst), false))
    poke(c.io.pc, pc) // pc or pc-4 ?
  }


  // write some tlb entries
  write_entry(tag = 0x01, set = 0x01, offset = 0x11, d = 0x11)
  step(5)
  //  write_entry(tag = 0x02, set = 0x02, offset = 0x22, d = 0x12)

  println("executing instruction")
  for( i <- 0 until insts.size ){
    cycle(i)
    step(1)
  }
  step(1)

}

class LSUTester extends ChiselFlatSpec
{
  behavior of "Load Store"

  backends foreach { backend =>
    it should s"Load store ops (with $backend)" in {
      Driver(() => new LoadStoreUnit, backend, verbose = false)((c) => new LSUTests(c)) should be (true)
    }
  }

}