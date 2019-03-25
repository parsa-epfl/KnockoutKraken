// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import org.scalacheck.Prop.True



class TLBTest(c: TLBUnit) extends PeekPokeTester(c)
{
  /* write tlb entries for testing
    *  | tag|set| offset|
    *  | 28 | 8 | 12    |
    */
  def write_entry(tag:Int, set:Int, offset:Int, d:Int){
    poke(c.io.vaddr.bits.tag, tag)
    poke(c.io.vaddr.bits.set, set)
    poke(c.io.vaddr.bits.offset, offset)
    poke(c.io.vaddr.valid, true)
    poke(c.io.write_tlb_entry, d)
    poke(c.io.write_tlb_valid, true)
    step(1)
    poke(c.io.vaddr.valid, false)
    poke(c.io.write_tlb_entry, 0)
    poke(c.io.write_tlb_valid, false)
    step(1)
  }

  // read tlb entries
  def read_entry(tag:Int, set:Int, offset:Int)={
    poke(c.io.vaddr.bits.tag, tag)
    poke(c.io.vaddr.bits.set, set)
    poke(c.io.vaddr.bits.offset, offset)
    poke(c.io.vaddr.valid, true)
    poke(c.io.rw, true)
    step(1)
    poke(c.io.vaddr.valid,false)
    poke(c.io.rw, false)
    step(1)
  }

  poke(c.io.vaddr.valid, false)
  step(5)
  write_entry(tag = 0x12, set = 0x1e, offset = 0x11, d = 0x12)
  write_entry(tag = 0x13, set = 0x13, offset = 0x33, d = 0x13)
  read_entry(tag = 0x12, set = 0x1e, offset = 0x11)
  read_entry(tag = 0x13, set = 0x1e, offset = 0x11)
  read_entry(tag = 0x12, set = 0x1e, offset = 0x11)


}

class TLBTester extends ChiselFlatSpec
{
  behavior of "TLB"

  backends foreach {backend =>
    it should s"test TLB (with $backend)" in {
      Driver(() => new TLBUnit, backend, verbose = false)((c) => new TLBTest(c)) should be (true)
    }
  }
}
