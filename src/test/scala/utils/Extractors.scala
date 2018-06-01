package utils

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import protoflex._

object DInstExtractor {
  def extract(input : DInst) : Seq[UInt] =
  {
    val out = Seq(
      input.itype,
      input.op,
      input.rd,
      input.rs1,
      input.rs2,
      input.imm,
      input.shift,
      input.cond,
      input.rd_en,
      input.rs1_en,
      input.rs2_en,
      input.imm_en,
      input.shift_en,
      input.cond_en,
      input.inst_en
    )
    out
  }

  def apply(c: IssueUnit) : (Seq[UInt], Seq[UInt]) = (extract(c.io.enq.bits), extract(c.io.deq.bits))
  def apply(c: DecodeUnit) : Seq[UInt] = extract(c.io.dinst)
  def apply(c: ExecuteUnit) : Seq[UInt] = extract(c.io.dinst)
}
