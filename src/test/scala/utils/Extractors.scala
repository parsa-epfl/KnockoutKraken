package utils

import chisel3._

import protoflex._

object DInstExtractor {
  def extract(input : DInst) : Seq[UInt] =
  {
    val out = Seq(
      input.itype,
      input.op,
      input.rd.bits,
      input.rs1.bits,
      input.rs2.bits,
      input.imm.bits,
      input.shift_val.bits,
      input.shift_type,
      input.cond.bits,
      input.rd.valid,
      input.rs1.valid,
      input.rs2.valid,
      input.imm.valid,
      input.shift_val.valid,
      input.cond.valid,
      input.nzcv_en,
      input.inst32.valid
    )
    out
  }

  def apply(c: IssueUnit) : (Seq[UInt], Seq[UInt]) = (extract(c.io.enq.bits), extract(c.io.deq.bits))
  def apply(c: DecodeUnit) : Seq[UInt] = extract(c.io.dinst)
  def apply(c: ExecuteUnit) : Seq[UInt] = extract(c.io.dinst)
}
