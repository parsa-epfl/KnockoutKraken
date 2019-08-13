
package protoflex

import chisel3._
import chisel3.util._
import common.{BRAMConfig, BRAMPort}
import common.PROCESSOR_TYPES._

class FInst(implicit val cfg: ProcConfig) extends Bundle
{
  val inst = INST_T
  val tag = cfg.TAG_T
}

class FetchUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  val en = Input(Bool())
  val PC = Input(DATA_T)
  val tagIn  = Input(cfg.TAG_T)

  val incr = Output(Bool())
  val flush = Input(Bool())

  // Program Page
  val ppageBRAM = Flipped(new BRAMPort(1)(cfg.ppageBRAMc))
  // Fetch -> decode
  val deq = Flipped(DeqIO(new FInst))
}


class FetchUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new FetchUnitIO())

  io.ppageBRAM.en := true.B
  io.ppageBRAM.addr := io.PC >> 2.U // PC is byte addressed, BRAM is 32bit word addressed
  io.ppageBRAM.dataIn.get := 0.U
  io.ppageBRAM.writeEn.get := false.B

  // One cycle delay for BRAM
  val valid = RegInit(false.B)
  val finst = Reg(new FInst)

  when(io.deq.ready || io.flush) {
    valid := io.en
    finst.inst := io.ppageBRAM.dataOut.get
    finst.tag := io.tagIn
  }.otherwise {
    valid := valid
    finst := finst
  }

  io.incr := io.deq.ready

  io.deq.valid := valid && !io.flush
  io.deq.bits := finst

}
