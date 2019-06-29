
package protoflex

import chisel3._
import chisel3.util._
import common.{BRAMConfig, BRAMPort}
import common.PROCESSOR_TYPES._


class FetchUnitIO() extends Bundle
{
  val en = Input(Bool())
  val PC = Input(DATA_T)
  val tag_in  = Input(TAG_T)
  // Fetch - decode
  val inst = Flipped(DeqIO(INST_T))
  val tag_out  = Output(TAG_T)

  // memory interface
  val data = Input(INST_T)
  val addr = Output(DATA_T)
  val rd_en = Output(Bool())
}


class FetchUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new FetchUnitIO())
  val enable = io.en && io.inst.ready
  io.addr := io.PC
  io.rd_en := enable
  io.inst.bits := io.data


  // assuming BRAM read latency 2
  require(cfg.ppageBRAMc.readLatency == 2)
  io.inst.valid := RegNext(RegNext(enable))

  io.tag_out := io.tag_in // TODO: modify for multiple thread
}
