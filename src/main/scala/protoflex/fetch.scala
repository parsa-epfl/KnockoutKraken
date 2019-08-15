
package protoflex

import chisel3._
import chisel3.util._
import common.{BRAMConfig, BRAMPort}
import common.PROCESSOR_TYPES._

class FInst(implicit val cfg: ProcConfig) extends Bundle
{
  val inst = INST_T
  val tag = cfg.TAG_T
  val pc = DATA_T
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
  val tag = RegInit(0.U)
  val pc = RegInit(0.U)
  val readIns = io.deq.ready || io.flush
  when(readIns) {
    valid := io.en
    tag := io.tagIn
    pc := io.PC
  }.otherwise {
    valid := valid
    tag := tag
    pc := pc
  }

  // Save last valid inst
  def vinstInit = {
    val wire = Wire(Valid(INST_T))
    wire.valid := false.B
    wire.bits := 0.U
    wire
  }

  val instV = RegInit(vinstInit)
  when(!io.deq.ready && RegNext(io.deq.ready)) {
    instV.valid := true.B
    instV.bits := io.ppageBRAM.dataOut.get
  }

  when(io.deq.ready) {
    instV.valid := false.B
  }

  io.incr := io.deq.ready && io.en
  io.deq.valid := valid && !io.flush
  io.deq.bits.tag := tag
  io.deq.bits.pc := pc
  io.deq.bits.inst := Mux(instV.valid, instV.bits, io.ppageBRAM.dataOut.get)
}
