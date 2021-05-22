package armflex_mmu.peripheral

import chisel3._
import chisel3.util._

import antmicro.Bus._
import antmicro.Frontend.AXI4Writer
import armflex_mmu.PageDemanderParameter
import armflex_cache.MemorySystemParameter

class DRAMResster(param: PageDemanderParameter) extends MultiIOModule {
  val M_AXI = IO(new AXI4(
    param.dramAddrWidth,
    param.dramDataWidth
  ))
  // When the restter is done.
  val ready_o = IO(Output(Bool()))
  val sFlush :: sIdle :: Nil = Enum(2)
  val state_r = RegInit(sFlush)
  ready_o := state_r === sIdle

  // TODO: Replace the constant with a function to FPGA DRAM Size.
  val endAddress = 1 << (param.dramAddrWidth - 12 + 4)
  val roundNumber = endAddress / (64 * 256)
  val round_cnt_r = RegInit(0.U(log2Ceil(roundNumber).W))

  val u_axi_write = Module(new AXI4Writer(
    param.dramAddrWidth,
    param.dramDataWidth
  ))
  u_axi_write.io.bus <> M_AXI
  u_axi_write.io.dataIn.bits := 0.U
  u_axi_write.io.dataIn.valid := true.B

  u_axi_write.io.xfer.address := Cat(round_cnt_r, 0.U(log2Ceil(64 * 256)))
  u_axi_write.io.xfer.length := 256.U
  u_axi_write.io.xfer.valid := state_r === sIdle

  when(u_axi_write.io.xfer.done){
    round_cnt_r := round_cnt_r + 1.U
  }

  when(state_r === sFlush && u_axi_write.io.xfer.done && round_cnt_r === (roundNumber-1).U){
    state_r := sIdle
  }
}

object DRAMResetterVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new DRAMResster(new PageDemanderParameter())))
}

