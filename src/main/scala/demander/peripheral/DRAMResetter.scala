package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import DMAController.Bus._
import DMAController.Frontend.AXI4Writer
import armflex.demander.software_bundle.ParameterConstants

class DRAMResster extends MultiIOModule {
  val M_AXI = IO(new AXI4(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))
  // When the restter is done.
  val ready_o = IO(Output(Bool()))
  val sFlush :: sIdle :: Nil = Enum(2)
  val state_r = RegInit(sFlush)
  ready_o := state_r === sIdle

  // TODO: Replace the constant with a function to FPGA DRAM Size.
  val endAddress = 0x10000000
  val roundNumber = endAddress / (64 * 256)
  val round_cnt_r = RegInit(0.U(log2Ceil(roundNumber).W))

  val u_axi_write = Module(new AXI4Writer(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
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

