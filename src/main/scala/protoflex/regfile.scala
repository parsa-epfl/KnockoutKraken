package protoflex

import chisel3._
import chisel3.util._

import common.PROCESSOR_TYPES._

class RFileIO extends Bundle
{
  val rs1_addr = Input(REG_T)
  val rs1_data = Output(DATA_T)
  val rs2_addr = Input(REG_T)
  val rs2_data = Output(DATA_T)

  val waddr    = Input(REG_T)
  val wdata    = Input(DATA_T)
  val wen      = Input(Bool())
}

class RegisterFile extends Module
{
  val io = IO(new RFileIO())

  val regfile = Mem(DATA_T, 32)

  when (io.wen && (io.waddr =/= 0.U))
  {
    regfile(io.waddr) := io.wdata
  }

  io.rs1_data := Mux((io.rs1_addr != 0.U), regfile(io.rs1_addr), 0.U)
  io.rs2_data := Mux((io.rs2_addr != 0.U), regfile(io.rs2_addr), 0.U)
}
