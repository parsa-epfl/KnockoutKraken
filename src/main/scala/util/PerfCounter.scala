package armflex.util

import chisel3._

class PerfCounter(val width: Int) extends MultiIOModule {
  val io = IO(new Bundle {
    val incr  = Input(Bool())
    val count = Output(UInt(width.W))
    val reset = Input(Bool())
  })
  val cnt = RegInit(0.U(width.W))
  when(io.reset) {
    cnt := 0.U
  }
  when(io.incr) {
    cnt := cnt + 1.U
  }
  io.count := cnt
}
