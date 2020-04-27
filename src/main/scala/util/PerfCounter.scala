package armflex.util

import chisel3._

class PerfCounter extends MultiIOModule {
  val io = IO(new Bundle {
    val incr  = Input(Bool())
    val flush = Input(Bool())
    val count = Output(UInt(32.W))
  })
  val cnt = RegInit(0.U(40))
  when(io.flush) {
    cnt := 0.U
  }
  when(io.incr) {
    cnt := cnt + 1.U
  }
  io.count := cnt
}
