package armflex_pmu

import chisel3._
import chisel3.experimental._
import chisel3.util._


class CycleCountingPort(threadNumber: Int = 128) extends Bundle {
  val start = Valid(UInt(log2Ceil(threadNumber).W))
  val stop = Valid(UInt(log2Ceil(threadNumber).W))
}

class CycleCounters(counterCount: Int = 8, counterSize: Int = 32, threadNumber: Int = 128) extends Module {
  val rCounters = RegInit(0.U.asTypeOf(Vec(counterCount, UInt(counterSize.W))))
  val rCurrent = RegInit(0.U(log2Ceil(counterCount).W))

  val iReq = IO(Input(new CycleCountingPort(threadNumber)))

  val oCounters = IO(Output(rCounters.cloneType))
  oCounters := rCounters

  val sIdle :: sCounting :: Nil = Enum(2)
  val rState = RegInit(sIdle)
  val rTargetThreadID = RegInit(0.U(log2Ceil(threadNumber).W))

  // At present we only record one request from one thread. All the stop signal from other threads are ignored.
  switch(rState){
    is(sIdle){
      when(iReq.start.valid){
        rState := sCounting
      }
    }
    is(sCounting){
      when(iReq.stop.valid && iReq.stop.bits === rTargetThreadID){
        rState := sIdle
      }
    }
  }

  when(iReq.start.valid && rState === sIdle){
    rTargetThreadID := iReq.start.bits
  }

  when(rState === sIdle && iReq.start.valid){
    rCounters(rCurrent) := 0.U // clear
  }.elsewhen(rState === sCounting){
    rCounters(rCurrent) := rCounters(rCurrent) + 1.U
  }

  when(rState === sCounting && iReq.stop.valid && iReq.stop.bits === rTargetThreadID){
    rCurrent := rCurrent + 1.U
  }

}