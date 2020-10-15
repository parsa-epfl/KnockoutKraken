package armflex

import chisel3._
import chisel3.util._


/**
 * Module to merge the wake up/sleep notification from more than one requesters. (TLB, Cache)
 * 
 * @param threadNumber The number threads to be manipulated.
 * @param inputPortNumber The number of requesters.
 * 
 */ 
class NotifyMerger(
  threadNumber: Int,
  inputPortNumber: Int
) extends Module{
  val threadWidth = log2Ceil(threadNumber)
  val io = IO(new Bundle{
    val o = Output(UInt(threadNumber.W))
    val i = Input(Vec(inputPortNumber, Flipped(ValidIO(UInt(threadWidth.W)))))
  })

  val to_aggregate = Wire(Vec(inputPortNumber, UInt(threadNumber.W)))
  for(i <- 0 until inputPortNumber){
    to_aggregate(i) := UIntToOH(io.i(i).bits) & Fill(threadNumber, io.i(i).valid)
  }
  io.o := to_aggregate.reduce({(x, y) => x | y})
}