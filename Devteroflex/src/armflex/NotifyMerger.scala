package armflex

import chisel3._
import chisel3.util._


/**
 * Module to merge the wake up/sleep notification from more than one requesters. (TLB, Cache)
 * 
 * @params thidN The number threads to be manipulated.
 * @params inputPortsN The number of requesters.
 *
 */
class NotifyMerger(
  thidN: Int,
  inputPortsN: Int
) extends Module{
  val threadWidth = log2Ceil(thidN)
  val io = IO(new Bundle{
    val o = Output(UInt(thidN.W))
    val i = Input(Vec(inputPortsN, Flipped(ValidIO(UInt(threadWidth.W)))))
  })

  val to_aggregate = Wire(Vec(inputPortsN, UInt(thidN.W)))
  for(i <- 0 until inputPortsN){
    to_aggregate(i) := UIntToOH(io.i(i).bits) & Fill(thidN, io.i(i).valid)
  }
  io.o := to_aggregate.reduce({(x, y) => x | y})
}