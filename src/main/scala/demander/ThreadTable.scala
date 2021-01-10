package armflex.demander

import chisel3._
import chisel3.util._

class ThreadTable(
  threadNumber: Int,
  processIDWidth: Int
) extends MultiIOModule {
  val tid_i = IO(Input(UInt(log2Ceil(threadNumber).W)))
  val pid_o = IO(Output(UInt(processIDWidth.W)))

  class insertion_pair_t extends Bundle {
    val thread_id = UInt(log2Ceil(threadNumber).W)
    val process_id = UInt(processIDWidth.W)
  }
  val write_request_i = IO(Flipped(Valid(new insertion_pair_t)))

  val table = Mem(threadNumber, UInt(processIDWidth.W))
  pid_o := table(tid_i)

  when(write_request_i.valid){
    table(write_request_i.bits.thread_id) := write_request_i.bits.process_id
  }
  
}

