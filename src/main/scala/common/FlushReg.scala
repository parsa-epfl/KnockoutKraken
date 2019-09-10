package common

import chisel3._
import chisel3.util.{log2Ceil, Counter, Decoupled, ReadyValidIO, IrrevocableIO, Valid}
import chisel3.experimental.{DataMirror, Direction, MultiIOModule, requireIsChiselType}

/** An I/O Bundle for FlushReg (FlushRegister)
  * @param gen The type of data of the Reg
  */
class FlushRegIO[T <: Data](private val gen: T) extends Bundle
{
  val enq = Flipped(Decoupled(gen))
  val deq = Decoupled(gen)
  /** Flush */
  val flush = Input(Bool())
}

class FlushReg[T <: Data](gen: T)
    extends Module() {

  val io = IO(new FlushRegIO(gen))

  private val reg = Reg(gen)
  private val valid = RegInit(false.B)

  private val do_enq = WireInit(!valid || io.deq.ready || io.flush)

  when (do_enq) {
    reg := io.enq.bits
    valid := io.enq.valid
  }
  io.enq.ready := do_enq

  io.deq.bits := reg
  io.deq.valid := valid && !io.flush

}
