package common

import chisel3._
import chisel3.util.{EnqIO, DeqIO, log2Ceil, Counter, DecoupledIO, ReadyValidIO, IrrevocableIO, Valid}
import chisel3.experimental.{DataMirror, Direction, MultiIOModule, requireIsChiselType}

/* Note: This Queue is almost entirely copy pasted from chisel3.util.Queue
 * It is created in order to add a flushing mechanism ( needed in armflex )
 */

/** An I/O Bundle for FReg
  * @param gen The type of data of the Reg
  */
class FRegIO[T <: Data](private val gen: T) extends Bundle
{ // See github.com/freechipsproject/chisel3/issues/765 for why gen is a private val and proposed replacement APIs.

  /* These may look inverted, because the names (enq/deq) are from the perspective of the client,
   *  but internally.
   */
  val enq = Flipped(EnqIO(gen))
  val deq = Flipped(DeqIO(gen))
  /** Flush */
  val flush = Input(Bool())
}

/** A hardware module implementing a Queue
  * @param gen The type of data to queue
  * @param entries The max number of entries in the queue
  * @param pipe True if a single entry queue can run at full throughput (like a pipeline). The ''ready'' signals are
  * combinationally coupled.
  * @param flow True if the inputs can be consumed on the same cycle (the inputs "flow" through the queue immediately).
  * The ''valid'' signals are coupled.
  *
  * @example {{{
  * val q = Module(new Queue(UInt(), 16))
  * q.io.enq <> producer.io.out
  * consumer.io.in <> q.io.deq
  * }}}
  */
class FReg[T <: Data](gen: T)
    extends Module() {

  val io = IO(new FRegIO(gen))

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
