package armflex.util

import chisel3._
import chisel3.util.{log2Ceil, Counter, Decoupled, ReadyValidIO, IrrevocableIO, Valid, DecoupledIO}
import chisel3.experimental.{DataMirror, Direction, requireIsChiselType}

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

class FlushReg[T <: Data](private val gen: T)
    extends Module() {

  val io = IO(new FlushRegIO(gen))

  val reg = Reg(gen)
  val valid = RegInit(false.B)

  val do_enq = WireInit(!valid || io.deq.ready || io.flush)

  when (do_enq) {
    reg := io.enq.bits
    valid := io.enq.valid
  }
  io.enq.ready := do_enq

  io.deq.bits := reg
  io.deq.valid := valid && !io.flush
}

class FlushQueue[T <: Data](
  gen: T, 
  entries: Int = 2,
  pipe: Boolean = false,
  flow: Boolean = false
) extends Module {

  val io = IO(new FlushRegIO(gen))

  val ram = Mem(entries, gen)
  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)
  val maybe_full = RegInit(false.B)

  val ptr_match = enq_ptr.value === deq_ptr.value
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val do_enq = WireDefault(io.enq.fire())
  val do_deq = WireDefault(io.deq.fire())

  when (do_enq) {
    ram(enq_ptr.value) := io.enq.bits
    enq_ptr.inc()
  }
  when (do_deq) {
    deq_ptr.inc()
  }
  when (do_enq =/= do_deq) {
    maybe_full := do_enq
  }

  io.deq.valid := !empty
  io.enq.ready := !full
  io.deq.bits := ram(deq_ptr.value)

  if (flow) {
    when (io.enq.valid) { io.deq.valid := true.B }
    when (empty) {
      io.deq.bits := io.enq.bits
      do_deq := false.B
      when (io.deq.ready) { do_enq := false.B }
    }
  }

  if (pipe) {
    when (io.deq.ready) { io.enq.ready := true.B }
  }

  when(io.flush) {
    enq_ptr.reset()
    deq_ptr.reset()
    maybe_full := false.B

    io.deq.valid := false.B
    io.enq.ready := false.B
  }
}

object FlushQueue {
  def apply[T <: Data](genTag: T, entries: Int): FlushQueue[T] = new FlushQueue(genTag, entries)
  def apply[T <: Data](in: DecoupledIO[T], entries: Int = 2, pipe: Boolean = false, flow: Boolean = false, flush: Bool = false.B)(name: String): DecoupledIO[T] = {
    if(entries == 0){
      val res = Wire(Decoupled(in.bits))
      res <> in
      when(flush){
        res.valid := false.B
        res.ready := false.B
      }
      res
    } else {
      val m = Module(new FlushQueue(in.bits.cloneType, entries, pipe, flow))
      val u_queue = if(name.nonEmpty) m.suggestName(name) else m
      u_queue.io.enq <> in
      u_queue.io.flush := flush
      u_queue.io.deq
    }
  }
}
