package armflex.util

import chisel3._
import chisel3.util._

class DoubleLatencyQueueCtrlIO extends Bundle {
  val drop = Bool()
  val done = Bool()
  val long = Bool()
  val longDone = Bool()
}

// TODO: Short path should allocate less entries, close to the latency of a hit request
class DoubleLatencyQueue[T <: Data](gen: T, maxElements: Int) extends MultiIOModule {

  // IO
  val ctrl_i = IO(Input(new DoubleLatencyQueueCtrlIO))
  val req_i = IO(Flipped(Decoupled(gen.cloneType)))
  val resp_o = IO(new Bundle {
    val base = Valid(gen.cloneType)
    val long = Valid(gen.cloneType)
  })
  val pending = IO(Output(UInt(log2Ceil(maxElements).W)))

  // Control Signals
  private val drop = ctrl_i.drop
  private val done = ctrl_i.done
  private val long = ctrl_i.long
  private val longDone = ctrl_i.longDone

  // Meta Data
  private val baseQ = Module(new Queue(gen.cloneType, maxElements, false, false))
  private val longQ = Module(new Queue(gen.cloneType, maxElements, false, false))

  // ---- Control -----
  // Receive requests
  baseQ.io.enq <> req_i

  // Base path
  // drop -> Transaction dropped
  // done -> Transaction completes
  // long -> Transaction takes long path
  baseQ.io.deq.ready := drop || done || long

  // Long path
  longQ.io.enq.valid := long
  // longDone -> Long path completed
  longQ.io.deq.ready := longDone

  // Response
  resp_o.base.valid := done
  resp_o.long.valid := longDone

  // ---- Data ----
  longQ.io.enq.bits := baseQ.io.deq.bits
  resp_o.base.bits := baseQ.io.deq.bits
  resp_o.long.bits := longQ.io.deq.bits

  // ---- Track Pending Requests ----
  pending := baseQ.io.count + longQ.io.count

  if (true) { // TODO, conditional asserts
    // A request must be pending in order to pop it
    when(baseQ.io.deq.ready) { assert(baseQ.io.deq.valid) }
    when(longQ.io.deq.ready) { assert(longQ.io.deq.valid) }
    // Long path must pop request from short path
    when(longQ.io.enq.fire) { assert(baseQ.io.deq.fire) }
    // Responses must be valid
    when(resp_o.base.valid) { assert(baseQ.io.deq.fire) }
    when(resp_o.long.valid) { assert(longQ.io.deq.fire) }
    // Can't respond at the same time both long and short response
    when(resp_o.base.valid || resp_o.long.valid) { assert(!(resp_o.base.valid && resp_o.long.valid)) }
  }
}

/**
 * This module keeps track whenever the next stage queue has enough entries left to
 * receive the transaction.
 *
 * IO: trans
 *     - in             Transaction was sent requiring an free entry at receiver
 *     - out            Transaction left receiver queue freeing an entry
 *     - dropped        Transaction completed but dropped for next stage
 *     ready            Receiver has entries left
 *
 * @params size Max number of elements in receiver Queue
 *
 */
class CreditQueueController(size: Int) extends MultiIOModule {
  val trans = IO(new Bundle {
    val in = Input(Bool())
    val dropped = Input(Bool())
    val out = Input(Bool())
  })
  val ready = IO(Output(Bool()))

  private val cnt = RegInit(0.U(log2Ceil(size).W))
  private val diff = WireInit(0.U(2.W))
  diff := trans.in.asUInt - (trans.out.asUInt + trans.dropped.asUInt)
  cnt := cnt + diff
  ready := cnt < (size-1).U
}