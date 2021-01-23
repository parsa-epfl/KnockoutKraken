package armflex.demander

import chisel3._
import chisel3.util._

import armflex.demander.software_bundle._

/**
 * This module converts various types of messages to a single type.
 * 
 * The problem is that Chisel now doesn't support Union. 
 */ 
class DemanderMessageCompositor(
  fifoDepth: Int = 2
) extends MultiIOModule {
  val o = IO(Decoupled(new PageDemanderMessage))

  val itlb_miss_request_i = IO(Flipped(Decoupled(new TLBMissRequestMessage)))
  val dtlb_miss_request_i = IO(Flipped(Decoupled(new TLBMissRequestMessage)))

  val itlb_evict_request_i = IO(Flipped(Decoupled(new TLBEvictionMessage)))
  val dtlb_evict_request_i = IO(Flipped(Decoupled(new TLBEvictionMessage)))

  val qemu_miss_reply_i = IO(Flipped(Decoupled(new QEMUMissReply)))
  val qemu_evict_reply_i = IO(Flipped(Decoupled(new QEMUEvictReply)))

  // Create FIFOs for each message.
  val itlb_miss_req_q = Queue(itlb_miss_request_i, fifoDepth)
  val dtlb_miss_req_q = Queue(dtlb_miss_request_i, fifoDepth)

  val itlb_evict_req_q = Queue(itlb_evict_request_i, fifoDepth)
  val dtlb_evict_req_q = Queue(dtlb_evict_request_i, fifoDepth)

  val qemu_miss_rep_q = Queue(qemu_miss_reply_i, fifoDepth)
  val qemu_evict_req_q = Queue(qemu_evict_reply_i, fifoDepth)

  // TODO: Generate the message for each type.
  val messages = Seq(
    qemu_evict_req_q,
    qemu_miss_rep_q,
    dtlb_evict_req_q,
    itlb_evict_req_q,
    dtlb_miss_req_q,
    itlb_miss_req_q,
  ).map({ mess =>
    val res = Decoupled(mess.bits.getRawMessage.cloneType)
    res.bits <> mess.bits.getRawMessage
    res.valid <> mess.valid
    res.ready <> mess.ready
    res
  })
  // TODO: Create an arbiter.
  val u_arb = Module(new Arbiter(new PageDemanderMessage, 6))
  u_arb.io.in <> VecInit(messages)
  u_arb.io.out <> o
}

class QEMUMessageCompositor(
  fifoDepth: Int = 2
) extends MultiIOModule {
  val evict_notify_req_i = IO(Flipped(Decoupled(new PageEvictRequest(QEMUMessagesType.sEvictNotify))))
  val evict_done_req_i = IO(Flipped(Decoupled(new PageEvictRequest(QEMUMessagesType.sEvictDone))))

  val page_fault_req_i = IO(Flipped(Decoupled(new PageFaultRequest)))

  val o = IO(Decoupled(new QEMUMessage))

  val evict_notify_req_q = Queue(evict_notify_req_i, fifoDepth)
  val evict_done_req_q = Queue(evict_done_req_i, fifoDepth)
  val page_fault_req_q = Queue(page_fault_req_i, fifoDepth)

  val messages = Seq(
    evict_done_req_q,
    evict_notify_req_q,
    page_fault_req_q
  ).map({ mess =>
    val res = Decoupled(mess.bits.getRawMessage.cloneType)
    res.bits <> mess.bits.getRawMessage
    res.valid <> mess.valid
    res.ready <> mess.ready
    res
  })
}

