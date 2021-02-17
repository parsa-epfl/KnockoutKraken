package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import armflex.demander.software_bundle._

class QEMUMessageEncoder(
  fifoDepth: Int = 2
) extends MultiIOModule {
  val evict_notify_req_i = IO(Flipped(Decoupled(new PageEvictNotification(QEMUMessagesType.sEvictNotify))))
  val evict_done_req_i = IO(Flipped(Decoupled(new PageEvictNotification(QEMUMessagesType.sEvictDone))))

  val page_fault_req_i = IO(Flipped(Decoupled(new PageFaultNotification)))

  val o = IO(Decoupled(UInt(512.W)))

  val evict_notify_req_q = Queue(evict_notify_req_i, fifoDepth)
  val evict_done_req_q = Queue(evict_done_req_i, fifoDepth)
  val page_fault_req_q = Queue(page_fault_req_i, fifoDepth)

  val messages = Seq(
    evict_done_req_q,
    evict_notify_req_q,
    page_fault_req_q
  ).map({ mess =>
    val res = Wire(Decoupled(mess.bits.getRawMessage.cloneType))
    res.bits := mess.bits.getRawMessage
    res.valid := mess.valid
    mess.ready := res.ready
    res
  })

  val u_arb = Module(new Arbiter(new QEMUTxMessage, 3))
  
  u_arb.io.in.zip(messages).foreach({
    case (s, d) => s <> d
  })
  
  o.valid := u_arb.io.out.valid
  u_arb.io.out.ready := o.ready
  o.bits := Cat(u_arb.io.out.bits.asVec(32).reverse)

}

object QEMUMessageEncoderVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new QEMUMessageEncoder))
}
