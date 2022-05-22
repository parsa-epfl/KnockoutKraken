package armflex_mmu.peripheral

import armflex.{PageEvictNotification, PageFaultNotification, QEMUMessagesType, TxMessage}
import chisel3._
import chisel3.util._
import armflex_cache.PageTableParams
import armflex_mmu.MemoryHierarchyParams

class QEMUMessageEncoder(
  param: MemoryHierarchyParams,
  fifoDepth: Int = 2
) extends Module {
  val evict_notify_req_i = IO(Flipped(Decoupled(new PageEvictNotification(
    QEMUMessagesType.sEvictNotify,
    param.getPageTableParams
  ))))
  val evict_done_req_i = IO(Flipped(Decoupled(new PageEvictNotification(
    QEMUMessagesType.sEvictDone,
    param.getPageTableParams
  ))))

  val page_fault_req_i = IO(Flipped(Decoupled(new PageFaultNotification(param.getPageTableParams))))

  val o = IO(Decoupled(UInt(512.W)))
  val oq = Wire(Decoupled(UInt(512.W)))

  val oDebug = IO(new Bundle {
    val pageFaultReq = Output(page_fault_req_i.cloneType)
  })

  oDebug.pageFaultReq := page_fault_req_i

  val messages = Seq(
    evict_done_req_i,
    evict_notify_req_i,
    page_fault_req_i
  ).map({ mess =>
    val res = Wire(Decoupled(new TxMessage))
    res.bits := mess.bits.getRawMessage
    res.valid := mess.valid
    mess.ready := res.ready
    res
  })

  val u_arb = Module(new Arbiter(new TxMessage, 3))
  
  u_arb.io.in.zip(messages).foreach({
    case (s, d) => s <> d
  })

  oq.valid := u_arb.io.out.valid
  u_arb.io.out.ready := oq.ready
  oq.bits := Cat(u_arb.io.out.bits.asVec.reverse)
  o <> Queue(oq, 2) // 1k register.
}

object QEMUMessageEncoderVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new QEMUMessageEncoder(new MemoryHierarchyParams)))
}
