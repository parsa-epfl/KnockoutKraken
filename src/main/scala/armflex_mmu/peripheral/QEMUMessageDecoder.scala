package armflex_mmu.peripheral

import chisel3._
import chisel3.util._
import armflex.{QEMUEvictReply, QEMUMessagesType, QEMUMissReply, QEMUPageEvictRequest, TxMessage}
import armflex_cache.PageTableParams
import armflex_mmu.MemoryHierarchyParams

class QEMUMessageDecoder(
  param: MemoryHierarchyParams,
  fifoDepth: Int = 1
) extends MultiIOModule {
  val message_i = IO(Flipped(Decoupled(UInt(param.dramdataW.W))))
  val qemu_miss_reply_o = IO(Decoupled(new QEMUMissReply(param.getPageTableParams)))
  val qemu_evict_reply_o = IO(Decoupled(new QEMUEvictReply(param.getPageTableParams)))
  val qemu_evict_page_req_o = IO(Decoupled(new QEMUPageEvictRequest(param.getPageTableParams)))

  val raw_message = (new TxMessage).parseFromVec(VecInit(message_i.bits.asBools().grouped(32).map{x=> Cat(x.reverse)}.toSeq))
  
  val qemu_miss_reply_q = Wire(Decoupled(new QEMUMissReply(param.getPageTableParams)))
  qemu_miss_reply_q.bits := qemu_miss_reply_q.bits.parseFromVec(raw_message.data)
  qemu_miss_reply_q.valid := raw_message.message_type === QEMUMessagesType.sMissReply && message_i.valid
  qemu_miss_reply_o <> Queue(qemu_miss_reply_q, fifoDepth)

  val qemu_evict_reply_q = Wire(Decoupled(new QEMUEvictReply(param.getPageTableParams)))
  qemu_evict_reply_q.bits := qemu_evict_reply_q.bits.parseFromVec(raw_message.data)
  qemu_evict_reply_q.valid := raw_message.message_type === QEMUMessagesType.sEvictReply && message_i.valid
  qemu_evict_reply_o <> Queue(qemu_evict_reply_q, fifoDepth)

  val qemu_evict_page_req_q = Wire(Decoupled(new QEMUPageEvictRequest(param.getPageTableParams)))
  qemu_evict_page_req_q.bits := qemu_evict_page_req_q.bits.parseFromVec(raw_message.data)
  qemu_evict_page_req_q.valid := raw_message.message_type === QEMUMessagesType.sPageEvict && message_i.valid
  qemu_evict_page_req_o <> Queue(qemu_evict_page_req_q, fifoDepth)

  // FIXME: RAW Hazard here. If qemu_miss (which resolves page fault) and qemu_evict_page (which evicts a page) pointing to the same page, we're done. The minimal latency between these two operations should be around 100 cycles!

  // Why default true here? Unrelated messages will be dropped directly.
  message_i.ready := MuxLookup(raw_message.message_type, true.B, Seq(
    QEMUMessagesType.sMissReply -> qemu_miss_reply_q.ready,
    QEMUMessagesType.sEvictReply -> qemu_evict_reply_q.ready,
    QEMUMessagesType.sPageEvict -> qemu_evict_page_req_q.ready
  ))

  when(message_i.valid && !(qemu_miss_reply_q.valid || qemu_evict_reply_q.valid || qemu_evict_page_req_q.valid)){
    printf("Warning[QEMUMessageDecoder]: A message has been dropped due to its uncleared type.\n")
  }
}

object PCIEAdaptorVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new QEMUMessageDecoder(new MemoryHierarchyParams())))
}