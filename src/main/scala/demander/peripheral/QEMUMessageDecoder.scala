package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import antmicro.Bus._
import armflex.util.AXIRAMController
import armflex.demander.software_bundle.QEMUTxMessage
import armflex.demander.software_bundle.QEMUEvictReply
import armflex.demander.software_bundle.QEMUMissReply
import armflex.demander.software_bundle.QEMUMessagesType
import armflex.demander.software_bundle.QEMUPageEvictRequest
import armflex.demander.software_bundle.ParameterConstants

class QEMUMessageDecoder(
  fifoDepth: Int = 1
) extends MultiIOModule {
  val message_i = IO(Flipped(Decoupled(UInt(ParameterConstants.dram_data_width.W))))
  val qemu_miss_reply_o = IO(Decoupled(new QEMUMissReply))
  val qemu_evict_reply_o = IO(Decoupled(new QEMUEvictReply))
  val qemu_evict_page_req_o = IO(Decoupled(new QEMUPageEvictRequest))

  val raw_message = (new QEMUTxMessage).parseFromVec(VecInit(message_i.bits.asBools().grouped(32).map{x=> Cat(x.reverse)}.toSeq))
  
  val qemu_miss_reply_q = Wire(Decoupled(new QEMUMissReply))
  qemu_miss_reply_q.bits := qemu_miss_reply_q.bits.parseFromVec(raw_message.data)
  qemu_miss_reply_q.valid := raw_message.message_type === QEMUMessagesType.sMissReply && message_i.valid
  qemu_miss_reply_o <> Queue(qemu_miss_reply_q, fifoDepth)

  val qemu_evict_reply_q = Wire(Decoupled(new QEMUEvictReply))
  qemu_evict_reply_q.bits := qemu_evict_reply_q.bits.parseFromVec(raw_message.data)
  qemu_evict_reply_q.valid := raw_message.message_type === QEMUMessagesType.sEvictReply && message_i.valid
  qemu_evict_reply_o <> Queue(qemu_evict_reply_q, fifoDepth)

  val qemu_evict_page_req_q = Wire(Decoupled(new QEMUPageEvictRequest))
  qemu_evict_page_req_q.bits := qemu_evict_page_req_q.bits.parseFromVec(raw_message.data)
  qemu_evict_page_req_q.valid := raw_message.message_type === QEMUMessagesType.sPageEvict && message_i.valid
  qemu_evict_page_req_o <> Queue(qemu_evict_page_req_q, fifoDepth)

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
  println(c.emitVerilog(new QEMUMessageDecoder))
}