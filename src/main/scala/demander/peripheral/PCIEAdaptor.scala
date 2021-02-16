package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import DMAController.Bus._
import armflex.util.AXIRAMController
import armflex.demander.software_bundle.QEMUTxMessage
import armflex.demander.software_bundle.QEMUEvictReply
import armflex.demander.software_bundle.QEMUMissReply
import armflex.demander.software_bundle.QEMUMessagesType
import armflex.demander.software_bundle.QEMUPageEvictRequest

class QEMUMessageReceiver (
  trigger: (UInt) => Bool
) extends MultiIOModule {
  val S_AXI = IO(Flipped(new AXI4(64, 512)))
  val qemu_miss_reply_o = IO(Decoupled(new QEMUMissReply))
  val qemu_evict_reply_o = IO(Decoupled(new QEMUEvictReply))
  val qemu_evict_page_req_o = IO(Decoupled(new QEMUPageEvictRequest))
  val u_axi_ram_controller = Module(new AXIRAMController(64, 512))

  u_axi_ram_controller.S_AXI <> S_AXI
  u_axi_ram_controller.read_request_o.ready := false.B
  u_axi_ram_controller.read_reply_i := DontCare

  val raw_message = (new QEMUTxMessage).parseFromVec(VecInit(u_axi_ram_controller.write_request_o.bits.data.asBools().grouped(32).map{x=> Cat(x.reverse)}.toSeq))
  
  // fifo_o.valid := u_axi_ram_controller.write_request_o.valid && trigger(u_axi_ram_controller.write_request_o.bits.addr)
  
  // u_axi_ram_controller.write_request_o.ready := fifo_o.ready

  qemu_miss_reply_o.bits := qemu_miss_reply_o.bits.parseFromVec(raw_message.data)
  qemu_miss_reply_o.valid := raw_message.message_type === QEMUMessagesType.sMissReply && u_axi_ram_controller.write_request_o.valid && trigger(u_axi_ram_controller.write_request_o.bits.addr)

  qemu_evict_reply_o.bits := qemu_evict_reply_o.bits.parseFromVec(raw_message.data)
  qemu_evict_reply_o.valid := raw_message.message_type === QEMUMessagesType.sEvictReply && u_axi_ram_controller.write_request_o.valid && trigger(u_axi_ram_controller.write_request_o.bits.addr)

  qemu_evict_page_req_o.bits := qemu_evict_page_req_o.bits.parseFromVec(raw_message.data)
  qemu_evict_page_req_o.valid := raw_message.message_type === QEMUMessagesType.sPageEvict && u_axi_ram_controller.write_request_o.valid && trigger(u_axi_ram_controller.write_request_o.bits.addr)

  u_axi_ram_controller.write_request_o.ready := MuxLookup(raw_message.message_type, false.B, Seq(
    QEMUMessagesType.sMissReply -> qemu_miss_reply_o.ready,
    QEMUMessagesType.sEvictReply -> qemu_evict_reply_o.ready,
    QEMUMessagesType.sPageEvict -> qemu_evict_page_req_o.ready
  )) && trigger(u_axi_ram_controller.write_request_o.bits.addr)

}

/**
 * This module sends message to a software FIFO implemented as a range of the host memory.
 * 
 * @param baseAddr the basic address of the FIFO. At present it's not used since we don't know the memory layout of the QEMU.
 * @param fifoRange the size of the FIFO. This will determine the size of the hardware FIFO pointer.
 * 
 * TODO: Mention this part in the semester project
 */ 
class QEMUMessageSender (
  baseAddr: UInt = 0x0.U, // TODO: Reference this parameter in the module
  fifoRange: Int = 16,
) extends MultiIOModule {
  val M_AXI = IO(new AXI4(64, 512))
  val fifo_i = IO(Flipped(Decoupled(new QEMUTxMessage)))

  val fifo_q = Queue(fifo_i, 2)

  val addr_cnt_r = RegInit(0.U(log2Ceil(fifoRange).W))
  
  val u_dma_write = Module(new DMAController.Frontend.AXI4Writer(64, 512))
  // Cat(Seq(1.U(4.W), 2.U(4.W), 3.U(4.W), 4.U(w.$))) -> 0x1234. Thus we need reverse the result
  u_dma_write.io.dataIn.bits := Cat(Cat(fifo_q.bits.asVec(32).reverse), 1.U(32.W))
  u_dma_write.io.dataIn.valid := fifo_q.valid
  u_dma_write.io.bus <> M_AXI
  u_dma_write.io.xfer.address := Cat(addr_cnt_r, 0.U(6.W))
  u_dma_write.io.xfer.length := 1.U
  u_dma_write.io.xfer.valid := fifo_q.valid

  fifo_q.ready := u_dma_write.io.xfer.done

  when(fifo_q.fire()){
    addr_cnt_r := addr_cnt_r + 1.U
  }
}

class QEMUMessageConverter(
  fifoDepth: Int = 2
) extends MultiIOModule {
  val i = IO(Flipped(Decoupled(new QEMUTxMessage)))

  val miss_reply_o = IO(Decoupled(new QEMUMissReply))
  val evict_reply_o = IO(Decoupled(new QEMUEvictReply))

  val miss_reply_enq = Wire(Decoupled(new QEMUMissReply))
  miss_reply_enq.valid := i.valid && i.bits.message_type === QEMUMessagesType.sMissReply
  miss_reply_enq.bits := miss_reply_enq.bits.parseFromVec(i.bits.data)

  val evict_reply_enq = Wire(Decoupled(new QEMUEvictReply))
  evict_reply_enq.valid := i.valid && i.bits.message_type === QEMUMessagesType.sEvictReply
  evict_reply_enq.bits := evict_reply_enq.bits.parseFromVec(i.bits.data)

  miss_reply_o <> Queue(miss_reply_enq, fifoDepth)
  evict_reply_o <> Queue(evict_reply_enq, fifoDepth)

  i.ready := miss_reply_enq.fire() || evict_reply_enq.fire()
}  


object PCIEAdaptorVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new QEMUMessageReceiver((x: UInt) => true.B)))
  println(c.emitVerilog(new QEMUMessageSender))
  println(c.emitVerilog(new QEMUMessageConverter))
}