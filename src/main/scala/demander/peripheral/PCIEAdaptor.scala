package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import DMAController.Bus._
import armflex.util.AXIRAMController
import armflex.demander.software_bundle.QEMUMessage
import armflex.demander.software_bundle.PageDemanderMessage
import armflex.demander.software_bundle.QEMUEvictReply
import armflex.demander.software_bundle.QEMUMissReply
import armflex.demander.software_bundle.PageDemanderMessageType

class QEMUMessageReceiver (
  trigger: (UInt) => Bool
) extends MultiIOModule {
  val S_AXI = IO(Flipped(new AXI4(64, 512)))
  val fifo_o = IO(Decoupled(new PageDemanderMessage))
  val u_axi_ram_controller = Module(new AXIRAMController(64, 512))

  u_axi_ram_controller.S_AXI <> S_AXI
  u_axi_ram_controller.read_request_o.ready := false.B
  u_axi_ram_controller.read_reply_i := DontCare
  fifo_o.bits := fifo_o.bits.parseFromVec(VecInit(u_axi_ram_controller.write_request_o.bits.data.asBools().grouped(32).map(Cat(_)).toSeq))

  fifo_o.valid := u_axi_ram_controller.write_request_o.valid && trigger(u_axi_ram_controller.write_request_o.bits.addr)
  u_axi_ram_controller.write_request_o.ready := fifo_o.ready
}

class QEMUMessageSender (
  baseAddr: UInt = 0x0.U,
  fifoRange: Int = 16,
) extends MultiIOModule {
  val M_AXI = IO(new AXI4(64, 512))
  val fifo_i = IO(Flipped(Decoupled(new QEMUMessage)))

  val fifo_q = Queue(fifo_i, 2) 
  
  val u_dma_write = Module(new DMAController.Frontend.AXI4Writer(64, 512))

  u_dma_write.io.dataIn.bits <> Cat(Cat(fifo_i.bits.asVec(32)), 1.U(32.W))
  u_dma_write.io.dataIn.valid := fifo_q.valid
  u_dma_write.io.bus <> M_AXI
  u_dma_write.io.xfer.address := baseAddr
  u_dma_write.io.xfer.length := 1.U
  u_dma_write.io.xfer.valid := fifo_q.valid

  fifo_q.ready := u_dma_write.io.xfer.done
}

class QEMUMessageConverter(
  fifoDepth: Int = 2
) extends MultiIOModule {
  val i = IO(Flipped(Decoupled(new PageDemanderMessage)))

  val miss_reply_o = IO(Decoupled(new QEMUMissReply))
  val evict_reply_o = IO(Decoupled(new QEMUEvictReply))

  val miss_reply_enq = Wire(Decoupled(new QEMUMissReply))
  miss_reply_enq.valid := i.valid && i.bits.message_type === PageDemanderMessageType.sQEMUMissReply
  miss_reply_enq.bits := miss_reply_enq.bits.parseFromVec(i.bits.data)

  val evict_reply_enq = Wire(Decoupled(new QEMUEvictReply))
  evict_reply_enq.valid := i.valid && i.bits.message_type === PageDemanderMessageType.sQEMUEvictReply
  evict_reply_enq.bits := evict_reply_enq.bits.parseFromVec(i.bits.data)

  miss_reply_o <> Queue(miss_reply_enq, fifoDepth)
  evict_reply_o <> Queue(evict_reply_enq, fifoDepth)

  i.ready := miss_reply_enq.fire() || evict_reply_enq.fire()
}