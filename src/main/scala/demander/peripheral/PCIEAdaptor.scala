package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import DMAController.Bus._
import armflex.util.AXIRAMController
import armflex.demander.software_bundle.QEMUMessage

class QEMUMessageReceiver (
  trigger: (UInt) => Bool
) extends MultiIOModule {
  val S_AXI = IO(Flipped(new AXI4(64, 512)))
  val fifo_o = IO(Decoupled(UInt(512.W)))
  val u_axi_ram_controller = Module(new AXIRAMController(64, 512))

  u_axi_ram_controller.S_AXI <> S_AXI
  u_axi_ram_controller.read_reply_i := DontCare
  fifo_o.bits := u_axi_ram_controller.write_request_o.bits.data
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

