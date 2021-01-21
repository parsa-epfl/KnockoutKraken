package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import DMAController.Bus.AXI4

/**
 * Define the addres range for the PCI-Express.
 * 
 * At present, four channels are used.
 */ 
case class PCIEAddressSpaceParameter(
  iPageStart: Int, // Page from host to FPGA
  oPageStart: Int, // Page from FPGA to Host
  iMessStart: Int, // Message from host to FPGA
  oMessStart: Int, // Message from FPGA to host
)

// TODO: Determine the way to notify the terminal for each channel.
/**
 * For page: we will use the buffer in the host (4k) to store the page. (PCI-E's AXI Slave)
 * For message: An AXI controlled FIFO to read / write message. (PCI-E's AXI Master)
 * But how to notify the host when there is a message available? (interrupt maybe finally)
 */ 

/**
 * Access FIFOs (Rx and Tx) using the AXI protocols.
 * 
 * @param messageType the type of the message. The width of this message should be less than 511 bit.
 * @param baseAddr the activate address of this controller. 
 * 
 * @note the AXI burst request is accepted.
 * 
 * TODO: Add methods to notify the host that there is a message available.
 */ 
class AXIFIFOController[T <: Data](
  messageType: T,
  baseAddr: Long = 0x0
) extends MultiIOModule {
  val S_AXI = IO(Flipped(new AXI4(64, 512)))

  assert(messageType.getWidth <= 512)

  val fifo_o = IO(Decoupled(messageType.cloneType))
  val fifo_i = IO(Flipped(Decoupled(messageType.cloneType)))

  val sIdle :: sWorking :: Nil = Enum(2)

  class transaction_context_t extends Bundle {
    val id = S_AXI.ar.arid.cloneType
    val length = S_AXI.ar.arlen.cloneType
    val cnt = S_AXI.ar.arlen.cloneType
    val state = sIdle.cloneType
  }

  // Read context registers.
  val read_context_r = Reg(new transaction_context_t)
  read_context_r.state := sIdle // initial value

  S_AXI.r.rdata := fifo_i.bits.asUInt()
  S_AXI.r.rvalid := fifo_i.valid && read_context_r.state === sWorking
  S_AXI.r.rresp := 0.U
  S_AXI.r.rid := read_context_r.id
  S_AXI.r.rlast := read_context_r.length === read_context_r.cnt
  fifo_i.ready := S_AXI.r.rready && read_context_r.state === sWorking

  S_AXI.ar.arready := read_context_r.state === sIdle

  when(S_AXI.ar.arvalid && S_AXI.ar.arready && S_AXI.ar.araddr === baseAddr.U){
    read_context_r.state := sWorking
    read_context_r.cnt := 0.U
    read_context_r.id := S_AXI.ar.arid
    read_context_r.length := S_AXI.ar.arlen
  }.elsewhen(S_AXI.r.rvalid && S_AXI.r.rready){
    // transaction occurs.
    read_context_r.cnt := read_context_r.cnt + 1.U
    read_context_r.state := Mux(S_AXI.r.rlast, sIdle, sWorking)
  }

  // Write state register.
  val write_state_r = RegInit(sIdle)
  when(S_AXI.aw.awvalid && S_AXI.aw.awready && S_AXI.aw.awaddr === baseAddr.U){
    write_state_r := sWorking
  }.elsewhen(S_AXI.b.bvalid && S_AXI.b.bready){
    write_state_r := sIdle
  }
  
  // Write response
  val write_response_vr = RegInit(false.B)
  when(S_AXI.b.bvalid && S_AXI.b.bready){
    write_response_vr := false.B
  }.elsewhen(S_AXI.w.wvalid && S_AXI.w.wready && S_AXI.w.wlast){
    write_response_vr := true.B
  }

  fifo_o.bits := S_AXI.w.wdata
  fifo_o.valid := S_AXI.w.wvalid && write_state_r === sWorking
  S_AXI.w.wready := fifo_o.ready && write_state_r === sWorking
}

// TODO: Define the message body for all messages related to the QEMU.
object QEMUMessages {
  val sMissPacket :: sEvictNotify :: sEvictRequest :: Nil = Enum(3)

  

  class MissRequestPacket extends SoftwareControlledBundle {
    val vpn = UInt(52.W)
    val process_id = UInt(16.W)
    def asVec(width: Int): Vec[UInt] = {
      return VecInit(Seq(
        vpn(31, 0),
        vpn(51, 32),
        process_id
      ))
    }

    def parseFromVec(f: Vec[UInt]): this.type = {
      val res = Wire(this.cloneType)
      res.vpn := Cat(f(1), f(0))
      res.process_id := f(2)
      return res.asInstanceOf[this.type]
    }
  }

  // EvictRequestPacket is just a PTE.
}



