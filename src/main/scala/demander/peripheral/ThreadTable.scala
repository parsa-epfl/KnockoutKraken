package armflex.demander.peripheral

import chisel3._
import chisel3.util._
import armflex.util.{
  AxiLiteSlave,
  AxiLiteConfig
}

class ThreadTable(
  threadNumber: Int = 4,
  processIDWidth: Int = 16
) extends MultiIOModule {
  // TODO: Wrap the ThreadTable with: 1. AXI port 2. Request port from the mini-riscv
  val request_i = IO(Flipped(Valid(new MemoryRequestPacket(32, 32))))
  val reply_o = IO(Output(UInt(32.W)))

  val S_AXI = IO(AxiLiteSlave(new AxiLiteConfig(32)))

  val table = SyncReadMem(threadNumber, UInt(processIDWidth.W))

  val internal_address = request_i.bits.addr(1 + log2Ceil(threadNumber),2)
  reply_o := table(internal_address)

  S_AXI.arready := true.B
  S_AXI.awready := true.B

  val axi_internal_read_address = S_AXI.araddr(1 + log2Ceil(threadNumber),2)
  val axi_internal_write_address = S_AXI.awaddr(1 + log2Ceil(threadNumber),2)
  
  S_AXI.rdata := table(axi_internal_read_address)
  S_AXI.rresp := 0.U

  val rvalid_r = RegInit(false.B)
  S_AXI.rvalid := rvalid_r
  when(S_AXI.rvalid && S_AXI.rready){
    rvalid_r := false.B
  }.elsewhen(S_AXI.arvalid){
    rvalid_r := true.B
  }

  val axi_write_addr_r = Reg(Valid(UInt(log2Ceil(threadNumber).W)))
  when(S_AXI.awvalid){
    axi_write_addr_r.bits := true.B
    axi_write_addr_r.bits := axi_internal_write_address
  }.elsewhen(S_AXI.wvalid){
    table(axi_write_addr_r.bits) := S_AXI.wdata
  }

  S_AXI.awready := !axi_write_addr_r.valid
  S_AXI.wready := axi_write_addr_r.valid

  S_AXI.bresp := 0.U
  val bvalid_r = RegInit(false.B)
  S_AXI.bvalid := bvalid_r
  when(S_AXI.bready && S_AXI.bvalid){
    bvalid_r := false.B
  }.elsewhen(S_AXI.wvalid){
    bvalid_r := true.B
  }
}

object ThreadTableVerilogEmitter extends App {
  import chisel3.stage.ChiselStage
  val c = new ChiselStage
  println(c.emitVerilog(new ThreadTable()))
}
