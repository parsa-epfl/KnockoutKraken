package armflex.demander.peripheral

import chisel3._
import chisel3.util._
import armflex.util.{
  AxiLiteSlave,
  AxiLiteConfig
}

class ThreadLookupResultPacket(
  threadNumber: Int = 4
) extends Bundle {
  val thread_id = UInt(log2Ceil(threadNumber).W)
  val hit_v = Bool()

  override def cloneType: this.type = new ThreadLookupResultPacket(threadNumber).asInstanceOf[this.type]
}

class ThreadTable(
  threadNumber: Int = 4,
  processIDWidth: Int = 15
) extends MultiIOModule {
  // TODO: Wrap the ThreadTable with: 1. AXI port 2. Request port from the mini-riscv
  val request_i = IO(Flipped(Valid(new MemoryRequestPacket(32, 32))))
  val reply_o = IO(Output(UInt(32.W)))

  val S_AXI = IO(AxiLiteSlave(new AxiLiteConfig(32)))

  val table = Reg(Vec(threadNumber, Valid(UInt(processIDWidth.W)))) // SyncReadMem(threadNumber, UInt(processIDWidth.W))

  val internal_address = request_i.bits.addr(1 + log2Ceil(threadNumber),2)
  reply_o := RegNext(table(internal_address).bits)

  val axi_internal_read_address = S_AXI.araddr(1 + log2Ceil(threadNumber),2)
  val axi_internal_write_address = S_AXI.awaddr(1 + log2Ceil(threadNumber),2)

  val axi_read_addr_r = Reg(Valid(UInt(log2Ceil(threadNumber).W)))
  when(S_AXI.rready && S_AXI.rvalid){
    axi_read_addr_r.valid := false.B
  }.elsewhen(S_AXI.arready && S_AXI.arvalid){
    axi_read_addr_r.valid := true.B
    axi_read_addr_r.bits := axi_internal_read_address
  }

  S_AXI.arready := !axi_read_addr_r.valid

  S_AXI.rvalid := axi_read_addr_r.valid
  S_AXI.rdata := table(axi_read_addr_r.bits).bits
  S_AXI.rresp := 0.U

  val axi_write_addr_r = Reg(Valid(UInt(log2Ceil(threadNumber).W)))
  axi_write_addr_r.valid := false.B
  
  when(S_AXI.wvalid && S_AXI.wready){
    table(axi_write_addr_r.bits).bits := S_AXI.wdata
    table(axi_write_addr_r.bits).valid := true.B
    axi_write_addr_r.valid := false.B
  }.elsewhen(S_AXI.awvalid && S_AXI.awready){
    axi_write_addr_r.valid := true.B
    axi_write_addr_r.bits := axi_internal_write_address
  }

  S_AXI.awready := !axi_write_addr_r.valid
  S_AXI.wready := axi_write_addr_r.valid

  S_AXI.bresp := 0.U
  val bvalid_r = RegInit(false.B)
  S_AXI.bvalid := bvalid_r
  when(S_AXI.bready && S_AXI.bvalid){
    bvalid_r := false.B
  }.elsewhen(S_AXI.wvalid && S_AXI.wready){
    bvalid_r := true.B
  }

  val lookup_request_i = IO(Input(UInt(processIDWidth.W)))
  val hit_oh = table.map({ term =>
    term.valid && term.bits === lookup_request_i
  })
  val loopup_reply_o = IO(Output(new ThreadLookupResultPacket(threadNumber)))
  loopup_reply_o.hit_v := VecInit(hit_oh).asUInt() =/= 0.U
  loopup_reply_o.thread_id := Mux1H(hit_oh, Seq.tabulate(threadNumber)(_.U))
}

object ThreadTableVerilogEmitter extends App {
  import chisel3.stage.ChiselStage
  val c = new ChiselStage
  println(c.emitVerilog(new ThreadTable()))
}
