package armflex.demander.peripheral

import chisel3._
import chisel3.util._
import armflex.util.{
  AxiLiteSlave,
  AxiLiteConfig
}

import chisel3.experimental.BundleLiterals._

class ThreadLookupResultPacket(
  threadNumber: Int = 4
) extends Bundle {
  val thread_id = UInt(log2Ceil(threadNumber).W)
  val hit_v = Bool()

  override def cloneType: this.type = new ThreadLookupResultPacket(threadNumber).asInstanceOf[this.type]
}

class ThreadTable(
  threadNumber: Int = 4,
  processIDWidth: Int = 15,
  lookupPIDPortNumber: Int = 1,
  lookupTIDPortNumber: Int = 1,
) extends MultiIOModule {
  val S_AXI = IO(AxiLiteSlave(new AxiLiteConfig(32)))

  val table = RegInit(
    Vec(threadNumber, Valid(UInt(processIDWidth.W))),
    0.U.asTypeOf(Vec(threadNumber, Valid(UInt(processIDWidth.W))))
  )

  val axi_internal_read_address = S_AXI.araddr(1 + log2Ceil(threadNumber),2)
  val axi_internal_write_address = S_AXI.awaddr(1 + log2Ceil(threadNumber),2)

  val axi_read_addr_r = RegInit(
    Valid(UInt(log2Ceil(threadNumber).W)),
    0.U.asTypeOf(Valid(UInt(log2Ceil(threadNumber).W)))
  )
  
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

  val tid_i = IO(Input(Vec(lookupPIDPortNumber, UInt(log2Ceil(threadNumber).W))))
  val pid_o = IO(Output(Vec(lookupPIDPortNumber, Valid(UInt(processIDWidth.W)))))

  for(i <- 0 until lookupPIDPortNumber){
    pid_o(i) := table(tid_i(i))
  }

  val pid_i = IO(Input(Vec(lookupTIDPortNumber, UInt(processIDWidth.W))))
  val tid_o = IO(Output(Vec(lookupTIDPortNumber, new ThreadLookupResultPacket(threadNumber))))
  for(i <- 0 until lookupTIDPortNumber){
    val hit_oh = table.map({ term =>
      term.valid && term.bits === pid_i(i)
    })
    tid_o(i).hit_v := VecInit(hit_oh).asUInt() =/= 0.U
    tid_o(i).thread_id := OHToUInt(hit_oh.toSeq)
  }
}

object ThreadTableVerilogEmitter extends App {
  import chisel3.stage.ChiselStage
  val c = new ChiselStage
  println(c.emitVerilog(new ThreadTable()))
}
