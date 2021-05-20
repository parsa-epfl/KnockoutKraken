package armflex

import chisel3._
import chisel3.util._
import antmicro.Bus.AXI4Lite._
import antmicro.CSR._

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
  val S_BUS = IO(Flipped(new CSRBusBundle(
    32, threadNumber
  )))

  val table = RegInit(
    Vec(threadNumber, Valid(UInt(processIDWidth.W))),
    0.U.asTypeOf(Vec(threadNumber, Valid(UInt(processIDWidth.W))))
  )

  // Read logic
  S_BUS.dataIn := RegNext(table(S_BUS.addr).bits)
  
  // Write logic
  when(S_BUS.write){
    table(S_BUS.addr).bits := S_BUS.dataOut
    table(S_BUS.addr).valid := true.B
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
