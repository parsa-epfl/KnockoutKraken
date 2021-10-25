package armflex

import chisel3._
import chisel3.util._
import antmicro.CSR._

class CSR_thid2asid(
  thidN: Int = 4,
  asidW: Int = 15,
  thid2asidPortsN: Int = 1
) extends MultiIOModule {
  val bus = IO(Flipped(new CSRBusBundle(32, thidN)))

  private val table = RegInit(Vec(thidN, Valid(UInt(asidW.W))), 0.U.asTypeOf(Vec(thidN, Valid(UInt(asidW.W)))))

  // Read logic
  bus.dataIn := RegNext(table(bus.addr).bits)

  // Write logic
  when(bus.write) {
    table(bus.addr).bits := bus.dataOut
    table(bus.addr).valid := true.B
    when(bus.dataOut.asSInt === -1.S) {
      table(bus.addr).valid := false.B
    }
  }

  val thid_i = IO(Input(Vec(thid2asidPortsN, UInt(log2Ceil(thidN).W))))
  val asid_o = IO(Output(Vec(thid2asidPortsN, Valid(UInt(asidW.W)))))

  for(i <- 0 until thid2asidPortsN){
    asid_o(i) := table(thid_i(i))
  }
}

object ThreadTableVerilogEmitter extends App {
  import chisel3.stage.ChiselStage
  val c = new ChiselStage
  println(c.emitVerilog(new CSR_thid2asid()))
}
