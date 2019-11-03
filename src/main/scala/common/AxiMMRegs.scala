/** Credits to my friend Laurier Loiselle
  *  source: https://github.com/laurierloi/fpgabits/
  */

package common

import scala.language.reflectiveCalls
import chisel3._
import chisel3.util.{log2Up}
import common.AxiLite._

case class AxiMemoryMappedRegFileConfig(
  val nbrReg: Int,
  val readOnly: Seq[Boolean],
  val pulseMode: Seq[Boolean]
){
  assert(readOnly.size == nbrReg)
  assert(pulseMode.size == nbrReg)

  val addrWidth = log2Up(nbrReg) + 2 // 2 extra bits for the byte level adressing
  val axiLiteConfig = new AxiLiteConfig(addrWidth)
}

class AxiMemoryMappedRegFile(implicit val cfg: AxiMemoryMappedRegFileConfig) extends Module {
  import common.AxiLiteConsts._
  val io = IO(new Bundle {
                val axiLite = AxiLiteSlave(cfg.axiLiteConfig)
                val regsInput = Input(Vec(cfg.nbrReg, UInt(dataWidth.W)))
                val regsOutput = Output(Vec(cfg.nbrReg, UInt(dataWidth.W)))
              })
  val regFile = RegInit(VecInit(Seq.fill(cfg.nbrReg)(0.U(dataWidth.W))))
  val regsOutput = VecInit(Seq.fill(cfg.nbrReg)(0.U(dataWidth.W)))
  val regsInput = io.regsInput
  val axiLite_awaddr = io.axiLite.awaddr >> 2.U // 32Bit words addressed
  val axiLite_araddr = io.axiLite.araddr >> 2.U

  // Write Transaction, always ready
  io.axiLite.awready := true.B
  io.axiLite.wready := true.B

  def getByte(bits:UInt, idx:Int) = { bits((idx+1)*8-1, idx*8) }
  val byteVecReg = Wire(Vec(4, UInt(8.W)))
  val byteVecIn  = Wire(Vec(4, UInt(8.W)))
  val byteVecOut = Wire(Vec(4, UInt(8.W)))
  for (i <- 0 until 4) {
    byteVecReg(i) := getByte(regFile(axiLite_awaddr), i)
    byteVecIn(i) := getByte(io.axiLite.wdata, i)
    byteVecOut(i) := Mux(io.axiLite.wstrb(i), byteVecIn(i), byteVecReg(i))
  }

  when(io.axiLite.awvalid && io.axiLite.wvalid) {
   regFile(axiLite_awaddr) := byteVecOut.asUInt
  }
  io.axiLite.bresp := RegNext(Mux((io.axiLite.wvalid && !io.axiLite.awvalid), rSLVERR, rOKAY))
  io.axiLite.bvalid := RegNext(io.axiLite.awvalid && io.axiLite.wvalid)

  for (i <- 0 until cfg.nbrReg) {
    val wrValid = io.axiLite.awvalid && io.axiLite.wvalid && axiLite_awaddr === i.U
    if(cfg.pulseMode(i)) {
      // Pulse registers
      regsOutput(i) := RegNext(Mux(wrValid, io.axiLite.wdata, 0.U))

    } else if (cfg.readOnly(i)) {
      // Read only registers
      regsOutput(i) := regsInput(i)
      when(wrValid) { io.axiLite.bresp := RegNext(rSLVERR) }

    } else if (!cfg.pulseMode(i) && !cfg.readOnly(i)){
      // Read Write registers
      regsOutput(i) := regFile(i)

    }
  }

  // Read
  io.axiLite.arready := true.B
  io.axiLite.rdata := regsOutput(RegNext(axiLite_araddr))
  io.axiLite.rvalid := RegNext(io.axiLite.arvalid && io.axiLite.rready)
  io.axiLite.rresp := RegNext(rOKAY)

  io.regsOutput := regsOutput
}
