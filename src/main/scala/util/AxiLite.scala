/** Credits to my friend Laurier Loiselle
  *  source: https://github.com/laurierloi/fpgabits/
  */
package armflex.util

import scala.language.reflectiveCalls
import chisel3._
import chisel3.util._
import chisel3.experimental._

case class AxiLiteConfig(val addrWidth: Int)

object AxiLiteConsts {

  val respWidth = log2Ceil(4)
  val Seq(rOKAY, rEXOKAY, rSLVERR, rDECERR) = Enum(4)
  def RESP_T = rOKAY.cloneType

  val dataWidth = 32
  val protWidth = 3
  val strobeWidth = dataWidth/8

  object ports {
    private val Seq(portWA, portWD, portWR, portRA, portRD) = (0 until 5)
    val PortWrAddr = portWA
    val PortWrData = portWD
    val PortWrResp = portWR
    val PortRdAddr = portRA
    val PortRdData = portRD
  }
}

abstract class AxiLiteSignals(val cfg: AxiLiteConfig) extends Bundle {
  import AxiLiteConsts._

  // Write Address  - Flipped(Decoupled)
  val awaddr  =  Input(UInt(cfg.addrWidth.W))
  val awprot  =  Input(UInt(protWidth.W))
  val awvalid =  Input(Bool())
  val awready = Output(Bool())
  // Write Data     - Flipped(Decoupled)
  val wdata  =  Input(UInt(dataWidth.W))
  val wstrb  =  Input(UInt(strobeWidth.W))
  val wvalid =  Input(Bool())
  val wready = Output(Bool())
  // Write Response - Decoupled
  val bresp  = Output(RESP_T)
  val bvalid = Output(Bool())
  val bready =  Input(Bool())
  // Read Address   - Flipped(Decoupled)
  val araddr  =  Input(UInt(cfg.addrWidth.W))
  val arprot  =  Input(UInt(protWidth.W))
  val arvalid =  Input(Bool())
  val arready = Output(Bool())
  // Read Data      - Decoupled
  val rdata  = Output(UInt(dataWidth.W))
  val rresp  = Output(RESP_T)
  val rvalid = Output(Bool())
  val rready =  Input(Bool())

  override def toPrintable: Printable = {
    s"""AxiLiteSignals
  Write Address:
    awaddr   0x${Hexadecimal(awaddr)}
    awprot   ${Binary(awprot)}
    awvalid  ${Binary(awvalid)}
    awready  ${Binary(awready)}
  Write Data
    wdata    0x${Hexadecimal(wdata)}
    wstrb    ${Binary(wstrb)}
    wvalid   ${Binary(wvalid)}
    wready   ${Binary(wready)}
  Write Response
    bresp    ${Binary(bresp)}
    bvalid   ${Binary(bvalid)}
    bready   ${Binary(bready)}
  Read Address
    araddr   0x${Hexadecimal(araddr)}
    arprot   ${Binary(arprot)}
    arvalid  ${Binary(arvalid)}
    arready  ${Binary(arready)}
  Read Data
    rdata    0x${Hexadecimal(rdata)}
    rresp    ${Binary(rresp)}
    rvalid   ${Binary(rvalid)}
    rready   ${Binary(rready)}
  """
  }
}

private class AxiLiteIO(c: AxiLiteConfig) extends AxiLiteSignals(c) {
  override def cloneType: this.type = new AxiLiteIO(c).asInstanceOf[this.type]
}

object AxiLiteSlave { def apply(cfg: AxiLiteConfig): AxiLiteSignals = new AxiLiteIO(cfg) }
object AxiLiteMaster{ def apply(cfg: AxiLiteConfig): AxiLiteSignals = Flipped(new AxiLiteIO(cfg)) }
