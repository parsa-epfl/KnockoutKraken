/** Credits to my friend Laurier Loiselle
  *  source: https://github.com/laurierloi/fpgabits/
  */

package common

import scala.language.reflectiveCalls
import chisel3._
import chisel3.util._
import chisel3.experimental._

case class AxiLiteConfig(val addrWidth: Int)

object AxiLite {

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

  def apply(cfg: AxiLiteConfig): AxiLiteSignals = new AxiLiteIO(cfg)
}

abstract class AxiLiteSignals(val cfg: AxiLiteConfig) extends Bundle{
  import common.AxiLite._

  // Write Address
  val awaddr  =  Input(UInt(cfg.addrWidth.W))
  val awprot  =  Input(UInt(protWidth.W))
  val awvalid =  Input(Bool())
  val awready = Output(Bool())
  // Write Data
  val wdata  =  Input(UInt(dataWidth.W))
  val wstrb  =  Input(UInt(strobeWidth.W))
  val wvalid =  Input(Bool())
  val wready = Output(Bool())
  // Write Response
  val bresp  = Output(RESP_T)
  val bvalid = Output(Bool())
  val bready =  Input(Bool())
  // Read Address
  val araddr  =  Input(UInt(cfg.addrWidth.W))
  val arprot  =  Input(UInt(protWidth.W))
  val arvalid =  Input(Bool())
  val arready = Output(Bool())
  // Read Data
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

object AxiLiteSignals {
  import AxiLite.ports._

  implicit class AddMethodsToAxiLite(target: AxiLiteSignals){
    private val cfg = target.cfg

    def getDecoupledPort(port: Int): DecoupledIO[UInt] = {
      val width = port match{
        case PortWrAddr => cfg.addrWidth
        case PortWrData => AxiLite.dataWidth
        case PortWrResp => AxiLite.respWidth
        case PortRdAddr => cfg.addrWidth
        case PortRdData => AxiLite.dataWidth
        case _ => println("Invalid Port"); 0
      }

      val bits  = Wire(UInt())
      val valid = Wire(Bool())
      val ready = Wire(Bool())
      port match{
        case PortWrAddr =>
          bits  <> target.awaddr
          valid <> target.awvalid
          ready <> target.awready
        case PortWrData =>
          bits  <> target.wdata
          valid <> target.wvalid
          ready <> target.wready
        case PortWrResp =>
          bits  <> target.bresp
          valid <> target.bvalid
          ready <> target.bready
        case PortRdAddr =>
          bits  <> target.araddr
          valid <> target.arvalid
          ready <> target.arready
        case PortRdData =>
          bits  <> target.rdata
          valid <> target.rvalid
          ready <> target.rready
      }

      val dec = Wire(DecoupledIO(UInt(width.W)))

      // Is an enq port
      if (port == PortWrAddr |
          port == PortWrData |
          port == PortRdAddr ){
        ready     := dec.ready
        dec.valid := valid
        dec.bits  := bits
      } else { // Is a  deq port
        dec.ready := ready
        valid     := dec.valid
        bits      := dec.bits
      }
      dec
    }

    def getWrDataEnq: DecoupledIO[UInt] = getDecoupledPort(PortWrData)
    def getWrAddrEnq: DecoupledIO[UInt] = getDecoupledPort(PortWrAddr)
    def getWrRespDeq: DecoupledIO[UInt] = getDecoupledPort(PortWrResp)
    def getRdAddrEnq: DecoupledIO[UInt] = getDecoupledPort(PortRdAddr)
    def getRdDataDeq: DecoupledIO[UInt] = getDecoupledPort(PortRdData)
  }
}

class AxiLiteIO(c: AxiLiteConfig) extends AxiLiteSignals(c){
  override def cloneType: this.type = new AxiLiteIO(c).asInstanceOf[this.type]
}

object AxiLiteSlave {
  def apply(cfg: AxiLiteConfig): AxiLiteSignals = AxiLite(cfg)
}

object AxiLiteMaster{
  def apply(cfg: AxiLiteConfig): AxiLiteSignals = Flipped(AxiLite(cfg))
}
