/** Credits to my friend Laurier Loiselle
  *  source: https://github.com/laurierloi/fpgabits/
  */

package common

import scala.language.reflectiveCalls
import chisel3._
import chisel3.util._
import chisel3.experimental._

case class AxiLiteConfig(
  val addrWidth: Int,
  val dataWidth: Int = 32
){
  val strobeWidth: Int = dataWidth/8
  val protWidth = AxiLiteConfig.protWidth
  val respWidth = AxiLiteConfig.respWidth
}

object AxiLiteConfig{
  val protWidth:   Int = 3
  val respWidth:   Int = 2
  val PortWriteAddress  = 0
  val PortWriteData     = 1
  val PortWriteResponse = 2
  val PortReadAddress   = 3
  val PortReadData      = 4
  def apply(addrWidth: Int, dataWidth: Int):AxiLiteConfig = AxiLiteConfig(addrWidth, dataWidth)
}

abstract class AxiLiteSignals(val c: AxiLiteConfig) extends Bundle{
  /* write address */
  val awaddr     = Input  (UInt(c.addrWidth.W))
  val awprot     = Input  (UInt(c.protWidth.W))
  val awvalid    = Input  (Bool ())
  val awready    = Output (Bool ())
  /* write data */
  val wdata      = Input  (UInt(c.dataWidth.W))
  val wstrb      = Input  (UInt(c.strobeWidth.W))
  val wvalid     = Input  (Bool ())
  val wready     = Output (Bool ())
  /* write response */
  val bresp      = Output (UInt(c.respWidth.W))
  val bvalid     = Output (Bool ())
  val bready     = Input  (Bool ())
  /* read address */
  val araddr     = Input  (UInt(c.addrWidth.W))
  val arprot     = Input  (UInt(c.protWidth.W))
  val arvalid    = Input  (Bool ())
  val arready    = Output (Bool ())
  /* read data */
  val rdata      = Output (UInt(c.dataWidth.W))
  val rresp      = Output (UInt(c.respWidth.W))
  val rvalid     = Output (Bool ())
  val rready     = Input  (Bool ())

  override def toPrintable: Printable = {
  p"AxiLiteSignals"     +
  p"\n  Write Address:  "  +
  p"awaddr: "             + p"0x${Hexadecimal(awaddr)}"  +
  p", awprot: "           + p"${Binary(awprot)}"  +
  p", awvalid: "          + p"${Binary(awvalid)}" +
  p", awready: "          + p"${Binary(awready)}" +
  p"\n  Write Data:     "     +
  p"wdata: "            + p"0x${Hexadecimal(wdata)}"   +
  p", wstrb: "          + p"${Binary(wstrb)}"   +
  p", wvalid: "         + p"${Binary(wvalid)}"  +
  p", wready: "         + p"${Binary(wready)}"  +
  p"\n  Write Response: " +
  p"bresp: "            + p"${Binary(bresp)}"   +
  p", bvalid: "         + p"${Binary(bvalid)}"  +
  p", bready: "         + p"${Binary(bready)}"  +
  p"\n  Read Address:   "   +
  p"araddr: "           + p"0x${Hexadecimal(araddr)}"  +
  p", arprot: "         + p"${Binary(arprot)}"  +
  p", arvalid: "        + p"${Binary(arvalid)}" +
  p", arready: "        + p"${Binary(arready)}" +
  p"\n  Read Data:      "      +
  p"rdata: "            + p"0x${Hexadecimal(rdata)}"   +
  p", rresp: "          + p"${Binary(rresp)}"   +
  p", rvalid: "         + p"${Binary(rvalid)}"  +
  p", rready: "         + p"${Binary(rready)}"  +
  p"\n"
  }
}

object AxiLiteSignals {
  implicit class AddMethodsToAxiLite(target: AxiLiteSignals){
    private val c = target.c

    def getDecoupledPort(port: Int): DecoupledIO[UInt] = {
      val width = port match{
        case AxiLiteConfig.PortWriteAddress  => c.addrWidth
        case AxiLiteConfig.PortWriteData     => c.dataWidth
        case AxiLiteConfig.PortWriteResponse => c.respWidth
        case AxiLiteConfig.PortReadAddress   => c.addrWidth
        case _                   => c.dataWidth //c.PortReadData
      }

      val bits  = Wire(UInt())
      val valid = Wire(Bool())
      val ready = Wire(Bool())
      port match{
        case AxiLiteConfig.PortWriteAddress  =>
          bits  <> target.awaddr
          valid <> target.awvalid
          ready <> target.awready
        case AxiLiteConfig.PortWriteData     =>
          bits  <> target.wdata
          valid <> target.wvalid
          ready <> target.wready
        case AxiLiteConfig.PortWriteResponse =>
          bits  <> target.bresp
          valid <> target.bvalid
          ready <> target.bready
        case AxiLiteConfig.PortReadAddress   =>
          bits  <> target.araddr
          valid <> target.arvalid
          ready <> target.arready
        case _                   => //AxiLiteConfig.PortReadData
          bits  <> target.rdata
          valid <> target.rvalid
          ready <> target.rready
      }

      val dec = Wire(DecoupledIO(UInt(width.W)))

      // Is an enq port
      if (port == AxiLiteConfig.PortWriteAddress |
          port == AxiLiteConfig.PortWriteData    |
          port == AxiLiteConfig.PortReadAddress){
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

    def getAddressWriteEnq:  DecoupledIO[UInt] = getDecoupledPort(AxiLiteConfig.PortWriteAddress)
    def getWriteEnq:         DecoupledIO[UInt] = getDecoupledPort(AxiLiteConfig.PortWriteData)
    def getWriteResponseDeq: DecoupledIO[UInt] = getDecoupledPort(AxiLiteConfig.PortWriteResponse)
    def getAddressReadEnq:   DecoupledIO[UInt] = getDecoupledPort(AxiLiteConfig.PortReadAddress)
    def getReadDataDeq:      DecoupledIO[UInt] = getDecoupledPort(AxiLiteConfig.PortReadData)

  }
}

class AxiLiteIO(c: AxiLiteConfig) extends AxiLiteSignals(c){
  override def cloneType: this.type = new AxiLiteIO(c).asInstanceOf[this.type]
}

object AxiLite {
  def apply(c: AxiLiteConfig): AxiLiteSignals = new AxiLiteIO(c)
}

object AxiLiteSlave{
  def apply(c: AxiLiteConfig): AxiLiteSignals = AxiLite(c)
}

object AxiLiteMaster{
  def apply(c: AxiLiteConfig): AxiLiteSignals = Flipped(AxiLite(c))
}
