/** Credits to my friend Laurier Loiselle
  *  source: https://github.com/laurierloi/fpgabits/
  */

package common

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
  import common.AxiLiteConsts._

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

object AxiLite {

  import chisel3.tester._

  object AxiLiteSlave { def apply(cfg: AxiLiteConfig): AxiLiteSignals = new AxiLiteIO(cfg) }

  object AxiLiteMaster{ def apply(cfg: AxiLiteConfig): AxiLiteSignals = Flipped(new AxiLiteIO(cfg)) }

  implicit class AxiLiteSlaveDriver(target: AxiLiteSignals)(implicit clock: Clock) {
    def init: this.type = {
      target.wstrb.poke(0xF.U)
      target.awprot.poke(0xF.U)
      target.arprot.poke(0xF.U)
      target.awvalid.poke(false.B)
      target.wvalid.poke(false.B)
      target.wrResp.ready.poke(false.B)
      target.rdAddr.valid.poke(false.B)
      target.rdData.ready.poke(false.B)
      this
    }

    // Check DecoupledDriver.scala from chisel3.testers2 for enqueue and dequeue
    private def enqueue(data:BigInt, bits: UInt, valid: Bool, ready: Bool) : Unit = timescope {
      bits.poke(data.U)
      valid.poke(true.B)
      fork.withRegion(Monitor) {
        while (ready.peek().litToBoolean == false) {
          clock.step(1)
        }
      }.joinAndStep(clock)
    }

    private def expectDequeue(data: BigInt, bits: UInt, valid: Bool, ready: Bool): Unit = timescope {
      // TODO: check for init
      ready.poke(true.B)
      fork.withRegion(Monitor) {
        while (valid.peek().litToBoolean == false) { clock.step(1) }
        valid.expect(true.B)
        bits.expect(data.U)
      }.joinAndStep(clock)
    }

    def rd32B(addr: BigInt): BigInt = {
      var data: UInt = 0.U
      timescope {
        fork {
          enqueue(addr, target.araddr, target.arvalid, target.arready)
        }.fork {
          target.rready.poke(true.B)
          fork.withRegion(Monitor) {
            while (target.rvalid.peek().litToBoolean == false) {
              clock.step(1)
            }
            target.rvalid.expect(true.B)
            data = target.rdata.peek()
          }.joinAndStep(clock)
        }.join()
      }
      data.litValue
    }

    // TODO Strobe ?
    // def wr8B(offst: Long, data: Int, strb: Int = 0xFF) = {}
    // def wr16B(offst: Long, data: Int) = {
    def wr32B(offst: BigInt, data: BigInt) = timescope {
      fork {
        enqueue(offst, target.awaddr, target.awvalid, target.awready)
      }.fork {
        enqueue(data, target.wdata, target.wvalid, target.wready)
      }.fork {
        expectDequeue(0, target.bresp, target.bvalid, target.bready)
      }.join()
    }
  }
}
