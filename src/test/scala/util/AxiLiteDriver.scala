/** Credits to my friend Laurier Loiselle
  *  source: https://github.com/laurierloi/fpgabits/
  */

package armflex.util

import scala.language.reflectiveCalls
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chiseltest._

object AxiLite {
  implicit class AxiLiteSlaveDriver(target: AxiLiteSignals)(implicit clock: Clock) {
    def init: this.type = {
      target.wstrb.poke(0xF.U)
      target.awprot.poke(0xF.U)
      target.arprot.poke(0xF.U)
      target.awvalid.poke(false.B)
      target.wvalid.poke(false.B)
      target.bready.poke(false.B)
      target.arvalid.poke(false.B)
      target.rready.poke(false.B)
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
