package armflex.util

import chisel3._
import chiseltest._
import chiseltest.internal._

import chisel3.util.DecoupledIO
import chisel3.experimental.BundleLiterals._

object MemHelperDrivers {
  implicit class DecoupledMemReqDriver[T <: MemReqBurst](target: DecoupledIO[T])(implicit clock: Clock) 
    extends DecoupledDriver[T](target) {
    def init(): Unit = {
      target.ready.poke(false.B)
      target.setSinkClock(clock)
      target.setSourceClock(clock)
    }

    def expectDequeue(addr: UInt, burst: UInt): Unit = timescope {
      target.ready.poke(true.B)
      fork
        .withRegion(Monitor) {
          waitForValid()
          target.valid.expect(true.B)
          target.bits.addr.expect(addr)
          target.bits.burst.expect(burst)
        }
        .joinAndStep(getSinkClock)
    }

    
  }
  implicit class WritePortDriver(target: WritePort)(implicit clock: Clock) {
    def init(): Unit = {
      target.req.init()
      target.data.setSinkClock(clock)
      target.data.setSourceClock(clock)
    }

    def expect(expectedAddr: BigInt, 
               expectedBurst: BigInt, 
               expectedData: Seq[BigInt]): Unit = 
    {
      target.req.expectDequeue(expectedAddr.U, expectedBurst.U)
      expectedData.foreach(x => println(x.toString(16)))
      for(bits <- expectedData)
        target.data.expectDequeue(bits.U)
    }

    def getWriteBurst(): Seq[BigInt] = {
      target.req.ready.poke(true.B)
      target.req.waitForValid()
      val addr = target.req.bits.addr.peek().litValue
      val burstS = target.req.bits.burst.peek().litValue
      clock.step()
      target.req.ready.poke(false.B)

      target.data.ready.poke(true.B)
      target.data.waitForValid()
      val dataWrite = for(block <- 0 until burstS.toInt) yield {
        val block = target.data.bits.peek().litValue
        target.data.valid.expect(true.B)
        clock.step(1)
        block
      }
      target.data.ready.poke(false.B)
      dataWrite.toSeq
    }
  }
  implicit class ReadPortDriver(target: ReadPort)(implicit clock: Clock) {
    def init(): Unit = {
      target.req.init()
      target.data.setSinkClock(clock)
      target.data.setSourceClock(clock)
    }

    def expect(expectedAddr: BigInt, 
               expectedBurst: BigInt): Unit = {
      target.req.expectDequeue(expectedAddr.U, expectedBurst.U)
      target.data.valid.poke(true.B)
      while (target.data.ready.peek().litToBoolean == false) {
        clock.step(1)
      }
      for(block <- 0 until expectedBurst.toInt) {
        target.data.ready.expect(true.B)
        target.data.bits.poke(block.U)
      }
      target.data.valid.poke(false.B)
    }

    def pushWriteBurst(data: Seq[BigInt]): Unit = {
      target.req.ready.poke(true.B)
      target.req.waitForValid()
      val addr = target.req.bits.addr.peek().litValue
      val burstS = target.req.bits.burst.peek().litValue
      clock.step()
      target.req.ready.poke(false.B)

      target.data.valid.poke(true.B)
      while (target.data.ready.peek().litToBoolean == false) {
        clock.step(1)
      }
      for(block <- 0 until burstS.toInt) yield {
        target.data.ready.expect(true.B)
        target.data.bits.poke(data(block).U)
      }
      target.data.valid.poke(false.B)
    }
  }
}
