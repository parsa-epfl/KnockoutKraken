package instrumentation

import chisel3._
import chiseltest._
import chiseltest.internal._
import chiseltest.experimental.TestOptionBuilder._
import chisel3.util.Cat
import armflex.util.AXIDrivers.AXI4LiteDriver
import armflex.util.MemHelperDrivers._
import armflex.util.SoftwareStructs._
import org.scalatest.FlatSpec
import firrtl.options.TargetDirAnnotation
import java.nio.ByteBuffer

import InstrumentationDrivers._
import instrumentation.TraceDumpDrivers.TraceDumpTestSimple

import scala.collection.compat.immutable.ArraySeq
object TraceDumpDrivers {
  abstract class DriverBase[T <: MultiIOModule](target: T) {
    implicit val clock = target.clock
  }

  implicit class TraceDumpDriver(target: TraceDump) extends DriverBase(target) {
    def init() {
      target.trace_data.setSourceClock(clock)
      target.init_addr.setSourceClock(clock)
      target.dram_write_port.init()
    }

    def step(i : Int): Unit = {
      clock.step(i)
    }

    def sendWriteBurst(pcs: Seq[Long]): Unit = {
      for(pc <- pcs) {
        target.trace_data.enqueue(pc.U)
        clock.step(pc.toInt)
      }
    }

    def startMemWrite(expectedAddr: Long, expectedData: Seq[Long]): Unit = {
      val data = expectedData.grouped(512/64).map{
        seqInt =>
          val byteBuffers = seqInt.reverse.map {
            int => ByteBuffer.allocate(8).putLong(int).array()
          }
          BigInt(0.toByte +: byteBuffers.flatten.toArray)
      }
      target.dram_write_port.req.valid.expect(true.B)
      target.dram_write_port.expect(expectedAddr, target.params.burstSize, data.toSeq)
    }
  }

  class TraceDumpTestSimple(dut: TraceDump){
    def run() {
      dut.init()
      val pcs1: Seq[Long] = ArraySeq(6, 5, 6, 14, 1, 5, 3, 2)
      val pcs2: Seq[Long] = ArraySeq(1, 11, 9, 3, 17, 13, 2, 7)
      val init_addr: Long = 0x0
      dut.init_addr.enqueue(init_addr.U)
      dut.step(3)
      for(i <- 0 to 7) {
        val pcs: Seq[Long] = if(i % 2 == 0) pcs1 else pcs2
        dut.sendWriteBurst(pcs)
        dut.startMemWrite(init_addr + i, pcs)
        dut.step(5)
      }
      dut.sendWriteBurst(pcs1)
      dut.sendWriteBurst(pcs2)
      dut.startMemWrite(init_addr, pcs1)
      dut.step(5)
      dut.startMemWrite(init_addr + 1, pcs2)
    }

    def runMultiBurst() {
      dut.init()
      val pcs1: Seq[Long] = ArraySeq(6, 5, 6, 14, 1, 5, 3, 2)
      val pcs2: Seq[Long] = ArraySeq(1, 11, 9, 3, 17, 13, 2, 7)
      val pcs3: Seq[Long] = ArraySeq(5, 7, 15, 2, 1, 1, 12, 9)
      val init_addr: Long = 0x0
      dut.init_addr.enqueue(init_addr.U)
      dut.step(3)
      for(i <- 0 to 3) {
        dut.sendWriteBurst(pcs1)
        dut.sendWriteBurst(pcs2)
        dut.startMemWrite(init_addr + 2 * i, pcs1)
        dut.startMemWrite(init_addr + 2 * i, pcs2)
        dut.step(5)
      }
      dut.sendWriteBurst(pcs1)
      dut.sendWriteBurst(pcs2)
      dut.sendWriteBurst(pcs3)
      dut.startMemWrite(init_addr, pcs1)
      dut.step(5)
      dut.startMemWrite(init_addr, pcs2)
      dut.step(10)
      dut.sendWriteBurst(pcs1)
      dut.startMemWrite(init_addr + 2, pcs3)
      dut.startMemWrite(init_addr + 2, pcs1)
    }

    def runStressTest() {
      dut.init()
      val init_addr: Long = 0x0
      dut.init_addr.enqueue(init_addr.U)
      dut.step(3)
      for(i <- 0 to 8199) {
        println(i)
        dut.trace_data.enqueue(i.U)
      }
    }
  }
}

class TestTraceDump extends FlatSpec with ChiselScalatestTester {
  val annos = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/instrumentation"), WriteVcdAnnotation)


  behavior of "DevteroFlex Instrumentation Example"

  it should "Test Pushing and Pulling a memory block" in {
    test(new TraceDump(new TraceDumpParams(windowSize = 8, burstSize = 2))).withAnnotations(annos) {
      dut =>
        val dutDriver = new TraceDumpTestSimple(dut)
        dutDriver.runMultiBurst()
    }
  }
}
