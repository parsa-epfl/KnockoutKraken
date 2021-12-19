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
import armflex.util.{DRAMPortParams}
object InstrumentationDrivers {
  abstract class DriverBase[T <: MultiIOModule](target: T) {
    implicit val clock = target.clock
  }
  implicit class TopLevelExampleDriver(target: TopLevelExample) 
    extends DriverBase(target) {
      private val csrParams = target.params.csrParams
      def init() {
        target.S_AXIL.init()
        target.dram_io.read.init()
        target.dram_io.write.init()
      }
      def startTransaction(addr: Long, burst: Int) = {
        target.S_AXIL.wr(csrParams.getAddrAXIL(csrParams.ctrlRegsBaseAddr + csrParams.ctrlRegAddrAddr).U, addr.U)
        target.S_AXIL.wr(csrParams.getAddrAXIL(csrParams.ctrlRegsBaseAddr + csrParams.ctrlRegBurstAddr).U, burst.U)
        target.S_AXIL.wr(csrParams.getAddrAXIL(csrParams.ctrlRegsBaseAddr + csrParams.ctrlRegStartAddr).U, ((CommandTypes.cWrite.litValue << 1) + 1).U)
      }

      def prepareMemory(block: Seq[BigInt]) = {
        for(idx <- 0 until block.length) {
          target.S_AXIL.wr((csrParams.bramBaseAddr + idx*4).U, block(idx).U)
        }
      }

      def sendWriteBurst(addr: Long, burst: Int, data: Seq[Int]): Unit = {
        val blocks = data.grouped(512/32).map{
          seqInt => 
            val byteBuffers = seqInt.reverse.map {
              int => ByteBuffer.allocate(4).putInt(int).array()
            }
            BigInt(0.toByte +: byteBuffers.flatten.toArray)
        }
        val words = data.map(int => BigInt(0.toByte +: ByteBuffer.allocate(4).putInt(int).array()))
        //words.foreach(w => println(w.toString(16)))
        blocks.foreach(b => println(b.toString(16)))
        prepareMemory(words)
        startTransaction(addr, burst)
        target.dram_io.write.expect(addr, burst, blocks.toSeq)
        clock.step(10)
      }

      def checkIfDone(): Boolean = target.S_AXIL.rd(csrParams.ctrlRegDoneAddr.U).litValue != 0
    }

  class TopLevelExampleTestSimple(dut: TopLevelExample) {

    def prepareBlocks(nBlocks: Int): Seq[Int] = 
      for(block <- 0 until nBlocks; 
          word <- 0 until dut.params.dramParams.pDataW/dut.params.csrParams.axilDataW) 
          yield ((block << 8) + word)

    def runExample(nBlocks: Int, isRead: Boolean): Unit = {
        dut.init()
        val blocksBigInt = prepareBlocks(nBlocks).map(BigInt(_)).toSeq
        dut.prepareMemory(blocksBigInt)
        dut.startTransaction(0x0, nBlocks)
        dut.clock.step(100)
    }

    def runBurst(nBlocks: Int, baseaddr: Long): Unit = {
        dut.init()
        dut.sendWriteBurst(baseaddr, nBlocks, prepareBlocks(nBlocks))
    }
  }
}

class TestInstrumentationExample extends FlatSpec with ChiselScalatestTester {
  val annos = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/instrumentation"), WriteVcdAnnotation)


  behavior of "DevteroFlex Instrumentation Example"

  it should "Test Pushing and Pulling a memory block" in {
    test(new TopLevelExample(new TopLevelExampleParams(
      new DRAMPortParams(12, 512),
      new CSRExampleParams(32),
      new ComputeExampleParams(12, 512)
    ))).withAnnotations(annos) {
      dut => 
        val dutDriver = new TopLevelExampleTestSimple(dut) 
        dutDriver.runBurst(8, 0x100)
      }
  }
}
