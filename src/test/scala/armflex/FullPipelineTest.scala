package armflex

import chisel3._
import chisel3.experimental._

import org.scalatest._
import chiseltest._
import chiseltest.internal._
import chiseltest.experimental.TestOptionBuilder._

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import firrtl.{EmitAllModulesAnnotation}
import firrtl.options.{TargetDirAnnotation}

import scala.collection.mutable.Queue

import armflex._
import armflex.util.VerificationDriver
import armflex.util.ArmflexProtoBuf._
import armflex.cache.MemorySystemParameter
import armflex.util.SoftwareStructs.CommitTrace
import armflex.TestDriversExtra._
import armflex.PipelineDrivers._
import org.scalatest.exceptions.TestFailedException

class PipelineTest(val dut: PipelineHardDriverModule, traceDrv: VerificationDriver) {
  private val init: Unit = {
    dut.initIO
    dut.clock.setTimeout(10000)
  }
  var running = true
  val transplantInsts = new Queue[BigInt]()

  def transplanter: Unit = {
    var trace: CommitTrace = null
    while (running) {
      if (dut.transplantOut.state.valid.peek.litToBoolean) {
        trace = dut.transplantOut.state.bits.peek()
        transplantInsts.enqueue(dut.transplantOut.inst.peek().litValue)
      }
      if (dut.transplantIO.transOut.valid.peek.litToBoolean) {
        dut.transplantAndStart(0, trace.state)
      }
      dut.clock.step()
    }
    println("Done Transplanting")
  }

  def run(rerun: Boolean = false, pc: BigInt = 0): Unit = {
    var trace = traceDrv.next

    if (rerun) {
      while (trace._1.state.pc != pc) {
        trace = traceDrv.next
      }
    }

    dut.transplantAndStart(0, trace._1.state)
    fork
      .withRegion(Monitor) {
        transplanter
      }
      .fork {
        while (traceDrv.hasNext) {
          // Fetch
          dut.traceIn(trace._1)
          if (trace._2.isDefined) {
            dut.traceExpect(trace._2.get)
          }
          // Next Inst
          trace = traceDrv.next
        }
        while (!dut.done.peek.litToBoolean) {
          dut.clock.step(1)
        }
        running = false
      }
      .joinAndStep(dut.clock)
     println("Done simulating")
     dut.clock.step(100)
     traceDrv.writeInstsToFile(transplantInsts.iterator)
  }
}

class FullPipelineTest extends FreeSpec with ChiselScalatestTester {
  val traceName: String = "binary100_0"
  val cfgProc:   ProcConfig = new ProcConfig(NB_THREADS = 2, DebugSignals = true)
  val annos = Seq(
    VerilatorBackendAnnotation,
    TargetDirAnnotation("test/TraceBase/" + traceName + "/"),
    WriteVcdAnnotation
  )

  val rerun = false
  val pc: BigInt = BigInt("ffff0000080838c0", 16)

  val traceDrv = new VerificationDriver(traceName)
  "Will verify correctness of the pipeline" in {

    test(new PipelineHardDriverModule()(cfgProc))
      .withAnnotations(annos) { dut =>
        val drv = new PipelineTest(dut, traceDrv)
        drv.run(rerun, pc)
      }
  }
}
