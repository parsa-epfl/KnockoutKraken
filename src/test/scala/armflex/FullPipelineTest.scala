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
    dut.clock.setTimeout(1000)
  }
  var running = true
  val transplantInsts = new Queue[BigInt]()

  def transplanter: Unit = {
    var trace: CommitTrace = null
    while (running) {
      if (dut.transplantOut.state.valid.peek.litToBoolean) {
        trace = dut.transplantOut.state.bits.peek()
        println("PC:" + trace.state.pc.toString(16))
        transplantInsts.enqueue(dut.transplantOut.inst.peek().litValue)
      }
      if (dut.hostIO.transOut.valid.peek.litToBoolean) {
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
    dut.clock.step(10)
    traceDrv.writeInstsToFile(transplantInsts.iterator)
    println("Done writing transplanted instructions")
  }
}

class FullPipelineTest extends FreeSpec with ChiselScalatestTester {
  val cfgProc: ProcConfig = new ProcConfig(NB_THREADS = 2, DebugSignals = true, simVerbose = false)
  def runExample(traceName: String, rerun: Boolean = false, pc: BigInt = BigInt("0000000000000000", 16)) {
    val annos = Seq(
      VerilatorBackendAnnotation,
      TargetDirAnnotation("test/TraceBase/" + traceName + "/")
      ,WriteVcdAnnotation
    )
    val traceDrv = new VerificationDriver(traceName)
    "Will verify pipeline's correctness with "+traceName+" file " in {

      test(new PipelineHardDriverModule()(cfgProc))
        .withAnnotations(annos) { dut =>
          val drv = new PipelineTest(dut, traceDrv)
          drv.run(rerun, pc)
        }
    }
  }

  //runExample("binary10000")
  //runExample("binary10000_0")
  //runExample("binary10000_1")
  //runExample("binary10000_2")
  runExample("binary100")
}
