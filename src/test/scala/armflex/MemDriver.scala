package armflex

import scala.language.implicitConversions
import java.util.concurrent.atomic.AtomicInteger 

import org.scalatest._
import chisel3._

import chiseltest._
import chiseltest.internal._
import chiseltest.experimental.TestOptionBuilder._

import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import firrtl.{EmitAllModulesAnnotation}
import firrtl.options.{TargetDirAnnotation}


import arm.PROCESSOR_TYPES.{DATA_SZ, REG_N}

import armflex.TPU2STATE._

import util._
import util.AxiLite.AxiLiteSlaveDriver
import util.BRAMPortDriver.BRAMPortDriver
import util.SoftwareStructs._

import armflex.MemoryDrivers.MemPortDriver

object MemoryDrivers {
  implicit class MemPortDriver(target: MemPort)(implicit val cfgProc: ProcConfig) {
    implicit val clock: Clock = cfgProc.clock
    val transCount = new AtomicInteger

    def init: this.type = {
      target.port.init
      this
    }

    def wr(addr: UInt, data: UInt): Unit = timescope {
      transCount.getAndIncrement()
      
      target.port.EN.poke(true.B)
      target.port.WE.poke(((BigInt(1) << target.port.WE_W) - 1).U)
      target.port.ADDR.poke(addr)
      target.port.DI.poke(data)
      fork.withRegion(Monitor) {
        while (target.busy.peek().litToBoolean == true) {
          clock.step()
        }
      }.joinAndStep(clock)
      if(cfgProc.simVerbose) {
        val cycle = Context().backend.getClockCycle()
        println(s"${"%3d".format((cycle-1)*2)}ns:WR:${addr.litValue}|${data.litValue}")
      }
    }

    def rd(addr:UInt): UInt = {
      transCount.getAndIncrement()
      var data: UInt = 0.U
      timescope {
        target.port.EN.poke(true.B)
        target.port.WE.poke(0.U)
        target.port.ADDR.poke(addr)
        fork.withRegion(Monitor) {
          while (target.busy.peek().litToBoolean == true) {
            clock.step()
          }
        }.joinAndStep(clock)
        data = target.port.DO.peek()
      }
      if(cfgProc.simVerbose) {
        val cycle = Context().backend.getClockCycle()
        println(s"${"%3d".format((cycle-1)*2)}ns:RD:${addr.litValue}|${data.litValue}")
      }
      return data
    }
  }
}

class MemDriver(target: MemorySystem)(implicit val cfgProc: ProcConfig) {
  implicit val clock : Clock = target.clock

  def init: Unit = {
    target.ports.fetch.init
    target.ports.ls.init
    target.ports.host.init
    target.ctrl.fillTLB.valid.poke(false.B)
    // target.ctrl.fillTLB.bits.poke()
    target.ctrl.reqMiss.ready.poke(false.B)
  }

  this.init

  def runSmall(): Unit = {
    var fetchData: BigInt = 0
    fork {
      clock.step(1)
      fetchData = target.ports.fetch.rd(1.U).litValue
      clock.step(2)
      fetchData = target.ports.fetch.rd(1.U).litValue
    }.fork {
      clock.step(2)
      target.ports.ls.wr(1.U, 10.U)
    }.fork {
      for(i <- 0 until 5) {
        target.ports.host.wr(i.U, i.U)
      }
    }.join
  }

  def Race_LSnHOST_WR(): Unit = {
    var fetchData: BigInt = 0
    fork {
      clock.step(3)
      fetchData = target.ports.fetch.rd(5.U).litValue
      clock.step(3)
      fetchData = target.ports.fetch.rd(5.U).litValue
    }.fork {
      clock.step(5)
      target.ports.ls.wr(5.U, 10.U)
    }.fork {
      for(i <- 0 until 10) {
        target.ports.host.wr(i.U, i.U)
      }
    }.join
  }
}

class Memory extends FlatSpec with ChiselScalatestTester {

  val annos = Seq(
    VerilatorBackendAnnotation,
    TargetDirAnnotation("simtest/Memory"),
    WriteVcdAnnotation)

  behavior of "3-Ported memory"

  implicit val cfgProc: ProcConfig = new ProcConfig(NB_THREADS = 2, 
    DebugSignals = true, simVerbose = true)

  it should "execute a chain of memory calls" in {
    test(new MemorySystem).withAnnotations(annos) { dut =>
      cfgProc.clock = dut.clock
      val drv = new MemDriver(dut)

      drv.runSmall
    }
  }
}

