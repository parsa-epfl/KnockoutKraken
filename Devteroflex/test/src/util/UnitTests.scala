package armflex.util

import chisel3._

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chiseltest.internal._

import firrtl.options.TargetDirAnnotation

class TestMultAddBlackbox extends AnyFlatSpec with ChiselScalatestTester {

  val annos = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/PseudoMultAdd"), WriteVcdAnnotation)

  val r = scala.util.Random
  behavior of "Armflex Simulator AXI interface"

  it should "Test MultiAdd" in {

    test(new MACC(32)).withAnnotations(annos) { macc =>
      for( i <- 0 until 1000) {
        macc.io.mult1.poke(r.nextInt().S)
        macc.io.mult2.poke(r.nextInt().S)
        macc.io.add.poke(r.nextInt().S)
        macc.clock.step()
      }
    }
  }
}


class CreditQueueTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit class CreditQueueControllerDriver(target: CreditQueueController) {
    def init() = {
      target.trans.in.poke(false.B)
      target.trans.out.poke(false.B)
      target.trans.dropped.poke(false.B)
    }

  }

  val r = scala.util.Random
  behavior of "Credit Queue"

  val size = 5
  it should "Check for limit credits" in {
    test(new CreditQueueController(size)).withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/CreditQueue/Limit"))) { dut =>
      dut.init()
      dut.ready.expect(true.B)
      dut.clock.step()
      timescope {
        for(step <- 0 until size) {
          dut.ready.expect(true.B)
          dut.trans.in.poke(true.B)
          dut.clock.step()
          dut.inflight.expect(step + 1)
        }
      }
      dut.ready.expect(false.B)
    }
  }

  it should "Check for popping credits" in {
    test(new CreditQueueController(size)).withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/CreditQueue/Popping"))) { dut =>
      dut.init()
      dut.ready.expect(true.B)
      timescope {
        for(step <- 0 until size) {
          dut.ready.expect(true.B)
          dut.trans.in.poke(true.B)
          dut.trans.out.poke(true.B)
          dut.clock.step()
          dut.inflight.expect(0.U)
        }
      }
      dut.ready.expect(true.B)
    }
  }

  it should "Charge and empty" in {
    test(new CreditQueueController(size)).withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/CreditQueue/Popping"))) { dut =>
      dut.init()
      dut.ready.expect(true.B)
      timescope {
        for(step <- 0 until size) {
          dut.ready.expect(true.B)
          dut.trans.in.poke(true.B)
          dut.clock.step()
          dut.inflight.expect(step + 1)
        }
      }
      dut.ready.expect(false.B)
      var entries = size
      while(entries > 0) {
        dut.inflight.expect(entries)
        dut.trans.out.poke(true.B)
        dut.clock.step()
        entries -= 1
      }
    }
  }

  it should "Charge and empty 2" in {
    test(new CreditQueueController(size)).withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/CreditQueue/ChargeNEmpty2"))) { dut =>
      dut.init()
      dut.ready.expect(true.B)
      timescope {
        for(step <- 0 until size) {
          dut.ready.expect(true.B)
          dut.trans.in.poke(true.B)
          dut.clock.step()
          dut.inflight.expect(step + 1)
        }
      }
      dut.ready.expect(false.B)
      var entries = size

      while(entries > 0) {
        dut.inflight.expect(entries)
        dut.trans.out.poke(true.B)
        dut.trans.dropped.poke(true.B)
        dut.clock.step()
        entries -= 2
        if(entries == 1) {
          dut.inflight.expect(1)
          dut.trans.dropped.poke(false.B)
          dut.clock.step()
          dut.inflight.expect(0)
          entries -= 1
          dut.clock.step()
        }
      }
    }
  }

  it should "Add correct amount of entries" in {
    test(new CreditQueueController(size)).withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/CreditQueue/ChargeNEmpty2"),  WriteVcdAnnotation)) { dut =>
      dut.init()
      dut.ready.expect(true.B)
      timescope {
        dut.trans.in.poke(true.B)
        dut.clock.step()
        dut.inflight.expect(1.U)
      }

      timescope {
        dut.inflight.expect(1.U)
        dut.trans.in.poke(true.B)
        dut.trans.out.poke(true.B)
        dut.clock.step()
        dut.inflight.expect(1.U)
      }

      timescope {
        dut.inflight.expect(1.U)
        dut.trans.in.poke(true.B)
        dut.trans.dropped.poke(true.B)
        dut.clock.step()
        dut.inflight.expect(1.U)
      }

      timescope {
        dut.inflight.expect(1.U)
        dut.trans.in.poke(true.B)
        dut.trans.out.poke(true.B)
        dut.trans.dropped.poke(true.B)
        dut.clock.step()
        dut.inflight.expect(0.U)
      }
    }
  }
}
