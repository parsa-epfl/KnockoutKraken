package armflex.util

import chisel3._

import org.scalatest._
import chiseltest._
import chiseltest.internal._

import firrtl.options.TargetDirAnnotation

class TestMultAddBlackbox extends FlatSpec with ChiselScalatestTester {

  val annos = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/PseudoMultAdd"), WriteVcdAnnotation)

  val r = scala.util.Random
  behavior of "Armflex Simulator AXI interface"

  it should "Test MultiAdd" in {

    test(new MACC(32)).withAnnotations(annos) { macc =>
      for( i <- 0 until 1000) {
        macc.io.mult1.poke(r.nextLong().S)
        macc.io.mult2.poke(r.nextLong().S)
        macc.io.add.poke(r.nextLong().S)
        macc.clock.step()
      }
    }
  }
}

