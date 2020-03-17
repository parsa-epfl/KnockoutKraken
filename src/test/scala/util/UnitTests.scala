package armflex.util

import chisel3._
import chisel3.experimental._

import org.scalatest._
import chiseltest._
import chiseltest.internal._
import chiseltest.experimental.TestOptionBuilder._

import firrtl.options.TargetDirAnnotation

class TestPseudoLRU extends FlatSpec with ChiselScalatestTester {

  val annos = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/Pseudo"), WriteVcdAnnotation)

  val r = scala.util.Random
  behavior of "Armflex Simulator AXI interface"

  it should "Test LRU Unit" in {

    test(new PseudoLRU(32, 5)).withAnnotations(annos) { lru =>
      for( i <- 0 until 1000) {
        val freeIdx = lru.io.lru_idx.peek.litValue

        lru.io.idx_1.valid.poke(true.B) // r.nextInt(2).B)

        lru.io.idx_1.bits.poke(r.nextInt(32).U)

        lru.io.idx_2.valid.poke(true.B) // r.nextInt(2).B)

        lru.io.idx_2.bits.poke(r.nextInt(32).U)

        lru.clock.step()
      }
    }
  }
}

