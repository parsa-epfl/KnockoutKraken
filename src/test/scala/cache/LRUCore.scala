package armflex.cache

import chisel3._
import chisel3.experimental._

import org.scalatest.FreeSpec
import chiseltest._
import chiseltest.internal._
import chiseltest.experimental.TestOptionBuilder._

import firrtl.options.TargetDirAnnotation

class TestPseudoTreeLRU extends FreeSpec with ChiselScalatestTester{
  // Plan: Just print the iteration value and see the maximum period.
  "Trace Correction" in {
    val wayNumber = 4
    val cacheParam = new CacheParameter(
      1, wayNumber
    )
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/Pseudo"), WriteVcdAnnotation)
    test(new LRU(cacheParam, () => new PseudoTreeLRUCore(wayNumber), true)).withAnnotations(anno){ dut =>
      // fix one term
      dut.io.addr_i.poke(0.U)
      dut.io.addr_vi.poke(true.B)
      // start to check its value
      for(i <- 0 until 4*cacheParam.associativity){
        println("cycle %d: %d".format(i, dut.io.lru_o.peek().litValue())) // 0 -> 2 -> 1 -> 3
        dut.clock.step(1)
      }
    }
  }
}
