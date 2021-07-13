package armflex_cache

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
    val wayNumber = 16
    val cacheParam = new CacheParameter(
      36, 1, wayNumber
    )
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/Pseudo"), WriteVcdAnnotation)
    test(new LRU(cacheParam.databankParameter, () => new PseudoTreeLRUCore(wayNumber))).withAnnotations(anno){ dut =>
      // fix one term
      // dut.addr_i.poke(0.U)
      dut.index_i.valid.poke(true.B)
      // start to check its value
      for(i <- 0 until 4*cacheParam.associativity){
        println("cycle %d: %d".format(i, dut.lru_o.peek().litValue())) // 0 -> 2 -> 1 -> 3
        dut.index_i.bits.poke(dut.lru_o.peek())
        dut.clock.step(1)
      }
    }
  }
}
