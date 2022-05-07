package armflex_pmu

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import chiseltest._
import chiseltest.experimental._

import chiseltest.simulator.VerilatorBackendAnnotation
import chiseltest.simulator.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation


import org.scalatest.freespec.AnyFreeSpec

package object PMUDriver {
  implicit class PMUDriver1(dut: PerformanceMonitor){
    def expectCycleCounter(expect: BigInt) = timescope {
      dut.S_CSR.addr.poke(0.U)
      dut.S_CSR.read.poke(true.B)
      dut.S_CSR.dataIn.expect((expect & 0xFFFFFFFF).U)

      dut.S_CSR.addr.poke(1.U)
      dut.S_CSR.dataIn.expect((expect >> 32).U)
    }

    def expectCommitCounter(expect: BigInt) = timescope {
      dut.S_CSR.addr.poke(3.U)
      dut.S_CSR.read.poke(true.B)
      dut.S_CSR.dataIn.expect((expect & 0xFFFFFFFF).U)

      dut.S_CSR.addr.poke(4.U)
      dut.S_CSR.dataIn.expect((expect >> 32).U)
    }

    def tk(cycle: Int = 1) = {
      dut.clock.step(cycle)
    }

    def expectCycleCounterValue(index: Int, result: Seq[Int]) = timescope {
      // At present we have 16 counters, and each counter is 16 bits.
      // so we will read 8 consecutive counters to get the value.
      assert(result.length == 16)
      val bias = 8 + index * 8
      for(i <- 0 until 8){
        // Read the counter value.
        dut.S_CSR.addr.poke((bias + i).U)
        dut.S_CSR.read.poke(true.B)
        val expectedValue = result(2*i) | (result(2*i + 1) << 16)
        // now we have the counter value here.
        dut.S_CSR.dataIn.expect(expectedValue.U)
      }
    }
  }
}

class PMUTester extends AnyFreeSpec with ChiselScalatestTester {
  import PMUDriver._
  "Counting cycles" in {
    val anno = Seq(
      TargetDirAnnotation("test/pmu/cycles"), 
      VerilatorBackendAnnotation, 
      WriteVcdAnnotation
    )
    test(new PerformanceMonitor(64)).withAnnotations(anno){ dut =>
      // the initial value of the counter should be zero.
      dut.expectCycleCounter(0)
      dut.tk(10)
      dut.expectCycleCounter(0)

      // Now we open the cycle counter.
      timescope {
        dut.S_CSR.addr.poke(2)
        dut.S_CSR.dataOut.poke(1.U)
        dut.S_CSR.write.poke(true.B)
        dut.tk()
      }

      // then we move forward by 20 cycles
      dut.tk(20)
      dut.expectCycleCounter(20)

      // now we close the cycle counter
      timescope {
        dut.S_CSR.addr.poke(2)
        dut.S_CSR.dataOut.poke(0.U)
        dut.S_CSR.write.poke(true.B)
        dut.tk()
      }
      // including the cycle to change the register, the value should be 21.
      dut.expectCycleCounter(21)

      dut.tk(10)
      // no update of the counter after it's closed.
      dut.expectCycleCounter(21)
    }
  }

  "Counting commit" in {
    val anno = Seq(
      TargetDirAnnotation("test/pmu/commit"), 
      VerilatorBackendAnnotation, 
      WriteVcdAnnotation
    )
    test(new PerformanceMonitor(64)).withAnnotations(anno){ dut =>
      // In the beginning, the commit counter is zero.
      dut.expectCommitCounter(0)
      dut.iCommittedValid.poke(true.B)
      dut.tk(10)
      dut.expectCommitCounter(10)
      dut.tk(10)
      dut.expectCommitCounter(20)
      dut.iCommittedValid.poke(false.B)
      dut.tk(10)
      dut.expectCommitCounter(20)
    }
  }

  "Counting events" in {
    val anno = Seq(
      TargetDirAnnotation("test/pmu/events"), 
      VerilatorBackendAnnotation, 
      WriteVcdAnnotation
    )
    test(new PerformanceMonitor(64)).withAnnotations(anno){ dut =>
      for (counter_idx <- 0 until 4){
        // we first believe that counter should be all zeros.
        dut.expectCycleCounterValue(counter_idx, Seq.fill(16){0})
        dut.tk(10)
        dut.expectCycleCounterValue(counter_idx, Seq.fill(16){0})

        // Now, the event start.
        timescope {
          dut.iCycleCountingReq(counter_idx).start.bits.poke(counter_idx * 10 + 15)
          dut.iCycleCountingReq(counter_idx).start.valid.poke(true.B)
          dut.tk()
        }

        dut.tk(10)

        // the first element should be 10.
        dut.expectCycleCounterValue(counter_idx, Seq(10) ++ Seq.fill(15){0})

        // During the middle, if the stop signal does not match, the counter will not stopped.
        timescope {
          dut.iCycleCountingReq(counter_idx).stop.bits.poke(0)
          dut.iCycleCountingReq(counter_idx).stop.valid.poke(true.B)
          dut.tk()
        }

        // the first element should be 11.
        dut.expectCycleCounterValue(counter_idx, Seq(11) ++ Seq.fill(15){0})

        // Let's do some cycles more
        dut.tk(10)
        dut.expectCycleCounterValue(counter_idx, Seq(21) ++ Seq.fill(15){0})

        // OK, now it's time to stop.
        timescope {
          dut.iCycleCountingReq(counter_idx).stop.bits.poke(counter_idx * 10 + 15)
          dut.iCycleCountingReq(counter_idx).stop.valid.poke(true.B)
          dut.tk()
        }
        // finally, we have the counter
        dut.expectCycleCounterValue(counter_idx, Seq(22) ++ Seq.fill(15){0})
        // if 10 cycles more, nothing happens.
        dut.tk(10)
        dut.expectCycleCounterValue(counter_idx, Seq(22) ++ Seq.fill(15){0})

        // Then we start a new recording.
        timescope {
          dut.iCycleCountingReq(counter_idx).start.bits.poke(counter_idx * 10 + 11)
          dut.iCycleCountingReq(counter_idx).start.valid.poke(true.B)
          dut.tk()
        }

        dut.tk(10)

        timescope {
          dut.iCycleCountingReq(counter_idx).stop.bits.poke(counter_idx * 10 + 11)
          dut.iCycleCountingReq(counter_idx).stop.valid.poke(true.B)
          dut.tk()
        }
        dut.expectCycleCounterValue(counter_idx, Seq(22, 11) ++ Seq.fill(14){0})
      }
    }
  }
}