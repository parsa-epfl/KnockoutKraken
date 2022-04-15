package armflex.util

import chisel3._
import chisel3.experimental._
import chisel3.util._
import chiseltest._

import armflex.util._

import antmicro.Bus._
import antmicro.Bus._

import chiseltest.simulator.VerilatorBackendAnnotation
import chiseltest.simulator.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation


import org.scalatest.freespec.AnyFreeSpec

class AXIInterconnectorTester extends AnyFreeSpec with ChiselScalatestTester {
  "Normal Read" in {
    val anno = Seq(
      TargetDirAnnotation("test/axi_interconnect/normal_read"), VerilatorBackendAnnotation, WriteVcdAnnotation
    )
    def axiAddrMap(addr: UInt, idx: Int): Bool = {
      /**
       * 0: 0x0000 - 0x1000
       * 1: 0x1000 - 0x2000
       * 2: 0x2000 - 0x3000
       * 3: 0x3000 - 0x4000
       */ 
      return addr >= (idx * 0x1000).U && addr < ((idx + 1) * 0x1000).U
    }
    test(new AXIInterconnector(
      4,
      axiAddrMap,
      32,
      512
    )).withAnnotations(anno) { dut =>
      // read request
      dut.S_AXI.ar.araddr.poke(0x2040)
      timescope {
        dut.S_AXI.ar.arvalid.poke(true.B)

        // 0,1,3 should not received the read request
        dut.M_AXI(0).ar.arvalid.expect(false.B)
        dut.M_AXI(1).ar.arvalid.expect(false.B)
        dut.M_AXI(3).ar.arvalid.expect(false.B)

        // 2 should receive the read request, but it's not ready
        dut.M_AXI(2).ar.arvalid.expect(true.B)
        dut.M_AXI(2).ar.arready.poke(false.B)
        // so the input should be not ready as well
        dut.S_AXI.ar.arready.expect(false.B)

        // even if the read reply is valid, it should not be broadcast to the source.
        timescope {
          dut.M_AXI(2).r.rvalid.poke(true.B)
          dut.S_AXI.r.rvalid.expect(false.B)
        }

        // let's intentionally wait for some cycles
        dut.clock.step(3)

        // 2 should still receive the read request
        dut.M_AXI(2).ar.arvalid.expect(true.B)
        // now let's make it ready
        dut.M_AXI(2).ar.arready.poke(true.B)

        dut.S_AXI.ar.arready.expect(true.B)

        // OK, the request should be received now.
        dut.clock.step()
      }

      // read data. 
      // No request should be received anymore.
      timescope {
        dut.S_AXI.ar.araddr.poke(0x1000.U)
        dut.M_AXI(1).ar.arready.poke(true.B)
        dut.S_AXI.ar.arready.expect(false.B)
      }
      timescope {
        // assume now we will send the last request.
        dut.M_AXI(2).r.rlast.poke(true.B)
        // and the slave is valid
        dut.M_AXI(2).r.rvalid.poke(true.B)
        // but the source is not ready.
        dut.S_AXI.r.rready.poke(false.B)
        // then the data is not ready.
        dut.M_AXI(2).r.rready.expect(false.B)

        dut.clock.step(2)

        // after 2 cycles, the source can receive the read reply
        dut.S_AXI.r.rready.poke(true.B)
        dut.M_AXI(2).r.rready.expect(true.B)

        dut.clock.step()
      }

      // Now the interconnect should be able to forward another read request
      dut.S_AXI.ar.araddr.poke(0x3040.U)
      timescope {
        dut.S_AXI.ar.arvalid.poke(true.B)
        dut.M_AXI(3).ar.arvalid.expect(true.B)
        dut.M_AXI(3).ar.arready.poke(true.B)
        dut.S_AXI.ar.arready.expect(true.B)
      }
    }
  }
  "Normal Write" in {
    val anno = Seq(
      TargetDirAnnotation("test/axi_interconnect/normal_write"), VerilatorBackendAnnotation, WriteVcdAnnotation
    )
    def axiAddrMap(addr: UInt, idx: Int): Bool = {
      /**
       * 0: 0x0000 - 0x1000
       * 1: 0x1000 - 0x2000
       * 2: 0x2000 - 0x3000
       * 3: 0x3000 - 0x4000
       */ 
      return addr >= (idx * 0x1000).U && addr < ((idx + 1) * 0x1000).U
    }
    test(new AXIInterconnector(
      4,
      axiAddrMap,
      32,
      512
    )).withAnnotations(anno) { dut =>
      dut.S_AXI.aw.awaddr.poke(0x80.U) // The first channel.

      timescope {
        dut.S_AXI.aw.awvalid.poke(true.B)
        
        // 1,2,3 should receive nothing.
        dut.M_AXI(1).aw.awvalid.expect(false.B)
        dut.M_AXI(2).aw.awvalid.expect(false.B)
        dut.M_AXI(3).aw.awvalid.expect(false.B)

        // 0 should indeed receive something
        dut.M_AXI(0).aw.awvalid.expect(true.B)
        // but we just indicate 0 is not ready.
        dut.M_AXI(0).aw.awready.poke(false.B)
        // then the source should see an invalid.
        dut.S_AXI.aw.awready.expect(false.B)

        // wait for some cycles
        dut.clock.step(2)

        // 0 decide to give a ready reply
        dut.M_AXI(0).aw.awready.poke(true.B)
        // then the source should see a ready.
        dut.S_AXI.aw.awready.expect(true.B)

        // Done!
        dut.clock.step()
      }

      // Now it's time to see the effect of the write data
      dut.S_AXI.aw.awaddr.poke(0x2000.U)
      timescope {
        // even if another channel is ready, not request forwarding is allowed.
        dut.M_AXI(2).aw.awready.poke(true.B)
        dut.S_AXI.aw.awready.expect(false.B)
      }
      timescope {
        dut.S_AXI.w.wlast.poke(true.B)
        dut.S_AXI.w.wvalid.poke(true.B)
        // channel zero should receive these data
        dut.M_AXI(0).w.wlast.expect(true.B)
        dut.M_AXI(0).w.wvalid.expect(true.B)
        // but I don't want to receive the request this cycle
        dut.M_AXI(0).w.wready.poke(false.B)
        dut.S_AXI.w.wready.expect(false.B)

        // wait for some cycles
        dut.clock.step(3)

        // now, I decide to receive
        dut.M_AXI(0).w.wready.poke(true.B)
        dut.S_AXI.w.wready.expect(true.B)

        dut.clock.step()
      }

      // Then at last, the response.
      // other responses are not forwarded at all.
      dut.S_AXI.aw.awaddr.poke(0x3000.U)
      timescope {
        dut.M_AXI(3).aw.awready.poke(true.B)
        dut.M_AXI(3).w.wready.poke(true.B)

        dut.S_AXI.aw.awready.expect(false.B)
        dut.S_AXI.w.wready.expect(false.B)
      }

      // the response signal
      timescope {
        dut.S_AXI.b.bready.poke(true.B)
        dut.M_AXI(0).b.bready.expect(true.B)
        dut.M_AXI(0).b.bvalid.poke(false.B)
        dut.S_AXI.b.bvalid.expect(false.B)

        dut.clock.step(2)

        timescope {
          dut.M_AXI(1).b.bvalid.poke(true.B)
          dut.S_AXI.b.bvalid.expect(false.B)
          dut.clock.step()
        }

        dut.M_AXI(0).b.bvalid.poke(true.B)
        dut.S_AXI.b.bvalid.expect(true.B)

        dut.clock.step()
      }

      // now the next request should be available.
      dut.M_AXI(2).aw.awready.poke(true.B)
      dut.S_AXI.aw.awaddr.poke(0x20C0.U)
      dut.S_AXI.aw.awready.expect(true.B)
    }
  }
  "No one accept" in {
    val anno = Seq(
      TargetDirAnnotation("test/axi_interconnect/no_one_accept"), VerilatorBackendAnnotation, WriteVcdAnnotation
    )
    def axiAddrMap(addr: UInt, idx: Int): Bool = {
      /**
       * 0: 0x0000 - 0x1000
       * 1: 0x1000 - 0x2000
       * 2: 0x2000 - 0x3000
       * 3: 0x3000 - 0x4000
       */ 
      return addr >= (idx * 0x1000).U && addr < ((idx + 1) * 0x1000).U
    }
    test(new AXIInterconnector(
      4,
      axiAddrMap,
      32,
      512
    )).withAnnotations(anno) { dut =>
      dut.S_AXI.ar.araddr.poke(0x5000)
      for (i <- 0 until 4) {
        dut.M_AXI(i).ar.arready.poke(true.B)
        dut.M_AXI(i).aw.awready.poke(true.B)
      }
      dut.S_AXI.ar.arready.expect(false.B)
      dut.S_AXI.aw.awaddr.poke(0x6080)
      dut.S_AXI.aw.awready.expect(false.B)
    }
  }
}

