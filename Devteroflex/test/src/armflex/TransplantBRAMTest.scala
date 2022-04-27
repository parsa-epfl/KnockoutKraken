package armflex

import chisel3._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._

import org.scalatest.freespec.AnyFreeSpec
import chiseltest._
import chiseltest.internal._

import firrtl.options.TargetDirAnnotation

class TransBram2HostUnitTestWrapper extends Module {
  val uCore = Module(new TransBram2HostUnit(16))

  // IO wrapper
  val S_AXI = IO(Flipped(uCore.S_AXI.cloneType))
  S_AXI <> uCore.S_AXI

  val wrPort = IO(uCore.transBram2Cpu.wr.cloneType)
  wrPort <> uCore.transBram2Cpu.wr

  val rdPort = IO(uCore.transBram2Cpu.rd.cloneType)
  rdPort <> uCore.transBram2Cpu.rd

  val ctrl = IO(uCore.transBram2Cpu.cloneType)
  ctrl <> uCore.transBram2Cpu.ctrl

  // Converters, between 512bit UInt and 64bit Vector UInt.
  val i512Bits = IO(Input(UInt(512.W)))
  val oVecUInt = IO(Output(Vec(8, UInt(64.W))))

  oVecUInt := i512Bits.asTypeOf(oVecUInt.cloneType)

  val iVecUInt = IO(Input(Vec(8, UInt(64.W))))
  val o512Bits = IO(Output(UInt(512.W)))

  o512Bits := iVecUInt.asUInt

}

class TransBram2HostUnitTest extends AnyFreeSpec with ChiselScalatestTester {
  // helper functions for TransBram2HostUnit
  implicit class TransBram2HostUnitHelper(dut: TransBram2HostUnitTestWrapper) {
    def tick() = {
      dut.clock.step()
    }

    def sendContextThroughAXI(thid: Int, context: Seq[UInt]) = {
      // do an AXI write transactions.
      // dut.S_AXI.aw.awid.poke(0.U)
      dut.S_AXI.aw.awburst.poke(1.U)
      dut.S_AXI.aw.awaddr.poke((thid * 512).U) // thread 13.
      dut.S_AXI.aw.awlen.poke(7.U)
      dut.S_AXI.aw.awsize.poke(6.U)

      // send the write request.
      timescope {
        dut.S_AXI.aw.awvalid.poke(true.B)
        dut.S_AXI.aw.awready.expect(true.B)
        dut.tick()
      }

      // send the write data.
      for(row <- 0 until 8){
        for (col <- 0 until 8){
          val idx = row * 8 + col;
          if (idx >= context.size){
            dut.iVecUInt(col).poke(0.U)
          } else {
            dut.iVecUInt(col).poke(context(idx))
          }
        }

        dut.S_AXI.w.wdata.poke(dut.o512Bits.peek())
        dut.S_AXI.w.wlast.poke((row == 7).B)
        dut.S_AXI.w.wstrb.poke("h_ffff_ffff_ffff_ffff".U)

        timescope {
          dut.S_AXI.w.wvalid.poke(true.B)
          dut.S_AXI.w.wready.expect(true.B)
          dut.tick()
        }
      }

      // wait for the response. It should be available immediately 
      dut.S_AXI.b.bvalid.expect(true.B)
      timescope {
        dut.S_AXI.b.bready.poke(true.B)
        dut.tick()
      }
    }

    def expectReadingContextThroughAXI(thid: Int, context : Seq[UInt]) = {
      // First, send the read request.
      dut.S_AXI.ar.araddr.poke((thid * 512).U)
      dut.S_AXI.ar.arburst.poke(1.U)
      dut.S_AXI.ar.arlen.poke(7.U)
      dut.S_AXI.ar.arsize.poke(6.U)

      timescope {
        dut.S_AXI.ar.arvalid.poke(true.B)
        dut.S_AXI.ar.arready.expect(true.B)
        dut.tick()
      }
      
      // there should be one additional cycle delay, because of the latency of the BRAM.
      dut.tick()

      // start receiving data
      for(row <- 0 until 8){
        dut.S_AXI.r.rvalid.expect(true.B)
        dut.i512Bits.poke(dut.S_AXI.r.rdata.peek())

        for (col <- 0 until 8){
          val idx = row * 8 + col;
          if (idx >= context.size){
            dut.oVecUInt(col).expect(0.U)
          } else {
            dut.oVecUInt(col).expect(context(idx))
          }
        }

        if (row == 7){
          dut.S_AXI.r.rlast.expect(true.B)
        }

        timescope {
          dut.S_AXI.r.rready.poke(true.B)
          dut.tick()
        }
      }
    }
  }

  "AXI -> Normal" in {
    val anno = Seq(
      VerilatorBackendAnnotation,
      TargetDirAnnotation("test/transplant/TransBram2HostUnitTest/AXIToNormal"),
      WriteVcdAnnotation
    )
    test(new TransBram2HostUnitTestWrapper).withAnnotations(anno) { dut =>

      dut.tick()
      dut.tick()
      dut.tick()

      // Send the data
      dut.sendContextThroughAXI(13, Seq(
        0, 1, 2, 3, 4, 5, 6, 7,
        0, 1, 2, 3, 4, 5, 6, 7,
        0, 1, 2, 3, 4, 5, 6, 7,
        0, 1, 2, 3, 4, 5, 6, 7,
        0, 1, 2, 3, 4, 5, 6, 7,
      ).map({x => x.U}))

      // read it back
      dut.rdPort.thid.poke(13.U)
      // Send the read request
      timescope {
        dut.rdPort.xreg.req.bits.regIdx.poke(1.U)
        dut.rdPort.xreg.req.ready.expect(true.B)
        dut.rdPort.xreg.req.valid.poke(true.B)

        dut.tick()
      }

      // Find the result!
      dut.rdPort.xreg.resp.expect(1.U)

      dut.rdPort.pstate.resp.PC.expect(0.U)
      dut.rdPort.pstate.resp.flags.expect(new PStateFlags().Lit(
        _.NZCV -> 1.U,
        _.isException -> false.B,
        _.isUndef -> false.B,
        _.isICountDepleted -> false.B,
        _.execMode -> 0.U
      ))
      dut.rdPort.pstate.resp.icount.expect(2.U)
      dut.rdPort.pstate.resp.icountBudget.expect(0.U)
    }
  }

  "Normal -> AXI" in {
    val anno = Seq(
      VerilatorBackendAnnotation,
      TargetDirAnnotation("test/transplant/TransBram2HostUnitTest/NormalToAXI"),
      WriteVcdAnnotation
    )
    test(new TransBram2HostUnitTestWrapper).withAnnotations(anno) { dut =>
      // write the registers.
      dut.wrPort.thid.poke(11.U)
      dut.wrPort.xreg.req.bits.regIdx.poke(18.U)
      dut.wrPort.xreg.req.bits.data.poke(0x7777F823.U)
      timescope {
        dut.wrPort.xreg.req.ready.expect(true.B)
        dut.wrPort.xreg.req.valid.poke(true.B)
        dut.tick()
      }

      // Then push another read request just near by. It should not pollute the data.
      dut.wrPort.thid.poke(11.U)
      dut.wrPort.xreg.req.bits.regIdx.poke(17.U)
      dut.wrPort.xreg.req.bits.data.poke(100.U)
      timescope {
        dut.wrPort.xreg.req.ready.expect(true.B)
        dut.wrPort.xreg.req.valid.poke(true.B)
        dut.tick()
      }

      // This write request is sent out. Then let us read it from the read port.
      dut.rdPort.thid.poke(11.U)
      dut.rdPort.xreg.req.bits.regIdx.poke(18.U)
      timescope {
        dut.rdPort.xreg.req.ready.expect(true.B)
        dut.rdPort.xreg.req.valid.poke(true.B)
        dut.tick()
      }
      dut.rdPort.xreg.resp.expect(0x7777F823.U)

      // prepare for the next request
      dut.rdPort.thid.poke(11.U)
      dut.rdPort.xreg.req.bits.regIdx.poke(17.U)
      timescope {
        dut.rdPort.xreg.req.ready.expect(true.B)
        dut.rdPort.xreg.req.valid.poke(true.B)
        dut.tick()
      }
      dut.rdPort.xreg.resp.expect(100.U)

      // Now we will commit some pState to the BRAM
      dut.wrPort.thid.poke(11.U)
      dut.wrPort.pstate.req.bits.state.PC.poke(128.U) // 32
      dut.wrPort.pstate.req.bits.state.flags.NZCV.poke(1.U) // 33
      dut.wrPort.pstate.req.bits.state.flags.isException.poke(false.B)
      dut.wrPort.pstate.req.bits.state.flags.isICountDepleted.poke(true.B)
      dut.wrPort.pstate.req.bits.state.flags.isUndef.poke(false.B)
      dut.wrPort.pstate.req.bits.state.flags.execMode.poke(PStateConsts.PSTATE_FLAGS_EXECUTE_WAIT.U)
      dut.wrPort.pstate.req.bits.state.icount.poke(0xB.U) // 34
      dut.wrPort.pstate.req.bits.state.icountBudget.poke(0xA.U)
      dut.wrPort.pstate.req.bits.state.asid.poke(0.U)
      dut.wrPort.pstate.req.bits.state.asid_unused.poke(0.U)
      timescope {
        dut.wrPort.pstate.req.ready.expect(true.B)
        dut.wrPort.pstate.req.valid.poke(true.B)
        dut.tick()
      }

      // and immediately, we write a register to 35, and then read them out.
      dut.wrPort.thid.poke(11.U)
      dut.wrPort.xreg.req.bits.regIdx.poke(35.U)
      dut.wrPort.xreg.req.bits.data.poke(1024.U)
      timescope {
        dut.wrPort.xreg.req.ready.expect(true.B)
        dut.wrPort.xreg.req.valid.poke(true.B)
        dut.tick()
      }

      // Then, I will read all of them out.
      // First, the read 35.
      dut.rdPort.thid.poke(11.U)
      dut.rdPort.xreg.req.bits.regIdx.poke(35.U)
      timescope {
        dut.rdPort.xreg.req.ready.expect(true.B)
        dut.rdPort.xreg.req.valid.poke(true.B)
        dut.tick()
      }
      dut.rdPort.xreg.resp.expect(1024.U)

      // Then, I read the pstate.
      timescope {
        dut.rdPort.pstate.req.ready.expect(true.B)
        dut.rdPort.pstate.req.valid.poke(true.B)
        dut.tick()
      }
      dut.rdPort.pstate.resp.PC.expect(128.U)
      dut.rdPort.pstate.resp.flags.NZCV.expect(1.U)
      dut.rdPort.pstate.resp.flags.isException.expect(false.B)
      dut.rdPort.pstate.resp.flags.isICountDepleted.expect(true.B)
      dut.rdPort.pstate.resp.flags.isUndef.expect(false.B)
      dut.rdPort.pstate.resp.icount.expect(0xB.U)
      dut.rdPort.pstate.resp.icountBudget.expect(0xA.U)


      // Now, finally, I have to read all these stuff from AXI.

      dut.expectReadingContextThroughAXI(11, Seq(
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 100, 0x7777F823, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,

        128, 0x41
      ).map {x => x.U} ++ Seq("h_a_0000_000b".U)  ++ Seq(1024.U))
    }
  }


}

