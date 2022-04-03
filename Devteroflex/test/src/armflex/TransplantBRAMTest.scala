package armflex

import chisel3._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._

import org.scalatest.freespec.AnyFreeSpec
import chiseltest._
import chiseltest.internal._

import firrtl.options.TargetDirAnnotation

class TransplantBRAMTestWrapper extends Module {
  val uCore = Module(new TransplantBRAM(16))

  // IO wrapper
  val S_AXI = IO(Flipped(uCore.S_AXI.cloneType))
  S_AXI <> uCore.S_AXI

  val iWriteRequest = IO(Flipped(uCore.iWriteRequest.cloneType))
  iWriteRequest <> uCore.iWriteRequest

  val iPstateWriteRequest = IO(Flipped(uCore.iPstateWriteRequest.cloneType))
  iPstateWriteRequest <> uCore.iPstateWriteRequest

  val iReadRequest = IO(Flipped(uCore.iReadRequest.cloneType))
  iReadRequest <> uCore.iReadRequest

  val iReadPStateRequest = IO(Flipped(uCore.iReadPStateRequest.cloneType))
  iReadPStateRequest <> uCore.iReadPStateRequest

  val oReadPStateReply = IO(Output(uCore.oReadPStateReply.cloneType))
  oReadPStateReply := uCore.oReadPStateReply

  val oReadReply = IO(Output(uCore.oReadReply.cloneType))
  oReadReply := uCore.oReadReply

  // Converters, between 512bit UInt and 64bit Vector UInt.
  val i512Bits = IO(Input(UInt(512.W)))
  val oVecUInt = IO(Output(Vec(8, UInt(64.W))))

  oVecUInt := i512Bits.asTypeOf(oVecUInt.cloneType)

  val iVecUInt = IO(Input(Vec(8, UInt(64.W))))
  val o512Bits = IO(Output(UInt(512.W)))

  o512Bits := iVecUInt.asUInt

}

class TransplantBRAMTest extends AnyFreeSpec with ChiselScalatestTester {
  // helper functions for TransplantBRAM
  implicit class TransplantBRAMHelper(dut: TransplantBRAMTestWrapper) {
    def tick() = {
      dut.clock.step()
    }

    def sendContextThroughAXI(threadID: Int, context: Seq[UInt]) = {
      // do an AXI write transactions.
      dut.S_AXI.aw.awid.poke(0.U)
      dut.S_AXI.aw.awburst.poke(1.U)
      dut.S_AXI.aw.awaddr.poke((threadID * 512).U) // thread 13.
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

    def expectReadingContextThroughAXI(threadID: Int, context : Seq[UInt]) = {
      // First, send the read request.
      dut.S_AXI.ar.araddr.poke((threadID * 512).U)
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
      TargetDirAnnotation("test/transplant/TransplantBRAMTest/AXIToNormal"),
      WriteVcdAnnotation
    )
    test(new TransplantBRAMTestWrapper).withAnnotations(anno) { dut =>

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
      dut.iReadRequest.bits.threadID.poke(13.U)

      // Send the read request
      timescope {
        dut.iReadRequest.bits.registerIndex.poke(1.U)
        dut.iReadRequest.ready.expect(true.B)
        dut.iReadRequest.valid.poke(true.B)

        dut.tick()
      }

      // Find the result!
      dut.oReadReply.expect(1.U)

      dut.oReadPStateReply.PC.expect(0.U)
      dut.oReadPStateReply.flags.expect(new PStateFlags().Lit(
        _.NZCV -> 1.U,
        _.isException -> false.B,
        _.isUndef -> false.B,
        _.isICountDepleted -> false.B
      ))
      dut.oReadPStateReply.icount.expect(2.U)
      dut.oReadPStateReply.icountBudget.expect(0.U)
    }
  }

  "Normal -> AXI" in {
    val anno = Seq(
      VerilatorBackendAnnotation,
      TargetDirAnnotation("test/transplant/TransplantBRAMTest/NormalToAXI"),
      WriteVcdAnnotation
    )
    test(new TransplantBRAMTestWrapper).withAnnotations(anno) { dut =>
      // write the registers.
      dut.iWriteRequest.bits.registerIndex.poke(18.U)
      dut.iWriteRequest.bits.threadID.poke(11.U)
      dut.iWriteRequest.bits.value.poke(0x7777F823.U)
      timescope {
        dut.iWriteRequest.ready.expect(true.B)
        dut.iWriteRequest.valid.poke(true.B)
        dut.tick()
      }

      // Then push another read request just near by. It should not pollute the value.
      dut.iWriteRequest.bits.registerIndex.poke(17.U)
      dut.iWriteRequest.bits.threadID.poke(11.U)
      dut.iWriteRequest.bits.value.poke(100.U)
      timescope {
        dut.iWriteRequest.ready.expect(true.B)
        dut.iWriteRequest.valid.poke(true.B)
        dut.tick()
      }

      // This write request is sent out. Then let us read it from the read port.
      dut.iReadRequest.bits.registerIndex.poke(18.U)
      dut.iReadRequest.bits.threadID.poke(11.U)
      timescope {
        dut.iReadRequest.ready.expect(true.B)
        dut.iReadRequest.valid.poke(true.B)
        dut.tick()
      }
      dut.oReadReply.expect(0x7777F823.U)

      // prepare for the next request
      dut.iReadRequest.bits.registerIndex.poke(17.U)
      dut.iReadRequest.bits.threadID.poke(11.U)
      timescope {
        dut.iReadRequest.ready.expect(true.B)
        dut.iReadRequest.valid.poke(true.B)
        dut.tick()
      }
      dut.oReadReply.expect(100.U)

      // Now we will commit some pState to the BRAM
      dut.iPstateWriteRequest.bits.threadID.poke(11.U)
      dut.iPstateWriteRequest.bits.state.PC.poke(128.U) // 32
      dut.iPstateWriteRequest.bits.state.flags.NZCV.poke(1.U) // 33
      dut.iPstateWriteRequest.bits.state.flags.isException.poke(false.B)
      dut.iPstateWriteRequest.bits.state.flags.isICountDepleted.poke(true.B)
      dut.iPstateWriteRequest.bits.state.flags.isUndef.poke(false.B)
      dut.iPstateWriteRequest.bits.state.icount.poke(0xB.U) // 34
      dut.iPstateWriteRequest.bits.state.icountBudget.poke(0xA.U)
      timescope {
        dut.iPstateWriteRequest.ready.expect(true.B)
        dut.iPstateWriteRequest.valid.poke(true.B)
        dut.tick()
      }

      // and immediately, we write a register to 35, and then read them out.
      dut.iWriteRequest.bits.registerIndex.poke(35.U)
      dut.iWriteRequest.bits.threadID.poke(11.U)
      dut.iWriteRequest.bits.value.poke(1024.U)
      timescope {
        dut.iWriteRequest.ready.expect(true.B)
        dut.iWriteRequest.valid.poke(true.B)
        dut.tick()
      }

      // Then, I will read all of them out.
      // First, the read 35.
      dut.iReadRequest.bits.registerIndex.poke(35.U)
      dut.iReadRequest.bits.threadID.poke(11.U)
      timescope {
        dut.iReadRequest.ready.expect(true.B)
        dut.iReadRequest.valid.poke(true.B)
        dut.tick()
      }
      dut.oReadReply.expect(1024.U)

      // Then, I read the pstate.
      dut.iReadPStateRequest.bits.poke(11.U)
      timescope {
        dut.iReadPStateRequest.ready.expect(true.B)
        dut.iReadPStateRequest.valid.poke(true.B)
        dut.tick()
      }
      dut.oReadPStateReply.PC.expect(128.U)
      dut.oReadPStateReply.flags.NZCV.expect(1.U)
      dut.oReadPStateReply.flags.isException.expect(false.B)
      dut.oReadPStateReply.flags.isICountDepleted.expect(true.B)
      dut.oReadPStateReply.flags.isUndef.expect(false.B)
      dut.oReadPStateReply.icount.expect(0xB.U)
      dut.oReadPStateReply.icountBudget.expect(0xA.U)


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

