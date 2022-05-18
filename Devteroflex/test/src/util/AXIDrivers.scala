package armflex.util

import chisel3._
import chisel3.experimental._
import chisel3.util._
import chiseltest._

import antmicro.Bus.AXI4

object AXIDrivers {
  implicit class AXI4LiteDriver(target: antmicro.Bus.AXI4Lite)(implicit clock: Clock) {
    def initSlave(): Unit = {
      target.aw.awaddr.poke(0.U)
      // target.aw.awprot.poke(0.U)
      target.aw.awvalid.poke(false.B)

      target.w.wdata.poke(0.U)
      target.w.wstrb.poke(0.U)
      target.w.wvalid.poke(false.B)

      target.b.bready.poke(false.B)

      target.ar.arvalid.poke(false.B)
      target.ar.araddr.poke(0.U)
      // target.ar.arprot.poke(0.U)

      target.r.rready.poke(false.B)
    }

    def rd(addr: UInt): UInt = {
      // Send address
      target.ar.arvalid.poke(true.B)
      target.ar.araddr.poke(addr)
      while(!target.ar.arready.peek().litToBoolean){
        clock.step(1)
      } 
      clock.step(1)
      target.ar.arvalid.poke(false.B)

      target.r.rready.poke(true.B)
      var resp = target.r.rdata.peek()
      while(!target.r.rvalid.peek().litToBoolean) {
        resp = target.r.rdata.peek()
        clock.step(1)
      } 
      clock.step(1)
      target.r.rready.poke(false.B)
      resp
    }

    def wr(addr: UInt, data: UInt): Unit = {
      // Send address
      target.aw.awvalid.poke(true.B)
      target.aw.awaddr.poke(addr)
      while(!target.aw.awready.peek().litToBoolean){
        clock.step(1)
      } 
      clock.step(1)
      target.aw.awvalid.poke(false.B)

      // Send write
      target.w.wvalid.poke(true.B)
      target.w.wdata.poke(data)
      while(!target.w.wready.peek().litToBoolean){
        clock.step(1)
      } 
      clock.step(1)
      target.w.wvalid.poke(false.B)

      // WriteResp
      target.b.bready.poke(true.B)
      while(!target.b.bvalid.peek().litToBoolean) {
        clock.step(1)
      } 
      clock.step(1)
      target.b.bready.poke(false.B)
    }
  }
  implicit class AXI4Driver(target: antmicro.Bus.AXI4)(implicit clock: Clock) {
    // ------------------ Slave  (Sink) ---------------
 
    def initSlave() = {
      target.aw.awaddr.poke(0.U)
      target.aw.awburst.poke(1.U)
      target.aw.awlen.poke(0.U)
      target.aw.awsize.poke(log2Ceil(target.dataWidth/8).U)
      target.aw.awvalid.poke(false.B)

      target.w.wdata.poke(0.U)
      target.w.wlast.poke(false.B)
      target.w.wstrb.poke(((BigInt(1) << (target.dataWidth/8)) - 1).U)
      target.w.wvalid.poke(false.B)
 
      target.b.bready.poke(false.B)

      target.ar.araddr.poke(0.U)
      target.ar.arburst.poke(1.U)
      target.ar.arlen.poke(0.U)
      target.ar.arsize.poke(log2Ceil(target.dataWidth/8).U)
      target.ar.arvalid.poke(false.B)

      target.r.rready.poke(false.B)
    }

    private def wrAddr(addr: UInt, burstS: Int) = timescope {
      target.aw.awaddr.poke(addr)
      target.aw.awburst.poke(1.U)
      target.aw.awlen.poke((burstS - 1).U)
      target.aw.awsize.poke(log2Ceil(target.dataWidth/8).U)
      target.aw.awvalid.poke(true.B)
      fork.withRegion(Monitor) {
          while (target.aw.awready.peek().litToBoolean == false) {
            clock.step(1)
          }
        }.joinAndStep(clock)
    }

    private def wrData(data: Seq[UInt]) = timescope {
      target.w.wstrb.poke(((BigInt(1) << (target.dataWidth/8)) - 1).U)
      target.w.wvalid.poke(true.B)
      for(idx <- 0 until data.size) {
        target.w.wdata.poke(data(idx))
        if(idx == data.size - 1) {
          target.w.wlast.poke(true.B)
        }
        fork.withRegion(Monitor) {
            while (target.w.wready.peek().litToBoolean == false) {
              clock.step(1)
            }
        }.joinAndStep(clock)
      }
    }

    private def wrB(expectedResp: UInt) = timescope {
      target.b.bready.poke(true.B)
      fork.withRegion(Monitor) {
        while (target.b.bvalid.peek().litToBoolean == false) {
          clock.step(1)
        }
        target.b.bresp.expect(expectedResp)
      }.joinAndStep(clock)
    }
    
    private def rdAddr(addr: UInt, burstS: Int) = timescope {
      target.ar.araddr.poke(addr)
      target.ar.arburst.poke(1.U)
      target.ar.arlen.poke((burstS - 1).U)
      target.ar.arsize.poke(log2Ceil(target.dataWidth/8).U)
      target.ar.arvalid.poke(true.B)
      fork.withRegion(Monitor) {
        while (target.ar.arready.peek().litToBoolean == false) {
          clock.step(1)
        }
      }.joinAndStep(clock)
    }

    private def rdData(expectData: Seq[UInt], burstS: Int, expectRespFlag: Int): Seq[UInt] = {
      if(!expectData.isEmpty) assert(expectData.size == burstS)
      var resp: Seq[UInt] = Seq()
      var errorMsg: BigInt = expectRespFlag
      timescope {
        target.r.rready.poke(true.B)
        for(idx <- 0 until burstS) {
          fork.withRegion(Monitor) {
            while (target.r.rvalid.peek().litToBoolean == false) {
              clock.step(1)
            }
            if(idx == burstS - 1) target.r.rlast.expect(true.B)
            val rresp = target.r.rresp.peek().litValue
            if(!expectData.isEmpty && rresp == AXI4.OKAY) target.r.rdata.expect(expectData(idx))
            if(rresp != expectRespFlag) {
              errorMsg = rresp
            }
            resp = resp :+ target.r.rdata.peek()
          }.joinAndStep(clock)
        }
      }
      assert(errorMsg == expectRespFlag)
      resp
    }
    
    def rd(addr: UInt, expectedData: Seq[UInt], burstS: Int, expectRespFlag: Int): Seq[UInt] = {
      rdAddr(addr, burstS)
      rdData(expectedData, burstS, expectRespFlag)
    }

    def rd(addr: UInt, expectedData: Seq[UInt], expectedRespFlag: Int): Seq[UInt] = {
      rd(addr, expectedData, expectedData.size, expectedRespFlag)
    }

    def rd(addr: UInt, expectedData: Seq[UInt]): Seq[UInt] = {
      rd(addr, expectedData, expectedData.size, AXI4.OKAY)
    }

    def rd(addr: UInt, burstS: Int): Seq[UInt] = {
      rd(addr, Nil, burstS, AXI4.OKAY)
    }

    def wr(addr: UInt, data: Seq[UInt], expectRespFlag: Int): Unit = {
      wrAddr(addr, data.size)
      wrData(data)
      wrB(expectRespFlag.U)
    }

    def wr(addr: UInt, data: Seq[UInt]): Unit = {
      wr(addr, data, AXI4.OKAY)
    }

    // ------------------ Master (Source) ---------------
    def initMaster() = {
      target.aw.awready.poke(false.B)

      target.w.wready.poke(false.B)

      target.b.bresp.poke(AXI4.OKAY.U)
      target.b.bvalid.poke(false.B)

      target.ar.arready.poke(false.B)

      target.r.rdata.poke(0.U)
      target.r.rlast.poke(false.B)
      target.r.rresp.poke(AXI4.OKAY.U)
      target.r.rvalid.poke(false.B)
    }

    private def expectRdAddr(expectAddr: UInt, burstS: Int) = timescope {
      target.ar.arready.poke(true.B)
      fork.withRegion(Monitor) {
        while (target.ar.arvalid.peek().litToBoolean == false) {
          clock.step(1)
        }
        target.ar.araddr.expect(expectAddr)
        target.ar.arburst.expect(1.U)
        target.ar.arlen.expect((burstS - 1).U)
        target.ar.arsize.expect(log2Ceil(target.dataWidth/8).U)
      }.joinAndStep(clock)
    }

    private def rdRespData(respData: Seq[UInt]) = timescope {
      for(idx <- 0 until respData.size) {
        target.r.rdata.poke(respData(idx))
        target.r.rresp.poke(AXI4.OKAY.U)
        target.r.rlast.poke((idx == respData.size - 1).B)
        target.r.rvalid.poke(true.B)
        fork.withRegion(Monitor) {
         while (target.r.rready.peek().litToBoolean == false) {
            clock.step(1)
          }
        }.joinAndStep(clock)
      }
    }

    def expectRd(respData: Seq[UInt], expectAddr: UInt) = {
      expectRdAddr(expectAddr, respData.size)
      rdRespData(respData)
    }

    private def expectWrAddr(expectAddr: UInt, burstS: Int) = timescope {
      target.aw.awready.poke(true.B)
      fork.withRegion(Monitor) {
        while (target.aw.awvalid.peek().litToBoolean == false) {
          clock.step(1)
        }
        target.aw.awaddr.expect(expectAddr)
        target.aw.awburst.expect(1.U)
        target.aw.awlen.expect((burstS - 1).U)
        target.aw.awsize.expect(log2Ceil(target.dataWidth/8).U)
      }.joinAndStep(clock)
    }

    private def wrExpectData(data: Seq[UInt]) = timescope {
      target.w.wready.poke(true.B)
      for(idx <- 0 until data.size) {
        fork.withRegion(Monitor) {
          while (target.w.wvalid.peek().litToBoolean == false) {
             clock.step(1)
           }
          target.w.wdata.expect(data(idx))
          target.w.wlast.expect((idx == data.size - 1).B)
          target.w.wvalid.expect(true.B)
        }.joinAndStep(clock)
      }
    }

    private def wrRespB() = timescope {
      target.b.bvalid.poke(true.B)
      target.b.bresp.poke(AXI4.OKAY.U)
      fork.withRegion(Monitor) {
        while (target.b.bready.peek().litToBoolean == false) {
          clock.step(1)
        }
      }.joinAndStep(clock)
    }

    def expectWr(data: Seq[UInt], expectAddr: UInt) = {
      expectWrAddr(expectAddr, data.size)
      wrExpectData(data)
      wrRespB()
    }
  }
}
