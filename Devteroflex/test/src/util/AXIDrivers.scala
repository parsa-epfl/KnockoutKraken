package armflex.util

import chisel3._
import chisel3.experimental._
import chisel3.util._
import chiseltest._

object AXIDrivers {
  implicit class AXI4LiteDriver(target: antmicro.Bus.AXI4Lite)(implicit clock: Clock) {
    def init(): Unit = {
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
    // TODO Implement AXI4 chisel testers Protocol
    def rd(addr: UInt): UInt = {
      target.r.rdata
    }
    def wr(addr: UInt, data: UInt): Unit = {
    }
  }
}
