package antmicro.util

import chisel3._
import chisel3.experimental._
import chisel3.util._
import chiseltest._

import antmicro.CSR.CSRBusBundle

object CSRDrivers {
  implicit class CSRBusBundleDriver(target: CSRBusBundle)(implicit clock: Clock) {
    def init() = {
      target.addr.poke(0.U)
      target.dataOut.poke(0.U)
      target.read.poke(false.B)
      target.write.poke(false.B)
    }
    def readReg(addr: BigInt): BigInt = {
      target.addr.poke(addr.U)
      target.dataOut.poke(0.U)
      target.read.poke(true.B)
      target.write.poke(false.B)
      clock.step()
      target.dataIn.peek().litValue
    }

    def writeReg(addr: BigInt, value: BigInt): Unit = {
      target.addr.poke(addr.U)
      target.dataOut.poke(value.U)
      target.read.poke(false.B)
      target.write.poke(true.B)
      clock.step()
    }
  }
}