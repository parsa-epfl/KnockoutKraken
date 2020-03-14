package armflex.util

import chisel3._
import chisel3.experimental._
import chisel3.util._
import chiseltest._

import armflex.util._

object BRAMPortDriver {
  implicit class BRAMPortDriver(target: BRAMPort)(implicit clock: Clock) {
    def wrBRAM(bits:BigInt, offst: BigInt) = {
      target.EN.poke(true.B)
      target.WE.poke("b11111111".U) // Assumes 64bit
      if(target.cfg.isAXI) {
        target.ADDR.poke((offst << 2).U)
      } else {
        target.ADDR.poke(offst.U)
      }
      target.DI.poke(bits.U)
      clock.step()
      target.EN.poke(false.B)
      target.WE.poke(0.U)
    }

    def wrBRAM(bits:BigInt, offst: BigInt, strobe: BigInt) = {
      target.EN.poke(true.B)
      target.WE.poke(strobe.U)
      if(target.cfg.isAXI) {
        target.ADDR.poke((offst << 2).U)
      } else {
        target.ADDR.poke(offst.U)
      }
      target.DI.poke(bits.U)
      clock.step()
      target.EN.poke(false.B)
      target.WE.poke(0.U)
    }

    def rdBRAM(offst:BigInt): BigInt = {
      target.EN.poke(true.B)
      target.WE.poke(false.B)
      if(target.cfg.isAXI) {
        target.ADDR.poke((offst << 2).U)
      } else {
        target.ADDR.poke(offst.U)
      }
      clock.step()
      target.EN.poke(false.B)
      val uint32 = target.DO.peek
      return uint32.litValue()
    }
  }
}
