package armflex.util

import chisel3._
import chisel3.experimental._
import chisel3.util._
import chiseltest._

import armflex.util._

import armflex.util.SoftwareStructs._
import armflex.Trans2State._

object BRAMPortDriver {
  implicit class BRAMPortDriver(target: BRAMPort)(implicit clock: Clock) {
    def init: Unit = {
      target.EN.poke(false.B)
      target.WE.poke(0.U)
      target.ADDR.poke(0.U)
      target.DI.poke(0.U)
    }

    def wr(tag: Int, state: PState): Unit = {
      val baseOffset = tag << log2Ceil(ARCH_MAX_OFFST)
      for (i <- ARCH_XREGS_OFFST until ARCH_XREGS_OFFST + 32) {
        target.wr(state.xregs(i), baseOffset + i)
      }
      target.wr(state.pc, baseOffset + ARCH_PC_OFFST)
      target.wr(state.sp, baseOffset + ARCH_SP_OFFST)
      target.wr(state.nzcv, baseOffset + ARCH_PSTATE_OFFST)
    }

    def wr(bits:BigInt, offst: BigInt) = {
      target.EN.poke(true.B)
      target.WE.poke("b11111111".U) // Assumes 64bit
      if(target.params.isAXI) {
        target.ADDR.poke((offst << 2).U)
      } else {
        target.ADDR.poke(offst.U)
      }
      target.DI.poke(bits.U)
      clock.step()
      target.EN.poke(false.B)
      target.WE.poke(0.U)
    }

    def wr(bits:BigInt, offst: BigInt, strobe: BigInt) = {
      target.EN.poke(true.B)
      target.WE.poke(strobe.U)
      if(target.params.isAXI) {
        target.ADDR.poke((offst << 2).U)
      } else {
        target.ADDR.poke(offst.U)
      }
      target.DI.poke(bits.U)
      clock.step()
      target.EN.poke(false.B)
      target.WE.poke(0.U)
    }

    def rdState(tag: Int): PState = {
      val baseOffset = tag << log2Ceil(ARCH_MAX_OFFST)
      val xregs = for (reg <- ARCH_XREGS_OFFST until ARCH_XREGS_OFFST + 32) yield {
        val regVal = target.rd(baseOffset + reg)
        regVal
      }
      val pc = target.rd(baseOffset + ARCH_PC_OFFST)
      val sp = target.rd(baseOffset + ARCH_SP_OFFST)
      val nzcv = target.rd(baseOffset + ARCH_PSTATE_OFFST)

      val pstate = new PState(xregs.toList: List[BigInt], pc: BigInt, sp: BigInt, nzcv.toInt: Int)
      pstate
    }

    def rd(offst:BigInt): BigInt = {
      target.EN.poke(true.B)
      target.WE.poke(false.B)
      if(target.params.isAXI) {
        target.ADDR.poke((offst << 2).U)
      } else {
        target.ADDR.poke(offst.U)
      }
      clock.step()
      target.EN.poke(false.B)
      val uint32 = target.DO.peek()
      return uint32.litValue
    }
  }
}
