package armflex

import chisel3._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._

import org.scalatest.freespec.AnyFreeSpec
import chiseltest._
import chiseltest.internal._

object ArmflexStructsLits {
    object PStateRegs {
        def apply(): PStateRegs = apply(0.U, 0.U, 0.U)
        def apply(pc: UInt, asid: UInt, execMode: UInt): PStateRegs = new PStateRegs().Lit(
            _.PC -> pc,
            _.asid -> asid,
            _.asid_unused -> 0.U,
            _.flags -> new PStateFlags().Lit(
                _.NZCV -> 0.U,
                _.execMode -> execMode,
                _.isException -> false.B,
                _.isICountDepleted -> false.B,
                _.isUndef -> false.B
            ),
            _.icount -> 0.U,
            _.icountExecuted -> 0.U,
            _.icountBudget -> 0.U
        )
    }
}

object GeneralDrivers{
    implicit class RFileIOWrPortDriver(target: RFileIO.WRPort)(implicit clock: Clock) {
        def init() = {
            target.addr.poke(0.U)
            target.data.poke(0.U)
            target.tag.poke(0.U)
            target.en.poke(false.B)
        }

        def wr(reg: UInt, data: UInt) = timescope {
            target.addr.poke(reg)
            target.data.poke(data)
            target.en.poke(true.B)
            clock.step()
        }
    }
}