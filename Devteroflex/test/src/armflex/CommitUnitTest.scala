package armflex

import chisel3._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._

import org.scalatest.freespec.AnyFreeSpec
import chiseltest._
import chiseltest.internal._

import firrtl.options.TargetDirAnnotation

import chisel3.util.{Queue, Valid}
import armflex.PStateConsts._
import armflex.TransplantConsts._
import armflex.ArmflexStructsLits._
import antmicro.util.CSRDrivers.CSRBusBundleDriver
import armflex.util.SoftwareStructs
import armflex.GeneralDrivers._

class CommitUnitTest extends AnyFreeSpec with ChiselScalatestTester {
  import CommitUnitDrivers._
  val pc = 0x404
  val asid = 0x10
  val execMode = PSTATE_FLAGS_EXECUTE_NORMAL

  "CommitUnit should not writeback with exception" in {
    test(new CommitUnit(32)).withAnnotations(
    Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/transplant/Cpu2TransTest/Normal"), WriteVcdAnnotation)
    ) {
      dut =>  
        dut.init()
        dut.setCurrPState(PStateRegs(pc.U, asid.U, execMode.U))
        var running = true
        val checkWriteBacks = fork {
          while(running) {
            dut.expectNoWriteBack()
            dut.clock.step(1)
          }
        }

        dut.enq.enqueueNow(CommitInst(tag = 2.U, inst = 0xABCD.U,
          rdRes = Seq((true.B, 3.U, 0x400.U), (true.B, 4.U, 0x300.U), (true.B, 5.U, 0x100.U)),
          nzcv = (true.B, 0x4.U), br_taken = (false.B, (pc + 0x40).U), exceptions = (true.B, 1.U),
          undef = false.B, is32bit = false.B)(dut.thidN))

        dut.waitCommit()
        running = false
        checkWriteBacks.join()
    }
  }

  "CommitUnit should not writeback with undef" in {
    test(new CommitUnit(32)).withAnnotations(
    Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/transplant/Cpu2TransTest/Normal"), WriteVcdAnnotation)
    ) {
      dut =>  
        dut.init()
        dut.setCurrPState(PStateRegs(pc.U, asid.U, execMode.U))
        var running = true
        val checkWriteBacks = fork {
          while(running) {
            dut.expectNoWriteBack()
            dut.clock.step(1)
          }
        }

        dut.enq.enqueueNow(CommitInst(tag = 2.U, inst = 0xABCD.U,
          rdRes = Seq((true.B, 3.U, 0x400.U), (true.B, 4.U, 0x300.U), (true.B, 5.U, 0x100.U)),
          nzcv = (true.B, 0x4.U), br_taken = (true.B, (pc + 0x40).U), exceptions = (false.B, 1.U),
          undef = true.B, is32bit = false.B)(dut.thidN))

        dut.waitCommit()
        running = false
        checkWriteBacks.join()
    }
  }
}

object CommitUnitDrivers {
  implicit class CommitUnitDriver(target: CommitUnit) {
    implicit val clock: Clock = target.clock
    def init() = {
      target.enq.initSource()
      target.enq.setSourceClock(clock)
      target.commit.archstate.ready.poke(true.B)
      target.deq.initSink()         
      target.deq.setSinkClock(clock)
      target.deq.ready.poke(true.B)
    }

    def setCurrPState(currPState: PStateRegs) = {
      target.commit.archstate.pstate.curr.poke(currPState)
    }

    def expectNoWriteBack(): Unit = {
      target.commit.archstate.wr.en.expect(false.B)
      target.commit.archstate.pstate.next.PC.expect(target.commit.archstate.pstate.curr.PC.peek())
      target.commit.archstate.pstate.next.flags.NZCV.expect(target.commit.archstate.pstate.curr.flags.NZCV.peek())
 
 }

    def waitCommit() = {
      while(!target.commit.commited.valid.peek().litToBoolean) {
        target.clock.step(1)
      }
      target.clock.step(1)
    }
  }

  object CommitInst {
    def apply (
      tag: UInt, inst: UInt,
      rdRes: Seq[(Bool, UInt, UInt)],
      nzcv: (Bool, UInt), br_taken: (Bool, UInt), exceptions: (Bool, UInt),
      undef: Bool, is32bit: Bool
    )(thidN: Int): CommitInst = new CommitInst(thidN).Lit(
      _.tag -> tag, _.inst -> inst,
      _.rd(0).valid -> rdRes(0)._1, _.rd(1).valid -> rdRes(1)._1, _.rd(2).valid -> rdRes(2)._1,
      _.rd(0).bits -> rdRes(0)._2, _.rd(1).bits -> rdRes(1)._2, _.rd(2).bits -> rdRes(2)._2,
      _.res(0) -> rdRes(0)._3, _.res(1) -> rdRes(1)._3, _.res(2) -> rdRes(2)._3,
      _.nzcv.valid -> nzcv._1, _.nzcv.bits -> nzcv._2,
      _.br_taken.valid -> br_taken._1, _.br_taken.bits -> br_taken._2,
      _.exceptions.valid -> exceptions._1, _.exceptions.bits -> exceptions._2,
      _.undef -> undef, _.is32bit -> is32bit
    )
  }
}