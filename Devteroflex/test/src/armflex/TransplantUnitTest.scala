package armflex

import chisel3._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._

import org.scalatest.freespec.AnyFreeSpec
import chiseltest._
import chiseltest.internal._

import firrtl.options.TargetDirAnnotation

import chisel3.util.{Queue}
import armflex.PStateConsts._
import armflex.TransplantConsts._
import armflex.ArmflexStructsLits._
import antmicro.util.CSRDrivers.CSRBusBundleDriver
import armflex.util.SoftwareStructs
import armflex.GeneralDrivers._

import armflex.TransplantUnitDrivers.{
  TransBRAM2CpuIODrivers, CPU2TransDriver, Cpu2TransBramUnitTest
}


class Cpu2TransUnitTest extends AnyFreeSpec with ChiselScalatestTester {
  "Start transplant with normal execution" in {
    test(new Cpu2TransBramUnitTestDriver(32)).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("test/transplant/Cpu2TransTest"), 
      WriteVcdAnnotation)) {
        
        dut => 
          val xregs = for(reg <- 0 until 32) yield BigInt(reg * 15)
          val pstate = new PStateRegs().Lit(
            _.PC -> 0x100.U, _.asid -> 0x10.U, _.asid_unused -> 0.U,
            _.flags -> new PStateFlags().Lit(
              _.NZCV -> 3.U, _.isException -> false.B, _.isICountDepleted -> false.B,
              _.isUndef -> false.B, _.execMode -> PSTATE_FLAGS_EXECUTE_NORMAL.U),
            _.icount -> 0.U, _.icountBudget -> 0.U
          )
          dut.init()
          dut.startTransBram2Cpu(4, xregs, pstate)
    }
  }
}

object TransplantUnitDrivers {
  implicit class CPU2TransDriver(target: TransplantIO.CPU2Trans)(implicit clock: Clock) {
    def init() = {
      target.doneCPU.valid.poke(false.B)
      target.doneCPU.tag.poke(0.U)
      target.rfile_wr.init()
      target.pstate.poke(ArmflexStructsLits.PStateRegs.makeLit())
    }
    def sendCpu2Trans(thid: Int): Unit = {
      target.doneCPU.valid.poke(true.B)
      target.doneCPU.tag.poke(thid.U)
      clock.step()
    }
  }

  implicit class Trans2CpuDriver(target: TransplantIO.Trans2CPU)(implicit clock: Clock) {
    def init() = {
    }
  }
 

  implicit class TransBRAM2CpuIODrivers(target: TransBram2HostUnitIO.TransBRAM2CpuIO)(implicit clock: Clock) {
    def wr(thid: Int, xregs: Seq[BigInt]): Unit = {
      assert(xregs.size == 32)
      target.wr.thid.poke((1 << thid).U)
      for(reg <- 0 until xregs.size) {
        target.wr.xreg.req.enqueue(new TransBram2HostUnitIO.WrReq().Lit(
          _.regIdx -> reg.U, _.data -> xregs(reg).U))
      }
    }
  }

  implicit class Cpu2TransBramUnitTest(target: Cpu2TransBramUnitTestDriver) {
    implicit val clock: Clock = target.clock
    def init() = {
      target.S_CSR.init()
      target.cpu2trans.init()
      target.pstateRdRespValidate.initSink()
      target.pstateRdRespValidate.setSinkClock(clock)
      target.xregsRdRespValidate.initSink()
      target.xregsRdRespValidate.setSinkClock(clock)
      target.trans2cpu.init()
      target.enqXRegsRespQ.initSource()
      target.enqXRegsRespQ.setSourceClock(clock)
      target.enqPStateRespQ.initSource()
      target.enqPStateRespQ.setSourceClock(clock)
      target.transBram2Cpu.rd.xreg.req.initSink()
      target.transBram2Cpu.rd.pstate.req.initSink()
      target.transBram2Cpu.wr.xreg.req.initSink()
      target.transBram2Cpu.wr.pstate.req.initSink()
      target.transBram2Cpu.rd.xreg.req.setSourceClock(clock)
      target.transBram2Cpu.rd.pstate.req.setSourceClock(clock)
      target.transBram2Cpu.wr.xreg.req.setSourceClock(clock)
      target.transBram2Cpu.wr.pstate.req.setSourceClock(clock)
    }
    def setTrans2CpuStart(thid: Int) = timescope { target.transBram2Cpu.ctrl.setTrans2Cpu.poke((1 << thid).U); clock.step() }
    def clearTrans2CpuPending(thid: Int) = timescope { target.transBram2Cpu.ctrl.clearTrans2Host.poke((1 << thid).U); clock.step() }

    def hasRecvForceTrans(): Boolean = target.cpu2trans.forceTransplant.peek().litValue != 0
    def hasRecvSetStopCpu(): Boolean = target.cpu2trans.stopCPU.peek().litValue != 0
    def hasStallCpu2Trans(): Boolean = target.cpu2trans.stallPipeline.litToBoolean
    def hasStallTrans2Cpu(): Boolean = target.trans2cpu.stallPipeline.litToBoolean

    def pipeSendStateExpect(thid: Int, xregs: Seq[BigInt], pstate: PStateRegs): Unit = {
      assert(!target.cpu2trans.stallPipeline.litToBoolean)
      target.cpu2trans.pstate.poke(pstate)
      target.cpu2trans.rfile_wr.tag.poke(thid.U)
      for(reg <- 0 until xregs.size) {
        target.transBram2Cpu.wr.xreg.req.expectDequeue(new TransBram2HostUnitIO.WrReq().Lit(
          _.regIdx -> reg.U,
          _.data -> xregs(reg).U
        ))
      }
      for(reg <- 0 until xregs.size) {
        target.cpu2trans.rfile_wr.wr(reg.U, xregs(reg).U)
        clock.step()
      }
    }
    def prepareTransResp(thid: Int, xregs: Seq[BigInt], pstate: PStateRegs) = {
      target.xregsRdRespValidate.ready.poke(true.B)
      target.pstateRdRespValidate.ready.poke(true.B)
      for(reg <- 0 until xregs.size) {
        target.xregsRdRespValidate.expectDequeue(xregs(reg).U)
        target.enqXRegsRespQ.enqueueNow(xregs(reg).U)
      }
      target.enqPStateRespQ.enqueueNow(pstate)
      target.pstateRdRespValidate.expectDequeue(pstate)
    }
 
    def expectTrans2CpuState(thid: Int, xregs: Seq[BigInt], pstate: PStateRegs) = {
      target.transBram2Cpu.rd.xreg.req.ready.poke(true.B)
      target.transBram2Cpu.rd.pstate.req.ready.poke(true.B)
      for(reg <- 0 until xregs.size) {
        target.transBram2Cpu.rd.xreg.req.enqueueNow(
          new TransBram2HostUnitIO.RdReq().Lit(_.regIdx -> reg.U)
        )
      }
      target.transBram2Cpu.rd.pstate.req.enqueueNow(0.U)
      target.trans2cpu.pstate.valid.expect(true.B)
    }

    def startTransBram2Cpu(thid: Int, xregs: Seq[BigInt], pstate: PStateRegs) = {
      prepareTransResp(thid, xregs, pstate)
      setTrans2CpuStart(thid)
      expectTrans2CpuState(thid, xregs, pstate)
      target.trans2cpu.thread.expect(thid.U)
      if (pstate.flags.execMode.litValue == PSTATE_FLAGS_EXECUTE_WAIT) {
        clock.step()
        assert(target.S_CSR.readReg(TRANS_REG_OFFST_WAITING) == 1 << thid)
      } else if (pstate.flags.execMode.litValue == PSTATE_FLAGS_EXECUTE_NORMAL) {
        target.trans2cpu.start.expect(1.U << thid)
      } else if (pstate.flags.execMode.litValue == PSTATE_FLAGS_EXECUTE_SINGLESTEP) {
        target.trans2cpu.start.expect(1.U << thid)
        clock.step()
        assert(target.S_CSR.readReg(TRANS_REG_OFFST_STOP_CPU) == 1 << thid)
      }
    }
  }
}
    
class Cpu2TransBramUnitTestDriver(thidN: Int) extends Module {
  val dut = Module(new Cpu2TransBramUnit(thidN))
  val cpu2trans = IO(dut.cpu2trans.cloneType)
  val trans2cpu = IO(dut.trans2cpu.cloneType)
  val transBram2Cpu = IO(Flipped(dut.transBram2Cpu.cloneType))
  val S_CSR = IO(Flipped(dut.S_CSR.cloneType))
  cpu2trans <> dut.cpu2trans
  trans2cpu <> dut.trans2cpu
  transBram2Cpu <> dut.transBram2Cpu
  S_CSR <> dut.S_CSR

  // Buffer resp for testbench
  val xregsRespQ = Module(new Queue(transBram2Cpu.rd.xreg.resp.cloneType, 512, true, true))
  val xregsRdRespValidate = IO(xregsRespQ.io.deq.cloneType)
  val enqXRegsRespQ = IO(Flipped(xregsRespQ.io.enq.cloneType))
  xregsRespQ.io.enq <> enqXRegsRespQ
  xregsRespQ.io.deq.ready := RegNext(transBram2Cpu.rd.xreg.req.fire)

  xregsRdRespValidate.valid := xregsRespQ.io.deq.fire
  xregsRdRespValidate.bits := trans2cpu.rfile_wr.data
  dut.transBram2Cpu.rd.xreg.resp := xregsRespQ.io.deq.bits
  when(xregsRdRespValidate.valid) { 
    assert(trans2cpu.rfile_wr.en)
    assert(trans2cpu.rfile_wr.addr === RegNext(transBram2Cpu.rd.xreg.req.bits.regIdx))
    assert(trans2cpu.rfile_wr.tag === RegNext(transBram2Cpu.rd.thid))
    assert(trans2cpu.stallPipeline)
    assert(trans2cpu.busyTrans2Cpu)
  }

  val pstateRespQ = Module(new Queue(transBram2Cpu.rd.pstate.resp.cloneType, 8, true, true))
  val pstateRdRespValidate = IO(pstateRespQ.io.deq.cloneType)
  val enqPStateRespQ = IO(Flipped(pstateRespQ.io.enq.cloneType))
  pstateRespQ.io.enq <> enqPStateRespQ
  pstateRespQ.io.deq.ready := RegNext(transBram2Cpu.rd.pstate.req.fire)
  pstateRdRespValidate.valid := RegNext(pstateRespQ.io.deq.fire)
  pstateRdRespValidate.bits := trans2cpu.pstate.bits
  dut.transBram2Cpu.rd.pstate.resp := pstateRespQ.io.deq.bits
  when(xregsRdRespValidate.valid) {
    assert(trans2cpu.pstate.valid)
  }
}