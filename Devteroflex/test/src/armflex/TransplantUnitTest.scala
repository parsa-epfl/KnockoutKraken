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
  val thid = 4
  val xregs = for(reg <- 0 until 32) yield BigInt(reg * 15)

  "Start transplant with normal execution" in {
    test(new Cpu2TransBramUnitTestDriver(32)).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("test/transplant/Cpu2TransTest/Normal"), 
      WriteVcdAnnotation)) {
        dut => 
          dut.init()
          val pstate = PStateRegs(0x100.U, 0x10.U, PSTATE_FLAGS_EXECUTE_NORMAL.U)
          dut.startTransBram2Cpu(thid, xregs, pstate)
      }
    }
    "Start transplant with singlestep execution" in {
      test(new Cpu2TransBramUnitTestDriver(32)).withAnnotations(Seq(
        VerilatorBackendAnnotation, TargetDirAnnotation("test/transplant/Cpu2TransTest/Singlestep"), 
        WriteVcdAnnotation)) {
          dut => 
            dut.init()
            val pstate = PStateRegs(0x100.U, 0x10.U, PSTATE_FLAGS_EXECUTE_SINGLESTEP.U)
            dut.startTransBram2Cpu(thid, xregs, pstate)
            dut.clock.step(3)
      }
    }

    "Start transplant with wait execution" in {
    test(new Cpu2TransBramUnitTestDriver(32)).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("test/transplant/Cpu2TransTest/Wait"), 
      WriteVcdAnnotation)) {
        dut => 
          dut.init()
          val pstate = PStateRegs(0x100.U, 0x10.U, PSTATE_FLAGS_EXECUTE_WAIT.U)
          dut.startTransBram2Cpu(thid, xregs, pstate)
          dut.wrCSRCmd(TRANS_REG_OFFST_START, 1 << thid)
          dut.clock.step()
          dut.trans2cpu.start.valid.expect(true.B)
          dut.trans2cpu.start.bits.expect(thid.U)
          dut.clock.step()
          assert(dut.rdCSRCmd(TRANS_REG_OFFST_WAITING) == 0)
    }
  }
    "Send start without state available" in {
    test(new Cpu2TransBramUnitTestDriver(32)).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("test/transplant/Cpu2TransTest/WaitNotEnabled"), 
      WriteVcdAnnotation)) {
        dut => 
          dut.init()
          dut.clock.step()
          assert(dut.rdCSRCmd(TRANS_REG_OFFST_WAITING) == 0)
          dut.wrCSRCmd(TRANS_REG_OFFST_START, 1 << thid)
          dut.clock.step()
          dut.trans2cpu.start.valid.expect(false.B)
          dut.clock.step()
    }
  }

  "Check Running CSR" in {
    test(new Cpu2TransBramUnitTestDriver(32)).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("test/transplant/Cpu2TransTest/RunningCSR"), 
      WriteVcdAnnotation)) {
        dut => 
          implicit val clock: Clock = dut.clock
          dut.init()
          assert(dut.rdCSRCmd(TRANS_REG_OFFST_RUNNING) == 0)
          val pstate = PStateRegs(0x100.U, 0x10.U, PSTATE_FLAGS_EXECUTE_SINGLESTEP.U)
          dut.startTransBram2Cpu(thid, xregs, pstate)
          dut.clock.step()
          assert(dut.rdCSRCmd(TRANS_REG_OFFST_RUNNING) != 0)
          dut.clock.step()
          dut.cpu2trans.sendCpu2Trans(thid)
          dut.clock.step()
          assert(dut.rdCSRCmd(TRANS_REG_OFFST_RUNNING) == 0)
    }
  }

  "Multiple writing is prohibit" in {
    test(new TransBram2HostUnit(32)).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      TargetDirAnnotation("test/transplant/Cpu2TransTest/multi-write-prohibit"),
      WriteFstAnnotation
    )) { dut =>
      dut.wrPort.pstate.req.ready.expect(true.B)
      timescope {
        dut.wrPort.pstate.req.valid.poke(true.B)
        dut.wrPort.xreg.req.ready.expect(false.B)
      }
    }
  }
}

object TransplantUnitDrivers {
  implicit class CPU2TransDriver(target: TransplantIO.CPU2Trans)(implicit clock: Clock) {
    def init() = {
      target.doneCPU.valid.poke(false.B)
      target.doneCPU.bits.poke(0.U)
      target.rfile_wr.init()
      target.pstate.poke(ArmflexStructsLits.PStateRegs())
    }
    def sendCpu2Trans(thid: Int): Unit = {
      target.doneCPU.valid.poke(true.B)
      target.doneCPU.bits.poke(thid.U)
      clock.step()
      target.doneCPU.valid.poke(false.B)
    }
  }

  implicit class Trans2CpuDriver(target: TransplantIO.Trans2CPU)(implicit clock: Clock) {
    def init() = { }
  }

  implicit class TransBRAM2CpuIODrivers(target: TransBram2HostUnitIO.TransBRAM2CpuIO)(implicit clock: Clock) {
    def wr(thid: Int, xregs: Seq[BigInt]): Unit = {
      assert(xregs.size == 32)
      target.wr.xreg.thid.poke((1 << thid).U)
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
    def wrCSRCmd(reg: Int, value: BigInt) = target.S_CSR.writeReg(reg, value)
    def rdCSRCmd(reg: Int): BigInt = target.S_CSR.readReg(reg)

    def recvCpu2TransReq() = target.transBram2Cpu.wr.pstate.req.ready.poke(true.B)

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
      println("Preparing state responses")
      target.enqXRegsRespQ.enqueueSeq(xregs.map(_.U))
      target.enqPStateRespQ.enqueue(pstate)
      println(s"Finished preparing responses")
    }
 
    def expectTrans2CpuState(thid: Int, xregs: Seq[BigInt], pstate: PStateRegs) = {
      target.transBram2Cpu.rd.xreg.req.setSinkClock(clock)
      target.transBram2Cpu.rd.pstate.req.setSinkClock(clock)
      for(reg <- 0 until xregs.size) {
        target.transBram2Cpu.rd.xreg.req.expectDequeue(
          new TransBram2HostUnitIO.RdReq().Lit(_.regIdx -> reg.U)
        )
      }
      target.transBram2Cpu.rd.pstate.req.ready.poke(true.B)
      target.transBram2Cpu.rd.pstate.req.valid.expect(true.B)
      clock.step()
      target.trans2cpu.thid.expect(thid)
      target.trans2cpu.pstate.valid.expect(true.B)
      clock.step()
      target.trans2cpu.thid.expect(thid)
      // Done state now
    }

    def startTransBram2Cpu(thid: Int, xregs: Seq[BigInt], pstate: PStateRegs) = {
      println("Starting transplant transaction")
      prepareTransResp(thid, xregs, pstate)
      setTrans2CpuStart(thid)
      expectTrans2CpuState(thid, xregs, pstate)
      target.trans2cpu.thid.expect(thid.U)
      if (pstate.flags.execMode.litValue == PSTATE_FLAGS_EXECUTE_WAIT) {
        clock.step()
        assert(target.S_CSR.readReg(TRANS_REG_OFFST_WAITING) == 1 << thid)
      } else if (pstate.flags.execMode.litValue == PSTATE_FLAGS_EXECUTE_NORMAL) {
        target.trans2cpu.start.valid.expect(true.B)
        target.trans2cpu.start.bits.expect(thid.U)
        clock.step()
        target.trans2cpu.start.valid.expect(false.B)
      } else if (pstate.flags.execMode.litValue == PSTATE_FLAGS_EXECUTE_SINGLESTEP) {
        target.trans2cpu.start.valid.expect(true.B)
        target.trans2cpu.start.bits.expect(thid.U)
        clock.step()
        assert(target.S_CSR.readReg(TRANS_REG_OFFST_WAIT_STOP) == 1 << thid)
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
  val enqXRegsRespQ = IO(Flipped(xregsRespQ.io.enq.cloneType))
  xregsRespQ.io.enq <> enqXRegsRespQ
  xregsRespQ.io.deq.ready := RegNext(transBram2Cpu.rd.xreg.req.fire)

  dut.transBram2Cpu.rd.xreg.resp := xregsRespQ.io.deq.bits
  when(xregsRespQ.io.deq.fire) { 
    assert(trans2cpu.rfile_wr.en)
    assert(trans2cpu.rfile_wr.addr === RegNext(transBram2Cpu.rd.xreg.req.bits.regIdx))
    assert(trans2cpu.rfile_wr.tag === RegNext(transBram2Cpu.rd.thid))
    assert(trans2cpu.rfile_wr.data === xregsRespQ.io.deq.bits)
    assert(trans2cpu.stallPipeline)
    assert(trans2cpu.busyTrans2Cpu)
  }

  val pstateRespQ = Module(new Queue(transBram2Cpu.rd.pstate.resp.cloneType, 8, true, true))
  val enqPStateRespQ = IO(Flipped(pstateRespQ.io.enq.cloneType))
  pstateRespQ.io.enq <> enqPStateRespQ
  pstateRespQ.io.deq.ready := RegNext(transBram2Cpu.rd.pstate.req.fire)
  dut.transBram2Cpu.rd.pstate.resp := pstateRespQ.io.deq.bits
  when(pstateRespQ.io.deq.fire) {
    assert(trans2cpu.pstate.valid)
    assert(trans2cpu.pstate.bits.asUInt === pstateRespQ.io.deq.bits.asUInt)
  }
}