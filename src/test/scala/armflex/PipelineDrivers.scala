package armflex

import org.scalatest._
import chisel3._
import chisel3.util._

import chiseltest._
import chiseltest.internal._
import chiseltest.experimental.TestOptionBuilder._

import armflex.TPU2STATE._
import armflex.DriversExtra._

import armflex.util._
import armflex.util.SoftwareStructs._
import armflex.util.BRAMPortDriver.BRAMPortDriver
import chisel3.util.Queue
import firrtl.annotations.MemoryLoadFileType.Hex

object ArmflexDrivers {
  implicit class FullStateBundleDriver(self: FullStateBundle) {
    def poke(state: PState) = {
      for (reg <- 0 until 32) {
        self.rfile(reg).poke(state.xregs(reg).U)
      }
      self.regs.PC.poke(state.pc.U)
      self.regs.SP.poke(state.sp.U)
      self.regs.NZCV.poke(state.nzcv.U)
    }

    def compareAssert(target: FullStateBundle): Unit = {
      val success = WireInit(true.B)
      when(self.regs.PC =/= target.regs.PC) {
        printf(p"PC:${Hexadecimal(self.regs.PC)}=/=${Hexadecimal(target.regs.PC)}\n")
        success := false.B
      }
      when(self.regs.SP =/= target.regs.SP) {
        printf(p"SP${Hexadecimal(self.regs.SP)}=/=${Hexadecimal(target.regs.SP)}\n")
        success := false.B
      }
      when(self.regs.NZCV =/= target.regs.NZCV) {
        printf(p"NZCV${Hexadecimal(self.regs.NZCV)}=/=${Hexadecimal(target.regs.NZCV)}\n")
        success := false.B
      }
      for (reg <- 0 until 32) {
        val expect = self.rfile(reg)
        val actual = target.rfile(reg)
        when(expect =/= actual) {
          printf(p"${reg}:${Hexadecimal(expect)}=/=${Hexadecimal(actual)}\n")
          success := false.B
        }
      }
      assert(success)
    }
  }
  implicit class TransplantUnitHostIODriver(target: TransplantUnitHostIO) {
    def fire(tag: Int)(implicit clock: Clock) = timescope {
      target.fire.valid.poke(true.B)
      target.fire.tag.poke(tag.U)
      clock.step()
    }
  }

  implicit class PipelineHardDriver(pipeline: PipelineHardDriverModule) {
    implicit val clock = pipeline.clock

    def initIO: Unit = {
      pipeline.traceIn.setSourceClock(clock)
      pipeline.traceExpect.setSourceClock(clock)

      pipeline.traceIn.valid.poke(false.B)
      pipeline.traceExpect.valid.poke(false.B)

      pipeline.transplantIO.state.init
      pipeline.transplantIO.ctrl.fire.valid.poke(false.B)
    }

    def transplantAndStart(tag: Int, pstate: PState): Unit = {
      val statePort: BRAMPort = pipeline.transplantIO.state
      for (i <- ARCH_XREGS_OFFST until ARCH_XREGS_OFFST + 32) {
        statePort.wr(pstate.xregs(i), i)
      }
      statePort.wr(pstate.pc, ARCH_PC_OFFST)
      statePort.wr(pstate.sp, ARCH_SP_OFFST)
      statePort.wr(pstate.nzcv, ARCH_PSTATE_OFFST)
      pipeline.transplantIO.ctrl.fire(tag)
    }

    def getTransplantOut: PState = {
      val statePort: BRAMPort = pipeline.transplantIO.state
      val xregs = for (reg <- ARCH_XREGS_OFFST until ARCH_XREGS_OFFST + 32) yield {
        val regVal = statePort.rd(reg)
        regVal
      }
      val pc = statePort.rd(ARCH_PC_OFFST)
      val sp = statePort.rd(ARCH_SP_OFFST)
      val nzcv = statePort.rd(ARCH_PSTATE_OFFST)

      val pstate = new PState(xregs.toList: List[BigInt], pc: BigInt, sp: BigInt, nzcv.toInt: Int)
      pstate
    }

    private def getState(state: FullStateBundle): PState = {
      val pstate = state.regs
      val rfile = state.rfile

      val xregs = for (reg <- 0 until 32) yield rfile(reg).peek.litValue
      val pc = pstate.PC.peek.litValue
      val sp = pstate.SP.peek.litValue
      val nzcv = pstate.NZCV.peek.litValue.toInt
      new PState(xregs.toList: List[BigInt], pc: BigInt, sp: BigInt, nzcv: Int)
    }

    def traceIn(trace: CommitTrace): Unit =
      pipeline.traceIn.enqueue(trace)
    def traceExpect(trace: CommitTrace): Unit =
      pipeline.traceExpect.enqueue(trace)
  }
}

import chisel3.util.DecoupledIO
import chisel3.experimental.BundleLiterals._

object DriversExtra {
  implicit class CommitTraceBundleDriver[T <: CommitTraceBundle](target: CommitTraceBundle) {
    def peekTrace(): CommitTrace = {
      // TODO: check for init
      val xregs = (for (reg <- 0 until 32) yield target.state.rfile(reg).peek().litValue).toList
      val pc = target.state.regs.PC.peek().litValue
      val sp = target.state.regs.SP.peek().litValue
      val nzcv = target.state.regs.NZCV.peek().litValue

      val inst = target.inst.peek().litValue
      val mem_addr = List(target.memReq(0).addr.peek().litValue, target.memReq(1).addr.peek().litValue)
      val mem_data = List(target.memReq(0).data.peek().litValue, target.memReq(1).data.peek().litValue)
      val state = new PState(xregs, pc, sp, nzcv.toInt)
      new CommitTrace(state, inst, mem_addr, mem_data)
    }
  }
  implicit class CommitTraceDecoupledDriver[T <: CommitTraceBundle](target: DecoupledIO[T])
      extends DecoupledDriver[T](target) {
    def enqueue(trace: CommitTrace): Unit = timescope {
      val state = trace.state
      // TODO: check for init
      for (reg <- 0 until 32) {
        target.bits.state.rfile(reg).poke(state.xregs(reg).U)
      }
      target.bits.state.regs.poke(
        (new PStateRegs).Lit(
          _.PC -> state.pc.U,
          _.SP -> state.sp.U,
          _.NZCV -> state.nzcv.U
        )
      )
      target.bits.inst.poke(trace.inst.U)
      target.bits.memReq(0).addr.poke(trace.mem_addr(0).U)
      target.bits.memReq(1).addr.poke(trace.mem_addr(1).U)
      target.bits.memReq(0).data.poke(trace.mem_data(0).U)
      target.bits.memReq(1).data.poke(trace.mem_data(1).U)

      target.valid.poke(true.B)
      fork
        .withRegion(Monitor) {
          while (target.ready.peek().litToBoolean == false) {
            getSourceClock.step(1)
          }
        }
        .joinAndStep(getSourceClock)
    }
  }
}

import chisel3.util.{Decoupled, Queue, ShiftRegister}
import armflex.util.DecoupledTools._
import arm.PROCESSOR_TYPES._

class CommitTraceBundle extends Bundle {
  val state = new FullStateBundle
  val inst = INST_T
  val memReq = Vec(
    2,
    new Bundle {
      val addr = DATA_T
      val data = DATA_T
    }
  )
}

class PipelineHardDriverModule(implicit val cfg: ProcConfig) extends MultiIOModule {
  import armflex.ArmflexDrivers._
  val pipeline = Module(new PipelineWithTransplant)
  val transplantIO = IO(pipeline.transplantIO.cloneType)
  transplantIO <> pipeline.transplantIO

  val traceIn = IO(Flipped(Decoupled(new CommitTraceBundle)))
  val traceExpect = IO(Flipped(Decoupled(new CommitTraceBundle)))
  val done = IO(Output(Bool()))

  val fetch = Module(new Queue(new CommitTraceBundle, 128, true, true))
  val issue = Module(new Queue(new CommitTraceBundle, 128, true, true))
  val memResp = Module(new Queue(new CommitTraceBundle, 128, true, true))

  fetch.io.enq <> traceIn

  // Simulate Fetch
  pipeline.mem.inst.req.ready := true.B
  fetch.io.deq.ready := pipeline.mem.inst.req.fire
  // Shift by latency of memory, here 1
  val fetchLatency = 1
  pipeline.mem.inst.resp.valid := ShiftRegister(fetch.io.deq.fire, fetchLatency)
  pipeline.mem.inst.resp.bits := DontCare
  pipeline.mem.inst.resp.bits.data := ShiftRegister(fetch.io.deq.bits.inst, fetchLatency)

  issue.io.enq.valid := fetch.io.deq.fire
  issue.io.enq.bits := fetch.io.deq.bits

  issue.io.deq.ready := pipeline.dbg.issue.valid
  memResp.io.enq.bits := issue.io.deq.bits
  memResp.io.enq.valid := issue.io.deq.fire && pipeline.dbg.issuingMem

  pipeline.mem.data.req.ready := true.B
  memResp.io.deq.ready := pipeline.mem.data.req.fire

  pipeline.mem.data.resp.valid := ShiftRegister(memResp.io.deq.fire, pipeline.mem.data.latency)
  pipeline.mem.data.resp.bits(0).valid := ShiftRegister(pipeline.mem.data.req.bits.isLoad, pipeline.mem.data.latency)
  pipeline.mem.data.resp.bits(1).valid := ShiftRegister(pipeline.mem.data.req.bits.isPair && pipeline.mem.data.req.bits.isLoad, pipeline.mem.data.latency)
  pipeline.mem.data.resp.bits(0).bits := ShiftRegister(memResp.io.deq.bits.memReq(0).data, pipeline.mem.data.latency)
  pipeline.mem.data.resp.bits(1).bits := ShiftRegister(memResp.io.deq.bits.memReq(1).data, pipeline.mem.data.latency)

  // TODO Simulate memory misses?
  pipeline.mem.wake.tag := DontCare
  pipeline.mem.wake.valid := false.B

  val commit = Module(new Queue(new CommitTraceBundle, 128, true, true))
  commit.io.enq <> traceExpect
  // 1 Cycle after commit singnal is rised, state is fully updated
  commit.io.deq.ready := ShiftRegister(pipeline.dbg.commit.valid, 1)
  val commitTransplant = ShiftRegister(pipeline.dbg.commitTransplant, 1)

  when(issue.io.deq.fire) {
    printf(p"Issuing:0x${Hexadecimal(pipeline.dbg.issue.bits.get.regs.PC)}\n")
    issue.io.deq.bits.state.compareAssert(pipeline.dbg.issue.bits.get)
    printf("  Success Issue\n")
  }
  when(memResp.io.deq.fire) {
    assert(memResp.io.deq.bits.memReq(0).addr === pipeline.mem.data.req.bits.memReq(0).addr)
    when(pipeline.mem.data.req.bits.isPair) {
      assert(memResp.io.deq.bits.memReq(1).addr === pipeline.mem.data.req.bits.memReq(1).addr)
    }
  }
  when(commit.io.deq.fire) {
    printf(
      p"Commit :0x${Hexadecimal(pipeline.dbg.commit.bits.get.regs.PC)}:${Hexadecimal(pipeline.dbg.commit.bits.get.regs.PC)}\n"
    )
    when(!commitTransplant.valid) {
      commit.io.deq.bits.state.compareAssert(pipeline.dbg.commit.bits.get)
      printf("  Success Commit\n")
    }.otherwise {
      printf(" Detected Transplant!\n")
    }
  }

  val transplantOut = IO(new Bundle {
    val state = Valid(new CommitTraceBundle)
    val inst = Output(INST_T)
  })
  transplantOut.state.bits := commit.io.deq.bits
  transplantOut.state.valid := commit.io.deq.fire && commitTransplant.valid
  transplantOut.inst := commitTransplant.bits

  done := commit.io.count === 0.U
}
