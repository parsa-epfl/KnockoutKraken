package armflex

import org.scalatest._
import chisel3._
import chisel3.util._

import chiseltest._
import chiseltest.internal._
import chiseltest.experimental.TestOptionBuilder._
import chisel3.util.DecoupledIO
import chisel3.experimental.BundleLiterals._

import armflex.Trans2State._
import armflex.TestDriversExtra._

import armflex.util._
import armflex.util.SoftwareStructs._
import armflex.util.BRAMPortDriver.BRAMPortDriver
import armflex.util.ExtraUtils._
import armflex.TestDriversExtra._
import chisel3.util.Queue
import firrtl.annotations.MemoryLoadFileType.Hex

object PipelineDrivers {
  implicit class PipelineHardDriver(pipeline: PipelineHardDriverModule) {
    implicit val clock = pipeline.clock

    def initIO: Unit = {
      pipeline.traceIn.setSourceClock(clock)
      pipeline.traceExpect.setSourceClock(clock)

      pipeline.traceIn.valid.poke(false.B)
      pipeline.traceExpect.valid.poke(false.B)

      pipeline.transplantIO.port.init
      pipeline.transplantIO.done.valid.poke(false.B)
    }

    def transplantAndStart(tag: Int, pstate: PState): Unit = {
      val statePort: BRAMPort = pipeline.transplantIO.state
      for (i <- ARCH_XREGS_OFFST until ARCH_XREGS_OFFST + 32) {
        statePort.wr(pstate.xregs(i), i)
      }
      statePort.wr(pstate.nzcv, ARCH_PSTATE_OFFST)
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

    def traceIn(trace: CommitTrace): Unit =
      pipeline.traceIn.enqueue(trace)
    def traceExpect(trace: CommitTrace): Unit =
      pipeline.traceExpect.enqueue(trace)
  }
}

object TestDriversExtra {
  implicit class FullStateBundleDriver(self: FullStateBundle) {
    def poke(state: PState) = {
      for (reg <- 0 until 32) {
        self.rfile(reg).poke(state.xregs(reg).U)
      }
      self.regs.PC.poke(state.pc.U)
      self.regs.SP.poke(state.sp.U)
      self.regs.NZCV.poke(state.nzcv.U)
    }
    def peek(): PState = {
      val pstate = self.regs
      val rfile = self.rfile

      val xregs = for (reg <- 0 until 32) yield rfile(reg).peek.litValue
      val pc = pstate.PC.peek.litValue
      val sp = pstate.SP.peek.litValue
      val nzcv = pstate.NZCV.peek.litValue.toInt
      new PState(xregs.toList: List[BigInt], pc: BigInt, sp: BigInt, nzcv: Int)
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

  implicit class CommitTraceBundleDriver(self: CommitTraceBundle) {
    def poke(trace: CommitTrace) = {
      self.state.poke(trace.state)
      self.inst.poke(trace.inst.U)
      self.memReq(0).addr.poke(trace.mem_addr(0).U)
      self.memReq(1).addr.poke(trace.mem_addr(1).U)
      self.memReq(0).data.poke(trace.mem_data(0).U)
      self.memReq(1).data.poke(trace.mem_data(1).U)
    }
    def peek(): CommitTrace = {
      val state = self.state.peek()
      val inst = self.inst.peek.litValue
      val mem_addr = List(self.memReq(0).addr.peek.litValue, self.memReq(1).addr.peek.litValue)
      val mem_data = List(self.memReq(0).data.peek.litValue, self.memReq(1).data.peek.litValue)
      new CommitTrace(state, inst, mem_addr, mem_data)
    }
  }

  implicit class CommitTraceDecoupledDriver[T <: CommitTraceBundle](target: DecoupledIO[T])
      extends DecoupledDriver[T](target) {
    def enqueue(trace: CommitTrace): Unit = timescope {
      target.bits.poke(trace)
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

  val pairJustFired = pipeline.mem.data.req.fire && memResp.io.deq.bits.memReq(1).addr =/= 0.U
  val pairReq = RegNext(pairJustFired)
  val singleResponse = ShiftRegister(memResp.io.deq.fire, cfg.cacheLatency)
  val pairResponse = ShiftRegister(pairReq, cfg.cacheLatency)
  val memRespBits = ShiftRegister(memResp.io.deq.bits, cfg.cacheLatency)
  memResp.io.deq.ready := pipeline.mem.data.req.valid && !pairReq
  pipeline.mem.data.req.ready := !pairReq
  pipeline.mem.data.resp.valid := singleResponse || pairResponse
  pipeline.mem.data.resp.bits.dirty := false.B
  pipeline.mem.data.resp.bits.hit := false.B
  pipeline.mem.data.resp.bits.thread_id := 0.U
  pipeline.mem.data.resp.bits.data := memRespBits.memReq(0).data
  when(pairResponse) {
    pipeline.mem.data.resp.bits.data := RegNext(memRespBits.memReq(1).data)
  }

  // TODO Simulate memory misses?
  pipeline.mem.wake := DontCare
  pipeline.mem.wake foreach (_.valid := false.B)

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
    assert(memResp.io.deq.bits.memReq(0).addr === pipeline.mem.data.req.bits.addr)
  }
  when(pairReq) {
    assert(RegNext(memResp.io.deq.bits.memReq(1).addr) === pipeline.mem.data.req.bits.addr)
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
