package armflex

import org.scalatest._
import chisel3._
import chisel3.util._

import chiseltest._
import chiseltest.internal._
import chisel3.util.DecoupledIO
import chisel3.experimental.BundleLiterals._

import armflex.Trans2State._
import armflex.TestDriversExtra._

import armflex.util._
import armflex.util.SoftwareStructs._
import armflex.util.BRAMPortDriver.BRAMPortDriver
import armflex.util.ExtraUtils._
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

      pipeline.hostIO.port.init
      pipeline.hostIO.done.valid.poke(false.B)
    }

    def transplantAndStart(tag: Int, pstate: PState): Unit = {
      pipeline.hostIO.port.wr(tag, pstate)
      timescope {
        pipeline.hostIO.done.valid.poke(true.B)
        pipeline.hostIO.done.tag.poke(tag.U)
        clock.step()
      }
    }

    def getTransplantOut(tag: Int): PState = {
      val state = pipeline.hostIO.port.rdState(tag)
      state
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
      //self.regs.SP.poke(state.sp.U)
      self.regs.NZCV.poke(state.nzcv.U)
    }
    def peek(): PState = {
      val pstate = self.regs
      val rfile = self.rfile

      val xregs = for (reg <- 0 until 32) yield rfile(reg).peek().litValue
      val pc = pstate.PC.peek().litValue
      val sp = 0 //pstate.SP.peek.litValue
      val nzcv = pstate.NZCV.peek().litValue.toInt
      new PState(xregs.toList: List[BigInt], pc: BigInt, sp: BigInt, nzcv: Int)
    }
    def compareAssert(target: FullStateBundle): Unit = {
      val success = WireInit(true.B)
      when(self.regs.PC =/= target.regs.PC) {
        printf(p"PC:${Hexadecimal(self.regs.PC)}=/=${Hexadecimal(target.regs.PC)}\n")
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
      assert(success, "State didn't match with QEMU execution")
    }
  }

  implicit class CommitTraceBundleDriver(self: CommitTraceBundle) {
    def poke(trace: CommitTrace) = {
      self.state.poke(trace.state)
      self.inst.poke(trace.inst.U)
      self.inst_block.poke(trace.inst_block.U)
      self.memReq(0).addr.poke(trace.mem_addr(0).U)
      self.memReq(1).addr.poke(trace.mem_addr(1).U)
      self.memReq(0).data.poke(trace.mem_data(0).U)
      self.memReq(1).data.poke(trace.mem_data(1).U)
      self.memReq(0).block.poke(trace.mem_block(0).U)
      self.memReq(1).block.poke(trace.mem_block(1).U)
    }
    def peek(): CommitTrace = {
      val state = self.state.peek()
      val inst = self.inst.peek().litValue
      val inst_block = self.inst_block.peek().litValue
      val mem_addr = List(self.memReq(0).addr.peek().litValue, self.memReq(1).addr.peek().litValue)
      val mem_data = List(self.memReq(0).data.peek().litValue, self.memReq(1).data.peek().litValue)
      val mem_block = List(self.memReq(0).block.peek().litValue, self.memReq(0).block.peek().litValue) 
      new CommitTrace(state, inst, inst_block, mem_addr, mem_data, mem_block)
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

class CommitTraceBundle(val blockSize: Int) extends Bundle {
  val state = new FullStateBundle
  val inst = INST_T
  val inst_block = UInt(blockSize.W)
  val memReq = Vec(
    2,
    new Bundle {
      val addr = DATA_T
      val data = DATA_T
      val block = UInt(blockSize.W)
    }
  )
}

import antmicro.CSR.ClearReg
class PipelineHardDriverModule(params: PipelineParams) extends Module {
  val pipeline = Module(new PipelineWithTransplant(params))
  val hostIO = IO(new Bundle {
    val port = pipeline.hostIO.port.cloneType
    val done = Input(ValidTag(params.thidN))
    val transOut = Output(ValidTag(params.thidN))
  })
  val traceIn = IO(Flipped(Decoupled(new CommitTraceBundle(params.blockSize))))
  val traceExpect = IO(Flipped(Decoupled(new CommitTraceBundle(params.blockSize))))
  val done = IO(Output(Bool()))
  val transplantOut = IO(new Bundle {
    val expected = Valid(new CommitTraceBundle(params.blockSize))
    val actual = Output(new FullStateBundle)
    val inst = Output(INST_T)
  })
 

  /* TODO: Redo Pipeline Test with new MemoryUnit
  pipeline.hostIO.port <> hostIO.port
  val set = Wire(Valid(params.thidT))
  set.bits := hostIO.done.tag
  set.valid := hostIO.done.valid
  val clear = Wire(Valid(params.thidT))
  clear.bits := pipeline.hostIO.trans2host.clear.tag
  clear.valid := pipeline.hostIO.trans2host.clear.valid
  val pendingHost = ClearReg(set, clear, params.thidN)
  pipeline.hostIO.host2trans.pending := pendingHost

  hostIO.transOut := pipeline.hostIO.trans2host.done

  val fetch = Module(new Queue(new CommitTraceBundle(params.blockSize), 128, true, true))
  val issue = Module(new Queue(new CommitTraceBundle(params.blockSize), 128, true, true))
  val memResp = Module(new Queue(new CommitTraceBundle(params.blockSize), 128, true, true))

  fetch.io.enq <> traceIn

  // Simulate Fetch
  pipeline.mem.inst.req.ready := true.B
  fetch.io.deq.ready := pipeline.mem.inst.req.fire
  // Shift by latency of memory, here 1
  val fetchLatency = 1
  pipeline.mem.inst.resp.valid := ShiftRegister(fetch.io.deq.fire, fetchLatency)
  pipeline.mem.inst.resp.bits := DontCare // TODO
  pipeline.mem.inst.resp.bits.data := ShiftRegister(fetch.io.deq.bits.inst_block, fetchLatency)
  pipeline.mem.inst.resp.bits.hit := true.B

  issue.io.enq.valid := fetch.io.deq.fire
  issue.io.enq.bits := fetch.io.deq.bits

  issue.io.deq.ready := pipeline.dbg.bits.get.issue.valid
  memResp.io.enq.bits := issue.io.deq.bits
  memResp.io.enq.valid := issue.io.deq.fire && pipeline.dbg.bits.get.issuingMem && !pipeline.dbg.bits.get.issuingTransplant

  val pairJustFired = memResp.io.deq.fire && memResp.io.deq.bits.memReq(1).addr =/= 0.U
  val pairReqIn = RegNext(pairJustFired)
  pipeline.mem.data.req.ready := true.B
  memResp.io.deq.ready := !pairReqIn && pipeline.mem.data.req.fire
  val singleResponse = ShiftRegister(pipeline.mem.data.req.fire, params.cacheLatency)
  val pairResponse = ShiftRegister(pairReqIn, params.cacheLatency)
  val memRespBits = ShiftRegister(memResp.io.deq.bits, params.cacheLatency)

  pipeline.mem.data.resp.valid := singleResponse || pairResponse
  // pipeline.mem.data.resp.bits.dirty := false.B
  pipeline.mem.data.resp.bits.hit := true.B
  pipeline.mem.data.resp.bits.thid := 0.U
  pipeline.mem.data.resp.bits.data := memRespBits.memReq(0).block
  when(pairResponse) {
    printf("PairLoadST\n")
    pipeline.mem.data.resp.bits.data := RegNext(memRespBits.memReq(1).block)
  }

  // TODO Simulate memory misses?
  pipeline.mem.wake := DontCare
  pipeline.mem.wake foreach (_.valid := false.B)
  pipeline.mem.dataFault.valid := false.B
  pipeline.mem.instFault.valid := false.B
  pipeline.mem.dataFault.tag := 0.U
  pipeline.mem.instFault.tag := 0.U
  val commit = Module(new Queue(new CommitTraceBundle(params.blockSize), 128, true, true))
  commit.io.enq <> traceExpect
  // 1 Cycle after commit singnal is rised, state is fully updated
  commit.io.deq.ready := ShiftRegister(pipeline.dbg.bits.get.commit.valid, 1)
  val commitTransplant = ShiftRegister(pipeline.dbg.bits.get.commitTransplant, 1)

  when(issue.io.deq.fire) {
    params.simLog(p"Issuing:0x${Hexadecimal(pipeline.dbg.bits.get.issue.bits.get.regs.PC)}\n")
    issue.io.deq.bits.state.compareAssert(pipeline.dbg.bits.get.issue.bits.get)
    params.simLog("  Success Issue\n")
  }
  val commitCnt = RegInit(0.U(32.W))
  when(commit.io.deq.fire) {
    params.simLog(
      p"Commit :0x${Hexadecimal(pipeline.dbg.bits.get.commit.bits.get.regs.PC)}:${Hexadecimal(pipeline.dbg.bits.get.commit.bits.get.regs.PC)}\n"
    )
    when(!commitTransplant.valid) {
      commit.io.deq.bits.state.compareAssert(pipeline.dbg.bits.get.commit.bits.get)
      params.simLog("  Success Commit\n")
      if(!params.simVerbose) {
        commitCnt := commitCnt + 1.U
        printf(p"Commit Success:${Hexadecimal(pipeline.dbg.bits.get.commit.bits.get.regs.PC)}${commitCnt}\n")
      }
    }.otherwise {
      printf(" Detected Transplant!\n")
    }
  }

  transplantOut.expected.bits := commit.io.deq.bits
  transplantOut.expected.valid := commit.io.deq.fire && commitTransplant.valid
  transplantOut.actual := pipeline.dbg.bits.get.stateVec(0)
  transplantOut.inst := commitTransplant.bits

  done := commit.io.count === 0.U
  */
}
