package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._
import armflex.util._
import armflex.util.ExtraUtils._

class CommitInst(thidN: Int) extends Bundle {
  // Update State
  val tag = Output(UInt(log2Ceil(thidN).W))
  val inst = Output(INST_T)
  val rd = Output(Vec(3, Valid(REG_T)))
  val res = Output(Vec(3, DATA_T))
  val nzcv = Output(Valid(NZCV_T))
  val br_taken = Output(Valid(DATA_T))
  val exceptions = Valid(UInt(1.W))
  val undef = Output(Bool())
  val is32bit = Output(Bool())
}

object CommitInst {
  def apply(thidN: Int): CommitInst = {
    val wire = Wire(new CommitInst(thidN))
    // DontCare data
    wire := DontCare
    // Invalidate signals
    wire.rd(0).valid := false.B
    wire.rd(1).valid := false.B
    wire.rd(2).valid := false.B
    wire.nzcv.valid := false.B
    wire.br_taken.valid := false.B
    wire.exceptions.valid := false.B
    wire.undef := false.B
    wire
  }
}

class CommitArchStateIO(thidN: Int) extends Bundle {
  val tag = Output(UInt(log2Ceil(thidN).W))
  val fire = Output(Bool())
  val pstate = new Bundle {
    val curr = Input(new PStateRegs)
    val next = Output(new PStateRegs)
  }
  val wr = Flipped(new RFileIO.WRPort(thidN))
  val ready = Input(Bool())
  val icountLastInst = Input(Bool())
  val isTransplantUnit = Output(Bool())
  val isCommitUnit = Output(Bool())
}

class CommitInstrument(thidN: Int) extends Bundle {
  val pc = DATA_T
}

class CommitUnit(val thidN: Int) extends Module {
  val enq = IO(Flipped(Decoupled(new CommitInst(thidN))))
  val commit = IO(new Bundle {
    val archstate = new CommitArchStateIO(thidN)
    val transplant = Output(ValidTag(thidN, INST_T))
    val commited = Output(ValidTag(thidN))
    val count = Output(UInt(log2Ceil(thidN).W))
  })

  val commitQueue = Module(new Queue(new CommitInst(thidN), thidN, true, false))
  commitQueue.io.enq <> enq
  commit.count := commitQueue.io.count

  val tag = WireInit(commitQueue.io.deq.bits.tag)
  val rd = WireInit(commitQueue.io.deq.bits.rd)
  val res = WireInit(commitQueue.io.deq.bits.res)


  // -- Transplant cases --
  val undef = WireInit(commitQueue.io.deq.bits.undef)
  val exception = WireInit(commitQueue.io.deq.bits.exceptions.valid)
  val transplant = WireInit(undef || exception)

  // -- Flush pipeline --
  val branch_taken = WireInit(commitQueue.io.deq.bits.br_taken.valid)

  // Transplant
  commit.transplant.tag := tag
  commit.transplant.valid := false.B
  commit.transplant.bits.get := commitQueue.io.deq.bits.inst

  // connect RFile's write interface
  commit.archstate.tag := tag
  commit.archstate.fire := commitQueue.io.deq.fire

  // Default don't modify State
  commit.archstate.pstate.next := commit.archstate.pstate.curr
  commit.archstate.wr.en := false.B
  commit.archstate.wr.addr := commitQueue.io.deq.bits.rd(0).bits
  commit.archstate.wr.data := commitQueue.io.deq.bits.res(0)
  commit.archstate.wr.tag := tag

  commit.archstate.pstate.next.flags.isException := exception
  commit.archstate.pstate.next.flags.isUndef := undef
  commit.archstate.isTransplantUnit := false.B
  commit.archstate.isCommitUnit := true.B

  // WriteBack
  val instrumentReady = Wire(Bool())
  val s_WB :: s_WB1 :: s_WB2 :: Nil = Enum(3)
  val canWB = WireInit(commit.archstate.ready && commitQueue.io.deq.valid && instrumentReady)
  val wbState = RegInit(s_WB)
  val wbState_next = WireInit(wbState)
  wbState := wbState_next

  assert(s_WB === 0.U && s_WB1 === 1.U && s_WB2 === 2.U)
  commit.archstate.wr.addr := rd(wbState).bits
  commit.archstate.wr.data := res(wbState)

  switch(wbState) {
    is(s_WB) {
      when(transplant) {
         wbState_next := s_WB
      }.elsewhen(canWB) {
        commit.archstate.wr.en := rd(0).valid
        wbState_next := Mux(rd(1).valid, s_WB1, Mux(rd(2).valid, s_WB2, s_WB))
      }
    }
    is(s_WB1) {
      when(canWB) {
        commit.archstate.wr.en := rd(1).valid
        wbState_next := Mux(rd(2).valid, s_WB2, s_WB)
        assert(rd(1).valid)
      }
    }
    is(s_WB2) {
      when(canWB) {
        commit.archstate.wr.en := rd(2).valid
        wbState_next := s_WB
        assert(rd(2).valid)
      }
    }
  }

  val lastCommitCycle = WireInit(canWB && wbState_next === s_WB)
  commitQueue.io.deq.ready := lastCommitCycle
  val finalCommit = WireInit(commitQueue.io.deq.fire)
  commit.commited.tag := tag
  commit.commited.valid := finalCommit
  when(finalCommit) {
    when(transplant) {
      commit.transplant.valid := true.B
    }.otherwise {
      when(branch_taken) {
        commit.archstate.pstate.next.PC := commitQueue.io.deq.bits.br_taken.bits
      }.otherwise {
        commit.archstate.pstate.next.PC := commit.archstate.pstate.curr.PC + 4.U
      }
      when(commitQueue.io.deq.bits.nzcv.valid) {
        commit.archstate.pstate.next.flags.NZCV := commitQueue.io.deq.bits.nzcv.bits
      }
    }
  }

  // Instrumentation interface
  val deq = IO(Decoupled(new CommitInstrument(thidN)))
  instrumentReady := deq.ready
  deq.valid := lastCommitCycle
  deq.bits.pc := commit.archstate.pstate.curr.PC

  assert((commitQueue.io.deq.fire && deq.fire) || (!commitQueue.io.deq.fire && !deq.fire), "Both deq and commitQueue must dequeue at the same time")
}
