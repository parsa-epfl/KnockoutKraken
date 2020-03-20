package armflex

import chisel3._
import chisel3.util.{Decoupled, PriorityEncoder, PriorityMux, Queue, Reverse, Valid, log2Ceil}

import arm.PROCESSOR_TYPES._

import util.FlushQueue

class RRArbiter(arbN: Int)
    extends Module {

  val io = IO(new Bundle {
           // Ready threads
           val ready = Input(UInt(arbN.W))
           // Next thread to issue
           val next  = Decoupled(UInt(log2Ceil(arbN).W))
         })

  def rotateRight[T <: Data](norm: UInt, rot : UInt) : UInt = {
    val right = norm >> rot
    val left  = norm << ~rot + 1.U;
    val res = left | right
    res(arbN - 1, 0)
  }

  /*
   * valid      At least one thread is ready to be issued
   * curr       Thread with highest priority during round
   * ready_ofst Ready vector shifted such as lsb is the
   *            offset from curr thread to the thread with
   *            highest priority
   * ofst       Offset from curr thread to thread to be issued
   * next       Thread being issued on this cycle if next stage ready
   */
  val valid = io.ready.orR
  val curr = RegInit(0.U(log2Ceil(arbN).W))  // Last thread issued
  val ready_ofst = rotateRight(io.ready, curr)
  val ofst = PriorityEncoder(ready_ofst)
  val next = curr + ofst

  io.next.bits := next
  io.next.valid := valid
  when(io.next.ready && valid) { curr := next + 1.U}
}

class IssueUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new Bundle {
    // Decode - Issue
    val enq = Flipped(Decoupled(new DInst))
    // Issue - Exec
    val deq = Decoupled(new DInst)

    // Back Pressure
    val commitReg = Flipped(Valid(new CommitInst))
    val flush = Input(ValidTag(cfg.TAG_T))
  })

  val fifos = VecInit(Seq.fill(cfg.NB_THREADS)(Module(FlushQueue(io.enq.bits.asUInt.cloneType, 4)).io))
  // Defaults
  for (cpu <- 0 until cfg.NB_THREADS) {
    fifos(cpu).enq.valid := false.B
    fifos(cpu).enq.bits  := io.enq.bits.asUInt
    fifos(cpu).deq.ready := false.B
    fifos(cpu).flush := false.B
  }

  io.enq.ready := fifos(io.enq.bits.tag).enq.ready
  fifos(io.enq.bits.tag).enq.valid := io.enq.valid

  /** Issue stage arbiter
    * arbiter        Round Robin Arbiter
    * valid_threads  This signal indicates which threads are ready to be issued
    * issue_thread   This signal indicates the next thread to issue
    */
  val arbiter = Module(new RRArbiter(cfg.NB_THREADS))
  val ready_threads = VecInit(fifos map (_.deq.valid))
  ready_threads(io.commitReg.bits.tag) := !io.commitReg.valid
  arbiter.io.ready := ready_threads.asUInt

  arbiter.io.next.ready := io.deq.ready
  val issue_thread = WireInit(arbiter.io.next.bits)

  io.deq.bits  := fifos(issue_thread).deq.bits.asTypeOf(new DInst)
  io.deq.valid := fifos(issue_thread).deq.valid && arbiter.io.next.valid
  fifos(issue_thread).deq.ready := io.deq.ready && arbiter.io.next.valid

  when(io.flush.valid) {
    fifos(io.flush.tag).flush := true.B
    ready_threads(io.flush.tag) := false.B
  }
}
