// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.{Decoupled, PriorityEncoder, PriorityMux, Queue, Reverse, Valid, log2Ceil}
import common.PROCESSOR_TYPES._
import common._

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

/** IssueUnit
  *
  * Recieves the decoded instruction.
  * Fine-grained selects the next thread to issue, and issues it.
  * The arbiter chooses in a Round Robin way, takes into account
  * hazard such as the non commited instructions of the Mem and Exec stages.
  * Stalls if none of the threads is ready.
  *
  * It's capable of buffering 3 instructions per thread.
  */
class IssueUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new Bundle {
    // Decode - Issue
    val enq = Flipped(Decoupled(new DInst))
    // Issue - Exec
    val deq = Decoupled(new DInst)

    // Back Pressure
    val commitReg = Flipped(Valid(new CommitInst))
    val flush = Input(ValidTagged(cfg.TAG_T))
  })

  /** Issue stage pipeline register
    * reg_pipe   Contains the next decoded instruction to issue per thread.
    *            The Round Robine Arbiter selects which threassangead issues during the cycle.
    * reg_pipe_v The valid bit indicates whenever the register contains a valid decoded instruction.
    * sig_pipe_r This signal indicates whenever the register is ready to recieve the next instruction.
    */
  val reg_pipe   = RegInit(VecInit(Seq.fill(cfg.NB_THREADS)(DInst())))
  val reg_pipe_v = RegInit(VecInit(cfg.TAG_VEC_X.asBools))
  val sig_pipe_r = WireInit(VecInit(cfg.TAG_VEC_X.asBools))

  /** Issue stage buffer
    * FIFO_i To buffer instructions we have a FIFO queue with flow and pipe modes enabled per thread.
    *        Flow, this implies that if the register is ready to recieve a decoded instruction, it will
    *            bypass the FIFO connecting directly into the register, thus saving a cycle.
    *        Pipe, this implies that if the thread is issuing this stage, a new instruction is coming,
    *            and the queue is full, the system will still enq, deq and register the incoming instruction.
    * fifo_vec N-Vector of fifos, one per thread.
    */
  def FIFO_i   = new Queue(new DInst, 2, pipe = true, flow = true)
  // Instanciates queue modules, expresses their IO through the vector
  val fifo_vec = VecInit(Seq.tabulate(cfg.NB_THREADS)(
                           cpu => withReset(reset.asBool || (cpu.U === io.flush.tag && io.flush.valid)) { Module(FIFO_i).io }))

  /** Issue stage arbiter
    * arbiter        Round Robin Arbiter
    * sig_pipe_i     This signal indicates which threads are ready to be issued
    * sig_next_idx   This signal indicates the next instruction to issue
    */
  val arbiter       = Module(new RRArbiter(cfg.NB_THREADS))
  val sig_pipe_i    = VecInit(cfg.TAG_VEC_X.asBools)
  val sig_next_idx  = WireInit(cfg.TAG_X)

  /** Managing backpressure
    */
  val exe_stall = WireInit(false.B)
  val mem_stall = WireInit(false.B)

  // Enqueuing interface Decode -> FIFO
  // Enqueues the Decoded instrcution with matching tag
  fifo_vec map { f =>
    f.enq.valid := false.B
    f.enq.bits := io.enq.bits
  }
  io.enq.ready := fifo_vec(io.enq.bits.tag).enq.ready
  fifo_vec(io.enq.bits.tag).enq.valid := io.enq.valid

  for (cpu <- 0 until cfg.NB_THREADS) {
    // Dequeing buffers interface : FIFO -> Reg
    // Registers who are invalid are ready to recieve new value
    sig_pipe_r(cpu) := ~reg_pipe_v(cpu)

    fifo_vec(cpu).deq.ready := sig_pipe_r(cpu)
    reg_pipe(cpu)   := Mux(sig_pipe_r(cpu), fifo_vec(cpu).deq.bits, reg_pipe(cpu))
    reg_pipe_v(cpu) := Mux(sig_pipe_r(cpu), fifo_vec(cpu).deq.valid, reg_pipe_v(cpu))

    // Choose next thread to Issue (Reg status + hazards -> idx)
    // Thread is ready to be issued if it has a valid decoded instruction in register and has no hazard detected.
    sig_pipe_i(cpu) := reg_pipe_v(cpu)
  }

  // Backpressure: thread committing doesn't issue
  sig_pipe_i(io.commitReg.bits.tag) := reg_pipe_v(io.commitReg.bits.tag) && !io.commitReg.valid

  // Issue -> Exec
  // Get idx to issue
  arbiter.io.ready := sig_pipe_i.asUInt
  sig_next_idx     := arbiter.io.next.bits

  io.deq.bits  := reg_pipe(sig_next_idx)
  io.deq.valid := arbiter.io.next.valid
  arbiter.io.next.ready := io.deq.ready

  // If next stage ready, and one of the threads is ready to be issued
  // Set issued thread register ready to recieve next instruction
  when(arbiter.io.next.valid && io.deq.ready) {
    fifo_vec(sig_next_idx).deq.ready := true.B
    reg_pipe_v(sig_next_idx) := fifo_vec(sig_next_idx).deq.valid
    reg_pipe(sig_next_idx) := fifo_vec(sig_next_idx).deq.bits
  }

  // Flushing ----------------------------------------------------------------
  when(io.flush.valid) {
    reg_pipe(io.flush.tag) := DInst()
    reg_pipe_v(io.flush.tag) := false.B
    sig_pipe_i(io.flush.tag) := false.B
  }
}

class IssueUnitRevamp(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new Bundle {
    // Decode - Issue
    val enq = Flipped(Decoupled(new DInst))
    // Issue - Exec
    val deq = Decoupled(new DInst)

    // Back Pressure
    val commitReg = Flipped(Valid(new CommitInst))
    val flush = Input(ValidTagged(cfg.TAG_T))
  })

  val fifos = VecInit(Seq.fill(cfg.NB_THREADS)(Module(FlushQueue(new DInst, 4)).io))
  // Defaults
  for (cpu <- 0 until cfg.NB_THREADS) {
    fifos(cpu).enq.valid := false.B
    fifos(cpu).enq.bits  := io.enq.bits
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

  io.deq <> fifos(issue_thread).deq

  when(io.flush.valid) {
    fifos(io.flush.tag).flush := true.B
    ready_threads(io.flush.tag) := false.B
  }
}
