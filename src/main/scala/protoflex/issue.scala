// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.Queue
import chisel3.util.log2Ceil
import chisel3.util.EnqIO
import chisel3.util.DeqIO
import chisel3.util.PriorityEncoder

import common.PROCESSOR_TYPES._

class IssueUnitIO extends Bundle {
  // Decode - Issue
  val enq = EnqIO(new DInst)
  val tag = Input(TAG_T)

  // Issue - Exec
  val deq = DeqIO(new DInst)


  //`Exec - Issue
  val exe_rd = Input(REG_T)
  val exe_tag = Input(TAG_T)
  val exe_rd_v = Input(Bool())
  // Mem - Issue
  val mem_rd = Input(REG_T)
  val mem_tag = Input(TAG_T)
  val mem_rd_v =Input(Bool())
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
class IssueUnit extends Module
{
  val io = IO(new IssueUnitIO)

  /** Issue stage pipeline register
    * reg_pipe   Contains the next decoded instruction to issue per thread.
    *            The Round Robine Arbiter selects which thread issues during the cycle.
    * reg_pipe_v The valid bit indicates whenever the register contains a valid decoded instruction.
    * sig_pipe_r This signal indicates whenever the register is ready to recieve the next instruction.
    */
  val reg_pipe   = RegInit(VecInit(Seq.fill(NUM_THREADS)(new DInst)))
  val reg_pipe_v = RegInit(VecInit(0.U(NUM_THREADS.W).toBools))
  val sig_pipe_r = WireInit(VecInit(UInt(NUM_THREADS.W).toBools))

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
  val fifo_vec = VecInit(Seq.fill(NUM_THREADS)(Module(FIFO_i).io))

  /** Issue stage arbiter
    * arbiter        Round Robin Arbiter
    * reg_issue_idx  Contains the index of the last issued thread
    * sig_pipe_i     This signal indicates which threads are ready to be issued
    * sig_next_idx   This signal indicates the next instruction to issue
    */
  val arbiter       = new RRArbiter(NUM_THREADS)
  val reg_issue_idx = Reg(0.U(TAG_T))
  val sig_pipe_i    = Wire(UInt(NUM_THREADS.W))
  val sig_next_idx  = Wire(TAG_T)

  /** Managing backpressure
    */
  val exe_stall = WireInit(Bool())
  val mem_stall = WireInit(Bool())

  // Enqueuing interface Decode -> FIFO
  // Enqueues the Decoded instrcution with matching tag
  fifo_vec map { f =>
    f.enq.valid := false.B
    f.enq.bits := io.enq.bits
  }
  io.enq.ready := fifo_vec(io.tag).enq.ready
  fifo_vec(io.tag).enq.valid := io.enq.valid

  // Dequeing buffers interface : FIFO -> Reg
  // Registers who are invalid are ready to recieve new value
  sig_pipe_r zip reg_pipe_v map { case (reg_r, reg_v) => reg_r := ~reg_v}
  fifo_vec zip (reg_pipe, reg_pipe_v, sig_pipe_r).zipped.toSeq map {
    case (fifo, (reg, reg_v, sig_r)) =>
      fifo.deq.ready := sig_r
      reg   := Mux(sig_r, fifo.deq.bits, reg)
      reg_v := Mux(sig_r, fifo.deq.valid, reg_v)
  }

  // Choose next thread to Issue (Reg status + hazards -> idx)
  // Thread is ready to be issued if it has a valid decoded instruction in register and has no hazard detected.
  sig_pipe_i.toBools zip reg_pipe_v map { case (sig_rdy,reg_v) => sig_rdy := reg_v}
  exe_stall := (io.exe_rd_v && ((reg_pipe(io.exe_tag).rs1 === io.exe_rd) || (reg_pipe(io.exe_tag).rs2 === io.exe_rd)))
  mem_stall := (io.mem_rd_v && ((reg_pipe(io.mem_tag).rs1 === io.mem_rd) || (reg_pipe(io.mem_tag).rs2 === io.mem_rd)))
  when(io.exe_tag === io.mem_tag) {
    sig_pipe_i(io.exe_tag) := reg_pipe_v(io.exe_tag) && !(exe_stall || mem_stall)
  }.otherwise {
    sig_pipe_i(io.exe_tag) := reg_pipe_v(io.exe_tag) && !exe_stall
    sig_pipe_i(io.mem_tag) := reg_pipe_v(io.mem_tag) && !mem_stall
  }

  // Get idx to issue
  arbiter.io.ready := sig_pipe_i
  arbiter.io.last  := reg_issue_idx
  sig_next_idx     := arbiter.io.next

  io.deq.bits  := reg_pipe(sig_next_idx)
  io.deq.valid := false.B
  // If at least one of the threads is ready to be issued
  when(sig_pipe_i.orR() && io.deq.ready) {
    // Set issued thread register ready to recieve next instruction
    sig_pipe_r(sig_next_idx) := true.B
    // Update for next round
    reg_issue_idx := sig_next_idx
    // Issues the thread : Reg -> Exec
    io.deq.valid := true.B
  }
}

class RRArbiter(arbN: Int)
    extends Module {

  val io = new Bundle {
    val ready = Input(UInt(arbN.W))            // Ready threads
    val last  = Input(UInt(log2Ceil(arbN).W))  // Last thread issued
    val next  = Output(UInt(log2Ceil(arbN).W)) // Next thread to issue
  }

  def rotateLeft[T <: Data](norm: Vec[T], rot: UInt): Vec[T] = {
    val n = norm.size
    VecInit.tabulate(n) { i =>
      Mux(rot < (n - i).U, norm(i.U + rot), norm(rot - (n - i).U))
    }
  }

  // Shift such as the lsb is the thread with highest priority
  val ready_next = rotateLeft(VecInit(io.ready), io.last + 1.U)
  io.next := PriorityEncoder(ready_next.asUInt)
}
