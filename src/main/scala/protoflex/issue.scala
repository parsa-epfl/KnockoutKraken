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
  // Fetch - (Decode) - Issue
  val enq = EnqIO(new DInst)
  val tag = Input(TAG_T)

  // Issue - Exec
  val deq = DeqIO(new DInst)

  //`Exec - Issue
  val exe_rd = Input(REG_T)
  val exe_tag = Input(TAG_T)
//  val exe_res = Input(DATA_T)
  // Mem - Issue
  val mem_rd = Input(REG_T)
  val mem_tag = Input(TAG_T)
//  val mem_res = Input(DATA_T)

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
    * reg_pipe_r This signal indicates whenever the register is ready to recieve the next instruction.
    */
  val reg_pipe   = RegInit(VecInit(Seq.fill(NUM_THREADS)(new DInst)))
  val reg_pipe_v = RegInit(VecInit(0.U(NUM_THREADS.W).toBools))
  val sig_pipe_r = WireInit(VecInit(UInt(NUM_THREADS.W).toBools))

  /** Issue stage buffer components
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

  /** Arbiter
    * arbiter        Round Robin Arbiter
    * reg_issue_idx  Contains the index of the last issued thread
    * sig_next_idx   This signal indicates the next instruction to issue
    * sig_pipe_i     This signal indicates which threads are ready to be issued
    */
  val arbiter       = new RRArbiter(NUM_THREADS)
  val reg_issue_idx = Reg(0.U(TAG_T))
  val sig_next_idx  = Wire(TAG_T)
  val sig_pipe_i    = Wire(UInt(NUM_THREADS.W))

  // Enqueuing interface Decode -> FIFO
  // By default not valid, if the tag matches, overwrites to the valid input
  io.enq.ready := fifo_vec(io.tag).enq.ready
  fifo_vec map ( f => f.enq.valid := false.B )
  fifo_vec(io.tag).enq.valid := io.enq.valid
  fifo_vec map ( f => f.enq.bits := io.enq.bits )

  // Dequeing buffered interface : FIFO -> Reg
  // Registers who are invalid are ready to recieve new value
  sig_pipe_r zip reg_pipe_v map { case (rdy, valid) => rdy := ~valid }
  fifo_vec zip (reg_pipe, reg_pipe_v, sig_pipe_r).zipped.toSeq map {
    case (fifo, (reg, reg_v, sig_r)) =>
      fifo.deq.ready := sig_r
      reg   := Mux(sig_r, fifo.deq.bits, reg)
      reg_v := Mux(sig_r, fifo.deq.valid, reg_v)
  }

  // Choose next thread to Issue (Reg, hazards -> idx)
  // Thread is ready to issue if it has a valid decoded instruction in register and has no hazard detected.
  sig_pipe_i.toBools zip reg_pipe_v map { case (rdy,valid) => rdy := valid }
  sig_pipe_i(io.exe_tag) := (reg_pipe_v(io.exe_tag) & (reg_pipe(io.exe_tag).rd != io.exe_rd)).asUInt
  sig_pipe_i(io.mem_tag) := (reg_pipe_v(io.mem_tag) & (reg_pipe(io.mem_tag).rd != io.mem_rd)).asUInt
  // Arbiter interface
  arbiter.io.ready := sig_pipe_i.data
  arbiter.io.last  := reg_issue_idx
  sig_next_idx     := arbiter.io.next
  // Update for next round
  reg_issue_idx := sig_next_idx

  // Issues the thread : Reg -> Exec
  io.deq.bits  := reg_pipe(sig_next_idx.data)
  io.deq.valid := reg_pipe_v(sig_next_idx.data)
  // Set thread ready to recieve next value if next stage is ready
  sig_pipe_i(sig_next_idx.data) := io.deq.ready

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
