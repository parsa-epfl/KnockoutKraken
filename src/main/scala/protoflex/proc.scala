// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.{Queue}

import common.PROCESSOR_TYPES._

/** Processor
  *
  */
class Proc extends Module
{
  val io = IO(new Bundle {
                val inst = Input(INST_T)
                val tag  = Input(TAG_T)
                val valid = Input(Bool())
                val ready = Output(Bool())
              })

  val pstate_vec = RegInit(VecInit(Seq.fill(NUM_THREADS)(new PSTATE())))
  val rfile = Module(new RegisterFile())

  val decoder = Module(new DecodeUnit())
  val dec_reg = Module(new Queue(new DInst, 1, pipe = true, flow = false))

  val issuer = Module(new IssueUnit())

  val executer = Module(new ExecuteUnit())
  val exe_reg = Module(new Queue(new EInst, 1, pipe = true, flow = false))

  val brancher = Module(new BranchUnit())


  // Decode instruction
  decoder.io.inst := io.inst
  decoder.io.tag := io.tag

  // Save into regsiter
  io.ready := dec_reg.io.enq.ready
  dec_reg.io.enq.valid := io.valid
  dec_reg.io.enq.bits  := decoder.io.dinst

  // Put in issue queues
  issuer.io.enq <> dec_reg.io.deq

  // Issue - Execute
  val issued_dinst = issuer.io.deq.bits
  executer.io.dinst := issued_dinst
  exe_reg.io.enq.bits  := executer.einst
  exe_reg.io.enq.valid := issuer.io.deq.valid
  issuer.io.deq.ready  := exe_reg.io.enq.ready
  // Issue - Branch
  brancher.io.dinst := issued_dinst

  when (brancher.io.valid) {
    pstate_vec(issued_dinst.tag).PC := (pstate_vec(issued_dinst.tag).PC.asSInt + brancher.io.offset.asSInt).asUInt
  }

}

