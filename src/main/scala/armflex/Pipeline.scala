// See LICENSE.txt for license details.
package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._
import arm.DECODE_CONTROL_SIGNALS._

import armflex.cache._
import armflex.util._
import armflex.util.ExtraUtils._
import Chisel.debug

class PipelineMemoryIO(implicit val cfg: ProcConfig) extends Bundle {
  val inst = new Bundle {
    val req =
      Decoupled(new CacheFrontendRequestPacket(DATA_SZ, cfg.NB_THREADS_W, cfg.BLOCK_SIZE))
    val resp =
      Input(Valid(new FrontendReplyPacket(cfg.BLOCK_SIZE, cfg.NB_THREADS_W)))
  }
  val data = new Bundle {
    val req = Decoupled(new CacheFrontendRequestPacket(DATA_SZ, cfg.NB_THREADS_W, cfg.BLOCK_SIZE))
    val resp = Input(Valid(new FrontendReplyPacket(cfg.BLOCK_SIZE, cfg.NB_THREADS_W)))
  }
  val wake = Input(Vec(4, ValidTag(cfg.TAG_T)))
}

class IssueArchStateIO[T <: UInt](gen: T) extends Bundle {
  val sel = Output(ValidTag(gen))
  val regs = new Bundle { val curr = Input(new PStateRegs) }
  val rd = Flipped(new RFileSingleIO.RDPort(gen))
  val ready = Input(Bool())
}

class PipeArchStateIO[T <: UInt](gen: T) extends Bundle {
  val issue = new IssueArchStateIO(gen)
  val commit = new CommitArchStateIO(gen)
  override def cloneType: this.type = new PipeArchStateIO[T](gen).asInstanceOf[this.type]
}

/** Processor
  */
class Pipeline(implicit val cfg: ProcConfig) extends MultiIOModule {
  // --------- IO -----------
  // Device communication
  //val io = IO(new Bundle {
  //  val perfStats = Output(new PerfStats)
  //  val resetStats = Input(UInt(8.W))
  //})
  // Memory Hierarchy
  val mem = IO(new PipelineMemoryIO)

  // Transplant case
  val transplantIO = IO(new Bundle {
    val start = Input(ValidTag(cfg.TAG_T, DATA_T))
    val done = Output(ValidTag(cfg.TAG_T, INST_T))
  })
  // ISA State
  val archstate = IO(new PipeArchStateIO(cfg.TAG_T))

  // ----- System modules ------

  // Pipeline -----------------------------------------------
  //val fetch = IO(Flipped(Decoupled(INST_T)))
  val fetch = Module(new FetchUnit)
  val fetchReg = Module(new FlushReg(cfg.TAG_T, INST_T))
  // Decode
  val decoder = Module(new DecodeUnit)
  val decReg = Module(new FlushReg(cfg.TAG_T, new DInst))
  // Issue
  val issuer = Module(new FlushReg(cfg.TAG_T, new DInst))
  // |         |        |            |
  // | Execute |        |            |
  // |         | Branch |            |
  // |         |        | Load Store |
  // |         |        |            |
  val executer = Module(new ExecuteUnit())
  val brancher = Module(new BranchUnit())
  val ldstU = Module(new LDSTUnit())
  // (MemInst || Issuer) -> CommitReg

  val memHandler = Module(new MemoryAdaptor)
  // Commit
  val commitU = Module(new CommitUnit(cfg.TAG_T, cfg.NB_THREADS))

  // Interconnect -------------------------------------------

  // Fetch next instruction ---
  // Transplant
  fetch.ctrl.start := transplantIO.start
  // Wake on memory miss completed
  fetch.ctrl.memWake := mem.wake
  // Wake on state commit
  fetch.ctrl.commit.valid := commitU.commit.commited.valid && !commitU.commit.transplant.valid
  fetch.ctrl.commit.tag := commitU.commit.commited.tag
  fetch.ctrl.commit.bits.get := commitU.commit.archstate.regs.next.PC

  mem.inst.req.valid := false.B
  mem.inst.req.bits.thread_id := fetch.mem.tag
  mem.inst.req.bits.addr := fetch.mem.bits
  mem.inst.req.bits.w_v := false.B
  mem.inst.req.bits.wData := DontCare
  mem.inst.req.bits.wMask := DontCare

  // 3 Way handshake fetch || mem.inst.req || fetchRegResp
  fetch.mem.ready := false.B
  fetch.mem.handshake(mem.inst.req, fetchReg.io.enq.ready)

  // TODO Ready signal from the fetching register has 1 cycle delay -> potential data error
  val fetchLatency = mem.inst.latency
  fetchReg.io.enq.valid := ShiftRegister(fetch.mem.fire, fetchLatency)
  fetchReg.io.enq.tag := ShiftRegister(fetch.mem.tag, fetchLatency)
  fetchReg.io.enq.bits := mem.inst.resp.bits.data
  fetchReg.io.enq.valid := mem.inst.resp.valid

  // Fetch -> Decode
  decoder.inst := fetchReg.io.deq.bits
  decReg.io.enq.bits := decoder.dinst
  decReg.io.enq.handshake(fetchReg.io.deq)

  // Decode -> Issue
  issuer.io.enq <> decReg.io.deq

  // Execute : Issue -> Execute
  val issued_dinst = WireInit(issuer.io.deq.bits)

  /** Execute */

  // connect rfile read(address) interface
  archstate.issue.sel.tag :=  issuer.io.deq.tag
  archstate.issue.sel.valid := issuer.io.deq.fire
  archstate.issue.rd.tag := issuer.io.deq.tag
  archstate.issue.rd.port(0).addr := issued_dinst.rs1
  archstate.issue.rd.port(1).addr := issued_dinst.rs2
  when(issued_dinst.itype === I_DP3S) {
    // TODO Triple register instruction
    // archstate.issue.rd.port(0).addr := issued_dinst.imm(4, 0)
  }

  // Read register data from rfile
  val rVal1 = archstate.issue.rd.port(0).data
  val rVal2 = archstate.issue.rd.port(1).data
  val rVal3 = archstate.issue.rd.port(0).data // TODO Triple source

  // connect executeUnit interface
  executer.io.dinst := issued_dinst
  executer.io.rVal1 := Mux(issued_dinst.itype === I_DP3S, RegNext(rVal1), rVal1)
  executer.io.rVal2 := Mux(issued_dinst.itype === I_DP3S, RegNext(rVal2), rVal2)
  executer.io.rVal3 := rVal3 // rVal3 takes an extra cycle to arrive -> Take RegNext()
  executer.io.nzcv := archstate.issue.regs.curr.NZCV

  // connect BranchUnit interface
  brancher.io.dinst := issued_dinst
  brancher.io.rVal1 := rVal1
  brancher.io.rVal2 := rVal2
  brancher.io.cond := executer.io.condRes
  brancher.io.pc := archstate.issue.regs.curr.PC

  // connect LDSTUnit interface
  ldstU.io.dinst := issued_dinst
  ldstU.io.rVal1 := rVal1
  ldstU.io.rVal2 := rVal2
  ldstU.io.pstate := archstate.issue.regs.curr

  // ------ Pack Execute/LDST result
  memHandler.pipe.req.bits := ldstU.io.minst.bits
  memHandler.pipe.req.bits.tag := issuer.io.deq.tag
  mem.data.req <> memHandler.mem.req
  memHandler.mem.resp <> mem.data.resp

  // - Exceptions -
  val memException = WireInit(
    ldstU.io.minst.valid &&
      !ldstU.io.minst.bits.exceptions.valid
  )
  val unalignedExcpData = WireInit(
    ldstU.io.minst.bits.exceptions.bits.unalignedExcp
  )
  val unalignedExcpSP = WireInit(
    ldstU.io.minst.bits.exceptions.bits.unalignedExcpSP
  )

  val branchException = WireInit(
    brancher.io.binst.valid &&
      brancher.io.binst.bits.unalignedExcp
  )

  // CommitReg
  val commitNext = WireInit(CommitInst(cfg.TAG_T))
  when(brancher.io.pcrel.valid) {
    commitNext.rd(0).valid := true.B
    commitNext.rd(0).bits := brancher.io.pcrel.bits.rd
    commitNext.res(0) := brancher.io.pcrel.bits.res
  }.elsewhen(executer.io.einst.bits.rd.valid) {
    commitNext.rd(0).valid := true.B
    commitNext.rd(0).bits := executer.io.einst.bits.rd.bits
    commitNext.res(0) := executer.io.einst.bits.res
  }
  assert(!(executer.io.einst.valid && brancher.io.pcrel.valid))

  commitNext.nzcv.valid := executer.io.einst.bits.nzcv.valid && executer.io.einst.valid
  commitNext.nzcv.bits := executer.io.einst.bits.nzcv.bits

  commitNext.br_taken.valid := brancher.io.binst.valid
  commitNext.br_taken.bits := brancher.io.binst.bits.pc

  commitNext.exceptions.valid := memException || branchException
  commitNext.exceptions.bits := Cat(
    brancher.io.binst.bits.unalignedExcp.asUInt,
    unalignedExcpData.asUInt,
    unalignedExcpSP.asUInt
  )

  commitNext.undef := !issued_dinst.inst32.valid
  commitNext.inst := issued_dinst.inst32.bits
  commitNext.is32bit := issued_dinst.is32bit
  commitNext.tag := issuer.io.deq.tag

  // Memory Resp


  // ----- Control Execute Stage Handshakes -----
  // Handle response from Memory Hierarchy before Issued Inst
  // Handshakes of IssuerDeq, memInst and CommitReg
  issuer.io.deq.ready := false.B
  memHandler.pipe.req.valid := false.B
  mem.data.req.valid := false.B
  commitU.enq.valid := false.B

  val ldstInstruction = ldstU.io.minst.valid && !ldstU.io.minst.bits.exceptions.valid
  val commitAsIssue = commitU.commit.commited.valid && issuer.io.deq.tag === commitU.commit.commited.tag
  commitU.enq.bits := Mux(mem.data.resp.valid, memHandler.pipe.resp.bits, commitNext)
  when(memHandler.pipe.resp.valid) {
    // When memory response arrives, don't take from issue but from response
    commitU.enq.valid := true.B
  }.otherwise {
    commitU.enq.handshake(issuer.io.deq, archstate.issue.ready && !ldstInstruction)
  }
  when(ldstInstruction) {
    issuer.io.deq.handshake(memHandler.pipe.req, archstate.issue.ready)
  }

  // Assertions:  Issuer Deq | memInst Req&Resp | CommitReg Enq
  when(issuer.io.deq.fire) {
    assert(commitU.enq.fire || memHandler.pipe.req.fire)
    assert(!(commitU.enq.fire && memHandler.pipe.req.fire))
    when(commitU.commit.commited.valid) { assert(issuer.io.deq.tag =/= commitU.commit.commited.tag) }
  }
  when(commitU.enq.fire) {
    assert(mem.data.resp.valid || issuer.io.deq.fire)
    assert(!(mem.data.resp.valid && issuer.io.deq.fire))
  }

  // ------ Commit Stage ------
  // Commit State
  archstate.commit <> commitU.commit.archstate
  transplantIO.done := commitU.commit.transplant

  // Flushing ----------------------------------------------------------------
  // No speculative state is kept in the pipeline in current version
  // The Flush signal is no longer needed
  decReg.io.flush.valid := false.B
  issuer.io.flush.valid := false.B
  fetchReg.io.flush.tag := DontCare
  decReg.io.flush.tag := DontCare
  issuer.io.flush.tag := DontCare

  // DEBUG Signals ------------------------------------------------------------
  val dbg = IO(new Bundle {
    val issuingMem = Output(Bool())
  })
  dbg.issuingMem := ldstU.io.minst.valid
}

import chisel3.util.{Cat, Queue, ShiftRegister}
import chisel3.experimental.BundleLiterals._

class FullStateBundle extends Bundle {
  val rfile = Vec(REG_N, DATA_T)
  val regs = new PStateRegs
}

object FullStateBundle {
  def apply(state: armflex.util.SoftwareStructs.PState): FullStateBundle = {
    // TODO No way to Vec.lit yet
    val rfile = state.xregs.map { _.U }
    val vec = Vec(REG_N, DATA_T)
    //.getElements zip rfile map {
    //  case (ele, xreg) => ele -> xreg
    //}
    val regs = (new PStateRegs).Lit(
      _.PC -> state.pc.U,
      _.SP -> state.sp.U,
      _.NZCV -> state.nzcv.U
    )
    (new FullStateBundle).Lit(
      _.rfile -> vec,
      _.regs -> regs.litValue.U
    )
  }
}
