// See LICENSE.txt for license details.
package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._
import arm.DECODE_CONTROL_SIGNALS._


import armflex.util._
import armflex.util.ExtraUtils._

class PipelineMemoryIO(paddrWidth: Int, thidWidth: Int, asidWidth: Int, blockSize: Int) extends Bundle {
  val inst = new PipeMemPortIO(DATA_SZ, paddrWidth, thidWidth, asidWidth, blockSize)
  val data = new PipeMemPortIO(DATA_SZ, paddrWidth, thidWidth, asidWidth, blockSize)
  val wake = Input(Vec(2, ValidTag(UInt(thidWidth.W))))
}

class IssueArchStateIO(nbThreads: Int) extends Bundle {
  val sel = Output(ValidTag(nbThreads))
  val regs = new Bundle { val curr = Input(new PStateRegs) }
  val rd = Flipped(new RFileIO.RDPort(nbThreads))
  val ready = Input(Bool())
}

class PipeArchStateIO(nbThreads: Int) extends Bundle {
  val issue = new IssueArchStateIO(nbThreads)
  val commit = new CommitArchStateIO(nbThreads)
  override def cloneType: this.type = new PipeArchStateIO(nbThreads).asInstanceOf[this.type]
}

/** Processor
  */
class Pipeline(implicit val cfg: ProcConfig) extends MultiIOModule {
  // --------- IO -----------
  // Memory Hierarchy
  val mem_io = IO(new PipelineMemoryIO(cfg.pAddressWidth, cfg.NB_THREADS_W, cfg.ASID_WIDTH, cfg.BLOCK_SIZE))
  val mmu_io = IO(new PipeMMUIO)
  // Transplant case
  val transplantIO = IO(new Bundle {
    val start = Input(ValidTag(cfg.TAG_T, DATA_T))
    val done = Output(ValidTag(cfg.TAG_T, INST_T))
  })
  // ISA State
  val archstate = IO(new PipeArchStateIO(cfg.NB_THREADS))

  // ----- System modules ------

  // Pipeline -----------------------------------------------
  val fetch = Module(new FetchUnit)
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

  val memUnit = Module(new MemoryUnit)
  // Commit
  val commitU = Module(new CommitUnit(cfg.NB_THREADS))

  // Interconnect -------------------------------------------

  // --- Enable Fetch ---
  // Start on transplant
  fetch.ctrl_i.start := transplantIO.start
  // Wake on TLB miss completed
  fetch.ctrl_i.memWake := mem_io.wake
  // Wake on instruction commit
  fetch.ctrl_i.commit.valid := commitU.commit.commited.valid && !commitU.commit.transplant.valid
  fetch.ctrl_i.commit.tag := commitU.commit.commited.tag
  fetch.ctrl_i.commit.bits.get := commitU.commit.archstate.regs.next.PC

  // --- Fetch PC from Mem ---
  fetch.mem_io <> mem_io.inst
  fetch.mmu_io <> mmu_io.inst

  // --- Fetch Inst -> Decode ---
  decoder.inst := fetch.instQ_o.bits.data
  decReg.io.enq.bits := decoder.dinst
  decReg.io.enq.tag := fetch.instQ_o.bits.tag
  decReg.io.enq.handshake(fetch.instQ_o)

  // --- Decode -> Issue ---
  // Read from RFile, 1 cycle delay
  archstate.issue.sel.tag := decReg.io.deq.tag
  archstate.issue.sel.valid := decReg.io.deq.fire
  archstate.issue.rd.tag := decReg.io.deq.tag
  archstate.issue.rd.port(0).addr := decReg.io.deq.bits.rs1
  archstate.issue.rd.port(1).addr := decReg.io.deq.bits.rs2
  issuer.io.enq <> decReg.io.deq // Issue is always ready, so no check for archstate.issue.ready necessary
  // TODO When Issuer io deq ! ready -> register RFile RD output

  // Execute : Issue -> Execute
  val issued_dinst = WireInit(issuer.io.deq.bits)

  // Issue ---------------------------
  // connect rfile read(address) interface
  when(issued_dinst.itype === I_DP3S) {
    // TODO Triple register instruction
    // archstate.issue.rd.port(0).addr := issued_dinst.imm(4, 0)
  }

  // Execute ---------------------------
  // Read register data from rfile
  val stateReadArrives = RegNext(decReg.io.deq.fire)
  val rVal1_reg = RegInit(DATA_X)
  val rVal2_reg = RegInit(DATA_X)
  val rVal3_reg = RegInit(DATA_X)
  val state_reg = RegInit(PStateRegs())
  val rVal1 = Mux(stateReadArrives, archstate.issue.rd.port(0).data, rVal1_reg)
  val rVal2 = Mux(stateReadArrives, archstate.issue.rd.port(1).data, rVal2_reg)
  val rVal3 = Mux(stateReadArrives, archstate.issue.rd.port(0).data, rVal3_reg) // TODO Triple source
  val curr_state = Mux(stateReadArrives, RegNext(archstate.issue.regs.curr), state_reg) // TODO Triple source
  when(stateReadArrives) { 
    // Read Register arrived
    rVal1_reg := archstate.issue.rd.port(0).data
    rVal2_reg := archstate.issue.rd.port(1).data
    rVal3_reg := archstate.issue.rd.port(0).data
    state_reg := archstate.issue.regs.curr
  }

  // connect executeUnit interface
  executer.io.dinst := issued_dinst
  executer.io.rVal1 := rVal1
  executer.io.rVal2 := rVal2
  executer.io.rVal3 := rVal3 // rVal3 takes an extra cycle to arrive -> Take RegNext()
  executer.io.nzcv := curr_state.NZCV

  // connect BranchUnit interface
  brancher.io.dinst := issued_dinst
  brancher.io.rVal1 := rVal1
  brancher.io.rVal2 := rVal2
  brancher.io.cond := executer.io.condRes
  brancher.io.pc := curr_state.PC

  // connect LDSTUnit interface
  ldstU.io.dinst := issued_dinst
  ldstU.io.rVal1 := rVal1
  ldstU.io.rVal2 := rVal2
  ldstU.io.pstate := curr_state

  // ------ Pack Execute/LDST result
  memUnit.pipe.req.bits.:=(ldstU.io.minst.bits) // Enforce := method of MInstTag
  memUnit.pipe.req.bits.tag := issuer.io.deq.tag
  memUnit.mem_io <> mem_io.data
  memUnit.mmu_io <> mmu_io.data

  // - Exceptions -
  val memException = WireInit(
    ldstU.io.minst.valid &&
      ldstU.io.minst.bits.exceptions.valid
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
  val commitNext = WireInit(CommitInst(cfg.NB_THREADS))
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
  memUnit.pipe.req.valid := false.B
  memUnit.pipe.resp.ready := false.B
  commitU.enq.valid := false.B

  val ldstInstruction = ldstU.io.minst.valid && !ldstU.io.minst.bits.exceptions.valid
  val commitAsIssue = commitU.commit.commited.valid && issuer.io.deq.tag === commitU.commit.commited.tag
  when(memUnit.pipe.resp.valid) {
    // When memory response arrives, don't take from issue but from response
    commitU.enq <> memUnit.pipe.resp
  }.otherwise {
    commitU.enq.handshake(issuer.io.deq, !ldstInstruction)
    commitU.enq.bits := commitNext
  }

  when(ldstInstruction) {
    issuer.io.deq.handshake(memUnit.pipe.req)
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
  decReg.io.flush.tag := DontCare
  issuer.io.flush.tag := DontCare

  // DEBUG Signals ------------------------------------------------------------
  val dbg = IO(new Bundle {
    val issue = Output(new Bundle {
      val valid = Bool()
      val thread = cfg.TAG_T
      val mem = Bool()
      val transplant = Bool()
    })
  })
  dbg.issue.valid := issuer.io.deq.fire
  dbg.issue.thread := issuer.io.deq.tag
  dbg.issue.mem := ldstU.io.minst.valid
  dbg.issue.transplant := commitU.enq.bits.exceptions.valid || commitU.enq.bits.undef

  if(true) { // TODO Conditional Assertions
    // Assertions:  Issuer Deq | memInst Req&Resp | CommitReg Enq
    when(issuer.io.deq.fire) {
        assert(commitU.enq.fire || memUnit.pipe.req.fire)
      assert(!(commitU.enq.fire && memUnit.pipe.req.fire))
      when(commitU.commit.commited.valid) { 
          assert(issuer.io.deq.tag =/= commitU.commit.commited.tag) 
        }
    }
    when(commitU.enq.fire) {
        assert(memUnit.pipe.resp.valid || issuer.io.deq.fire)
      assert(!(memUnit.pipe.resp.valid && issuer.io.deq.fire))
    }
  }
}

class FullStateBundle extends Bundle {
  val rfile = Vec(REG_N, DATA_T)
  val regs = new PStateRegs
}