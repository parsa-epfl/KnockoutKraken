// See LICENSE.txt for license details.
package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._
import arm.DECODE_CONTROL_SIGNALS._


import armflex.util._
import armflex.util.ExtraUtils._

class PipelineMemoryIO(pAddrW: Int, thidW: Int, asidW: Int, blockSize: Int) extends Bundle {
  val inst = new PipeMemPortIO(DATA_SZ, pAddrW, thidW, asidW, blockSize)
  val data = new PipeMemPortIO(DATA_SZ, pAddrW, thidW, asidW, blockSize)
  val wake = Input(Vec(2, ValidTag(UInt(thidW.W))))
}

class IssueArchStateIO(thidN: Int) extends Bundle {
  val sel = Output(ValidTag(thidN))
  val regs = new Bundle { val curr = Input(new PStateRegs) }
  val rd = Flipped(new RFileIO.RDPort(thidN))
  val ready = Input(Bool())
}

class PipeArchStateIO(thidN: Int) extends Bundle {
  val issue = new IssueArchStateIO(thidN)
  val commit = new CommitArchStateIO(thidN)
}

/** Processor
  */
class Pipeline(params: PipelineParams) extends Module {
  // --------- IO -----------
  // Memory Hierarchy
  val mem_io = IO(new PipelineMemoryIO(params.pAddrW, params.thidW, params.asidW, params.blockSize))
  val mmu_io = IO(new PipeMMUIO)
  // Transplant case
  val transplantIO = IO(new Bundle {
    val stopCPU = Input(UInt(params.thidN.W))
    val start = Input(ValidTag(params.thidT, DATA_T))
    val done = Output(ValidTag(params.thidT, INST_T))
  })
  // ISA State
  val archstate = IO(new PipeArchStateIO(params.thidN))

  // ----- System modules ------

  // Pipeline -----------------------------------------------
  val fetch = Module(new FetchUnit(params))
  // Decode
  val decoder = Module(new DecodeUnit)
  val decReg = Module(new FlushReg(params.thidT, new DInst))
  // Issue
  val issuer = Module(new FlushReg(params.thidT, new DInst))
  // |         |        |            |
  // | Execute |        |            |
  // |         | Branch |            |
  // |         |        | Load Store |
  // |         |        |            |
  val executer = Module(new ExecuteUnit)
  val brancher = Module(new BranchUnit)
  val ldstU = Module(new LDSTUnit)
  // (MemInst || Issuer) -> CommitReg

  val memUnit = Module(new MemoryUnit(params))
  // Commit
  val commitU = Module(new CommitUnit(params.thidN))

  // Interconnect -------------------------------------------

  // --- Enable Fetch ---
  // Start on transplant
  fetch.ctrl_i.start := transplantIO.start
  // Wake on TLB miss completed
  fetch.ctrl_i.memWake := mem_io.wake
  // Wake on instruction commit
  fetch.ctrl_i.commit.valid := 
    commitU.commit.commited.valid && 
    !commitU.commit.transplant.valid && 
    !transplantIO.stopCPU(commitU.commit.commited.tag).asBool && 
    !commitU.commit.archstate.last
  fetch.ctrl_i.commit.tag := commitU.commit.commited.tag
  fetch.ctrl_i.commit.bits.get := commitU.commit.archstate.pstate.next.PC

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
  archstate.issue.rd.port(2).addr := decReg.io.deq.bits.rd.bits
  issuer.io.enq <> decReg.io.deq // Issue is always ready, so no check for archstate.issue.ready necessary
  // connect rfile read(address) interface
  when(decReg.io.deq.bits.itype === I_DP3S) {
    archstate.issue.rd.port(2).addr := decReg.io.deq.bits.imm(4, 0)
  }

  // Issue ---------------------------
  // Execute : Issue -> Execute
  val issued_dinst = WireInit(issuer.io.deq.bits)

  // Execute ---------------------------
  // Read register data from rfile
  val stateReadArrives = RegNext(decReg.io.deq.fire)
  val rVal1_reg = RegInit(DATA_X)
  val rVal2_reg = RegInit(DATA_X)
  val rVal3_reg = RegInit(DATA_X)
  val state_reg = RegInit(PStateRegs())
  val rVal1 = WireInit(Mux(stateReadArrives, archstate.issue.rd.port(0).data, rVal1_reg))
  val rVal2 = WireInit(Mux(stateReadArrives, archstate.issue.rd.port(1).data, rVal2_reg))
  val rVal3 = WireInit(Mux(stateReadArrives, archstate.issue.rd.port(2).data, rVal3_reg))
  val curr_state = WireInit(Mux(stateReadArrives, RegNext(archstate.issue.regs.curr), state_reg))
  when(stateReadArrives) {
    // Read Register arrived
    rVal1_reg := archstate.issue.rd.port(0).data
    rVal2_reg := archstate.issue.rd.port(1).data
    rVal3_reg := archstate.issue.rd.port(2).data
    state_reg := RegNext(archstate.issue.regs.curr)
  }

  // connect executeUnit interface
  executer.io.dinst := issued_dinst
  executer.io.rVal1 := rVal1
  executer.io.rVal2 := rVal2
  executer.io.rVal3 := rVal3
  executer.io.nzcv := curr_state.flags.NZCV

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
  ldstU.io.rVal3 := rVal3
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

  val branchException = WireInit(
    brancher.io.binst.valid &&
      brancher.io.binst.bits.unalignedExcp
  )

  // CommitReg
  val commitNext = WireInit(CommitInst(params.thidN))
  when(brancher.io.pcrel.valid) {
    commitNext.rd(0).valid := true.B
    commitNext.rd(0).bits := brancher.io.pcrel.bits.rd
    commitNext.res(0) := brancher.io.pcrel.bits.res
  }.elsewhen(executer.io.einst.bits.rd.valid) {
    commitNext.rd(0).valid := true.B
    commitNext.rd(0).bits := executer.io.einst.bits.rd.bits
    commitNext.res(0) := executer.io.einst.bits.res
  }

  commitNext.nzcv.valid := executer.io.einst.bits.nzcv.valid && executer.io.einst.valid
  commitNext.nzcv.bits := executer.io.einst.bits.nzcv.bits

  commitNext.br_taken.valid := brancher.io.binst.valid
  commitNext.br_taken.bits := brancher.io.binst.bits.pc

  commitNext.exceptions.valid := memException || branchException
  commitNext.exceptions.bits := Cat(
    brancher.io.binst.bits.unalignedExcp.asUInt,
    unalignedExcpData.asUInt
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
  transplantIO.done.tag := commitU.commit.commited.tag
  transplantIO.done.bits.get := commitU.commit.transplant.bits.get
  when(transplantIO.stopCPU(commitU.commit.commited.tag).asBool) {
    transplantIO.done.valid := commitU.commit.commited.valid
  }.otherwise {
    transplantIO.done.valid := commitU.commit.transplant.valid || (commitU.commit.archstate.fire && commitU.commit.archstate.last)
  }

  // Flushing ----------------------------------------------------------------
  // No speculative state is kept in the pipeline in current version
  // The Flush signal is no longer needed
  decReg.io.flush.valid := false.B
  issuer.io.flush.valid := false.B
  decReg.io.flush.tag := DontCare
  issuer.io.flush.tag := DontCare

  // Instrumentation Interface -----------------------------------------------
  val instrument = IO(new Bundle {
    val commit = commitU.deq.cloneType
  })
  instrument.commit <> commitU.deq

  // DEBUG Signals ------------------------------------------------------------
  val dbg = IO(new Bundle {
    val issue = Output(new Bundle {
      val valid = Bool()
      val thread = params.thidT
      val mem = Bool()
      val transplant = Bool()
    })
  })
  dbg.issue.valid := issuer.io.deq.fire
  dbg.issue.thread := issuer.io.deq.tag
  dbg.issue.mem := ldstU.io.minst.valid
  dbg.issue.transplant := commitU.enq.bits.exceptions.valid || commitU.enq.bits.undef

  if(true) { // TODO Conditional Assertions
    assert(!(executer.io.einst.valid && brancher.io.pcrel.valid), "Can't have both instruction valid at the same time")
    // Assertions:  Issuer Deq | MemUnit | CommitReg Enq
    when(issuer.io.deq.fire) {
      assert(commitU.enq.fire || memUnit.pipe.req.fire, "In case of Issue fire, either the MemUnit or the commit queue must receive the transaction")
      assert(!((commitU.enq.fire && !memUnit.pipe.resp.fire) && memUnit.pipe.req.fire), "Both the MemUnit and commit queue can't receive the transaction at the same time")
      when(commitU.commit.commited.valid) { 
        assert(issuer.io.deq.tag =/= commitU.commit.commited.tag, "Instructions from the same context can't be present in two stages concurrently")
      }
    }
    when(commitU.enq.fire) {
      assert(memUnit.pipe.resp.valid || (issuer.io.deq.fire && !memUnit.pipe.req.fire), "When the commit queue receives a transaction, it must either come from the MemUnit or Issue stage")
      assert(!(memUnit.pipe.resp.valid && (issuer.io.deq.fire && !memUnit.pipe.req.fire)), "When the commit queue receives a transaction, it can't come from both the MemUnit or Issue stage")
    }
  }
}

