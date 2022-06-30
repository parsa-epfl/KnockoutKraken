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
    val done = Output(ValidTag(params.thidT))
    val status = Flipped(new TransplantIO.TransplantStatus(params.thidN))
  })
  // ISA State
  val archstate = IO(new PipeArchStateIO(params.thidN))

  // ----- System modules ------

  // Pipeline -----------------------------------------------
  val fetchU = Module(new FetchUnit(params))
  // Decode
  val decoderU = Module(new DecodeUnit)
  val decReg = Module(new Queue(new Tagged(params.thidT, new DInst), 1, true, false))
  // Issue
  val issuerU = Module(new Queue(new Tagged(params.thidT, new DInst), 1, true, false))
  // |         |        |            |
  // | Execute |        |            |
  // |         | Branch |            |
  // |         |        | Load Store |
  // |         |        |            |
  val executerU = Module(new ExecuteUnit)
  val brancherU = Module(new BranchUnit)
  val ldstU = Module(new LDSTUnit)
  // (MemInst || Issuer) -> CommitReg

  val memU = Module(new MemoryUnit(params))
  // Commit
  val commitU = Module(new CommitUnit(params.thidN))

  // Interconnect -------------------------------------------

  // --- Enable Fetch ---
  // Start on transplant
  fetchU.ctrl_i.start := transplantIO.start
  // Wake on TLB miss completed
  fetchU.ctrl_i.memWake := mem_io.wake
  // Wake on instruction commit
  fetchU.ctrl_i.commit.valid := 
    commitU.commit.commited.valid && 
    !commitU.commit.transplant.valid && 
    !transplantIO.stopCPU(commitU.commit.commited.tag).asBool && 
    !commitU.commit.archstate.icountLastInst
  fetchU.ctrl_i.commit.tag := commitU.commit.commited.tag
  fetchU.ctrl_i.commit.bits.get := commitU.commit.archstate.pstate.next.PC

  // --- Fetch PC from Mem ---
  fetchU.mem_io <> mem_io.inst
  fetchU.mmu_io <> mmu_io.inst

  // --- Fetch Inst -> Decode ---
  decoderU.inst := fetchU.instQ_o.bits.data
  decReg.io.enq.bits.data := decoderU.dinst
  decReg.io.enq.bits.tag := fetchU.instQ_o.bits.tag
  decReg.io.enq.handshake(fetchU.instQ_o)

  // --- Decode -> Issue ---
  // Read from RFile, 1 cycle delay
  archstate.issue.sel.tag := decReg.io.deq.bits.tag
  archstate.issue.sel.valid := decReg.io.deq.fire
  archstate.issue.rd.tag := decReg.io.deq.bits.tag
  archstate.issue.rd.port(0).addr := decReg.io.deq.bits.data.rs1
  archstate.issue.rd.port(1).addr := decReg.io.deq.bits.data.rs2
  archstate.issue.rd.port(2).addr := decReg.io.deq.bits.data.rd.bits
  issuerU.io.enq <> decReg.io.deq // Issue is always ready, so no check for archstate.issue.ready necessary
  // connect rfile read(address) interface
  when(decReg.io.deq.bits.data.itype === I_DP3S) {
    archstate.issue.rd.port(2).addr := decReg.io.deq.bits.data.imm(4, 0)
  }

  // Issue ---------------------------
  // Execute : Issue -> Execute
  val issued_dinst = WireInit(issuerU.io.deq.bits.data)

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
  executerU.io.dinst := issued_dinst
  executerU.io.rVal1 := rVal1
  executerU.io.rVal2 := rVal2
  executerU.io.rVal3 := rVal3
  executerU.io.nzcv := curr_state.flags.NZCV

  // connect BranchUnit interface
  brancherU.io.dinst := issued_dinst
  brancherU.io.rVal1 := rVal1
  brancherU.io.rVal2 := rVal2
  brancherU.io.cond := executerU.io.condRes
  brancherU.io.pc := curr_state.PC

  // connect LDSTUnit interface
  ldstU.io.dinst := issued_dinst
  ldstU.io.rVal1 := rVal1
  ldstU.io.rVal2 := rVal2
  ldstU.io.rVal3 := rVal3
  ldstU.io.pstate := curr_state

  // ------ Pack Execute/LDST result
  memU.pipe.req.bits.:=(ldstU.io.minst.bits) // Enforce := method of MInstTag
  memU.pipe.req.bits.tag := issuerU.io.deq.bits.tag
  memU.mem_io <> mem_io.data
  memU.mmu_io <> mmu_io.data

  // - Exceptions -
  val memException = WireInit(
    ldstU.io.minst.valid &&
      ldstU.io.minst.bits.exceptions.valid
  )
  val unalignedExcpData = WireInit(
    ldstU.io.minst.bits.exceptions.bits.unalignedExcp
  )

  val branchException = WireInit(
    brancherU.io.binst.valid &&
      brancherU.io.binst.bits.unalignedExcp
  )

  // CommitReg
  val commitNext = WireInit(CommitInst(params.thidN))
  when(brancherU.io.pcrel.valid) {
    commitNext.rd(0).valid := true.B
    commitNext.rd(0).bits := brancherU.io.pcrel.bits.rd
    commitNext.res(0) := brancherU.io.pcrel.bits.res
  }.elsewhen(executerU.io.einst.bits.rd.valid) {
    commitNext.rd(0).valid := true.B
    commitNext.rd(0).bits := executerU.io.einst.bits.rd.bits
    commitNext.res(0) := executerU.io.einst.bits.res
  }

  commitNext.nzcv.valid := executerU.io.einst.bits.nzcv.valid && executerU.io.einst.valid
  commitNext.nzcv.bits := executerU.io.einst.bits.nzcv.bits

  commitNext.br_taken.valid := brancherU.io.binst.valid
  commitNext.br_taken.bits := brancherU.io.binst.bits.pc

  commitNext.exceptions.valid := memException || branchException
  commitNext.exceptions.bits := Cat(
    brancherU.io.binst.bits.unalignedExcp.asUInt,
    unalignedExcpData.asUInt
  )

  commitNext.undef := !issued_dinst.inst32.valid
  commitNext.inst := issued_dinst.inst32.bits
  commitNext.is32bit := issued_dinst.is32bit
  commitNext.tag := issuerU.io.deq.bits.tag

  // Memory Resp


  // ----- Control Execute Stage Handshakes -----
  // Handle response from Memory Hierarchy before Issued Inst
  // Handshakes of IssuerDeq, memInst and CommitReg
  issuerU.io.deq.ready := false.B
  memU.pipe.req.valid := false.B
  memU.pipe.resp.ready := false.B
  commitU.enq.valid := false.B

  val ldstInstruction = ldstU.io.minst.valid && !ldstU.io.minst.bits.exceptions.valid
  when(memU.pipe.resp.valid) {
    // When memory response arrives, don't take from issue but from response
    commitU.enq <> memU.pipe.resp
  }.otherwise {
    commitU.enq.handshake(issuerU.io.deq, !ldstInstruction)
    commitU.enq.bits := commitNext
  }

  when(ldstInstruction) {
    issuerU.io.deq.handshake(memU.pipe.req)
  }


  // ------ Commit Stage ------
  // Commit State
  archstate.commit <> commitU.commit.archstate
  transplantIO.done.tag := commitU.commit.commited.tag
  when(transplantIO.stopCPU(commitU.commit.commited.tag).asBool) {
    transplantIO.done.valid := commitU.commit.commited.valid
  }.otherwise {
    transplantIO.done.valid := commitU.commit.transplant.valid || (commitU.commit.archstate.fire && commitU.commit.archstate.icountLastInst)
  }
  // Instrumentation Interface -----------------------------------------------
  val instrument = IO(new Bundle {
    val commit = commitU.deq.cloneType
  })
  instrument.commit <> commitU.deq

  // ---------- Asserts
  val nonRunnning_fetch  = WireInit(((~transplantIO.status.runningThreads) & (fetchU.instQ_o.valid.asUInt << fetchU.instQ_o.bits.tag)) =/= 0.U)
  val nonRunnning_decode = WireInit(((~transplantIO.status.runningThreads) & (decReg.io.deq.valid.asUInt << decReg.io.deq.bits.tag)) =/= 0.U)
  val nonRunnning_issue  = WireInit(((~transplantIO.status.runningThreads) & (issuerU.io.deq.valid.asUInt << issuerU.io.deq.bits.tag)) =/= 0.U)
  val nonRunnning_memory = WireInit(((~transplantIO.status.runningThreads) & (memU.pipe.resp.valid.asUInt << memU.pipe.resp.bits.tag)) =/= 0.U)
  val nonRunnning_commit = WireInit(((~transplantIO.status.runningThreads) & (commitU.commit.commited.valid.asUInt << commitU.commit.commited.tag)) =/= 0.U)
  val nonRunningFault = WireInit(nonRunnning_fetch || nonRunnning_decode || nonRunnning_issue || nonRunnning_memory || nonRunnning_commit)
  val fetchAndTransplant = WireInit((fetchU.ctrl_i.commit.valid && transplantIO.done.valid) && (fetchU.ctrl_i.commit.tag === transplantIO.done.tag))
  val parallelInsts = WireInit((issuerU.io.deq.valid && commitU.commit.commited.valid) && (issuerU.io.deq.bits.tag === commitU.commit.commited.tag))

  val asserts = IO(Output(new Bundle {
    val nonRunningFault = Bool()
    val fetchAndTransplant = Bool()
    val nonRunnning_fetch  = Bool()
    val nonRunnning_decode = Bool()
    val nonRunnning_issue  = Bool()
    val nonRunnning_memory = Bool()
    val nonRunnning_commit = Bool()
  }))

  asserts.nonRunnning_fetch  := nonRunnning_fetch 
  asserts.nonRunnning_decode := nonRunnning_decode
  asserts.nonRunnning_issue  := nonRunnning_issue 
  asserts.nonRunnning_memory := nonRunnning_memory
  asserts.nonRunnning_commit := nonRunnning_commit
  asserts.nonRunningFault := nonRunningFault
  asserts.fetchAndTransplant := fetchAndTransplant
 
  // DEBUG Signals ------------------------------------------------------------
  val dbg = IO(Output(new Bundle {
    val issue = new Bundle {
      val valid = Bool()
      val thid  = params.thidT
      val mem = Bool()
      val transplant = Bool()
    }

  }))
  dbg.issue.valid := issuerU.io.deq.fire
  dbg.issue.thid := issuerU.io.deq.bits.tag
  dbg.issue.mem := ldstU.io.minst.valid
  dbg.issue.transplant := commitU.enq.bits.exceptions.valid || commitU.enq.bits.undef


  if(false) { // TODO Conditional Assertions
    assert(!nonRunningFault, "Did an operation while not running anymore")
    assert(!fetchAndTransplant, "Started an instruction while transplant done")
    assert(!parallelInsts, "Instructions from the same context can't be present in two stages concurrently")
    assert(!(executerU.io.einst.valid && brancherU.io.pcrel.valid), "Can't have both instruction valid at the same time")
    // Assertions:  Issuer Deq | MemUnit | CommitReg Enq
    when(issuerU.io.deq.fire) {
      assert(commitU.enq.fire || memU.pipe.req.fire, "In case of Issue fire, either the MemUnit or the commit queue must receive the transaction")
      assert(!((commitU.enq.fire && !memU.pipe.resp.fire) && memU.pipe.req.fire), "Both the MemUnit and commit queue can't receive the transaction at the same time")
    }
    when(commitU.enq.fire) {
      assert(memU.pipe.resp.valid || (issuerU.io.deq.fire && !memU.pipe.req.fire), "When the commit queue receives a transaction, it must either come from the MemUnit or Issue stage")
      assert(!(memU.pipe.resp.valid && (issuerU.io.deq.fire && !memU.pipe.req.fire)), "When the commit queue receives a transaction, it can't come from both the MemUnit or Issue stage")
    }
  }
}

