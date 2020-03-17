// See LICENSE.txt for license details.
package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._
import arm.DECODE_CONTROL_SIGNALS._

import util._

class ProcConfig(val NB_THREADS : Int = 2, val DebugSignals : Boolean = false, EntriesTLB: Int = 128) {

  // Threads
  val NB_THREAD_W = log2Ceil(NB_THREADS) // 4 Threads
  def TAG_T = UInt(NB_THREAD_W.W)
  val TAG_X = 0.U(NB_THREAD_W.W)
  val TAG_VEC_X = 0.U(NB_THREADS.W)
  def TAG_VEC_T = UInt(NB_THREADS.W)

  // Memory
  val TLB_NB_ENTRY = EntriesTLB
  val TLB_NB_ENTRY_W = log2Ceil(TLB_NB_ENTRY)

  val bramConfigState = new BRAMConfig(8, 8, 512, "", false, false)
  val bramConfigMem = new BRAMConfig(8, 8, 1 << (9+TLB_NB_ENTRY_W), "", false, false)
}

class ProcStateDBG(implicit val cfg : ProcConfig) extends Bundle {

  val fetchReg = Output(Decoupled(new FInst))
  val decReg   = Output(Decoupled(new DInst))
  val issueReg = Output(Decoupled(new DInst))
  val commitReg = Output(Decoupled(new CommitInst))
  val commited = Output(Bool())

  val pregsVec = Output(Vec(cfg.NB_THREADS, new PStateRegs))
  val rfileVec = Output(Vec(cfg.NB_THREADS, Vec(REG_N, DATA_T)))

  val tuWorking = Output(ValidTag(cfg.TAG_T))
  val fillTLB = Input(Valid(new TLBFill)) // NOTE: Tag is the addr, not the thread
  val missTLB = Output(ValidTag(MISS_T, new TLBMiss))
}

/** Processor
  *
  */
class Proc(implicit val cfg: ProcConfig) extends MultiIOModule
{
  val io = IO(new Bundle {
    // BRAM Host Ports
    val memoryBRAM = new BRAMPort()(cfg.bramConfigMem)
    val stateBRAM = new BRAMPort()(cfg.bramConfigState)

    // AXI Host Communication
    val host2tpu = new TransplantUnitHostIO

    // Debug
    val procStateDBG = if(cfg.DebugSignals) Some(new ProcStateDBG) else None
  })

  // System modules -----------------------------------------

  // Host modules
  // BRAM Program page
  val memory = Module(new BRAM()(cfg.bramConfigMem))
  // BRAM PC State
  val state = Module(new BRAM()(cfg.bramConfigState))
  // Transplant Unit
  val tpu = Module(new TransplantUnit())

  // Internal State -----------------------------------------
  // PState
  val rfileVec = VecInit(Seq.fill(cfg.NB_THREADS)(Module(new RFile).io))
  val pregsVec = RegInit(VecInit(Seq.fill(cfg.NB_THREADS)(PStateRegs())))

  // Performance --------------------------------------------
  val insnTLB = Module(new TLBUnit())

  // Pipeline -----------------------------------------------
  // Fetch
  val fetch = Module(new FetchUnit())
  // fetchReg in Fetch Unit
  // Decode
  val decoder = Module(new DecodeUnit())
  val decReg = Module(new FlushReg(new DInst))
  // Issue
  val issuer = Module(new IssueUnit())
  // issueReg in Issue Unit
  // Commitement stage
  // |         |        |            |
  // | Execute |        |            |
  // |         | Branch |            |
  // |         |        | Load Store |
  // |         |        |            |
  val executer = Module(new ExecuteUnit())
  val brancher = Module(new BranchUnit())
  val ldstU = Module(new LDSTUnit())
  val commitReg = Module(new FlushReg(new CommitInst))


  // Extra Regs and wires
  val fetchEn = RegInit(VecInit(Seq.fill(cfg.NB_THREADS)(false.B)))
  val s_Commiting :: s_MemCommiting :: s_MemCommitingPair :: s_GettingPage :: s_RestartMem :: Nil = Enum(5)
  val commitingStage = RegInit(s_Commiting)
  val commitingLastStageWasMem = RegInit(false.B)

  val commitExec  = WireInit(commitReg.io.deq.bits.exe)
  val commitMem   = WireInit(commitReg.io.deq.bits.mem)
  val commitBr    = WireInit(commitReg.io.deq.bits.br)
  val commitPcRel = WireInit(commitReg.io.deq.bits.pcrel)
  val commitTag   = WireInit(commitReg.io.deq.bits.tag)

  val nextPC = Wire(DATA_T)
  val nextSP = Wire(DATA_T) // X[31] == SP
                            // Interconnect -------------------------------------------
  val memArbiterInst = Module(new MemArbiterInst)
  val memArbiterData = Module(new MemArbiterData)

  // HOST <> TP
  io.stateBRAM <> state.portA

  io.host2tpu <> tpu.io.host2tpu

  // Transplant Unit TP <> CPU
  tpu.io.stateBRAM <> state.portB
  tpu.io.tpu2cpu.done.valid := false.B
  tpu.io.tpu2cpu.done.tag := 0.U
  insnTLB.io.fillTLB := tpu.io.tpu2cpu.fillTLB

  // PState + Branching -> Fetch
  fetch.io.fire := tpu.io.tpu2cpu.fire
  fetch.io.fetchEn := fetchEn
  fetch.io.pcVec zip pregsVec foreach {case (pcFetch, pcState) => pcFetch := pcState.PC}
  fetch.io.nextPC := nextPC
  fetch.io.commitReg.bits := commitReg.io.deq.bits
  fetch.io.commitReg.valid := commitReg.io.deq.valid

  memArbiterInst.io.vaddr.bits := fetch.io.pc.bits.get
  memArbiterInst.io.vaddr.valid := fetch.io.pc.valid
  insnTLB.io.iPort <> memArbiterInst.io.tlbPort

  memArbiterInst.io.fillTLB := tpu.io.tpu2cpu.fillTLB

  when(memArbiterInst.io.reqMiss.valid) {
    tpu.io.tpu2cpu.missTLB.valid := true.B
    tpu.io.tpu2cpu.missTLB.tag := INST_FETCH.U
    tpu.io.tpu2cpu.missTLB.bits.get := memArbiterInst.io.reqMiss.bits
  }.elsewhen(memArbiterData.io.reqMiss.valid){
    tpu.io.tpu2cpu.missTLB.valid := true.B
    tpu.io.tpu2cpu.missTLB.tag := memArbiterData.io.reqMiss.tag
    tpu.io.tpu2cpu.missTLB.bits.get := memArbiterData.io.reqMiss.bits.get
  }.otherwise {
    tpu.io.tpu2cpu.missTLB.valid := false.B
    tpu.io.tpu2cpu.missTLB.tag := DontCare
    tpu.io.tpu2cpu.missTLB.bits.get := DontCare
  }

  memory.portB <> memArbiterInst.io.memPort
  when(memArbiterInst.io.selMem) {
    memory.portB <> memArbiterInst.io.memPort
  }.elsewhen(memArbiterInst.io.selHost) {
    memory.portB <> io.memoryBRAM
  }

  val sel32bit = RegNext(fetch.io.pc.bits.get(2))

  fetch.io.hit := !memArbiterInst.io.tlbPort.miss.valid
  fetch.io.insn := Mux(sel32bit, memory.portB.DO(63,32), memory.portB.DO(31,0))


  // Fetch -> Decode
  decoder.io.finst := fetch.io.deq.bits
  decReg.io.enq.bits := decoder.io.dinst

  fetch.io.deq.ready := decReg.io.enq.ready
  decReg.io.enq.valid := fetch.io.deq.valid

  // Decode -> Issue
  issuer.io.enq <> decReg.io.deq

  // Execute : Issue -> Execute
  val issued_dinst = issuer.io.deq.bits
  val issued_tag = issued_dinst.tag
  issuer.io.deq.ready := commitReg.io.enq.ready
  issuer.io.commitReg.bits := commitReg.io.deq.bits
  issuer.io.commitReg.valid := commitReg.io.deq.valid

  /** Execute */
  // connect rfile read(address) interface
  rfileVec map { case rfile =>
    rfile.rs1_addr := Mux(memArbiterData.io.rfileRd, memArbiterData.io.rfile.rs1_addr, issued_dinst.rs1)
    rfile.rs2_addr := Mux(memArbiterData.io.rfileRd, memArbiterData.io.rfile.rs2_addr, issued_dinst.rs2)
  }
  // Read register data from rfile
  val rVal1 = rfileVec(issued_dinst.tag).rs1_data
  val rVal2 = rfileVec(issued_dinst.tag).rs2_data

  // connect executeUnit interface
  executer.io.dinst := issued_dinst
  executer.io.rVal1 := rVal1
  executer.io.rVal2 := rVal2
  executer.io.nzcv := pregsVec(issued_tag).NZCV

  // connect BranchUnit interface
  brancher.io.dinst := issued_dinst
  brancher.io.rVal1 := rVal1
  brancher.io.rVal2 := rVal2
  brancher.io.cond := executer.io.condRes
  brancher.io.pc := pregsVec(issued_tag).PC

  // connect LDSTUnit interface
  ldstU.io.dinst := issued_dinst
  ldstU.io.rVal1 := rVal1
  ldstU.io.rVal2 := rVal2
  ldstU.io.pstate := pregsVec(issued_tag)

  // CommitReg
  val commitNext = Wire(new CommitInst)
  commitNext.exe := executer.io.einst
  commitNext.br := brancher.io.binst
  commitNext.pcrel := brancher.io.pcrel
  commitNext.mem := ldstU.io.minst
  commitNext.undef := !issued_dinst.inst32.valid
  commitNext.inst32 := issued_dinst.inst32.bits
  commitNext.pc := issued_dinst.pc
  commitNext.tag := issued_tag

  commitReg.io.enq.bits := commitNext
  commitReg.io.enq.valid := issuer.io.deq.valid

  // ---- Commit STATE ----
  val commited = WireInit(commitReg.io.deq.fire())
  memArbiterData.io.commitEnq := commitReg.io.enq
  memArbiterData.io.commitDeq := commitReg.io.deq
  memArbiterData.io.fillTLB := tpu.io.tpu2cpu.fillTLB
  memArbiterData.io.rfile.rfileVec foreach(_ := DontCare)
  memArbiterData.io.rfile.rs1_data := rfileVec(commitTag).rs1_data
  memArbiterData.io.rfile.rs2_data := rfileVec(commitTag).rs2_data
  // -- Transplant cases --
  val commitUndef = WireInit(commitReg.io.deq.valid && commitReg.io.deq.bits.undef)
  // - Exceptions -
  val unalignedExcpData = WireInit(commitMem.valid && memArbiterData.io.unalignedExcp)
  val unalignedExcpSP = WireInit(commitMem.valid && commitMem.bits.unalignedExcpSP)
  val unalignedExcpBranch = WireInit(commitBr.valid && commitBr.bits.unalignedExcp)

  val exception = WireInit(commitReg.io.deq.valid && (unalignedExcpData || unalignedExcpBranch || unalignedExcpSP))

  val transplant = WireInit(commitUndef || exception)

  // connect RFile's write interface
  for(cpu <- 0 until cfg.NB_THREADS) {
    // WB Port 1
    when(commitExec.valid) {
      rfileVec(cpu).w1_addr := commitExec.bits.rd.bits
      rfileVec(cpu).w1_data := commitExec.bits.res
    }.elsewhen(commitPcRel.valid) {
      rfileVec(cpu).w1_addr := commitPcRel.bits.rd
      rfileVec(cpu).w1_data := commitPcRel.bits.res
    }.elsewhen(memArbiterData.io.rfileWr){
      // Mem Writeback
      rfileVec(cpu).w1_addr := memArbiterData.io.rfile.w1_addr
      rfileVec(cpu).w1_data := memArbiterData.io.rfile.w1_data
    }.otherwise {
      rfileVec(cpu).w1_addr := DontCare
      rfileVec(cpu).w1_data := DontCare
    }
    rfileVec(cpu).w1_en := false.B

    rfileVec(cpu).w2_addr := memArbiterData.io.rfile.w2_addr
    rfileVec(cpu).w2_data := memArbiterData.io.rfile.w2_data
    rfileVec(cpu).w2_en := false.B
  }

  nextPC := pregsVec(commitTag).PC
  nextSP := pregsVec(commitTag).SP // X[31] == SP
  when(commited && !exception && !commitUndef) {
    rfileVec(commitTag).w1_en := (commitExec.valid && commitExec.bits.rd.valid) || (commitPcRel.valid)
    rfileVec(commitTag).w2_en := false.B

    when(commitExec.valid && commitExec.bits.nzcv.valid) {
      pregsVec(commitTag).NZCV := commitExec.bits.nzcv.bits
    }

    when(commitBr.valid) {
      nextPC := commitBr.bits.pc
    }.otherwise {
      nextPC := pregsVec(commitTag).PC + 4.U
    }

    pregsVec(commitTag).PC := nextPC
  }

  when(memArbiterData.io.rfileWr && !exception) {
    rfileVec(commitTag).w1_en := memArbiterData.io.rfile.w1_en
    rfileVec(commitTag).w2_en := memArbiterData.io.rfile.w2_en
  }

  // Memory Data Port
  io.memoryBRAM.DO := memory.portA.DO
  insnTLB.io.dPort <> memArbiterData.io.tlbPort

  memory.portA <> memArbiterData.io.memPort
  when(memArbiterData.io.selMem) {
    memory.portA <> memArbiterData.io.memPort
  }.elsewhen(memArbiterData.io.selHost) {
    memory.portA <> io.memoryBRAM
  }
  commitReg.io.deq.ready := exception || !memArbiterData.io.busy

  // Start and stop ---------------
  when(insnTLB.io.iPort.miss.valid)  { fetchEn(fetch.io.pc.tag) := false.B }
  when(tpu.io.tpu2cpu.fillTLB.valid) { fetchEn(0) := true.B } // TODO Support for multithread
  when(tpu.io.tpu2cpu.freeze.valid)  { fetchEn(tpu.io.tpu2cpu.freeze.tag) := false.B }
  when(tpu.io.tpu2cpu.fire.valid)    { fetchEn(tpu.io.tpu2cpu.fire.tag) := true.B }
  when(tpu.io.tpu2cpu.flush.valid)   { fetchEn(tpu.io.tpu2cpu.flush.tag) := false.B }

  // Hit unknown case -> Pass state to CPU
  tpu.io.tpu2cpu.done.valid := transplant
  tpu.io.tpu2cpu.done.tag := commitReg.io.deq.bits.tag

  // Flushing ----------------------------------------------------------------
  issuer.io.flush := tpu.io.tpu2cpu.flush
  fetch.io.flush := tpu.io.tpu2cpu.flush
  decReg.io.flush := false.B
  commitReg.io.flush := false.B
  when(tpu.io.tpu2cpu.flush.valid) {
    issuer.io.flush := tpu.io.tpu2cpu.flush
    fetch.io.flush := tpu.io.tpu2cpu.flush
    decReg.io.flush := decReg.io.deq.bits.tag === tpu.io.tpu2cpu.flush.tag
    commitReg.io.flush := commitReg.io.deq.bits.tag === tpu.io.tpu2cpu.flush.tag
  }.elsewhen(commited && commitBr.valid ) { // Branching, clean pipeline
    fetch.io.flush.valid := fetch.io.deq.bits.tag === commitTag
    fetch.io.flush.tag := commitTag
    issuer.io.flush.valid := true.B
    issuer.io.flush.tag := commitTag
    decReg.io.flush := decReg.io.deq.bits.tag === commitTag
  }

  // Transplant Activated ----------------------------------------------------
  // Default Inputs
  tpu.io.cpu2tpuState := pregsVec(tpu.io.tpu2cpu.freeze.tag)
  tpu.io.rfile.rs1_data := DontCare
  tpu.io.rfile.rs2_data := DontCare
  // When working
  when(tpu.io.tpu2cpu.freeze.valid) {
    // Redirect freezed cpu to tpu
    rfileVec(tpu.io.tpu2cpu.freeze.tag) <> tpu.io.rfile
    // Freeze PSTATE, When not written by TPU
    pregsVec(tpu.io.tpu2cpu.freeze.tag) := pregsVec(tpu.io.tpu2cpu.freeze.tag)
    when(tpu.io.tpu2cpuStateReg.valid) {
      when(tpu.io.tpu2cpuStateReg.bits === TPU2STATE.r_PC) {
        pregsVec(tpu.io.tpu2cpu.freeze.tag).PC := tpu.io.tpu2cpuState.PC
      }.elsewhen(tpu.io.tpu2cpuStateReg.bits === TPU2STATE.r_SP) {
        pregsVec(tpu.io.tpu2cpu.freeze.tag).SP := tpu.io.tpu2cpuState.SP
      }.elsewhen(tpu.io.tpu2cpuStateReg.bits === TPU2STATE.r_NZCV){
        pregsVec(tpu.io.tpu2cpu.freeze.tag).NZCV := tpu.io.tpu2cpuState.NZCV
      }
    }
  }

  // DEBUG Signals ------------------------------------------------------------
  if(cfg.DebugSignals) {
    val procStateDBG = io.procStateDBG.get
    procStateDBG.fetchReg.ready := fetch.io.deq.ready
    procStateDBG.fetchReg.valid := fetch.io.deq.valid
    procStateDBG.fetchReg.bits  := fetch.io.deq.bits
    procStateDBG.decReg.ready := decReg.io.deq.ready
    procStateDBG.decReg.valid := decReg.io.deq.valid
    procStateDBG.decReg.bits  := decReg.io.deq.bits
    procStateDBG.issueReg.ready := issuer.io.deq.ready
    procStateDBG.issueReg.valid := issuer.io.deq.valid
    procStateDBG.issueReg.bits  := issuer.io.deq.bits
    procStateDBG.commitReg.ready := commitReg.io.deq.ready
    procStateDBG.commitReg.valid := commitReg.io.deq.valid
    procStateDBG.commitReg.bits  := commitReg.io.deq.bits
    procStateDBG.commited := commited

    // Processor State (XREGS + PSTATE)
    val rfileVecWire = Wire(Vec(cfg.NB_THREADS, Vec(REG_N, DATA_T)))
    for(cpu <- 0 until cfg.NB_THREADS) {
      rfileVecWire(cpu) := rfileVec(cpu).rfileVec.get
    }
    tpu.io.rfile.rfileVec.get := rfileVecWire(0) // Give a default assignement
    procStateDBG.rfileVec := rfileVecWire
    procStateDBG.pregsVec := pregsVec
    procStateDBG.tuWorking := tpu.io.tpu2cpu.freeze

    procStateDBG.missTLB := DontCare // In ProcAxiWrap
  }

}

class CommitInst(implicit val cfg : ProcConfig) extends Bundle {
  val exe = Valid(new EInst)
  val br = Valid(new BInst)
  val pcrel = Valid(new PCRel)
  val mem = Valid(new MInst)
  val undef = Output(Bool())
  val pc = Output(DATA_T)
  val inst32 = Output(INST_T)
  val tag = Output(cfg.TAG_T)
}

class ValidTag[T1 <: Data, T2 <: Data](genTag: T1, genData: Option[T2]) extends Bundle
{
  val valid = Bool()
  val bits = genData
  val tag = genTag
  override def cloneType: this.type = ValidTag(genTag, genData).asInstanceOf[this.type]
}

object ValidTag {
  def apply[T1 <: Data, T2 <: Data](genTag: T1, genData : Option[T2]): ValidTag[T1,T2] = new ValidTag(genTag, genData)
  def apply[T1 <: Data, T2 <: Data](genTag: T1, genData : T2): ValidTag[T1,T2] = new ValidTag(genTag, Some(genData))
  def apply[T <: Data](genTag: T): ValidTag[T,T] = new ValidTag(genTag, None)
}

class DecoupledTag[T1 <: Data, T2 <: Data](genTag: T1, genData: T2) extends Bundle
{
  val ready = Input(Bool())
  val valid = Output(Bool())
  val bits = Output(genData)
  val tag = Output(genTag)
  override def cloneType: this.type = DecoupledTag(genTag, genData).asInstanceOf[this.type]
}

object DecoupledTag {
  def apply[T1 <: Data, T2 <: Data](genTag: T1, genData : T2): DecoupledTag[T1,T2] = new DecoupledTag(genTag, genData)
}
