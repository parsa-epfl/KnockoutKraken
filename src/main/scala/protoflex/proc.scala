// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.{Decoupled, Queue, RegEnable, Valid, log2Ceil}

import common.constBRAM.TDPBRAM36ParamDict
import common.PROCESSOR_TYPES._
import common._

case class ProcConfig(val NB_THREADS : Int = 4, val DebugSignals : Boolean = false, EntriesTLB: Int = 32) {
  val ppageBRAMc = new BRAMConfig(Seq(TDPBRAM36ParamDict(36), TDPBRAM36ParamDict(36)))
  val stateBRAMc = new BRAMConfig(Seq(TDPBRAM36ParamDict(36), TDPBRAM36ParamDict(36)))

  // Threads
  val NB_THREAD_W = log2Ceil(NB_THREADS) // 4 Threads
  def TAG_T = UInt(NB_THREAD_W.W)
  val TAG_X = 0.U(NB_THREAD_W.W)
  val TAG_VEC_X = 0.U(NB_THREADS.W)
  def TAG_VEC_T = UInt(NB_THREADS.W)

  // Memory
  val TLB_NB_ENTRY = EntriesTLB
  val TLB_NB_ENTRY_W = log2Ceil(TLB_NB_ENTRY)
}

class ValidTagged[T1 <: Data, T2 <: Data](genTag: T1, genData: Option[T2]) extends Bundle
{
  val valid = Bool()
  val data = genData
  val tag = genTag
  override def cloneType: this.type = ValidTagged(genTag, genData).asInstanceOf[this.type]
}

object ValidTagged {
  def apply[T1 <: Data, T2 <: Data](genTag: T1, genData : Option[T2]): ValidTagged[T1,T2] = new ValidTagged(genTag, genData)
  def apply[T1 <: Data, T2 <: Data](genTag: T1, genData : T2): ValidTagged[T1,T2] = new ValidTagged(genTag, Some(genData))
  def apply[T <: Data](genTag: T): ValidTagged[T,T] = new ValidTagged(genTag, None)
}

class ProcStateDBG(implicit val cfg : ProcConfig) extends Bundle
{
  val fetchReg = Output(Decoupled(new FInst))
  val decReg   = Output(Decoupled(new DInst))
  val issueReg = Output(Decoupled(new DInst))
  val commitReg = Output(Decoupled(new CommitInst))

  val pregsVec = Output(Vec(cfg.NB_THREADS, new PStateRegs))
  val rfileVec = Output(Vec(cfg.NB_THREADS, Vec(REG_N, DATA_T)))

  val tuWorking = Output(ValidTagged(cfg.TAG_T))
  val memResp = Input(DATA_T)
}

/** Processor
  *
  */
class Proc(implicit val cfg: ProcConfig) extends MultiIOModule
{
  val io = IO(new Bundle {
    // BRAM Host Ports
    val ppageBRAM = new BRAMPortAXI(0)(cfg.ppageBRAMc)
    val stateBRAM = new BRAMPortAXI(0)(cfg.stateBRAMc)

    // AXI Host Communication
    val host2tpu = new TransplantUnitHostIO

    // Memory Interface
    // TODO: For the moment the simulator fakes
    //       response in single cycle by requesting to QEMU
    // val memReq = Output(Valid(new MemReq))
    // val memResp = Input(Valid(DATA_T))

    // Debug
    val procStateDBG = if(cfg.DebugSignals) Some(new ProcStateDBG) else None
  })

  // System modules -----------------------------------------

  // Host modules
  // BRAM Program page
  val ppage = Module(new BRAM()(cfg.ppageBRAMc))
  // BRAM PC State
  val state = Module(new BRAM()(cfg.stateBRAMc))
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

  val commitExec  = WireInit(commitReg.io.deq.bits.exe)
  val commitMem   = WireInit(commitReg.io.deq.bits.mem)
  val commitBr    = WireInit(commitReg.io.deq.bits.br)
  val commitPcRel = WireInit(commitReg.io.deq.bits.pcrel)
  val commitTag   = WireInit(commitReg.io.deq.bits.tag)
  val commitValid = WireInit(commitReg.io.deq.valid)

  val nextPC = Wire(DATA_T)
  val nextSP = Wire(DATA_T)
  // Interconnect -------------------------------------------

  // HOST <> TP
  io.ppageBRAM <> ppage.io.getPort(0)
  io.stateBRAM <> state.io.getPort(0)

  io.host2tpu <> tpu.io.host2tpu

  // Transplant Unit TP <> CPU
  tpu.io.stateBRAM <> state.io.getPort(1)
  tpu.io.tpu2cpu.done.valid := false.B
  tpu.io.tpu2cpu.done.tag := 0.U
  insnTLB.io.fillTLB.valid := tpu.io.tpu2cpu.fillTLB.valid
  insnTLB.io.fillTLB.bits := tpu.io.tpu2cpu.fillTLB.data.get

  // PState + Branching -> Fetch
  tpu.io.tpu2cpu.missTLB.tag := fetch.io.pc.tag
  tpu.io.tpu2cpu.missTLB.valid := insnTLB.io.miss.valid
  tpu.io.tpu2cpu.missTLB.data.get := insnTLB.io.miss.bits

  ppage.io.getPort(1).en := true.B
  ppage.io.getPort(1).dataIn.get := 0.U
  ppage.io.getPort(1).writeEn.get := false.B
  ppage.io.getPort(1).addr := insnTLB.io.paddr // PC is byte addressed, BRAM is 32bit word addressed

  fetch.io.fire := tpu.io.tpu2cpu.fire
  fetch.io.fetchEn := fetchEn
  fetch.io.pcVec zip pregsVec foreach {case (pcFetch, pcState) => pcFetch := pcState.PC}
  fetch.io.nextPC := nextPC
  fetch.io.commitReg.bits := commitReg.io.deq.bits
  fetch.io.commitReg.valid := commitReg.io.deq.valid
  insnTLB.io.vaddr := fetch.io.pc.data.get
  fetch.io.hit := !insnTLB.io.miss.valid
  fetch.io.insn := ppage.io.getPort(1).dataOut.get

  // Fetch -> Decode
  decoder.io.finst := fetch.io.deq.bits
  decReg.io.enq.bits := decoder.io.dinst

  fetch.io.deq.ready := decReg.io.enq.ready
  decReg.io.enq.valid := fetch.io.deq.valid

  // Decode -> Issue
  issuer.io.enq <> decReg.io.deq

  // Execute : Issue -> Execute
  val issued_dinst = WireInit(issuer.io.deq.bits)
  val issued_tag = WireInit(issued_dinst.tag)
  issuer.io.deq.ready := commitReg.io.enq.ready
  issuer.io.commitReg.bits := commitReg.io.deq.bits
  issuer.io.commitReg.valid := commitReg.io.deq.valid

  /** Execute */
  // connect rfile read(address) interface
  rfileVec map { case rfile =>
    rfile.rs1_addr := issued_dinst.rs1.bits
    rfile.rs2_addr := issued_dinst.rs2.bits
  }
  // Read register data from rfile
  val rVal1 = rfileVec(issued_dinst.tag).rs1_data
  val rVal2 = rfileVec(issued_dinst.tag).rs2_data

  // connect executeUnit interface
  executer.io.dinst := issued_dinst
  executer.io.rVal1 := rVal1
  executer.io.rVal2 := rVal2
  executer.io.nzcv := pregsVec(issued_tag).NZCV
  executer.io.SP   := pregsVec(issued_tag).SP

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
  commitReg.io.enq.bits.exe := executer.io.einst
  commitReg.io.enq.bits.br := brancher.io.binst
  commitReg.io.enq.bits.pcrel := brancher.io.pcrel
  commitReg.io.enq.bits.mem := ldstU.io.minst
  commitReg.io.enq.bits.undef := !issued_dinst.inst32.valid
  commitReg.io.enq.bits.inst32 := issued_dinst.inst32.bits
  commitReg.io.enq.bits.pc := issued_dinst.pc
  commitReg.io.enq.bits.tag := issued_tag
  commitReg.io.enq.valid := issuer.io.deq.valid
  commitReg.io.deq.ready := true.B

  // ---- Commit STATE ----
  // Writeback : Execute -> PState

  // connect RFile's write interface
 for(cpu <- 0 until cfg.NB_THREADS) {
    when(commitExec.valid) {
      rfileVec(cpu).waddr := commitExec.bits.rd.bits
      rfileVec(cpu).wdata := commitExec.bits.res
    }.elsewhen(commitPcRel.valid) {
      rfileVec(cpu).waddr := commitPcRel.bits.rd
      rfileVec(cpu).wdata := commitPcRel.bits.res
// TODO NOT SUPPORTED wback LD/ST
// If LD triggers transplant, CPU went to inconsistent state
//    }.elsewhen(commitMem.valid) { // wback 
//      rfileVec(cpu).waddr := commitMem.bits.rd.bits
//      rfileVec(cpu).wdata := commitMem.bits.rd_res
    }.elsewhen(commitMem.valid) { // Load TODO : Real, here QEMU Loads in single cycle
      rfileVec(cpu).waddr := commitMem.bits.mem.reg
      rfileVec(cpu).wdata := io.procStateDBG.get.memResp
    }.otherwise {
      // Default
      rfileVec(cpu).waddr := commitExec.bits.rd.bits
      rfileVec(cpu).wdata := commitExec.bits.res
    }
    rfileVec(cpu).wen := false.B
  }

  nextPC := pregsVec(commitTag).PC
  nextSP := pregsVec(commitTag).SP
  when(commitValid && !commitReg.io.deq.bits.undef) {
    rfileVec(commitTag).wen :=
      (commitExec.valid && commitExec.bits.rd.valid) ||
      (commitMem.valid && !commitMem.bits.mem.is_store) ||
      (commitPcRel.valid)

    when(commitExec.valid && commitExec.bits.nzcv.valid) {
      pregsVec(commitTag).NZCV := commitExec.bits.nzcv.bits
    }

    when(commitBr.valid) {
      nextPC := commitBr.bits.pc
    }.otherwise {
      nextPC := pregsVec(commitTag).PC + 4.U
    }

    when(commitMem.valid) {
      when(commitMem.bits.rd.bits === 31.U) {
        nextSP := commitMem.bits.rd_res
      }
    }

    pregsVec(commitTag).PC := nextPC
    pregsVec(commitTag).SP := nextSP
  }

  // Start and stop ---------------
  when(insnTLB.io.miss.valid)        { fetchEn(fetch.io.pc.tag) := false.B }
  when(tpu.io.tpu2cpu.fire.valid)    { fetchEn(tpu.io.tpu2cpu.fire.tag) := true.B }
  when(tpu.io.tpu2cpu.fillTLB.valid) { fetchEn(tpu.io.tpu2cpu.fillTLB.tag) := true.B }
  when((issuer.io.deq.valid && !issued_dinst.inst32.valid)) { fetchEn(tpu.io.tpu2cpu.flush.tag) := false.B }

  // Hit unknown case -> Pass state to CPU
  tpu.io.tpu2cpu.done.valid := commitValid && commitReg.io.deq.bits.undef
  tpu.io.tpu2cpu.done.tag := commitReg.io.deq.bits.tag

  // Flushing ----------------------------------------------------------------
  issuer.io.flush := tpu.io.tpu2cpu.flush
  fetch.io.flush := tpu.io.tpu2cpu.flush
  commitReg.io.flush := false.B
  when(tpu.io.tpu2cpu.flush.valid) {
    decReg.io.flush := decReg.io.deq.bits.tag === tpu.io.tpu2cpu.flush.tag
    commitReg.io.flush := commitReg.io.deq.bits.tag === tpu.io.tpu2cpu.flush.tag
  }.elsewhen(commitValid && commitBr.valid ) { // Branching, clean pipeline
    fetch.io.flush.valid := fetch.io.deq.bits.tag === commitTag
    fetch.io.flush.tag := commitTag
    issuer.io.flush.valid := true.B
    issuer.io.flush.tag := commitTag
    decReg.io.flush := decReg.io.deq.bits.tag === commitTag
  }.otherwise {
    issuer.io.flush.valid := false.B
    decReg.io.flush := false.B
  }

  // Transplant Activated ----------------------------------------------------
  // Default Inputs
  tpu.io.cpu2tpuState := pregsVec(tpu.io.tpu2cpu.freeze.tag)
  tpu.io.rfile.rs1_data := rfileVec(0).rs1_data
  tpu.io.rfile.rs2_data := rfileVec(0).rs2_data
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
    procStateDBG.fetchReg.ready := decReg.io.enq.ready
    procStateDBG.fetchReg.valid := fetch.io.deq.valid
    procStateDBG.fetchReg.bits  := fetch.io.deq.bits
    procStateDBG.decReg.ready := issuer.io.enq.ready
    procStateDBG.decReg.valid := decReg.io.deq.valid
    procStateDBG.decReg.bits  := decReg.io.deq.bits
    procStateDBG.issueReg.ready := commitReg.io.enq.ready
    procStateDBG.issueReg.valid := issuer.io.deq.valid
    procStateDBG.issueReg.bits  := issuer.io.deq.bits
    procStateDBG.commitReg.ready := true.B
    procStateDBG.commitReg.valid := commitReg.io.deq.valid
    procStateDBG.commitReg.bits  := commitReg.io.deq.bits

    // Processor State (XREGS + PSTATE)
    val rfileVecWire = Wire(Vec(cfg.NB_THREADS, Vec(REG_N, DATA_T)))
    for(cpu <- 0 until cfg.NB_THREADS) {
      rfileVecWire(cpu) := rfileVec(cpu).rfileVec.get
    }
    tpu.io.rfile.rfileVec.get := rfileVecWire(0) // Give a default assignement
    procStateDBG.rfileVec := rfileVecWire
    procStateDBG.pregsVec := pregsVec
    procStateDBG.tuWorking := tpu.io.tpu2cpu.freeze
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

