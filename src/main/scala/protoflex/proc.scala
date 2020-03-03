// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util.{Decoupled, Queue, RegEnable, Valid, log2Ceil, MuxLookup}

import common.PROCESSOR_TYPES._
import common._

import common.DECODE_CONTROL_SIGNALS._

case class ProcConfig(val NB_THREADS : Int = 2, val DebugSignals : Boolean = false, EntriesTLB: Int = 4) {

  // Threads
  val NB_THREAD_W = log2Ceil(NB_THREADS) // 4 Threads
  def TAG_T = UInt(NB_THREAD_W.W)
  val TAG_X = 0.U(NB_THREAD_W.W)
  val TAG_VEC_X = 0.U(NB_THREADS.W)
  def TAG_VEC_T = UInt(NB_THREADS.W)

  // Memory
  val TLB_NB_ENTRY = EntriesTLB
  val TLB_NB_ENTRY_W = log2Ceil(TLB_NB_ENTRY)

  val bramConfigState = new BRAMConfig(10, 36, isAXI = false, isRegistered = true)
  val bramConfigMem = new BRAMConfig(10+TLB_NB_ENTRY_W, 36*2, isAXI = false, isRegistered = true)
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
  val memResp = Input(Vec(2, DATA_T))
  val fillTLB = Input(ValidTagged(DATA_T, new TLBEntry))
  val missTLB = Output(ValidTagged(cfg.TAG_T, DATA_T))
}

/** Processor
  *
  */
class Proc(implicit val cfg: ProcConfig) extends MultiIOModule
{
  val io = IO(new Bundle {
    // BRAM Host Ports
    val ppageBRAM = new BRAMPort()(cfg.bramConfigMem)
    val stateBRAM = new BRAMPort()(cfg.bramConfigState)

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
  val ppage = Module(new BRAM()(cfg.bramConfigMem))
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
  io.stateBRAM <> state.portA

  io.host2tpu <> tpu.io.host2tpu

  // Transplant Unit TP <> CPU
  tpu.io.stateBRAM <> state.portB
  tpu.io.tpu2cpu.done.valid := false.B
  tpu.io.tpu2cpu.done.tag := 0.U
  insnTLB.io.fillTLB := tpu.io.tpu2cpu.fillTLB
  insnTLB.io.fillTLB := tpu.io.tpu2cpu.fillTLB

  // PState + Branching -> Fetch
  tpu.io.tpu2cpu.missTLB.tag := fetch.io.pc.tag
  tpu.io.tpu2cpu.missTLB.valid := insnTLB.io.iPort.miss.valid
  tpu.io.tpu2cpu.missTLB.data.get := insnTLB.io.iPort.miss.bits

  fetch.io.fire := tpu.io.tpu2cpu.fire
  fetch.io.fetchEn := fetchEn
  fetch.io.pcVec zip pregsVec foreach {case (pcFetch, pcState) => pcFetch := pcState.PC}
  fetch.io.nextPC := nextPC
  fetch.io.commitReg.bits := commitReg.io.deq.bits
  fetch.io.commitReg.valid := commitReg.io.deq.valid
  insnTLB.io.iPort.vaddr.bits := fetch.io.pc.data.get
  insnTLB.io.iPort.vaddr.valid := fetch.io.pc.valid
  val sel32bit = if(cfg.bramConfigMem.isRegistered) {
    RegNext(fetch.io.pc.data.get(2))
  } else {
    fetch.io.pc.data.get(2)
  }

  ppage.portB.EN := true.B
  ppage.portB.DI := 0.U
  ppage.portB.WE := false.B
  ppage.portB.ADDR := insnTLB.io.iPort.paddr // PC is byte addressed, BRAM is 32bit word addressed

  fetch.io.hit := !insnTLB.io.iPort.miss.valid
  fetch.io.insn := Mux(sel32bit, ppage.portB.DO(63,32), ppage.portB.DO(31,0))


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
    rfile.rs1_addr := issued_dinst.rs1
    rfile.rs2_addr := issued_dinst.rs2
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
  val commitUndef = WireInit(commitReg.io.deq.bits.undef)

  // Writeback : Execute -> PState
  insnTLB.io.dPort.isWr := !commitMem.bits.isLoad
  insnTLB.io.dPort.vaddr.bits := commitMem.bits.memReq(0).addr
  insnTLB.io.dPort.vaddr.valid := commitValid && commitMem.valid

  ppage.portA.EN := commitValid && commitMem.valid && !insnTLB.io.dPort.miss.valid
  ppage.portA.DI := commitMem.bits.memReq(0).data
  ppage.portA.WE := !commitMem.bits.isLoad
  ppage.portA.ADDR := insnTLB.io.dPort.paddr

  val wbLD = Wire(Vec(2, DATA_T))
  if(cfg.DebugSignals) {
    for( i <- 0 until 2 ) {
      val wbData = io.procStateDBG.get.memResp(i) // ppage.portA.DO
      when(commitMem.bits.is32bit) {
        wbLD(i) := MuxLookup(commitMem.bits.size, wbData(31,0), Array(
          SIZEB -> Mux(commitMem.bits.isSigned,
            wbData(7,  0).asSInt.pad(32).asUInt,
            wbData(7,  0).pad(32)),
          SIZEH -> Mux(commitMem.bits.isSigned,
            wbData(15, 0).asSInt.pad(32).asUInt,
            wbData(15, 0).pad(32))
        )).pad(64)
      }.otherwise {
        wbLD(i) := MuxLookup(commitMem.bits.size, wbData, Array(
          SIZEB  -> Mux(commitMem.bits.isSigned,
            wbData(7,  0).asSInt.pad(64).asUInt,
            wbData(7,  0).pad(64)),
          SIZEH  -> Mux(commitMem.bits.isSigned,
            wbData(15, 0).asSInt.pad(64).asUInt,
            wbData(15, 0).pad(64)),
          SIZE32 -> Mux(commitMem.bits.isSigned,
            wbData(31, 0).asSInt.pad(64).asUInt,
            wbData(31, 0).pad(64)),
          SIZE64 -> wbData
        ))
      }
    }
  }

  io.ppageBRAM.DO := DontCare
  when(io.ppageBRAM.EN && io.ppageBRAM.WE) {
    io.ppageBRAM <> ppage.portA
    commitReg.io.deq.ready := !commitMem.valid
  }

  // connect RFile's write interface
  for(cpu <- 0 until cfg.NB_THREADS) {
    // WB Port 1
    when(commitExec.valid) {
      rfileVec(cpu).w1_addr := commitExec.bits.rd.bits
      rfileVec(cpu).w1_data := commitExec.bits.res
    }.elsewhen(commitPcRel.valid) {
      rfileVec(cpu).w1_addr := commitPcRel.bits.rd
      rfileVec(cpu).w1_data := commitPcRel.bits.res
    }.elsewhen(commitMem.valid) {
      rfileVec(cpu).w1_addr := commitMem.bits.memReq(0).reg
      rfileVec(cpu).w1_data := wbLD(0)
    }.otherwise {
      // Default
      rfileVec(cpu).w1_addr := commitExec.bits.rd.bits
      rfileVec(cpu).w1_data := commitExec.bits.res
    }
    rfileVec(cpu).w1_en := false.B

    // WB Port 2
    when(commitMem.valid ) { // Load TODO : Real, here QEMU Loads in single cycle
      when(commitMem.bits.isPair) {
        rfileVec(cpu).w2_addr := commitMem.bits.memReq(1).reg
        rfileVec(cpu).w2_data := wbLD(1)
      }.otherwise {
        rfileVec(cpu).w2_addr := commitMem.bits.rd.bits
        rfileVec(cpu).w2_data := commitMem.bits.rd_res
      }
    }.otherwise {
      rfileVec(cpu).w2_addr := commitMem.bits.rd.bits
      rfileVec(cpu).w2_data := commitMem.bits.rd_res
    }
    rfileVec(cpu).w2_en := false.B
  }

  nextPC := pregsVec(commitTag).PC
  nextSP := pregsVec(commitTag).SP
  when(commitValid) {
    rfileVec(commitTag).w1_en :=
      (commitExec.valid && commitExec.bits.rd.valid) ||
      (commitMem.valid && commitMem.bits.isLoad) ||
      (commitPcRel.valid)
    rfileVec(commitTag).w2_en := commitMem.valid &&
      ((commitMem.bits.isLoad && commitMem.bits.isPair) || commitMem.bits.rd.valid)

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
  when(insnTLB.io.iPort.miss.valid)        { fetchEn(fetch.io.pc.tag) := false.B }
  when(tpu.io.tpu2cpu.fire.valid)    { fetchEn(tpu.io.tpu2cpu.fire.tag) := true.B }
  when(tpu.io.tpu2cpu.fillTLB.valid) { fetchEn(tpu.io.tpu2cpu.fillTLB.tag) := true.B }
  when(tpu.io.tpu2cpu.flush.valid)   { fetchEn(tpu.io.tpu2cpu.flush.tag) := false.B }

  // Hit unknown case -> Pass state to CPU
  tpu.io.tpu2cpu.done.valid := commitValid && (commitUndef || insnTLB.io.excpWrProt)
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
  }.elsewhen(commitValid && commitBr.valid ) { // Branching, clean pipeline
    fetch.io.flush.valid := fetch.io.deq.bits.tag === commitTag
    fetch.io.flush.tag := commitTag
    issuer.io.flush.valid := true.B
    issuer.io.flush.tag := commitTag
    decReg.io.flush := decReg.io.deq.bits.tag === commitTag
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
