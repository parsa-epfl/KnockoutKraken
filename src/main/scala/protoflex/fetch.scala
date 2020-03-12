
package protoflex

import chisel3._
import chisel3.util._
import common.{FlushReg}
import common.PROCESSOR_TYPES._

class FInst(implicit val cfg: ProcConfig) extends Bundle
{
  val inst = INST_T
  val tag = cfg.TAG_T
  val pc = DATA_T
}

class FetchUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  val flush = Input(ValidTag(cfg.TAG_T))
  val fire = Input(ValidTag(cfg.TAG_T))
  val commitReg = Flipped(Valid(new CommitInst))
  val nextPC = Input(DATA_T)

  val fetchEn = Input(Vec(cfg.NB_THREADS, Bool()))
  val pcVec = Input(Vec(cfg.NB_THREADS, DATA_T))

  val pc = Output(ValidTag(cfg.TAG_T, DATA_T))

  val hit = Input(Bool())
  val insn = Input(INST_T)
  val deq = Decoupled(new FInst)
}

class FetchUnit(implicit val cfg: ProcConfig) extends Module
{
  val io = IO(new FetchUnitIO())

  val prefetchPC = RegInit(VecInit(Seq.fill(cfg.NB_THREADS)(DATA_X)))
  val arbiter = Module(new RRArbiter(cfg.NB_THREADS))

  val insnReq = Wire(Valid(new FInst))
  val fetchReg = Module(new FlushReg(new FInst))

  val insnHit = WireInit(arbiter.io.next.valid && io.hit)

  val readyThreads = WireInit(io.fetchEn)
  arbiter.io.ready := readyThreads.asUInt
  arbiter.io.next.ready := fetchReg.io.enq.ready
  val currFetchPC = prefetchPC(arbiter.io.next.bits)

  when(insnHit && fetchReg.io.enq.ready) {
    currFetchPC := currFetchPC + 4.U
  }

  io.pc.bits.get := currFetchPC
  io.pc.tag := arbiter.io.next.bits
  io.pc.valid := arbiter.io.next.valid

  val insnReg = Reg(Valid(new FInst))
  insnReq.valid := RegNext(insnHit)
  insnReq.bits.inst := io.insn
  insnReq.bits.tag := RegNext(arbiter.io.next.bits)
  insnReq.bits.pc := RegNext(currFetchPC)

  when(!fetchReg.io.enq.ready && !insnReg.valid) {
    insnReg := insnReq
  }.elsewhen(fetchReg.io.enq.ready) {
    insnReg.valid := false.B
  }

  fetchReg.io.enq.bits  := Mux(insnReg.valid, insnReg.bits, insnReq.bits)
  fetchReg.io.enq.valid := Mux(insnReg.valid, insnReg.valid, insnReq.valid)

  when(io.commitReg.valid && io.commitReg.bits.br.valid) {
    prefetchPC(io.commitReg.bits.tag) := io.nextPC
  }.elsewhen(io.flush.valid) { // Note, Branching also flushes, so prioritize branching
    prefetchPC(io.flush.tag) := io.pcVec(io.flush.tag)
  }
  when(io.fire.valid) {
    prefetchPC(io.fire.tag) := io.pcVec(io.fire.tag)
  }

  io.deq <> fetchReg.io.deq

  // Flush
  fetchReg.io.flush := false.B
  when(io.flush.valid) {
    when(arbiter.io.next.bits === io.flush.tag) { insnHit := false.B }
    readyThreads(io.flush.tag) := false.B
    val flushInternalReg = if(cfg.bramConfigMem.isRegistered) {
      Mux(insnReg.valid, insnReg.bits.tag === io.flush.tag, insnReq.bits.tag === io.flush.tag)
    } else {
      insnReq.bits.tag === io.flush.tag
    }
    when(flushInternalReg) { fetchReg.io.enq.valid := false.B }
    fetchReg.io.flush := fetchReg.io.deq.bits.tag === io.flush.tag
  }
}
