
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
  val flush = Input(ValidTagged(cfg.TAG_T))
  val fire = Input(ValidTagged(cfg.TAG_T))
  val commitReg = Flipped(Valid(new CommitInst))
  val nextPC = Input(DATA_T)

  val fetchEn = Input(Vec(cfg.NB_THREADS, Bool()))
  val pcVec = Input(Vec(cfg.NB_THREADS, DATA_T))

  val pc = Output(ValidTagged(cfg.TAG_T, DATA_T))

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
  val insnReg = Reg(Valid(new FInst))
  val fetchReg = Module(new FlushReg(new FInst))

  val insnHit = WireInit(io.pc.valid && io.hit && fetchReg.io.enq.ready)
  when(insnReq.valid && !io.deq.ready) {
    insnHit := false.B
  }
  val currPC = WireInit(prefetchPC(0))

  val isThreadReady = WireInit(io.fetchEn)
  arbiter.io.ready := isThreadReady.asUInt
  arbiter.io.next.ready := fetchReg.io.enq.ready
  when((arbiter.io.next.valid && fetchReg.io.enq.ready && !insnReg.valid) ||
         (insnReg.valid && fetchReg.io.enq.ready)) {
    currPC := prefetchPC(arbiter.io.next.bits)
    prefetchPC(arbiter.io.next.bits) := prefetchPC(arbiter.io.next.bits) + 4.U
  }

  io.pc.data.get := currPC
  io.pc.tag := arbiter.io.next.bits
  io.pc.valid := arbiter.io.next.valid

  insnReq.valid := RegNext(insnHit)
  insnReq.bits.inst := io.insn
  insnReq.bits.tag := RegNext(arbiter.io.next.bits)
  insnReq.bits.pc := RegNext(currPC)

  when(!fetchReg.io.enq.ready) {
    insnReg := insnReq
  }.elsewhen(fetchReg.io.enq.ready) {
    insnReg.valid := false.B
  }

  fetchReg.io.enq.bits  := Mux(insnReg.valid, insnReg.bits, insnReq.bits)
  fetchReg.io.enq.valid := Mux(insnReg.valid, insnReg.valid, insnReq.valid)

  when(io.fire.valid) {
    prefetchPC(io.fire.tag) := io.pcVec(io.fire.tag)
  }
  when(io.commitReg.valid && io.commitReg.bits.br.valid) {
    prefetchPC(io.commitReg.bits.tag) := io.nextPC
  }.elsewhen(io.flush.valid) {
    prefetchPC(io.flush.tag) := io.pcVec(io.flush.tag)
  }

  fetchReg.io.flush := false.B
  when(io.flush.valid) {
    isThreadReady(io.flush.tag) := false.B
    when(Mux(insnReg.valid, insnReg.bits.tag === io.flush.tag, insnReq.bits.tag === io.flush.tag)) {
      fetchReg.io.enq.valid := false.B
    }
    fetchReg.io.flush := fetchReg.io.deq.bits.tag === io.flush.tag
  }
 

  io.deq <> fetchReg.io.deq
}
