package armflex

import chisel3._
import chisel3.util._
import arm.PROCESSOR_TYPES._

import armflex.util._
import armflex.cache._
import armflex.util.DecoupledTools._

class FetchUnit(implicit val cfg: ProcConfig) extends MultiIOModule {
  val ctrl = IO(new Bundle {
    val start = Input(ValidTag(cfg.TAG_T, DATA_T))
    val commit = Input(ValidTag(cfg.TAG_T, DATA_T))
    val memWake = Vec(4, ValidTag(cfg.TAG_T))
  })

  val mem = IO(DecoupledTag(cfg.TAG_T, DATA_T))

  val pc = RegInit(VecInit(Seq.fill(cfg.NB_THREADS)(DATA_X)))
  val en = RegInit(VecInit(Seq.fill(cfg.NB_THREADS)(DATA_X)))
  val rra = Module(new RoundRobinArbiter(cfg.NB_THREADS))

  rra.io.ready := en.asUInt
  mem.handshake(rra.io.next)
  mem.bits := pc(rra.io.next.bits)
  mem.tag := rra.io.next.bits

  when(rra.io.next.fire) {
    en(rra.io.next.bits) := false.B
  }

  when(ctrl.start.valid) {
    // Wakeup from transplant
    pc(ctrl.start.tag) := ctrl.start.bits.get
    en(ctrl.start.tag) := true.B
  }
  when(ctrl.commit.valid) {
    // Wakeup from commited instruction
    pc(ctrl.commit.tag) := ctrl.commit.bits.get
    en(ctrl.commit.tag) := true.B
  }
  for(device <- 0 until 4) {
    when(ctrl.memWake(device).valid) {
    // Wakeup from Memory miss
      en(ctrl.memWake(device).tag) := true.B
    }
  }
}
