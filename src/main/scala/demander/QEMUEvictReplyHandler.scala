package armflex.demander

import armflex.QEMUEvictReply
import chisel3._
import chisel3.util._
import armflex.cache.TLBParameter

class QEMUEvictReplyHandler(param: TLBParameter) extends MultiIOModule {
  val req_i = IO(Flipped(Decoupled(new QEMUEvictReply(param))))
  val free_o = IO(Decoupled(UInt()))

  free_o.bits := req_i.bits.old_ppn
  free_o.valid := req_i.valid && !req_i.bits.synonym_v
  req_i.ready := Mux(
    req_i.bits.synonym_v,
    true.B,
    free_o.ready
  )
}

object QEMUEvictReplyHandlerVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new QEMUEvictReplyHandler(new TLBParameter)))
}