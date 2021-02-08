package armflex.demander

import chisel3._
import chisel3.util._

import software_bundle._

class QEMUEvictReplyHandler extends MultiIOModule {
  val req_i = IO(Flipped(Decoupled(new QEMUEvictReply)))
  val free_o = IO(Decoupled(UInt()))

  free_o.bits := req_i.bits.old_ppn
  free_o.valid := req_i.valid && !req_i.bits.synonym_v
  req_i.ready := Mux(
    req_i.bits.synonym_v,
    true.B,
    free_o.ready
  )
}
