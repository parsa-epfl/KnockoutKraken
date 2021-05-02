package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import armflex.demander.software_bundle

import armflex.cache.{
  MemorySystemParameter,
  TLBBackendReplyPacket,
  TLBAccessRequestPacket,
  TLBBackendRequestPacket,
  TLBTagPacket,
  TLBParameter,
  TLBFrontendReplyPacket,
  TLBEntryPacket
}
/**
 * This module converts a TLB backend request (evict or just flush) to a message.
 * The message will be sent to the core so that correct operations will be carried.
 * 
 * @param param the parameter of the TLB
 * 
 * @note Flush result will be ignored here because another module has already read the data.
 * @note To support multiple TLBs, please add an arbiter.
 * @note the fifos for the message is added in the message compositor.
 */
class TLBMessageConverter(
  param: TLBParameter
) extends MultiIOModule {
  val tlb_backend_request_i = IO(Flipped(Decoupled(new TLBBackendRequestPacket(param))))

  val miss_request = Wire(Decoupled(new software_bundle.TLBMissRequestMessage(param)))
  miss_request.bits.tag:= tlb_backend_request_i.bits.tag
  miss_request.bits.permission := tlb_backend_request_i.bits.permission
  miss_request.valid := tlb_backend_request_i.valid && !tlb_backend_request_i.bits.w_v

  val ev_request = Wire(Decoupled(new software_bundle.TLBEvictionMessage(param)))
  ev_request.bits.tag := tlb_backend_request_i.bits.tag
  ev_request.bits.entry.ppn := tlb_backend_request_i.bits.entry.pp
  ev_request.bits.entry.permission := tlb_backend_request_i.bits.entry.permission // Eviction should always evict a modified page.
  ev_request.bits.entry.modified := tlb_backend_request_i.bits.entry.modified
  // Why not flush: Flushed element is handled by the module TLBWrapper.
  ev_request.valid := tlb_backend_request_i.valid && tlb_backend_request_i.bits.w_v && !tlb_backend_request_i.bits.flush_v

  when(ev_request.valid){
    assert(ev_request.bits.entry.permission === 1.U, "Eviction only occurs in dirty and permission-granted TLB entry.")
  }

  // val miss_request_qo = Queue(miss_request, 4)
  val miss_request_o = IO(Decoupled(new software_bundle.TLBMissRequestMessage(param)))
  miss_request_o <> miss_request

  // val ev_request_qo = Queue(ev_request, 4)
  val eviction_request_o = IO(Decoupled(new software_bundle.TLBEvictionMessage(param)))
  eviction_request_o <> ev_request

  tlb_backend_request_i.ready := Mux(
    tlb_backend_request_i.bits.w_v,
    Mux(tlb_backend_request_i.bits.flush_v, true.B, eviction_request_o.ready),
    miss_request_o.ready
  )
}

object TLBWrapperVerilogEmitter extends App {
  import chisel3.stage.ChiselStage
  println((new ChiselStage).emitVerilog(new TLBMessageConverter(new TLBParameter)))
}