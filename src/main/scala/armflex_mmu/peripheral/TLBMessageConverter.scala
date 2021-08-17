package armflex_mmu.peripheral

import armflex_mmu.MemoryHierarchyParams
import armflex.{TLBEvictionMessage, TLBMissRequestMessage}
import chisel3._
import chisel3.util._
import armflex_cache.{TLBMMURequestPacket, PageTableParams}

/**
 * This module converts a TLB backend request (evict or just flush) to a message.
 * The message will be sent to the core so that correct operations will be carried.
 * 
 * @params params the parameter of the TLB
 * 
 * @note Flush result will be ignored here because another module has already read the data.
 * @note To support multiple TLBs, please add an arbiter.
 * @note the fifos for the message is added in the message compositor.
 */
class TLBMessageConverter(
  param: MemoryHierarchyParams
) extends MultiIOModule {
  val tlb_backend_request_i = IO(Flipped(Decoupled(new TLBMMURequestPacket(param.getPageTableParams))))

  val miss_request = Wire(Decoupled(new TLBMissRequestMessage(param.getPageTableParams)))
  miss_request.bits.tag:= tlb_backend_request_i.bits.tag
  miss_request.bits.perm := tlb_backend_request_i.bits.perm
  miss_request.bits.thid := tlb_backend_request_i.bits.thid
  miss_request.valid := tlb_backend_request_i.valid && !tlb_backend_request_i.bits.w_v

  val ev_request = Wire(Decoupled(new TLBEvictionMessage(param.getPageTableParams)))
  ev_request.bits.tag := tlb_backend_request_i.bits.tag
  ev_request.bits.entry := tlb_backend_request_i.bits.entry
  // Why not flush: Flushed element is handled by the module TLBWrapper.
  ev_request.valid := tlb_backend_request_i.valid && tlb_backend_request_i.bits.w_v && !tlb_backend_request_i.bits.flush_v



  // val miss_request_qo = Queue(miss_request, 4)
  val miss_request_o = IO(Decoupled(new TLBMissRequestMessage(param.getPageTableParams)))
  miss_request_o <> miss_request

  // val ev_request_qo = Queue(ev_request, 4)
  val eviction_request_o = IO(Decoupled(new TLBEvictionMessage(param.getPageTableParams)))
  eviction_request_o <> ev_request

  tlb_backend_request_i.ready := Mux(
    tlb_backend_request_i.bits.w_v,
    Mux(tlb_backend_request_i.bits.flush_v, true.B, eviction_request_o.ready),
    miss_request_o.ready
  )

  if(true) { // TODO Conditional asserts
    when(ev_request.valid){
      assert(ev_request.bits.entry.perm === 1.U, "Eviction only occurs in dirty and perm-granted TLB entry.")
    }
  }
}

object TLBWrapperVerilogEmitter extends App {
  import chisel3.stage.ChiselStage
  println((new ChiselStage).emitVerilog(new TLBMessageConverter(new MemoryHierarchyParams)))
}