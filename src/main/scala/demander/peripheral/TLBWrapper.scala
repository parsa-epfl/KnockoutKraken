package armflex.demander.peripheral

import chisel3._
import chisel3.util._

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
 * This module provides an interface for software to interact with TLBs.
 * 
 * - responseToTLB
 * - flushTLBEntry
 * 
 * @param param the parameter of the TLB.
 */ 
class TLBWrapper(
  processIDWidth: Int,
  param: TLBParameter,
) extends MultiIOModule {
  // The backend port of TLB.
  val tlb_backend_reply_o = IO(Decoupled(new TLBBackendReplyPacket(param)))
  val tlb_backend_reply_q = Wire(Decoupled(new TLBBackendReplyPacket(param)))
  tlb_backend_reply_o <> Queue(tlb_backend_reply_q, param.threadNumber)
  // TODO: add FIFO to the reply and request port. FIFO is also necessary for the flush request.
  // TLB flush request.
  val tlb_flush_request_o = IO(Decoupled(new TLBTagPacket(param)))
  // TLB reply, which stores the flush result.
  val tlb_frontend_reply_i = IO(Flipped(Valid(new TLBFrontendReplyPacket(param))))

  val process_id_r = Reg(UInt(processIDWidth.W))
  val tag_r = Reg(new TLBTagPacket(param))
  val pte_r = Reg(new TLBEntryPacket(param))

  val sIdle :: sFlush :: sReply :: Nil = Enum(3)
  val state_r = RegInit(sIdle)

  // TODO: Determine the CSR of this module
  /**
   * Address   |  CSR
   * 0x0 - 0x1 |  vpn
   * 0x2       |  process_id
   * 0x3       |  ppn
   * 0x4       |  permission
   * 0x5       |  modified
   * # For flushTLBEntry
   * 0x6       |  W: request flush R: Flush is done
   * 0x7       |  W: Nothing R: whether the flush is hit.
   * # For responseToTLB
   * 0x8       |  W: Response R: reply queue is not full
   */ 
  val request_i = IO(Flipped(Valid(new MemoryRequestPacket(32, 32))))
  val internal_address = request_i.bits.addr(5,2)
  val reply_r = RegInit(UInt(32.W), 0.U)
  val flush_hit_vr = RegInit(Bool(), false.B)
  switch(internal_address){
    is(0x0.U){
      reply_r := tag_r.vpage(31, 0)
    }
    is(0x1.U){
      reply_r := tag_r.vpage(tag_r.vpage.getWidth-1, 32)
    }
    is(0x2.U){
      reply_r := process_id_r
    }
    is(0x3.U){
      reply_r := pte_r.pp
    }
    is(0x4.U){
      reply_r := pte_r.permission
    }
    is(0x5.U){
      reply_r := pte_r.modified
    }
    is(0x6.U){
      reply_r := state_r === sIdle
    }
    is(0x7.U){
      reply_r := flush_hit_vr
    }
    is(0x8.U){
      reply_r := tlb_backend_reply_q.ready
    }
  }
  val reply_o = IO(Output(UInt(32.W)))
  reply_o := reply_r

  val write_v = request_i.valid && request_i.bits.w_v

  // Update of state_r
  switch(state_r){
    is(sIdle){
      when(write_v && internal_address === 0x6.U){
        state_r := sFlush
      }.elsewhen(write_v && internal_address === 0x7.U){
        state_r := sReply
      }
    }
    is(sFlush){
      when(tlb_flush_request_o.fire()){
        state_r := sIdle
      }
    }
    is(sReply){
      when(tlb_backend_reply_q.fire()){
        state_r := sIdle
      }
    }
  }

  // Update tag_r and process_id_r
  when(write_v && internal_address === 0x0.U){
    tag_r.vpage := Cat(tag_r.vpage(51, 32), request_i.bits.data)
  }.elsewhen(write_v && internal_address === 0x1.U){
    tag_r.vpage := Cat(request_i.bits.data, tag_r.vpage(31, 0))
  }.elsewhen(write_v && internal_address === 0x2.U){
    process_id_r := request_i.bits.data
  }

  val lookup_process_id_o = IO(Output(UInt(processIDWidth.W)))
  val lookup_thread_id_i = IO(Input(new ThreadLookupResultPacket(param.threadNumber)))
  lookup_process_id_o := process_id_r

  // tag_r is automatically updated according to the process_id.
  tag_r.thread_id := lookup_thread_id_i.thread_id

  // Update pte_r
  when(write_v && internal_address === 0x3.U){
    pte_r.pp := request_i.bits.data
  }.elsewhen(write_v && internal_address === 0x4.U){
    pte_r.permission := request_i.bits.data
  }.elsewhen(write_v && internal_address === 0x5.U){
    pte_r.modified := request_i.bits.data
  }.elsewhen(tlb_flush_request_o.fire()){
    assert(tlb_frontend_reply_i.valid)
    pte_r := tlb_frontend_reply_i.bits.entry
  }

  // Update flush_hit_vr
  when(tlb_flush_request_o.fire()){
    flush_hit_vr := tlb_frontend_reply_i.bits.dirty && tlb_frontend_reply_i.bits.hit
  }
  
  // Determine IO port
  tlb_backend_reply_q.valid := state_r === sReply
  tlb_backend_reply_q.bits.tag := tag_r
  tlb_backend_reply_q.bits.data := pte_r

  tlb_flush_request_o.bits := tag_r
  tlb_flush_request_o.valid := state_r === sFlush
}
/**
 * This module converts a TLB backend request (evict or just flush) to a message.
 * The message will be sent to the core so that correct operations will be carried.
 * 
 * @param param the parameter of the TLB
 * 
 * @note Flush result will be ignored here because another module has already ready the data.
 */
class TLBMessageCompositor(
  param: TLBParameter
) extends MultiIOModule {
  val tlb_backend_request_i = IO(Flipped(Decoupled(new TLBBackendRequestPacket(param))))
  // TODO: Add Two FIFOs for the message.

  val miss_request = Wire(Decoupled(new SoftwareBundle.TLBMissRequestMessage))
  miss_request.bits.tag.thread_id := tlb_backend_request_i.bits.tag.thread_id
  miss_request.bits.tag.vpn := tlb_backend_request_i.bits.tag.vpage
  miss_request.valid := tlb_backend_request_i.valid && !tlb_backend_request_i.bits.w_v

  val ev_request = Wire(Decoupled(new SoftwareBundle.TLBEvictionMessage))
  ev_request.bits.tag.vpn := tlb_backend_request_i.bits.tag.vpage
  ev_request.bits.tag.thread_id := tlb_backend_request_i.bits.tag.thread_id
  ev_request.bits.entry.ppn := tlb_backend_request_i.bits.entry.pp
  ev_request.bits.entry.permission := tlb_backend_request_i.bits.entry.permission
  ev_request.bits.entry.modified := tlb_backend_request_i.bits.entry.modified
  // Why not flush: Flushed element is handled by the module TLBWrapper.
  ev_request.valid := tlb_backend_request_i.valid && tlb_backend_request_i.bits.w_v && !tlb_backend_request_i.bits.flush_v

  val miss_request_qo = Queue(miss_request, 4)
  val miss_request_o = IO(Decoupled(new SoftwareBundle.TLBMissRequestMessage))
  miss_request_o <> miss_request_qo

  val ev_request_qo = Queue(ev_request, 4)
  val eviction_request_o = IO(Decoupled(new SoftwareBundle.TLBEvictionMessage))
  eviction_request_o <> ev_request_qo

  tlb_backend_request_i.ready := Mux(
    tlb_backend_request_i.bits.w_v,
    Mux(tlb_backend_request_i.bits.flush_v, true.B, ev_request_qo.ready),
    miss_request_qo.ready
  )
}

object TLBWrapperVerilogEmitter extends App {
  import chisel3.stage.ChiselStage
  println((new ChiselStage).emitVerilog(new TLBWrapper(16, new TLBParameter)))
  println((new ChiselStage).emitVerilog(new TLBMessageCompositor(new TLBParameter)))
}