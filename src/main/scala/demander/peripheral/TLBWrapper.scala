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
  param: TLBParameter
) extends MultiIOModule {
  // The backend port of TLB.
  val tlb_backend_reply_o = IO(Decoupled(new TLBBackendReplyPacket(param)))
  val tlb_backend_reply_qi = Wire(Decoupled(new TLBBackendReplyPacket(param)))
  tlb_backend_reply_o <> Queue(tlb_backend_reply_qi, 4)
  // TODO: add FIFO to the reply and request port. FIFO is also necessary for the flush request.
  // TLB flush request.
  val tlb_flush_request_o = IO(Decoupled(new TLBTagPacket(param)))
  // TLB reply, which stores the flush result.
  val tlb_frontend_reply_i = IO(Flipped(Valid(new TLBFrontendReplyPacket(param))))

  val tag_r = Reg(new TLBTagPacket(param))
  val pte_r = Reg(new TLBEntryPacket(param))

  val sIdle :: sFlush :: sReply :: Nil = Enum(3)
  val state_r = RegInit(sIdle)

  // TODO: Determine the CSR of this module
  /**
   * Address   |  CSR
   * 0x0 - 0x1 |  vpn
   * 0x2       |  t_id
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
      reply_r := tag_r.thread_id
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
      reply_r := tlb_backend_reply_qi.ready
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
      when(tlb_backend_reply_qi.fire()){
        state_r := sIdle
      }
    }
  }

  // Update tag_r
  when(write_v && internal_address === 0x0.U){
    tag_r.vpage := Cat(tag_r.vpage(51, 32), request_i.bits.data)
  }.elsewhen(write_v && internal_address === 0x1.U){
    tag_r.vpage := Cat(request_i.bits.data, tag_r.vpage(31, 0))
  }.elsewhen(write_v && internal_address === 0x2.U){
    tag_r.thread_id := request_i.bits.data
  }

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
  tlb_backend_reply_qi.valid := state_r === sReply
  tlb_backend_reply_qi.bits.tag := tag_r
  tlb_backend_reply_qi.bits.data := pte_r

  tlb_flush_request_o.bits := tag_r
  tlb_flush_request_o.valid := state_r === sFlush
}

class TLBBackendMissMessagePacket extends SoftwareControlledBundle {
  val v_page = UInt(52.W)
  val t_id = UInt(2.W)

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(Seq(
      v_page(31, 0),
      v_page(51, 32),
      t_id
    ))
  }
  
  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.v_page := Cat(f(1), f(0))
    res.v_page := f(2)
    res.asInstanceOf[this.type]
  }
}

class TLBBackendEvictMessagePacket extends SoftwareControlledBundle {
  val v_page = UInt(52.W)
  val t_id = UInt(2.W)
  val ppn = UInt(24.W)
  val permission = UInt(2.W)
  val modified = Bool()

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(Seq(
      v_page(31, 0),
      v_page(51, 32),
      t_id,
      ppn,
      permission,
      modified
    ))
  }

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.v_page := Cat(f(1), f(0))
    res.t_id := f(2)
    res.ppn := f(3)
    res.permission := f(4)
    res.modified := f(5)
    res.asInstanceOf[this.type]
  }
}

/**
 * This module converts a TLB backend request (evict or just flush) to a message.
 * The message will be sent to the core so that correct operations will be carried.
 * 
 * @param param the parameter of the TLB
 */
class TLBMessageCompositor(
  param: TLBParameter
) extends MultiIOModule {
  val tlb_backend_request_i = IO(Flipped(Decoupled(new TLBBackendRequestPacket(param))))
  // TODO: Add Two FIFOs for the message.

  val miss_request = Wire(Decoupled(new TLBBackendMissMessagePacket))
  miss_request.bits.t_id := tlb_backend_request_i.bits.tag.thread_id
  miss_request.bits.v_page := tlb_backend_request_i.bits.tag.vpage
  miss_request.valid := tlb_backend_request_i.valid && !tlb_backend_request_i.bits.w_v

  val ev_request = Wire(Decoupled(new TLBBackendEvictMessagePacket))
  ev_request.bits.v_page := tlb_backend_request_i.bits.tag.vpage
  ev_request.bits.t_id := tlb_backend_request_i.bits.tag.thread_id
  ev_request.bits.ppn := tlb_backend_request_i.bits.entry.pp
  ev_request.bits.permission := tlb_backend_request_i.bits.entry.permission
  ev_request.bits.modified := tlb_backend_request_i.bits.entry.modified
  // Why not flush: Flushed element is handled by the module TLBWrapper.
  ev_request.valid := tlb_backend_request_i.valid && tlb_backend_request_i.bits.w_v && !tlb_backend_request_i.bits.flush_v

  val miss_request_qo = Queue(miss_request, 4)
  val miss_request_o = IO(Decoupled(new TLBBackendMissMessagePacket))
  miss_request_o <> miss_request_qo

  val ev_request_qo = Queue(ev_request, 4)
  val eviction_request_o = IO(Decoupled(new TLBBackendEvictMessagePacket))
  eviction_request_o <> ev_request_qo

  tlb_backend_request_i.ready := Mux(
    tlb_backend_request_i.bits.w_v,
    Mux(tlb_backend_request_i.bits.flush_v, true.B, ev_request_qo.ready),
    miss_request_qo.ready
  )
}

object TLBWrapperVerilogEmitter extends App {
  import chisel3.stage.ChiselStage
  println((new ChiselStage).emitVerilog(new TLBWrapper(new TLBParameter)))
  println((new ChiselStage).emitVerilog(new TLBMessageCompositor(new TLBParameter)))
}