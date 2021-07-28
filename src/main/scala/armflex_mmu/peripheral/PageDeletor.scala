package armflex_mmu.peripheral

import armflex.{PTTagPacket, PageEvictNotification, PageTableItem, QEMUMessagesType}
import armflex_cache.{CacheFlushRequest, TLBFrontendReplyPacket}
import armflex_mmu._
import chisel3._
import chisel3.util._


/**
 * Delete a page according to the given PTE.
 * Replated function: movePageToQEMU
 * 
 * This module will:
 * 0. Wait for the Ack signal from LSU so that all pending requests are finished.
 * 1. Flush TLB. If hit and dirty, update the entry.
 * 2. Notify QEMU that an eviction will start.
 * 3. Flush I$ and D$ according to the property of this page.
 * 4. Wait for the writing list to complete
 * 5. Send notification signal to the LSU so that LSU knows the flush is complete.
 * 6. Send message to QEMU that the eviction is complete
 * 
 * @param param the parameter of the MemorySystem
 */ 
class PageDeletor(
  param: MMUParameter
) extends MultiIOModule {
  val sIdle :: sReqLSU :: sFlushTLB :: sNotify :: sFlushPage :: sPipe :: sWait :: sSend :: sNotifyLSU ::  Nil = Enum(9)
  val state_r = RegInit(sIdle)

  val page_delete_req_i = IO(Flipped(Decoupled(new PageTableItem(param.mem.toTLBParameter))))

  val item_r = Reg(new PageTableItem(param.mem.toTLBParameter))

  // sAck
  val lsu_page_delete_request_o = IO(Decoupled())
  lsu_page_delete_request_o.valid := state_r === sReqLSU

  class tlb_flush_request_t extends Bundle {
    val req = new PTTagPacket(param.mem.toTLBParameter)
    val which = UInt(1.W)
  }

  // sFlushTLB
  val tlb_flush_request_o = IO(Decoupled(new tlb_flush_request_t))
  // TODO: Let tlb_flush_request_o.bits.req and item_r.tag has the same type.
  tlb_flush_request_o.bits.req.asid := item_r.tag.asid
  tlb_flush_request_o.bits.req.vpn := item_r.tag.vpn
  tlb_flush_request_o.bits.which := Mux(item_r.entry.permission === 2.U, 0.U, 1.U) // TODO: Support more than one TLB.
  tlb_flush_request_o.valid := state_r === sFlushTLB
  val tlb_frontend_reply_i = IO(Flipped(Valid(new TLBFrontendReplyPacket(param.mem.toTLBParameter))))

  // update the modified bit
  when(page_delete_req_i.fire()){
    item_r := page_delete_req_i.bits
  }.elsewhen(
    state_r === sFlushTLB &&
    tlb_flush_request_o.fire() &&
    tlb_frontend_reply_i.bits.hit
  ){
    assert(tlb_frontend_reply_i.valid)
    item_r.entry.modified := tlb_frontend_reply_i.bits.entry.modified
  }

  // sNotify
  // Port to send a starting message.
  val start_message_o = IO(Decoupled(new PageEvictNotification(
    QEMUMessagesType.sEvictNotify,
    param.mem.toTLBParameter
  )))
  start_message_o.bits.item := item_r
  start_message_o.valid := state_r === sNotify

  // sFlush
  // Ports for flushing cache
  val icache_flush_request_o = IO(Decoupled(new CacheFlushRequest(param.mem.toCacheParameter)))
  val dcache_flush_request_o = IO(Decoupled(new CacheFlushRequest(param.mem.toCacheParameter)))

  // Counter to monitor the flush process
  val flush_cnt_r = RegInit(0.U(6.W))
  val flush_which = Mux(item_r.entry.permission =/= 2.U, true.B, false.B) // true: D Cache, false: I Cache
  val flush_fired = Mux(flush_which, dcache_flush_request_o.fire(), icache_flush_request_o.fire())
  when(page_delete_req_i.fire()){
    flush_cnt_r := 0.U
  }.elsewhen(state_r === sFlushPage){
    flush_cnt_r := Mux(
     flush_fired,
      flush_cnt_r + 1.U,
      flush_cnt_r
    )
  }

  icache_flush_request_o.bits.addr := Cat(item_r.entry.ppn, flush_cnt_r)
  dcache_flush_request_o.bits := icache_flush_request_o.bits

  icache_flush_request_o.valid := state_r === sFlushPage && !flush_which
  dcache_flush_request_o.valid := state_r === sFlushPage && flush_which

  // sPipe
  // Wait 4 cycles so that the request has been piped.
  val pipe_cnt_r = RegInit(0.U(2.W))
  when(state_r === sFlushPage && flush_cnt_r === 63.U && flush_fired){
    pipe_cnt_r := 0.U
  }.elsewhen(state_r === sPipe){
    pipe_cnt_r := pipe_cnt_r + 1.U
  }

  // sWait
  // Eviction done? (You have to wait for like two / three cycles to get the correct result.)
  val icache_wb_queue_empty_i = IO(Input(Bool()))
  val stall_icache_vo = IO(Output(Bool()))
  stall_icache_vo := state_r === sWait && item_r.entry.permission === 2.U || state_r === sPipe
  val dcache_wb_queue_empty_i = IO(Input(Bool()))
  val stall_dcache_vo = IO(Output(Bool()))
  stall_dcache_vo := state_r === sWait && item_r.entry.permission =/= 2.U || state_r === sPipe

  val queue_empty = Mux(item_r.entry.permission =/= 2.U, dcache_wb_queue_empty_i, icache_wb_queue_empty_i)

  // sMove

  // sSend
  // Port to send message to QEMU
  val done_message_o = IO(Decoupled(new PageEvictNotification(
    QEMUMessagesType.sEvictDone,
    param.mem.toTLBParameter
  )))
  done_message_o.bits.item := item_r
  done_message_o.valid := state_r === sSend

  // sNotifyLSU
  val lsu_complete_notify_o = IO(Decoupled())
  lsu_complete_notify_o.valid := state_r === sNotifyLSU

  // Update logic of the state machine
  switch(state_r){
    is(sIdle){
      state_r := Mux(page_delete_req_i.fire(), sReqLSU, sIdle)
    }
    is(sReqLSU){
      state_r := Mux(lsu_page_delete_request_o.fire(), sFlushTLB, sReqLSU)
    }
    is(sFlushTLB){
      state_r := Mux(tlb_flush_request_o.fire(), sNotify, sFlushTLB)
    }
    is(sNotify){
      state_r := Mux(
        start_message_o.fire(),
        sFlushPage,
        sNotify
      )
    }
    is(sFlushPage){
      state_r := Mux(flush_cnt_r === 63.U && flush_fired, sPipe, sFlushPage)
    }
    is(sPipe){
      state_r := Mux(pipe_cnt_r === 3.U, sWait, sPipe)
    }
    is(sWait){
      state_r := Mux(
        queue_empty,
        sSend,
        sWait
      )
    }
    is(sSend){
      state_r := Mux(done_message_o.fire(), sNotifyLSU, sSend)
    }
    is(sNotifyLSU){
      state_r := Mux(lsu_complete_notify_o.fire(), sIdle, sNotifyLSU)
    }
  }

  page_delete_req_i.ready := state_r === sIdle
  
  val done_o = IO(Output(Bool()))
  done_o := state_r === sWait && queue_empty && !item_r.entry.modified ||
    state_r === sSend && done_message_o.fire()
}

object PageDeletorVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new PageDeletor(new MMUParameter())))
}
