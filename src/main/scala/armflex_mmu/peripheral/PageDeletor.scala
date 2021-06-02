package armflex_mmu.peripheral

import armflex.{PTTagPacket, PageEvictNotification, PageTableItem, QEMUMessagesType}
import chisel3._
import chisel3.util._
import armflex_mmu._
import armflex_cache.{CacheFrontendFlushRequest, CacheParameter, TLBFrontendReplyPacket}
import armflex.util._


/**
 * Delete a page according to the given PTE.
 * Replated function: movePageToQEMU
 * 
 * This module will:
 * 1. Lookup the Thread table for the thread. If not paried with a thread, go to 3.
 * 2. Flush TLB. If hit and dirty, update the entry.
 * 3. Notify QEMU that an eviction will start.
 * 4. Flush I$ and D$ according to the property of this page.
 * idea: judge the type of cache (I or D) by the permission. If read only, I$.
 * 5. Wait for the writing list to complete
 * 6. Push the page to the QEMU page buffer if this page is dirty
 * 7. Send message to QEMU that the eviction is complete
 * 
 * @param param the parameter of the MemorySystem
 */ 
class PageDeletor(
  param: PageDemanderParameter
) extends MultiIOModule {
  val sIdle :: sFlushTLB :: sNotify :: sFlushPage :: sPipe :: sWait :: sMove :: sSend :: Nil = Enum(8)
  val state_r = RegInit(sIdle)

  val page_delete_req_i = IO(Flipped(Decoupled(new PageTableItem(param.mem.toTLBParameter()))))

  val item_r = Reg(new PageTableItem(param.mem.toTLBParameter()))


  class tlb_flush_request_t extends Bundle {
    val req = new PTTagPacket(param.mem.toTLBParameter())
    val which = UInt(1.W)
  }

  // sFlushTLB
  val tlb_flush_request_o = IO(Decoupled(new tlb_flush_request_t))
  // TODO: Let tlb_flush_request_o.bits.req and item_r.tag has the same type.
  tlb_flush_request_o.bits.req.asid := item_r.tag.asid
  tlb_flush_request_o.bits.req.vpn := item_r.tag.vpn
  tlb_flush_request_o.bits.which := Mux(item_r.entry.permission === 2.U, 0.U, 1.U) // TODO: Support more than one TLB.
  tlb_flush_request_o.valid := state_r === sFlushTLB
  val tlb_frontend_reply_i = IO(Flipped(Valid(new TLBFrontendReplyPacket(param.mem.toTLBParameter()))))

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
    param.mem.toTLBParameter()
  )))
  start_message_o.bits.item := item_r
  start_message_o.valid := state_r === sNotify

  // sFlush
  // Ports for flushing cache
  val icache_flush_request_o = IO(Decoupled(new CacheFrontendFlushRequest(param.mem.toCacheParameter())))
  val dcache_flush_request_o = IO(Decoupled(new CacheFrontendFlushRequest(param.mem.toCacheParameter())))

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
  icache_flush_request_o.bits.asid := item_r.tag.asid
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
  stall_icache_vo := state_r === sWait && item_r.entry.permission === 2.U
  val dcache_wb_queue_empty_i = IO(Input(Bool()))
  val stall_dcache_vo = IO(Output(Bool()))
  stall_dcache_vo := state_r === sWait && item_r.entry.permission =/= 2.U

  val queue_empty = Mux(item_r.entry.permission =/= 2.U, dcache_wb_queue_empty_i, icache_wb_queue_empty_i)

  // sMove
  // A DMA that moves data from the DRAM to page buffer.
  class page_buffer_write_request_t extends Bundle {
    val addr = UInt(10.W) // TODO: Make external and let it becomes a parameter.
    val data = UInt(512.W) // TODO: Make external.
    val last_v = Bool()
  }
  val page_buffer_write_o = IO(Decoupled(new page_buffer_write_request_t))

  // AXI DMA Read Channel
  val M_DMA_R = IO(new AXIReadMasterIF(
    param.dramAddrWidth,
    param.dramDataWidth
  ))

  M_DMA_R.req.bits.address := Cat(item_r.entry.ppn, Fill(12, 0.U(1.W)))
  M_DMA_R.req.bits.length := 64.U
  M_DMA_R.req.valid := state_r === sMove

  // A counter to control the address of the output
  // ? What if I want to delete more than one page at the same time?
  // TODO: We need a method to assign addresses to the page buffer. Maybe a stack or something else.
  // ? Actually I prefer a pointer.
  val page_buffer_addr_cnt_r = RegInit(0.U(6.W)) 
  page_buffer_write_o.bits.data := M_DMA_R.data.bits
  page_buffer_write_o.bits.addr := page_buffer_addr_cnt_r
  page_buffer_write_o.bits.last_v := page_buffer_addr_cnt_r === 63.U
  page_buffer_write_o.valid := M_DMA_R.data.valid
  M_DMA_R.data.ready := page_buffer_write_o.ready

  when(state_r === sIdle){
    page_buffer_addr_cnt_r := 0.U
  }.elsewhen(page_buffer_write_o.fire()){
    page_buffer_addr_cnt_r := page_buffer_addr_cnt_r + 1.U
  }

  // sSend
  // Port to send message to QEMU
  val done_message_o = IO(Decoupled(new PageEvictNotification(
    QEMUMessagesType.sEvictDone,
    param.mem.toTLBParameter()
  )))
  done_message_o.bits.item := item_r
  done_message_o.valid := state_r === sSend

  // Update logic of the state machine
  switch(state_r){
    is(sIdle){
      state_r := Mux(page_delete_req_i.fire(), sFlushTLB, sIdle)
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
        Mux(item_r.entry.modified, sMove, sIdle),
        sWait
      )
    }
    is(sMove){
      state_r := Mux(M_DMA_R.done, sSend, sMove)
    }
    is(sSend){
      state_r := Mux(done_message_o.fire(), sIdle, sSend)
    }
  }

  page_delete_req_i.ready := state_r === sIdle
  
  val done_o = IO(Output(Bool()))
  done_o := state_r === sWait && queue_empty && !item_r.entry.modified ||
    state_r === sSend && done_message_o.fire()
}

object PageDeletorVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new PageDeletor(new PageDemanderParameter())))
}
