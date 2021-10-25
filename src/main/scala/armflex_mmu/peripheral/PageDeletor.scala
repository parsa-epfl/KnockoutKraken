package armflex_mmu.peripheral

import armflex.{PTTagPacket, PageEvictNotification, PageTableItem, PipeMMUIO, QEMUMessagesType}
import armflex_cache.{CacheFlushRequest, CacheParams, DatabankParams, TLBPipelineResp, PageTableParams}
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
 * @params params the parameter of the MemorySystem
 */ 
class PageDeletor(
  params: MemoryHierarchyParams
) extends MultiIOModule {
  val sIdle :: sReqLSU :: sFlushTLBReq :: sFlushTLBReply :: sNotify :: sFlushPage :: sPipe :: sWait :: sSend :: sNotifyLSU ::  Nil = Enum(10)
  val state_r = RegInit(sIdle)

  val page_delete_req_i = IO(Flipped(Decoupled(new PageTableItem(params.getPageTableParams))))

  val item_r = Reg(new PageTableItem(params.getPageTableParams))

  // sAck
  val lsu_handshake_o = IO(Flipped(new PipeMMUIO))

  // TODO: Consider the case for multiple pipelines
  lsu_handshake_o.inst.flushPermReq.valid := state_r === sReqLSU && item_r.entry.perm === 2.U
  lsu_handshake_o.data.flushPermReq.valid := state_r === sReqLSU && item_r.entry.perm =/= 2.U

  class tlb_flush_request_t extends Bundle {
    val req = new PTTagPacket(params.getPageTableParams)
    val sel = UInt(1.W)
  }

  // sFlushTLB
  val tlb_flush_request_o = IO(Decoupled(new tlb_flush_request_t))
  // TODO: Let tlb_flush_request_o.bits.req and item_r.tag has the same type.
  tlb_flush_request_o.bits.req.asid := item_r.tag.asid
  tlb_flush_request_o.bits.req.vpn := item_r.tag.vpn
  tlb_flush_request_o.bits.sel := Mux(item_r.entry.perm === 2.U, 0.U, 1.U) // TODO: Support more than one TLB.
  tlb_flush_request_o.valid := state_r === sFlushTLBReq
  val tlb_frontend_reply_i = IO(Flipped(Valid(new TLBPipelineResp(params.getPageTableParams))))

  // update the modified bit
  when(page_delete_req_i.fire()){
    item_r := page_delete_req_i.bits
  }.elsewhen(state_r === sFlushTLBReply && tlb_frontend_reply_i.valid && tlb_frontend_reply_i.bits.hit){
    item_r.entry.modified := tlb_frontend_reply_i.bits.entry.modified
  }

  // sNotify
  // Port to send a starting message.
  val start_message_o = IO(Decoupled(new PageEvictNotification(
    QEMUMessagesType.sEvictNotify,
    params.getPageTableParams
    )))
  start_message_o.bits.item := item_r
  start_message_o.valid := state_r === sNotify

  // sFlush
  // Ports for flushing cache
  val icache_flush_request_o = IO(Decoupled(new CacheFlushRequest(params.getCacheParams)))
  val dcache_flush_request_o = IO(Decoupled(new CacheFlushRequest(params.getCacheParams)))

  // Counter to monitor the flush process
  val flush_cnt_r = RegInit(0.U(6.W))
  val flush_which = Mux(item_r.entry.perm =/= 2.U, true.B, false.B) // true: D Cache, false: I Cache
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

  icache_flush_request_o.bits.addr := Cat(Cat(item_r.entry.ppn, flush_cnt_r), 0.U(log2Ceil(params.cacheBlockSize/8).W))
  dcache_flush_request_o.bits.addr := Cat(Cat(item_r.entry.ppn, flush_cnt_r), 0.U(log2Ceil(params.cacheBlockSize/8).W))

  icache_flush_request_o.valid := state_r === sFlushPage && item_r.entry.perm === 2.U
  dcache_flush_request_o.valid := state_r === sFlushPage && item_r.entry.perm =/= 2.U

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
  stall_icache_vo := state_r === sWait && item_r.entry.perm === 2.U || state_r === sPipe
  val dcache_wb_queue_empty_i = IO(Input(Bool()))
  val stall_dcache_vo = IO(Output(Bool()))
  stall_dcache_vo := state_r === sWait && item_r.entry.perm =/= 2.U || state_r === sPipe

  val queue_empty = Mux(item_r.entry.perm =/= 2.U, dcache_wb_queue_empty_i, icache_wb_queue_empty_i)

  // sSend
  // Port to send message to QEMU
  val done_message_o = IO(Decoupled(new PageEvictNotification(
    QEMUMessagesType.sEvictDone,
    params.getPageTableParams
    )))
  done_message_o.bits.item := item_r
  done_message_o.valid := state_r === sSend

  // sNotifyLSU
  lsu_handshake_o.inst.flushCompled.valid := state_r === sNotifyLSU && item_r.entry.perm === 2.U
  lsu_handshake_o.data.flushCompled.valid := state_r === sNotifyLSU && item_r.entry.perm =/= 2.U

  // Update logic of the state machine
  val flushPermissionRequestFire = Mux(
    item_r.entry.perm === 2.U,
    lsu_handshake_o.inst.flushPermReq.fire(),
    lsu_handshake_o.data.flushPermReq.fire()
  )
  val flushCompleteRequestFire = Mux(
    item_r.entry.perm === 2.U,
    lsu_handshake_o.inst.flushCompled.fire(),
    lsu_handshake_o.data.flushCompled.fire()
  )

  switch(state_r){
    is(sIdle){
      when(page_delete_req_i.fire) {
        state_r := sReqLSU
      }
    }
    is(sReqLSU){
      when(flushPermissionRequestFire) {
        state_r := sFlushTLBReq
      }
    }
    is(sFlushTLBReq){
      when(tlb_flush_request_o.fire) {
        state_r := sFlushTLBReply
      }
    }
    is(sFlushTLBReply){
      when(tlb_frontend_reply_i.valid){
        state_r := sNotify
      }
    }
    is(sNotify){
      when(start_message_o.fire) {
        state_r := sFlushPage
      }
    }
    is(sFlushPage){
      when(flush_cnt_r === 63.U && flush_fired) {
        state_r := sPipe
      }
    }
    is(sPipe){
      when(pipe_cnt_r === 3.U) {
        state_r := sWait
      }
    }
    is(sWait){
      when(queue_empty) {
        state_r := sSend
      }
    }
    is(sSend){
      when(done_message_o.fire) {
        state_r := sNotifyLSU
      }
    }
    is(sNotifyLSU){
      when(flushCompleteRequestFire) {
        state_r := sIdle
      }
    }
  }

  page_delete_req_i.ready := state_r === sIdle
  
  val done_o = IO(Output(Bool()))
  done_o := state_r === sWait && queue_empty && !item_r.entry.modified ||
    state_r === sSend && done_message_o.fire()
}

object PageDeletorVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new PageDeletor(new MemoryHierarchyParams)))
}
