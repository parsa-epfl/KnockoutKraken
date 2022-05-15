package armflex_mmu.peripheral

import armflex.{PTTagPacket, PageEvictNotification, PageTableItem, PipeMMUIO, QEMUMessagesType}
import armflex_cache._
import armflex_mmu._
import chisel3._
import chisel3.util._

import armflex.MemoryAccessType._

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
 * @param params the parameter of the MemorySystem
 */ 
class PageDeletor(
  params: MemoryHierarchyParams
) extends Module {
  val sIdle :: sReqLSU :: sFlushTLBReq :: sFlushTLBReply :: sNotify :: sFlushPage :: sPipe :: sWait :: sSend :: sNotifyLSU ::  Nil = Enum(10)
  val state_r = RegInit(sIdle)

  val page_delete_req_i = IO(Flipped(Decoupled(new PageTableItem(params.getPageTableParams))))

  val item_r = Reg(new PageTableItem(params.getPageTableParams))

  // sAck
  val mmu_pipe_io = IO(Flipped(new PipeMMUIO))

  // TODO: Consider the case for multiple pipelines
  mmu_pipe_io.inst.flushPermReq.valid := state_r === sReqLSU && item_r.entry.perm === INST_FETCH.U
  mmu_pipe_io.data.flushPermReq.valid := state_r === sReqLSU && item_r.entry.perm =/= INST_FETCH.U

  // sFlushTLB
  val mmu_tlb_flush_io = IO(new Bundle {
    val req = Decoupled(new PTTagPacket(params.getPageTableParams))
    val resp = Flipped(Valid(new TLBPipelineResp(params.getPageTableParams)))
  })
  // TODO: Let mmu_tlb_flush_io.req.bits.req and item_r.tag has the same type.
  mmu_tlb_flush_io.req.bits.asid := item_r.tag.asid
  mmu_tlb_flush_io.req.bits.vpn := item_r.tag.vpn
  mmu_tlb_flush_io.req.valid := state_r === sFlushTLBReq

  // update the modified bit
  when(page_delete_req_i.fire){
    item_r := page_delete_req_i.bits
  }.elsewhen(state_r === sFlushTLBReply && mmu_tlb_flush_io.resp.valid && mmu_tlb_flush_io.resp.bits.hit){
    item_r.entry.modified := mmu_tlb_flush_io.resp.bits.entry.modified
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
  val mmu_cache_io = IO(new Bundle {
    val inst = Flipped(new CacheMMUIO(params.getCacheParams))
    val data = Flipped(new CacheMMUIO(params.getCacheParams))
  })

  // Counter to monitor the flush process
  val icache_flush_cnt_r = RegInit(0.U(6.W))
  val icache_flush_done_r = RegInit(false.B)
  val dcache_flush_cnt_r = RegInit(0.U(6.W))
  val dcache_flush_done_r = RegInit(false.B)
  // val flush_which = Mux(item_r.entry.perm =/= INST_FETCH.U, true.B, false.B) // true: D Cache, false: I Cache
  // It will be flushed at the same time!
  when(page_delete_req_i.fire){
    icache_flush_cnt_r := 0.U
    icache_flush_done_r := page_delete_req_i.bits.entry.perm =/= INST_FETCH.U
  }.elsewhen(state_r === sFlushPage){
    when(mmu_cache_io.inst.flushReq.fire){
      icache_flush_cnt_r := icache_flush_cnt_r + 1.U
      icache_flush_done_r := icache_flush_cnt_r === (params.cacheBlocksPerPage - 1).U
    }
  }

  when(page_delete_req_i.fire){
    dcache_flush_cnt_r := 0.U
    dcache_flush_done_r := false.B
  }.elsewhen(state_r === sFlushPage){
    when(mmu_cache_io.data.flushReq.fire){
      dcache_flush_cnt_r := dcache_flush_cnt_r + 1.U
      dcache_flush_done_r := dcache_flush_cnt_r === (params.cacheBlocksPerPage - 1).U
    }
  }

  mmu_cache_io.inst.flushReq.bits.addr := Cat(Cat(item_r.entry.ppn, icache_flush_cnt_r), 0.U(log2Ceil(params.cacheBlockSize/8).W))
  mmu_cache_io.data.flushReq.bits.addr := Cat(Cat(item_r.entry.ppn, dcache_flush_cnt_r), 0.U(log2Ceil(params.cacheBlockSize/8).W))
               
  mmu_cache_io.inst.flushReq.valid := state_r === sFlushPage && item_r.entry.perm === INST_FETCH.U && !icache_flush_done_r
  mmu_cache_io.data.flushReq.valid := state_r === sFlushPage && !dcache_flush_done_r

  // sPipe
  // Wait 4 cycles so that the request has been piped.
  val pipe_cnt_r = RegInit(0.U(2.W))
  when(page_delete_req_i.fire){
    pipe_cnt_r := 0.U
  }.elsewhen(state_r === sPipe){
    pipe_cnt_r := pipe_cnt_r + 1.U
  }

  // sWait
  // Eviction done? (You have to wait for like two / three cycles to get the correct result.)
  mmu_cache_io.inst.stallReq := state_r === sWait && item_r.entry.perm === INST_FETCH.U || state_r === sPipe
  mmu_cache_io.data.stallReq := state_r === sWait && item_r.entry.perm =/= INST_FETCH.U || state_r === sPipe

  val queue_empty = Mux(item_r.entry.perm =/= INST_FETCH.U, mmu_cache_io.data.wbEmpty, mmu_cache_io.inst.wbEmpty)

  // sSend
  // Port to send message to QEMU
  val done_message_o = IO(Decoupled(new PageEvictNotification(
    QEMUMessagesType.sEvictDone,
    params.getPageTableParams
  )))
  done_message_o.bits.item := item_r
  done_message_o.valid := state_r === sSend

  // sNotifyLSU
  mmu_pipe_io.inst.flushCompled.valid := state_r === sNotifyLSU && item_r.entry.perm === INST_FETCH.U
  mmu_pipe_io.data.flushCompled.valid := state_r === sNotifyLSU && item_r.entry.perm =/= INST_FETCH.U

  // Update logic of the state machine
  val flushPermissionRequestFire = Mux(
    item_r.entry.perm === 2.U,
    mmu_pipe_io.inst.flushPermReq.fire,
    mmu_pipe_io.data.flushPermReq.fire
  )
  val flushCompleteRequestFire = Mux(
    item_r.entry.perm === 2.U,
    mmu_pipe_io.inst.flushCompled.fire,
    mmu_pipe_io.data.flushCompled.fire
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
      when(mmu_tlb_flush_io.req.fire) {
        state_r := sFlushTLBReply
      }
    }
    is(sFlushTLBReply){
      when(mmu_tlb_flush_io.resp.valid){
        state_r := sNotify
      }
    }
    is(sNotify){
      when(start_message_o.fire) {
        state_r := sFlushPage
      }
    }
    is(sFlushPage){
      when(icache_flush_done_r && dcache_flush_done_r) {
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
    state_r === sSend && done_message_o.fire
}

object PageDeletorVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new PageDeletor(new MemoryHierarchyParams)))
}
