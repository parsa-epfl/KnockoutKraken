package armflex_mmu

import chisel3._
import chisel3.util._
import armflex_mmu.peripheral.PageTableSetBuffer
import armflex_mmu.peripheral.PageTableSetPacket
import armflex.{PageTableItem, QEMUPageEvictRequest}
import armflex.util._
import armflex_cache.PageTableParams

class QEMUPageEvictHandler(
  params: MemoryHierarchyParams
) extends Module {
  val evict_request_i = IO(Flipped(Decoupled(new QEMUPageEvictRequest(params.getPageTableParams))))

  val sIdle :: sLoadSet :: sGetEntry :: sDeletePageReq :: sDeletePage :: sFlushEntry :: sUpdatePT :: Nil = Enum(7)
  val state_r = RegInit(sIdle)

  val u_buffer = Module(new PageTableSetBuffer(
    params.getPageTableParams,
    new PageTableSetPacket(params.getPageTableParams)
    ))

  // AXI DMA read and write channels
  val M_DMA_R = IO(new AXIReadMasterIF(params.dramAddrW, params.dramdataW))
  val M_DMA_W = IO(new AXIWriteMasterIF(params.dramAddrW, params.dramdataW))

  u_buffer.dma_data_i <> M_DMA_R.data
  u_buffer.dma_data_o <> M_DMA_W.data

  // sIdle
  evict_request_i.ready := state_r === sIdle
  val request_r = Reg(new QEMUPageEvictRequest(params.getPageTableParams))
  when(evict_request_i.fire){
    request_r := evict_request_i.bits
  }

  // sLoadSet
  M_DMA_R.req.bits.address := params.vpn2ptSetPA(request_r.tag.asid, request_r.tag.vpn, params.getPageTableParams.ptAssociativity)
  M_DMA_R.req.bits.length := u_buffer.requestPacketNumber.U
  M_DMA_R.req.valid := state_r === sLoadSet

  val index_r = Reg(u_buffer.lookup_reply_o.index.cloneType)
  val victim_r = Reg(new PageTableItem(params.getPageTableParams))
  u_buffer.lookup_request_i := request_r.tag
  when(state_r === sGetEntry){
    victim_r := u_buffer.lookup_reply_o.item
    index_r := u_buffer.lookup_reply_o.index
    assert(u_buffer.lookup_reply_o.hit_v)
  }

  // sDeletePage
  // Judge by done_o
  val page_delete_req_o = IO(Decoupled(new PageTableItem(params.getPageTableParams)))
  val page_delete_done_i = IO(Input(Bool()))
  page_delete_req_o.bits := victim_r
  page_delete_req_o.valid := state_r === sDeletePageReq

  // sFlushEntry
  u_buffer.write_request_i.valid := state_r === sFlushEntry
  u_buffer.write_request_i.bits.flush_v := true.B
  u_buffer.write_request_i.bits.index := index_r
  u_buffer.write_request_i.bits.item := DontCare

  // sUpdatePT
  M_DMA_W.req.bits.address := params.vpn2ptSetPA(request_r.tag.asid, request_r.tag.vpn, params.getPageTableParams.ptAssociativity)
  M_DMA_W.req.bits.length := u_buffer.requestPacketNumber.U
  M_DMA_W.req.valid := state_r === sUpdatePT

  // state machine
  switch(state_r){
    is(sIdle){
      when(evict_request_i.fire) {
        state_r := sLoadSet
      }
    }
    is(sLoadSet){
      when(M_DMA_R.done) {
        state_r := sGetEntry
      }
    }
    is(sGetEntry){
      state_r := sDeletePageReq
    }
    is(sDeletePageReq){
      when(page_delete_req_o.fire) {
        state_r := sDeletePage
      }
    }
    is(sDeletePage){
      when(page_delete_done_i) {
        state_r := sFlushEntry
      }
    }
    is(sFlushEntry){
      when(u_buffer.write_request_i.fire) {
        state_r := sUpdatePT
      }
    }
    is(sUpdatePT){
      when(M_DMA_W.done){
        state_r := sIdle
      }
    }
  }
}

object QEMUPageEvictHandlerVerilogEmitter extends App {
  val c = new stage.ChiselStage
  println(c.emitVerilog(new QEMUPageEvictHandler(new MemoryHierarchyParams())))
}

