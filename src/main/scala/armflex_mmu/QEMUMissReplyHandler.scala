package armflex_mmu

import chisel3._
import chisel3.util._
import armflex_cache._
import armflex_mmu.peripheral.PageTableSetBuffer
import armflex_mmu.peripheral.PageTableSetPacket
import antmicro.Bus._
import antmicro.Frontend._
import armflex.{PageTableItem, QEMUMissReply}
import chisel3.experimental.Param
import armflex.util.AXIDMARequestPacket
import armflex.util.AXIReadMasterIF
import armflex.util.AXIWriteMasterIF

class QEMUMissReplyHandler(
  param: PageDemanderParameter
) extends MultiIOModule {
  val qemu_miss_reply_i = IO(Flipped(Decoupled(new QEMUMissReply(param.mem.toTLBParameter()))))
  
  val sIdle :: sCheckAndLoadSynonym :: sGetSynonym :: sLoadSet :: sGetEvict :: sDeletePageReq :: sDeletePage :: sReplace :: sInsertPage :: sMoveback :: sReplyToTLB :: Nil = Enum(11)
  val state_r = RegInit(sIdle)

  val u_buffer = Module(new PageTableSetBuffer(
    param.mem.toTLBParameter(),
    new PageTableSetPacket(param.mem.toTLBParameter())
  ))

  // AXI DMA Read Channels
  val M_DMA_R = IO(new AXIReadMasterIF(
    param.dramAddrWidth,
    param.dramDataWidth
  ))

  u_buffer.dma_data_i <> M_DMA_R.data

  // AXI DMA Write Channels
  val M_DMA_W = IO(new AXIWriteMasterIF(
    param.dramAddrWidth,
    param.dramDataWidth
  ))

  val move_out_enq = Wire(Decoupled(UInt(param.dramDataWidth.W)))
  move_out_enq.bits := u_buffer.dma_data_o.bits
  move_out_enq.valid := u_buffer.dma_data_o.valid && state_r === sMoveback
  u_buffer.dma_data_o.ready := move_out_enq.ready && state_r === sMoveback
  M_DMA_W.data <> move_out_enq

  // sIdle

  qemu_miss_reply_i.ready := state_r === sIdle
  val request_r = Reg(new QEMUMissReply(param.mem.toTLBParameter()))
  val ppn_r = RegInit(0.U(param.mem.pPageNumberWidth().W))
  when(qemu_miss_reply_i.fire()){
    request_r := qemu_miss_reply_i.bits
  }


  // sCheckAndLoadSynonym
  // Port for access Freelist
  val ppn_pop_i = IO(Flipped(Decoupled(UInt(param.mem.pPageNumberWidth().W))))
  ppn_pop_i.ready := !request_r.synonym_v && state_r === sCheckAndLoadSynonym

  // sGetSynonym
  u_buffer.lookup_request_i := request_r.synonym_tag
  when(ppn_pop_i.fire()){
    ppn_r := ppn_pop_i.bits
  }.elsewhen(state_r === sGetSynonym){
    assert(u_buffer.lookup_reply_o.hit_v)
    ppn_r := u_buffer.lookup_reply_o.item.entry.ppn
  }

  // sLoadSet
  M_DMA_R.req.bits.address := Mux(
    state_r === sCheckAndLoadSynonym,
    param.getPageTableAddressByVPN(request_r.synonym_tag.vpn),
    param.getPageTableAddressByVPN(request_r.tag.vpn)
  )
  M_DMA_R.req.bits.length := u_buffer.requestPacketNumber.U
  M_DMA_R.req.valid := Mux(
    state_r === sCheckAndLoadSynonym,
    request_r.synonym_v,
    state_r === sLoadSet
  )

  // sGetEvict
  val victim_r = Reg(new PageTableItem(param.mem.toTLBParameter()))
  val target_index_r = Reg(UInt(log2Ceil(u_buffer.entryNumber).W))
  when(state_r === sGetEvict){
    victim_r := u_buffer.lru_element_o.item
    target_index_r := u_buffer.lru_element_o.index
  }

  // sDeletePageReq
  // Judge by done_o
  val page_delete_req_o = IO(Decoupled(new PageTableItem(param.mem.toTLBParameter())))
  val page_delete_done_i = IO(Input(Bool()))
  page_delete_req_o.bits := victim_r
  page_delete_req_o.valid := state_r === sDeletePageReq

  // sReplace
  u_buffer.write_request_i.bits.flush_v := false.B
  u_buffer.write_request_i.bits.index := target_index_r
  u_buffer.write_request_i.bits.item.tag := request_r.tag
  u_buffer.write_request_i.bits.item.entry.modified := false.B
  u_buffer.write_request_i.bits.item.entry.permission := request_r.permission
  u_buffer.write_request_i.bits.item.entry.ppn := ppn_r
  u_buffer.write_request_i.valid := state_r === sReplace

  // sInsertPage
  // TODO: RAW Hazard detected. You have to wait for the Page deletor to complete before inserting pages.
  val page_insert_req_o = IO(Decoupled(UInt(param.mem.pPageNumberWidth().W)))
  page_insert_req_o.bits := ppn_r
  page_insert_req_o.valid := state_r === sInsertPage
  val page_insert_done_i = IO(Input(Bool()))

  // sMoveback
  M_DMA_W.req.bits.address := param.getPageTableAddressByVPN(request_r.tag.vpn)
  M_DMA_W.req.bits.length := u_buffer.requestPacketNumber.U
  M_DMA_W.req.valid := state_r === sMoveback

  // sReplyTLB
  val tlb_backend_reply_o = IO(Decoupled(new TLBBackendReplyPacket(param.mem.toTLBParameter())))
  tlb_backend_reply_o.bits.tag := request_r.tag
  tlb_backend_reply_o.bits.data.modified := false.B
  tlb_backend_reply_o.bits.data.permission := request_r.permission
  tlb_backend_reply_o.bits.data.ppn := ppn_r
  tlb_backend_reply_o.bits.wakeup_tid.bits := request_r.tid
  tlb_backend_reply_o.bits.wakeup_tid.valid := request_r.tid_v
  tlb_backend_reply_o.valid := state_r === sReplyToTLB

  // state machine
  switch(state_r){
    is(sIdle){
      state_r := Mux(qemu_miss_reply_i.fire(), sCheckAndLoadSynonym, sIdle)
    }
    is(sCheckAndLoadSynonym){
      state_r := Mux(
        !request_r.synonym_v, 
        Mux(ppn_pop_i.fire(), sLoadSet, sCheckAndLoadSynonym),
        Mux(M_DMA_R.done, sGetSynonym, sCheckAndLoadSynonym)
      )
    }
    is(sGetSynonym){
      state_r := sLoadSet
    }
    is(sLoadSet){
      state_r := Mux(M_DMA_R.done, sGetEvict, sLoadSet)
    }
    is(sGetEvict){
      state_r := Mux(u_buffer.lru_element_o.lru_v, sDeletePageReq, sReplace)
    }
    is(sDeletePageReq){
      state_r := Mux(page_delete_req_o.fire(), sDeletePage, sDeletePageReq)
    }
    is(sDeletePage){
      state_r := Mux(page_delete_done_i, sReplace, sDeletePage)
    }
    is(sReplace){
      state_r := Mux(
        u_buffer.write_request_i.fire(),
        Mux(request_r.synonym_v, sMoveback ,sInsertPage), // Synonym means no pages insertion is needed.
        sReplace
      )
    }
    is(sInsertPage){
      state_r := Mux(page_insert_done_i, sMoveback, sInsertPage)
    }
    is(sMoveback){
      state_r := Mux(
        M_DMA_W.done,
        sReplyToTLB,
        sMoveback
      )
    }
    is(sReplyToTLB){
      state_r := Mux(tlb_backend_reply_o.fire(), sIdle, sReplyToTLB)
    }
  }
}

object QEMUMissReplyHandlerVerilogEmitter extends App {
  val c = new stage.ChiselStage
  println(c.emitVerilog(new QEMUMissReplyHandler(new PageDemanderParameter())))
}

