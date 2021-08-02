package armflex_mmu

import armflex.util.{AXIReadMasterIF, AXIWriteMasterIF}
import armflex.{PageTableItem, QEMUMissReply}
import armflex_cache._
import armflex_mmu.peripheral.{PageTableSetBuffer, PageTableSetPacket}
import chisel3._
import chisel3.util._

class QEMUMissReplyHandler(
  params: MemoryHierarchyParams
) extends MultiIOModule {
  val qemu_miss_reply_i = IO(Flipped(Decoupled(new QEMUMissReply(params.getPageTableParams))))

  val sIdle ::  sLoadSet :: sGetEvict :: sDeletePageReq :: sDeletePage :: sReplace ::  sMoveback :: sReplyToTLB :: Nil = Enum(8)
  val state_r = RegInit(sIdle)

  val u_buffer = Module(new PageTableSetBuffer(
    params.getPageTableParams,
    new PageTableSetPacket(params.getPageTableParams)
    ))

  // AXI DMA Read Channels
  val M_DMA_R = IO(new AXIReadMasterIF(
    params.dramAddrW,
    params.dramdataW
    ))

  u_buffer.dma_data_i <> M_DMA_R.data

  // AXI DMA Write Channels
  val M_DMA_W = IO(new AXIWriteMasterIF(
    params.dramAddrW,
    params.dramdataW
    ))

  val move_out_enq = Wire(Decoupled(UInt(params.dramdataW.W)))
  move_out_enq.bits := u_buffer.dma_data_o.bits
  move_out_enq.valid := u_buffer.dma_data_o.valid && state_r === sMoveback
  u_buffer.dma_data_o.ready := move_out_enq.ready && state_r === sMoveback
  M_DMA_W.data <> move_out_enq

  // sIdle
  qemu_miss_reply_i.ready := state_r === sIdle
  val request_r = Reg(new QEMUMissReply(params.getPageTableParams))
  when(qemu_miss_reply_i.fire()){
    request_r := qemu_miss_reply_i.bits
  }

  u_buffer.lookup_request_i := request_r.tag

  // sLoadSet
  M_DMA_R.req.bits.address := params.vpn2ptSetPA(request_r.tag.vpn)
  M_DMA_R.req.bits.length := u_buffer.requestPacketNumber.U
  M_DMA_R.req.valid := state_r === sLoadSet

  // sGetEvict
  val victim_r = Reg(new PageTableItem(params.getPageTableParams))
  val target_index_r = Reg(UInt(log2Ceil(u_buffer.entryNumber).W))
  when(state_r === sGetEvict){
    victim_r := u_buffer.lru_element_o.item
    target_index_r := u_buffer.lru_element_o.index
  }

  // sDeletePageReq
  // Judge by done_o
  val page_delete_req_o = IO(Decoupled(new PageTableItem(params.getPageTableParams)))
  val page_delete_done_i = IO(Input(Bool()))
  page_delete_req_o.bits := victim_r
  page_delete_req_o.valid := state_r === sDeletePageReq

  // sReplace
  u_buffer.write_request_i.bits.flush_v := false.B
  u_buffer.write_request_i.bits.index := target_index_r
  u_buffer.write_request_i.bits.item.tag := request_r.tag
  u_buffer.write_request_i.bits.item.entry.modified := false.B
  u_buffer.write_request_i.bits.item.entry.perm := request_r.perm
  u_buffer.write_request_i.bits.item.entry.ppn := request_r.ppn
  u_buffer.write_request_i.valid := state_r === sReplace

  // sMoveback
  M_DMA_W.req.bits.address := params.vpn2ptSetPA(request_r.tag.vpn)
  M_DMA_W.req.bits.length := u_buffer.requestPacketNumber.U
  M_DMA_W.req.valid := state_r === sMoveback

  // sReplyTLB
  val tlb_backend_reply_o = IO(Decoupled(new TLBMMURespPacket(params.getPageTableParams)))
  tlb_backend_reply_o.bits.tag := request_r.tag
  tlb_backend_reply_o.bits.data.modified := false.B
  tlb_backend_reply_o.bits.data.perm := request_r.perm
  tlb_backend_reply_o.bits.data.ppn := request_r.ppn
  tlb_backend_reply_o.bits.thid := request_r.thid
  tlb_backend_reply_o.valid := state_r === sReplyToTLB

  // state machine
  switch(state_r){
    is(sIdle){
      state_r := Mux(qemu_miss_reply_i.fire(), sLoadSet, sIdle)
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
        sMoveback,
        sReplace
      )
    }
    is(sMoveback){
      state_r := Mux(
        M_DMA_W.done,
        Mux(request_r.thid_v, sReplyToTLB, sIdle),
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
  println(c.emitVerilog(new QEMUMissReplyHandler(new MemoryHierarchyParams())))
}

