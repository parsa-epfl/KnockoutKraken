package armflex_mmu

import chisel3._
import chisel3.util._
import armflex_cache._
import armflex_mmu.peripheral.PageTableSetBuffer
import armflex_mmu.peripheral.PageTableSetPacket
import antmicro.Bus._
import antmicro.Frontend._
import armflex.{PageTableItem, QEMUMissReply, QEMUPageEvictRequest}
import chisel3.experimental.Param
import armflex.util._

class QEMUPageEvictHandler(
  param: PageDemanderParameter
) extends MultiIOModule {
  val evict_request_i = IO(Flipped(Decoupled(new QEMUPageEvictRequest(param.mem.toTLBParameter))))
  
  val sIdle :: sLoadSet :: sGetEntry :: sDeletePageReq :: sDeletePage :: Nil = Enum(5)
  val state_r = RegInit(sIdle)

  val u_buffer = Module(new PageTableSetBuffer(
    param.mem.toTLBParameter(),
    new PageTableSetPacket(param.mem.toTLBParameter())
  ))

  // AXI DMA Read channels
  val M_DMA_R = IO(new AXIReadMasterIF(
    param.dramAddrWidth,
    param.dramDataWidth
  ))
  
  u_buffer.dma_data_i <> M_DMA_R.data
  u_buffer.dma_data_o.ready := false.B

  u_buffer.write_request_i.valid := false.B
  u_buffer.write_request_i.bits := DontCare

  // sIdle
  evict_request_i.ready := state_r === sIdle
  val request_r = Reg(new QEMUPageEvictRequest(param.mem.toTLBParameter()))
  when(evict_request_i.fire()){
    request_r := evict_request_i.bits
  }

  // sLoadSet
  M_DMA_R.req.bits.address := param.getPageTableAddressByVPN(request_r.tag.vpn)
  M_DMA_R.req.bits.length := u_buffer.requestPacketNumber.U
  M_DMA_R.req.valid := state_r === sLoadSet

  val victim_r = Reg(new PageTableItem(param.mem.toTLBParameter()))
  u_buffer.lookup_request_i := request_r.tag
  when(state_r === sGetEntry){
    victim_r := u_buffer.lookup_reply_o.item
    assert(u_buffer.lookup_reply_o.hit_v)
  }

  // sDeletePage
  // Judge by done_o
  val page_delete_req_o = IO(Decoupled(new PageTableItem(param.mem.toTLBParameter())))
  val page_delete_done_i = IO(Input(Bool()))
  page_delete_req_o.bits := victim_r
  page_delete_req_o.valid := state_r === sDeletePageReq

  // state machine
  switch(state_r){
    is(sIdle){
      state_r := Mux(evict_request_i.fire(), sLoadSet, sIdle)
    }
    is(sLoadSet){
      state_r := Mux(M_DMA_R.done, sGetEntry, sLoadSet)
    }
    is(sGetEntry){
      state_r := sDeletePageReq
    }
    is(sDeletePageReq){
      state_r := Mux(page_delete_req_o.fire(), sDeletePage, sDeletePageReq)
    }
    is(sDeletePage){
      state_r := Mux(page_delete_done_i, sIdle, sDeletePage)
    }
  }
}

object QEMUPageEvictHandlerrVerilogEmitter extends App {
  val c = new stage.ChiselStage
  println(c.emitVerilog(new QEMUPageEvictHandler(new PageDemanderParameter())))
}

