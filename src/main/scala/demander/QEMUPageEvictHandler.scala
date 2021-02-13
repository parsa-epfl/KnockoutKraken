package armflex.demander

import chisel3._
import chisel3.util._
import armflex.cache._
import armflex.demander.software_bundle.QEMUMissReply
import armflex.demander.software_bundle.ParameterConstants
import armflex.demander.peripheral.PageTableSetBuffer
import armflex.demander.peripheral.PageTableSetPacket
import armflex.demander.peripheral.ThreadLookupResultPacket
import DMAController.Bus._
import DMAController.Frontend._
import chisel3.experimental.Param
import armflex.demander.software_bundle.PageTableItem
import armflex.demander.software_bundle.QEMUPageEvictRequest

class QEMUPageEvictHandler extends MultiIOModule {
  val evict_request_i = IO(Flipped(Decoupled(new QEMUPageEvictRequest())))
  
  val sIdle :: sLoadSet :: sDeletePageReq :: sDeletePage :: Nil = Enum(4)
  val state_r = RegInit(sIdle)

  val u_buffer = Module(new PageTableSetBuffer(new PageTableSetPacket))
  val u_axi_read = Module(new AXI4Reader(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))
  u_buffer.dma_data_i <> u_axi_read.io.dataOut
  u_buffer.dma_data_o.ready := false.B

  u_buffer.write_request_i.valid := false.B
  u_buffer.write_request_i.bits := DontCare
  u_buffer.store_enable_vi := false.B


  // AXI Bus
  val M_AXI = IO(new AXI4(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))

  M_AXI <> u_axi_read.io.bus
  // sIdle
  evict_request_i.ready := state_r === sIdle
  val request_r = Reg(new QEMUPageEvictRequest)
  when(evict_request_i.fire()){
    request_r := evict_request_i.bits
  }

  u_buffer.load_enabled_vi := evict_request_i.fire()

  // sLoadSet
  u_axi_read.io.xfer.address := ParameterConstants.getPageTableAddressByVPN(request_r.tag.vpn)
  u_axi_read.io.xfer.length := u_buffer.requestPacketNumber.U
  u_axi_read.io.xfer.valid := state_r === sLoadSet

  val victim_r = Reg(new PageTableItem)
  u_buffer.lookup_request_i := request_r.tag
  when(state_r === sDeletePageReq){
    victim_r := u_buffer.lookup_reply_o.item
    assert(u_buffer.lookup_reply_o.hit_v)
  }

  // sDeletePage
  // Judge by done_o
  val page_delete_req_o = IO(Decoupled(new PageTableItem))
  val page_delete_done_i = IO(Input(Bool()))
  page_delete_req_o.bits := victim_r
  page_delete_req_o.valid := state_r === sDeletePageReq

  // state machine
  switch(state_r){
    is(sIdle){
      state_r := Mux(evict_request_i.fire(), sLoadSet, sIdle)
    }
    is(sLoadSet){
      state_r := Mux(u_axi_read.io.xfer.done, sDeletePageReq, sLoadSet)
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
  println(c.emitVerilog(new QEMUPageEvictHandler))
}

