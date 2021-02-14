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

class QEMUMissReplyHandler(
  param: TLBParameter
) extends MultiIOModule {
  val qemu_miss_reply_i = IO(Flipped(Decoupled(new QEMUMissReply())))
  
  val sIdle :: sCheckAndLoadSynonym :: sGetSynonym :: sLoadSet :: sGetEvict :: sDeletePageReq :: sDeletePage :: sReplace :: sInsertPage :: sMoveback :: sReplyToTLB :: Nil = Enum(11)
  val state_r = RegInit(sIdle)

  val u_buffer = Module(new PageTableSetBuffer(new PageTableSetPacket))
  val u_axi_read = Module(new AXI4Reader(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))
  u_buffer.dma_data_i <> u_axi_read.io.dataOut

  val u_axi_write = Module(new AXI4Writer(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))
  u_buffer.dma_data_o <> u_axi_write.io.dataIn

  // AXI Bus
  val M_AXI = IO(new AXI4(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))

  M_AXI.ar <> u_axi_read.io.bus.ar
  M_AXI.r <> u_axi_read.io.bus.r

  u_axi_read.io.bus.aw <> AXI4AW.stub(ParameterConstants.dram_addr_width)
  u_axi_read.io.bus.w <> AXI4W.stub(ParameterConstants.dram_data_width)
  u_axi_read.io.bus.b <> AXI4B.stub()
  
  M_AXI.aw <> u_axi_write.io.bus.aw
  M_AXI.w <> u_axi_write.io.bus.w
  M_AXI.b <> u_axi_write.io.bus.b

  u_axi_write.io.bus.ar <> AXI4AR.stub(ParameterConstants.dram_addr_width)
  u_axi_write.io.bus.r <> AXI4R.stub(ParameterConstants.dram_data_width)

  // sIdle
  val tt_pid_o = IO(Output(UInt(ParameterConstants.process_id_width.W)))
  tt_pid_o := qemu_miss_reply_i.bits.tag.process_id
  val tt_tid_i = IO(Input(new ThreadLookupResultPacket(param.threadNumber)))

  qemu_miss_reply_i.ready := state_r === sIdle
  val request_r = Reg(new QEMUMissReply)
  val ppn_r = RegInit(0.U(ParameterConstants.ppn_width.W))
  val tid_r = Reg(Valid(UInt(param.threadIDWidth().W)))
  when(qemu_miss_reply_i.fire()){
    request_r := qemu_miss_reply_i.bits
    tid_r.valid := tt_tid_i.hit_v
    tid_r.bits := tt_tid_i.thread_id
  }


  // sCheckAndLoadSynonym
  // Port for access Freelist
  val ppn_pop_i = IO(Flipped(Decoupled(UInt(ParameterConstants.ppn_width.W))))
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
  u_axi_read.io.xfer.address := Mux(
    state_r === sCheckAndLoadSynonym,
    ParameterConstants.getPageTableAddressByVPN(request_r.synonym_tag.vpn),
    ParameterConstants.getPageTableAddressByVPN(request_r.tag.vpn)
  )
  u_axi_read.io.xfer.length := u_buffer.requestPacketNumber.U
  u_axi_read.io.xfer.valid := Mux(
    state_r === sCheckAndLoadSynonym,
    request_r.synonym_v,
    state_r === sLoadSet
  )

  // sGetEvict
  val victim_r = Reg(new PageTableItem)
  val target_index_r = Reg(UInt(log2Ceil(u_buffer.entryNumber).W))
  when(state_r === sGetEvict){
    victim_r := u_buffer.lru_element_o.item
    target_index_r := u_buffer.lru_element_o.index
  }

  // sDeletePageReq
  // Judge by done_o
  val page_delete_req_o = IO(Decoupled(new PageTableItem))
  val page_delete_done_i = IO(Input(Bool()))
  page_delete_req_o.bits := victim_r
  page_delete_req_o.valid := state_r === sDeletePageReq

  // sReplace
  u_buffer.write_request_i.bits.index := target_index_r
  u_buffer.write_request_i.bits.item.tag := request_r.tag
  u_buffer.write_request_i.bits.item.entry.modified := false.B
  u_buffer.write_request_i.bits.item.entry.permission := request_r.permission
  u_buffer.write_request_i.bits.item.entry.ppn := ppn_r
  u_buffer.write_request_i.valid := state_r === sReplace

  // sInsertPage
  // TODO: RAW Hazard detected. You have to wait for the Page deletor to complete before inserting pages.
  // TODO: Reuse the AXI DMA.
  val page_insert_req_o = IO(Decoupled(UInt(ParameterConstants.ppn_width.W)))
  page_insert_req_o.bits := ppn_r
  page_insert_req_o.valid := state_r === sInsertPage
  val page_insert_done_i = IO(Input(Bool()))

  // sMoveback
  u_axi_write.io.xfer.address := ParameterConstants.getPageTableAddressByVPN(request_r.tag.vpn)
  u_axi_write.io.xfer.length := u_buffer.requestPacketNumber.U
  u_axi_write.io.xfer.valid := state_r === sMoveback

  // sReplyTLB
  val tlb_backend_reply_o = IO(Decoupled(new TLBBackendReplyPacket(param)))
  tlb_backend_reply_o.bits.tag.thread_id := tid_r.bits
  tlb_backend_reply_o.bits.tag.vpage := request_r.tag.vpn
  tlb_backend_reply_o.bits.data.modified := false.B
  tlb_backend_reply_o.bits.data.permission := request_r.permission
  tlb_backend_reply_o.bits.data.pp := ppn_r
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
        Mux(u_axi_read.io.xfer.done, sGetSynonym, sCheckAndLoadSynonym)
      )
    }
    is(sGetSynonym){
      state_r := sLoadSet
    }
    is(sLoadSet){
      state_r := Mux(u_axi_read.io.xfer.done, sGetEvict, sLoadSet)
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
        u_axi_write.io.xfer.done,
        Mux(tid_r.valid, sReplyToTLB, sIdle),
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
  println(c.emitVerilog(new QEMUMissReplyHandler(new TLBParameter)))
}

