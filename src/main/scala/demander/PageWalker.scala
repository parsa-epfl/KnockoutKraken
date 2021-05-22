package armflex.demander

import armflex.{PTEntryPacket, PageFaultNotification}
import peripheral._
import chisel3._
import chisel3.util._
import armflex.cache._
import armflex.util._

/**
 * A hardware page walker. 
 * 
 * @param param the parameter of the MMU/Page demander. 
 */ 
class PageWalker(
  param: PageDemanderParameter,
  tlbNumber: Int = 2
) extends MultiIOModule {
  import antmicro.Bus._
  import antmicro.Frontend._
  import armflex.TLBMissRequestMessage

  // IO of TLB miss.
  val tlb_miss_req_i = IO(Vec(
    tlbNumber, Flipped(Decoupled(new TLBMissRequestMessage(param.mem.toTLBParameter())))
  ))

  val u_miss_arb = Module(new RRArbiter(new TLBMissRequestMessage(param.mem.toTLBParameter()), tlbNumber))
  u_miss_arb.io.in <> tlb_miss_req_i

  // val selected_miss_req = u_miss_arb.io.out

  // AXI DMA Read Channels
  val M_DMA_R = IO(new AXIReadMasterIF(
    param.dramAddrWidth,
    param.dramDataWidth
  ))

  // The Page table set buffer.
  val u_buffer = Module(new PageTableSetBuffer(
    param.mem.toTLBParameter, 
    new PageTableSetPacket(param.mem.toTLBParameter)
  ))
  u_buffer.dma_data_i <> M_DMA_R.data
  u_buffer.dma_data_o.ready := false.B
  // u_buffer.lru_element_i.valid := false.B
  // u_buffer.lru_element_i.bits := DontCare

  // Reply to TLB
  val tlb_backend_reply_o = IO(Vec(tlbNumber, Decoupled(new TLBBackendReplyPacket(param.mem.toTLBParameter()))))

  // Message sent to QEMU
  val page_fault_req_o = IO(Decoupled(new PageFaultNotification(param.mem.toTLBParameter())))

  // The state machine.
  val sIdle :: sMove :: sLookup :: sReply :: Nil = Enum(4)
  val state_r = RegInit(sIdle)

  u_miss_arb.io.out.ready := state_r === sIdle

  // The request packet
  class request_packet_t extends Bundle {
    val vpn = UInt(param.mem.vPageNumberWidth.W)
    val asid = UInt(param.mem.toTLBParameter().asidWidth.W)
    val permission = UInt(param.mem.toTLBParameter().permissionWidth.W)
    val source = UInt(log2Ceil(tlbNumber).W)
  }

  val request_r = Reg(new request_packet_t)
  when(u_miss_arb.io.out.fire()){
    request_r.permission := u_miss_arb.io.out.bits.permission
    request_r.asid := u_miss_arb.io.out.bits.tag.asid
    request_r.vpn := u_miss_arb.io.out.bits.tag.vpn
    request_r.source := u_miss_arb.io.chosen
  }

  val pte_r = Reg(Valid(new PTEntryPacket(param.mem.toTLBParameter())))
  when(state_r === sLookup){
    pte_r.bits := u_buffer.lookup_reply_o.item.entry
    pte_r.valid := u_buffer.lookup_reply_o.hit_v
  }

  // IO assignment of u_buffer.
  u_buffer.lookup_request_i.asid := request_r.asid
  u_buffer.lookup_request_i.vpn := request_r.vpn

  u_buffer.write_request_i.valid := false.B
  u_buffer.write_request_i.bits := DontCare
  

  // IO assignment of AXI Read DMA
  M_DMA_R.req.bits.address := param.getPageTableAddressByVPN(request_r.vpn)
  M_DMA_R.req.bits.length := u_buffer.requestPacketNumber.U
  M_DMA_R.req.valid := state_r === sMove

  // IO assignment of tlb_backend_reply_o
  for(i <- 0 until tlbNumber){
    tlb_backend_reply_o(i).bits.data.modified := pte_r.bits.modified
    tlb_backend_reply_o(i).bits.data.permission := pte_r.bits.permission
    tlb_backend_reply_o(i).bits.data.ppn := pte_r.bits.ppn
    tlb_backend_reply_o(i).bits.tag.asid := request_r.asid
    tlb_backend_reply_o(i).bits.tag.vpn := request_r.vpn
    tlb_backend_reply_o(i).valid := state_r === sReply && pte_r.valid && request_r.source === i.U
  }

  // assignment of page_fault_req_o
  page_fault_req_o.bits.permission := request_r.permission
  page_fault_req_o.bits.tag.asid := request_r.asid
  page_fault_req_o.bits.tag.vpn := request_r.vpn
  page_fault_req_o.valid := state_r === sReply && !pte_r.valid

  // state machine
  switch(state_r){
    is(sIdle){
      state_r := Mux(u_miss_arb.io.out.fire(), sMove, sIdle)
    }
    is(sMove){
      state_r := Mux(M_DMA_R.done, sLookup, sMove)
    }
    is(sLookup){
      state_r := sReply
    }
    is(sReply){
      state_r := Mux(
        VecInit(tlb_backend_reply_o.map(_.fire()).toSeq :+ page_fault_req_o.fire()).asUInt.orR(),
        sIdle, 
        sReply
      )
    }
  }
}


object PageWalkerVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new PageWalker(new PageDemanderParameter())))
}

