package armflex.demander

import peripheral._

import chisel3._
import chisel3.util._
import armflex.cache._
import armflex.demander.software_bundle.ParameterConstants
import armflex.demander.software_bundle.PTEntry

/**
 * A hardware page walker. 
 * 
 * @param param the parameter of the TLB. 
 */ 
class PageWalker(
  param: TLBParameter,
  tlbNumber: Int = 2
) extends MultiIOModule {
  import software_bundle.TLBMissRequestMessage
  import software_bundle.PageFaultRequest
  import DMAController.Bus._
  import DMAController.Frontend._

  // IO of TLB miss.
  val tlb_miss_req_i = IO(Vec(
    tlbNumber, Flipped(Decoupled(new TLBMissRequestMessage))
  ))

  val u_miss_arb = Module(new RRArbiter(new TLBMissRequestMessage, tlbNumber))
  u_miss_arb.io.in <> tlb_miss_req_i

  // val selected_miss_req = u_miss_arb.io.out

  // lookup process id. Converting Thread ID to Process ID. TT means thread table.
  val tt_tid_o = IO(Output(UInt(param.threadIDWidth().W)))
  tt_tid_o := u_miss_arb.io.out.bits.tag.thread_id
  val tt_pid_i = IO(Flipped(Valid((UInt(ParameterConstants.process_id_width.W)))))

  // Activate signal to the Page 
  val u_axi_reader = Module(new AXI4Reader(
    ParameterConstants.dram_addr_width, 
    ParameterConstants.dram_data_width
  ))

  val M_AXI = IO(u_axi_reader.io.bus.cloneType)
  M_AXI <> u_axi_reader.io.bus
  // The Page table set buffer.
  val u_buffer = Module(new PageTableSetBuffer(new PTSetPacket()))
  u_buffer.dma_data_i <> u_axi_reader.io.dataOut
  u_buffer.dma_data_o.ready := false.B
  u_buffer.store_enable_vi := false.B
  // u_buffer.lru_element_i.valid := false.B
  // u_buffer.lru_element_i.bits := DontCare

  // Reply to TLB
  val tlb_backend_reply_o = IO(Vec(tlbNumber, Decoupled(new TLBBackendReplyPacket(param))))

  // Message sent to QEMU
  val page_fault_req_o = IO(Decoupled(new PageFaultRequest))

  // The state machine.
  val sIdle :: sMove :: sLookup :: sReply :: Nil = Enum(4)
  val state_r = RegInit(sIdle)

  u_miss_arb.io.out.ready := state_r === sIdle

  // The request packet
  class request_packet_t extends Bundle {
    val vpn = UInt(param.vPageWidth.W)
    val process_id = UInt(ParameterConstants.process_id_width.W)
    val thread_id = UInt(param.threadIDWidth().W)
    val permission = UInt(ParameterConstants.permission_bit_width.W)
    val source = UInt(log2Ceil(tlbNumber).W)
  }

  val request_r = Reg(new request_packet_t)
  when(u_miss_arb.io.out.fire()){
    request_r.permission := u_miss_arb.io.out.bits.permission
    request_r.process_id := tt_pid_i.bits
    request_r.thread_id := u_miss_arb.io.out.bits.tag.thread_id
    request_r.vpn := u_miss_arb.io.out.bits.tag.vpn
    request_r.source := u_miss_arb.io.chosen
  }

  val pte_r = Reg(Valid(new PTEntry))
  when(state_r === sLookup){
    pte_r.bits := u_buffer.lookup_reply_o.item.entry
    pte_r.valid := u_buffer.lookup_reply_o.hit_v
  }

  // IO assignment of u_buffer.
  u_buffer.load_enabled_vi := u_miss_arb.io.out.fire()
  u_buffer.lookup_request_i.process_id := request_r.process_id
  u_buffer.lookup_request_i.vpn := request_r.vpn

  u_buffer.write_request_i.valid := false.B
  u_buffer.write_request_i.bits := DontCare
  

  // IO assignment of AXI Read DMA
  u_axi_reader.io.xfer.address := ParameterConstants.getPageTableAddressByVPN(request_r.vpn)
  u_axi_reader.io.xfer.length := u_buffer.requestPacketNumber.U
  u_axi_reader.io.xfer.valid := state_r === sMove

  // IO assignment of tlb_backend_reply_o
  for(i <- 0 until tlbNumber){
    tlb_backend_reply_o(i).bits.data.modified := pte_r.bits.modified
    tlb_backend_reply_o(i).bits.data.permission := pte_r.bits.permission
    tlb_backend_reply_o(i).bits.data.pp := pte_r.bits.ppn
    tlb_backend_reply_o(i).bits.tag.thread_id := request_r.thread_id
    tlb_backend_reply_o(i).bits.tag.vpage := request_r.vpn
    tlb_backend_reply_o(i).valid := state_r === sReply && pte_r.valid && request_r.source === i.U
  }

  // assignment of page_fault_req_o
  page_fault_req_o.bits.permission := pte_r.bits.permission
  page_fault_req_o.bits.tag.process_id := request_r.process_id
  page_fault_req_o.bits.tag.vpn := request_r.vpn
  page_fault_req_o.valid := state_r === sReply && !pte_r.valid

  // state machine
  switch(state_r){
    is(sIdle){
      state_r := Mux(u_miss_arb.io.out.fire(), sMove, sIdle)
    }
    is(sMove){
      state_r := Mux(u_axi_reader.io.xfer.done, sLookup, sMove)
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
  println(c.emitVerilog(new PageWalker(new TLBParameter)))
}

