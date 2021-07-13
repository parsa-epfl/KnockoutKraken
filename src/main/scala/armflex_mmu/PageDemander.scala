package armflex_mmu

import chisel3._
import chisel3.util._
import armflex_cache.MemorySystemParameter
import armflex.{PTTagPacket, PageTableItem}
import armflex_mmu.peripheral._
import armflex_cache.TLBBackendReplyPacket
import armflex.util.AXIControlledMessageQueue
import armflex.util.AXIReadMasterIF
import armflex.util.AXIWriteMasterIF
import armflex.util.AXIInterconnector

class PageDemanderParameter(
  val mem: MemorySystemParameter = new MemorySystemParameter,
){
  def dramAddrWidth = mem.pAddressWidth
  def dramDataWidth = mem.cacheBlockSize


  def getPageTableAddressByVPN(vpn: UInt) = {
    def entryNumberInLog2 = mem.pAddressWidth - 12
    // val res = Wire()
    val pageset_number = vpn(entryNumberInLog2-1, 4)
    Cat(pageset_number * 3.U(2.W), 0.U(6.W))
  }
}


/**
 * Top module of the page demander. 
 * 
 * @param param the parameter of the memory system for communication with TLB and cache.
 * @param messageFIFODepth the depth of all FIFOs that contains messages.
 * 
 * @note In the paper this module contains two parts: the MMU and page demander. In reality they're combined.
 */ 
class PageDemander(
  param: PageDemanderParameter,
  messageFIFODepth: Int = 2,
) extends MultiIOModule {
  // TODO: split this module to two part: the MMU and the real Page demander.
  // TODO: Combine all AXI slave and AXI lite slave internally.


  // AXI DMA Access ports
  val M_DMA_R = IO(Vec(6, new AXIReadMasterIF(
    param.dramAddrWidth,
    param.dramDataWidth
  )))

  val M_DMA_W = IO(Vec(5, new AXIWriteMasterIF(
    param.dramAddrWidth,
    param.dramDataWidth
  )))

  // FIXME: Add a module to interact with multiple TLBs.

  // TLB Message receiver
  val u_itlb_mconv = Module(new TLBMessageConverter(param.mem.toTLBParameter))
  val itlb_backend_request_i = IO(Flipped(u_itlb_mconv.tlb_backend_request_i.cloneType))
  u_itlb_mconv.tlb_backend_request_i <> itlb_backend_request_i

  val u_dtlb_mconv = Module(new TLBMessageConverter(param.mem.toTLBParameter))
  val dtlb_backend_request_i = IO(Flipped(u_dtlb_mconv.tlb_backend_request_i.cloneType))
  u_dtlb_mconv.tlb_backend_request_i <> dtlb_backend_request_i

  // QEMU Message Decoder
  val u_qmd = Module(new QEMUMessageDecoder(param))

  // Hardware page walker
  // TODO: Page walker and TLB writer back handler should be merged into one module in order to keep the consistency.
  val u_page_walker = Module(new PageWalker(param))
  u_page_walker.M_DMA_R <> M_DMA_R(0)
  
  u_page_walker.tlb_miss_req_i(0) <> u_itlb_mconv.miss_request_o
  u_page_walker.tlb_miss_req_i(1) <> u_dtlb_mconv.miss_request_o

  // TLB writeback handler
  val u_tlb_wb = Module(new TLBWritebackHandler(param, 2))
  u_tlb_wb.M_DMA_R <> M_DMA_R(1)
  u_tlb_wb.M_DMA_W <> M_DMA_W(0)

  u_tlb_wb.tlb_evict_req_i(0) <> u_itlb_mconv.eviction_request_o
  u_tlb_wb.tlb_evict_req_i(1) <> u_dtlb_mconv.eviction_request_o

  // Miss request handler
  val u_qemu_miss = Module(new QEMUMissReplyHandler(param))
  u_qemu_miss.M_DMA_R <> M_DMA_R(2)
  u_qemu_miss.M_DMA_W <> M_DMA_W(1)

  // Page Evict Handler
  val u_qemu_page_evict = Module(new QEMUPageEvictHandler(param))
  u_qemu_page_evict.M_DMA_R <> M_DMA_R(3)
  u_qemu_page_evict.M_DMA_W <> M_DMA_W(2)

  u_qemu_page_evict.evict_request_i <> u_qmd.qemu_evict_page_req_o

  // export TLB backend reply
  // two source: one from the page walk and one from the QEMU miss resolution
  val itlb_backend_reply_o = IO(Decoupled(new TLBBackendReplyPacket(param.mem.toTLBParameter)))
  // FIXME: Here I cannot use RRArbiter and I don't know why.
  val u_itlb_backend_reply_arb = Module(new Arbiter(new TLBBackendReplyPacket(param.mem.toTLBParameter), 2))
  itlb_backend_reply_o <> u_itlb_backend_reply_arb.io.out
  // 0: reply from the page walker
  u_itlb_backend_reply_arb.io.in(0) <> u_page_walker.tlb_backend_reply_o(0)
  // 1: reply from the page fault resolver
  u_itlb_backend_reply_arb.io.in(1).bits := u_qemu_miss.tlb_backend_reply_o.bits
  u_itlb_backend_reply_arb.io.in(1).valid := u_qemu_miss.tlb_backend_reply_o.valid && u_qemu_miss.tlb_backend_reply_o.bits.data.permission === 2.U

  val dtlb_backend_reply_o = IO(u_page_walker.tlb_backend_reply_o(1).cloneType)
  // FIXME: Here I cannot use RRArbiter and I don't know why.
  val u_dtlb_backend_reply_arb = Module(new Arbiter(new TLBBackendReplyPacket(param.mem.toTLBParameter), 2))
  dtlb_backend_reply_o <> u_dtlb_backend_reply_arb.io.out
  // 0: reply from the page walker
  u_dtlb_backend_reply_arb.io.in(0) <> u_page_walker.tlb_backend_reply_o(1)
  // 1: reply from the page fault resolver
  u_dtlb_backend_reply_arb.io.in(1).bits := u_qemu_miss.tlb_backend_reply_o.bits
  u_dtlb_backend_reply_arb.io.in(1).valid := u_qemu_miss.tlb_backend_reply_o.valid && u_qemu_miss.tlb_backend_reply_o.bits.data.permission =/= 2.U
  // ready signal of page fault resolver
  u_qemu_miss.tlb_backend_reply_o.ready := Mux(
    u_qemu_miss.tlb_backend_reply_o.bits.data.permission === 2.U,
    u_itlb_backend_reply_arb.io.in(1).ready,
    u_dtlb_backend_reply_arb.io.in(1).ready
  )
  // u_qemu_miss.tlb_backend_reply_o.ready := false.B
  
  u_qemu_miss.qemu_miss_reply_i <> u_qmd.qemu_miss_reply_o

  // Page Deleter
  val u_page_deleter = Module(new PageDeletor(param))
  u_page_deleter.M_DMA_R <> M_DMA_R(4)

  val dcache_flush_request_o = IO(u_page_deleter.dcache_flush_request_o.cloneType)
  u_page_deleter.dcache_flush_request_o <> dcache_flush_request_o
  val dcache_wb_queue_empty_i = IO(Input(Bool()))
  u_page_deleter.dcache_wb_queue_empty_i <> dcache_wb_queue_empty_i
  val dcache_stall_request_vo = IO(Output(Bool()))
  dcache_stall_request_vo := u_page_deleter.stall_dcache_vo

  u_qemu_miss.page_delete_done_i := u_page_deleter.done_o
  u_qemu_page_evict.page_delete_done_i := u_page_deleter.done_o

  val icache_flush_request_o = IO(u_page_deleter.icache_flush_request_o.cloneType)
  u_page_deleter.icache_flush_request_o <> icache_flush_request_o
  val icache_wb_queue_empty_i = IO(Input(Bool()))
  u_page_deleter.icache_wb_queue_empty_i <> icache_wb_queue_empty_i
  val icache_stall_request_vo = IO(Output(Bool()))
  icache_stall_request_vo := u_page_deleter.stall_icache_vo

  // FIXME: Here I cannot even use Arbiter and I don't know why. Temporary I use a manual Arbiter to solve the problem...
//  val u_arb_page_delete_req = Module(new Arbiter(new PageTableItem(param.mem.toTLBParameter()), 2))
//  u_arb_page_delete_req.io.in(0) <> u_qemu_miss.page_delete_req_o
//  u_arb_page_delete_req.io.in(1) <> u_qemu_page_evict.page_delete_req_o
  u_page_deleter.page_delete_req_i.bits := Mux(
    u_qemu_miss.page_delete_req_o.valid,
    u_qemu_miss.page_delete_req_o.bits,
    u_qemu_page_evict.page_delete_req_o.bits
  )

  u_page_deleter.page_delete_req_i.valid := u_qemu_miss.page_delete_req_o.valid || u_qemu_page_evict.page_delete_req_o.valid
  u_qemu_miss.page_delete_req_o.ready := u_page_deleter.page_delete_req_i.ready
  u_qemu_page_evict.page_delete_req_o.ready := u_page_deleter.page_delete_req_i.ready && !u_qemu_miss.page_delete_req_o.valid

   val itlb_flush_request_o = IO(Decoupled(new PTTagPacket(param.mem.toTLBParameter)))
   itlb_flush_request_o.bits := u_page_deleter.tlb_flush_request_o.bits.req
   itlb_flush_request_o.valid := u_page_deleter.tlb_flush_request_o.valid && u_page_deleter.tlb_flush_request_o.bits.which === 0.U

  val dtlb_flush_request_o = IO(Decoupled(new PTTagPacket(param.mem.toTLBParameter)))
  dtlb_flush_request_o.bits := u_page_deleter.tlb_flush_request_o.bits.req
  dtlb_flush_request_o.valid := u_page_deleter.tlb_flush_request_o.valid && u_page_deleter.tlb_flush_request_o.bits.which === 1.U

  u_page_deleter.tlb_flush_request_o.ready := Mux(
    u_page_deleter.tlb_flush_request_o.bits.which === 0.U,
    itlb_flush_request_o.ready,
    dtlb_flush_request_o.ready
  )

  val itlb_flush_reply_i = IO(Flipped(u_page_deleter.tlb_frontend_reply_i.cloneType))
  val dtlb_flush_reply_i = IO(Flipped(u_page_deleter.tlb_frontend_reply_i.cloneType))

  u_page_deleter.tlb_frontend_reply_i := Mux(
    u_page_deleter.tlb_flush_request_o.bits.which === 0.U,
    itlb_flush_reply_i,
    dtlb_flush_reply_i
  )

  // Page Inserter
  val u_page_inserter = Module(new PageInserter(param))
  u_page_inserter.M_DMA_W <> M_DMA_W(3)
  u_page_inserter.done_o <> u_qemu_miss.page_insert_done_i
  u_page_inserter.req_i <> u_qemu_miss.page_insert_req_o

  // Page Buffer
  val u_page_buffer = Module(new PageBuffer())
  // val S_AXI_PAGE = IO(Flipped(u_page_buffer.S_AXI.cloneType))
  // u_page_buffer.S_AXI <> S_AXI_PAGE
  u_page_buffer.normal_read_request_i <> u_page_inserter.read_request_o
  u_page_buffer.normal_read_reply_o <> u_page_inserter.read_reply_i
  u_page_buffer.normal_write_request_i <> u_page_deleter.page_buffer_write_o
  
  // the PA Pool
  val u_pool = Module(new FreeList(param))
  u_pool.M_DMA_R <> M_DMA_R(5)
  u_pool.M_DMA_W <> M_DMA_W(4)

  val pa_pool_empty_o = IO(Output(Bool()))
  pa_pool_empty_o := u_pool.empty_o
  val pa_pool_full_o = IO(Output(Bool()))
  pa_pool_full_o := u_pool.full_o
  u_pool.pop_o <> u_qemu_miss.ppn_pop_i

  // QEMU eviction notifier
  val u_qemu_evict_reply = Module(new QEMUEvictReplyHandler(param.mem.toTLBParameter))
  u_qemu_evict_reply.req_i <> u_qmd.qemu_evict_reply_o
  u_qemu_evict_reply.free_o <> u_pool.push_i
  
  // QEMU message encoder
  val u_qme = Module(new QEMUMessageEncoder(param.mem.toTLBParameter, messageFIFODepth))
  u_qme.evict_done_req_i <> u_page_deleter.done_message_o
  u_qme.evict_notify_req_i <> u_page_deleter.start_message_o
  u_qme.page_fault_req_i <> u_page_walker.page_fault_req_o

  // QEMU Message FIFO
  val u_qemu_mq = Module(new AXIControlledMessageQueue)
  // val S_AXI_QEMU_MQ = IO(Flipped(u_qemu_mq.S_AXI.cloneType))
  // u_qemu_mq.S_AXI <> S_AXI_QEMU_MQ
  val S_AXIL_QEMU_MQ = IO(Flipped(u_qemu_mq.S_AXIL.cloneType))
  u_qemu_mq.S_AXIL <> S_AXIL_QEMU_MQ
  u_qemu_mq.fifo_i <> u_qme.o
  u_qmd.message_i <> Queue(u_qemu_mq.fifo_o, 1)
  // For interrupt
  val qemu_message_available_o = IO(Output(Bool()))
  qemu_message_available_o := u_qme.o.valid

  // AXI Slave interconnector
  val u_s_axi_int = Module(new AXIInterconnector(
    Seq(0x0000, 0x8000), Seq(0x8000, 0x8000), 16, 512
  ))

  val S_AXI = IO(Flipped(u_s_axi_int.S_AXI.cloneType))
  S_AXI <> u_s_axi_int.S_AXI

  u_s_axi_int.M_AXI(0) <> u_page_buffer.S_AXI // 0x0000 - 0x8000: Page
  u_s_axi_int.M_AXI(1) <> u_qemu_mq.S_AXI // 0x8000 - 0xF000: message
  

}

object PageDemanderVerilogEmitter extends App{
  val c = chisel3.stage.ChiselStage
  println(c.emitVerilog(new PageDemander(new PageDemanderParameter())));
}

