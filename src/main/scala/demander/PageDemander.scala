package armflex.demander

import chisel3._
import chisel3.util._

import armflex.cache.MemorySystemParameter
import DMAController.Bus._
import armflex.demander.software_bundle.ParameterConstants
import armflex.demander.peripheral.TLBMessageConverter
import armflex.demander.peripheral.ThreadTable
import armflex.demander.peripheral.FreeList
import armflex.cache.TLBBackendReplyPacket
import armflex.demander.peripheral.PageDeletor
import armflex.demander.peripheral.PageInserter
import armflex.demander.peripheral.PageBuffer
import armflex.demander.peripheral.QEMUMessageDecoder
import armflex.demander.peripheral.QEMUMessageEncoder
import armflex.demander.peripheral.DRAMResster
import armflex.demander.software_bundle.PageTableItem
import armflex.cache.TLBTagPacket
import armflex.util.AXIControlledMessageQueue
import armflex.util.AXIReadMasterIF
import armflex.util.AXIWriteMasterIF


/**
 * Top module of the page demander. 
 * 
 * @param param the parameter of the memory system for communication with TLB and cache.
 * @param messageFIFODepth the depth of all FIFOs that contains messages.
 * 
 * @note In the paper this module contains two parts: the MMU and page demander. In reality they're combined.
 */ 
class PageDemander(
  param: MemorySystemParameter,
  messageFIFODepth: Int = 2,
  enableDRAMResetter: Boolean = true
) extends MultiIOModule {
  // TODO: split this module to two part: the MMU and the real Page demander.
  // TODO: Combine all AXI slave and AXI lite slave internally.


  // AXI DMA Access ports
  val M_DMA_R = IO(Vec(6, new AXIReadMasterIF(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  )))

  val M_DMA_W = IO(Vec(4, new AXIWriteMasterIF(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  )))

  val reset_done = Wire(Bool()) // whether the reset process is done.
  reset_done := true.B

  // TLB Message receiver
  val u_itlb_mconv = Module(new TLBMessageConverter(param.toTLBParameter(), 2.U))
  val itlb_backend_request_i = IO(Flipped(u_itlb_mconv.tlb_backend_request_i.cloneType))
  u_itlb_mconv.tlb_backend_request_i.bits := itlb_backend_request_i.bits
  u_itlb_mconv.tlb_backend_request_i.valid := itlb_backend_request_i.valid && reset_done
  itlb_backend_request_i.ready := u_itlb_mconv.tlb_backend_request_i.ready && reset_done

  val u_dtlb_mconv = Module(new TLBMessageConverter(param.toTLBParameter(), 0.U))
  val dtlb_backend_request_i = IO(Flipped(u_dtlb_mconv.tlb_backend_request_i.cloneType))
  u_dtlb_mconv.tlb_backend_request_i.bits := dtlb_backend_request_i.bits
  u_dtlb_mconv.tlb_backend_request_i.valid := dtlb_backend_request_i.valid && reset_done
  dtlb_backend_request_i.ready := u_dtlb_mconv.tlb_backend_request_i.ready && reset_done

  // QEMU Message Decoder
  val u_qmd = Module(new QEMUMessageDecoder(messageFIFODepth))


  // Hardware page walker
  // TODO: Page walker and TLB writer back handler should be merged into one module in order to keep the consistency.
  val u_page_walker = Module(new PageWalker(param.toTLBParameter()))
  u_page_walker.M_DMA_R <> M_DMA_R(0)
  
  u_page_walker.tlb_miss_req_i(0) <> u_itlb_mconv.miss_request_o
  u_page_walker.tlb_miss_req_i(1) <> u_dtlb_mconv.miss_request_o

  // Thread table
  val u_tt = Module(new ThreadTable(param.threadNumber, ParameterConstants.process_id_width, 2, 2))

  val S_AXI_TT = IO(u_tt.S_AXI.cloneType)
  S_AXI_TT <> u_tt.S_AXI

  u_page_walker.tt_pid_i <> u_tt.pid_o(0)
  u_page_walker.tt_tid_o <> u_tt.tid_i(0)


  // TLB writeback handler
  val u_tlb_wb = Module(new TLBWritebackHandler(param.toTLBParameter(), 2))
  u_tlb_wb.M_DMA_R <> M_DMA_R(1)
  u_tlb_wb.M_DMA_W <> M_DMA_W(0)

  u_tlb_wb.tlb_evict_req_i(0) <> u_itlb_mconv.eviction_request_o
  u_tlb_wb.tlb_evict_req_i(1) <> u_dtlb_mconv.eviction_request_o

  u_tlb_wb.tt_pid_i <> u_tt.pid_o(1)
  u_tlb_wb.tt_tid_o <> u_tt.tid_i(1)

  // Miss request handler
  val u_qemu_miss = Module(new QEMUMissReplyHandler(param.toTLBParameter()))
  u_qemu_miss.M_DMA_R <> M_DMA_R(2)
  u_qemu_miss.M_DMA_W <> M_DMA_W(1)

  u_qemu_miss.tt_pid_o <> u_tt.pid_i(0)
  u_qemu_miss.tt_tid_i <> u_tt.tid_o(0)
  // Page Evict Handler
  val u_qemu_page_evict = Module(new QEMUPageEvictHandler)
  u_qemu_page_evict.M_DMA_R <> M_DMA_R(3)

  u_qemu_page_evict.evict_request_i <> u_qmd.qemu_evict_page_req_o

  // export TLB backend reply
  // two source: one from the page walk and one from the QEMU miss resolution
  val itlb_backend_reply_o = IO(Decoupled(new TLBBackendReplyPacket(param.toTLBParameter())))
  // FUCK: RRArbiter doesn't work in this scenario.
  val u_itlb_backend_reply_arb = Module(new Arbiter(new TLBBackendReplyPacket(param.toTLBParameter()), 2))
  itlb_backend_reply_o <> u_itlb_backend_reply_arb.io.out
  // 0: reply from the page walker
  u_itlb_backend_reply_arb.io.in(0) <> u_page_walker.tlb_backend_reply_o(0)
  // 1: reply from the page fault resolver
  u_itlb_backend_reply_arb.io.in(1).bits := u_qemu_miss.tlb_backend_reply_o.bits
  u_itlb_backend_reply_arb.io.in(1).valid := u_qemu_miss.tlb_backend_reply_o.valid && u_qemu_miss.tlb_backend_reply_o.bits.data.permission === 2.U

  val dtlb_backend_reply_o = IO(u_page_walker.tlb_backend_reply_o(1).cloneType)
  val u_dtlb_backend_reply_arb = Module(new Arbiter(new TLBBackendReplyPacket(param.toTLBParameter()), 2))
  dtlb_backend_reply_o <> u_dtlb_backend_reply_arb.io.out // u_page_walker.tlb_backend_reply_o(1)
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

  val u_arb_page_delete_req = Module(new Arbiter(new PageTableItem, 2))
  u_arb_page_delete_req.io.in(0) <> u_qemu_miss.page_delete_req_o
  u_arb_page_delete_req.io.in(1) <> u_qemu_page_evict.page_delete_req_o
  u_page_deleter.page_delete_req_i <> u_arb_page_delete_req.io.out

  val itlb_flush_request_o = IO(Decoupled(new TLBTagPacket(param.toTLBParameter())))
  itlb_flush_request_o.bits := u_page_deleter.tlb_flush_request_o.bits.req
  itlb_flush_request_o.valid := u_page_deleter.tlb_flush_request_o.valid && u_page_deleter.tlb_flush_request_o.bits.which === 0.U

  val dtlb_flush_request_o = IO(Decoupled(new TLBTagPacket(param.toTLBParameter())))
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

  u_page_deleter.tt_pid_o <> u_tt.pid_i(1)
  u_page_deleter.tt_tid_i <> u_tt.tid_o(1)


  // Page Inserter
  val u_page_inserter = Module(new PageInserter())
  u_page_inserter.M_DMA_W <> M_DMA_W(2)
  u_page_inserter.done_o <> u_qemu_miss.page_insert_done_i
  u_page_inserter.req_i <> u_qemu_miss.page_insert_req_o

  // Page Buffer
  val u_page_buffer = Module(new PageBuffer())
  val S_AXI_PAGE = IO(Flipped(u_page_buffer.S_AXI.cloneType))
  u_page_buffer.S_AXI <> S_AXI_PAGE
  u_page_buffer.normal_read_request_i <> u_page_inserter.read_request_o
  u_page_buffer.normal_read_reply_o <> u_page_inserter.read_reply_i
  u_page_buffer.normal_write_request_i <> u_page_deleter.page_buffer_write_o
  
  // the PA Pool
  val u_pool = Module(new FreeList)
  u_pool.M_DMA_R <> M_DMA_R(5)
  u_pool.M_DMA_W <> M_DMA_W(3)

  val pa_pool_empty_o = IO(Output(Bool()))
  pa_pool_empty_o := u_pool.empty_o
  val pa_pool_full_o = IO(Output(Bool()))
  pa_pool_full_o := u_pool.full_o
  u_pool.pop_o <> u_qemu_miss.ppn_pop_i

  // QEMU eviction notifier
  val u_qemu_evict_reply = Module(new QEMUEvictReplyHandler())
  u_qemu_evict_reply.req_i <> u_qmd.qemu_evict_reply_o
  u_qemu_evict_reply.free_o <> u_pool.push_i
  
  // QEMU message encoder
  val u_qme = Module(new QEMUMessageEncoder(messageFIFODepth))
  u_qme.evict_done_req_i <> u_page_deleter.done_message_o
  u_qme.evict_notify_req_i <> u_page_deleter.start_message_o
  u_qme.page_fault_req_i <> u_page_walker.page_fault_req_o

  // QEMU Message FIFO
  val u_qemu_mq = Module(new AXIControlledMessageQueue)
  val S_AXI_QEMU_MQ = IO(Flipped(u_qemu_mq.S_AXI.cloneType))
  u_qemu_mq.S_AXI <> S_AXI_QEMU_MQ
  val S_AXIL_QEMU_MQ = IO(u_qemu_mq.S_AXIL.cloneType)
  u_qemu_mq.S_AXIL <> S_AXIL_QEMU_MQ
  u_qemu_mq.fifo_i <> u_qme.o
  u_qmd.message_i.bits := u_qemu_mq.fifo_o.bits
  u_qmd.message_i.valid := u_qemu_mq.fifo_o.valid & reset_done
  u_qemu_mq.fifo_o.ready := u_qmd.message_i.ready & reset_done
  // For interrupt
  val qemu_message_available_o = IO(Output(Bool()))
  qemu_message_available_o := u_qme.o.valid
}

object PageDemanderVerilogEmitter extends App{
  val c = chisel3.stage.ChiselStage
  println(c.emitVerilog(new PageDemander(new MemorySystemParameter)))
}

