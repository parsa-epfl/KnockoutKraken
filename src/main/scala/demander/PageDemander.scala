package armflex.demander

import chisel3._
import chisel3.util._

import armflex.cache.MemorySystemParameter
import DMAController.Bus._
import armflex.demander.software_bundle.ParameterConstants
import armflex.demander.peripheral.TLBMessageConverter
import armflex.demander.peripheral.ThreadTable
import armflex.demander.peripheral.QEMUMessageCompositor
import armflex.demander.peripheral.FreeList
import armflex.cache.TLBBackendReplyPacket
import armflex.demander.peripheral.PageDeletor
import armflex.demander.peripheral.PageInserter
import armflex.demander.peripheral.PageBuffer
import armflex.demander.peripheral.QEMUMessageReceiver
import armflex.demander.peripheral.QEMUMessageSender
import armflex.demander.peripheral.DRAMResster
import armflex.demander.software_bundle.PageTableItem


// TODO: Get the Page Demander work.
class PageDemander(
  param: MemorySystemParameter,
  messageFIFODepth: Int = 2,
  enableDRAMResetter: Boolean = true
) extends MultiIOModule {
  
  // TLB Message converter
  val u_itlb_mconv = Module(new TLBMessageConverter(param.toTLBParameter(), 2.U))
  val itlb_backend_request_i = IO(Flipped(u_itlb_mconv.tlb_backend_request_i.cloneType))
  u_itlb_mconv.tlb_backend_request_i.bits := itlb_backend_request_i.bits


  val u_dtlb_mconv = Module(new TLBMessageConverter(param.toTLBParameter(), 0.U))
  val dtlb_backend_request_i = IO(Flipped(u_dtlb_mconv.tlb_backend_request_i.cloneType))
  u_dtlb_mconv.tlb_backend_request_i.bits := dtlb_backend_request_i.bits

  // QEMU Message Receiver
  // TODO: Separate the message type here.
  val u_qemu_rx = Module(new QEMUMessageReceiver((x:UInt) => true.B))
  u_qemu_rx.S_AXI

  // DRAM Resetter.
  val M_AXI_RESET = IO(new AXI4(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))

  if(enableDRAMResetter){
    val u_dram_resetter = Module(new DRAMResster())
    u_dram_resetter.M_AXI <> M_AXI_RESET
    u_itlb_mconv.tlb_backend_request_i.valid := itlb_backend_request_i.valid && u_dram_resetter.ready_o
    itlb_backend_request_i.ready := u_itlb_mconv.tlb_backend_request_i.ready && u_dram_resetter.ready_o

    u_dtlb_mconv.tlb_backend_request_i.valid := dtlb_backend_request_i.valid && u_dram_resetter.ready_o
    dtlb_backend_request_i.ready := u_dtlb_mconv.tlb_backend_request_i.ready && u_dram_resetter.ready_o
  } else {
    u_itlb_mconv.tlb_backend_request_i.valid := itlb_backend_request_i.valid 
    itlb_backend_request_i.ready := u_itlb_mconv.tlb_backend_request_i.ready 
    u_dtlb_mconv.tlb_backend_request_i.valid := dtlb_backend_request_i.valid 
    dtlb_backend_request_i.ready := u_dtlb_mconv.tlb_backend_request_i.ready

    M_AXI_RESET.ar <> AXI4AR.stub(ParameterConstants.dram_addr_width)
    M_AXI_RESET.r <> AXI4R.stub(ParameterConstants.dram_data_width)
    M_AXI_RESET.aw <> AXI4AW.stub(ParameterConstants.dram_addr_width)
    M_AXI_RESET.w <> AXI4W.stub(ParameterConstants.dram_data_width)
    M_AXI_RESET.b <> AXI4B.stub()
  }

  // Hardware page walker
  // TODO: Page walker and TLB writer back handler should be merged into one module in order to keep the consistency.
  val u_page_walker = Module(new PageWalker(param.toTLBParameter(), 2))

  // Hardware page walker
  val M_AXI_PW = IO(new AXI4(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))
  u_page_walker.M_AXI <> M_AXI_PW // To DRAM. Read-only
  
  u_page_walker.tlb_miss_req_i(0) <> u_itlb_mconv.miss_request_o
  u_page_walker.tlb_miss_req_i(1) <> u_dtlb_mconv.miss_request_o

  // Thread table
  val u_tt = Module(new ThreadTable(param.threadNumber, ParameterConstants.process_id_width, 3, 2))

  val S_AXI_TT = IO(u_tt.S_AXI.cloneType)
  S_AXI_TT <> u_tt.S_AXI

  u_page_walker.tt_pid_i <> u_tt.pid_o(0)
  u_page_walker.tt_tid_o <> u_tt.tid_i(0)


  // TLB writeback handler
  val u_tlb_wb = Module(new TLBWritebackHandler(param.toTLBParameter(), 2))
  // AXI for write back handler
  val M_AXI_TLBWB = IO(u_tlb_wb.M_AXI.cloneType)
  u_tlb_wb.M_AXI <> M_AXI_TLBWB

  u_tlb_wb.tlb_evict_req_i(0) <> u_itlb_mconv.eviction_request_o
  u_tlb_wb.tlb_evict_req_i(1) <> u_dtlb_mconv.eviction_request_o

  u_tlb_wb.tt_pid_i <> u_tt.pid_o(1)
  u_tlb_wb.tt_tid_o <> u_tt.tid_i(1)

  // Miss request handler
  val u_qemu_miss = Module(new QEMUMissReplyHandler(param.toTLBParameter()))
  u_qemu_miss.tt_pid_o <> u_tt.pid_i(0)
  u_qemu_miss.tt_tid_i <> u_tt.tid_o(0)
  val M_AXI_QEMU_MISS = IO(u_qemu_miss.M_AXI.cloneType)
  M_AXI_QEMU_MISS <> u_qemu_miss.M_AXI

  // Page Evict Handler
  val u_qemu_page_evict = Module(new QEMUPageEvictHandler)
  val M_AXI_QEMU_PAGE_EVICT = IO(u_qemu_page_evict.M_AXI.cloneType)
  u_qemu_page_evict.M_AXI <> M_AXI_QEMU_PAGE_EVICT

  u_qemu_page_evict.evict_request_i <> u_qemu_rx.qemu_evict_page_req_o

  
  u_qemu_page_evict.page_delete_req_o

  // export TLB backend reply
  val itlb_backend_reply_o = IO(u_page_walker.tlb_backend_reply_o(0).cloneType)
  val u_itlb_backend_reply_arb = Module(new RRArbiter(new TLBBackendReplyPacket(param.toTLBParameter()), 2))
  itlb_backend_reply_o <> u_itlb_backend_reply_arb.io.out
  // 0: reply from the page walker
  u_itlb_backend_reply_arb.io.in(0) <> u_page_walker.tlb_backend_reply_o(0)
  // 1: reply from the page fault resolver
  u_itlb_backend_reply_arb.io.in(1).bits := u_qemu_miss.tlb_backend_reply_o.bits
  u_itlb_backend_reply_arb.io.in(1).valid := u_qemu_miss.tlb_backend_reply_o.valid && u_qemu_miss.tlb_backend_reply_o.bits.data.permission === 2.U

  val dtlb_backend_reply_o = IO(u_page_walker.tlb_backend_reply_o(1).cloneType)
  val u_dtlb_backend_reply_arb = Module(new RRArbiter(new TLBBackendReplyPacket(param.toTLBParameter()), 2))
  // 0: reply from the page walker
  dtlb_backend_reply_o <> u_dtlb_backend_reply_arb.io.out // u_page_walker.tlb_backend_reply_o(1)
  // 1: reply from the page fault resolver
  u_dtlb_backend_reply_arb.io.in(1).bits := u_qemu_miss.tlb_backend_reply_o.bits
  u_dtlb_backend_reply_arb.io.in(1).valid := u_qemu_miss.tlb_backend_reply_o.valid && u_qemu_miss.tlb_backend_reply_o.bits.data.permission =/= 2.U
  // ready signal of page fault resolver
  u_qemu_miss.tlb_backend_reply_o.ready := Mux(
    u_qemu_miss.tlb_backend_reply_o.bits.data.permission === 2.U, 
    u_itlb_backend_reply_arb.io.in(1).ready,
    u_dtlb_backend_reply_arb.io.in(1).ready
  )
  
  u_qemu_miss.qemu_miss_reply_i <> Queue(u_qemu_rx.qemu_miss_reply_o, messageFIFODepth)

  val M_AXI_PAGE = IO(new AXI4(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))

  // Page Deleter
  val u_page_deleter = Module(new PageDeletor(param))
  u_page_deleter.M_AXI.r <> M_AXI_PAGE.r
  u_page_deleter.M_AXI.ar <> M_AXI_PAGE.ar
  u_page_deleter.M_AXI.aw <> AXI4AW.stub(ParameterConstants.dram_addr_width)
  u_page_deleter.M_AXI.w <> AXI4W.stub(ParameterConstants.dram_data_width)
  u_page_deleter.M_AXI.b <> AXI4B.stub()
  u_page_deleter.dcache_flush_request_o
  u_page_deleter.dcache_wb_queue_empty_i

  u_qemu_miss.page_delete_done_i := u_page_deleter.done_o
  u_qemu_page_evict.page_delete_done_i := u_page_deleter.done_o

  u_page_deleter.icache_flush_request_o
  u_page_deleter.icache_wb_queue_empty_i

  val u_arb_page_delete_req = Module(new RRArbiter(new PageTableItem, 2))
  u_arb_page_delete_req.io.in(0) <> u_qemu_miss.page_delete_req_o
  u_arb_page_delete_req.io.in(0) <> u_qemu_page_evict.page_delete_req_o
  u_page_deleter.page_delete_req_i <> u_arb_page_delete_req.io.out
  
  u_page_deleter.tlb_flush_request_o
  u_page_deleter.tlb_frontend_reply_i

  u_page_deleter.tt_pid_o <> u_tt.pid_i(2)
  u_page_deleter.tt_tid_i <> u_tt.tid_o(2)


  // Page Inserter
  val u_page_inserter = Module(new PageInserter())
  u_page_inserter.M_AXI.aw <> M_AXI_PAGE.aw
  u_page_inserter.M_AXI.w <> M_AXI_PAGE.w
  u_page_inserter.M_AXI.b <> M_AXI_PAGE.b
  u_page_inserter.M_AXI.ar <> AXI4AR.stub(ParameterConstants.dram_addr_width)
  u_page_inserter.M_AXI.r <> AXI4R.stub(ParameterConstants.dram_data_width)
  u_page_inserter.done_o <> u_qemu_miss.page_insert_done_i
  u_page_inserter.req_i <> u_qemu_miss.page_insert_req_o

  // Page Buffer
  val u_page_buffer = Module(new PageBuffer())
  u_page_buffer.S_AXI
  u_page_buffer.normal_read_request_i <> u_page_inserter.read_request_o
  u_page_buffer.normal_read_reply_o <> u_page_inserter.read_reply_i
  u_page_buffer.normal_write_request_i <> u_page_deleter.page_buffer_write_o
  
  
  // the PA Pool
  val u_pool = Module(new FreeList(1 << (ParameterConstants.dram_addr_width - 12)))
  val M_AXI_PAPOOL = IO(u_pool.M_AXI.cloneType)
  M_AXI_PAPOOL <> u_pool.M_AXI

  u_pool.empty_o
  u_pool.full_o
  u_pool.pop_o <> u_qemu_miss.ppn_pop_i

  // QEMU eviction notifier
  val u_qemu_evict_reply = Module(new QEMUEvictReplyHandler())
  u_qemu_evict_reply.req_i <> Queue(u_qemu_rx.qemu_evict_reply_o, messageFIFODepth)
  u_qemu_evict_reply.free_o <> u_pool.push_i


  
  // QEMU message compositor
  val u_qmc = Module(new QEMUMessageCompositor(messageFIFODepth))
  u_qmc.evict_done_req_i <> u_page_deleter.done_message_o
  u_qmc.evict_notify_req_i <> u_page_deleter.start_message_o
  u_qmc.page_fault_req_i <> u_page_walker.page_fault_req_o

  // QEMU message sender
  val u_qemu_tx = Module(new QEMUMessageSender())
  val M_AXI_QEMUTX = IO(u_qemu_tx.M_AXI.cloneType)
  u_qemu_tx.M_AXI <> M_AXI_QEMUTX
  u_qemu_tx.fifo_i <> u_qmc.o
}