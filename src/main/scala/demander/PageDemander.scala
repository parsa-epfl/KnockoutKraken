package armflex.demander

import chisel3._
import chisel3.util._

import armflex.cache.MemorySystemParameter
import DMAController.Bus._
import armflex.demander.software_bundle.ParameterConstants

class PageDemander(
  param: MemorySystemParameter,
  instructionFile: String = ""
) extends MultiIOModule {
  // The address map of the functional units.
  // The core
  val u_core = Module(new mini.Core()(new mini.MiniConfig()))
  u_core.io.host.fromhost := DontCare

  // The instruction buffer
  val u_ibuffer = Module(new peripheral.SyncMemory(1 << 14)(instructionFile))
  u_ibuffer.request_i.bits.addr := u_core.io.icache.req.bits.addr
  u_ibuffer.request_i.bits.data := 0.U
  u_ibuffer.request_i.bits.w_mask := 0.U
  u_ibuffer.request_i.bits.w_v := false.B
  u_ibuffer.request_i.valid := u_core.io.icache.req.valid

  u_core.io.icache.resp.bits.data := u_ibuffer.reply_o
  u_core.io.icache.resp.valid := true.B

  // The bridge
  val u_bus = Module(new peripheral.MemoryInterconnector(
    addresses = Seq.tabulate(9)({ i =>
      (i + 1) << 16
    }),
    masks = Seq.fill(9)(0xFFFF0000),
  ));  

  u_bus.master_request_i.bits.addr := u_core.io.dcache.req.bits.addr
  u_bus.master_request_i.bits.data := u_core.io.dcache.req.bits.data
  u_bus.master_request_i.bits.w_mask := u_core.io.dcache.req.bits.mask
  u_bus.master_request_i.bits.w_v := u_core.io.dcache.req.bits.mask =/= 0.U
  u_bus.master_request_i.valid := u_core.io.dcache.req.valid

  u_core.io.dcache.resp.bits.data := u_bus.master_reply_o.bits
  u_core.io.dcache.resp.valid := u_bus.master_reply_o.valid

  // The functional units

  // The data buffer
  val u_dbuffer = Module(new peripheral.SyncMemory(1 << 14)())
  u_dbuffer.request_i <> u_bus.slave_requests_o(0)
  u_dbuffer.reply_o <> u_bus.slave_replies_i(0)

  // The message queue
  // fetchMessage
  val u_r_mmq = Module(new peripheral.MMReadQueue(new software_bundle.PageDemanderMessage))
  u_r_mmq.request_i <> u_bus.slave_requests_o(1)
  u_r_mmq.reply_o <> u_bus.slave_replies_i(1)

  // The thread table (with parameter)
  // lookupThreadTable
  val u_thread_table = Module(new peripheral.ThreadTable(
    param.threadNumber,
    ParameterConstants.process_id_width
  ))
  u_thread_table.request_i <> u_bus.slave_requests_o(2)
  u_thread_table.reply_o <> u_bus.slave_replies_i(2)
  
  val S_AXI_Threadtable = IO(u_thread_table.S_AXI.cloneType)
  u_thread_table.S_AXI <> S_AXI_Threadtable

  // TThe page table set manager
  // loadPTSet
  // lookupPT
  // getLRU
  // replaceLRU
  // syncPTSet
  val u_ptset = Module(new peripheral.PTSetCache())
  u_ptset.reply_o <> u_bus.slave_replies_i(3)
  u_ptset.request_i <> u_bus.slave_requests_o(3)
  val M_AXI_PTSet = IO(u_ptset.M_AXI.cloneType) // DRAM(R/W)
  u_ptset.M_AXI <> M_AXI_PTSet

  // The TLB wrapper
  // responseToTLB
  // flushTLBEntry
  // TODO: Where does the parameter comes from?
  val u_tlb_wrapper = Module(new peripheral.TLBFlushController(
    16, param.toTLBParameter()
  ))

  u_tlb_wrapper.lookup_process_id_o <> u_thread_table.lookup_request_i
  u_tlb_wrapper.lookup_thread_id_i <> u_thread_table.loopup_reply_o

  u_tlb_wrapper.reply_o <> u_bus.slave_replies_i(4)
  u_tlb_wrapper.request_i <> u_bus.slave_requests_o(4)

  val tlb_backend_reply_o = IO(u_tlb_wrapper.tlb_backend_reply_o.cloneType)
  u_tlb_wrapper.tlb_backend_reply_o <> tlb_backend_reply_o
  val tlb_flush_request_o = IO(u_tlb_wrapper.tlb_flush_request_o.cloneType)
  u_tlb_wrapper.tlb_flush_request_o <> tlb_flush_request_o
  val tlb_frontend_reply_i = IO(u_tlb_wrapper.tlb_frontend_reply_i.cloneType)
  u_tlb_wrapper.tlb_frontend_reply_i <> tlb_frontend_reply_i


  // Free PA list
  // getFreePPN
  // recyclePPN
  val u_freelist = Module(new peripheral.FreeListWrapper(
    1 << 24
  ))

  val M_AXI_FL = IO(u_freelist.M_AXI.cloneType)
  u_freelist.M_AXI <> M_AXI_FL
  val freelist_empty_vo = IO(Output(u_freelist.empty_vo.cloneType))
  u_freelist.empty_vo <> freelist_empty_vo
  val freelist_full_vo = IO(Output(u_freelist.full_vo.cloneType))
  u_freelist.full_vo <> freelist_full_vo
  u_freelist.reply_o <> u_bus.slave_replies_i(5)
  u_freelist.request_i <> u_bus.slave_requests_o(5)

  // sendMissRequestToQEMU
  val u_miss_to_qemu = Module(new peripheral.MMWriteQueue(new software_bundle.PageFaultRequest()))

  u_miss_to_qemu.request_i <> u_bus.slave_requests_o(6)
  u_miss_to_qemu.reply_o <> u_bus.slave_replies_i(6)

  // Page deletor
  // movePageToQEMU
  val u_page_deletor = Module(new peripheral.PageDeletor(
    param.toCacheParameter()
  ))

  u_bus.slave_requests_o(7) <> u_page_deletor.request_i
  u_bus.slave_replies_i(7) <> u_page_deletor.reply_o

  val M_AXI_Page = IO(u_page_deletor.M_AXI.cloneType)
  // u_page_deletor.M_AXI // (DRAM: R)
  u_page_deletor.M_AXI.aw <> AXI4AW.stub(36)
  u_page_deletor.M_AXI.w <> AXI4W.stub(512)
  u_page_deletor.M_AXI.b <> AXI4B.stub()

  u_page_deletor.M_AXI.ar <> M_AXI_Page.ar
  u_page_deletor.M_AXI.r <> M_AXI_Page.r

  val dcache_flush_request_o = IO(u_page_deletor.dcache_flush_request_o.cloneType)
  u_page_deletor.dcache_flush_request_o <> dcache_flush_request_o

  val dcache_wb_queue_empty_i = IO(Input(u_page_deletor.dcache_wb_queue_empty_i.cloneType))
  u_page_deletor.dcache_wb_queue_empty_i <> dcache_wb_queue_empty_i

  val icache_flush_request_o = IO(u_page_deletor.icache_flush_request_o.cloneType)
  u_page_deletor.icache_flush_request_o <> icache_flush_request_o

  val icache_wb_queue_empty_i = IO(Input(u_page_deletor.icache_wb_queue_empty_i.cloneType))
  u_page_deletor.icache_wb_queue_empty_i <> icache_wb_queue_empty_i
  

  // Page Inserter
  // insertPageFromQEMU
  val u_page_inserter = Module(new peripheral.PageInserter)

  // u_page_inserter.M_AXI // to DRAM (W)
  u_page_inserter.M_AXI.ar <> AXI4AR.stub(36)
  u_page_inserter.M_AXI.r <> AXI4R.stub(512)
  u_page_inserter.M_AXI.aw <> M_AXI_Page.aw
  u_page_inserter.M_AXI.w <> M_AXI_Page.w
  u_page_inserter.M_AXI.b <> M_AXI_Page.b

  u_page_inserter.reply_o <> u_bus.slave_replies_i(8)
  u_page_inserter.request_i <> u_bus.slave_requests_o(8)

  // The page buffer
  val u_page_buffer = Module(new peripheral.PageBuffer)
  val S_AXI_PageBuffer = IO(Flipped(u_page_buffer.S_AXI.cloneType))
  u_page_buffer.S_AXI <> S_AXI_PageBuffer // From QEMU (RW)
  u_page_buffer.normal_read_reply_o <> u_page_inserter.read_reply_i
  u_page_buffer.normal_read_request_i <> u_page_inserter.read_request_o
  u_page_buffer.normal_write_request_i <> u_page_deletor.page_buffer_write_o

  // QEMU Message complex
  val u_qemu_message_compositor = Module(new QEMUMessageCompositor())
  u_qemu_message_compositor.evict_done_req_i <> u_page_deletor.done_message_o
  u_qemu_message_compositor.evict_notify_req_i <> u_page_deletor.start_message_o
  u_qemu_message_compositor.page_fault_req_i <> u_miss_to_qemu.queue_o 

  
  val u_qemu_message_sender = Module(new peripheral.QEMUMessageSender())
  val M_AXI_QEMU_Message = IO(u_qemu_message_sender.M_AXI.cloneType)
  // u_qemu_message_sender.M_AXI // To QEMU (W)
  u_qemu_message_sender.M_AXI <> M_AXI_QEMU_Message
  u_qemu_message_sender.fifo_i <> u_qemu_message_compositor.o

  // Demander Message complex
  val u_demander_message_complex = Module(new DemanderMessageCompositor())
  
  u_demander_message_complex.o <> u_r_mmq.queue_i

  val u_itlb_message_convert = Module(new peripheral.TLBMessageConverter(param.toTLBParameter(), 2.U))
  u_itlb_message_convert.eviction_request_o <> u_demander_message_complex.itlb_evict_request_i
  u_itlb_message_convert.miss_request_o <> u_demander_message_complex.itlb_miss_request_i
  val itlb_backend_request_i = IO(Flipped(u_itlb_message_convert.tlb_backend_request_i.cloneType))
  u_itlb_message_convert.tlb_backend_request_i <> itlb_backend_request_i

  val u_dtlb_message_convert = Module(new peripheral.TLBMessageConverter(param.toTLBParameter(), 0.U))
  u_dtlb_message_convert.eviction_request_o <> u_demander_message_complex.dtlb_evict_request_i
  u_dtlb_message_convert.miss_request_o <> u_demander_message_complex.dtlb_miss_request_i
  val dtlb_backend_request_i = IO(Flipped(u_dtlb_message_convert.tlb_backend_request_i.cloneType))
  u_dtlb_message_convert.tlb_backend_request_i <> dtlb_backend_request_i

  val u_qemu_to_demander_message_conveter = Module(new peripheral.QEMUMessageConverter())
  u_demander_message_complex.qemu_miss_reply_i <> u_qemu_to_demander_message_conveter.miss_reply_o 
  u_demander_message_complex.qemu_evict_reply_i <> u_qemu_to_demander_message_conveter.evict_reply_o

  val u_qemu_message_receiver = Module(new peripheral.QEMUMessageReceiver({
    addr => true.B // TODO: Replace with correct function.
  }))
  val S_AXI_QEMU_Message = IO(Flipped(u_qemu_message_receiver.S_AXI.cloneType))
  u_qemu_message_receiver.S_AXI <> S_AXI_QEMU_Message // From QEMU (R)

  u_qemu_message_receiver.fifo_o <> u_qemu_to_demander_message_conveter.i


  // TODO: DRAM Reset

}

