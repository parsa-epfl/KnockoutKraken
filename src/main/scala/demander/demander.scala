package armflex.demander

import chisel3._
import chisel3.util._

import armflex.cache.MemorySystemParameter

class PageDemander(
  param: MemorySystemParameter,
  instructionFile: String = ""
) extends MultiIOModule {
  // The address map of the functional units.
  /**
   * Address Range | Peripheral
   * 0x00000 - 0x10000    | Instruction Buffer
   * 0x10000 - 0x20000    | Data Buffer (stack)
   * 0x20000 - 0x30000    | Message Queue
   * 0x30000 - 0x40000    | Truth Table
   * 0x40000 - 0x50000    | Page Table Set manager
   * 0x50000 - 0x60000    | TLB wrapper
   * 0x60000 - 0x70000    | Free List
   * 0x70000 - 0x80000    | QEMU Queue
   * 0x80000 - 0x90000    | Page Deletor
   * 
   */ 
  // The core
  val u_core = Module(new mini.Core()(new mini.MiniConfig()))

  // The instruction buffer
  val u_ibuffer = Module(new peripheral.SyncReadMemory(1 << 14)(instructionFile))
  u_ibuffer.request_i.bits.addr := u_core.io.icache.req.bits.addr
  u_ibuffer.request_i.bits.data := DontCare
  u_ibuffer.request_i.bits.w_mask := DontCare
  u_ibuffer.request_i.bits.w_v := false.B
  u_ibuffer.request_i.valid := u_core.io.icache.req.valid

  u_core.io.icache.resp.bits := u_ibuffer.reply_o
  u_core.io.icache.resp.valid := RegNext(u_core.io.icache.req.valid)

  // The bridge
  val u_bus = Module(new peripheral.MemoryInterconnector(
    addresses = Seq.tabulate(8)({ i =>
      (i + 1) << 16
    }),
    masks = Seq.fill(8)(0xFFFF0000),
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
  val u_dbuffer = Module(new peripheral.SyncReadMemory(1 << 14)())
  u_dbuffer.request_i <> u_bus.slave_requests_o(0)
  u_dbuffer.reply_o <> u_bus.slave_replies_i(0)

  // The message queue
  // fetchMessage
  val u_r_mmq = Module(new peripheral.MMReadQueue(new software_bundle.PageDemanderMessage))
  u_r_mmq.request_i <> u_bus.slave_requests_o(1)
  u_r_mmq.reply_o <> u_bus.slave_replies_i(1)

  // The truth table (with parameter)
  // lookupThreadTable
  val u_truth_table = Module(new peripheral.ThreadTable())
  u_truth_table.request_i <> u_bus.slave_requests_o(2)
  u_truth_table.reply_o <> u_bus.slave_replies_i(2)

  u_truth_table.S_AXI // from QEMU (lite) (R/W)

  // TThe page table set manager
  // loadPTSet
  // lookupPT
  // getLRU
  // replaceLRU
  // syncPTSet
  val u_ptset = Module(new peripheral.PTSetCache())
  u_ptset.reply_o <> u_bus.slave_replies_i(3)
  u_ptset.request_i <> u_bus.slave_requests_o(3)
  u_ptset.M_AXI // to DRAM (R/W)

  // The TLB wrapper
  // responseToTLB
  // flushTLBEntry
  // TODO: Where does the parameter comes from?
  val u_tlb_wrapper = Module(new peripheral.TLBFlushController(
    16, param.toTLBParameter()
  ))

  u_tlb_wrapper.lookup_process_id_o <> u_truth_table.lookup_request_i
  u_tlb_wrapper.lookup_thread_id_i <> u_truth_table.loopup_reply_o

  u_tlb_wrapper.reply_o <> u_bus.slave_replies_i(4)
  u_tlb_wrapper.request_i <> u_bus.slave_requests_o(4)

  u_tlb_wrapper.tlb_backend_reply_o
  u_tlb_wrapper.tlb_flush_request_o
  u_tlb_wrapper.tlb_frontend_reply_i


  // Free PA list
  // getFreePPN
  // recyclePPN
  val u_freelist = Module(new peripheral.FreeListWrapper(
    1 << 24
  ))
  u_freelist.M_AXI // to DRAM (R/W)
  u_freelist.empty_vo
  u_freelist.full_vo
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

  u_page_deletor.M_AXI // to DRAM (R)
  u_page_deletor.dcache_flush_request_o
  u_page_deletor.dcache_wb_queue_empty_i

  u_page_deletor.icache_flush_request_o
  u_page_deletor.icache_wb_queue_empty_i
  

  // Page Inserter
  // insertPageFromQEMU
  val u_page_inserter = Module(new peripheral.PageInserter)
  u_page_inserter.M_AXI // to DRAM (W)
  
  u_page_inserter.reply_o <> u_bus.slave_replies_i(8)
  u_page_inserter.request_i <> u_bus.slave_requests_o(8)

  // The page buffer
  val u_page_buffer = Module(new peripheral.PageBuffer)
  u_page_buffer.S_AXI // From QEMU (RW)
  u_page_buffer.normal_read_reply_o <> u_page_inserter.read_reply_i
  u_page_buffer.normal_read_request_i <> u_page_inserter.read_request_o
  u_page_buffer.normal_write_request_i <> u_page_deletor.page_buffer_write_o

  // QEMU Message complex
  val u_qemu_message_compositor = Module(new QEMUMessageCompositor())
  u_qemu_message_compositor.evict_done_req_i <> u_page_deletor.done_message_o
  u_qemu_message_compositor.evict_notify_req_i <> u_page_deletor.start_message_o
  u_qemu_message_compositor.page_fault_req_i <> u_miss_to_qemu.queue_o 

  val u_qemu_message_sender = Module(new peripheral.QEMUMessageSender())
  u_qemu_message_sender.M_AXI // To QEMU (W)
  u_qemu_message_sender.fifo_i <> u_qemu_message_compositor.o

  // Demander Message complex
  val u_demander_message_complex = Module(new DemanderMessageCompositor())
  
  u_demander_message_complex.o <> u_r_mmq.queue_i

  val u_itlb_message_convert = Module(new peripheral.TLBMessageConverter(param.toTLBParameter()))
  u_itlb_message_convert.eviction_request_o <> u_demander_message_complex.itlb_evict_request_i
  u_itlb_message_convert.miss_request_o <> u_demander_message_complex.itlb_miss_request_i
  u_itlb_message_convert.tlb_backend_request_i

  val u_dtlb_message_convert = Module(new peripheral.TLBMessageConverter(param.toTLBParameter()))
  u_dtlb_message_convert.eviction_request_o := u_demander_message_complex.dtlb_evict_request_i
  u_dtlb_message_convert.miss_request_o := u_demander_message_complex.dtlb_miss_request_i
  u_dtlb_message_convert.tlb_backend_request_i

  val u_qemu_to_demander_message_conveter = Module(new peripheral.QEMUMessageConverter())
  u_demander_message_complex.qemu_miss_reply_i <> u_qemu_to_demander_message_conveter.miss_reply_o 
  u_demander_message_complex.qemu_evict_reply_i <> u_qemu_to_demander_message_conveter.evict_reply_o

  val u_qemu_message_receiver = Module(new peripheral.QEMUMessageReceiver({
    addr => true.B // TODO: Replace with correct function.
  }))
  u_qemu_message_receiver.S_AXI // From QEMU (R)
  u_qemu_message_receiver.fifo_o <> u_qemu_to_demander_message_conveter.i


  // TODO: DRAM Reset

}

// TODO: We need a PageBuffer to record:
// - Page from QEMU to FPGA (AXI_Slave)
// - Page from FPGA to QEMU (AXI_Slave)

// TODO: A message sender (Basically a DMA writer)

