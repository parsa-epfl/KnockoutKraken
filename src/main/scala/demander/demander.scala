package armflex.demander

import chisel3._
import chisel3.util._
import armflex.demander.peripheral.SoftwareBundle
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
  val u_r_mmq = Module(new peripheral.MMReadQueue(new SoftwareBundle.PageDemanderMessage))
  u_r_mmq.request_i <> u_bus.slave_requests_o(1)
  u_r_mmq.reply_o <> u_bus.slave_replies_i(1)

  // TODO:  u_r_mmq.queue_i

  // The truth table (with parameter)
  val u_truth_table = Module(new peripheral.ThreadTable())
  u_truth_table.request_i <> u_bus.slave_requests_o(2)
  u_truth_table.reply_o <> u_bus.slave_replies_i(2)

  // TODO: u_truth_table.S_AXI

  // TODO: the page table set manager
  val u_ptset = Module(new peripheral.PTSetCache())
  u_ptset.reply_o <> u_bus.slave_replies_i(3)
  u_ptset.request_i <> u_bus.slave_requests_o(3)
  // u_ptset.M_AXI

  // The TLB wrapper
  // TODO: Where does the parameter comes from?
  val u_tlb_wrapper = Module(new peripheral.TLBWrapper(
    16, param.toTLBParameter()
  ))

  u_tlb_wrapper.lookup_process_id_o <> u_truth_table.lookup_request_i
  u_tlb_wrapper.lookup_thread_id_i <> u_truth_table.loopup_reply_o

  u_tlb_wrapper.reply_o <> u_bus.slave_replies_i(4)
  u_tlb_wrapper.request_i <> u_bus.slave_requests_o(4)
  //! TODO: How to determine the TLB to be flushed? I-TLB or D-TLB?
  // TODO: u_tlb_wrapper.tlb_backend_reply_o
  // TODO: u_tlb_wrapper.tlb_flush_request_o
  // TODO: u_tlb_wrapper.tlb_frontend_reply_i


  // TODO: the free list
  val u_freelist = Module(new peripheral.FreeListWrapper(
    1 << 24
  ))
  // u_freelist.M_AXI
  // u_freelist.empty_vo
  // u_freelist.full_vo
  u_freelist.reply_o <> u_bus.slave_replies_i(5)
  u_freelist.request_i <> u_bus.slave_requests_o(5)

  // TODO: the QEMU message queue
  // The messages are:
  // - Replies from the QEMU:
  //   - Miss Reply
  //   - Evict Reply
  // - Requests to the QEMU:
  //   - Miss Request
  //   - Evict Request I
  //   - Evict Request II
  val u_qemu_mq = Module(new peripheral.AXIFIFOController(new SoftwareBundle.QEMUMessage)) // TODO: @param baseAddr

  // u_qemu_mq.S_AXI
  // u_qemu_mq.fifo_o

  // MMWriteQueue to QEMU.
  val u_w_mmq = Module(new peripheral.MMWriteQueue(new SoftwareBundle.QEMUMessage))
  u_w_mmq.request_i <> u_bus.slave_requests_o(5)
  u_w_mmq.reply_o <> u_bus.slave_replies_i(5)
  u_w_mmq.queue_o <> u_qemu_mq.fifo_i

  // TODO: Page deletor
  val u_page_deletor = Module(new peripheral.PageDeletor(param.toCacheParameter()))
  
  

  // TODO: DRAM Reset

}



