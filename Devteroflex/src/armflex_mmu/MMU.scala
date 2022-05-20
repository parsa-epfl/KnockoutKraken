package armflex_mmu

import antmicro.Bus.{AXI4, AXI4Lite}
import armflex.{PipeMMUIO}
import armflex.util.{AXIControlledMessageQueue, AXIReadMasterIF, AXIWriteMasterIF}
import armflex_cache.{CacheMMUIO, CacheParams, PageTableParams, TLB2MMUIO, TLBMMURespPacket}
import armflex_mmu.peripheral._
import chisel3._
import chisel3.util._
import antmicro.CSR.CSRBusBundle
import armflex_pmu.CycleCountingPort

import armflex.MemoryAccessType._

/**
 * Parameter structure for the whole memory system
 */
case class MemoryHierarchyParams(
  // Core config
  thidN:    Int = 32,
  asidW:    Int = 15, // ISA property
  pageSize: Int = 4096,
  vAddrW:   Int = 64, // ISA property
  pAddrW:   Int = 36, // How much physcal memory has the platform (AWS 64GB)

  cacheBlockSize: Int = 512, // Depend on platform (AWS 512 bit)
  cacheMaxMissInFlight: Int = 16,
  cacheSetNumber: Int = 1024,
  cacheWayNumber: Int = 4,

  tlbSetNumber:   Int = 1024,
  tlbWayNumber:   Int = 4
) {
  val permW: Int = 2 // 0: Data_Load, 1: Data_Store, 2: Inst_Fetch
  val vPageW: Int = vAddrW - log2Ceil(pageSize)
  val pPageW: Int = pAddrW - log2Ceil(pageSize)
  val blockBiasW: Int = log2Ceil(cacheBlockSize / 8)
  val dramAddrW: Int = pAddrW
  val dramdataW: Int = cacheBlockSize

  val cacheBlocksPerPage = pageSize/(cacheBlockSize/8)

  def getPageTableParams: PageTableParams = new PageTableParams(
    log2Ceil(pageSize),
    vPageW,
    pPageW,
    2,
    asidW,
    tlbSetNumber,
    tlbWayNumber,
    thidN
    )

  def getCacheParams: CacheParams = new CacheParams(
    pAddrW,
    cacheSetNumber,
    cacheWayNumber,
    cacheBlockSize,
    thidN + 1 // + 1 is for refilling.
  )

  /**
   * Function returns the physical address in DRAM to access the Page Table Set containing the
   * `vpn` page table entry.
   *
   * @param asid the address space id.
   * @param vpn the virtual page number
   * @return physical address of the page table set containing the `vpn` PTE
   *
   * @note sync this function with MMUDriver.vpn2ptSetPA
   */
  def vpn2ptSetPA(asid: UInt, vpn: UInt, PTEsPerLine: Int) = {
    def lineNumberInLog2 = pAddrW - log2Ceil(pageSize) - log2Ceil(PTEsPerLine)
    val pageset_number = Cat(vpn(vpn.getWidth-1, 6), asid)
    Cat(pageset_number(lineNumberInLog2-1, 0) * 3.U(2.W), 0.U(6.W)) // Zero pad 6
  }
}

class MMU2ShellIO(params: MemoryHierarchyParams) extends Bundle {
  // Page Table DMA ports
  val M_DMA_R = Vec(4, new AXIReadMasterIF(params.dramAddrW, params.dramdataW))
  val M_DMA_W = Vec(3, new AXIWriteMasterIF(params.dramAddrW, params.dramdataW))
  // Host interrupt
  val msgPendingInt = Output(Bool())
}

/**
 * Top module of MMU.
 *
 * @params params the parameter of the memory system for communication with TLB and cache.
 * @params messageFIFODepth the depth of all FIFOs that contains messages.
 *
 */
class MMU(
  params: MemoryHierarchyParams,
  messageFIFODepth: Int = 2
) extends Module {

  // ---- Modules -----
  // FIXME: Add a module to interact with multiple TLBs.

  // Hardware page walker
  // TODO: Page walker and TLB writer back handler should be merged into one module in order to keep the consistency.
  private val u_page_walker = Module(new PageWalker(params))
  // Page Evictor
  private val u_page_deleter = Module(new PageDeletor(params))

  // TLB Message receiver/decoder
  private val u_tlbEntryWbHandler = Module(new TLBWritebackHandler(params, 2))

  // QEMU Message receiver/decoder
  private val u_qemuMsgDecoder = Module(new QEMUMessageDecoder(params))
  private val u_qemuMsgEncoder = Module(new QEMUMessageEncoder(params, messageFIFODepth))
  private val u_qemuMsgQueue = Module(new AXIControlledMessageQueue(params.cacheBlockSize, 32, 1))
  private val u_qemuMissHandler = Module(new QEMUMissReplyHandler(params))
  private val u_qemuPageEvictHandler = Module(new QEMUPageEvictHandler(params))
  u_qemuMsgDecoder.qemu_evict_reply_o.ready := true.B // always ignore this message
  assert(!u_qemuMsgDecoder.qemu_evict_reply_o.valid, "QEMU Eviction Reply should be deprecated!")

  // ---- IO ----

  // Amazon Shell IO
  val axiShell_io = IO(new MMU2ShellIO(params))

  // Cache-AXI controller
  // Pipeline IO:
  val mmu_pipe_io = IO(Flipped(new PipeMMUIO))

  // TLB IO
  val mmu_tlb_io = IO(new Bundle {
    val inst = Flipped(new TLB2MMUIO(params.getPageTableParams))
    val data = Flipped(new TLB2MMUIO(params.getPageTableParams))
  })

  // Cache IO
  val mmu_cache_io = IO(new Bundle {
    val inst = Flipped(new CacheMMUIO(params.getCacheParams))
    val data = Flipped(new CacheMMUIO(params.getCacheParams))
  })

  // Bus to the host
  val S_CSR = IO(Flipped(u_qemuMsgQueue.S_CSR.cloneType)) // Control registers for MMU.
  val S_AXI = IO(Flipped(u_qemuMsgQueue.S_AXI.cloneType)) // F1 AWS exposed 512-bit AXI bus. For message transferring only.
  S_CSR <> u_qemuMsgQueue.S_CSR
  S_AXI <> u_qemuMsgQueue.S_AXI

  // Debug ports
  val oDebug = IO(new Bundle {
    val pageFaultReq = Output(u_qemuMsgEncoder.oDebug.pageFaultReq.cloneType)
    val pageFaultReply = Output(u_qemuMsgDecoder.oDebug.pageFaultReply.cloneType)
    val inFIFOHandshake = Output(u_qemuMsgQueue.oDebug.inFIFOHandshake.cloneType)
    val outFIFOHandshake = Output(u_qemuMsgQueue.oDebug.outFIFOHandshake.cloneType)
  })

  oDebug.pageFaultReq := u_qemuMsgEncoder.oDebug.pageFaultReq
  oDebug.pageFaultReply := u_qemuMsgDecoder.oDebug.pageFaultReply
  oDebug.inFIFOHandshake := u_qemuMsgQueue.oDebug.inFIFOHandshake
  oDebug.outFIFOHandshake := u_qemuMsgQueue.oDebug.outFIFOHandshake

  // DRAM Access Modules
  // Page Walker DRAM Accesses
  axiShell_io.M_DMA_R(0) <> u_page_walker.M_DMA_R
  when(u_page_walker.M_DMA_R.req.fire) {
    assert(u_page_walker.M_DMA_R.req.bits.address < (1 << (params.pAddrW - 8)).U, "Page walker should not read the page region.")
  }
  // TLB writeback handler
  axiShell_io.M_DMA_R(1) <> u_tlbEntryWbHandler.M_DMA_R
  when(u_tlbEntryWbHandler.M_DMA_R.req.fire) {
    assert(u_tlbEntryWbHandler.M_DMA_R.req.bits.address < (1 << (params.pAddrW - 8)).U, "TLB eviction handler should not read the page region.\"")
  }
  axiShell_io.M_DMA_W(0) <> u_tlbEntryWbHandler.M_DMA_W
  when(u_tlbEntryWbHandler.M_DMA_W.req.fire) {
    assert(u_tlbEntryWbHandler.M_DMA_W.req.bits.address < (1 << (params.pAddrW - 8)).U, "TLB eviction handler should not write the page region.\"")
  }
  // Miss request handler
  axiShell_io.M_DMA_R(2) <> u_qemuMissHandler.M_DMA_R
  when(u_qemuMissHandler.M_DMA_R.req.fire) {
    assert(u_qemuMissHandler.M_DMA_R.req.bits.address < (1 << (params.pAddrW - 8)).U, "Page fault handler should not read the page region.\"")
  }
  axiShell_io.M_DMA_W(1) <> u_qemuMissHandler.M_DMA_W
  when(u_qemuMissHandler.M_DMA_W.req.fire) {
    assert(u_qemuMissHandler.M_DMA_W.req.bits.address < (1 << (params.pAddrW - 8)).U, "Page fault handler should not write the page region.\"")
  }
  // Page Evict Handler
  axiShell_io.M_DMA_R(3) <> u_qemuPageEvictHandler.M_DMA_R
  when(u_qemuPageEvictHandler.M_DMA_R.req.fire) {
    assert(u_qemuPageEvictHandler.M_DMA_R.req.bits.address < (1 << (params.pAddrW - 8)).U, "Page eviction handler should not write the page region.\"")
  }
  axiShell_io.M_DMA_W(2) <> u_qemuPageEvictHandler.M_DMA_W
  when(u_qemuPageEvictHandler.M_DMA_W.req.fire) {
    assert(u_qemuPageEvictHandler.M_DMA_W.req.bits.address < (1 << (params.pAddrW - 8)).U, "Page eviction handler should not write the page region.\"")
  }


  // Host interrupt TODO: We use polling at the moment, not interrupts
  axiShell_io.msgPendingInt := u_qemuMsgEncoder.o.valid

  // TLB Miss requests for Page Walking
  mmu_tlb_io.inst.missReq <> u_page_walker.tlb_miss_req_i(0)
  mmu_tlb_io.data.missReq <> u_page_walker.tlb_miss_req_i(1)
  // TLB Eviction request for memory writeback
  mmu_tlb_io.inst.writebackReq <> u_tlbEntryWbHandler.tlb_evict_req_i(0)
  mmu_tlb_io.data.writebackReq <> u_tlbEntryWbHandler.tlb_evict_req_i(1)

  u_qemuPageEvictHandler.evict_request_i <> u_qemuMsgDecoder.qemu_evict_page_req_o

  // Arbitrer for TLB MMU port
  // two sources:
  // 0. Page walk hit
  // 1. QEMU miss resolution
  // FIXME: Here I cannot use RRArbiter and I don't know why.
  private val u_itlbPortArb = Module(new Arbiter(new TLBMMURespPacket(params.getPageTableParams), 2))
  private val u_dtlbPortArb = Module(new Arbiter(new TLBMMURespPacket(params.getPageTableParams), 2))
  mmu_tlb_io.inst.refillResp <> u_itlbPortArb.io.out
  mmu_tlb_io.data.refillResp <> u_dtlbPortArb.io.out
  // 0: reply from the page walker
  u_itlbPortArb.io.in(0) <> u_page_walker.tlb_backend_reply_o(0)
  u_dtlbPortArb.io.in(0) <> u_page_walker.tlb_backend_reply_o(1)
  // 1: reply from the page fault resolver
  u_itlbPortArb.io.in(1).bits := u_qemuMissHandler.tlb_backend_reply_o.bits
  u_dtlbPortArb.io.in(1).bits := u_qemuMissHandler.tlb_backend_reply_o.bits
  u_itlbPortArb.io.in(1).valid := u_qemuMissHandler.tlb_backend_reply_o.valid && u_qemuMissHandler.tlb_backend_reply_o.bits.data.perm === INST_FETCH.U
  u_dtlbPortArb.io.in(1).valid := u_qemuMissHandler.tlb_backend_reply_o.valid && u_qemuMissHandler.tlb_backend_reply_o.bits.data.perm =/= INST_FETCH.U
  // ready signal of page fault resolver
  u_qemuMissHandler.tlb_backend_reply_o.ready := Mux(
    u_qemuMissHandler.tlb_backend_reply_o.bits.data.perm === INST_FETCH.U,
    u_itlbPortArb.io.in(1).ready,
    u_dtlbPortArb.io.in(1).ready)

  // Page Eviction --------
  // clears TLB entry and Cache blocks
  mmu_pipe_io <> u_page_deleter.mmu_pipe_io

  u_qemuMissHandler.qemu_miss_reply_i <> u_qemuMsgDecoder.qemu_miss_reply_o
  u_qemuMissHandler.page_delete_done_i := u_page_deleter.done_o
  u_qemuPageEvictHandler.page_delete_done_i := u_page_deleter.done_o

  mmu_cache_io <> u_page_deleter.mmu_cache_io

  /* FIXME: Here I cannot even use Arbiter and I don't know why. Temporary I use a manual Arbiter to solve the problem...
  val u_arb_page_delete_req = Module(new Arbiter(new PageTableItem(params.mem.getPageTableParams()), 2))
  u_arb_page_delete_req.io.in(0) <> u_qemu_miss.page_delete_req_o
  u_arb_page_delete_req.io.in(1) <> u_qemu_page_evict.page_delete_req_o
  // */
  u_page_deleter.page_delete_req_i.bits := Mux(
    u_qemuMissHandler.page_delete_req_o.valid,
    u_qemuMissHandler.page_delete_req_o.bits,
    u_qemuPageEvictHandler.page_delete_req_o.bits
  )

  u_page_deleter.page_delete_req_i.valid := u_qemuMissHandler.page_delete_req_o.valid || u_qemuPageEvictHandler.page_delete_req_o.valid
  u_qemuMissHandler.page_delete_req_o.ready := u_page_deleter.page_delete_req_i.ready
  u_qemuPageEvictHandler.page_delete_req_o.ready := u_page_deleter.page_delete_req_i.ready && !u_qemuMissHandler.page_delete_req_o.valid

  // Evict TLB Entry. Now the flush request will be sent to all TLBs.
  mmu_tlb_io.inst.flushReq.bits := u_page_deleter.mmu_tlb_flush_io.req.bits
  mmu_tlb_io.inst.flushReq.valid := u_page_deleter.mmu_tlb_flush_io.req.valid

  mmu_tlb_io.data.flushReq.bits := u_page_deleter.mmu_tlb_flush_io.req.bits
  mmu_tlb_io.data.flushReq.valid := u_page_deleter.mmu_tlb_flush_io.req.valid
  u_page_deleter.mmu_tlb_flush_io.req.ready := mmu_tlb_io.inst.flushReq.ready && mmu_tlb_io.data.flushReq.ready

  when(u_page_deleter.mmu_tlb_flush_io.req.valid){
    assert(mmu_tlb_io.inst.flushReq.ready, "iTLB flush port should be always ready when receiving flush request.")
    assert(mmu_tlb_io.data.flushReq.ready, "dTLB flush port should be always ready when receiving flush request.")
    assert(u_page_deleter.mmu_tlb_flush_io.req.ready)
  }

  // Pick the one that cause a flush hit. But the dTLB has a higher priority, because the entry can be dirty.
  u_page_deleter.mmu_tlb_flush_io.resp := Mux(
    mmu_tlb_io.data.flushResp.bits.hit,
    mmu_tlb_io.data.flushResp,
    mmu_tlb_io.inst.flushResp
  )

  // QEMU message encoder
  u_qemuMsgEncoder.evict_done_req_i <> u_page_deleter.done_message_o
  u_qemuMsgEncoder.evict_notify_req_i <> u_page_deleter.start_message_o
  u_qemuMsgEncoder.page_fault_req_i <> u_page_walker.page_fault_req_o

  // QEMU Message FIFO
  u_qemuMsgQueue.rdFifo.deq <> u_qemuMsgEncoder.o
  u_qemuMsgQueue.rdFifo.msgCnt := u_qemuMsgEncoder.o.valid.asUInt
  u_qemuMsgDecoder.message_i <> Queue(u_qemuMsgQueue.wrFifo.enq, 1)
  u_qemuMsgQueue.wrFifo.freeCnt := Mux(u_qemuMsgDecoder.message_i.ready, 1.U, 0.U)

  if(true) { // TODO Conditional printing 
    // Page Walker DRAM Accesses
    when(u_page_walker.M_DMA_R.req.fire) {
      printf(p"MMU:AXI DRAM: Page Walk, Get PT set\n")
    }
    // TLB writeback handler
    when(u_tlbEntryWbHandler.M_DMA_R.req.fire) {
      printf(p"MMU:AXI DRAM:TLB miss get PTE\n")
    }
    when(u_tlbEntryWbHandler.M_DMA_W.req.fire) {
      printf(p"MMU:AXI DRAM:TLB eviction writeback PTE\n")
    }
    // Miss request handler
    when(u_qemuMissHandler.M_DMA_R.req.fire) {
      printf(p"MMU:AXI DRAM:QEMU get PT set to insert new PTE\n")
    }
    when(u_qemuMissHandler.M_DMA_W.req.fire) {
      printf(p"MMU:AXI DRAM:QEMU push updated PT set\n")
    }
    // Page Evict Handler
    when(u_qemuPageEvictHandler.M_DMA_R.req.fire){

    }
    when(u_qemuPageEvictHandler.M_DMA_W.req.fire) {
      printf(p"MMU:AXI DRAM:Evict entry from PT\n")
    }
  }

  // MMU counting port to measure the page fault latency.
  val oPMUCountingReq = IO(Output(new CycleCountingPort(params.thidN)))
  oPMUCountingReq.start.bits := u_qemuMsgEncoder.page_fault_req_i.bits.thid
  oPMUCountingReq.start.valid := u_qemuMsgEncoder.page_fault_req_i.fire
  oPMUCountingReq.stop.bits := u_qemuMsgDecoder.qemu_miss_reply_o.bits.thid
  oPMUCountingReq.stop.valid := u_qemuMsgDecoder.qemu_miss_reply_o.bits.thid_v && u_qemuMsgDecoder.qemu_miss_reply_o.fire
}

object PageDemanderVerilogEmitter extends App{
  val c = chisel3.stage.ChiselStage
  println(c.emitVerilog(new MMU(new MemoryHierarchyParams)))
}

