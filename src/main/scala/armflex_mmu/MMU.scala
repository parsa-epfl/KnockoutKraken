package armflex_mmu

import antmicro.Bus.{AXI4, AXI4Lite}
import armflex.{PipeMMUIO}
import armflex.util.{AXIControlledMessageQueue, AXIReadMasterIF, AXIWriteMasterIF}
import armflex_cache.{CacheMMUIO, CacheParams, PageTableParams, TLB2MMUIO, TLBMMURespPacket}
import armflex_mmu.peripheral._
import chisel3._
import chisel3.util._

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

  tlbSetNumber:   Int = 16,
  tlbWayNumber:   Int = 4
) {
  val permW: Int = 2 // 0: Data_Load, 1: Data_Store, 2: Inst_Fetch
  val vPageW: Int = vAddrW - log2Ceil(pageSize)
  val pPageW: Int = pAddrW - log2Ceil(pageSize)
  val blockBiasW: Int = log2Ceil(cacheBlockSize / 8)
  val dramAddrW: Int = pAddrW
  val dramdataW: Int = cacheBlockSize

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
    cacheBlockSize
  )

  /**
   * Function returns the physical address in DRAM to access the Page Table Set containing the
   * `vpn` page table entry.
   *
   * @param asid the address space id.
   * @param vpn the virtual page number
   * @return physical address of the page table set containing the `vpn` PTE
   *
   * @note sync this function with PageDemanderDriver.vpn2ptSetPA
   */
  def vpn2ptSetPA(asid: UInt, vpn: UInt) = {


    def entryNumberInLog2 = pAddrW - log2Ceil(pageSize)
    //val reducedAsid: Iterator[Seq[Bool]] = asid.asBools.sliding(asid.getWidth/4, asid.getWidth/4)
    //val resAsidReduced = reducedAsid.map { case boolSeq: Seq[Bool] => 
    //    val vecBool: Vec[Bool] = VecInit(boolSeq)
    //    val res: Bool = vecBool.asUInt.xorR
    //    res.asUInt
    //}
    //resAsidReduced.reduce(Cat(_,_))
    val pageset_number = Cat(vpn(vpn.getWidth-1, 6), asid)
    Cat(pageset_number(entryNumberInLog2-1, 0) * 3.U(2.W), 0.U(6.W)) // Zero pad 6
  }
}

class MMU2ShellIO(params: MemoryHierarchyParams) extends Bundle {
  // Amazon Shell IO
  // AXI DMA wide access ports
  val M_DMA_R = Vec(4, new AXIReadMasterIF(params.dramAddrW, params.dramdataW))
  val M_DMA_W = Vec(3, new AXIWriteMasterIF(params.dramAddrW, params.dramdataW))
  // AXIL DMA host access ports
  // Minimal AXI address space is 128 bytes -> log2Ceil 128 bits for addressing
  val S_AXI = Flipped(new AXI4(log2Ceil(128), 512)) // F1 AWS exposed 512-bit AXI bus
  val S_AXIL_QEMU_MQ = Flipped(new AXI4Lite(32, 32)) // F1 AWS exposed 32-bit AXIL bus to host
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
) extends MultiIOModule {

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
  private val u_qemuMsgQueue = Module(new AXIControlledMessageQueue)
  private val u_qemuMissHandler = Module(new QEMUMissReplyHandler(params))
  private val u_qemuPageEvictHandler = Module(new QEMUPageEvictHandler(params))
  u_qemuMsgDecoder.qemu_evict_reply_o.ready := true.B // always ignore this message
  assert(!u_qemuMsgDecoder.qemu_evict_reply_o.valid, "QEMU Eviction Reply should be deprecated!")

  // ---- IO ----

  // Amazon Shell IO
  val axiShell_io = IO(new MMU2ShellIO(params))

  // Cache-AXI controller
  val cacheAxiCtrl_io = IO(new Bundle {
    val icacheWbEmpty = Input(Bool())
    val dcacheWbEmpty = Input(Bool())
  })

  // Pipeline IO:
  val pipeline_io = IO(Flipped(new PipeMMUIO))

  // TLB IO
  val tlb_io = IO(new Bundle {
    val inst = Flipped(new TLB2MMUIO(params.getPageTableParams))
    val data = Flipped(new TLB2MMUIO(params.getPageTableParams))
  })

  // Cache IO
  val cache_io = IO(new Bundle {
    val inst = Flipped(new CacheMMUIO(params.getCacheParams))
    val data = Flipped(new CacheMMUIO(params.getCacheParams))
  })


  // DRAM Access Modules
  // Page Walker DRAM Accesses
  axiShell_io.M_DMA_R(0) <> u_page_walker.M_DMA_R
  // TLB writeback handler
  axiShell_io.M_DMA_R(1) <> u_tlbEntryWbHandler.M_DMA_R
  axiShell_io.M_DMA_W(0) <> u_tlbEntryWbHandler.M_DMA_W
  // Miss request handler
  axiShell_io.M_DMA_R(2) <> u_qemuMissHandler.M_DMA_R
  axiShell_io.M_DMA_W(1) <> u_qemuMissHandler.M_DMA_W
  // Page Evict Handler
  axiShell_io.M_DMA_R(3) <> u_qemuPageEvictHandler.M_DMA_R
  axiShell_io.M_DMA_W(2) <> u_qemuPageEvictHandler.M_DMA_W

  // Host access
  axiShell_io.S_AXIL_QEMU_MQ <> u_qemuMsgQueue.S_AXIL
  axiShell_io.S_AXI <> u_qemuMsgQueue.S_AXI

  // Host interrupt TODO: We use polling at the moment, not interrupts
  axiShell_io.msgPendingInt := u_qemuMsgEncoder.o.valid

  // TLB Miss requests for Page Walking
  tlb_io.inst.missReq <> u_page_walker.tlb_miss_req_i(0)
  tlb_io.data.missReq <> u_page_walker.tlb_miss_req_i(1)
  // TLB Eviction request for memory writeback
  tlb_io.inst.writebackReq <> u_tlbEntryWbHandler.tlb_evict_req_i(0)
  tlb_io.data.writebackReq <> u_tlbEntryWbHandler.tlb_evict_req_i(1)

  u_qemuPageEvictHandler.evict_request_i <> u_qemuMsgDecoder.qemu_evict_page_req_o

  // Arbitrer for TLB MMU port
  // two sources:
  // 0. Page walk hit
  // 1. QEMU miss resolution
  // FIXME: Here I cannot use RRArbiter and I don't know why.
  private val u_itlbPortArb = Module(new Arbiter(new TLBMMURespPacket(params.getPageTableParams), 2))
  private val u_dtlbPortArb = Module(new Arbiter(new TLBMMURespPacket(params.getPageTableParams), 2))
  tlb_io.inst.refillResp <> u_itlbPortArb.io.out
  tlb_io.data.refillResp <> u_dtlbPortArb.io.out
  // 0: reply from the page walker
  u_itlbPortArb.io.in(0) <> u_page_walker.tlb_backend_reply_o(0)
  u_dtlbPortArb.io.in(0) <> u_page_walker.tlb_backend_reply_o(1)
  // 1: reply from the page fault resolver
  u_itlbPortArb.io.in(1).bits := u_qemuMissHandler.tlb_backend_reply_o.bits
  u_dtlbPortArb.io.in(1).bits := u_qemuMissHandler.tlb_backend_reply_o.bits
  u_itlbPortArb.io.in(1).valid := u_qemuMissHandler.tlb_backend_reply_o.valid && u_qemuMissHandler.tlb_backend_reply_o.bits.data.perm === 2.U
  u_dtlbPortArb.io.in(1).valid := u_qemuMissHandler.tlb_backend_reply_o.valid && u_qemuMissHandler.tlb_backend_reply_o.bits.data.perm =/= 2.U
  // ready signal of page fault resolver
  u_qemuMissHandler.tlb_backend_reply_o.ready := Mux(
    u_qemuMissHandler.tlb_backend_reply_o.bits.data.perm === 2.U,
    u_itlbPortArb.io.in(1).ready,
    u_dtlbPortArb.io.in(1).ready)

  // Page Eviction --------
  // clears TLB entry and Cache blocks
  pipeline_io <> u_page_deleter.lsu_handshake_o

  u_qemuMissHandler.qemu_miss_reply_i <> u_qemuMsgDecoder.qemu_miss_reply_o
  u_qemuMissHandler.page_delete_done_i := u_page_deleter.done_o
  u_qemuPageEvictHandler.page_delete_done_i := u_page_deleter.done_o

  cache_io.inst.flushReq <> u_page_deleter.icache_flush_request_o
  cache_io.data.flushReq <> u_page_deleter.dcache_flush_request_o
  cache_io.inst.stallReq <> u_page_deleter.stall_icache_vo
  cache_io.data.stallReq <> u_page_deleter.stall_dcache_vo

  u_page_deleter.icache_wb_queue_empty_i <> cacheAxiCtrl_io.icacheWbEmpty
  u_page_deleter.dcache_wb_queue_empty_i <> cacheAxiCtrl_io.dcacheWbEmpty

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

  // Evict TLB Entry
  tlb_io.inst.flushReq.bits := u_page_deleter.tlb_flush_request_o.bits.req
  tlb_io.inst.flushReq.valid := u_page_deleter.tlb_flush_request_o.valid && u_page_deleter.tlb_flush_request_o.bits.sel === 0.U

  tlb_io.data.flushReq.bits := u_page_deleter.tlb_flush_request_o.bits.req
  tlb_io.data.flushReq.valid := u_page_deleter.tlb_flush_request_o.valid && u_page_deleter.tlb_flush_request_o.bits.sel === 1.U

  // u_page_deleter.tlb_flush_request_o.bits.sel === 0 -> Instruction TLB; === 1 -> Data TLB
  u_page_deleter.tlb_flush_request_o.ready := Mux(
    u_page_deleter.tlb_flush_request_o.bits.sel === 0.U,
    tlb_io.inst.flushReq.ready,
    tlb_io.data.flushReq.ready
    )

  u_page_deleter.tlb_frontend_reply_i := Mux(
    u_page_deleter.tlb_flush_request_o.bits.sel === 0.U,
    tlb_io.inst.flushResp,
    tlb_io.data.flushResp
    )

  // QEMU message encoder
  u_qemuMsgEncoder.evict_done_req_i <> u_page_deleter.done_message_o
  u_qemuMsgEncoder.evict_notify_req_i <> u_page_deleter.start_message_o
  u_qemuMsgEncoder.page_fault_req_i <> u_page_walker.page_fault_req_o

  // QEMU Message FIFO
  u_qemuMsgQueue.fifo_i <> u_qemuMsgEncoder.o
  u_qemuMsgDecoder.message_i <> Queue(u_qemuMsgQueue.fifo_o, 1)

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
}

object PageDemanderVerilogEmitter extends App{
  val c = chisel3.stage.ChiselStage
  println(c.emitVerilog(new MMU(new MemoryHierarchyParams)))
}

