package armflex_mmu

import antmicro.Bus.{AXI4, AXI4Lite}
import armflex.{PipeMMUIO}
import armflex.util.{AXIControlledMessageQueue, WritePort, ReadPort}
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
  val dramDataW: Int = cacheBlockSize

  val cacheBlocksPerPage = pageSize/(cacheBlockSize/8)

  def getPageTableParams: PageTableParams = new PageTableParams(
    log2Ceil(pageSize),
    vPageW,
    pPageW,
    2,
    asidW,
    tlbSetNumber,
    tlbWayNumber,
    thidN,
    vpn2ptSetPA = (asid: UInt, vpn: UInt, ptePerLine: Int) => this.vpn2ptSetPA(asid, vpn, ptePerLine)
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
  val M_DMA_RD = new ReadPort(params.dramAddrW, params.dramDataW)
  val M_DMA_WR = new WritePort(params.dramAddrW, params.dramDataW)
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
class MMU(params: MemoryHierarchyParams, messageFIFODepth: Int = 2) extends Module {

  // ---- Modules -----
  // FIXME: Add a module to interact with multiple TLBs.

  // Receive and send requests from and to the host (QEMU)
  private val uHostMsgQueue = Module(new AXIControlledMessageQueue(params.cacheBlockSize, 32, 1))

  // Hardware page walker
  private val uPageTableReqQueue = Module(new PageTableReqQueue(params))
  private val uPageTableSetOperator = Module(new PageTableSetOperator(params.getPageTableParams, params.dramAddrW, params.dramDataW))
  private val uPageTableReqHandler = Module(new PageTableReqHandler(params))
  private val uPageTableEntryDeletor = Module(new PageTableEntryDeletor(params))

  private val msgFPGA2HOST = Queue(uPageTableReqHandler.FPGA_MSG_QUEUE.req, 4)

  // ---- IO ----
  // Amazon Shell IO
  val axiShell_io = IO(new MMU2ShellIO(params))
  axiShell_io.M_DMA_RD <> uPageTableSetOperator.DMA.rd
  axiShell_io.M_DMA_WR <> uPageTableSetOperator.DMA.wr
  axiShell_io.msgPendingInt := uHostMsgQueue.rdFifo.deq.valid

  // Cache-AXI controller
  // Pipeline IO:
  val mmu_pipe_io = IO(Flipped(new PipeMMUIO))
  mmu_pipe_io <> uPageTableEntryDeletor.MMU_FLUSH_REQ_PIPELINE_IO

  // TLB IO
  val mmu_tlb_io = IO(new Bundle {
    val inst = Flipped(new TLB2MMUIO(params.getPageTableParams))
    val data = Flipped(new TLB2MMUIO(params.getPageTableParams))
  })
  mmu_tlb_io.inst.flush <> uPageTableEntryDeletor.MMU_FLUSH_TLB_IO.inst
  mmu_tlb_io.data.flush <> uPageTableEntryDeletor.MMU_FLUSH_TLB_IO.data

  mmu_tlb_io.inst.pageTableReq <> uPageTableReqQueue.TLB_MSG_QUEUE.inst
  mmu_tlb_io.data.pageTableReq <> uPageTableReqQueue.TLB_MSG_QUEUE.data
  mmu_tlb_io.inst.refillResp <> DontCare
  mmu_tlb_io.inst.refillResp.valid := false.B
  mmu_tlb_io.data.refillResp <> DontCare
  mmu_tlb_io.data.refillResp.valid := false.B
  when(uPageTableReqHandler.PAGE_ENTRY_TLB_IO.refillResp.bits.data.perm === INST_FETCH.U) {
    mmu_tlb_io.inst.refillResp <> uPageTableReqHandler.PAGE_ENTRY_TLB_IO.refillResp
  }.otherwise {
    mmu_tlb_io.data.refillResp <> uPageTableReqHandler.PAGE_ENTRY_TLB_IO.refillResp
  }

  // Cache IO
  val mmu_cache_io = IO(new Bundle {
    val inst = Flipped(new CacheMMUIO(params.getCacheParams))
    val data = Flipped(new CacheMMUIO(params.getCacheParams))
  })
  mmu_cache_io <> uPageTableEntryDeletor.MMU_FLUSH_CACHE_IO

  // Bus to the host
  val S_CSR = IO(Flipped(uHostMsgQueue.S_CSR.cloneType)) // Control registers for MMU.
  val S_AXI = IO(Flipped(uHostMsgQueue.S_AXI.cloneType)) // F1 AWS exposed 512-bit AXI bus. For message transferring only.
  S_CSR <> uHostMsgQueue.S_CSR
  S_AXI <> uHostMsgQueue.S_AXI

  // ------------- Logic ---------------
  uHostMsgQueue.rdFifo.deq <> msgFPGA2HOST
  uHostMsgQueue.rdFifo.msgCnt := msgFPGA2HOST.valid.asUInt
  uHostMsgQueue.wrFifo.enq <> uPageTableReqQueue.HOST_MSG_QUEUE.req
  uHostMsgQueue.wrFifo.freeCnt := uPageTableReqQueue.HOST_MSG_QUEUE.req.ready.asUInt

  uPageTableReqHandler.PAGE_ENTRY_REQ <> uPageTableReqQueue.PAGE_TABLE_OPERATOR_QUEUE
  uPageTableReqHandler.PAGE_ENTRY_PAGE_WALK <> uPageTableSetOperator.PORT
  uPageTableReqHandler.PAGE_ENTRY_DELETOR <> uPageTableEntryDeletor.PORT

  if(true) { // TODO Conditional printing 
    // Miss request handler
    when(uPageTableSetOperator.DMA.rd.req.fire) {
      printf(p"MMU:AXI DRAM:QEMU get PT set to insert new PTE\n")
    }
    when(uPageTableSetOperator.DMA.wr.req.fire) {
      printf(p"MMU:AXI DRAM:QEMU push updated PT set\n")
    }
  }

  // MMU counting port to measure the page fault latency.
  val oPMUCountingReq = IO(Output(new CycleCountingPort(params.thidN)))
  oPMUCountingReq.start.bits := uPageTableReqHandler.PAGE_ENTRY_REQ.bits.thid
  oPMUCountingReq.start.valid := uPageTableReqHandler.PAGE_ENTRY_REQ.bits.op === PageTableOps.opLookup && uPageTableReqHandler.PAGE_ENTRY_REQ.fire
  oPMUCountingReq.stop.bits := uPageTableReqHandler.PAGE_ENTRY_TLB_IO.refillResp.bits.thid
  oPMUCountingReq.stop.valid := uPageTableReqHandler.PAGE_ENTRY_TLB_IO.refillResp.fire
}