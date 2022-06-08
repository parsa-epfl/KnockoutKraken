package armflex_cache

import armflex._
import chisel3._
import chisel3.util._
import armflex_pmu.CycleCountingPort

import armflex.MemoryAccessType._
import armflex_mmu.peripheral.PageTableReq
import scala.annotation.meta.param
import armflex.util.ExtraUtils.AddMethodsToDecoupled
import armflex_mmu.peripheral.PageTableOps
case class PageTableParams(
  pageW: Int = 12,
  vPageW: Int = 52,
  pPageW: Int = 24,
  permW: Int = 2,
  asidW: Int = 15, // FIXME: This should appear here. But we have plan to merge TLBParam and Page Table params together.
  tlbSetNumber: Int = 1,
  tlbAssociativity: Int = 32,
  thidN: Int = 32,
  ptAssociativity: Int = 16,
  vpn2ptSetPA: (UInt, UInt, Int) => UInt = (_,_,_) => 0.U
){
  def getDatabankParams: DatabankParams = DatabankParams(
    tlbSetNumber,
    tlbAssociativity,
    pPageW + permW + 1, // PPN + perm + dirty
    vPageW + asidW,
    asidW,
    thidN,
    false
  )
}

/**
 * Pipeline access (translate) request to TLB.
 * @params params the TLB Parameter
 */
class TLBPipelineReq(params: PageTableParams) extends Bundle {
  val tag = new PTTagPacket(params)
  val perm = UInt(2.W)
  val thid = UInt(log2Ceil(params.thidN).W)

  def := (o: PipeTLB.PipeTLBReq): Unit = {
    this.tag.asid := o.asid
    this.tag.vpn := o.addr >> params.pageW
    this.perm := o.perm
    this.thid := o.thid
  }

}

/**
 * The translation result we get from the TLB.
 * @params params the Page Table Parameter
 */
class TLBPipelineResp(params: PageTableParams) extends Bundle {
  val entry = new PTEntryPacket(params)
  val hit = Bool()
  val violation = Bool()
  val thid = UInt(log2Ceil(params.thidN).W)

  def toPipeTLBResponse: PipeTLB.PipeTLBResp = {
    val res = Wire(new PipeTLB.PipeTLBResp(params.thidN, params.pPageW + params.pageW))
    res.thid := thid
    res.addr := entry.ppn << params.pageW
    res.hit := hit
    res.miss := !hit
    res.violation := violation
    res
  }

}

/**
 * The reply from the mmu to a TLB. We assume that the backend is stateless.
 * @params params the TLB Parameter
 */
class TLBMMURespPacket(params: PageTableParams) extends Bundle {
  val tag = new PTTagPacket(params)
  val data = new PTEntryPacket(params)
  val thid = UInt(log2Ceil(params.thidN).W)
  val dest = PageTableOps.destType // Contains destionation of refill
}

class FlushTLBIO(params: PageTableParams) extends Bundle {
  val req = Flipped(Decoupled(new PTTagPacket(params)))
  val resp = Valid(new TLBPipelineResp(params))
}

class TLB2MMUIO(params: PageTableParams) extends Bundle {
  val pageTableReq = Decoupled(new PageTableReq(params))
  val flush = new FlushTLBIO(params)
  val refillResp = Flipped(Decoupled(new TLBMMURespPacket(params)))
}

class TLB2PipelineIO(params: PageTableParams) extends Bundle {
  val translationReq = Flipped(Decoupled(new TLBPipelineReq(params)))
  val translationResp = Valid(new TLBPipelineResp(params))
  val wakeAfterMiss = Valid(UInt(params.asidW.W))
}

class TLB(
  val params: PageTableParams,
  lruCore: () => LRUCore
) extends Module {

  def tlbUpdateFunction(req: DataBankFrontendRequestPacket, oldEntry: CacheEntry): CacheEntry = {
    val oldTLBEntry = oldEntry.asTypeOf(new PTEntryPacket(params))
    Mux(oldTLBEntry.permValid(req.perm),
      oldEntry.write(req.wData, req.wMask, false.B, true.B),
      oldEntry)
  }

  val pipeline_io = IO(new TLB2PipelineIO(params))
  val mmu_io = IO(new TLB2MMUIO(params))

  val oDebug = IO(new Bundle {
    val translateReq = Output(pipeline_io.translationReq.cloneType)
    val translateResp = Output(pipeline_io.translationResp.cloneType)
    val refillResp = Output(mmu_io.refillResp.cloneType)
  })

  oDebug.translateReq := pipeline_io.translationReq
  oDebug.translateResp := pipeline_io.translationResp
  oDebug.refillResp := mmu_io.refillResp
  
  private val u_dataBankManager = Module(new DataBankManager(params.getDatabankParams, tlbUpdateFunction))
  private val u_bramPortsAdapter = Module(new BRAMPortAdapter(params.getDatabankParams))
  private val bram = Module(new BRAMorRegister(false)(u_bramPortsAdapter.bramParams))

  u_bramPortsAdapter.frontend_read_reply_data_o <> u_dataBankManager.bank_ram_reply_data_i
  u_bramPortsAdapter.frontend_read_request_i <> u_dataBankManager.bank_ram_request_addr_o
  u_bramPortsAdapter.frontend_write_request_i <> u_dataBankManager.bank_ram_write_request_o

  u_bramPortsAdapter.bram_ports(0) <> bram.portA
  u_bramPortsAdapter.bram_ports(1) <> bram.portB

  private val u_lruCore = Module(new LRU(params.getDatabankParams, lruCore))
  u_lruCore.addr_i <> u_dataBankManager.lru_addr_o
  u_lruCore.index_i <> u_dataBankManager.lru_index_o
  u_dataBankManager.lru_which_i := u_lruCore.lru_o


  private val refill2databankReq = Wire(u_dataBankManager.frontend_request_i.cloneType)
  refill2databankReq.valid := mmu_io.refillResp.valid
  refill2databankReq.bits.addr := Cat(mmu_io.refillResp.bits.tag.asid, mmu_io.refillResp.bits.tag.vpn)
  refill2databankReq.bits.flush_v := false.B
  refill2databankReq.bits.thid := mmu_io.refillResp.bits.thid
  refill2databankReq.bits.asid := mmu_io.refillResp.bits.tag.asid
  refill2databankReq.bits.wData := DontCare
  refill2databankReq.bits.wMask := DontCare
  refill2databankReq.bits.refill_v := true.B
  refill2databankReq.bits.perm := DontCare
  refill2databankReq.bits.refillData := mmu_io.refillResp.bits.data.asUInt

  mmu_io.refillResp.ready := refill2databankReq.ready

  pipeline_io.wakeAfterMiss.valid := mmu_io.refillResp.fire
  pipeline_io.wakeAfterMiss.bits := mmu_io.refillResp.bits.thid

  private val u_3wayArbiter = Module(new Arbiter(u_dataBankManager.frontend_request_i.bits.cloneType, 3))
  private val arbFlushPort = u_3wayArbiter.io.in(0)
  private val arbRefillPort = u_3wayArbiter.io.in(1)
  private val arbPipelinePort = u_3wayArbiter.io.in(2)

  // The normal request from the pipeline
  arbPipelinePort.valid := pipeline_io.translationReq.valid
  arbPipelinePort.bits.thid := pipeline_io.translationReq.bits.thid
  arbPipelinePort.bits.asid := pipeline_io.translationReq.bits.tag.asid
  arbPipelinePort.bits.addr := Cat(pipeline_io.translationReq.bits.tag.asid, pipeline_io.translationReq.bits.tag.vpn)

  private val modified_pte = Wire(new PTEntryPacket(params))
  modified_pte.modified := true.B
  modified_pte.perm := 0.U
  modified_pte.ppn := 0.U
  arbPipelinePort.bits.wData := modified_pte.asUInt
  arbPipelinePort.bits.flush_v := false.B
  arbPipelinePort.bits.wMask := Mux(pipeline_io.translationReq.bits.perm === DATA_STORE.U, modified_pte.asUInt, 0.U)
  arbPipelinePort.bits.perm := pipeline_io.translationReq.bits.perm
  arbPipelinePort.bits.refill_v := false.B
  arbPipelinePort.bits.refillData := DontCare
  pipeline_io.translationReq.ready := arbPipelinePort.ready

  // The flush request from other place
  arbFlushPort.valid := mmu_io.flush.req.valid
  arbFlushPort.bits.addr := Cat(mmu_io.flush.req.bits.asid, mmu_io.flush.req.bits.vpn)
  arbFlushPort.bits.thid := DontCare
  arbFlushPort.bits.asid := mmu_io.flush.req.bits.asid
  arbFlushPort.bits.wData := DontCare
  arbFlushPort.bits.flush_v := true.B
  arbFlushPort.bits.wMask := DontCare
  arbFlushPort.bits.perm := DontCare
  arbFlushPort.bits.refill_v := false.B
  arbFlushPort.bits.refillData := DontCare
  mmu_io.flush.req.ready := arbFlushPort.ready

  // The refilling request from the backend of the cache
  arbRefillPort <> refill2databankReq

  u_dataBankManager.frontend_request_i.bits := u_3wayArbiter.io.out.bits
  u_dataBankManager.frontend_request_i.valid := u_3wayArbiter.io.out.valid
  u_3wayArbiter.io.out.ready := u_dataBankManager.frontend_request_i.ready

  // convert the reply to PTE.
  private val replied_pte = u_dataBankManager.frontend_reply_o.bits.rData.asTypeOf(pipeline_io.translationResp.bits.entry)

  // pipeline_io.translationResp: Response to the R/W request from the pipeline
  pipeline_io.translationResp.bits.hit := u_dataBankManager.frontend_reply_o.bits.hit
  // pipeline_io.translationResp.bits.dirty := u_dataBankManager.pipeline_io.translationResp.bits.dirty
  pipeline_io.translationResp.bits.thid := u_dataBankManager.frontend_reply_o.bits.thid
  pipeline_io.translationResp.bits.violation := !replied_pte.permValid(RegNext(pipeline_io.translationReq.bits.perm))
  pipeline_io.translationResp.bits.entry := replied_pte

  pipeline_io.translationResp.valid := u_dataBankManager.frontend_reply_o.valid && !u_dataBankManager.frontend_reply_o.bits.flush && !u_dataBankManager.frontend_reply_o.bits.refill

  // mmu_i.flush.resp
  mmu_io.flush.resp.bits.hit := u_dataBankManager.frontend_reply_o.bits.hit
  mmu_io.flush.resp.bits.entry := replied_pte
  mmu_io.flush.resp.bits.thid := u_dataBankManager.frontend_reply_o.bits.thid
  // mmu_i.flush.resp.bits.dirty := u_dataBankManager.pipeline_io.translationResp.bits.dirty
  mmu_io.flush.resp.bits.violation := false.B // Flush will never cause perm violation.
  mmu_io.flush.resp.valid := u_dataBankManager.frontend_reply_o.valid && u_dataBankManager.frontend_reply_o.bits.flush

  // Request to Page Table (miss or writeback)
  val uArbMissWriteBack = Module(new Arbiter(new PageTableReq(params), 2))
  val uReqQueueOut = Queue(uArbMissWriteBack.io.out, params.thidN + 1) // the extra 1 is for refilling.
  mmu_io.pageTableReq <> uReqQueueOut

  // write back request
  // Flush has its own path
  uArbMissWriteBack.io.in(0).bits := DontCare
  uArbMissWriteBack.io.in(0).bits.entry.tag.asid := (u_dataBankManager.writeback_request_o.bits.addr >> params.vPageW)
  uArbMissWriteBack.io.in(0).bits.entry.tag.vpn := u_dataBankManager.writeback_request_o.bits.addr
  uArbMissWriteBack.io.in(0).bits.entry.entry := u_dataBankManager.writeback_request_o.bits.data.asTypeOf(new PTEntryPacket(params))
  uArbMissWriteBack.io.in(0).bits.op := PageTableOps.opInsert
  //uArbMissWriteBack.io.in(1).handshake(u_dataBankManager.writeback_request_o, u_dataBankManager.writeback_request_o.bits.flush_v)
  when(u_dataBankManager.writeback_request_o.bits.flush_v) {
    // Ignore in case it's flush -> flush takes response from mmu_io.flush.resp port
    uArbMissWriteBack.io.in(0).valid := false.B
    u_dataBankManager.writeback_request_o.ready := true.B
  }.otherwise {
    uArbMissWriteBack.io.in(0).valid := u_dataBankManager.writeback_request_o.valid
    u_dataBankManager.writeback_request_o.ready := uArbMissWriteBack.io.in(0).ready
  }


  // Miss Request
  uArbMissWriteBack.io.in(1).bits := DontCare
  uArbMissWriteBack.io.in(1).bits.entry.tag.asid := u_dataBankManager.miss_request_o.bits.asid
  uArbMissWriteBack.io.in(1).bits.entry.tag.vpn := u_dataBankManager.miss_request_o.bits.addr // only reserve the lower bits.
  uArbMissWriteBack.io.in(1).bits.entry.entry.perm := u_dataBankManager.miss_request_o.bits.perm
  uArbMissWriteBack.io.in(1).bits.thid := u_dataBankManager.miss_request_o.bits.thid
  uArbMissWriteBack.io.in(1).bits.op := PageTableOps.opLookup
  uArbMissWriteBack.io.in(1).valid := u_dataBankManager.miss_request_o.valid
  uArbMissWriteBack.io.in(1).ready <> u_dataBankManager.miss_request_o.ready

  if(false) { // TODO Conditional printing
    val location = "TLB"
    when(u_dataBankManager.frontend_request_i.fire){
      when(u_dataBankManager.frontend_request_i.bits.refill_v) {
        printf(p"${location}:Bank:Refill[0x${Hexadecimal(u_dataBankManager.frontend_request_i.bits.addr)}]\n")
      }.elsewhen(u_dataBankManager.frontend_request_i.bits.wMask =/= 0.U) {
        printf(p"${location}:Bank:WR[0x${Hexadecimal(u_dataBankManager.frontend_request_i.bits.addr)}]\n")
      }.otherwise {
        printf(p"${location}:Bank:RD[0x${Hexadecimal(u_dataBankManager.frontend_request_i.bits.addr)}]\n")
      }
    }
//    when(u_dataBankManager.bank_ram_reply_data_i.fire){
//      when(u_dataBankManager.bank_ram_write_request_o.fire && u_dataBankManager.bank_ram_write_request_o.bits.addr === u_dataBankManager.bank_ram_request_addr_o.bits) {
//        // Forwarding from writeport
//        printf(p"${location}:Bank:Resp:Forward[${u_dataBankManager.bank_ram_write_request_o.bits.data}]\n")
//      }.otherwise{
//        printf(p"${location}:Bank:Resp:Get[SET TOO BIG TO PRINT]\n")
//      }
//    }
  }

  // Port to PMU to measure the penalty of the TLB miss.
  val oPMUCountingReq = IO(Output(new CycleCountingPort(params.thidN)))
  oPMUCountingReq.start.bits := mmu_io.pageTableReq.bits.thid
  oPMUCountingReq.start.valid := mmu_io.pageTableReq.fire
  oPMUCountingReq.stop.bits := mmu_io.refillResp.bits.thid
  oPMUCountingReq.stop.valid := mmu_io.refillResp.fire
}

