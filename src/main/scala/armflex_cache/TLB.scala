package armflex_cache

import armflex._
import chisel3._
import chisel3.util._

case class PageTableParams(
  vPageW: Int = 52,
  pPageW: Int = 24,
  permW: Int = 2,
  asidW: Int = 15, // FIXME: This should appear here. But we have plan to merge TLBParam and Page Table params together.
  tlbSetNumber: Int = 1,
  tlbAssociativity: Int = 32,
  thidN: Int = 32,
){
  def getDatabankParams: DatabankParams = DatabankParams(
    tlbSetNumber,
    tlbAssociativity,
    pPageW + permW + 1, // PPN + perm + dirty
    vPageW,
    asidW,
    thidN
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
    this.tag.vpn := o.addr >> 12 // page size
    this.perm := o.perm
    this.thid := o.thid
  }

  override def cloneType: this.type = new TLBPipelineReq(params).asInstanceOf[this.type]
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
    val res = Wire(new PipeTLB.PipeTLBResp(params.pPageW))
    res.addr := entry.ppn << 12
    res.hit := hit
    res.miss := !hit
    res.violation := violation
    res
  }

  override def cloneType: this.type = new TLBPipelineResp(params).asInstanceOf[this.type]
}

/**
 * Request TLB send to the mmu for looking up the miss entry or writing back.
 * @params params the TLB Parameter
 *
 */
class TLBMMURequestPacket(params: PageTableParams) extends Bundle {
  val tag = new PTTagPacket(params)
  val entry = new PTEntryPacket(params)
  val w_v = Bool()
  val flush_v = Bool()
  val perm = UInt(2.W)
  val thid = UInt(log2Ceil(params.thidN).W)

  override def cloneType: this.type = new TLBMMURequestPacket(params).asInstanceOf[this.type]

  def toAccessRequestPacket: TLBPipelineReq = {
    val res = new TLBPipelineReq(params)
    res.tag := this.tag
    res.perm := perm
    res.thid := this.thid
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

  override def cloneType: this.type = new TLBMMURespPacket(params).asInstanceOf[this.type]
}

class TLB2MMUIO(params: PageTableParams) extends Bundle {
  val flushReq = Flipped(Decoupled(new PTTagPacket(params)))
  val flushResp = Valid(new TLBPipelineResp(params))
  val missReq = Decoupled(new TLBMissRequestMessage(params))
  val writebackReq = Decoupled(new TLBEvictionMessage(params))
  val refillResp = Flipped(Decoupled(new TLBMMURespPacket(params)))

  override def cloneType: this.type = new TLB2MMUIO(params).asInstanceOf[this.type]
}

class TLB2PipelineIO(params: PageTableParams) extends Bundle {
  val translationReq = Flipped(Decoupled(new TLBPipelineReq(params)))
  val translationResp = Valid(new TLBPipelineResp(params))
  val wakeAfterMiss = Valid(UInt(params.asidW.W))
}

class TLB(
  val params: PageTableParams,
  lruCore: () => LRUCore
) extends MultiIOModule {

  def tlbUpdateFunction(req: DataBankFrontendRequestPacket, oldEntry: CacheEntry): CacheEntry = {
    val oldTLBEntry = oldEntry.asTypeOf(new PTEntryPacket(params))
    Mux(oldTLBEntry.permValid(req.perm),
      oldEntry.write(req.wData, req.wMask, false.B, true.B),
      oldEntry)
  }

  val pipeline_io = IO(new TLB2PipelineIO(params))
  val mmu_io = IO(new TLB2MMUIO(params))

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

  pipeline_io.wakeAfterMiss.valid := mmu_io.refillResp.fire()
  pipeline_io.wakeAfterMiss.bits := mmu_io.refillResp.bits.thid

  private val u_3wayArbiter = Module(new Arbiter(u_dataBankManager.frontend_request_i.bits.cloneType(), 3))
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
  arbPipelinePort.bits.wMask := Mux(pipeline_io.translationReq.bits.perm === 1.U, modified_pte.asUInt, 0.U)
  arbPipelinePort.bits.perm := pipeline_io.translationReq.bits.perm
  arbPipelinePort.bits.refill_v := false.B
  arbPipelinePort.bits.refillData := DontCare
  pipeline_io.translationReq.ready := !mmu_io.flushReq.valid && !mmu_io.refillResp.valid

  // The flush request from other place
  arbFlushPort.valid := mmu_io.flushReq.valid
  arbFlushPort.bits.addr := Cat(mmu_io.flushReq.bits.asid, mmu_io.flushReq.bits.vpn)
  arbFlushPort.bits.thid := DontCare
  arbFlushPort.bits.asid := mmu_io.flushReq.bits.asid
  arbFlushPort.bits.wData := DontCare
  arbFlushPort.bits.flush_v := true.B
  arbFlushPort.bits.wMask := DontCare
  arbFlushPort.bits.perm := DontCare
  arbFlushPort.bits.refill_v := false.B
  arbFlushPort.bits.refillData := DontCare
  mmu_io.flushReq.ready := arbFlushPort.ready

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

  // mmu_i.flushResp
  mmu_io.flushResp.bits.hit := u_dataBankManager.frontend_reply_o.bits.hit
  mmu_io.flushResp.bits.entry := replied_pte
  mmu_io.flushResp.bits.thid := u_dataBankManager.frontend_reply_o.bits.thid
  // mmu_i.flushResp.bits.dirty := u_dataBankManager.pipeline_io.translationResp.bits.dirty
  mmu_io.flushResp.bits.violation := false.B // Flush will never cause perm violation.
  mmu_io.flushResp.valid := u_dataBankManager.frontend_reply_o.valid && u_dataBankManager.frontend_reply_o.bits.flush

  // miss request
  mmu_io.missReq.valid := u_dataBankManager.miss_request_o.valid
  mmu_io.missReq.bits.tag.asid := u_dataBankManager.miss_request_o.bits.asid
  mmu_io.missReq.bits.tag.vpn := u_dataBankManager.miss_request_o.bits.addr // only reserve the lower bits.
  mmu_io.missReq.bits.perm := u_dataBankManager.miss_request_o.bits.perm
  mmu_io.missReq.bits.thid := u_dataBankManager.miss_request_o.bits.thid
  u_dataBankManager.miss_request_o.ready := mmu_io.missReq.ready

  // write back request
  // Flush has its own path
  mmu_io.writebackReq.valid := u_dataBankManager.writeback_request_o.valid && !u_dataBankManager.writeback_request_o.bits.flush_v
  mmu_io.writebackReq.bits.tag.asid := (u_dataBankManager.writeback_request_o.bits.addr >> params.vPageW)
  mmu_io.writebackReq.bits.tag.vpn := u_dataBankManager.writeback_request_o.bits.addr
  mmu_io.writebackReq.bits.entry := u_dataBankManager.writeback_request_o.bits.data.asTypeOf(mmu_io.writebackReq.bits.entry.cloneType)
  u_dataBankManager.writeback_request_o.ready := Mux(
    u_dataBankManager.writeback_request_o.bits.flush_v,
    true.B, // if flush request, ignored directly.
    mmu_io.writebackReq.ready
  )

  if(true) { // TODO Conditional printing
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
    when(u_dataBankManager.bank_ram_reply_data_i.fire){
      when(u_dataBankManager.bank_ram_write_request_o.fire && u_dataBankManager.bank_ram_write_request_o.bits.addr === u_dataBankManager.bank_ram_request_addr_o.bits) {
        // Forwarding from writeport
        printf(p"${location}:Bank:Resp:Forward[${u_dataBankManager.bank_ram_write_request_o.bits.data}]\n")
      }.otherwise{
        printf(p"${location}:Bank:Resp:Get[SET TOO BIG TO PRINT]\n")
      }
    }
  }
}

