package armflex_cache

import armflex.{PTEntryPacket, PTTagPacket, PipeTLB}
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
 * Frontend access (translate) request to TLB.
 * @params params the TLB Parameter
 */
class TLBAccessRequestPacket(params: PageTableParams) extends Bundle {
  val tag = new PTTagPacket(params)
  val perm = UInt(2.W)
  val thid = UInt(log2Ceil(params.thidN).W)

  override def cloneType: this.type = new TLBAccessRequestPacket(params).asInstanceOf[this.type]

  def := (o: armflex.PipeTLB.PipeTLBRequest): Unit = {
    this.tag.asid := o.asid
    this.tag.vpn := o.addr >> 12 // page size
    this.perm := o.perm
    this.thid := o.thid
  }
}

/**
 * The translation result we get from the TLB.
 * @params params the Page Table Parameter
 */
class TLBFrontendReplyPacket(params: PageTableParams) extends Bundle {
  // val pp = UInt(params.pPageW.W)
  // val modified =
  val entry = new PTEntryPacket(params)
  val hit = Bool()
  val violation = Bool()
  // val dirty = Bool()
  val thid = UInt(log2Ceil(params.thidN).W)

  def toPipeTLBResponse: PipeTLB.PipeTLBResponse = {
    val res = Wire(new PipeTLB.PipeTLBResponse(params.pPageW))
    res.addr := entry.ppn << 12
    res.hit := hit
    res.miss := !hit
    res.violation := violation
    res
  }

  override def cloneType: this.type = new TLBFrontendReplyPacket(params).asInstanceOf[this.type]
}

/**
 * Request TLB send to the backend for looking up the miss entry or writing back.
 * @params params the TLB Parameter
 *
 */
class TLBBackendRequestPacket(params: PageTableParams) extends Bundle {
  val tag = new PTTagPacket(params)
  val entry = new PTEntryPacket(params)
  val w_v = Bool()
  val flush_v = Bool()
  val perm = UInt(2.W)
  val thid = UInt(log2Ceil(params.thidN).W)

  override def cloneType: this.type = new TLBBackendRequestPacket(params).asInstanceOf[this.type]

  def toAccessRequestPacket: TLBAccessRequestPacket = {
    val res = new TLBAccessRequestPacket(params)
    res.tag := this.tag
    res.perm := perm
    res.thid := this.thid
    res
  }
}

/**
 * The reply from the backend to a TLB. We assume that the backend is stateless.
 * @params params the TLB Parameter
 */
class TLBBackendReplyPacket(params: PageTableParams) extends Bundle {
  val tag = new PTTagPacket(params)
  val data = new PTEntryPacket(params)
  val thid = UInt(log2Ceil(params.thidN).W)

  override def cloneType: this.type = new TLBBackendReplyPacket(params).asInstanceOf[this.type]
}

class BRAMTLB(
  val params: PageTableParams,
  lruCore: () => LRUCore
) extends MultiIOModule {

  def tlbUpdateFunction(req: DataBankFrontendRequestPacket, oldEntry: CacheEntry): CacheEntry = {
    val oldTLBEntry = oldEntry.asTypeOf(new PTEntryPacket(params))
    Mux(
      oldTLBEntry.permValid(req.perm),
      oldEntry.write(req.wData, req.wMask, false.B, true.B),
      oldEntry
      )
  }

  val u_bank_frontend = Module(new DataBankManager(params.getDatabankParams, tlbUpdateFunction))
  val u_bram_adapter = Module(new BRAMPortAdapter(params.getDatabankParams))

  implicit val bramParams = u_bram_adapter.bramParams
  val bram = Module(new BRAMorRegister(false))
  u_bram_adapter.bram_ports(0) <> bram.portA
  u_bram_adapter.bram_ports(1) <> bram.portB

  u_bram_adapter.frontend_read_reply_data_o <> u_bank_frontend.bank_ram_reply_data_i
  u_bram_adapter.frontend_read_request_i <> u_bank_frontend.bank_ram_request_addr_o
  u_bram_adapter.frontend_write_request_i <> u_bank_frontend.bank_ram_write_request_o

  val u_lruCore = Module(new LRU(params.getDatabankParams, lruCore))
  u_lruCore.addr_i <> u_bank_frontend.lru_addr_o
  u_lruCore.index_i <> u_bank_frontend.lru_index_o
  u_bank_frontend.lru_which_i := u_lruCore.lru_o

  // frontend
  val frontend_request_i = IO(Flipped(Decoupled(new TLBAccessRequestPacket(params))))
  val frontend_reply_o = IO(Valid(new TLBFrontendReplyPacket(params)))
  // flush
  val flush_request_i = IO(Flipped(Decoupled(new PTTagPacket(params))))
  val flush_reply_o = IO(Valid(new TLBFrontendReplyPacket(params)))
  // backend
  val miss_request_o = IO(Decoupled(new TLBBackendRequestPacket(params)))
  val refill_request_i = IO(Flipped(Decoupled(new TLBBackendReplyPacket(params))))
  // activate
  val packet_arrive_o = IO(Valid(UInt(params.asidW.W)))

  val refill_request_internal = Wire(u_bank_frontend.frontend_request_i.cloneType)
  refill_request_internal.valid := refill_request_i.valid
  refill_request_internal.bits.addr := Cat(refill_request_i.bits.tag.asid, refill_request_i.bits.tag.vpn)
  refill_request_internal.bits.flush_v := false.B
  refill_request_internal.bits.thid := refill_request_i.bits.thid
  refill_request_internal.bits.asid := refill_request_i.bits.tag.asid
  refill_request_internal.bits.wData := DontCare
  refill_request_internal.bits.wMask := DontCare
  refill_request_internal.bits.refill_v := true.B
  refill_request_internal.bits.perm := DontCare
  refill_request_internal.bits.refillData := refill_request_i.bits.data.asUInt

  refill_request_i.ready := refill_request_internal.ready

  packet_arrive_o.valid := refill_request_i.fire()
  packet_arrive_o.bits := refill_request_i.bits.tag.asid

  //val flush_ack_o = IO(Output(Bool()))

  val u_frontend_arb = Module(new Arbiter(u_bank_frontend.frontend_request_i.bits.cloneType(), 3))
  // the order:
  // - 0: Flush
  // - 1: Refill
  // - 2: Request

  // The normal request from the pipeline
  u_frontend_arb.io.in(2).valid := frontend_request_i.valid
  u_frontend_arb.io.in(2).bits.thid := frontend_request_i.bits.thid
  u_frontend_arb.io.in(2).bits.asid := frontend_request_i.bits.tag.asid
  u_frontend_arb.io.in(2).bits.addr := Cat(frontend_request_i.bits.tag.asid, frontend_request_i.bits.tag.vpn)
  val modified_pte = Wire(new PTEntryPacket(params))
  modified_pte.modified := true.B
  modified_pte.perm := 0.U
  modified_pte.ppn := 0.U
  u_frontend_arb.io.in(2).bits.wData := modified_pte.asUInt
  u_frontend_arb.io.in(2).bits.flush_v := false.B
  u_frontend_arb.io.in(2).bits.wMask := Mux(frontend_request_i.bits.perm === 1.U, modified_pte.asUInt, 0.U)
  u_frontend_arb.io.in(2).bits.perm := frontend_request_i.bits.perm
  u_frontend_arb.io.in(2).bits.refill_v := false.B
  u_frontend_arb.io.in(2).bits.refillData := DontCare

  frontend_request_i.ready := !flush_request_i.valid && !refill_request_i.valid

  // The flush request from other place
  u_frontend_arb.io.in(0).valid := flush_request_i.valid
  u_frontend_arb.io.in(0).bits.addr := Cat(frontend_request_i.bits.tag.asid, frontend_request_i.bits.tag.vpn)
  u_frontend_arb.io.in(0).bits.thid := DontCare
  u_frontend_arb.io.in(0).bits.asid := frontend_request_i.bits.tag.asid
  u_frontend_arb.io.in(0).bits.wData := DontCare
  u_frontend_arb.io.in(0).bits.flush_v := true.B
  u_frontend_arb.io.in(0).bits.wMask := DontCare
  u_frontend_arb.io.in(0).bits.perm := DontCare
  u_frontend_arb.io.in(0).bits.refill_v := false.B
  u_frontend_arb.io.in(0).bits.refillData := DontCare

  flush_request_i.ready := u_frontend_arb.io.in(0).ready
  // The refilling request from the backend of the cache
  u_frontend_arb.io.in(1) <> refill_request_internal

  u_bank_frontend.frontend_request_i.bits := u_frontend_arb.io.out.bits
  u_bank_frontend.frontend_request_i.valid := u_frontend_arb.io.out.valid
  u_frontend_arb.io.out.ready := u_bank_frontend.frontend_request_i.ready

  // convert the reply to PTE.
  val replied_pte = u_bank_frontend.frontend_reply_o.bits.rData.asTypeOf(new PTEntryPacket(params))

  // frontend_reply_o: Response to the R/W request from the pipeline
  frontend_reply_o.bits.hit := u_bank_frontend.frontend_reply_o.bits.hit
  // frontend_reply_o.bits.dirty := u_bank_frontend.frontend_reply_o.bits.dirty
  frontend_reply_o.bits.thid := u_bank_frontend.frontend_reply_o.bits.thid
  frontend_reply_o.bits.violation := !replied_pte.permValid(RegNext(frontend_request_i.bits.perm))
  frontend_reply_o.bits.entry := replied_pte

  frontend_reply_o.valid := u_bank_frontend.frontend_reply_o.valid && !u_bank_frontend.frontend_reply_o.bits.flush && !u_bank_frontend.frontend_reply_o.bits.refill
  // val packet_arrive_o = IO(u_bank_frontend.packet_arrive_o.cloneType)

  // flush_reply_o
  flush_reply_o.bits.hit := u_bank_frontend.frontend_reply_o.bits.hit
  flush_reply_o.bits.entry := replied_pte
  flush_reply_o.bits.thid := u_bank_frontend.frontend_reply_o.bits.thid
  // flush_reply_o.bits.dirty := u_bank_frontend.frontend_reply_o.bits.dirty
  flush_reply_o.bits.violation := false.B // Flush will never cause perm violation.
  flush_reply_o.valid := u_bank_frontend.frontend_reply_o.valid && u_bank_frontend.frontend_reply_o.bits.flush

  // miss request
  // val miss_request_o = IO(u_bank_frontend.miss_request_o.cloneType)
  miss_request_o.valid := u_bank_frontend.miss_request_o.valid
  miss_request_o.bits.tag.asid := u_bank_frontend.miss_request_o.bits.asid
  miss_request_o.bits.tag.vpn := u_bank_frontend.miss_request_o.bits.addr // only reserve the lower bits.
  miss_request_o.bits.perm := u_bank_frontend.miss_request_o.bits.perm
  miss_request_o.bits.thid := u_bank_frontend.miss_request_o.bits.thid
  miss_request_o.bits.flush_v := DontCare
  miss_request_o.bits.entry := DontCare
  miss_request_o.bits.w_v := u_bank_frontend.miss_request_o.bits.wMask =/= 0.U
  u_bank_frontend.miss_request_o.ready := miss_request_o.ready

  // write back request
  // So make the request pop all the time since we will not use it.
  u_bank_frontend.writeback_request_o.ready := true.B
}

