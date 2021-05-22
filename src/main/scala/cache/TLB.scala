package armflex.cache

import armflex.{PTEntryPacket, PTTagPacket}
import chisel3._
import chisel3.util._

class TLBParameter(
  val vPageWidth: Int = 52,
  val pPageWidth: Int = 24,
  val permissionWidth: Int = 2,
  asidWidth: Int = 15, // FIXME: This should appear here. But we have plan to merge TLBParam and Page Table param together.
  setNumber: Int = 1,
  associativity: Int = 32,
  implementedWithRegister: Boolean = true
) extends CacheParameter(
  setNumber,
  associativity,
  pPageWidth + permissionWidth + 1, // permission bits + pPageWidth + modifiedBit
  vPageWidth + asidWidth , // VA + TID
  asidWidth,
  implementedWithRegister
){
}

/**
 * Frontend access (translate) request to TLB. 
 * @param param the TLB Parameter
 */ 
class TLBAccessRequestPacket(param: TLBParameter) extends Bundle {
  val tag = new PTTagPacket(param)
  val permission = UInt(2.W)

  override def cloneType: this.type = new TLBAccessRequestPacket(param).asInstanceOf[this.type]
}

/**
 * The translation result we get from the TLB.
 * @param param the TLB Parameter
 */ 

class TLBFrontendReplyPacket(param: TLBParameter) extends Bundle {
  // val pp = UInt(param.pPageWidth.W)
  // val modified = 
  val entry = new PTEntryPacket(param)
  val hit = Bool()
  val violation = Bool()
  val dirty = Bool()

  override def cloneType: this.type = new TLBFrontendReplyPacket(param).asInstanceOf[this.type]
}

/**
 * Request TLB send to the backend for looking up the miss entry or writing back.
 * @param param the TLB Parameter
 * 
 */ 
class TLBBackendRequestPacket(param: TLBParameter) extends Bundle {
  val tag = new PTTagPacket(param)
  val entry = new PTEntryPacket(param)
  val w_v = Bool()
  val flush_v = Bool()
  val permission = UInt(2.W)

  override def cloneType: this.type = new TLBBackendRequestPacket(param).asInstanceOf[this.type]

  def toAccessRequestPacket(): TLBAccessRequestPacket = {
    val res = new TLBAccessRequestPacket(param)
    res.tag := this.tag
    res.permission := permission
    res
  }
}

/**
 * The reply from the backend to a TLB. We assume that the backend is stateless.
 * @param param the TLB Parameter
 */ 
class TLBBackendReplyPacket(param: TLBParameter) extends Bundle {
  val tag = new PTTagPacket(param)
  val data = new PTEntryPacket(param)

  override def cloneType: this.type = new TLBBackendReplyPacket(param).asInstanceOf[this.type]
}

/**
 * The base model of TLB.
 * @param param the TLB Parameter
 * @param refillEnabled whether keep ports for refilling.
 */ 
// TODO: Build the TLB with data manager + register file rather than the BaseCache.
class BaseTLB(
  val param: TLBParameter,
  lruCore: () => LRUCore
) extends MultiIOModule {
  // frontend
  val frontend_request_i = IO(Flipped(Decoupled(new TLBAccessRequestPacket(param))))
  val flush_request_i = IO(Flipped(Decoupled(new PTTagPacket(param))))
  val frontend_reply_o = IO(Valid(new TLBFrontendReplyPacket(param)))
  // permission violation
  val violation_o = IO(Valid(UInt(param.asidWidth.W)))
  // backend
  val backend_request_o = IO(Decoupled(new TLBBackendRequestPacket(param)))
  val backend_reply_i = IO(Flipped(Decoupled(new TLBBackendReplyPacket(param))))
  // activate
  val packet_arrive_o = IO(Valid(UInt(param.asidWidth.W)))

  def tlbUpdateFunction(req: DataBankFrontendRequestPacket, oldEntry: CacheEntry): CacheEntry = {
      val oldTLBEntry = oldEntry.asTypeOf(new PTEntryPacket(param))
      return Mux(
        oldTLBEntry.permissionValid(req.permission),
        oldEntry.write(req.wData, req.wMask, false.B, true.B),
        oldEntry
      )
    }

  // basically, a cache.
  val u_cache = Module(new BaseCache(
    param, lruCore, tlbUpdateFunction
  ))

  // It's not necessary to stall the TLB from outside.
  u_cache.stall_request_vi := false.B

  // bind the frontend_request
  u_cache.frontend_request_i.bits.addr := frontend_request_i.bits.tag.asUInt()
  u_cache.frontend_request_i.bits.asid := frontend_request_i.bits.tag.asid
  // mark it modified
  val modified_pte = Wire(new PTEntryPacket(param))
  modified_pte.modified := true.B
  modified_pte.permission := 0.U
  modified_pte.ppn := 0.U
  // bind the frontend_request
  u_cache.frontend_request_i.bits.wData := modified_pte.asUInt()
  u_cache.frontend_request_i.bits.wMask := Mux(frontend_request_i.bits.permission === 1.U, modified_pte.asUInt(), 0.U)
  u_cache.frontend_request_i.bits.permission := frontend_request_i.bits.permission
  u_cache.frontend_request_i.valid := frontend_request_i.valid
  frontend_request_i.ready := u_cache.frontend_request_i.ready

  // bind the flush request
  u_cache.flush_request_i.bits.addr := flush_request_i.bits.asUInt()
  u_cache.flush_request_i.bits.asid := frontend_request_i.bits.tag.asid
  u_cache.flush_request_i.valid := flush_request_i.valid
  flush_request_i.ready := u_cache.flush_request_i.ready

  // store the write permission so that the data flow is aligned
  val s1_wr_v_r = if(param.implementedWithRegister) frontend_request_i.bits.permission else RegNext(frontend_request_i.bits.permission)
  
  val frontend_response =  u_cache.frontend_reply_o.bits.data.asTypeOf(new PTEntryPacket(param))
  // after get response, check the permission
  violation_o.bits := u_cache.frontend_reply_o.bits.asid
  violation_o.valid := u_cache.frontend_reply_o.valid && !frontend_response.permissionValid(s1_wr_v_r)
  // assign frontend_reply_o
  frontend_reply_o.valid := u_cache.frontend_reply_o.valid
  frontend_reply_o.bits.hit := u_cache.frontend_reply_o.bits.hit
  frontend_reply_o.bits.entry := frontend_response
  frontend_reply_o.bits.violation := violation_o.valid
  frontend_reply_o.bits.dirty := u_cache.frontend_reply_o.bits.dirty

  backend_request_o.bits.tag := u_cache.backend_request_o.bits.addr.asTypeOf(new PTTagPacket(param))
  backend_request_o.bits.entry := u_cache.backend_request_o.bits.data.asTypeOf(new PTEntryPacket(param))
  backend_request_o.bits.w_v := u_cache.backend_request_o.bits.w_v
  backend_request_o.bits.flush_v := u_cache.backend_request_o.bits.flush_v
  backend_request_o.bits.permission := u_cache.backend_request_o.bits.permission
  backend_request_o.valid := u_cache.backend_request_o.valid
  u_cache.backend_request_o.ready := backend_request_o.ready

  //There should be a queue for the refilling (In the module TLBWrapper)
  u_cache.refill_request_i.bits.addr := backend_reply_i.bits.tag.asUInt()
  u_cache.refill_request_i.bits.data := backend_reply_i.bits.data.asUInt()
  u_cache.refill_request_i.bits.not_sync_with_data_v := false.B
  u_cache.refill_request_i.bits.asid := backend_reply_i.bits.tag.asid
  u_cache.refill_request_i.valid := backend_reply_i.valid
  backend_reply_i.ready := u_cache.refill_request_i.ready

  packet_arrive_o.bits := u_cache.packet_arrive_o.bits.asid
  packet_arrive_o.valid := u_cache.packet_arrive_o.valid
}

