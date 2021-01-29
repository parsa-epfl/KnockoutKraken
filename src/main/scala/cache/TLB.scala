package armflex.cache

import chisel3._
import chisel3.util._

class TLBParameter(
  val vPageWidth: Int = 52,
  val pPageWidth: Int = 24,
  threadNumber: Int = 4,
  setNumber: Int = 1,
  associativity: Int = 32,
  implementedWithRegister: Boolean = true
) extends CacheParameter(
  setNumber,
  associativity,
  pPageWidth + 2 + 1, // permission bits + pPageWidth + modifiedBit
  vPageWidth + log2Ceil(threadNumber) , // VA + TID
  threadNumber,
  implementedWithRegister
){
  
}
/**
 * The tag used to access a TLB entry
 * In order to be compatible with cache, we use this tag as address.
 * @param param the TLB Parameter
 */ 
class TLBTagPacket(param: TLBParameter) extends Bundle {
  val vpage = UInt(param.vPageWidth.W)
  val thread_id = UInt(param.threadIDWidth().W)

  override def cloneType: this.type = new TLBTagPacket(param).asInstanceOf[this.type]
}

/**
 * The data part of a PTE. Still make it compatible with Cache
 * @param param the TLB Parameter
 */ 
class TLBEntryPacket(param: TLBParameter) extends Bundle {
  val pp = UInt(param.pPageWidth.W)
  val permission = UInt(2.W)
  val modified = Bool()

  def isWritable(): Bool = {
    return modified === 1.U;
  }

  override def cloneType: this.type = new TLBEntryPacket(param).asInstanceOf[this.type]
}

/**
 * Frontend access (translate) request to TLB. 
 * @param param the TLB Parameter
 */ 
class TLBAccessRequestPacket(param: TLBParameter) extends Bundle {
  val tag = new TLBTagPacket(param)
  val w_v = Bool()

  override def cloneType: this.type = new TLBAccessRequestPacket(param).asInstanceOf[this.type]
}

/**
 * The translation result we get from the TLB.
 * @param param the TLB Parameter
 */ 

class TLBFrontendReplyPacket(param: TLBParameter) extends Bundle {
  // val pp = UInt(param.pPageWidth.W)
  // val modified = 
  val entry = new TLBEntryPacket(param)
  val hit = Bool()
  val violation = Bool()
  val dirty = Bool()

  override def cloneType: this.type = new TLBFrontendReplyPacket(param).asInstanceOf[this.type]
}

/**
 * Request TLB send to the backend for looking up the miss entry or writing back.
 * @param param the TLB Parameter
 * 
 * TODO: Make it compatible with TLBAccessRequestPacket
 */ 
class TLBBackendRequestPacket(param: TLBParameter) extends Bundle {
  val tag = new TLBTagPacket(param)
  val entry = new TLBEntryPacket(param)
  val w_v = Bool()
  val flush_v = Bool()
  val need_write_permission_v = Bool()

  override def cloneType: this.type = new TLBBackendRequestPacket(param).asInstanceOf[this.type]

  def toAccessRequestPacket(): TLBAccessRequestPacket = {
    val res = new TLBAccessRequestPacket(param)
    res.tag := this.tag
    res.w_v := this.w_v
    res
  }
}

/**
 * The reply from the backend to a TLB. We assume that the backend is stateless.
 * @param param the TLB Parameter
 */ 
class TLBBackendReplyPacket(param: TLBParameter) extends Bundle {
  val tag = new TLBTagPacket(param)
  val data = new TLBEntryPacket(param)

  override def cloneType: this.type = new TLBBackendReplyPacket(param).asInstanceOf[this.type]
}

/**
 * The base model of TLB.
 * @param param the TLB Parameter
 * @param refillEnabled whether keep ports for refilling.
 */ 
class BaseTLB(
  val param: TLBParameter,
  lruCore: () => LRUCore,
  refillEnabled: Boolean = true
) extends MultiIOModule {
  // frontend
  val frontend_request_i = IO(Flipped(Decoupled(new TLBAccessRequestPacket(param))))
  val flush_request_i = IO(Flipped(Decoupled(new TLBTagPacket(param))))
  val frontend_reply_o = IO(Valid(new TLBFrontendReplyPacket(param)))
  // permission violation
  val violation_o = IO(Valid(UInt(param.threadIDWidth().W)))
  // backend
  val backend_request_o = IO(Decoupled(new TLBBackendRequestPacket(param)))
  val backend_reply_i = IO(Flipped(Decoupled(new TLBBackendReplyPacket(param))))
  // activate
  val packet_arrive_o = IO(Valid(UInt(param.threadIDWidth().W)))

  // basically, a cache.
  val u_cache = Module(new BaseCache(
    param, lruCore,
    {
      (oldOne, newOne) =>
      val oD = oldOne.read().asTypeOf(new TLBEntryPacket(param))
      oD.isWritable() && (oldOne.read() =/= newOne.read())
    }
  ))

  // bind the frontend_request
  u_cache.frontend_request_i.bits.addr := frontend_request_i.bits.asUInt()
  u_cache.frontend_request_i.bits.thread_id := frontend_request_i.bits.tag.thread_id
  // mark it modified
  val modified_pte = Wire(new TLBEntryPacket(param))
  modified_pte.modified := true.B
  modified_pte.permission := 0.U
  modified_pte.pp := 0.U
  // bind the frontend_request
  u_cache.frontend_request_i.bits.wData := modified_pte.asUInt()
  u_cache.frontend_request_i.bits.wMask := modified_pte.asUInt()
  u_cache.frontend_request_i.bits.w_v := frontend_request_i.bits.w_v
  u_cache.frontend_request_i.valid := frontend_request_i.valid
  frontend_request_i.ready := u_cache.frontend_request_i.ready

  // bind the flush request
  u_cache.flush_request_i.bits.addr := flush_request_i.bits.asUInt()
  u_cache.flush_request_i.bits.thread_id := frontend_request_i.bits.tag.thread_id
  u_cache.flush_request_i.valid := flush_request_i.valid
  flush_request_i.ready := u_cache.flush_request_i.ready

  // store the write permission so that the data flow is aligned
  val s1_wr_v_r = if(param.implementedWithRegister) frontend_request_i.bits.w_v else RegNext(frontend_request_i.bits.w_v)
  
  val frontend_response =  u_cache.frontend_reply_o.bits.data.asTypeOf(new TLBEntryPacket(param))
  // after get response, check the permission
  violation_o.bits := u_cache.frontend_reply_o.bits.thread_id
  violation_o.valid := u_cache.frontend_reply_o.valid && s1_wr_v_r && !frontend_response.isWritable()
  // assign frontend_reply_o
  frontend_reply_o.valid := u_cache.frontend_reply_o.valid
  frontend_reply_o.bits.hit := u_cache.frontend_reply_o.bits.hit
  frontend_reply_o.bits.entry := frontend_response
  frontend_reply_o.bits.violation := violation_o.valid
  frontend_reply_o.bits.dirty := u_cache.frontend_reply_o.bits.dirty

  backend_request_o.bits.tag := u_cache.backend_request_o.bits.addr.asTypeOf(new TLBTagPacket(param))
  backend_request_o.bits.entry := u_cache.backend_request_o.bits.data.asTypeOf(new TLBEntryPacket(param))
  backend_request_o.bits.w_v := u_cache.backend_request_o.bits.w_v
  backend_request_o.bits.flush_v := u_cache.backend_request_o.bits.flush_v
  backend_request_o.bits.need_write_permission_v := u_cache.backend_request_o.bits.need_write_permission_v
  backend_request_o.valid := u_cache.backend_request_o.valid
  u_cache.backend_request_o.ready := backend_request_o.ready

  //There should be a queue for the refilling (In the module TLBWrapper)
  u_cache.refill_request_i.bits.addr := backend_reply_i.bits.tag.asUInt()
  u_cache.refill_request_i.bits.data := backend_reply_i.bits.data.asUInt()
  u_cache.refill_request_i.bits.not_sync_with_data_v := false.B
  u_cache.refill_request_i.bits.thread_id := backend_reply_i.bits.tag.thread_id
  u_cache.refill_request_i.valid := backend_reply_i.valid
  backend_reply_i.ready := u_cache.refill_request_i.ready

  packet_arrive_o.bits := u_cache.packet_arrive_o.bits.thread_id
  packet_arrive_o.valid := u_cache.packet_arrive_o.valid
}

