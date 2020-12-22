package armflex.cache

import chisel3._
import chisel3.util._

class TLBParameter(
  val vPageWidth: Int = 64,
  val pPageWidth: Int = 36,
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
 * Translation request send to the TLB.
 * @param param the TLB Parameter. 
 */

class TLBRequestPacket(param: TLBParameter) extends Bundle {
  val vpage = UInt(param.vPageWidth.W)
  val thread_id = UInt(param.threadIDWidth().W)

  def getEquivalentAddress(): UInt = Cat(vpage, thread_id)
}

class TLBEntryPacket(param: TLBParameter) extends Bundle {
  val pp = UInt(param.pPageWidth.W)
  val permission = UInt(2.W)
  val modified = Bool()
}


class TLBAccessRequestPacket(param: TLBParameter) extends TLBRequestPacket(param) {
  val permission_v = UInt(2.W) // 00: Read, 01: Write, 10: Execute
}

class TLBFrontendReplyPacket(param: TLBParameter) extends Bundle {
  val pp = UInt(param.pPageWidth.W)
  val hit = Bool()
  val violation = Bool()
}

class TLBBackendRequestPacket(param: TLBParameter) extends Bundle {
  val addr = new TLBRequestPacket(param)
  val data = new TLBEntryPacket(param)
  val w_v = Bool()
}

/**
 * The reply from the backend to a TLB. We assume that the backend is stateless.
 * @param param the TLB Parameter
 */ 
class TLBBackendReplyPacket(param: TLBParameter) extends Bundle {
  val addr = new TLBRequestPacket(param)
  val data = new TLBEntryPacket(param)
}

/**
 * The base model of TLB.
 * @param param the TLB Parameter
 * @param refillEnabled whether keep ports for refilling.
 */ 
class BaseTLB(
  param: TLBParameter,
  lruCore: () => LRUCore,
  refillEnabled: Boolean = true
) extends MultiIOModule{
  // frontend
  val frontend_access_request_i = IO(Flipped(Decoupled(new TLBAccessRequestPacket(param))))
  val frontend_flush_request_i = IO(Flipped(Decoupled(new TLBRequestPacket(param))))
  val frontend_reply_o = IO(Valid(new TLBFrontendReplyPacket(param)))
  // permission violation
  val violation_o = IO(Valid(UInt(param.threadIDWidth().W)))
  // backend
  val backend_request_o = IO(Decoupled(new TLBBackendRequestPacket(param)))
  val backend_reply_i = IO(Decoupled(new TLBBackendReplyPacket(param)))
  // activate
  val packet_arrive_o = IO(Valid(UInt(param.threadIDWidth().W)))

  // basically, a cache.
  val u_cache = Module(new BaseCache(
    param, lruCore,
    {
      (oldOne, newOne) =>
      val oD = oldOne.read().asTypeOf(new TLBEntryPacket(param))
      oD.permission === 1.U && (oldOne.read() =/= newOne.read())
    }
  ))

  // bind the frontend_request
  u_cache.frontendRequest_i.bits.addr := frontend_access_request_i.bits.getEquivalentAddress()
  u_cache.frontendRequest_i.bits.thread_id := frontend_access_request_i.bits.thread_id
  // mark it modified
  val modified_pte = Wire(new TLBEntryPacket(param))
  modified_pte.modified := true.B
  modified_pte.permission := 0.U
  modified_pte.pp := 0.U
  // bind the frontend_request
  u_cache.frontendRequest_i.bits.wData := modified_pte.asUInt()
  u_cache.frontendRequest_i.bits.wMask := modified_pte.asUInt()
  u_cache.frontendRequest_i.bits.w_v := frontend_access_request_i.bits.permission_v === 1.U // write operation (01)
  u_cache.frontendRequest_i.valid := frontend_access_request_i.valid
  frontend_access_request_i.ready := u_cache.frontendRequest_i.ready

  // bind the flush request
  u_cache.flushRequest_i.bits.addr := frontend_flush_request_i.bits.getEquivalentAddress()
  u_cache.flushRequest_i.bits.thread_id := frontend_access_request_i.bits.thread_id
  u_cache.flushRequest_i.valid := frontend_flush_request_i.valid
  frontend_flush_request_i.ready := u_cache.flushRequest_i.ready

  // store the write permission so that the data flow is aligned
  val s1_wr_v_r = if(param.implementedWithRegister) frontend_access_request_i.bits.permission_v === 1.U else RegNext(frontend_access_request_i.bits.permission_v === 1.U)
  
  val frontend_response =  u_cache.frontendReply_o.bits.data.asTypeOf(new TLBEntryPacket(param))
  // after get response, check the permission
  violation_o.bits := u_cache.frontendReply_o.bits.thread_id
  violation_o.valid := u_cache.frontendReply_o.valid && s1_wr_v_r && (frontend_response.permission =/= 1.U)
  // assign frontend_reply_o
  frontend_reply_o.valid := u_cache.frontendReply_o.valid
  frontend_reply_o.bits.hit := u_cache.frontendReply_o.bits.hit
  frontend_reply_o.bits.pp := frontend_response.pp
  frontend_reply_o.bits.violation := violation_o.valid

  backend_request_o.bits.addr := u_cache.backendRequest_o.bits.addr.asTypeOf(new TLBRequestPacket(param))
  backend_request_o.bits.data := u_cache.backendRequest_o.bits.data.asTypeOf(new TLBEntryPacket(param))
  backend_request_o.bits.w_v := u_cache.backendRequest_o.bits.w_v
  backend_request_o.valid := u_cache.backendRequest_o.valid
  backend_request_o.ready := u_cache.backendRequest_o.ready

  //backend_reply_i.bits.addr := u_cache.backendReadReply_i.bits


}

