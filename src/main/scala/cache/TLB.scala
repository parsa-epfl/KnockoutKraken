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
  pPageWidth + 2, // permission bits + pPageWidth
  vPageWidth,
  threadNumber,
  0,
  implementedWithRegister
){
  
}

/**
 * Translation request send to the TLB.
 * @param param the TLB Parameter. 
 */
class TLBRequest(param: TLBParameter) extends Bundle{
  val vpage = UInt(param.vPageWidth.W)
  val thread_id = UInt(param.threadIDWidth().W)
  val permission_v = UInt(2.W) // 00: Read, 01: Write, 10: Execute
}


/**
 * The reply from the backend to a TLB. We assume that the backend is stateless.
 * @param param the TLB Parameter
 */ 
class TLBBackendReply(param: TLBParameter) extends Bundle{
  val vpage = UInt(param.vPageWidth.W)
  val thread_id = UInt(param.threadIDWidth().W)
  val permission_v = UInt(2.W)
  val ppage = UInt(param.pPageWidth.W)
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
  val frontend_request_i = IO(Flipped(Decoupled(new TLBRequest(param))))
  val frontend_reply_o = IO(UInt(param.vPageWidth.W))
  // permission violation
  val violation_o = IO(Valid(UInt(param.threadIDWidth().W)))
  // backend
  val backend_request_o = IO(Decoupled(new TLBRequest(param)))
  val backend_reply_i = IO(Decoupled(new TLBBackendReply(param)))
  // activate
  val packet_arrive_o = IO(Valid(UInt(param.threadIDWidth().W)))

  // basically, a cache.
  val u_cache = Module(BaseCache.generateTLB(param, lruCore))
  // bind the input
  u_cache.frontendRequest_i.bits.addr := frontend_request_i.bits.vpage
  // u_cache.
}

