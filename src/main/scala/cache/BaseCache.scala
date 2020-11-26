package armflex.cache

import chisel3._
import chisel3.util._

import chisel3.stage.ChiselStage

import armflex.util.Diverter
import armflex.util.BRAMConfig
import armflex.util.BRAMPort
import armflex.util.BRAM
import armflex.util.FlushQueue
//import firrtl.PrimOps.Mul

import scala.collection.mutable
import treadle.executable.DataType
import scala.xml.dtd.impl.Base

case class CacheParameter(
  // Cache properties
  setNumber: Int = 1024,
  associativity: Int = 4,
  blockBit: Int = 512,  
  // Address
  addressWidth: Int = 55, // address to access the whole block instead of one byte
  // Thread id
  threadNumber: Int = 4,
  // backend data width. Sometimes this is not equal to the block size, for instance, TLB.
  backendPortSize: Int = 512,
  // the data bank is implemented in register?
  implementedWithRegister: Boolean = false
){
  assert (isPow2(setNumber))
  assert (isPow2(associativity))

  def tagWidth(): Int = {
    addressWidth - log2Ceil(setNumber)
  }

  def threadIDWidth(): Int = {
    if(threadNumber > 0)
      log2Ceil(threadNumber)
    else
      0
  }

  def wayWidth(): Int = {
    log2Ceil(associativity)
  }

  def setWidth(): Int = {
    log2Ceil(setNumber)
  }
  
}

/**
 * Queue that stores the miss requests orderly. These requests are necessary when generating refilling packet.
 * @param t the Chisel type of the entry
 * @param param the Cache Parameter.
 */ 
class RefillingQueue(t: Entry, param: CacheParameter) extends MultiIOModule{
  val miss_request_i = IO(Flipped(Decoupled(new MissRequestPacket(param))))
  val backend_reply_i = IO(Flipped(Decoupled(UInt(param.blockBit.W))))
  val refill_o = IO(Decoupled(new MissResolveReplyPacket(t, param)))
  // there is a queue for the miss request

  val miss_request_q = Queue(miss_request_i, param.threadNumber * 2)
  assert(miss_request_i.ready)

  refill_o.bits.thread_id := miss_request_q.bits.thread_id
  refill_o.bits.lru := miss_request_q.bits.lru
  refill_o.bits.not_sync_with_data_v := miss_request_q.bits.not_sync_with_data_v
  refill_o.bits.set_number := miss_request_q.bits.addr(param.setWidth()-1, 0)
  refill_o.bits.grouped_v := miss_request_q.bits.grouped_v
  refill_o.bits.data := t.refill(miss_request_q.bits.addr, miss_request_q.bits.thread_id, backend_reply_i.bits)

  refill_o.valid := miss_request_q.valid && Mux(miss_request_q.bits.not_sync_with_data_v, true.B, backend_reply_i.valid)
  miss_request_q.ready := refill_o.ready && Mux(miss_request_q.bits.not_sync_with_data_v, true.B, backend_reply_i.valid)
  backend_reply_i.ready := refill_o.ready && !(miss_request_q.bits.not_sync_with_data_v && backend_reply_i.valid)
}


class MergedBackendRequestPacket(param: CacheParameter) extends Bundle{
  val addr = UInt(param.addressWidth.W)
  val w_v = Bool()
  val data = UInt(param.blockBit.W)

  override def cloneType: this.type = new MergedBackendRequestPacket(param).asInstanceOf[this.type]
}

class BackendRequestMerger(param: CacheParameter) extends MultiIOModule{
  val read_request_i = IO(Flipped(Decoupled(new MissRequestPacket(param))))
  val write_request_i = IO(Flipped(Decoupled(new WriteBackRequestPacket(param))))

  val backend_request_o = IO(Decoupled(new MergedBackendRequestPacket(param)))

  val read_request_converted = Wire(Decoupled(new MergedBackendRequestPacket(param)))
  read_request_converted.bits.addr := read_request_i.bits.addr
  read_request_converted.bits.data := DontCare
  read_request_converted.bits.w_v := false.B
  read_request_converted.valid := read_request_i.valid
  read_request_i.ready := read_request_converted.ready

  val write_request_converted = Wire(Decoupled(new MergedBackendRequestPacket(param)))
  write_request_converted.bits.addr := write_request_i.bits.addr
  write_request_converted.bits.data := write_request_i.bits.data
  write_request_converted.bits.w_v := true.B
  write_request_converted.valid := write_request_i.valid
  write_request_i.ready := write_request_converted.ready

  val merger_rr = Module(new RRArbiter(new MergedBackendRequestPacket(param), 2))
  merger_rr.io.in(0) <> read_request_converted
  merger_rr.io.in(1) <> write_request_converted
  backend_request_o <> merger_rr.io.out
}

/**
 *  base class of LRU module.
 *  @param lruCore the LRU updating logic
 *  @param param Cache parameters
 */ 
class LRU[T <: LRUCore](
  param: CacheParameter,
  lruCore: () => T
) extends MultiIOModule{
  val addr_i = IO(Flipped(ValidIO(UInt(param.setWidth.W))))
  val index_i = IO(Flipped(ValidIO(UInt(param.wayWidth().W))))
  val lru_o = IO(Output(UInt(param.wayWidth().W)))
  // add an extra stage to store the addr. They will be used for the LRU bits writing back.
  val addr_s1_r = RegNext(addr_i.bits)
  val addr_s1_vr = RegNext(addr_i.valid)

  // Connected to the LRU Core
  val core = Module(lruCore())

  implicit val bramConfig = new BRAMConfig(
    1,
    core.encodingWidth(),
    param.setNumber
  )

  val bram = Module(new BRAMorRegister(param.implementedWithRegister))

  bram.portA.EN := addr_i.valid
  bram.portA.ADDR := addr_i.bits
  bram.portA.WE := false.B
  bram.portA.DI := 0.U


  core.io.encoding_i := bram.portA.DO

  core.io.index_i := index_i.bits
  core.io.index_vi := index_i.valid
  core.io.vi := addr_s1_vr // if true, there is a request.
  lru_o := core.io.lru_o

  // write back
  bram.portB.EN := addr_s1_vr
  bram.portB.ADDR := addr_s1_r
  bram.portB.WE := addr_s1_vr
  bram.portB.DI := core.io.encoding_o
}

/**
 * The generator for BRAM-based cache.
 * 
 * The size of all operands is `param.blockBit`, which is typically 512bit
 * for a cache. If you hope to access the cache by 32bit, please partition 
 * the block manually. 
 * 
 * @param param the parameters of the cache
 * @param entry the entry Chisel Type. See Entry.scala for all options.
 * @param lruCore an generator of the LRU updating logic. See LRUCore.scala for all options.
 */ 
class BaseCache(
  val param: CacheParameter,
  entry: Entry,
  lruCore: () => LRUCore,
) extends MultiIOModule{
  val u_bank_frontend = Module(new DataBankFrontend(entry, param))
  val u_bram_adapter = Module(new BRAMPortAdapter(entry, param))

  implicit val cfg = u_bram_adapter.bramCfg
  val bram = Module(new BRAMorRegister(param.implementedWithRegister))
  u_bram_adapter.bram_ports(0) <> bram.portA
  u_bram_adapter.bram_ports(1) <> bram.portB

  u_bram_adapter.frontend_read_reply_data_o <> u_bank_frontend.bank_ram_reply_data_i
  u_bram_adapter.frontend_read_request_i <> u_bank_frontend.bank_ram_request_addr_o
  u_bram_adapter.frontend_write_request_i <> u_bank_frontend.bank_ram_write_request_o

  val frontendRequest_i = IO(Flipped(u_bank_frontend.frontend_request_i.cloneType))
  frontendRequest_i <> u_bank_frontend.frontend_request_i
  val frontendReply_o = IO(u_bank_frontend.frontend_reply_o.cloneType)
  frontendReply_o <> u_bank_frontend.frontend_reply_o
  val packet_arrive_o = IO(u_bank_frontend.packet_arrive_o.cloneType)
  packet_arrive_o <> u_bank_frontend.packet_arrive_o

  val u_lruCore = Module(new LRU(param, lruCore))
  u_lruCore.addr_i <> u_bank_frontend.lru_addr_o
  u_lruCore.index_i <> u_bank_frontend.lru_index_o
  u_bank_frontend.lru_which_i := u_lruCore.lru_o

  // Connect to the Refill Queue
  val u_refill_queue = Module(new RefillingQueue(entry, param))
  u_refill_queue.miss_request_i <> u_bank_frontend.miss_request_o
  u_refill_queue.refill_o <> u_bank_frontend.refill_request_i
  val backendReadReply_i = IO(Flipped(u_refill_queue.backend_reply_i.cloneType)) //! When clone a Flipped Decoupled type, remember to use Flipped to make it correct direction
  backendReadReply_i <> u_refill_queue.backend_reply_i

  // Connect to the backend merger
  val u_backend_merger = Module(new BackendRequestMerger(param))
  u_backend_merger.read_request_i <> u_bank_frontend.miss_request_o
  u_backend_merger.write_request_i <> u_bank_frontend.writeback_request_o
  
  // Backend Queue
  val backendRequest_o = IO(u_backend_merger.backend_request_o.cloneType)
  backendRequest_o <> u_backend_merger.backend_request_o
}

object BaseCache{
  def generateCache(param: CacheParameter, lruCore: () => LRUCore): BaseCache = {
    new BaseCache(param, new CacheEntry(param), lruCore)
  }
}