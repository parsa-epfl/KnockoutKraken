package armflex.cache

import chisel3._
import chisel3.util._

import armflex.util.Diverter
import armflex.util.BRAMConfig
import armflex.util.BRAM

case class CacheParameter(
  // Cache properties
  setNumber: Int = 1024,
  associativity: Int = 4,
  blockBit: Int = 512,  
  // Address
  addressWidth: Int = 55, // address to access the whole block instead of one byte
  // Thread id
  threadNumber: Int = 4,
  // Permission recordings. A D cache needs one bit to record the permission of the data.
  writable: Boolean = false,
  // operation port width
  operandWidth: Int = 32, // 32 for ICache, 128 usually for DCache?
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
  
}
/**
 * The memory and its controlling logic in a cache to store all data entries.
 * 
 * Signals in the DataBank are divided into 4 channels:
 * - Frontend:  The request / response ports to the pipeline
 * - Backend:   The request / response ports to the DRAM & high-level controller
 * - LRU:       The port to activate the LRU module
 * - Notify:    The ports to notify a waking up, sleeping, and kill of one thread due to the data arrival, missing, and permission mismatching
 * 
 * Since we use SyncMem in this module, we need an extra cycle to judge whether to hit and another cycle to write back.
 * - Latch the frontend request as well as access memory.
 * -------------------- Register --------------------
 * - With the data entry, we can judge the request is hit or miss. 
 *  - if hit, a read response is immediately available. A write request will be pushed to the writing queue.
 *  - if miss, a request to access the DRAM is pushed to the queue
 * 
 * Two-ported BRAM is used in this module, the two ports are designed to:
 *  - Access the data entries for the frontend request (Read-only)
 *  - Write back the data packet from the high-level response or the frontend write request.
 *    - RRArbiter to prevent starving
 * 
 * Write requests from the frontend will be broadcast to two queues:
 *  - DRAM / high-level write queue, and its capacity is 4 * threadNumber
 *  - Write port of the BRAM to update data in the cache
 * Therefore we employ a diverter to make sure two requests are pushed to the two queues only once.
 * 
 * @param param the parameter of cache.
 * @param t the Data entry Chisel type. See Entry.scala for all options.
 * @param implemented_with_register build the bank with register, which is useful and necessary for the L1 TLB.
 * 
 */ 
class DataBank[T <: DataBankEntry](
  param: CacheParameter, 
  t: T,
  implemented_with_register: Boolean = false //! If implemented in register, how to handle the rhythm of dataflow?
) extends Module{
  val io = IO(new Bundle{
    // two ports, one for the frontend and one for the backend.
    val frontend = new Bundle{
      val req_i = Flipped(Decoupled(new Bundle{
        val addr = UInt(param.addressWidth.W) // access address.
        val thread_id = UInt(log2Ceil(param.threadNumber).W)
        val w_v = if(param.writable) Some(Bool()) else None // write valid?
        val w_data = if(param.writable) Some(UInt(param.blockBit.W)) else None
      }))
      val rep_o = Decoupled(new Bundle{
        val data = UInt(param.blockBit.W)
      })
    }

    val backend = new Bundle{
      val r_addr_o = Decoupled(UInt(param.addressWidth.W))
      val r_data_i = Flipped(Decoupled(UInt(param.blockBit.W)))
      val w_req_o = if(param.writable) Some(Decoupled(new Bundle{
        val addr = UInt(param.addressWidth.W)
        val data = UInt(param.blockBit.W)
      })) else None
    }

    val lru = new Bundle{
      val addr_o = Output(UInt(param.addressWidth.W))
      val addr_vo = Output(Bool())

      val index_o = Output(UInt(param.wayWidth().W))
      val index_vo = Output(Bool())

      val lru_i = Input(UInt(param.wayWidth().W))
    }

    val t_notify = new Bundle{
      val wakeup_o = ValidIO(UInt(param.threadIDWidth().W))
      val sleep_o = ValidIO(UInt(param.threadIDWidth().W))
      val kill_o = ValidIO(UInt(param.threadIDWidth().W))
    }
  })

  val bram_row_type_t = Vec(param.associativity, t)
  implicit val bram_config = new BRAMConfig(
    param.associativity,
    t.getWidth,
    param.setNumber
  )
  val mem = Module(new BRAM())

  // ######## FRONTEND ########
  val setWidth = log2Ceil(param.setNumber)
  val f_addr = io.frontend.req_i.bits.addr
  val f_thread = io.frontend.req_i.bits.thread_id
  val f_v_s0 = RegNext(io.frontend.req_i.valid)
  val f_addr_s0 = RegNext(f_addr)
  val f_thread_s0 = RegNext(f_thread)
  val wv_r = if(param.writable) RegNext(io.frontend.req_i.valid && io.frontend.req_i.bits.w_v.get) else false.B

  mem.portA.ADDR := f_addr(setWidth-1, 0)
  mem.portA.EN := io.frontend.req_i.valid
  mem.portA.WE := 0.U

  // ----- CYCLE 0: Access BRAM & store the request information -----
  val f_acc = mem.portA.DO.asTypeOf(bram_row_type_t)
  
  io.lru.addr_o := f_addr // forward to LRU
  io.lru.addr_vo := io.frontend.req_i.valid

  // ----- CYCLE 1: Read out or trigger state machine -----
  val match_bits = f_acc.map({ x=>
    x.checkHit(f_addr, f_thread) && x.v
  }) // get all the comparison results
  val hits_which = OHToUInt(match_bits) // encode from the comparison
  val is_hit: Bool = PopCount(match_bits) === 1.U // this value should only be 1 or zero.

  //! TBD here for how to handle the permission check of bits.
  val is_valid = true.B

  io.frontend.rep_o.valid := f_v_s0 && is_hit && is_valid
  io.frontend.rep_o.bits.data := f_acc(hits_which).read() // read

  io.lru.index_o := hits_which
  io.lru.index_vo := is_hit

  class WritebackPacket extends Bundle{
    val addr = UInt(param.addressWidth.W)  // address
    val t_id = UInt(param.threadIDWidth().W) // tid
    val lru_position = UInt(param.wayWidth().W) // which position to replace
    val data_pack = UInt(param.blockBit.W)
  }

  // Write request from the frontend
  val frontend_wb_port = Wire(Decoupled(new WritebackPacket))

  if(param.writable){
    val w_data_r = RegNext(io.frontend.req_i.bits.w_data.get)
    frontend_wb_port.valid := wv_r && is_hit && is_valid
    frontend_wb_port.bits.addr := f_addr_s0
    frontend_wb_port.bits.data_pack := w_data_r
    frontend_wb_port.bits.t_id := f_thread_s0
    frontend_wb_port.bits.lru_position := Mux(is_hit, hits_which, io.lru.lru_i) 
    io.frontend.req_i.ready := Mux(io.frontend.req_i.bits.w_v.get, frontend_wb_port.ready, true.B) // new write request will not be handled until the writing back is processed.
  } else {
    frontend_wb_port.bits := DontCare
    frontend_wb_port.valid := false.B
    io.frontend.req_i.ready := true.B
  }

  // -------- Push missing request to the backend --------
  class F2BData extends Bundle{
    val addr = UInt(param.addressWidth.W)
    val thread_id = UInt(param.threadIDWidth().W)
    val lru = UInt(param.wayWidth().W)
  }

  val f2b_missing_req = Wire(Decoupled(new F2BData))
  f2b_missing_req.bits.addr := f_addr_s0
  f2b_missing_req.bits.thread_id := f_thread_s0
  f2b_missing_req.bits.lru := io.lru.lru_i
  f2b_missing_req.valid := !is_hit && f_v_s0 && is_valid // when miss occurs, push request to the backend.

  val f2b_fifo = Queue(f2b_missing_req, param.threadNumber) // FIFO to transmit the missing request from front to back.


  // ######## BACKEND ########
  // ---- Read from the DRAM ----
  val eB_IDLE :: eB_WAIT :: eB_WRITE :: Nil = Enum(3)
  val backend_state_r = RegInit(eB_IDLE)
  val backend_ready = backend_state_r === eB_IDLE && io.backend.r_addr_o.ready

  val backend_accept_data = backend_state_r === eB_WAIT // the backend is ready to receive the data from the DRAM

  f2b_fifo.ready := backend_accept_data

  // val backend_context_t =  // the bundle of the working context of the backend.
  val backend_wb_r = Reg(new WritebackPacket)
  // update logic of backend_context
  when(f2b_fifo.valid && backend_ready){
    // save address and thread from the FIFO
    backend_wb_r.addr := f2b_fifo.bits.addr
    backend_wb_r.t_id := f2b_fifo.bits.thread_id
    backend_wb_r.lru_position := f2b_fifo.bits.lru
  }.elsewhen(io.backend.r_data_i.valid && backend_accept_data){
    backend_wb_r.data_pack := io.backend.r_data_i.bits
  }
  val backend_wb_port = Wire(Decoupled(backend_wb_r))
  backend_wb_port.valid := backend_state_r === eB_WRITE

  // State machine of the backend logic
  switch(backend_state_r){
    is(eB_IDLE){
      backend_state_r := Mux(f2b_fifo.valid && backend_ready, eB_WAIT, eB_IDLE)
    }
    is(eB_WAIT){ // wait the data response from the backend port
      backend_state_r := Mux(io.backend.r_data_i.valid && backend_accept_data, eB_WRITE, eB_WAIT)
    }
    is(eB_WRITE){ // Wait write to complete.
      backend_state_r := Mux(backend_wb_port.ready, eB_IDLE, eB_WRITE)
    }
  }

  // diverter the frontend_wb_port. It should point to both the data block as well as the DRAM.
  var diversion = Diverter(1, frontend_wb_port)
  if(param.writable){
    diversion = Diverter(2, frontend_wb_port)
  }

  // Write port arbiter 
  val arb = Module(new RRArbiter(new WritebackPacket, 2))
  arb.io.in(0) <> backend_wb_port
  arb.io.in(1) <> diversion(0)

  val wb_pkt = arb.io.out // arbiter result.
  wb_pkt.ready := true.B

  // full write back data. We only modify the relevant part.
  val mem_wb_data = WireInit(0.U.asTypeOf(bram_row_type_t))
  mem_wb_data(wb_pkt.bits.lru_position).buildFrom(wb_pkt.bits.addr, wb_pkt.bits.t_id, wb_pkt.bits.data_pack)

  mem.portB.EN := wb_pkt.valid
  mem.portB.WE := UIntToOH(wb_pkt.bits.lru_position)
  mem.portB.ADDR := wb_pkt.bits.addr
  mem.portB.DI := mem_wb_data.asUInt()

  // ######## BACKEND Write ########
  if(param.writable){
    val b2mem_if = Decoupled(new WritebackPacket)
    b2mem_if <> diversion(1)
    val b2mem_fifo = Queue(b2mem_if, 4*param.threadNumber)
    val w_req = io.backend.w_req_o.get
    w_req.valid := b2mem_fifo.valid
    b2mem_fifo.ready := w_req.ready
    w_req.bits.addr := b2mem_fifo.bits.addr
    w_req.bits.data := b2mem_fifo.bits.data_pack
  }

  // ######## Notify ########
  // Wake up
  io.t_notify.wakeup_o.bits := backend_wb_r.t_id
  io.t_notify.wakeup_o.valid := backend_wb_port.valid && backend_wb_port.ready // when the write back is committed, wake up the thread.
  // Sleep
  io.t_notify.sleep_o.bits := f2b_missing_req.bits.thread_id
  io.t_notify.sleep_o.valid := f2b_missing_req.valid && f2b_missing_req.ready // when the miss request is committed.

}


/**
 *  base class of LRU module.
 *  @param lru_core the LRU updating logic
 *  @param param Cache parameters
 *  @param implemented_with_register use register as the buffer to store the encoding if true.
 */ 
class LRU[T <: LRUCore](
  param: CacheParameter,
  lru_core: T,
  implemented_with_register: Boolean
) extends Module{
  val io = IO(new Bundle {
    // First cycle: give me the address for me to fetch the LRU
    val addr_i = Input(UInt(param.addressWidth.W))
    val addr_vi = Input(Bool())
    // Second cycle: give me whether or not you hits a specific position
    val index_i = Input(UInt(param.wayWidth().W))
    val index_vi = Input(Bool())
    // I will also tell you which position is available for replacement.
    val lru_o = Output(UInt(param.wayWidth().W))
    // The update of lru_o is valid in the third cycle.
  })
  // add an extra stage to store the addr. They will be used for the LRU bits writing back.
  val addr_s1_r = RegNext(io.addr_i)
  val addr_s1_vr = RegNext(io.addr_vi)

  implicit val bram_config = new BRAMConfig(
    1,
    lru_core.encodingWidth(),
    param.setNumber
  )

  val bram = Module(new BRAM())

  bram.portA.EN := io.addr_vi
  bram.portA.ADDR := io.addr_i
  bram.portA.WE := false.B

  // Connected to the LRU Core
  val core = Module(lru_core)

  core.io.encoding_i := bram.portA.DO

  core.io.index_i := io.index_i
  core.io.index_vi := io.index_vi
  core.io.vi := addr_s1_vr // if true, there is a request.
  io.lru_o := core.io.lru_o

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
 * @param entry_t the entry Chisel Type. See Entry.scala for all options.
 * @param lru_core_t the LRU updating logic. See LRUCore.scala for all options.
 */ 
class BaseCache[T_ENTRY <: DataBankEntry, T_LRU_CORE <: LRUCore](
  param: CacheParameter,
  entry_t: T_ENTRY,
  lru_core_t: LRUCore,
) extends Module{
  val data_bank = Module(new DataBank(param, entry_t))
  val lru = Module(new LRU(param, lru_core_t, false))
  val io = IO(new Bundle{
    val frontend = data_bank.io.frontend.cloneType
    val backend = data_bank.io.backend.cloneType
    val t_notify = data_bank.io.t_notify.cloneType
  })

  io.frontend <> data_bank.io.frontend
  io.backend <> data_bank.io.backend
  io.t_notify <> data_bank.io.t_notify

  data_bank.io.lru <> lru.io
}