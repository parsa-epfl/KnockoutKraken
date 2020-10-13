package armflex.cache

import armflex.util.Diverter
import armflex.util.BRAMConfig
import armflex.util.BRAM

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage}

abstract class DataBankEntry extends Bundle{
  val v = Bool() // valid bit
  def buildFrom(address:UInt, threadID:UInt, data: UInt)

  def checkHit(address: UInt, threadID: UInt): Bool
  def read(): UInt
  def write(value: UInt)
}

class DataBank[T <: DataBankEntry](param: CacheParameter, t: T) extends Module{
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

    val notify_wakeup_o = ValidIO(UInt(param.threadIDWidth().W))
    val notify_sleep_o = ValidIO(UInt(param.threadIDWidth().W))
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

  io.frontend.rep_o.valid := f_v_s0 && is_hit
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
    val wv_r = RegNext(io.frontend.req_i.valid && io.frontend.req_i.bits.w_v.get)
    val w_data_r = RegNext(io.frontend.req_i.bits.w_data.get)
    frontend_wb_port.valid := wv_r && is_hit
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
  f2b_missing_req.valid := !is_hit && f_v_s0 // when miss occurs, push request to the backend.

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
  io.notify_wakeup_o.bits := backend_wb_r.t_id
  io.notify_wakeup_o.valid := backend_wb_port.valid && backend_wb_port.ready // when the write back is committed, wake up the thread.
  // Sleep
  io.notify_sleep_o.bits := f2b_missing_req.bits.thread_id
  io.notify_sleep_o.valid := f2b_missing_req.valid && f2b_missing_req.ready // when the miss request is committed.

}
