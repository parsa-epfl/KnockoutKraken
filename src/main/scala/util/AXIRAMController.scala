package armflex.util

import chisel3._
import chisel3.util._

import antmicro.Bus.AXI4

/**
 * Adaptor from AXI4 to normal memory port, with only INCR burst support.
 * 
 * @param addressWidth the width of the address bits.
 * @param dataWidth the width of the data bits.
 */
class AXIRAMController(
  addressWidth: Int = 64,
  dataWidth: Int = 512
) extends MultiIOModule {
  val S_AXI = IO(Flipped(new AXI4(
    addressWidth, dataWidth
  )))

  val read_request_o = IO(Decoupled(UInt(addressWidth.W)))
  val read_reply_i = IO(Flipped(Decoupled(UInt(dataWidth.W))))

  class write_request_t extends Bundle {
    val addr = UInt(addressWidth.W)
    val mask = UInt((dataWidth / 8).W)
    val data = UInt(dataWidth.W)
  }

  val write_request_o = IO(Decoupled(new write_request_t))
  val sIdle :: sTrans :: sReply :: Nil = Enum(3)

  class context_t extends Bundle {
    val id = UInt(AXI4.idWidth.W)
    val current_addr = UInt(addressWidth.W)
    val current_send_cnt = UInt(AXI4.lenWidth.W)
    val send_done = Bool()
    val current_receive_cnt = UInt(AXI4.lenWidth.W)
    val target_length = UInt(AXI4.lenWidth.W)
    val size = UInt((1 << AXI4.sizeWidth).W)
  }

  val read_state_r = RegInit(sIdle)
  val read_context_r = Reg(new context_t)

  //  Read logic

  //! Only INCR burst mode is support.
  when(S_AXI.ar.arvalid && S_AXI.ar.arlen =/= 0.U){
    assert(S_AXI.ar.arburst === 1.U)
  }

  val ar_fire = S_AXI.ar.arready && S_AXI.ar.arvalid
  val ar_final = read_context_r.current_send_cnt === read_context_r.target_length

  val r_fire = S_AXI.r.rready && S_AXI.r.rvalid
  val r_final = read_context_r.current_receive_cnt === read_context_r.target_length

  when(ar_fire){
    read_context_r.current_addr := S_AXI.ar.araddr
    read_context_r.id := S_AXI.ar.arid
    read_context_r.size := (1.U << S_AXI.ar.arsize)
    read_context_r.target_length := S_AXI.ar.arlen
    read_context_r.current_send_cnt := 0.U
    read_context_r.send_done := false.B
  }.elsewhen(read_request_o.fire()){
    read_context_r.current_addr := read_context_r.current_addr + read_context_r.size
    read_context_r.current_send_cnt := read_context_r.current_send_cnt + 1.U
    read_context_r.send_done := ar_final
  }

  when(r_fire){
    read_context_r.current_receive_cnt := read_context_r.current_receive_cnt + 1.U
  }

  switch(read_state_r){
    is(sIdle){
      read_state_r := Mux(ar_fire, sTrans, sIdle)
    }
    is(sTrans){
      read_state_r := Mux(r_final && r_fire, sIdle, sTrans)
    }
  }

  S_AXI.ar.arready := read_state_r === sIdle

  S_AXI.r.rvalid := read_state_r === sTrans && read_reply_i.valid
  S_AXI.r.rid := read_context_r.id
  S_AXI.r.rdata := read_reply_i.bits
  S_AXI.r.rlast := r_final
  S_AXI.r.rresp := 0.U //! No exclusive case

  read_request_o.valid := read_state_r === sTrans && !read_context_r.send_done
  read_request_o.bits := read_context_r.current_addr

  read_reply_i.ready := read_state_r === sTrans && S_AXI.r.rready

  val write_state_r = RegInit(sIdle)
  val write_context_r = Reg(new context_t)

  // Write logic

  //! Only INCR burst mode is support.
  when(S_AXI.aw.awvalid){
    assert(S_AXI.aw.awburst === 1.U)
  }

  val aw_fire = S_AXI.aw.awvalid && S_AXI.aw.awready
  val w_fire = S_AXI.w.wvalid && S_AXI.w.wready
  val w_final = S_AXI.w.wlast

  val b_fire = S_AXI.b.bvalid && S_AXI.b.bready

  when(aw_fire){
    write_context_r.current_addr := S_AXI.aw.awaddr
    write_context_r.current_receive_cnt := 0.U
    write_context_r.current_send_cnt := 0.U
    write_context_r.id := S_AXI.aw.awid
    write_context_r.send_done := false.B
    write_context_r.size := (1.U << S_AXI.aw.awsize)
    write_context_r.target_length := S_AXI.aw.awlen
  }.elsewhen(w_fire){
    write_context_r.current_send_cnt := write_context_r.current_send_cnt + 1.U
    write_context_r.current_addr := write_context_r.current_addr + write_context_r.size
  }

  switch(write_state_r){
    is(sIdle){
      write_state_r := Mux(aw_fire, sTrans, sIdle)
    }
    is(sTrans){
      write_state_r := Mux(w_final && w_fire, sReply, sTrans)
    }
    is(sReply){
      write_state_r := Mux(b_fire, sIdle, sReply)
    }
  }

  S_AXI.aw.awready := write_state_r === sIdle
  S_AXI.w.wready := write_state_r === sTrans && write_request_o.ready

  S_AXI.b.bid := write_context_r.id
  S_AXI.b.bresp := 0.U //! No exclusive case
  S_AXI.b.bvalid := write_state_r === sReply

  write_request_o.valid := write_state_r === sTrans && S_AXI.w.wvalid
  write_request_o.bits.addr := write_context_r.current_addr
  write_request_o.bits.data := S_AXI.w.wdata
  write_request_o.bits.mask := S_AXI.w.wstrb

}

object AXIRAMControllerVerilogEmitter extends App {
  import chisel3.stage.ChiselStage
  val s = new ChiselStage
  println(s.emitVerilog(new AXIRAMController()))
}
