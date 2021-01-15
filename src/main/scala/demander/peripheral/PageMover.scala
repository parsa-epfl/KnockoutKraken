package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import DMAController.Bus._
import DMAController.Frontend._
import armflex.util.SoftwareStructs

class PageMover(
  sourceAddressWidth: Int,
  targetAddressWidth: Int
) extends MultiIOModule {
  class request_t extends Bundle {
    val src_addr = UInt(sourceAddressWidth.W)
    val dst_addr = UInt(targetAddressWidth.W) 
  }
  val move_request_i = IO(Flipped(Decoupled(new request_t)))
  val done_o = IO(Output(Bool()))
  val SRC_AXI_M = IO(new AXI4(sourceAddressWidth, 512))
  val DST_AXI_M = IO(new AXI4(targetAddressWidth, 512))

  val u_src_to_stream = Module(new AXI4Reader(sourceAddressWidth, 512))
  val u_stream_to_dst = Module(new AXI4Writer(targetAddressWidth, 512))

  u_src_to_stream.io.dataOut <> u_stream_to_dst.io.dataIn
  u_src_to_stream.io.bus <> SRC_AXI_M
  u_stream_to_dst.io.bus <> DST_AXI_M

  u_src_to_stream.io.xfer.address := move_request_i.bits.src_addr
  u_src_to_stream.io.xfer.length := 64.U
  u_src_to_stream.io.xfer.valid := move_request_i.valid

  u_stream_to_dst.io.xfer.address := move_request_i.bits.dst_addr
  u_stream_to_dst.io.xfer.length := 64.U
  u_stream_to_dst.io.xfer.valid := move_request_i.valid

  // The ready order of two movers is:
  //  1. u_src_to_stream
  //  2. u_stream_to_dst
  val first_ready_r = RegInit(false.B)
  val ready_r = RegInit(true.B)

  when(move_request_i.fire()){
    first_ready_r := false.B
    ready_r := false.B
  }.elsewhen(u_src_to_stream.io.xfer.done){
    first_ready_r := true.B
  }.elsewhen(first_ready_r && u_stream_to_dst.io.xfer.done){
    ready_r := true.B
    first_ready_r := false.B
  }

  move_request_i.ready := ready_r
  done_o := first_ready_r && u_stream_to_dst.io.xfer.done

}


import armflex.cache.{
  CacheFrontendFlushRequest,
  CacheParameter,
}

/**
 * Delete a page according to the given PTE.
 * 
 * This module will:
 * 1. Flush I$ and D$ according to the property of this page.
 * idea: judge the type of cache (I or D) by the permission. If read only, I$.
 * 2. Wait for the writing list to complete
 * TODO: Determine a way for this module to monitor the WB queue of the cache
 * 3. Push the page to the QEMU page buffer if this page is dirty
 * 4. Send message to QEMU
 * 
 * @param param the parameter of the cache
 */ 
class PageDeletor(
  param: CacheParameter
) extends MultiIOModule {
  val sIdle :: sFlush :: sPipe :: sWait :: sMove :: sSend :: Nil = Enum(6)
  val state_r = RegInit(sIdle)

  // Port to receive request
  val request_i = IO(Flipped(Decoupled(new SoftwareBundle.PTEntry)))
  val request_r = Reg(new SoftwareBundle.PTEntry)
  when(request_i.fire()){
    request_r := request_i.bits
  }
  request_i.ready := state_r === sIdle

  // Ports for flushing cache
  val icache_flush_request_o = IO(Decoupled(new CacheFrontendFlushRequest(param)))
  val dcache_flush_request_o = IO(Decoupled(new CacheFrontendFlushRequest(param)))

  // Counter to monitor the flush process
  val flush_cnt_r = RegInit(0.U(6.W))
  val flush_which = Mux(request_r.permission, true.B, false.B) // true: D Cache, false: I Cache
  val flush_fired = Mux(flush_which, dcache_flush_request_o.fire(), icache_flush_request_o.fire())
  when(request_i.fire()){
    flush_cnt_r := 0.U
  }.elsewhen(state_r === sFlush){
    flush_cnt_r := Mux(
     flush_fired,
      flush_cnt_r + 1.U,
      flush_cnt_r
    )
  }

  icache_flush_request_o.bits.addr := Cat(request_r.ppn, flush_cnt_r)
  icache_flush_request_o.bits.thread_id := 0.U
  dcache_flush_request_o.bits := icache_flush_request_o.bits
  
  icache_flush_request_o.valid := state_r === sFlush && !flush_which
  dcache_flush_request_o.valid := state_r === sFlush && flush_which

  // Wait 4 cycles so that the request has been piped.
  val pipe_cnt_r = RegInit(0.U(2.W))
  when(state_r === sFlush && flush_cnt_r === 63.U && flush_fired){
    pipe_cnt_r := 0.U
  }.elsewhen(state_r === sPipe){
    pipe_cnt_r := pipe_cnt_r + 1.U
  }

  // Eviction done? (You have to wait for like two / three cycles to get the correct result.)
  val icache_wb_queue_empty_i = IO(Input(Bool()))
  val dcache_wb_queue_empty_i = IO(Input(Bool()))
  val wait_which = Mux(request_r.permission, dcache_wb_queue_empty_i, icache_wb_queue_empty_i)

  // Port to drive DMA
  val u_page_mover = Module(new PageMover(36, 64))
  u_page_mover.move_request_i.bits.src_addr := Cat(request_r.ppn, 0.U(12.W))
  // TODO: The base address of the pageO
  u_page_mover.move_request_i.bits.dst_addr := 0x69ABCDEF.U
  u_page_mover.move_request_i.valid := state_r === sMove

  val SRC_AXI_M = IO(u_page_mover.SRC_AXI_M.cloneType)  
  SRC_AXI_M <> u_page_mover.SRC_AXI_M
  val DST_AXI_M = IO(u_page_mover.DST_AXI_M.cloneType)
  DST_AXI_M <> u_page_mover.DST_AXI_M

  // Port to send message to QEMU
  val message_to_qemu_o = IO(Decoupled(new SoftwareBundle.PTEntry))
  message_to_qemu_o.bits := request_r
  message_to_qemu_o.valid := state_r === sSend

  // Update logic of the state machine
  switch(state_r){
    is(sIdle){
      state_r := Mux(request_i.fire(), sFlush, sIdle)
    }
    is(sFlush){
      state_r := Mux(flush_cnt_r === 63.U && flush_fired, sPipe, sFlush)
    }
    is(sPipe){
      state_r := Mux(pipe_cnt_r === 3.U, sWait, sPipe)
    }
    is(sWait){
      state_r := Mux(
        wait_which, 
        Mux(request_r.modified, sMove, sSend),
        sWait)
    }
    is(sMove){
      state_r := Mux(u_page_mover.done_o, sSend, sMove)
    }
    is(sSend){
      state_r := Mux(message_to_qemu_o.fire(), sIdle, sSend)
    }
  }
}

object PageMoverVerilogEmitter extends App {
  import chisel3.stage.ChiselStage
  val c = new ChiselStage
  println(c.emitVerilog(new PageDeletor(
    new CacheParameter()
  )))
}
