package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import armflex.demander.software_bundle
import armflex.demander.software_bundle.ParameterConstants
import armflex.util._


/**
 * Move page from Page buffer to the DRAM.
 * 
 * implementing function: insertPageFromQEMU
 */ 
class PageInserter extends MultiIOModule {  
  val req_i = IO(Flipped(Decoupled(UInt(
    ParameterConstants.ppn_width.W
  ))))

  val ppn_r = RegInit(0.U(ParameterConstants.ppn_width.W))
  when(req_i.fire()){
    ppn_r := req_i.bits
  }

  val sIdle :: sBusy :: Nil = Enum(2)
  val state_r = RegInit(sIdle)

  val M_DMA_W = IO(new AXIWriteMasterIF(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))

  M_DMA_W.req.bits.address := Cat(ppn_r, Fill(12, 0.U(1.W)))
  M_DMA_W.req.bits.length := 64.U
  M_DMA_W.req.valid := state_r === sBusy

  // TODO: logic to send read request to the Page buffer.
  val read_request_o = IO(Decoupled(UInt(10.W)))
  val addr_cnt_r = RegInit(0.U(10.W))
  val read_done_r = RegInit(false.B)

  read_request_o.bits := addr_cnt_r
  read_request_o.valid := !read_done_r

  switch(state_r){
    is(sIdle){
      read_done_r := false.B
      addr_cnt_r := 64.U // TODO: Make the initial address as an external parameter
    }
    is(sBusy){
      when(read_request_o.fire() && M_DMA_W.data.ready){
        addr_cnt_r := Mux(read_done_r, addr_cnt_r, addr_cnt_r + 1.U)
        read_done_r := addr_cnt_r === 127.U
      }
    }
  }
  // It's possible that the read result has the back pressure.
  val read_reply_i = IO(Flipped(Decoupled(UInt(ParameterConstants.dram_data_width.W))))
  M_DMA_W.data <> read_reply_i

  when(req_i.fire()){
    state_r := sBusy
  }.elsewhen(state_r === sBusy && M_DMA_W.done){
    state_r := sIdle
  }

  val done_o = IO(Output(Bool()))
  done_o := M_DMA_W.done
  req_i.ready := state_r === sIdle
}

object PageInserterVerilogEmitter extends App {
  val c = chisel3.stage.ChiselStage
  println(c.emitVerilog(new PageInserter()))
}