package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import armflex.demander.software_bundle
import armflex.demander.software_bundle.ParameterConstants


/**
 * Move page from Page buffer to the DRAM.
 * 
 * implementing function: insertPageFromQEMU
 */ 
class PageInserter extends MultiIOModule {
  import DMAController.Bus._
  val M_AXI = IO(new AXI4(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))
  
  val req_i = IO(Flipped(Decoupled(UInt(
    ParameterConstants.ppn_width.W
  ))))

  val ppn_r = RegInit(0.U(ParameterConstants.ppn_width.W))
  when(req_i.fire()){
    ppn_r := req_i.bits
  }

  val sIdle :: sBusy :: Nil = Enum(2)
  val state_r = RegInit(sIdle)

  val u_write_dma = Module(new DMAController.Frontend.AXI4Writer(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))
  u_write_dma.io.xfer.address := Cat(ppn_r, Fill(12, 0.U(1.W)))
  u_write_dma.io.xfer.length := 64.U
  u_write_dma.io.xfer.valid := state_r === sBusy
  u_write_dma.io.bus <> M_AXI

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
      when(read_request_o.fire() && u_write_dma.io.dataIn.ready){
        addr_cnt_r := Mux(read_done_r, addr_cnt_r, addr_cnt_r + 1.U)
        read_done_r := addr_cnt_r === 127.U
      }
    }
  }
  // It's possible that the read result has the back pressure.
  val read_reply_i = IO(Flipped(Decoupled(UInt(ParameterConstants.dram_data_width.W))))
  u_write_dma.io.dataIn <> read_reply_i

  when(req_i.fire()){
    state_r := sBusy
  }.elsewhen(state_r === sBusy && u_write_dma.io.xfer.done){
    state_r := sIdle
  }

  val done_o = IO(Output(Bool()))
  done_o := u_write_dma.io.xfer.done
  req_i.ready := state_r === sIdle
}

object PageInserterVerilogEmitter extends App {
  val c = chisel3.stage.ChiselStage
  println(c.emitVerilog(new PageInserter()))
}