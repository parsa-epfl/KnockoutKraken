package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import armflex.demander.software_bundle

import armflex.cache.{
  CacheFrontendFlushRequest,
  CacheParameter,
}

/**
 * Delete a page according to the given PTE.
 * Replated function: movePageToQEMU
 * 
 * This module will:
 * 0. Notify QEMU that an eviction will start.
 * 1. Flush I$ and D$ according to the property of this page.
 * idea: judge the type of cache (I or D) by the permission. If read only, I$.
 * 2. Wait for the writing list to complete
 * 3. Push the page to the QEMU page buffer if this page is dirty
 * 4. Send message to QEMU that the eviction is complete
 * 
 * @param param the parameter of the cache
 */ 
class PageDeletor(
  param: CacheParameter
) extends MultiIOModule {
  val sIdle :: sNotify :: sFlush :: sPipe :: sWait :: sMove :: sSend :: Nil = Enum(7)
  val state_r = RegInit(sIdle)

  // val request_i = IO(Flipped(Decoupled(new software_bundle.PTEntry)))
  /**
   * Address  | Meaning
   * 0x0      | ppn
   * 0x1      | permission
   * 0x2      | modified 
   * 0x3      | r: is ready, w: start
   */ 
  val request_i = IO(Flipped(Valid(new MemoryRequestPacket(32, 32))))
  val reply_o = IO(UInt(32.W))

  val pte_r = Reg(new software_bundle.PTEntry)
  val start_r = RegInit(false.B)
  val reply_r = RegInit(0.U(32.W))
  reply_o := reply_r

  val internal_address = request_i.bits.addr(3,2)
  val write_v = request_i.bits.w_v && request_i.valid

  switch(internal_address){
    is(0x0.U){
      reply_r := pte_r.ppn
    }
    is(0x1.U){
      reply_r := pte_r.permission
    }
    is(0x2.U){
      reply_r := pte_r.modified
    }
    is(0x3.U){
      reply_r := state_r === sIdle
    }
  }

  when(write_v){
    switch(internal_address){
      is(0x0.U){
        pte_r.ppn := request_i.bits.data
      }
      is(0x1.U){
        pte_r.permission := request_i.bits.data
      }
      is(0x2.U){
        pte_r.modified := request_i.bits.data
      }
    }
  }

  when(write_v && internal_address === 0x3.U && state_r === sIdle){
    start_r := true.B
  }.otherwise {
    start_r := false.B
  }

  // Port to send a starting message.
  val start_message_o = IO(Decoupled(new software_bundle.PageEvictRequest(software_bundle.QEMUMessagesType.sEvictNotify)))
  start_message_o.bits.pte := pte_r
  start_message_o.valid := state_r === sNotify

  // Ports for flushing cache
  val icache_flush_request_o = IO(Decoupled(new CacheFrontendFlushRequest(param)))
  val dcache_flush_request_o = IO(Decoupled(new CacheFrontendFlushRequest(param)))

  // Counter to monitor the flush process
  val flush_cnt_r = RegInit(0.U(6.W))
  val flush_which = Mux(pte_r.permission =/= 2.U, true.B, false.B) // true: D Cache, false: I Cache
  val flush_fired = Mux(flush_which, dcache_flush_request_o.fire(), icache_flush_request_o.fire())
  when(start_r){
    flush_cnt_r := 0.U
  }.elsewhen(state_r === sFlush){
    flush_cnt_r := Mux(
     flush_fired,
      flush_cnt_r + 1.U,
      flush_cnt_r
    )
  }

  icache_flush_request_o.bits.addr := Cat(pte_r.ppn, flush_cnt_r)
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
  val wait_which = Mux(pte_r.permission =/= 2.U, dcache_wb_queue_empty_i, icache_wb_queue_empty_i)

  // A DMA that moves data from the DRAM to page buffer.
  class page_buffer_write_request_t extends Bundle {
    val addr = UInt(10.W) // TODO: Make external and let it becomes a parameter.
    val data = UInt(512.W) // TODO: Make external.
  }
  val page_buffer_write_o = IO(Decoupled(new page_buffer_write_request_t))

  val u_read_dma = Module(new DMAController.Frontend.AXI4Reader(36, 512))
  val M_AXI = IO(new DMAController.Bus.AXI4(36, 512))
  u_read_dma.io.bus <> M_AXI
  u_read_dma.io.xfer.address := Cat(pte_r.ppn, Fill(12, 0.U(1.W)))
  u_read_dma.io.xfer.length := 64.U
  u_read_dma.io.xfer.valid := state_r === sMove
  val dma_data_q = Queue(u_read_dma.io.dataOut, 2) // shrink the critical path.

  // A counter to control the address of the output
  // ? What if I want to delete more than one page at the same time?
  // TODO: We need a method to assign addresses to the page buffer. Maybe a stack or something else.
  // ? Actually I prefer a pointer.
  val page_buffer_addr_cnt_r = RegInit(0.U(6.W)) 
  page_buffer_write_o.bits.data := dma_data_q.bits
  page_buffer_write_o.bits.addr := page_buffer_addr_cnt_r
  page_buffer_write_o.valid := dma_data_q.valid
  dma_data_q.ready := page_buffer_write_o.ready

  when(state_r === sIdle){
    page_buffer_addr_cnt_r := 0.U
  }.elsewhen(page_buffer_write_o.fire()){
    page_buffer_addr_cnt_r := page_buffer_addr_cnt_r + 1.U
  }

  // Port to send message to QEMU
  val done_message_o = IO(Decoupled(new software_bundle.PageEvictRequest(software_bundle.QEMUMessagesType.sEvictDone)))
  done_message_o.bits.pte := pte_r
  done_message_o.valid := state_r === sSend

  // Update logic of the state machine
  switch(state_r){
    is(sIdle){
      state_r := Mux(start_r, sNotify, sIdle)
    }
    is(sNotify){
      state_r := Mux(start_message_o.fire(), sFlush, sNotify)
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
        Mux(pte_r.modified, sMove, sSend),
        sWait)
    }
    is(sMove){
      state_r := Mux(u_read_dma.io.xfer.done, sSend, sMove)
    }
    is(sSend){
      state_r := Mux(done_message_o.fire(), sIdle, sSend)
    }
  }
}

/**
 * Move page from Page buffer to the DRAM.
 * 
 * implementing function: insertPageFromQEMU
 */ 
class PageInserter extends MultiIOModule {
  import DMAController.Bus._
  val M_AXI = IO(new AXI4(36, 512))
  
  val request_i = IO(Flipped(Valid(new MemoryRequestPacket(32, 32))))
  val reply_o = IO(Output(UInt(32.W)))

  /**
   * Address    | Meaning
   * 0x0        | ppn
   * 0x1        | r:ready w: valid
   */ 

  val internal_address = request_i.bits.addr(2)
  val write_v = request_i.valid && request_i.bits.w_v

  val ppn_r = RegInit(0.U(24.W))
  val reply_r = RegInit(0.U(32.W))
  reply_o := reply_r

  val sIdle :: sBusy :: Nil = Enum(2)
  val state_r = RegInit(sIdle)

  switch(internal_address.asUInt()){
    is(0x0.U){
      reply_r := ppn_r
    }
    is(0x1.U){
      reply_r := state_r === sIdle
    }
  }

  when(write_v && internal_address === 0x0.U){
    ppn_r := request_i.bits.data
  }

  val u_write_dma = Module(new DMAController.Frontend.AXI4Writer(36, 512))
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
      addr_cnt_r := 0x3C.U // TODO: Make external
    }
    is(sBusy){
      when(read_request_o.fire() && u_write_dma.io.dataIn.ready){
        addr_cnt_r := Mux(read_done_r, addr_cnt_r, addr_cnt_r + 1.U)
        read_done_r := addr_cnt_r === 0x3FF.U
      }
    }
  }
  // It's possible that the read result has the back pressure.
  val read_reply_i = IO(Flipped(Decoupled(UInt(512.W))))
  u_write_dma.io.dataIn <> read_reply_i

  when(internal_address.asBool() === 0x1.U && state_r === sIdle && write_v){
    state_r := sBusy
  }.elsewhen(state_r === sBusy && u_write_dma.io.xfer.done){
    state_r := sIdle
  }
}

/**
 * The buffer where the interactive pages are stored.
 * It contains a normal R/W port(For page deleater and page inserter) and a AXI slave port(for QEMU)
 */ 
class PageBuffer extends MultiIOModule {
  import DMAController.Bus._
  import armflex.util._

  class page_buffer_write_request_t extends Bundle {
    val addr = UInt(10.W) // TODO: Make external and let it becomes a parameter.
    val data = UInt(512.W) // TODO: Make external.
  }
  val normal_write_request_i = IO(Flipped(Decoupled(new page_buffer_write_request_t)))
  val normal_read_request_i = IO(Flipped(Decoupled(UInt(10.W))))
  val normal_read_reply_o = IO(Decoupled(UInt(512.W)))

  val S_AXI = IO(Flipped(new AXI4(64, 512))) // TODO: Restrict the address rage of this AXI bus.

  val u_converter = Module(new AXIRAMController(64, 512))
  u_converter.S_AXI <> S_AXI

  val bramCfg = new BRAMConfig(
    512 / 8,
    8,
    1024
  )

  val u_bram = Module(new BRAM()(bramCfg))

  // Port A is for read request
  val u_port_a_arb = Module(new RRArbiter(UInt(10.W), 2))
  u_port_a_arb.io.in(0) <> normal_read_request_i
  u_port_a_arb.io.in(1).bits := u_converter.read_request_o.bits(9, 0)
  u_port_a_arb.io.in(1).valid := u_converter.read_request_o.valid //? Judge the range of the address.
  u_converter.read_request_o.ready := u_port_a_arb.io.in(1).ready

  val chosen_port = Wire(Decoupled(u_port_a_arb.io.chosen.cloneType))
  chosen_port.valid := u_port_a_arb.io.out.valid
  chosen_port.ready := u_port_a_arb.io.out.ready
  chosen_port.bits := u_port_a_arb.io.chosen

  val chosen_port_q = Queue(chosen_port, 1, true)

  u_bram.portA.ADDR := chosen_port.bits
  u_bram.portA.DI := DontCare
  u_bram.portA.EN := chosen_port.fire()
  u_bram.portA.WE := 0.U

  chosen_port_q.ready := Mux(chosen_port_q.bits === 0.U, normal_read_reply_o.ready, u_converter.read_reply_i.ready)

  normal_read_reply_o.bits := u_bram.portA.DO
  normal_read_reply_o.valid := chosen_port_q.valid && chosen_port_q.bits === 0.U

  u_converter.read_reply_i.bits := u_bram.portA.DO
  u_converter.read_reply_i.valid := chosen_port_q.valid && chosen_port_q.bits === 1.U

  // Port B is for write request

  val u_port_b_arb = Module(new RRArbiter(u_converter.write_request_o.bits.cloneType, 2))
  u_port_b_arb.io.in(0).bits.addr := normal_write_request_i.bits.addr
  u_port_b_arb.io.in(0).bits.data := normal_write_request_i.bits.data
  u_port_b_arb.io.in(0).bits.mask := Fill(512 / 8, 1.U(1.W))
  u_port_b_arb.io.in(1) <> u_converter.write_request_o

  u_bram.portB.ADDR := u_port_b_arb.io.out.bits.addr
  u_bram.portB.DI := u_port_b_arb.io.out.bits.data
  u_bram.portB.EN := u_port_b_arb.io.out.valid
  u_bram.portB.WE := u_port_b_arb.io.out.bits.mask
}

