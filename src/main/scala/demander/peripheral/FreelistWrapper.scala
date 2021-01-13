package armflex.demander.peripheral

import chisel3._
import chisel3.util._
import DMAController.Frontend._
import DMAController.Bus._

/**
 * This module basically is a buffer of a stack located in DRAM.
 * 
 * @param entryNumber the number of entry in total.
 */ 
class FreeList(
  entryNumber: Int,
) extends MultiIOModule {
  // each entry is 24 bit
  // 512 * 3 bit for each chunk -> 64 element.
  assert(entryNumber % 64 == 0)
  val chunkNumber = entryNumber / 64

  val chunk_id_r = RegInit(UInt(log2Ceil(chunkNumber).W), 0.U)

  val chunk_r = RegInit(VecInit(Seq.tabulate(64)(_.U(24.W))))
  val ptr_r = RegInit(0.U) // chunk_r(ptr_r) is the next value.

  val push_i = IO(Flipped(Decoupled(UInt(24.W))))
  val pop_o = IO(Decoupled(UInt(24.W)))
  val full_o = IO(Output(Bool()))
  val empty_o = IO(Output(Bool()))

  // The state machine
  val sIdle :: sFetch :: sStore :: sEmpty :: Nil = Enum(4)

  // The policy is that store back when full and fetch new when empty.
  val state_r = RegInit(sIdle)

  val enter_load_v = WireInit(false.B)
  val enter_store_v = WireInit(false.B)

  val u_axi_reader = Module(new AXI4Reader(64, 512))
  val u_axi_writer = Module(new AXI4Writer(64, 512))

  // The DMA's port.
  val M_AXI = IO(new AXI4(64, 512))

  M_AXI.ar <> u_axi_reader.io.bus.ar
  M_AXI.r <> u_axi_reader.io.bus.r
  u_axi_reader.io.bus.aw.awready := false.B
  u_axi_reader.io.bus.w.wready := false.B
  u_axi_reader.io.bus.b.bid := DontCare
  u_axi_reader.io.bus.b.bvalid := false.B
  u_axi_reader.io.bus.b.bresp := DontCare

  M_AXI.aw <> u_axi_writer.io.bus.aw
  M_AXI.w <> u_axi_writer.io.bus.w
  M_AXI.b <> u_axi_writer.io.bus.b
  u_axi_writer.io.bus.ar.arready := false.B
  u_axi_writer.io.bus.r.rvalid := false.B
  u_axi_writer.io.bus.r.rdata := DontCare
  u_axi_writer.io.bus.r.rid := DontCare
  u_axi_writer.io.bus.r.rlast := DontCare
  u_axi_writer.io.bus.r.rresp := DontCare

  u_axi_reader.io.xfer.address := (chunk_id_r + 1.U) * 3.U
  u_axi_reader.io.xfer.length := 3.U
  u_axi_reader.io.xfer.valid := enter_load_v

  u_axi_writer.io.xfer.address := (chunk_id_r) * 3.U
  u_axi_writer.io.xfer.length := 3.U
  u_axi_writer.io.xfer.valid := enter_store_v

  //u_axi_writer.io.dataIn.bits.

  val dma_view_of_chunk = chunk_r.asTypeOf(Vec(3, UInt(512.W)))
  val updated_dma_view_of_chunk = WireInit(dma_view_of_chunk)
  // TODO: store counter.
  val store_vr = RegInit(false.B)
  // TODO: load counter.
  val load_vr = RegInit(false.B)
  val dma_cnt_r = RegInit(UInt(2.W), 0.U)

  updated_dma_view_of_chunk(dma_cnt_r) := u_axi_reader.io.dataOut.bits
  u_axi_reader.io.dataOut.ready := load_vr && state_r === sFetch

  u_axi_writer.io.dataIn.bits := updated_dma_view_of_chunk(dma_cnt_r) 
  u_axi_writer.io.dataIn.valid := store_vr && store_vr === sStore

  // update of dma_cnt_r 
  when(load_vr && u_axi_reader.io.dataOut.fire() || store_vr && u_axi_writer.io.dataIn.fire()
  ){
    dma_cnt_r := Mux(dma_cnt_r === 2.U, 0.U, dma_cnt_r + 1.U)
  }

  // update of load_r
  when(dma_cnt_r === 2.U && u_axi_reader.io.dataOut.fire()){
    load_vr := false.B
  }.elsewhen(enter_load_v){
    load_vr := true.B
  }

  // update of store_r
  when(dma_cnt_r === 2.U && u_axi_writer.io.dataIn.fire()){
    store_vr := false.B
  }.elsewhen(enter_store_v){
    store_vr := true.B
  }

  // The state machine.
  switch(state_r){
    is(sIdle){
      when(push_i.fire() && pop_o.fire()){
        state_r := sIdle
      }.elsewhen(push_i.fire() && ptr_r === 1.U && chunk_id_r =/= 0.U){
        state_r := sStore
        enter_store_v := true.B
      }.elsewhen(pop_o.fire() && ptr_r === 63.U){
        enter_load_v := Mux(chunk_id_r =/= (chunkNumber - 1).U, true.B, false.B)
        state_r := Mux(chunk_id_r =/= (chunkNumber - 1).U, sFetch, sEmpty)
      }
    }
    is(sFetch){
      when(u_axi_reader.io.xfer.done){
        state_r := sIdle
      }
    }
    is(sStore){
      when(u_axi_writer.io.xfer.done){
        state_r := sIdle
      }
    }
    is(sEmpty){
      when(push_i.fire() && !pop_o.fire()){
        state_r := sIdle
      }
    }
  }

  // TODO: Determine the logic of pop and push.
  full_o := ptr_r === 0.U && chunk_id_r === 0.U
  empty_o := state_r === sEmpty
  pop_o.valid := state_r === sIdle
  push_i.ready := state_r === sIdle || state_r === sEmpty
  // TODO: Determine the update of these registers.
  // ptr_r
  when(push_i.fire() && pop_o.fire()){
    ptr_r := ptr_r
  }.elsewhen(push_i.fire()){
    ptr_r := ptr_r - 1.U
  }.elsewhen(pop_o.fire()){
    ptr_r := ptr_r + 1.U
  }.elsewhen(state_r === sFetch && u_axi_reader.io.xfer.done){
    ptr_r := 0.U
  }.elsewhen(state_r === sStore && u_axi_writer.io.xfer.done){
    ptr_r := 63.U
  }

  // chunk_id_r
  when(state_r === sFetch && u_axi_reader.io.xfer.done){
    chunk_id_r := chunk_id_r + 1.U
  }.elsewhen(state_r === sStore && u_axi_writer.io.xfer.done){
    chunk_id_r := chunk_id_r - 1.U
  }

  // chunk_r
  when(push_i.fire()){
    chunk_r(ptr_r) := push_i.bits
  }.elsewhen(state_r === sFetch && u_axi_reader.io.dataOut.valid){
    chunk_r := updated_dma_view_of_chunk.asTypeOf(chunk_r.cloneType)
  }

  // pop_o
  pop_o.bits := Mux(push_i.valid, push_i.bits, chunk_r(ptr_r))
}

/**
 * This modules wraps the FreeList with a memory i/o interface.
 * 
 * @param entryNumber will be passwd to the FreeList
 */ 
class FreeListWrapper(entryNumber: Int) extends MultiIOModule {
  val request_i = IO(Flipped(Valid(new MemoryRequestPacket(32, 32))))
  val reply_o = IO(Output(UInt(32.W)))
  /**
   * CSR
   * Address        |  Meaning
   * 0x0            |  R: pop is ready W: pop once
   * 0x1            |  R: push is ready W: push once
   * 0x2            |  Result register
   */ 

  val internal_address = request_i.bits.addr(3, 2)
  val write_v = request_i.valid && request_i.bits.w_v

  val pop_vr = RegInit(false.B)
  val push_vr = RegInit(false.B)
  val result_r = RegInit(0.U(24.W))

  val reply_r = RegInit(0.U(32.W))
  reply_o := reply_r

  val u_core = Module(new FreeList(entryNumber))
  val M_AXI = IO(new AXI4(64, 512))
  val full_vo = IO(Output(Bool()))
  val empty_vo = IO(Output(Bool()))
  M_AXI <> u_core.M_AXI
  full_vo <> u_core.full_o
  empty_vo <> u_core.empty_o
  // read logic
  switch(internal_address){
    is(0x0.U){
      reply_r := u_core.pop_o.valid
    }
    is(0x1.U){
      reply_r := u_core.push_i.ready
    }
    is(0x2.U){
      reply_r := result_r
    }
  }

  // Update registers.
  // pop_vr
  when(u_core.pop_o.fire()){
    pop_vr := false.B
  }.elsewhen(write_v && internal_address === 0x0.U){
    pop_vr := request_i.bits.data
  }
  u_core.pop_o.ready := pop_vr

  // push_vr
  when(u_core.push_i.fire()){
    push_vr := false.B
  }.elsewhen(write_v && internal_address === 0x1.U){
    push_vr := request_i.bits.data
  }
  u_core.push_i.valid := push_vr

  // result_r
  when(u_core.pop_o.fire()){
    result_r := u_core.pop_o.bits
  }.elsewhen(write_v && internal_address === 0x2.U){
    result_r := request_i.bits.data
  }
  u_core.push_i.bits := result_r
  
}


object FreelistWrapperVerilogEmitter extends App {
  import chisel3.stage.ChiselStage
  val s = new ChiselStage
  println(s.emitVerilog(new FreeListWrapper(128)))
}
