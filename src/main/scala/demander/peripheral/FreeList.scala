package armflex.demander.peripheral

import chisel3._
import chisel3.util._
import antmicro.Frontend._
import antmicro.Bus._
import armflex.demander.software_bundle.ParameterConstants
import armflex.util._

/**
 * This module basically is a buffer of a stack located in DRAM.
 * 
 */ 
class FreeList(
) extends MultiIOModule {
  // each entry is 32 bit
  // 512 bit for each chunk -> 16 element.
  val chunkSize = 16
  val entryNumberlog2 = ParameterConstants.dram_addr_width - 12
  val entryNumber = 1 << (entryNumberlog2)
  assert(entryNumber % chunkSize == 0)
  val chunkNumber = entryNumber / chunkSize

  // the starting address of the page bank is 0x000010000000
  // first ppn is 0x000010000
  // first ppn's entry position: 0x00001000
  val endEntryAddr: BigInt = 1 << (entryNumberlog2 + 4 - log2Ceil(chunkSize))
  val firstEntryAddr = 1 << (log2Ceil(endEntryAddr) - 12)
  
  val dram_read_ptr_r = RegInit(firstEntryAddr.U(log2Ceil(chunkNumber).W))
  val dram_write_ptr_r = RegInit((firstEntryAddr-1).U(log2Ceil(chunkNumber).W))
  val dram_history_ptr_r = RegInit(firstEntryAddr.U(log2Ceil(chunkNumber).W))

  val chunk_r = RegInit(VecInit(Seq.tabulate(chunkSize)(_.U(32.W))))
  val read_ptr_r = RegInit(0.S(log2Ceil(chunkSize * 2).W))
  val write_ptr_r = RegInit(-1.S(log2Ceil(chunkSize * 2).W))


  val push_i = IO(Flipped(Decoupled(UInt(32.W))))
  val pop_o = IO(Decoupled(UInt(32.W)))
  val full_o = IO(Output(Bool()))
  val empty_o = IO(Output(Bool()))

  // Register to buffer previous unsuccessful writing operation
  val last_push_r = Reg(UInt(32.W))
  when(push_i.fire()){
    last_push_r := push_i.bits
  }

  // The state machine
  val sIdle :: sFetch :: sStore :: Nil = Enum(3)

  // The policy is that store back when full and fetch new when empty.
  val state_r = RegInit(sFetch)

  // AXI DMA Read channel
  val M_DMA_R = IO(new AXIReadMasterIF(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))

  // AXI DMA Write channel
  val M_DMA_W = IO(new AXIWriteMasterIF(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))

  M_DMA_R.req.bits.address := Cat(dram_read_ptr_r + 0x300000.U, 0.U(6.W))
  M_DMA_R.req.bits.length := 1.U
  M_DMA_R.req.valid := state_r === sFetch && dram_read_ptr_r =/= dram_history_ptr_r

  M_DMA_W.req.bits.address := Cat(dram_write_ptr_r + 0x300000.U, 0.U(6.W))
  M_DMA_W.req.bits.length := 1.U
  M_DMA_W.req.valid := state_r === sStore

  M_DMA_R.data.ready := state_r === sFetch

  M_DMA_W.data.bits := chunk_r.asUInt()
  M_DMA_W.data.valid := state_r === sStore

  // The state machine.
  switch(state_r){
    is(sIdle){
      when(push_i.fire() && pop_o.fire()){
        state_r := sIdle
      }.elsewhen(push_i.fire() && write_ptr_r === -1.S && dram_write_ptr_r =/= firstEntryAddr.U){
        state_r := sStore
      }.elsewhen(pop_o.fire() && read_ptr_r === (chunkSize-1).S && dram_read_ptr_r =/= endEntryAddr.U){
        state_r := sFetch
      }
    }
    is(sFetch){
      when(M_DMA_R.done || dram_read_ptr_r === dram_history_ptr_r){
        state_r := sIdle
      }
    }
    is(sStore){
      when(M_DMA_W.done){
        state_r := sIdle
      }
    }
  }

  full_o := write_ptr_r === -1.S && dram_write_ptr_r === firstEntryAddr.U
  empty_o := read_ptr_r === chunkSize.S && dram_read_ptr_r === endEntryAddr.U
  pop_o.valid := state_r === sIdle && !empty_o
  push_i.ready := state_r === sIdle && !full_o
  // ptr_r. ptr always points to the effective one.
  when(push_i.fire() && pop_o.fire()){
    read_ptr_r := read_ptr_r
    write_ptr_r := write_ptr_r
  }.elsewhen(push_i.fire()){
    read_ptr_r := read_ptr_r - 1.S
    write_ptr_r := write_ptr_r - 1.S
  }.elsewhen(pop_o.fire()){
    read_ptr_r := read_ptr_r + 1.S
    write_ptr_r := write_ptr_r + 1.S
  }.elsewhen(state_r === sFetch){
    read_ptr_r := 0.S
    write_ptr_r := -1.S
  }.elsewhen(state_r === sStore){
    read_ptr_r := (chunkSize-1).S
    write_ptr_r := (chunkNumber-2).S
  }

  // update the dram counter
  when(state_r === sFetch && (dram_read_ptr_r === dram_history_ptr_r || M_DMA_R.done)){
    dram_read_ptr_r := dram_read_ptr_r + 1.U
    dram_write_ptr_r := dram_write_ptr_r + 1.U
    dram_history_ptr_r := dram_history_ptr_r + 1.U
  }.elsewhen(state_r === sStore && M_DMA_W.done){
    dram_read_ptr_r := dram_read_ptr_r - 1.U
    dram_write_ptr_r := dram_write_ptr_r - 1.U
  }

  // chunk_r
  when(push_i.fire() && write_ptr_r =/= -1.S){
    chunk_r(write_ptr_r(log2Ceil(chunkSize)-1, 0)) := push_i.bits
  }.elsewhen(state_r === sFetch && dram_read_ptr_r === dram_history_ptr_r){
    val sequence = Seq.tabulate(chunkSize)({ i =>
      Cat(dram_read_ptr_r,i.U(log2Ceil(chunkSize).W))
    })
    chunk_r := VecInit(sequence)
  }.elsewhen(M_DMA_R.data.fire()){
    chunk_r := M_DMA_R.data.bits.asTypeOf(chunk_r.cloneType)
  }.elsewhen(M_DMA_W.data.fire()){
    val leaveOne = Seq.fill(chunkSize-1)(0.U(32.W)) :+ last_push_r
    chunk_r := VecInit(leaveOne)
  }

  // pop_o
  pop_o.bits := Mux(push_i.valid, push_i.bits, chunk_r(read_ptr_r(log2Ceil(chunkSize)-1 ,0)))
}

object FreeListVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new FreeList()))
}
