package armflex.demander.peripheral

import chisel3._
import chisel3.util._
import DMAController.Frontend._
import DMAController.Bus._
import armflex.demander.software_bundle.ParameterConstants

/**
 * This module basically is a buffer of a stack located in DRAM.
 * 
 * @param entryNumber the number of entry in total.
 */ 
class FreeList(
  entryNumber: Int,
) extends MultiIOModule {
  // each entry is 32 bit
  // 512 bit for each chunk -> 16 element.
  val chunkSize = 16
  assert(entryNumber % chunkSize == 0)
  val chunkNumber = entryNumber / chunkSize

  // the starting address of the page bank is 0x000010000000
  // first ppn is 0x000010000
  // first ppn's entry position: 0x00001000
  val firstEntryAddr = 0x1000
  val endEntryAddr: BigInt = 0x10000000
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
  val sIdle :: sFetch :: sStore :: Nil = Enum(4)

  // The policy is that store back when full and fetch new when empty.
  val state_r = RegInit(sFetch)

  val u_axi_reader = Module(new AXI4Reader(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width,
  ))
  val u_axi_writer = Module(new AXI4Writer(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))

  // The DMA's port.
  val M_AXI = IO(new AXI4(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))

  M_AXI.ar <> u_axi_reader.io.bus.ar
  M_AXI.r <> u_axi_reader.io.bus.r
  u_axi_reader.io.bus.aw <> AXI4AW.stub(ParameterConstants.dram_addr_width)
  u_axi_reader.io.bus.w <> AXI4W.stub(ParameterConstants.dram_data_width)
  u_axi_reader.io.bus.b <> AXI4B.stub()

  M_AXI.aw <> u_axi_writer.io.bus.aw
  M_AXI.w <> u_axi_writer.io.bus.w
  M_AXI.b <> u_axi_writer.io.bus.b
  M_AXI.ar <> AXI4AR.stub(ParameterConstants.dram_addr_width)
  M_AXI.r <> AXI4R.stub(ParameterConstants.dram_data_width)

  u_axi_reader.io.xfer.address := Cat(dram_read_ptr_r + 0x30000.U, 0.U(6.W))
  u_axi_reader.io.xfer.length := 1.U
  u_axi_reader.io.xfer.valid := state_r === sFetch && dram_read_ptr_r =/= dram_history_ptr_r

  u_axi_writer.io.xfer.address := Cat(dram_write_ptr_r + 0x30000.U, 0.U(6.W))
  u_axi_writer.io.xfer.length := 1.U
  u_axi_writer.io.xfer.valid := state_r === sStore

  //u_axi_writer.io.dataIn.bits.
  u_axi_reader.io.dataOut.ready := state_r === sFetch

  u_axi_writer.io.dataIn.bits := chunk_r
  u_axi_writer.io.dataIn.valid := state_r === sStore

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
      when(u_axi_reader.io.xfer.done || dram_read_ptr_r === dram_history_ptr_r){
        state_r := sIdle
      }
    }
    is(sStore){
      when(u_axi_writer.io.xfer.done){
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
  when(state_r === sFetch && (dram_read_ptr_r === dram_history_ptr_r || u_axi_reader.io.xfer.done)){
    dram_read_ptr_r := dram_read_ptr_r + 1.U
    dram_write_ptr_r := dram_write_ptr_r + 1.U
    dram_history_ptr_r := dram_history_ptr_r + 1.U
  }.elsewhen(state_r === sStore && u_axi_writer.io.xfer.done){
    dram_read_ptr_r := dram_read_ptr_r - 1.U
    dram_write_ptr_r := dram_write_ptr_r - 1.U
  }

  // chunk_r
  when(push_i.fire() && write_ptr_r =/= -1.S){
    chunk_r(write_ptr_r(log2Ceil(chunkSize)-1, 0)) := push_i.bits
  }.elsewhen(state_r === sFetch && dram_read_ptr_r === dram_history_ptr_r){
    val sequence = Seq.tabulate(chunkSize)({ i =>
      Cat(dram_read_ptr_r,i.U(log2Ceil(chunkSize)))
    })
    chunk_r := Cat(sequence.reverse)
  }.elsewhen(u_axi_reader.io.dataOut.fire()){
    chunk_r := u_axi_reader.io.dataOut.bits.asTypeOf(chunk_r.cloneType)
  }.elsewhen(u_axi_writer.io.dataIn.fire()){
    val leaveOne = Seq.fill(chunkSize-1)(0.U(32.W)) :+ last_push_r
    chunk_r := Cat(leaveOne.reverse)
  }

  // pop_o
  pop_o.bits := Mux(push_i.valid, push_i.bits, chunk_r(read_ptr_r(log2Ceil(chunkSize)-1 ,0)))
}

