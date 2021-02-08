package armflex.demander

import chisel3._
import chisel3.util._
import armflex.cache._

class TLBWritebackHandler(
  param: TLBParameter,
  tlbNumber: Int = 2
) extends MultiIOModule {
  import software_bundle._
  import DMAController.Frontend._
  import DMAController.Bus._

  // the eviction request of the TLB
  val tlb_evict_req_i = IO(Vec(
    tlbNumber, Flipped(Decoupled(new TLBEvictionMessage))
  ))
  // arbiter to select the evict request
  val u_arb = Module(new RRArbiter(new TLBEvictionMessage, tlbNumber))
  u_arb.io.in <> tlb_evict_req_i

  // lookup pid by tid
  val tt_tid_o = IO(Output(UInt(param.threadIDWidth().W)))
  tt_tid_o := u_arb.io.out.bits.tag.thread_id
  val tt_pid_i = IO(Input(UInt(ParameterConstants.process_id_width.W)))

  // Add page table set buffer and axi dma
  val u_buffer = Module(new peripheral.PageTableSetBuffer(new peripheral.PageTableSetPacket))

  val u_axi_read = Module(new AXI4Reader(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))
  u_buffer.dma_data_i <> u_axi_read.io.dataOut

  val u_axi_write = Module(new AXI4Writer(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))
  u_buffer.dma_data_o <> u_axi_write.io.dataIn

  // AXI Bus
  val M_AXI = IO(new AXI4(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))

  M_AXI.ar <> u_axi_read.io.bus.ar
  M_AXI.r <> u_axi_read.io.bus.r

  u_axi_read.io.bus.aw <> AXI4AW.stub(ParameterConstants.dram_addr_width)
  u_axi_read.io.bus.w <> AXI4W.stub(ParameterConstants.dram_data_width)
  u_axi_read.io.bus.b <> AXI4B.stub()
  
  M_AXI.aw <> u_axi_write.io.bus.aw
  M_AXI.w <> u_axi_write.io.bus.w
  M_AXI.b <> u_axi_write.io.bus.b

  u_axi_write.io.bus.ar <> AXI4AR.stub(ParameterConstants.dram_addr_width)
  u_axi_write.io.bus.r <> AXI4R.stub(ParameterConstants.dram_data_width)

  val sIdle :: sMoveIn :: sPick :: sUpdatePT :: sMoveOut :: Nil = Enum(5)
  val state_r = RegInit(sIdle)

  // sIdle
  u_arb.io.out.ready := state_r === sIdle
  class request_t extends Bundle {
    val tag = new PTTag
    val evicted_pte = new PTEntry
    val thread_id = UInt(param.threadIDWidth().W)
    val source = UInt(log2Ceil(tlbNumber).W)
  }

  val request_r = Reg(new request_t)
  when(u_arb.io.out.fire()){
    request_r.tag.vpn := u_arb.io.out.bits.tag.vpn
    request_r.tag.process_id := tt_pid_i
    request_r.evicted_pte := u_arb.io.out.bits.entry
    request_r.source := u_arb.io.chosen
    request_r.thread_id := u_arb.io.out.bits.tag.thread_id
  }

  u_buffer.load_enabled_vi := u_arb.io.out.fire()
  u_buffer.lookup_request_i := request_r.tag

  // sMoveIn
  u_axi_read.io.xfer.address := ParameterConstants.getPageTableAddressByVPN(request_r.tag.vpn)
  u_axi_read.io.xfer.length := u_buffer.requestPacketNumber.U
  u_axi_read.io.xfer.valid := state_r === sMoveIn

  // sPick
  val victim_index_r = RegInit(0.U(u_buffer.entryNumber.W))
  when(state_r === sPick){
    assert(u_buffer.lookup_reply_o.hit_v)
    victim_index_r := u_buffer.lookup_reply_o.index
  }

  val tt_pid_o = IO(Output(UInt(ParameterConstants.process_id_width.W)))
  tt_pid_o := u_buffer.lru_element_o.item.tag.process_id
  val tt_tid_i = IO(Input(new peripheral.ThreadLookupResultPacket(param.threadNumber))) // If miss, directly jump to the delete page.

  // sUpdatePT
  u_buffer.write_request_i.bits.index := victim_index_r
  u_buffer.write_request_i.bits.item.entry := request_r.evicted_pte
  u_buffer.write_request_i.bits.item.tag := request_r.tag
  u_buffer.write_request_i.valid := state_r === sUpdatePT
  u_buffer.store_enable_vi := u_buffer.write_request_i.fire()


  // sMoveOut
  u_axi_write.io.xfer.address := u_axi_read.io.xfer.address
  u_axi_write.io.xfer.length := u_buffer.requestPacketNumber.U
  u_axi_write.io.xfer.valid := state_r === sMoveOut

  switch(state_r){
    is(sIdle){
      state_r := Mux(u_arb.io.out.fire(), sMoveIn, sIdle)
    }
    is(sMoveIn){
      state_r := Mux(u_axi_read.io.xfer.done, sPick, sMoveIn)
    }
    is(sPick){
      state_r := sUpdatePT
    }
    is(sUpdatePT){
      state_r := Mux(u_buffer.write_request_i.fire(), sMoveOut, sUpdatePT)
    }
    is(sMoveOut){
      state_r := Mux(u_axi_write.io.xfer.done, sIdle, sMoveOut)
    }
  }
}

object TLBWritebackHandlerVerilogEmitter extends App{
  val c = chisel3.stage.ChiselStage
  println(c.emitVerilog(new TLBWritebackHandler(new TLBParameter)))
}
