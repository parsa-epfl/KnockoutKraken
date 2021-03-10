package armflex.demander

import chisel3._
import chisel3.util._
import armflex.cache._
import armflex.util._

class TLBWritebackHandler(
  param: TLBParameter,
  tlbNumber: Int = 2
) extends MultiIOModule {
  import software_bundle._
  import antmicro.Frontend._
  import antmicro.Bus._

  // the eviction request of the TLB
  val tlb_evict_req_i = IO(Vec(
    tlbNumber, Flipped(Decoupled(new TLBEvictionMessage(param)))
  ))
  // arbiter to select the evict request
  val u_arb = Module(new RRArbiter(new TLBEvictionMessage(param), tlbNumber))
  u_arb.io.in <> tlb_evict_req_i

  // lookup pid by tid
  val tt_tid_o = IO(Output(UInt(param.threadIDWidth().W)))
  tt_tid_o := u_arb.io.out.bits.tag.thread_id
  val tt_pid_i = IO(Input(Valid(UInt(ParameterConstants.process_id_width.W))))

  // Add page table set buffer and axi dma
  val u_buffer = Module(new peripheral.PageTableSetBuffer(new peripheral.PageTableSetPacket))

  // AXI DMA Read channels
  val M_DMA_R = IO(new AXIReadMasterIF(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))

  u_buffer.dma_data_i <> M_DMA_R.data

  // AXI DMA Write channels
  val M_DMA_W = IO(new AXIWriteMasterIF(
    ParameterConstants.dram_addr_width,
    ParameterConstants.dram_data_width
  ))

  M_DMA_W.data <> u_buffer.dma_data_o

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
    request_r.tag.vpn := u_arb.io.out.bits.tag.vpage
    request_r.tag.process_id := tt_pid_i.bits
    request_r.evicted_pte := u_arb.io.out.bits.entry
    request_r.source := u_arb.io.chosen
    request_r.thread_id := u_arb.io.out.bits.tag.thread_id
    assert(u_arb.io.out.bits.entry.modified, "Only modified entry can be written back")
  }

  u_buffer.lookup_request_i := request_r.tag

  // sMoveIn
  M_DMA_R.req.bits.address := ParameterConstants.getPageTableAddressByVPN(request_r.tag.vpn)
  M_DMA_R.req.bits.length := u_buffer.requestPacketNumber.U
  M_DMA_R.req.valid := state_r === sMoveIn

  // sPick
  val victim_index_r = RegInit(0.U(u_buffer.entryNumber.W))
  when(state_r === sPick){
    assert(u_buffer.lookup_reply_o.hit_v)
    victim_index_r := u_buffer.lookup_reply_o.index
  }

  // sUpdatePT
  u_buffer.write_request_i.bits.index := victim_index_r
  u_buffer.write_request_i.bits.item.entry := request_r.evicted_pte
  u_buffer.write_request_i.bits.item.tag := request_r.tag
  u_buffer.write_request_i.valid := state_r === sUpdatePT


  // sMoveOut
  M_DMA_W.req.bits.address := M_DMA_R.req.bits.address
  M_DMA_W.req.bits.length := u_buffer.requestPacketNumber.U
  M_DMA_W.req.valid := state_r === sMoveOut

  switch(state_r){
    is(sIdle){
      state_r := Mux(u_arb.io.out.fire() && tt_pid_i.valid, sMoveIn, sIdle)
    }
    is(sMoveIn){
      state_r := Mux(M_DMA_R.done, sPick, sMoveIn)
    }
    is(sPick){
      state_r := sUpdatePT
    }
    is(sUpdatePT){
      state_r := Mux(u_buffer.write_request_i.fire(), sMoveOut, sUpdatePT)
    }
    is(sMoveOut){
      state_r := Mux(M_DMA_W.done, sIdle, sMoveOut)
    }
  }
}

object TLBWritebackHandlerVerilogEmitter extends App{
  val c = chisel3.stage.ChiselStage
  println(c.emitVerilog(new TLBWritebackHandler(new TLBParameter)))
}
