package armflex_mmu

import chisel3._
import chisel3.util._
import armflex_cache._
import armflex.util._

class TLBWritebackHandler(
  param: PageDemanderParameter,
  tlbNumber: Int = 2
) extends MultiIOModule {
  import armflex.{PTEntryPacket, PTTagPacket, TLBEvictionMessage}
  import antmicro.Frontend._
  import antmicro.Bus._

  // the eviction request of the TLB
  val tlb_evict_req_i = IO(Vec(
    tlbNumber, Flipped(Decoupled(new TLBEvictionMessage(param.mem.toTLBParameter())))
  ))
  // arbiter to select the evict request
  val u_arb = Module(new RRArbiter(new TLBEvictionMessage(param.mem.toTLBParameter()), tlbNumber))
  u_arb.io.in <> tlb_evict_req_i

  // Add page table set buffer and axi dma
  val u_buffer = Module(new peripheral.PageTableSetBuffer(
    param.mem.toTLBParameter(),
    new peripheral.PageTableSetPacket(param.mem.toTLBParameter())
  ))

  // AXI DMA Read channels
  val M_DMA_R = IO(new AXIReadMasterIF(
    param.dramAddrWidth,
    param.dramDataWidth
  ))

  u_buffer.dma_data_i <> M_DMA_R.data

  // AXI DMA Write channels
  val M_DMA_W = IO(new AXIWriteMasterIF(
    param.dramAddrWidth,
    param.dramDataWidth
  ))

  M_DMA_W.data <> u_buffer.dma_data_o

  val sIdle :: sMoveIn :: sPick :: sUpdatePT :: sMoveOut :: Nil = Enum(5)
  val state_r = RegInit(sIdle)

  // sIdle
  u_arb.io.out.ready := state_r === sIdle
  class request_t extends Bundle {
    val tag = new PTTagPacket(param.mem.toTLBParameter())
    val evicted_pte = new PTEntryPacket(param.mem.toTLBParameter())
    val source = UInt(log2Ceil(tlbNumber).W)
  }

  val request_r = Reg(new request_t)
  when(u_arb.io.out.fire()){
    request_r.tag := u_arb.io.out.bits.tag
    request_r.evicted_pte := u_arb.io.out.bits.entry
    request_r.source := u_arb.io.chosen
    assert(u_arb.io.out.bits.entry.modified, "Only modified entry can be written back")
  }

  u_buffer.lookup_request_i := request_r.tag

  // sMoveIn
  M_DMA_R.req.bits.address := param.getPageTableAddressByVPN(request_r.tag.vpn)
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
      state_r := Mux(u_arb.io.out.fire(), sMoveIn, sIdle)
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
  println(c.emitVerilog(new TLBWritebackHandler(new PageDemanderParameter())))
}
