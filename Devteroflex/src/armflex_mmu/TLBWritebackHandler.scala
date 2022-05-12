package armflex_mmu

import chisel3._
import chisel3.util._
import armflex_cache._
import armflex.util._

class TLBWritebackHandler(
  params: MemoryHierarchyParams,
  tlbNumber: Int = 2
) extends Module {
  import armflex.{PTEntryPacket, PTTagPacket, TLBEvictionMessage}

  // the eviction request of the TLB
  val tlb_evict_req_i = IO(Vec(
    tlbNumber, Flipped(Decoupled(new TLBEvictionMessage(params.getPageTableParams)))
  ))
  // arbiter to select the evict request
  val u_arb = Module(new RRArbiter(new TLBEvictionMessage(params.getPageTableParams), tlbNumber))
  u_arb.io.in <> tlb_evict_req_i

  // Add page table set buffer and axi dma
  val u_buffer = Module(new peripheral.PageTableSetBuffer(params.getPageTableParams,
                        new peripheral.PageTableSetPacket(params.getPageTableParams)))

  // AXI DMA Read channels
  val M_DMA_R = IO(new AXIReadMasterIF(params.dramAddrW, params.dramdataW))
  // AXI DMA Write channels
  val M_DMA_W = IO(new AXIWriteMasterIF(params.dramAddrW, params.dramdataW))

  u_buffer.dma_data_i <> M_DMA_R.data
  M_DMA_W.data <> u_buffer.dma_data_o

  val sIdle :: sMoveIn :: sPick :: sUpdatePT :: sMoveOut :: Nil = Enum(5)
  val state_r = RegInit(sIdle)

  // sIdle
  u_arb.io.out.ready := state_r === sIdle
  class request_t extends Bundle {
    val tag = new PTTagPacket(params.getPageTableParams)
    val evicted_pte = new PTEntryPacket(params.getPageTableParams)
    val source = UInt(log2Ceil(tlbNumber).W)
  }

  val request_r = Reg(new request_t)
  when(u_arb.io.out.fire){
    request_r.tag := u_arb.io.out.bits.tag
    request_r.evicted_pte := u_arb.io.out.bits.entry
    request_r.source := u_arb.io.chosen
  }

  u_buffer.lookup_request_i := request_r.tag

  // sMoveIn
  M_DMA_R.req.bits.address := params.vpn2ptSetPA(request_r.tag.asid, request_r.tag.vpn, params.getPageTableParams.ptAssociativity)
  M_DMA_R.req.bits.length := u_buffer.requestPacketNumber.U
  M_DMA_R.req.valid := state_r === sMoveIn

  // sPick
  val victim_index_r = RegInit(0.U(params.getPageTableParams.ptAssociativity.W))
  when(state_r === sPick){
    victim_index_r := u_buffer.lookup_reply_o.index
  }

  // sUpdatePT
  u_buffer.write_request_i.bits.flush_v := false.B
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
      when(u_arb.io.out.fire) {
        state_r := sMoveIn
      }
    }
    is(sMoveIn){
      when(M_DMA_R.done) {
        state_r := sPick
      }
    }
    is(sPick){
      state_r := sUpdatePT
    }
    is(sUpdatePT){
      when(u_buffer.write_request_i.fire) {
        state_r := sMoveOut
      }
    }
    is(sMoveOut){
      when(M_DMA_W.done) {
        state_r := sIdle
      }
    }
  }

  if(true) { // TODO Conditional asserts
    when(state_r === sPick){
      when(u_buffer.lookup_reply_o.hit_v) {
        printf("If picking entry, must be a hit")
        //assert(u_buffer.lookup_reply_o.hit_v, "If picking entry, must be a hit")
      }
    }
    when(u_arb.io.out.fire){
      assert(u_arb.io.out.bits.entry.modified, "Only modified entry can be written back")
    }
  }
}

object TLBWritebackHandlerVerilogEmitter extends App{
  val c = chisel3.stage.ChiselStage
  println(c.emitVerilog(new TLBWritebackHandler(new MemoryHierarchyParams())))
}
