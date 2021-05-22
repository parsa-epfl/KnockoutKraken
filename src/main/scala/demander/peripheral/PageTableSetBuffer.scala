package armflex.demander.peripheral

import armflex.{PTEntryPacket, PTTagPacket, PageTableItem}
import chisel3._
import chisel3.util._
import armflex.cache.PseudoTreeLRUCore
import armflex.cache.TLBParameter


// The purpose of this module is:
// - Two scratchpads to buffer the PT Set.
// - One DMA to fetch one PT Set to specific buffer and moving back
// - Get the LRU Element
// - replace the LRU Element with given PTE.

/**
 * One Set in the Page Table. It should contains more than one PTEs.
 * 
 * @note the size of this bundle is 96 * entryNumber
 */ 
class PageTableSetPacket(
  param: TLBParameter,
  val entryNumber: Int = 16
) extends Bundle {
  val tags = Vec(entryNumber, new PTTagPacket(param))
  val ptes = Vec(entryNumber, new PTEntryPacket(param))
  
  val valids = UInt(entryNumber.W)
  val lru_bits = UInt(entryNumber.W)

  override def cloneType: this.type = new PageTableSetPacket(param, entryNumber).asInstanceOf[this.type]
} 

class PageSetBufferWriteRequestPacket(
  param: TLBParameter,
  entryNumber: Int = 16
) extends Bundle {
  val item = new PageTableItem(param)
  val index = UInt(log2Ceil(entryNumber).W)

  override def cloneType: this.type = new PageSetBufferWriteRequestPacket(param, entryNumber).asInstanceOf[this.type]
}

class PageSetBufferLookupReplyPacket(
  param: TLBParameter,
  entryNumber: Int = 16
) extends Bundle {
  val item = new PageTableItem(param)
  val index = UInt(log2Ceil(entryNumber).W)
  val hit_v = Bool()

  override def cloneType: this.type = new PageSetBufferLookupReplyPacket(param, entryNumber).asInstanceOf[this.type]
}

/**
 * Page table set buffer and its attached logic.
 * 
 * @param t the Chisel type of the PT Set
 */ 
class PageTableSetBuffer(
  param: TLBParameter,
  t: PageTableSetPacket,
) extends MultiIOModule {
  val dma_data_i = IO(Flipped(Decoupled(UInt(512.W))))
  val entryNumber = t.entryNumber
  val requestPacketNumber = (entryNumber / 16) * 3
  val buffer_r = Reg(Vec(requestPacketNumber, UInt(512.W)))

  // Load logic.
  val dma_cnt_r = RegInit(UInt(log2Ceil(requestPacketNumber).W), 0.U)
  // Bind updated_buffer to buffer_r
  val updated_buffer = WireInit(buffer_r)
  updated_buffer(dma_cnt_r) := dma_data_i.bits

  val sIdle :: sMoveIn :: sMoveOut :: Nil = Enum(3)
  val state_r = RegInit(sIdle)

  // Store logic.
  val dma_data_o = IO(Decoupled(UInt(512.W)))

  // update of dma_cnt_r
  when(dma_data_i.fire() || dma_data_o.fire()){
    dma_cnt_r := Mux(
      dma_cnt_r === (requestPacketNumber - 1).U,
      0.U,
      dma_cnt_r + 1.U
    )
  }
  
  dma_data_o.valid := state_r === sMoveOut || state_r === sIdle
  dma_data_o.bits := buffer_r(dma_cnt_r)

  val pt_set_r = buffer_r.asTypeOf(t.cloneType)
  // Get LRU Element (Maybe available element)
  val space_index = PriorityEncoder(~pt_set_r.valids)
  val u_lru_core = Module(new PseudoTreeLRUCore(t.entryNumber))
  u_lru_core.io.encoding_i := pt_set_r.lru_bits(t.entryNumber-2, 0)
  val lru_index = u_lru_core.io.lru_o
  val lru_item = Wire(new PageTableItem(param))
  lru_item.entry := pt_set_r.ptes(lru_index)
  lru_item.tag := pt_set_r.tags(lru_index)

  class get_lru_element_response_t extends Bundle {
    val item = new PageTableItem(param)
    val lru_v = Bool()
    val index = UInt(log2Ceil(t.entryNumber).W)
    // That lru_v is true means the item is valid. 
    // False means this set is not full and there is a available place.
  }

  val lru_element_o = IO(Output(new get_lru_element_response_t))
  lru_element_o.item := lru_item
  lru_element_o.lru_v := ~pt_set_r.valids === 0.U
  lru_element_o.index := Mux(lru_element_o.lru_v, lru_index, space_index)

  // Update LRU Element
  val updated_pt_set = WireInit(pt_set_r)

  // val lru_element_i = IO(Flipped(Decoupled(new software_bundle.PageTableItem)))
  val write_request_i = IO(Flipped(Decoupled(new PageSetBufferWriteRequestPacket(param, t.entryNumber))))

  updated_pt_set.ptes(write_request_i.bits.index) := write_request_i.bits.item.entry
  updated_pt_set.tags(write_request_i.bits.index) := write_request_i.bits.item.tag

  u_lru_core.io.lru_i := write_request_i.bits.index
  updated_pt_set.lru_bits := u_lru_core.io.encoding_o
  updated_pt_set.valids := Cat(0.U(1.W), UIntToOH(write_request_i.bits.index))

  val lookup_request_i = IO(Input(new PTTagPacket(param)))
  val hit_vector = pt_set_r.tags.zip(pt_set_r.valids.asBools()).map({
    case (tag, valid) => 
    tag.asid === lookup_request_i.asid && 
    tag.vpn === lookup_request_i.vpn &&
    valid
  })

  val hit_v = VecInit(hit_vector).asUInt() =/= 0.U  
  assert(PopCount(hit_vector) === 1.U || PopCount(hit_vector) === 0.U, "There should be only one hit at most!!!")

  val hit_index = OHToUInt(hit_vector)
  val lookup_reply_o = IO(Output(new PageSetBufferLookupReplyPacket(param)))
  lookup_reply_o.hit_v := hit_v
  lookup_reply_o.item.entry := pt_set_r.ptes(hit_index)
  lookup_reply_o.item.tag := lookup_request_i
  lookup_reply_o.index := hit_index

  when(write_request_i.fire() && !dma_data_i.fire()){
    buffer_r := updated_pt_set.asTypeOf(buffer_r.cloneType)
  }.elsewhen(dma_data_i.fire()){
    buffer_r := updated_buffer
  }

  write_request_i.ready := state_r === sIdle
  dma_data_i.ready := state_r === sMoveIn || (state_r === sIdle && !write_request_i.fire())

  switch(state_r){
    is(sIdle){
      when(dma_data_i.fire()){
        state_r := sMoveIn
      }.elsewhen(dma_data_o.fire()){
        state_r := sMoveOut
      }.otherwise {
        state_r := sIdle
      }
    }
    is(sMoveIn){
      state_r := Mux(dma_data_i.fire() && dma_cnt_r === (requestPacketNumber - 1).U, sIdle, sMoveIn)
    }
    is(sMoveOut){
      state_r := Mux(dma_data_o.fire() && dma_cnt_r === (requestPacketNumber - 1).U, sIdle, sMoveOut)

    }
  }

}

object PageTableSetBufferVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new PageTableSetBuffer(new TLBParameter, new PageTableSetPacket(new TLBParameter))))
}
