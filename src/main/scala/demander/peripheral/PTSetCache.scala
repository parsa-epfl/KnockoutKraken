package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import armflex.cache.PseudoTreeLRUCore


// The purpose of this module is:
// - Two scratchpads to buffer the PT Set.
// - One DMA to fetch one PT Set to specific buffer and moving back
// - Get the LRU Element
// - replace the LRU Element with given PTE.

/**
 * Size-fixed tag bundle for the page table.
 */ 
class PTTagPacket extends Bundle {
  val vpn = UInt(52.W)
  val process_id = UInt(16.W)
}

/**
 * Size-fixed entry bundle for the page table.
 */ 
class PTEPacket extends Bundle {
  val ppn = UInt(24.W)
  val permission = UInt(1.W)
  val modified = Bool()
}

class PTItem extends Bundle {
  val tag = new PTTagPacket
  val entry = new PTEPacket
}

// TODO: Replace previous bundles with the coresponding bundle defined in the SoftwareStruct

/**
 * One Set in the Page Table. It should contains more than one PTEs.
 * 
 * @note the size of this bundle is 96 * entryNumber
 */ 
class PTSetPacket(
  val entryNumber: Int = 16
) extends Bundle {
  val tags = Vec(entryNumber, new PTTagPacket)
  val ptes = Vec(entryNumber, new PTEPacket)
  
  val valids = UInt(entryNumber.W)
  val lru_bits = UInt(entryNumber.W)

  override def cloneType: this.type = new PTSetPacket(entryNumber).asInstanceOf[this.type]
} 

/**
 * Page table set buffer and its attached logic.
 * 
 * @param t the Chisel type of the PT Set
 * @param dmaWidth the width of DMA request.
 */ 
class PTSetBuffer(
  t: PTSetPacket,
  dmaWidth: Int = 512
) extends MultiIOModule {
  val dma_data_i = IO(Flipped(Decoupled(UInt(dmaWidth.W))))
  val requestPacketNumber = (t.getWidth / dmaWidth) + (if(t.getWidth % dmaWidth == 0) 0 else 1)
  val buffer_r = Reg(Vec(requestPacketNumber, UInt(dmaWidth.W)))

  // Load logic.
  val dma_cnt_r = RegInit(UInt(log2Ceil(requestPacketNumber).W), 0.U)
  val load_enabled_vi = IO(Input(Bool()))
  val load_vr = RegInit(Bool(), false.B)
  //load_vr := Mux(load_cnt_r === (requestPacketNumber - 1).U, false.B, load_vr)
  when(dma_cnt_r === (requestPacketNumber - 1).U && dma_data_i.fire()){
    load_vr := false.B
  }.elsewhen(load_enabled_vi){
    load_vr := true.B
  }
  // Bind updated_buffer to buffer_r
  val updated_buffer = WireInit(buffer_r)
  updated_buffer(dma_cnt_r) := dma_data_i.bits

  // Store logic.
  val dma_data_o = IO(Decoupled(UInt(dmaWidth.W)))
  val store_enable_vi = IO(Input(Bool()))
  val store_vr = RegInit(Bool(), false.B)
  when(dma_cnt_r === (requestPacketNumber-1).U && dma_data_o.fire()){
    store_vr := false.B
  }.elsewhen(store_enable_vi){
    store_vr := true.B
  }

  // update of dma_cnt_r
  when(dma_data_i.fire() && load_vr || dma_data_o.fire() && store_vr){
    dma_cnt_r := Mux(
      dma_cnt_r === (requestPacketNumber - 1).U,
      0.U,
      dma_cnt_r + 1.U
    )
  }
  
  dma_data_o.valid := store_vr
  dma_data_o.bits := buffer_r(dma_cnt_r)

  val pt_set_r = buffer_r.asTypeOf(t.cloneType)
  // Get LRU Element (Maybe available element)
  val space_index = PriorityEncoder(~pt_set_r.valids)
  val u_lru_core = Module(new PseudoTreeLRUCore(t.entryNumber))
  u_lru_core.io.encoding_i := pt_set_r.lru_bits(t.entryNumber-2, 0)
  val lru_index = u_lru_core.io.lru_o
  val lru_item = Wire(new PTItem)
  lru_item.entry := pt_set_r.ptes(lru_index)
  lru_item.tag := pt_set_r.tags(lru_index)

  class get_lru_element_response_t extends Bundle {
    val item = new PTItem
    val lru_v = Bool()
    // That lru_v is true means the item is valid. 
    // False means this set is not full and there is a available place.
  }

  val lru_element_o = IO(Output(new get_lru_element_response_t))
  lru_element_o.item := lru_item
  lru_element_o.lru_v := ~pt_set_r.valids === 0.U

  // Update LRU Element
  val updated_pt_set = WireInit(pt_set_r)
  val index_to_update = Mux(lru_element_o.lru_v, lru_index, space_index)

  val lru_element_i = IO(Flipped(Decoupled(new PTItem)))

  updated_pt_set.ptes(index_to_update) := lru_element_i.bits.entry
  updated_pt_set.tags(index_to_update) := lru_element_i.bits.tag
  u_lru_core.io.lru_i := index_to_update
  updated_pt_set.lru_bits := u_lru_core.io.encoding_o
  updated_pt_set.valids := Cat(0.U(1.W), UIntToOH(index_to_update))

  val lookup_request_i = IO(Input(new PTTagPacket))
  val hit_vector = pt_set_r.tags.zip(pt_set_r.valids.asBools()).map({
    case (tag, valid) => 
    tag.process_id === lookup_request_i.process_id && 
    tag.vpn === lookup_request_i.vpn &&
    valid
  })

  val hit_v = VecInit(hit_vector).asUInt() =/= 0.U  
  assert(PopCount(hit_vector) === 1.U || PopCount(hit_vector) === 0.U, "There should be only one hit at most!!!")

  val hit_index = OHToUInt(hit_vector)
  val lookup_reply_o = IO(Valid(new PTItem))
  lookup_reply_o.valid := hit_v
  lookup_reply_o.bits.entry := pt_set_r.ptes(hit_index)
  lookup_reply_o.bits.tag := lookup_request_i

  when(lru_element_i.valid){
    buffer_r := updated_pt_set.asTypeOf(buffer_r.cloneType)
  }.elsewhen(dma_data_i.fire()){
    buffer_r := updated_buffer
  }
  lru_element_i.ready := true.B

  dma_data_i.ready := load_vr && !lru_element_i.valid

}

/**
 * The purpose of this module is to let software rapidly handle the PT Entries in the same set.
 * It implements 5 basic functions:
 * 
 * - loadPTSet
 * - lookupPT
 * - getLRU
 * - replaceLRU
 * - syncPTSet
 */ 
class PTSetCache extends MultiIOModule {
  import DMAController.Frontend.AXI4Reader
  import DMAController.Frontend.AXI4Writer
  import DMAController.Bus._

  val u_buffer_0 = Module(new PTSetBuffer(new PTSetPacket))
  val u_buffer_1 = Module(new PTSetBuffer(new PTSetPacket))
  val u_axi_read = Module(new AXI4Reader(64, 512))
  val u_axi_write = Module(new AXI4Writer(64, 512))

  val M_AXI = IO(new AXI4(64, 512))
  M_AXI.ar <> u_axi_read.io.bus.ar
  M_AXI.r <> u_axi_read.io.bus.r
  u_axi_read.io.bus.aw.awready := false.B
  u_axi_read.io.bus.w.wready := false.B
  u_axi_read.io.bus.b.bid := DontCare
  u_axi_read.io.bus.b.bvalid := false.B
  u_axi_read.io.bus.b.bresp := DontCare
  M_AXI.aw <> u_axi_write.io.bus.aw
  M_AXI.w <> u_axi_write.io.bus.w
  M_AXI.b <> u_axi_write.io.bus.b

  u_axi_write.io.bus.ar.arready := false.B
  u_axi_write.io.bus.r.rvalid := false.B
  u_axi_write.io.bus.r.rdata := DontCare
  u_axi_write.io.bus.r.rid := DontCare
  u_axi_write.io.bus.r.rlast := DontCare
  u_axi_write.io.bus.r.rresp := DontCare

  u_buffer_0.dma_data_i.bits := u_axi_read.io.dataOut.bits
  u_buffer_1.dma_data_i.bits := u_axi_read.io.dataOut.bits

  /**
   * Memory-mapped CSRs:
   * Address    | CSR
   * [0x0, 0x1] | VPN
   * 0x2        | process id
   * 0x3        | Index
   * # Start DMA to load
   * 0x4        | W: start load, R: load is ready
   * # Start DMA to store
   * 0x5        | W: Start store, R: store is ready
   * # Lookup result
   * 0x6        | PPN
   * 0x7        | Permission
   * 0x8        | Modified
   * 0x9        | R: Lookup is hit W: trigger lookup
   * # get LRU
   * 0xA        | R: LRU is valid, W: trigger LRU
   * # replace 
   * 0xB        | R: 1, W: replace LRU
   */ 
  val request_i = IO(Flipped(Valid(new MemoryRequestPacket(32, 32))))
  val reply_o = IO(Output(UInt(32.W)))
  val reply_r = Reg(UInt(32.W))

  val tag_r = Reg(new PTTagPacket)
  val pte_r = Reg(new PTEPacket)
  val buffer_index_r = RegInit(UInt(1.W), 0.U)

  val internal_address = request_i.bits.addr(5,2)
  // Read logic
  switch(internal_address){
    is(0x0.U){
      reply_r := tag_r.vpn(31, 0)
    }
    is(0x1.U){
      reply_r := tag_r.vpn(51, 32)
    }
    is(0x2.U){
      reply_r := tag_r.process_id
    }
    is(0x3.U){
      reply_r := buffer_index_r
    }
    is(0x4.U){
      reply_r := u_axi_read.io.xfer.done
    }
    is(0x5.U){
      reply_r := u_axi_write.io.xfer.done
    }
    is(0x6.U){
      reply_r := pte_r.ppn
    }
    is(0x7.U){
      reply_r := pte_r.permission
    }
    is(0x8.U){
      reply_r := pte_r.modified
    }
    is(0x9.U){
      reply_r := Mux(
        buffer_index_r.asBool(),
        u_buffer_1.lookup_reply_o.valid,
        u_buffer_0.lookup_reply_o.valid
      )
    }
    is(0xA.U){
      reply_r := Mux(
        buffer_index_r.asBool(),
        u_buffer_1.lru_element_o.lru_v,
        u_buffer_0.lru_element_o.lru_v
      )
    }
    is(0xB.U){
      reply_r := true.B
    }
  }
  reply_o := reply_r

  val write_v = request_i.valid && request_i.bits.w_v
  // Status register
  val sIdle :: sDMARead :: sDMAWrite :: sLookup :: sLookupLRU :: sReplaceLRU :: Nil = Enum(6)
  val status_r = RegInit(sIdle)
  // update of status_r
  switch(status_r){
    is(sIdle){
      val transactionType = Seq(
        0x4.U -> sDMARead,
        0x5.U -> sDMAWrite,
        0x9.U -> sLookup,
        0xA.U -> sLookupLRU,
        0xB.U -> sReplaceLRU
      )
      when(write_v){
        status_r := MuxLookup(internal_address, sIdle, transactionType)
      }
    }
    is(sDMARead){
      status_r := Mux(u_axi_read.io.xfer.done, sIdle, sDMARead)
    }
    is(sDMAWrite){
      status_r := Mux(u_axi_write.io.xfer.done, sIdle, sDMAWrite)
    }
    is(sLookup){
      status_r := sIdle
    }
    is(sLookupLRU){
      status_r := sIdle
    }
    is(sReplaceLRU){
      status_r := sIdle
    }
  }

  // update tag_r
  when(write_v && internal_address === 0x0.U){
    tag_r.vpn := Cat(tag_r.vpn(51, 32), request_i.bits.data)
  }.elsewhen(write_v && internal_address === 0x1.U){
    tag_r.vpn := Cat(request_i.bits.data, tag_r.vpn(31, 0))
  }.elsewhen(write_v && internal_address === 0x2.U){
    tag_r.process_id := request_i.bits.data
  }

  // update buffer_index_r
  when(write_v && internal_address === 0x3.U){
    buffer_index_r := request_i.bits.data
  }

  // update pte_r
  when(write_v && internal_address === 0x6.U){
    pte_r.ppn := request_i.bits.data
  }.elsewhen(write_v && internal_address === 0x7.U){
    pte_r.permission := request_i.bits.data
  }.elsewhen(write_v && internal_address === 0x8.U){
    pte_r.modified := request_i.bits.data
  }.elsewhen(status_r === sLookup){
    pte_r := Mux(
      buffer_index_r.asBool(),
      u_buffer_1.lookup_reply_o.bits.entry,
      u_buffer_0.lookup_reply_o.bits.entry
    )
  }.elsewhen(status_r === sLookupLRU){
    pte_r := Mux(
      buffer_index_r.asBool(),
      u_buffer_1.lru_element_o.item.entry,
      u_buffer_0.lru_element_o.item.entry
    )
  }

  // Bind port to u_buffer_0
  u_buffer_0.lookup_request_i := tag_r
  u_buffer_0.lru_element_i.bits.tag := tag_r
  u_buffer_0.lru_element_i.bits.entry := pte_r
  u_buffer_0.lru_element_i.valid := status_r === sReplaceLRU && buffer_index_r === 0.U
  u_buffer_0.load_enabled_vi := write_v && internal_address === 0x4.U && buffer_index_r === 0.U
  u_buffer_0.dma_data_i.valid := u_axi_read.io.dataOut.valid && buffer_index_r === 0.U
  u_buffer_0.store_enable_vi := write_v && internal_address === 0x5.U && buffer_index_r === 0.U
  u_buffer_0.dma_data_o.ready := u_axi_write.io.dataIn.ready && buffer_index_r === 0.U

  // Bind port to u_buffer_1
  u_buffer_1.lookup_request_i := tag_r
  u_buffer_1.lru_element_i.bits.tag := tag_r
  u_buffer_1.lru_element_i.bits.entry := pte_r
  u_buffer_1.lru_element_i.valid := status_r === sReplaceLRU && buffer_index_r === 1.U
  u_buffer_1.load_enabled_vi := write_v && internal_address === 0x4.U && buffer_index_r === 1.U
  u_buffer_1.dma_data_i.valid := u_axi_read.io.dataOut.valid && buffer_index_r === 1.U
  u_buffer_1.store_enable_vi := write_v && internal_address === 0x5.U && buffer_index_r === 1.U
  u_buffer_1.dma_data_o.ready := u_axi_write.io.dataIn.ready && buffer_index_r === 1.U

  // Bind port to u_axi_read
  u_axi_read.io.xfer.length := u_buffer_0.requestPacketNumber.U
  u_axi_read.io.xfer.valid := write_v && internal_address === 0x4.U
  // TODO: Calculate the address of the DMA.
  u_axi_read.io.xfer.address := 0x0.U
  u_axi_read.io.dataOut.ready := Mux(
    buffer_index_r.asBool(),
    u_buffer_1.dma_data_i.ready,
    u_buffer_0.dma_data_i.ready
  )

  // Bind port to u_axi_write
  u_axi_write.io.xfer.length := u_buffer_0.requestPacketNumber.U
  u_axi_write.io.xfer.valid := write_v && internal_address === 0x5.U
  // TODO: Calculate the address of the DMA.
  u_axi_write.io.xfer.address := 0x0.U
  u_axi_write.io.dataIn.valid := Mux(
    buffer_index_r.asBool(),
    u_buffer_1.dma_data_o.valid,
    u_buffer_0.dma_data_o.valid
  )
  u_axi_write.io.dataIn.bits := Mux(
    buffer_index_r.asBool(),
    u_buffer_1.dma_data_o.bits,
    u_buffer_0.dma_data_o.bits
  )

}

object PTSetCacheVerilogEmitter extends App{
  import chisel3.stage.ChiselStage
  println((new ChiselStage).emitVerilog(new PTSetCache))
}

