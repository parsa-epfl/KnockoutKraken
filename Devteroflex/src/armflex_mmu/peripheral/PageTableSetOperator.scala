package armflex_mmu.peripheral

import _root_.armflex_mmu._

import armflex.util._
import armflex.{PTEntryPacket, PTTagPacket, PageTableItem}
import chisel3._
import chisel3.util._
import armflex_cache.{PseudoTreeLRUCore, PageTableParams}

/**
 * Structures that keeps Page Table entries in DRAM.
 * 
 * It's set associative cache with PseudoLRU policy.
 * 
 * @note the size of this bundle is 96 * params.ptAssociativity
 */ 
class PageTableSetPacket(val params: PageTableParams) extends Bundle {
  val entries = Vec(params.ptAssociativity, new PageTableItem(params))
  val valids = UInt(params.ptAssociativity.W)
  val lru_bits = UInt(params.ptAssociativity.W)
}

object PageTableOps {
  val opInsert :: opLookup :: opEvict :: Nil = Enum(3)
  def opType = opInsert.cloneType
}

class PageTableReq(params: PageTableParams) extends Bundle {
  val entry = new PageTableItem(params)
  val op = PageTableOps.opType
  val thid = UInt(log2Ceil(params.thidN).W)
  val thid_v = Bool()
}

import PageTableOps._

/**
 * Operator for accessing Page Table Set
 * - Insert
 * - Lookup
 * - Evict
 * 
 * Updates LRU, solves conflicts, and accesses DRAM
 */
class PageTableSetOperator(params: PageTableParams, dramAddrW: Int, dramDataW: Int) extends Module {
  val DMA = IO(new Bundle {
    val rd = new ReadPort(dramAddrW, dramDataW)
    val wr = new WritePort(dramAddrW, dramDataW)
  })
  
  val PORT = IO(new Bundle {
    val req = Flipped(Decoupled(new PageTableReq(params)))
    val resp = Decoupled(Valid(new PageTableItem(params)))
  })

  val sIdle :: sRdDMAAddr :: sRdDramSet :: sCheckSet :: sRespOp :: sWrDMAAddr :: sWrDramSet :: Nil = Enum(7)
  val state_r = RegInit(sIdle)
  
  // Working Page Table Item from req
  val requestOp = RegInit(opInsert)
  val requestPageTableItem_r = RegInit(0.U.asTypeOf(PORT.req.bits.entry))
  val respPageTableItem_r = RegInit(0.U.asTypeOf(PORT.req.bits.entry))
  val isEvicted_r = RegInit(false.B)
  val isHit_r = RegInit(false.B)
  
  PORT.resp.bits.bits := respPageTableItem_r
  when(requestOp === opInsert) {
    PORT.resp.bits.valid := isEvicted_r
  }.elsewhen(requestOp === opLookup) {
    PORT.resp.bits.valid := isHit_r
  }.elsewhen(requestOp === opEvict) {
    PORT.resp.bits.valid := isEvicted_r
  }.otherwise {
    PORT.resp.bits.valid := false.B
  }
  
  // Extract working PageTableSet from buffer
  val workingPageTableSet_w = Wire(new PageTableSetPacket(params))
  val updatedPageTableSet_w = WireInit(workingPageTableSet_w)
  val pageTableSetSize = workingPageTableSet_w.getWidth
  val requestPacketNumber = Math.ceil(pageTableSetSize.toDouble/dramDataW).toInt

  // For DMA read
  val blockBuffer_r = Reg(Vec(requestPacketNumber, UInt(dramDataW.W)))
  val currBlock_r = RegInit(0.U(log2Ceil(requestPacketNumber).W))
  val isLastBlock = currBlock_r === (requestPacketNumber - 1).U
  workingPageTableSet_w := blockBuffer_r.asTypeOf(workingPageTableSet_w)
  assert(requestPacketNumber == 3, s"Current size is: ${requestPacketNumber} PageTablePacket: ${pageTableSetSize}\n")
  
  // Check for hit with working entry
  val hitVector = VecInit(workingPageTableSet_w.entries zip workingPageTableSet_w.valids.asBools map {
    case (entry, valid) => valid && 
    entry.tag.asUInt === requestPageTableItem_r.tag.asUInt
  })
  assert(hitVector.length == workingPageTableSet_w.valids.asBools.length)
  val hitVector_w = WireInit(UInt(hitVector.length.W), hitVector.asUInt)
  val isHit_w = WireInit(hitVector_w.orR)
  val hitEntryIdx = WireInit(OHToUInt(hitVector_w))

  // Check for eviction: Hit + invalidate
  val evictDone = WireInit(false.B)
  
  // Replace on LRU entry
  val insertDone = WireInit(false.B)
  val lookupDone = WireInit(false.B)
  val uLruCore = Module(new PseudoTreeLRUCore(params.ptAssociativity))
  val lruOfWorkingSet = WireInit(uLruCore.io.lru_o)
  val lruNextEncoding = WireInit(uLruCore.io.encoding_o)
  uLruCore.io.encoding_i := workingPageTableSet_w.lru_bits
  uLruCore.io.access_i := Mux(isHit_w, hitEntryIdx, lruOfWorkingSet)
  val nextVecValid_w = WireInit(VecInit(workingPageTableSet_w.valids.asBools))

  when(state_r === sIdle) {
    isHit_r := false.B
    isEvicted_r := false.B

  }.elsewhen(insertDone) {
    isHit_r := isHit_w

    when(!isHit_w) {
      // Replace new entry
      nextVecValid_w(lruOfWorkingSet) := true.B
      updatedPageTableSet_w.entries(lruOfWorkingSet) := requestPageTableItem_r
      updatedPageTableSet_w.valids := nextVecValid_w.asUInt
    
      // Evict old entry
      respPageTableItem_r := workingPageTableSet_w.entries(lruOfWorkingSet)
      isEvicted_r := workingPageTableSet_w.valids(lruOfWorkingSet).asBool
    }.otherwise {
      // Update already present entry
      updatedPageTableSet_w.entries(hitEntryIdx) := requestPageTableItem_r
    }
    
    // Update entry of page table set
    updatedPageTableSet_w.lru_bits := lruNextEncoding
    blockBuffer_r := updatedPageTableSet_w.asTypeOf(blockBuffer_r)

  }.elsewhen(lookupDone) {
    // Get hit entry
    respPageTableItem_r := workingPageTableSet_w.entries(hitEntryIdx)
    isHit_r := isHit_w
    
    // Update LRU access of PageTableSet
    updatedPageTableSet_w.lru_bits := lruNextEncoding
    blockBuffer_r := updatedPageTableSet_w.asTypeOf(blockBuffer_r)

  }.elsewhen(evictDone) {
    // Check if eviction is working
    isHit_r := isHit_w

    // Evict old entry
    respPageTableItem_r := workingPageTableSet_w.entries(hitEntryIdx)
    isEvicted_r := isHit_w
 
    // Update valid bits of PageTableSet
    nextVecValid_w(hitEntryIdx) := false.B
    updatedPageTableSet_w.valids := nextVecValid_w.asUInt
    blockBuffer_r := updatedPageTableSet_w.asTypeOf(blockBuffer_r)
  }
  
  when(DMA.rd.data.fire || DMA.wr.data.fire) {
    currBlock_r := currBlock_r + 1.U
    when(isLastBlock) {
      currBlock_r := 0.U
    }
    
    when(DMA.rd.data.valid) {
      blockBuffer_r(currBlock_r) := DMA.rd.data.bits
    }
  }
  
  val setAddr = WireInit(params.vpn2ptSetPA(requestPageTableItem_r.tag.asid, requestPageTableItem_r.tag.vpn, params.ptAssociativity))
  
  DMA.rd.req.bits.addr := setAddr
  DMA.rd.req.bits.burst := requestPacketNumber.U
  DMA.rd.req.bits.w_en := 0.U
  DMA.rd.req.valid := state_r === sRdDMAAddr

  DMA.rd.data.ready := state_r === sRdDramSet
  
  DMA.wr.req.bits.addr := setAddr
  DMA.wr.req.bits.burst := requestPacketNumber.U
  DMA.wr.req.bits.w_en := Fill(DMA.wr.req.bits.w_en.getWidth, 1.U)
  DMA.wr.req.valid := state_r === sWrDMAAddr

  DMA.wr.data.valid := state_r === sWrDramSet
  DMA.wr.data.bits := blockBuffer_r(currBlock_r)
  
  PORT.req.ready := state_r === sIdle
  PORT.resp.valid := state_r === sRespOp
  switch(state_r) {
    is(sIdle) {
      when(PORT.req.fire) {
        requestPageTableItem_r := PORT.req.bits.entry
        requestOp := PORT.req.bits.op
        state_r := sRdDMAAddr
      }
    }

    is(sRdDMAAddr) {
      when(DMA.rd.req.fire) {
        state_r := sRdDramSet
      }
    }
    
    is(sRdDramSet) {
      when(DMA.rd.data.fire) {
        when(isLastBlock) {
          state_r := sCheckSet
        }
      } 
    }
    
    is(sCheckSet) {
      when(requestOp === opInsert) {
        insertDone := true.B
      }.elsewhen(requestOp === opLookup) {
        lookupDone := true.B
      }.elsewhen(requestOp === opEvict) {
        evictDone := true.B
      }
      state_r := sRespOp
    }
    
    is(sRespOp) {
      when(PORT.resp.fire) {
        state_r := sWrDMAAddr
      }
    }

    is(sWrDMAAddr) {
      when(DMA.wr.req.fire) {
        state_r := sWrDramSet
      }
    }
    
    is(sWrDramSet) {
      when(DMA.wr.data.fire) {
        when(isLastBlock) {
          state_r := sIdle
        }
      }
    }
  }

  val oILA = IO(new Bundle {
    val ptAccessReq = Output(DMA.rd.req.cloneType)
    val ptes = Output(blockBuffer_r.cloneType)
    val pteBufferState = Output(state_r.cloneType)
    val pteHitVec = Output(hitVector_w.cloneType)
  })

  oILA.ptAccessReq := DMA.rd.req
  oILA.ptes := blockBuffer_r
  oILA.pteBufferState := state_r
  oILA.pteHitVec := hitVector_w

  if(true) { // TODO Conditional asserts
    val multiple_hit = !(PopCount(hitVector_w) === 1.U || PopCount(hitVector_w) === 0.U)
    when(multiple_hit && state_r === sCheckSet) {
      printf(p"Multiple set hits detected:v[${Hexadecimal(workingPageTableSet_w.valids)}]:${workingPageTableSet_w.entries.map{entry => (entry.tag.asid, entry.tag.vpn)}}\n")
      assert(false.B, "There should be only one hit at most!!!")
    }
  }
}
