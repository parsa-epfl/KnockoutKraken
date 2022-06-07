package armflex_mmu.peripheral

import armflex.{PTTagPacket, PageEvictNotification, PageTableItem, PipeMMUIO, QEMUMessagesType}
import armflex_cache._
import armflex_mmu._
import chisel3._
import chisel3.util._

import armflex.MemoryAccessType._
import os.stat

/**
 * Delete a page according to the given PTE.
 * Replated function: movePageToQEMU
 * 
 * This module will:
 * 0. Wait for the Ack signal from LSU so that all pending requests are finished.
 * 1. Flush TLB. If hit and dirty, update the entry.
 * 2. Notify QEMU that an eviction will start.
 * 3. Flush I$ and D$ according to the property of this page.
 * 4. Wait for the writing list to complete
 * 5. Send notification signal to the LSU so that LSU knows the flush is complete.
 * 6. Send message to QEMU that the eviction is complete
 * 
 * @param params the parameter of the MemorySystem
 */ 

class PageTableEntryDeletorPort(params: MemoryHierarchyParams) extends Bundle {
  val req = Flipped(Decoupled(new PageTableItem(params.getPageTableParams)))
  val resp = Decoupled(new PageTableItem(params.getPageTableParams))
}

class PageTableEntryDeletor(params: MemoryHierarchyParams) extends Module {
  val sIdle :: sReqPipePerm :: sFlushTLB :: sEvictCacheBlocks :: sWaitWbLatency :: sWaitWbEmpty :: sReleasePipePerm :: sComplete ::  Nil = Enum(8)
  val state_r = RegInit(sIdle)

  // Receive flush requests
  val PORT = IO(new PageTableEntryDeletorPort(params))

  // Request flush permission to pipeline before working
  val MMU_FLUSH_REQ_PIPELINE_IO = IO(Flipped(new PipeMMUIO))

  // Flush TLB
  val MMU_FLUSH_TLB_IO = IO(new Bundle {
    val inst = Flipped(new FlushTLBIO(params.getPageTableParams))
    val data = Flipped(new FlushTLBIO(params.getPageTableParams))
  })

  // Flush Cache
  val MMU_FLUSH_CACHE_IO = IO(new Bundle {
    val inst = Flipped(new CacheMMUIO(params.getCacheParams))
    val data = Flipped(new CacheMMUIO(params.getCacheParams))
  })

  // Get flush request
  val workingPageTableEntry = Reg(PORT.req.bits.cloneType)
  PORT.req.ready := state_r === sIdle
  when(PORT.req.fire) {
    workingPageTableEntry := PORT.req.bits
  }

  // Flush request completed
  PORT.resp.valid := state_r === sComplete
  PORT.resp.bits := workingPageTableEntry

  // To synchronize flushing both Data and Inst paths
  val reqInstDone_r = RegInit(false.B)
  val reqDataDone_r = RegInit(false.B)
  val respInstDone_r = RegInit(false.B)
  val respDataDone_r = RegInit(false.B)
  val reqBothDone = WireInit(reqInstDone_r && reqDataDone_r)
  val respBothDone = WireInit(respInstDone_r && respDataDone_r)
  // Request Pipeline flush permission
  MMU_FLUSH_REQ_PIPELINE_IO.inst.flushPermReq.valid := state_r === sReqPipePerm && workingPageTableEntry.entry.perm === INST_FETCH.U && !reqInstDone_r
  MMU_FLUSH_REQ_PIPELINE_IO.data.flushPermReq.valid := state_r === sReqPipePerm && !reqDataDone_r
  when(MMU_FLUSH_REQ_PIPELINE_IO.inst.flushPermReq.fire) { reqInstDone_r := true.B }
  when(MMU_FLUSH_REQ_PIPELINE_IO.data.flushPermReq.fire) { reqDataDone_r := true.B }

  // Release Pipeline flush permission 
  MMU_FLUSH_REQ_PIPELINE_IO.inst.flushCompled.valid := state_r === sReleasePipePerm && workingPageTableEntry.entry.perm === INST_FETCH.U && !reqInstDone_r
  MMU_FLUSH_REQ_PIPELINE_IO.data.flushCompled.valid := state_r === sReleasePipePerm && !reqDataDone_r
  when(MMU_FLUSH_REQ_PIPELINE_IO.inst.flushCompled.fire) { reqInstDone_r := true.B }
  when(MMU_FLUSH_REQ_PIPELINE_IO.data.flushCompled.fire) { reqDataDone_r := true.B }

  // Flush and get TLB entry latest value
  MMU_FLUSH_TLB_IO.inst.req.bits := workingPageTableEntry.tag
  MMU_FLUSH_TLB_IO.inst.req.valid := state_r === sFlushTLB && workingPageTableEntry.entry.perm === INST_FETCH.U && !reqInstDone_r
  MMU_FLUSH_TLB_IO.data.req.bits := workingPageTableEntry.tag
  MMU_FLUSH_TLB_IO.data.req.valid := state_r === sFlushTLB && !reqDataDone_r
  when(MMU_FLUSH_TLB_IO.inst.req.fire) { reqInstDone_r := true.B }
  when(MMU_FLUSH_TLB_IO.data.req.fire) { reqDataDone_r := true.B }
  when(MMU_FLUSH_TLB_IO.inst.resp.fire) { respInstDone_r := true.B }
  when(MMU_FLUSH_TLB_IO.data.resp.fire) { respDataDone_r := true.B }

  // TLB Resp, hit => evicted entry, update modified bit
  when(MMU_FLUSH_TLB_IO.data.resp.fire && MMU_FLUSH_TLB_IO.data.resp.bits.hit) {
    // NOTE: Only the data can modify the page, so we don't check for the instruction side
    workingPageTableEntry.entry.modified := MMU_FLUSH_TLB_IO.data.resp.bits.entry.modified
  }
 

  // Flush cache page blocks
  val iCacheFlushBlock_r = RegInit(0.U(log2Ceil(params.cacheBlocksPerPage).W))
  val dCacheFlushBlock_r = RegInit(0.U(log2Ceil(params.cacheBlocksPerPage).W))

  MMU_FLUSH_CACHE_IO.inst.flushReq.bits.addr := Cat(workingPageTableEntry.entry.ppn, iCacheFlushBlock_r) << log2Ceil(params.cacheBlockSize/8).U
  MMU_FLUSH_CACHE_IO.data.flushReq.bits.addr := Cat(workingPageTableEntry.entry.ppn, dCacheFlushBlock_r) << log2Ceil(params.cacheBlockSize/8).U
  MMU_FLUSH_CACHE_IO.inst.flushReq.valid := state_r === sEvictCacheBlocks && workingPageTableEntry.entry.perm === INST_FETCH.U && !reqInstDone_r
  MMU_FLUSH_CACHE_IO.data.flushReq.valid := state_r === sEvictCacheBlocks && !reqDataDone_r
  when(MMU_FLUSH_CACHE_IO.inst.flushReq.fire) { 
    iCacheFlushBlock_r := iCacheFlushBlock_r + 1.U 
    when(iCacheFlushBlock_r === (params.cacheBlocksPerPage - 1).U) {
      reqInstDone_r := true.B
    }
  }

  when(MMU_FLUSH_CACHE_IO.data.flushReq.fire) { 
    dCacheFlushBlock_r := dCacheFlushBlock_r + 1.U 
    when(dCacheFlushBlock_r === (params.cacheBlocksPerPage - 1).U) {
      reqDataDone_r := true.B
    }
  }

  // Wait flushing process to complete
  val cacheFlushLatency = 4
  val cacheFlushLatencyCycle_r = RegInit(0.U(log2Ceil(cacheFlushLatency).W))

  switch(state_r) {
    is(sIdle) {
      when(PORT.req.fire) {
        state_r := sReqPipePerm
        when(PORT.req.bits.entry.perm =/= INST_FETCH.U) {
          // No need to flush instructions on Data permissions
          reqInstDone_r := true.B
          respInstDone_r := true.B
        }
      }
    }

    is(sReqPipePerm) {
      when(reqBothDone) {
        reqDataDone_r := false.B
        reqInstDone_r := false.B
        state_r := sFlushTLB
        when(workingPageTableEntry.entry.perm =/= INST_FETCH.U) {
          // No need to flush instructions on Data permissions
          reqInstDone_r := true.B
        }
      }
    }

    is(sFlushTLB) {
      when(reqBothDone && respBothDone) {
        reqDataDone_r := false.B
        reqInstDone_r := false.B
        respDataDone_r := false.B
        respInstDone_r := false.B
        state_r := sEvictCacheBlocks
        when(workingPageTableEntry.entry.perm =/= INST_FETCH.U) {
          // No need to flush instructions on Data permissions
          reqInstDone_r := true.B
          respInstDone_r := true.B
        }
      }
    }

    is(sEvictCacheBlocks) {
      when(reqBothDone) {
        reqInstDone_r := false.B
        reqDataDone_r := false.B
        iCacheFlushBlock_r := 0.U
        dCacheFlushBlock_r := 0.U
        state_r := sWaitWbLatency
        when(workingPageTableEntry.entry.perm =/= INST_FETCH.U) {
          // No need to flush instructions on Data permissions
          reqInstDone_r := true.B
        }
      }
    }

    is(sWaitWbLatency) {
      // Eviction done? (You have to wait for like two / three cycles to get the correct result of wbQueue)
      cacheFlushLatencyCycle_r := cacheFlushLatencyCycle_r + 1.U
      when(cacheFlushLatencyCycle_r === (cacheFlushLatency - 1).U) {
        cacheFlushLatencyCycle_r := 0.U
        state_r := sWaitWbEmpty
      }
    }

    is(sWaitWbEmpty) {
      when(MMU_FLUSH_CACHE_IO.data.wbEmpty) {
        cacheFlushLatencyCycle_r := 0.U
        state_r := sReleasePipePerm
      }
    }

    is(sReleasePipePerm) {
      when(reqBothDone) {
        reqDataDone_r := false.B
        reqInstDone_r := false.B
        state_r := sComplete
        when(workingPageTableEntry.entry.perm =/= INST_FETCH.U) {
          // No need to flush instructions on Data permissions
          reqInstDone_r := true.B
        }
      }
    }

    is(sComplete) {
      when(PORT.resp.fire) {
        reqInstDone_r := false.B
        reqDataDone_r := false.B
        state_r := sIdle
      }
    }

  }
  if (true) { // TODO: Conditional asserts
    //assert(MMU_FLUSH_CACHE_IO.inst.wbEmpty, "Instruction cache should never evict entries")
  }
}

object PageTableEntryDeletorVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new PageTableEntryDeletor(new MemoryHierarchyParams)))
}
