package armflex_mmu

import chisel3._
import chisel3.util._

import armflex._
import armflex.MemoryAccessType._
import armflex_mmu.MemoryHierarchyParams
import armflex_cache._
import armflex_mmu.peripheral._

class PageTableReqIO(params: PageTableParams) extends Bundle {
  val req = Flipped(Decoupled(new PageTableReq(params)))
  val resp = Decoupled(new TLBMMURespPacket(params))
}

class PageTableReqHandler(params: MemoryHierarchyParams) extends Module {
  // Send messages from FPGA to HOST
  val FPGA_MSG_QUEUE = IO(new Bundle {
    val req = Decoupled(UInt(params.dramDataW.W))
  })

  // Receive messages from message handler
  val PAGE_ENTRY_REQ = IO(Flipped(Decoupled(new PageTableReq(params.getPageTableParams))))

  // Operate on Page Table
  val PAGE_ENTRY_PAGE_WALK = IO(new Bundle {
    val req = Decoupled(new PageTableReq(params.getPageTableParams))
    val resp = Flipped(Decoupled(Valid(new PageTableItem(params.getPageTableParams))))
  })

  // Evict entry
  val PAGE_ENTRY_DELETOR = IO(Flipped(new PageTableEntryDeletorPort(params)))

  // Forward message to TLB
  val PAGE_ENTRY_TLB_IO = IO(new Bundle {
    val refillResp = Decoupled(new TLBMMURespPacket(params.getPageTableParams))
  })

  val sIdle :: sSendPageTableReq :: sWaitPageTableResp :: sNotifyStartResp :: sSendPageTableDelete :: sWaitPageTableDelete :: sNotifyDoneResp :: sForwardTLB :: sNotifyFaultResp :: Nil = Enum(9)
  val state_r = RegInit(sIdle)

  val workingReq_r = RegInit(PAGE_ENTRY_REQ.bits.cloneType, 0.U.asTypeOf(PAGE_ENTRY_REQ.bits))

  // Receive request
  PAGE_ENTRY_REQ.ready := state_r === sIdle

  // ------- Do the Page Table walk and operation -------
  PAGE_ENTRY_PAGE_WALK.req.bits := workingReq_r
  PAGE_ENTRY_PAGE_WALK.req.valid := state_r === sSendPageTableReq
  PAGE_ENTRY_PAGE_WALK.resp.ready := state_r === sWaitPageTableResp

  // ------- Pack messages to HOST ------
  val pageTableRespValid_r = RegInit(false.B)
  val pageTableResp_r = Reg(new PageTableItem(params.getPageTableParams))

  val evictNotifyStartMsg_w = Wire(new PageEvictNotification(QEMUMessagesType.sEvictNotify, params.getPageTableParams))
  val evictNotifyDoneMsg_w = Wire(new PageEvictNotification(QEMUMessagesType.sEvictDone, params.getPageTableParams))
  val pageFaultMsg_w = Wire(new PageFaultNotification(params.getPageTableParams))
  evictNotifyStartMsg_w.item := pageTableResp_r
  evictNotifyDoneMsg_w.item := pageTableResp_r
  pageFaultMsg_w.perm := workingReq_r.entry.entry.perm
  pageFaultMsg_w.tag := workingReq_r.entry.tag
  pageFaultMsg_w.thid := workingReq_r.thid

  val txMessage = WireInit(MuxLookup(state_r, 0.U.asTypeOf(new TxMessage), Seq(
    sNotifyDoneResp -> evictNotifyDoneMsg_w.getRawMessage,
    sNotifyStartResp -> evictNotifyStartMsg_w.getRawMessage,
    sNotifyFaultResp -> pageFaultMsg_w.getRawMessage)
  ))
  FPGA_MSG_QUEUE.req.bits := Cat(txMessage.asVec.reverse)
  FPGA_MSG_QUEUE.req.valid := state_r === sNotifyDoneResp || state_r === sNotifyStartResp || state_r === sNotifyFaultResp

  // ----- Delete evicted entry -------
  PAGE_ENTRY_DELETOR.req.bits := pageTableResp_r
  PAGE_ENTRY_DELETOR.req.valid := state_r === sSendPageTableDelete

  PAGE_ENTRY_DELETOR.resp.ready := state_r === sWaitPageTableDelete

  // ---- Refill TLB's following a lookup or resolving a page fault -----
  PAGE_ENTRY_TLB_IO.refillResp.bits.data := workingReq_r.entry.entry
  PAGE_ENTRY_TLB_IO.refillResp.bits.tag := workingReq_r.entry.tag
  PAGE_ENTRY_TLB_IO.refillResp.bits.thid := workingReq_r.thid
  PAGE_ENTRY_TLB_IO.refillResp.valid := state_r === sForwardTLB

  when(PAGE_ENTRY_PAGE_WALK.resp.valid) {
    pageTableResp_r := PAGE_ENTRY_PAGE_WALK.resp.bits.bits
    pageTableRespValid_r := PAGE_ENTRY_PAGE_WALK.resp.bits.valid
  }

  // state machine
  switch(state_r){
    is(sIdle){
      when(PAGE_ENTRY_REQ.fire) {
        workingReq_r := PAGE_ENTRY_REQ.bits
        state_r := sSendPageTableReq
        when(PAGE_ENTRY_REQ.bits.op =/= PageTableOps.opInsert &&
             PAGE_ENTRY_REQ.bits.op =/= PageTableOps.opLookup &&
             PAGE_ENTRY_REQ.bits.op =/= PageTableOps.opEvict) {
          state_r := sIdle
        }
      }
    }

    is(sSendPageTableReq){
      when(PAGE_ENTRY_PAGE_WALK.req.fire) {
        state_r := sWaitPageTableResp
      }
    }

    is(sWaitPageTableResp) {
      when(PAGE_ENTRY_PAGE_WALK.resp.fire) {
        when(workingReq_r.op === PageTableOps.opInsert) {
          when(PAGE_ENTRY_PAGE_WALK.resp.bits.valid) {
            // Evict due to associatibity colition
            state_r := sNotifyStartResp
          }.elsewhen(workingReq_r.thid_v) {
            // Wake up thread after insertion
            state_r := sForwardTLB
          }.otherwise {
            // Done Inserting, don't forward
            state_r := sIdle
          }
        }.elsewhen(workingReq_r.op === PageTableOps.opEvict) {
          when(PAGE_ENTRY_PAGE_WALK.resp.bits.valid) {
            // Evict entry
            state_r := sNotifyStartResp
          }.otherwise {
            // Done, entry not found
            state_r := sIdle
          }
        }.elsewhen(workingReq_r.op === PageTableOps.opLookup) {
          when(PAGE_ENTRY_PAGE_WALK.resp.bits.valid) {
            // Hit Page Table
            workingReq_r.entry := PAGE_ENTRY_PAGE_WALK.resp.bits.bits
            state_r := sForwardTLB
          }.otherwise {
            // Miss Page Table
            state_r := sNotifyFaultResp
          }
        }
      }
    }

    is(sForwardTLB) {
      when(PAGE_ENTRY_TLB_IO.refillResp.fire) {
        state_r := sIdle
      }
    }

    is(sNotifyFaultResp) {
      when(FPGA_MSG_QUEUE.req.fire) {
        state_r := sIdle
      }
    }

    is(sNotifyStartResp) {
      when(FPGA_MSG_QUEUE.req.fire) {
        state_r := sSendPageTableDelete
      }
    }

    is(sSendPageTableDelete) {
      when(PAGE_ENTRY_DELETOR.req.fire) {
        state_r := sWaitPageTableDelete
      }
    }

    is(sWaitPageTableDelete) {
      when(PAGE_ENTRY_DELETOR.resp.fire) {
        pageTableResp_r.entry.modified := PAGE_ENTRY_DELETOR.resp.bits.entry.modified
        state_r := sNotifyDoneResp
      }
    }

    is(sNotifyDoneResp) {
      when(FPGA_MSG_QUEUE.req.fire) {
        when(workingReq_r.op === PageTableOps.opInsert) {
          when(workingReq_r.thid_v) {
            // Forward TLB
            state_r := sForwardTLB
          }.otherwise {
            // No need to wakeup thread
            state_r := sIdle
          }
        }.otherwise {
          // Done Eviction
          state_r := sIdle
        }
      }
    }
  }

  if (true) { // TODO Conditional asserts
  }
}
