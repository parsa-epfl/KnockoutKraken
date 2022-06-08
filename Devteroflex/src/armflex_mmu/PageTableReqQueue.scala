package armflex_mmu

import chisel3._
import chisel3.util._

import armflex._
import armflex.MemoryAccessType._
import armflex_mmu.MemoryHierarchyParams
import armflex_cache._
import armflex_mmu.peripheral._

class PageTableReqQueue(params: MemoryHierarchyParams) extends Module {
  // Receive messages from HOST to FPGA
  val HOST_MSG_QUEUE = IO(new Bundle {
    val req = Flipped(Decoupled(UInt(params.dramDataW.W)))
  })

  // Receive messages from TLB's
  val TLB_MSG_QUEUE = IO(new Bundle {
    val inst = Flipped(Decoupled(new PageTableReq(params.getPageTableParams)))
    val data = Flipped(Decoupled(new PageTableReq(params.getPageTableParams)))
  })

  val PAGE_TABLE_OPERATOR_QUEUE = IO(Decoupled(new PageTableReq(params.getPageTableParams)))

  val hostMsgs = Queue(HOST_MSG_QUEUE.req, 2, false, false)
  val hostReq_w = WireInit(PAGE_TABLE_OPERATOR_QUEUE.bits.cloneType, DontCare)

  val uArbiter = Module(new Arbiter(new PageTableReq(params.getPageTableParams), 3))

  // Host port has always priority
  uArbiter.io.in(0).bits := hostReq_w
  hostMsgs.ready := uArbiter.io.in(0).ready
  uArbiter.io.in(0).valid := hostMsgs.valid

  uArbiter.io.in(1) <> TLB_MSG_QUEUE.data
  uArbiter.io.in(2) <> TLB_MSG_QUEUE.inst

  uArbiter.io.in(0).bits.refillDest := Mux(hostReq_w.entry.entry.perm === MemoryAccessType.INST_FETCH.U, PageTableOps.destITLB, PageTableOps.destDTLB)
  uArbiter.io.in(1).bits.refillDest := PageTableOps.destDTLB
  uArbiter.io.in(2).bits.refillDest := PageTableOps.destITLB

  // uArbiter.io.out <> PAGE_TABLE_OPERATOR_QUEUE
  PAGE_TABLE_OPERATOR_QUEUE <> Queue(uArbiter.io.out, params.thidN)

  // -------- Extract Request from HOST --------
  val hostReqBits_w = WireInit(UInt(params.dramDataW.W), hostMsgs.bits)
  val rawMsg_w = (new TxMessage).parseFromVec(VecInit(hostReqBits_w.asBools().grouped(32).map{x=> Cat(x.reverse)}.toSeq))

  val qemuMissReplyMsg_w = Wire(new QEMUMissReply(params.getPageTableParams))
  val isQemuMissReply_w = WireInit(rawMsg_w.message_type === QEMUMessagesType.sMissReply.U)
  qemuMissReplyMsg_w := qemuMissReplyMsg_w.parseFromVec(rawMsg_w.data)

  /* sEvictReply not used anymore
  val qemuEvictReplyMsg_w = Wire(new QEMUEvictReply(params.getPageTableParams))
  val isQemuEvictReply_w = WireInit(rawMsg_w.message_type === QEMUMessagesType.sEvictReply.U)
  qemuEvictReplyMsg_w := qemuEvictReplyMsg_w.parseFromVec(rawMsg_w.data)
  // */

  val qemuEvictRequestMsg_w = Wire(new QEMUPageEvictRequest(params.getPageTableParams))
  val isQemuEvictRequest_w = WireInit(rawMsg_w.message_type === QEMUMessagesType.sPageEvict.U)
  qemuEvictRequestMsg_w := qemuEvictRequestMsg_w.parseFromVec(rawMsg_w.data)

  val pageTableEntryInsert = Wire(new PageTableItem(params.getPageTableParams))
  pageTableEntryInsert.tag := qemuMissReplyMsg_w.tag
  pageTableEntryInsert.entry.modified := false.B
  pageTableEntryInsert.entry.perm := qemuMissReplyMsg_w.perm
  pageTableEntryInsert.entry.ppn := qemuMissReplyMsg_w.ppn

  val pageTableEntryEvict = Wire(new PageTableItem(params.getPageTableParams))
  pageTableEntryEvict.tag := qemuEvictRequestMsg_w.tag
  pageTableEntryEvict.entry := DontCare

  hostReq_w.thid := qemuMissReplyMsg_w.thid
  hostReq_w.thid_v := qemuMissReplyMsg_w.thid_v
  when(isQemuEvictRequest_w) {
    hostReq_w.entry := pageTableEntryEvict
    hostReq_w.op := PageTableOps.opEvict
  }.elsewhen(isQemuMissReply_w) {
    hostReq_w.entry := pageTableEntryInsert
    hostReq_w.op := PageTableOps.opInsert
  }

  if (true) { // TODO Conditional asserts

  }
}
