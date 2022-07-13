package armflex_mmu

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import chiseltest._
import chiseltest.experimental._

import chiseltest.simulator.VerilatorBackendAnnotation
import chiseltest.simulator.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation

import armflex._
import armflex.MemoryAccessType._
import armflex.util.AXIDrivers._

import org.scalatest.freespec.AnyFreeSpec

import armflex_cache.TLBMMURespPacket
import MMUBundleDrivers._
import peripheral.QEMUMessageEncoder
import peripheral.QEMUMessageDecoder
import armflex.QEMUMessagesType
import armflex_cache.PageTableParams

object QEMUMessageEncoderDecoderDrivers {

  class QEMUMessageEncoderDecoderDUT(val param: MemoryHierarchyParams, fifoDepth: Int = 2) extends Module {
    val dutEncoder = Module(new QEMUMessageEncoder(param, fifoDepth))
    val dutDecoder = Module(new QEMUMessageDecoder(param, fifoDepth))

    val message = IO(new Bundle {
      val evictDone = Flipped(dutEncoder.evict_done_req_i.cloneType)
      val evictNotif = Flipped(dutEncoder.evict_notify_req_i.cloneType)
      val pageFault = Flipped(dutEncoder.page_fault_req_i.cloneType)

      val evictPage = dutDecoder.qemu_evict_page_req_o.cloneType
      val evictReply = dutDecoder.qemu_evict_reply_o.cloneType
      val missReply = dutDecoder.qemu_miss_reply_o.cloneType

      val decoded_o = dutEncoder.o.cloneType
      val encoded_i = Flipped(dutDecoder.message_i.cloneType)
    })

    message.evictDone  <> dutEncoder.evict_done_req_i
    message.evictNotif <> dutEncoder.evict_notify_req_i
    message.pageFault  <> dutEncoder.page_fault_req_i

    message.evictPage  <> dutDecoder.qemu_evict_page_req_o
    message.evictReply <> dutDecoder.qemu_evict_reply_o
    message.missReply  <> dutDecoder.qemu_miss_reply_o

    message.decoded_o <> dutEncoder.o
    message.encoded_i <> dutDecoder.message_i

    val uHelperEncodeRawMessage = Module(new MMUHelpers.RawMessageHelper()(param.getPageTableParams))
    val rawMessageHelper = IO(uHelperEncodeRawMessage.io.cloneType)
    rawMessageHelper <> uHelperEncodeRawMessage.io
  }

  implicit class QEMUMessageEncoderDecoderDUTDriver(target: QEMUMessageEncoderDecoderDUT) {
    implicit val clock: Clock = target.clock
    def init() = {
      target.message.evictDone.initSource()
      target.message.evictNotif.initSource()
      target.message.pageFault.initSource()
      target.message.decoded_o.initSink()

      target.message.evictDone.setSourceClock(clock)
      target.message.evictNotif.setSourceClock(clock)
      target.message.pageFault.setSourceClock(clock)
      target.message.decoded_o.setSinkClock(clock)

      target.message.evictPage.initSink()
      target.message.evictReply.initSink()
      target.message.missReply.initSink()
      target.message.encoded_i.initSource()

      target.message.evictPage.setSinkClock(clock)
      target.message.evictReply.setSinkClock(clock)
      target.message.missReply.setSinkClock(clock)
      target.message.encoded_i.setSourceClock(clock)
    }
  }
}

class QEMUMessageEncoderDecoderTester extends AnyFreeSpec with ChiselScalatestTester {
  import MMUDriver._
  import QEMUMessageEncoderDecoderDrivers._
  val (thid, asid, vpn, ppn) = (0x1, 0x10, 0xABCD, 0xCBA)
  val perm = INST_FETCH

  "Encode Evict Notify" in {
    val anno = Seq(TargetDirAnnotation("test/MMU/Messages/evictNotify"), VerilatorBackendAnnotation)
    test(new QEMUMessageEncoderDecoderDUT(new MemoryHierarchyParams())).withAnnotations(anno){ dut => dut.init()
      implicit val params = dut.param.getPageTableParams
      val pageSet = PageTableItem(PTTagPacket(vpn.U, asid.U), PTEntryPacket(ppn.U, perm.U, false.B))
      val msgNotif = PageEvictNotif(pageSet)
      val msg = dut.rawMessageHelper.encodeMsgPageEvictNotifStart(msgNotif)
      dut.message.evictNotif.enqueueNow(msgNotif)
      dut.message.decoded_o.expectDequeue(msg)
    }
  }

  "Encode Evict Done" in {
    val anno = Seq(TargetDirAnnotation("test/MMU/Messages/evictDone"), VerilatorBackendAnnotation)
    test(new QEMUMessageEncoderDecoderDUT(new MemoryHierarchyParams())).withAnnotations(anno){ dut => dut.init()
      implicit val params = dut.param.getPageTableParams
      val pageSet = PageTableItem(PTTagPacket(vpn.U, asid.U), PTEntryPacket(ppn.U, perm.U, false.B))
      val msg = PageEvictNotifDone(pageSet)
      val msgRaw = dut.rawMessageHelper.encodeMsgPageEvictNotifDone(msg)
      dut.message.evictDone.enqueueNow(msg)
      dut.message.decoded_o.expectDequeue(msgRaw)
    }
  }

  "Page Fault" in {
    val anno = Seq(TargetDirAnnotation("test/MMU/Messages/pageFault"), VerilatorBackendAnnotation)
    test(new QEMUMessageEncoderDecoderDUT(new MemoryHierarchyParams())).withAnnotations(anno){ 
      dut => dut.init()
      implicit val params = dut.param.getPageTableParams
      val pageSet = PageTableItem(PTTagPacket(vpn.U, asid.U), PTEntryPacket(ppn.U, perm.U, false.B))
      val msg = PageFaultNotification(pageSet.tag, pageSet.entry.perm, thid.U)
      val msgRaw = dut.rawMessageHelper.encodeMsgPageFaultNotif(msg)
      dut.message.pageFault.enqueueNow(msg)
      dut.message.decoded_o.expectDequeue(msgRaw)
    }
  }

  "QEMU Send Miss Reply" in {
    val anno = Seq(TargetDirAnnotation("test/MMU/Messages/missReply"), VerilatorBackendAnnotation)
    test(new QEMUMessageEncoderDecoderDUT(new MemoryHierarchyParams())).withAnnotations(anno){ 
      dut => dut.init()
      implicit val params = dut.param.getPageTableParams
      val pageSet = PageTableItem(PTTagPacket(vpn.U, asid.U), PTEntryPacket(ppn.U, perm.U, false.B))
      val msg = QEMUMissReply(pageSet.tag, pageSet.entry.perm, thid.U, ppn.U)
      val msgRaw = dut.rawMessageHelper.encodeMsgMissReply(msg)
      dut.message.encoded_i.enqueueNow(msgRaw)
      dut.message.missReply.expectDequeueNow(msg)
    }
  }

  "QEMU Send Page Eviction" in {
    val anno = Seq(TargetDirAnnotation("test/MMU/Messages/pageEvictRequest"), VerilatorBackendAnnotation)
    test(new QEMUMessageEncoderDecoderDUT(new MemoryHierarchyParams())).withAnnotations(anno){ 
      dut => dut.init()
      implicit val params = dut.param.getPageTableParams
      val pageSet = PageTableItem(PTTagPacket(vpn.U, asid.U), PTEntryPacket(ppn.U, perm.U, false.B))
      val msg = QEMUPageEvictRequest(pageSet.tag, true.B, true.B)
      val msgRaw = dut.rawMessageHelper.encodeMsgPageEvictReq(msg)
      dut.message.encoded_i.enqueueNow(msgRaw)
      dut.message.evictPage.expectDequeueNow(msg)
    }
  }

  "QEMU Send Evict Reply" in {
    val anno = Seq(TargetDirAnnotation("test/MMU/Messages/evictReply"), VerilatorBackendAnnotation)
    test(new QEMUMessageEncoderDecoderDUT(new MemoryHierarchyParams())).withAnnotations(anno){ 
      dut => dut.init()
      implicit val params = dut.param.getPageTableParams
      val pageSet = PageTableItem(PTTagPacket(vpn.U, asid.U), PTEntryPacket(ppn.U, perm.U, false.B))
      val msg = QEMUEvictReply(pageSet.tag, ppn.U)
      val msgRaw = dut.rawMessageHelper.encodeMsgEvictReply(msg)
      dut.message.encoded_i.enqueueNow(msgRaw)
      dut.message.evictReply.expectDequeueNow(msg)
    }
  }
}