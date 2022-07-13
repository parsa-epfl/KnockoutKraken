package armflex_mmu


import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import chiseltest._
import chiseltest.experimental._

import chisel3.stage.PrintFullStackTraceAnnotation
import chiseltest.simulator.VerilatorBackendAnnotation
import chiseltest.simulator.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation

import armflex_cache._
import MMUBundleDrivers._

import org.scalatest.freespec.AnyFreeSpec
import armflex.QEMUMessagesType
import armflex.MemoryAccessType._
import armflex_cache.LRU
import armflex_cache.LRUCorePseudo

class PageFaultResolutionTester extends AnyFreeSpec with ChiselScalatestTester {
  val (thid, asid, vpn, ppn) = (0x5, 0x1A, 0xABCD, 0x789D)

  "QEMU miss resp with no synonym" in {
    import MMUDriver._
    val anno = Seq(TargetDirAnnotation("test/demander/pagefault_resolution/no_synonym"), VerilatorBackendAnnotation, PrintFullStackTraceAnnotation)
    test(new MMUDUT(new MemoryHierarchyParams(pAddrW = 36))).withAnnotations(anno){ dut => dut.init()
      implicit val ptparams = (dut.params.getPageTableParams)
      val perm = INST_FETCH
      val lru = 0x000F.U
      val tag = PTTagPacket(vpn.U, asid.U)
      val entry = PTEntryPacket(ppn.U, perm.U, false.B)
      val pageTableSet = PageTableItem(tag, entry)

      val msg = QEMUMissReply(tag, perm.U, thid.U, ppn.U)
      val msgPacked = dut.rawMessageHelper.encodeMsgMissReply(msg)

      println("0. Send a Miss Request response")
      dut.sendMMUMsg(msgPacked)
      println("1. Read Page Table from DRAM")
      dut.respEmptyPageTableSet(asid.U, vpn.U)
      println("2. Write updated page table")
      dut.expectWrPageTableSetPacket(PageTableSetPacket(Seq((0, pageTableSet)), lru), dut.vpn2ptSetPA(tag).U)
      println("3. Expect TLB fill response")
      dut.expectMissResp(perm, thid.U, pageTableSet)
    }
  }

  "QEMU force entry eviction" in {
    import MMUDriver._
    val anno = Seq(TargetDirAnnotation("test/demander/PageFaultEviction"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new MMUDUT(new MemoryHierarchyParams(pAddrW = 36))).withAnnotations(anno){ dut => dut.init()
      implicit val ptparams = (dut.params.getPageTableParams)
      val perm = DATA_STORE

      // Prepare evicted entry
      val evictedTag = PTTagPacket((vpn + 1).U, asid.U)
      val evictedEntry = PTEntryPacket((ppn + 1).U, perm.U, false.B)
      val evictedItem = PageTableItem(evictedTag, evictedEntry)

      // Prepare PageTableSet with LRU conflict
      val evictedEntryIdx = 13
      val lruPath = LRUCorePseudo.getLRUEncodedPath(evictedEntryIdx, dut.params.getPageTableParams.ptAssociativity)

      val bitVec = LRUCorePseudo.createBitvector(dut.params.getPageTableParams.ptAssociativity - 1)
      val lruBits = LRUCorePseudo.getBigIntFromVector(bitVec)
      val sets = Seq((evictedEntryIdx -> evictedItem))

      // Prepare Evict Request
      val qemuEvictRequest = QEMUPageEvictRequest(evictedTag, false.B, true.B)
      val qemuEvictRequestMsg = dut.rawMessageHelper.encodeMsgPageEvictReq(qemuEvictRequest)

      // Send miss reply
      println("0. Send a Miss Request response")
      dut.sendMMUMsg(qemuEvictRequestMsg)

      val flushes = fork { 
        dut.handlePageEvictionData(evictedItem)
        // dut.expectFlushReqTLB(perm, thid.U, evictedItem) 
      }

      println("1. Read Page Table from DRAM")
      dut.expectRdPageTablePacket(PageTableSetPacket(sets, lruBits.U), dut.vpn2ptSetPA(evictedTag).U)

      println("2. Expect TLB's and Cache flush requests")
      flushes.join()

      println("3.4 Send Eviction Start")
      dut.rawMessageHelper.expectMsgPageEvictNotifStart(dut.getMMUMsg(), PageEvictNotif(evictedItem))

      println("3.5 Send eviction done")
      dut.rawMessageHelper.expectMsgPageEvictNotifDone(dut.getMMUMsg(), PageEvictNotifDone(evictedItem))

      // Expect set with evicted entry
      println("4.1 Write updated page table set to DRAM")
      val newLruBitVec = LRUCorePseudo.updateBitVector(lruPath, bitVec)
      val newLruBits = LRUCorePseudo.getBigIntFromVector(newLruBitVec)

      val packet = PageTableSetPacket(0, PageTableSetPacket(sets, lruBits.U))
      dut.expectWrPageTableSetPacket(packet, dut.vpn2ptSetPA(evictedTag).U)
    }
  }

  "QEMU miss resp with entry eviction from set because of conflict" in {
    import MMUDriver._
    val anno = Seq(TargetDirAnnotation("test/demander/pagefault_resolution/trigger_page_eviction"), VerilatorBackendAnnotation)
    test(new MMUDUT(new MemoryHierarchyParams(pAddrW = 36))).withAnnotations(anno){ dut => dut.init()
      implicit val ptparams = (dut.params.getPageTableParams)
      val perm = DATA_STORE

      // Prepare evicted entry
      val evictedTag = PTTagPacket((vpn + 1).U, asid.U)
      val evictedEntry = PTEntryPacket((ppn + 1).U, perm.U, false.B)
      val evictedItem = PageTableItem(evictedTag, evictedEntry)

      // Prpeare inserting entry
      val insertedTag = PTTagPacket(vpn.U, asid.U)
      val insertedEntry = PTEntryPacket(ppn.U, perm.U, false.B)
      val insertedItem  = PageTableItem(insertedTag, insertedEntry)

      // Prepare PageTableSet with LRU conflict
      val lru = 13
      val lruPath = LRUCorePseudo.getLRUEncodedPath(lru, dut.params.getPageTableParams.ptAssociativity)
      val randomString = BigInt(scala.util.Random.nextLong(Math.pow(2, dut.params.getPageTableParams.ptAssociativity-1).toLong)).toString(2).padTo(dut.params.getPageTableParams.ptAssociativity-1, '0')
      val lruBitsString = lruPath.foldLeft(randomString) { (currEncode, step) => currEncode.updated(step._1, step._2) }
      val lruBits = BigInt(lruBitsString.reverse, 2)
      val sets = PageTableSetPacket.makeEmptySet.updated[(Int, armflex.PageTableItem)](lru, (lru -> evictedItem))

      // Prepare Miss Request
      val qemuMissReplyReq = QEMUMissReply(insertedTag, perm.U, thid.U, insertedItem.entry.ppn)
      val pageFaultMissReply = dut.rawMessageHelper.encodeMsgMissReply(qemuMissReplyReq)

      // Send miss reply
      println("0. Send a Miss Request response")
      dut.sendMMUMsg(pageFaultMissReply)

      val flushes = fork { 
        dut.handlePageEviction(evictedItem)
      }

      println("1. Read Page Table from DRAM")
      dut.expectRdPageTablePacket(PageTableSetPacket(sets, lruBits.U), dut.vpn2ptSetPA(insertedTag).U)

      println("2. Expect TLB's and Cache flush requests")
      flushes.join()

      println("3.4 Send Eviction Start")
      dut.rawMessageHelper.expectMsgPageEvictNotifStart(dut.getMMUMsg(), PageEvictNotif(evictedItem))
      println("3.5 Send eviction done")
      dut.rawMessageHelper.expectMsgPageEvictNotifDone(dut.getMMUMsg(), PageEvictNotifDone(evictedItem))

      // Expect set with inserted entry
      println("4.0 Calculate the new LRU vector")
      val newLruBitsString = LRUCorePseudo.updateBitVector(lruPath, lruBitsString)
      val newLruBits = LRUCorePseudo.getBigIntFromVector(newLruBitsString)

      println("4.1 Write updated page table set to DRAM")
      dut.expectWrPageTableSetPacket(PageTableSetPacket(sets.updated[(Int, armflex.PageTableItem)](lru, (lru -> insertedItem)), newLruBits.U), dut.vpn2ptSetPA(insertedTag).U)

      println("5. Refill TLB with inserted translation")
      dut.expectMissResp(perm, thid.U, insertedItem)
    }
  }
}
