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

import armflex.MemoryAccessType._
import MMUBundleDrivers._

import org.scalatest.freespec.AnyFreeSpec
import armflex.QEMUMessagesType
import armflex.MemoryAccessType

class PageFaultResolutionTester extends AnyFreeSpec with ChiselScalatestTester {
  val (thid, asid, vpn, ppn) = (0x5, 0x1A, 0xABCD, 0x789D)

  /*
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
      val msgPacked = dut.encodeMsgMissReply(QEMUMessagesType.sMissReply.U, msg)
      dut.sendMMUMsg(msgPacked)
      dut.respEmptyPageTableSet(asid.U, vpn.U)
      dut.expectWrPageTableSet(Seq((0, pageTableSet)), lru)
      dut.expectMissResp(perm, thid.U, pageTableSet)
    }
  }
  // */

  "QEMU miss resp with entry eviction from set" in {
    import MMUDriver._
    val anno = Seq(TargetDirAnnotation("test/demander/pagefault_resolution/trigger_page_eviction"), VerilatorBackendAnnotation)
    test(new MMUDUT(new MemoryHierarchyParams(pAddrW = 36))).withAnnotations(anno){ dut => dut.init()
      implicit val ptparams = (dut.params.getPageTableParams)
      val perm = DATA_STORE
      val evictedTag = PTTagPacket((vpn + 1).U, asid.U)
      val evictedEntry = PTEntryPacket((ppn + 1).U, perm.U, false.B)
      val insertedTag = PTTagPacket(vpn.U, asid.U)
      val insertedEntry = PTEntryPacket(ppn.U, perm.U, false.B)
      val insertedItem  = PageTableItem(PTTagPacket(vpn.U, asid.U), PTEntryPacket(ppn.U, perm.U, false.B))
      val evictedItem = PageTableItem(PTTagPacket((vpn + 1).U, asid.U), PTEntryPacket((ppn + 1).U, perm.U, false.B))
      val makeMsg = QEMUMissReply(PTTagPacket(vpn.U, asid.U), perm.U, thid.U, insertedItem.entry.ppn)
      val insertMsg = dut.encodeMsgMissReply(QEMUMessagesType.sMissReply.U, makeMsg)

      val emptySet = PageTableSetPacket.makeEmptySet
      val lru = 13
      val lruBits = BigInt("1000000010000100".reverse, 2)
      val sets = emptySet.updated[(Int, armflex.PageTableItem)](lru, (lru -> evictedItem))
      sets foreach (set => println(s"VPN[${set._2.tag.vpn.litValue}]:ASID[${set._2.tag.asid.litValue}]"))

      dut.sendMMUMsg(insertMsg)
      fork {
        dut.expectFlushReq(perm, thid.U, evictedItem) 
      }
      dut.expectRdPageTableSet(sets, lruBits.U, lru)
      dut.expectWrPageTableSet(sets, lruBits.U)
      dut.expectFlushReq(perm, thid.U, evictedItem)

    /*
      // 3.2 Send eviction start
      fork {
        dut.expectQEMUMessage(
          5,
          Seq(0x10, 0x20, 0x0, 0x10, 1, 1)
        )
      }
      // 3.3 Cache Eviction (At the same time with eviction message.)
      timescope {
        dut.dcache_flush_request_o.ready.poke(true.B)
        for(i <- 0 until 64){
          dut.waitForSignalToBe(dut.dcache_flush_request_o.valid)
          dut.dcache_flush_request_o.bits.addr.expect((0x10 * 4096 + i * 64).U)
          dut.tk()
        }
      }
      
      // 3.4 Wait Cache to be empty
      dut.mmu_cache_io.data.wbEmpty.poke(true.B)

      // 3.5 Send eviction done
      dut.expectQEMUMessage(
        6,
        Seq(0x10, 0x20, 0x0, 0x10, 1, 1)
      )

      println("3.4 Send Eviction Start")
      dut.expectMsgPageEvictNotif(dut.getMMUMsg(), PageEvictNotif(evictedItem))
      println("3.5 Send eviction done")
      dut.expectMsgPageEvictNotif(dut.getMMUMsg(), PageEvictNotifDone(evictedItem))

      // Expect set with inserted entry
      println("4.0 Calculate the new LRU vector")
      val newLruBitsString = lruPath.foldLeft(lruBitsString) { 
        (currEncode, step) => currEncode.updated(step._1, step._2 match {
          case '0' => '1'
          case '1' => '0'
        })}
      val newLruBits = BigInt(newLruBitsString.reverse, 2)

      println("4.1 Write updated page table set to DRAM")
      dut.expectWrPageTableSet(sets.updated[(Int, armflex.PageTableItem)](lru, (lru -> insertedItem)), newLruBits.U, lru)

      println("5. Refill TLB with inserted translation")
      dut.expectMissResp(perm, thid.U, insertedItem)
    }
  }
}