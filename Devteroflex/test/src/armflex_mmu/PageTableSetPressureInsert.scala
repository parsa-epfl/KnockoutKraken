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
import armflex.util.AXIDrivers._

import org.scalatest.freespec.AnyFreeSpec

import MMUBundleDrivers._
import armflex_cache.TLBMMURespPacket
import armflex.QEMUMessagesType
import armflex_cache.LRUCore
import armflex_cache.LRUCorePseudo

class PageTableSetPressureInsert extends AnyFreeSpec with ChiselScalatestTester {
  import MMUDriver._
  val (thid, asid, vpn, ppn) = (0xF, 0x10, 0xABCD, 0xCBA)
  val perm = INST_FETCH

  "Pressure same set page walks" in {
    val anno = Seq(TargetDirAnnotation("test/MMU/pagewalk/pressure"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new MMUDUT(new MemoryHierarchyParams())).withAnnotations(anno){ dut => dut.init()
      implicit val ptparams = dut.params.getPageTableParams
      val lru = 0
      val pageSetPacketAddr = dut.vpn2ptSetPA(asid, vpn)
      val pageSetPacketItems = for (item <- 0 to ptparams.ptAssociativity) yield {
        val tag = PTTagPacket((vpn + item * 2).U, asid.U)
        val entry = PTEntryPacket((ppn + item * 2).U, perm.U, false.B)
        PageTableItem(tag, entry)
      }

      fork {
        for(item <- 0 to ptparams.ptAssociativity) {
          val msg = QEMUMissReply(pageSetPacketItems(item).tag, perm.U, thid.U, pageSetPacketItems(item).entry.ppn, false.B)
          val msgPacked = dut.rawMessageHelper.encodeMsgMissReply(msg)
          dut.waitTillFreeMMUMsg()
          dut.sendMMUMsg(msgPacked)
        }
      }.fork {
        var currSets = PageTableSetPacket.makeEmptySet
        var validBits = LRUCorePseudo.getBitVector(0, ptparams.ptAssociativity)
        val lruSize = ptparams.ptAssociativity
        var lruBits = LRUCorePseudo.createBitvector(lruSize).map{ _ => '0'}
        var valid = LRUCorePseudo.getBigIntFromVector(validBits)
        var lru = LRUCorePseudo.getLRU(lruBits, lruSize)
        var pageTablePacket = PageTableSetPacket(currSets, LRUCorePseudo.getBigIntFromVector(lruBits).U, valid.U)
        for(item <- 0 to ptparams.ptAssociativity) {
          dut.expectRdPageTablePacket(pageTablePacket, pageSetPacketAddr.U)

          currSets = currSets.updated(lru.toInt, (lru.toInt, pageSetPacketItems(item)))
          validBits = validBits.updated(lru.toInt, '1')

          val path = LRUCorePseudo.getLRUEncodedPath(lru, lruSize)
          lruBits = LRUCorePseudo.updateBitVector(path, lruBits)
          valid = LRUCorePseudo.getBigIntFromVector(validBits)
          pageTablePacket = PageTableSetPacket(currSets, LRUCorePseudo.getBigIntFromVector(lruBits).U, valid.U)

          dut.expectWrPageTableSetPacket(pageTablePacket, pageSetPacketAddr.U)
        }
      }.join()
    }
  }
}
