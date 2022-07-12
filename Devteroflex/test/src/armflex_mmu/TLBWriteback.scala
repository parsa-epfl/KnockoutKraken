package armflex_mmu

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import chiseltest._
import chiseltest.experimental._

import chiseltest.simulator.VerilatorBackendAnnotation
import chiseltest.simulator.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation

import MMUBundleDrivers._

import armflex.QEMUMessagesType
import armflex.MemoryAccessType._
import armflex_cache.LRU
import armflex_cache.LRUCorePseudo


import org.scalatest.freespec.AnyFreeSpec
import armflex_mmu.peripheral.PageTableOps

class TLBWritebackTester extends AnyFreeSpec with ChiselScalatestTester {
  import MMUDriver._
  val (thid, asid, vpn, ppn) = (0x5, 0x1A, 0xABCD, 0x789D)

  "Normal" in {
    val anno = Seq(TargetDirAnnotation("test/demander/tlbwriteback/normal"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new MMUDUT(new MemoryHierarchyParams())).withAnnotations(anno){ dut => dut.init()
      implicit val ptparams = (dut.params.getPageTableParams)
      val perm = DATA_STORE
      val tag = PTTagPacket(vpn.U, asid.U)
      val baseEntry = PTEntryPacket(ppn.U, perm.U, false.B)
      val modifiedEntry = PTEntryPacket(ppn.U, perm.U, true.B)
      val pageTableAddr = dut.vpn2ptSetPA(tag)
      val basePageTableSet = PageTableItem(tag, baseEntry)
      val modifiedPageTableSet = PageTableItem(tag, modifiedEntry)

      val basePageTablePacket = PageTableSetPacket(Seq((0 -> basePageTableSet)), 0.U)
      val modifiedPageTablePacket = PageTableSetPacket(Seq((0 -> modifiedPageTableSet)), 0xF.U)
      dut.mmu_tlb_io.data.pageTableReq.enqueueNow(PageTableReq(modifiedPageTableSet, PageTableOps.opInsert, thid.U, false.B, PageTableOps.destDTLB, false.B, false.B))
      dut.expectRdPageTablePacket(basePageTablePacket, pageTableAddr.U)
      dut.expectWrPageTableSetPacket(modifiedPageTablePacket, pageTableAddr.U)
    }
  }
}
