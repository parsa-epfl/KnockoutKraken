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

class PageWalkTester extends AnyFreeSpec with ChiselScalatestTester {
  import MMUDriver._
  val (thid, asid, vpn, ppn) = (0x1, 0x10, 0xABCD, 0xCBA)

  "Normal TLB miss request" in {
    val anno = Seq(TargetDirAnnotation("test/demander/pagewalk/normal"), VerilatorBackendAnnotation, WriteVcdAnnotation, PrintFullStackTraceAnnotation)
    test(new MMUDUT(new MemoryHierarchyParams())).withAnnotations(anno){ dut => dut.init()
      implicit val params = dut.params.getPageTableParams
      val perm = INST_FETCH
      dut.sendMissReq(perm, thid.U, asid.U, vpn.U, perm.U)
      val entry = PTEntryPacket(ppn.U, perm.U, false.B)
      val tag = PTTagPacket(vpn.U, asid.U)
      val pageTableSet = PageTableItem(tag, entry)
      dut.expectRdPageTablePacket(PageTableSetPacket(Seq((0, pageTableSet)), 0.U), dut.vpn2ptSetPA(tag).U)
      dut.expectMissResp(perm, thid.U, pageTableSet)
    }
  }

  "Page Fault" in {
    val anno = Seq(TargetDirAnnotation("test/demander/pagewalk/pagefault"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new MMUDUT(new MemoryHierarchyParams())).withAnnotations(anno){ dut => dut.init()
      val perm = DATA_STORE
      val msgPageFaultNotif = PageFaultNotification(PTTagPacket(vpn.U, asid.U)(dut.params.getPageTableParams), perm.U, thid.U)(dut.params.getPageTableParams)
      dut.sendMissReq(perm, thid.U, asid.U, vpn.U, perm.U)
      dut.respEmptyPageTableSet(asid.U, vpn.U)
      dut.waitTillPendingMMUMsg()
      val msg = dut.getMMUMsg()
      dut.rawMessageHelper.expectMsgPageFaultNotif(msg, msgPageFaultNotif)
      dut.clock.step()
    }
  }
}
