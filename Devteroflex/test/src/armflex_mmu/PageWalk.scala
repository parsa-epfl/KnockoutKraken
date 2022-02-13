package armflex_mmu

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import chiseltest._
import chiseltest.experimental._

import chiseltest.simulator.VerilatorBackendAnnotation
import chiseltest.simulator.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation


import org.scalatest.freespec.AnyFreeSpec

class PageWalkTester extends AnyFreeSpec with ChiselScalatestTester {
  import PageDemanderDriver._
  "Normal" in {
    val anno = Seq(TargetDirAnnotation("test/demander/pagewalk/normal"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new MMUDUT(new MemoryHierarchyParams())).withAnnotations(anno){ dut =>
      // apply a miss request
      // Set the thread table.
      // dut.registerThreadTable(0, 10)
      timescope {
        dut.itlb_miss_request_i.bits.perm.poke(2.U)
        dut.itlb_miss_request_i.bits.tag.asid.poke(0x10.U)
        dut.itlb_miss_request_i.bits.tag.vpn.poke(0xABC.U)
        dut.itlb_miss_request_i.valid.poke(true.B)
        dut.itlb_miss_request_i.ready.expect(true.B)
        dut.tk()
      }

      // prepare for the data
      dut.pageset_packet_i.lru_bits.poke(1.U)
      dut.pageset_packet_i.valids.poke(1.U)
      dut.pageset_packet_i.tags(0).vpn.poke(0xABC.U)
      dut.pageset_packet_i.tags(0).asid.poke(0x10.U)
      dut.pageset_packet_i.ptes(0).ppn.poke(0xCBA.U)
      dut.pageset_packet_i.ptes(0).modified.poke(false.B)
      dut.pageset_packet_i.ptes(0).perm.poke(2.U)

      dut.sendPageTableSet(dut.M_AXI, dut.vpn2ptSetPA(0x10, 0xABC).U)
      // wait for the reply to the TLB
      dut.waitForSignalToBe(dut.itlb_backend_reply_o.valid)
      dut.itlb_backend_reply_o.bits.tag.asid.expect(0x10.U)
      dut.itlb_backend_reply_o.bits.tag.vpn.expect(0xABC.U)
      dut.itlb_backend_reply_o.bits.data.modified.expect(false.B)
      dut.itlb_backend_reply_o.bits.data.ppn.expect(0xCBA.U)
      dut.itlb_backend_reply_o.bits.data.perm.expect(2.U)
    }
  }

  "Page Fault" in {
    val anno = Seq(TargetDirAnnotation("test/demander/pagefault/pagefault"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new MMUDUT(new MemoryHierarchyParams())).withAnnotations(anno){ dut =>
      // apply a miss request
      // Set the thread table.
      // dut.registerThreadTable(0, 10)
      timescope {
        dut.dtlb_miss_request_i.bits.perm.poke(0.U)
        dut.dtlb_miss_request_i.bits.tag.asid.poke(0x10.U)
        dut.dtlb_miss_request_i.bits.tag.vpn.poke(0xABC.U)
        dut.dtlb_miss_request_i.bits.thid.poke(1.U) // set pid.
        dut.dtlb_miss_request_i.valid.poke(true.B)
        dut.dtlb_miss_request_i.ready.expect(true.B)
        dut.tk()
      }
      dut.sendPageTableSet(dut.M_AXI, dut.vpn2ptSetPA(0x10, 0xABC).U)
      // This will trigger an page fault.
      println("Page fault should have been triggered. Check it now.")
      
      dut.expectQEMUMessage(
        4, Seq(0x10, 0xABC, 0, 0, 1)
      )
    }
  }
}
