package armflex.demander


import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import chiseltest._
import chiseltest.experimental._

import TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.internal.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation


import org.scalatest.FreeSpec
import armflex.cache.MemorySystemParameter

class PageFaultResolutionTester extends FreeSpec with ChiselScalatestTester {
  "No synonym" in {
    import PageDemanderDriver._
    val anno = Seq(TargetDirAnnotation("test/demander/pagefault_resolution/no_synonym"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new PageDemanderDUT(new MemorySystemParameter())).withAnnotations(anno){ dut=>
      // send Page
      dut.registerThreadTable(0.U, 0x10.U)
      dut.movePageIn(0x01234567)
      dut.sendPageFaultResponse(
        0xABC,
        0x10,
        1,
        false,
        0xCCD,
        0xEF
      )
      // 1. wait for the response of fetching PTEs and response with nothing.
      dut.sendPageTableSet(dut.M_AXI_QEMU_MISS, (0xAB * 64 * 3).U)
      // 2. It should allocate Free PPN

      // 3. It should inserting pages.
      dut.waitForSignalToBe(dut.M_AXI_PAGE.aw.awvalid)
      dut.M_AXI_PAGE.aw.awaddr.expect(0x10000000.U)
      dut.M_AXI_PAGE.aw.awlen.expect(63.U)
      timescope {
        dut.M_AXI_PAGE.aw.awready.poke(true.B)
        dut.tk()
      }

      for (i <- 0 until 64) timescope {
        dut.M_AXI_PAGE.w.wready.poke(true.B)
        dut.waitForSignalToBe(dut.M_AXI_PAGE.w.wvalid)
        dut.M_AXI_PAGE.w.wdata.expect(0x01234567.U)
        dut.tk()
      }

      timescope {
        dut.M_AXI_PAGE.b.bvalid.poke(true.B)
        dut.waitForSignalToBe(dut.M_AXI_PAGE.b.bready)
        dut.tk()
      }
      
      // 4. It should insert the PTE to page table
      dut.receivePageTableSet(dut.M_AXI_QEMU_MISS, (0xAB * 64 * 3).U)
      dut.pageset_packet_o.ptes(0).ppn.expect(0x10000.U)
      dut.pageset_packet_o.ptes(0).modified.expect(false.B)
      dut.pageset_packet_o.ptes(0).permission.expect(1.U)
      dut.pageset_packet_o.tags(0).process_id.expect(0x10.U)
      dut.pageset_packet_o.tags(0).vpn.expect(0xABC.U)
      // 5. It should response to the TLB for page arrive.
      dut.waitForSignalToBe(dut.dtlb_backend_reply_o.valid)
      dut.dtlb_backend_reply_o.bits.tag.thread_id.expect(0.U)
      dut.dtlb_backend_reply_o.bits.tag.vpage.expect(0xABC.U)
      dut.dtlb_backend_reply_o.bits.data.modified.expect(false.B)
      dut.dtlb_backend_reply_o.bits.data.pp.expect(0x10000.U)
      dut.dtlb_backend_reply_o.bits.data.permission.expect(1.U)
    }
  }
  "Trigger Page Eviction" in {
    
  }

  "With Synonym" in {

  }
}