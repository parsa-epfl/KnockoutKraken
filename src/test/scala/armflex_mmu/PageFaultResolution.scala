package armflex_mmu


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
import armflex_cache.MemorySystemParameter

class PageFaultResolutionTester extends FreeSpec with ChiselScalatestTester {
  "No synonym" in {
    import PageDemanderDriver._
    val anno = Seq(TargetDirAnnotation("test/demander/pagefault_resolution/no_synonym"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new PageDemanderDUT(new PageDemanderParameter())).withAnnotations(anno){ dut=>
      // send Page
      // dut.registerThreadTable(0, 0x10)
      dut.movePageIn(0x01234567)
      dut.sendPageFaultResponse(
        0xABC,
        1,
        0x10,
        1,
        false,
        0xCCD,
        0xEF
      )
      // 1. wait for the response of fetching PTEs and response with nothing.
      dut.sendPageTableSet(dut.M_AXI, (0xAB * 64 * 3).U)
      // 2. It should allocate Free PPN

      // 3. It should inserting pages.
      dut.waitForSignalToBe(dut.M_AXI.aw.awvalid)
      dut.M_AXI.aw.awaddr.expect(0x10000000.U)
      dut.M_AXI.aw.awlen.expect(63.U)
      timescope {
        dut.M_AXI.aw.awready.poke(true.B)
        dut.tk()
      }

      for (i <- 0 until 64) timescope {
        dut.M_AXI.w.wready.poke(true.B)
        dut.waitForSignalToBe(dut.M_AXI.w.wvalid)
        dut.M_AXI.w.wdata.expect(0x01234567.U)
        dut.tk()
      }

      timescope {
        dut.M_AXI.b.bvalid.poke(true.B)
        dut.waitForSignalToBe(dut.M_AXI.b.bready)
        dut.tk()
      }
      
      // 4. It should insert the PTE to page table
      dut.receivePageTableSet(dut.M_AXI, (0xAB * 64 * 3).U)
      dut.pageset_packet_o.ptes(0).ppn.expect(0x10000.U)
      dut.pageset_packet_o.ptes(0).modified.expect(false.B)
      dut.pageset_packet_o.ptes(0).permission.expect(1.U)
      dut.pageset_packet_o.tags(0).asid.expect(0x10.U)
      dut.pageset_packet_o.tags(0).vpn.expect(0xABC.U)
      // 5. It should response to the TLB for page arrive.
      dut.waitForSignalToBe(dut.dtlb_backend_reply_o.valid)
      dut.dtlb_backend_reply_o.bits.tag.asid.expect(0x10.U)
      dut.dtlb_backend_reply_o.bits.tag.vpn.expect(0xABC.U)
      dut.dtlb_backend_reply_o.bits.data.modified.expect(false.B)
      dut.dtlb_backend_reply_o.bits.data.ppn.expect(0x10000.U)
      dut.dtlb_backend_reply_o.bits.data.permission.expect(1.U)
      dut.dtlb_backend_reply_o.bits.wakeup_tid.bits.expect(1.U)
      dut.dtlb_backend_reply_o.bits.wakeup_tid.valid.expect(true.B)
    }
  }
  "Trigger Page Eviction" in {
    import PageDemanderDriver._
    val anno = Seq(TargetDirAnnotation("test/demander/pagefault_resolution/trigger_page_eviction"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new PageDemanderDUT(new PageDemanderParameter())).withAnnotations(anno){ dut=>
      // send Page
      // dut.registerThreadTable(0, 0x10)
      dut.movePageIn(0x01234567)
      dut.sendPageFaultResponse(
        0xABC,
        1,
        0x10,
        1,
        false,
        0xCCD,
        0xEF
      )
      // 0. Fake a Page table set
      dut.pageset_packet_i.valids.poke(0xFFFF.U)
      dut.pageset_packet_i.lru_bits.poke(0.U)
      for(i <- 0 until 16){
        dut.pageset_packet_i.ptes(i).modified.poke(false.B)
        dut.pageset_packet_i.ptes(i).permission.poke(1.U)
        dut.pageset_packet_i.ptes(i).ppn.poke((0x10 + i).U)
        dut.pageset_packet_i.tags(i).asid.poke(0x10.U)
        dut.pageset_packet_i.tags(i).vpn.poke((0x20 + i).U)
      }
      // 1. wait for the response of fetching PTEs and response with nothing.
      dut.sendPageTableSet(dut.M_AXI, (0xAB * 64 * 3).U)
      // 2. It should allocate Free PPN

      // 3. The page deletor should be activated.
      // 3.1 TLB eviction
      dut.waitForSignalToBe(dut.dtlb_flush_request_o.valid)
      dut.dtlb_flush_request_o.bits.asid.expect(0x10.U)
      dut.dtlb_flush_request_o.bits.vpn.expect(0x20.U)
      dut.dtlb_flush_reply_i.bits.dirty.poke(true.B)
      dut.dtlb_flush_reply_i.bits.hit.poke(true.B)
      dut.dtlb_flush_reply_i.bits.violation.poke(false.B)
      dut.dtlb_flush_reply_i.bits.entry.modified.poke(true.B)
      dut.dtlb_flush_reply_i.bits.entry.permission.poke(1.U)
      dut.dtlb_flush_reply_i.bits.entry.ppn.poke(0x10.U)
      timescope {
        dut.dtlb_flush_reply_i.valid.poke(true.B)
        dut.dtlb_flush_request_o.ready.poke(true.B)
        dut.tk()
      }
      // 3.2 Send eviction start
      dut.expectQEMUMessage(
        5,
        Seq(0x20, 0x0, 0x10, 0x10, 1, 1)
      )
      // 3.3 Cache Eviction
      for(i <- 0 until 64){
        dut.waitForSignalToBe(dut.dcache_flush_request_o.valid)
        dut.dcache_flush_request_o.bits.addr.expect((0x10 * 64 + i).U)
        dut.dcache_flush_request_o.bits.asid.expect(0x10.U)
        timescope {
          dut.dcache_flush_request_o.ready.poke(true.B)
          dut.tk()
        }
      }

      // 3.4 Wait Cache to be empty
      dut.dcache_wb_queue_empty_i.poke(true.B)

      // 3.5 Move Page out of the DRAM
      dut.waitForSignalToBe(dut.M_AXI.ar.arvalid)
      dut.M_AXI.ar.araddr.expect((0x10 * 4096).U)
      timescope {
        dut.M_AXI.ar.arready.poke(true.B)
        dut.tk()
      }

      for(i <- 0 until 64){
        dut.M_AXI.r.rdata.poke(0x0ABCDEF.U)
        dut.M_AXI.r.rresp.poke(0.U)
        dut.M_AXI.r.rlast.poke((i == 63).B)
        dut.waitForSignalToBe(dut.M_AXI.r.rready)
        timescope {
          dut.M_AXI.r.rvalid.poke(true.B)
          dut.tk()
        }
      }

      // 3.6 Send eviction done
      dut.expectQEMUMessage(
        6,
        Seq(0x20, 0x0, 0x10, 0x10, 1, 1)

      )

      // 4. It should inserting pages.
      dut.waitForSignalToBe(dut.M_AXI.aw.awvalid)
      dut.M_AXI.aw.awaddr.expect(0x10000000.U)
      dut.M_AXI.aw.awlen.expect(63.U)
      timescope {
        dut.M_AXI.aw.awready.poke(true.B)
        dut.tk()
      }

      for (i <- 0 until 64) timescope {
        dut.M_AXI.w.wready.poke(true.B)
        dut.waitForSignalToBe(dut.M_AXI.w.wvalid)
        dut.M_AXI.w.wdata.expect(0x01234567.U)
        dut.tk()
      }

      timescope {
        dut.M_AXI.b.bvalid.poke(true.B)
        dut.waitForSignalToBe(dut.M_AXI.b.bready)
        dut.tk()
      }
      
      // 5. It should insert the PTE to page table
      dut.receivePageTableSet(dut.M_AXI, (0xAB * 64 * 3).U)
      dut.pageset_packet_o.ptes(0).ppn.expect(0x10000.U)
      dut.pageset_packet_o.ptes(0).modified.expect(false.B)
      dut.pageset_packet_o.ptes(0).permission.expect(1.U)
      dut.pageset_packet_o.tags(0).asid.expect(0x10.U)
      dut.pageset_packet_o.tags(0).vpn.expect(0xABC.U)
      // 6. It should response to the TLB for page arrive.
      dut.waitForSignalToBe(dut.dtlb_backend_reply_o.valid)
      dut.dtlb_backend_reply_o.bits.tag.asid.expect(0x10.U)
      dut.dtlb_backend_reply_o.bits.tag.vpn.expect(0xABC.U)
      dut.dtlb_backend_reply_o.bits.data.modified.expect(false.B)
      dut.dtlb_backend_reply_o.bits.data.ppn.expect(0x10000.U)
      dut.dtlb_backend_reply_o.bits.data.permission.expect(1.U)
      dut.dtlb_backend_reply_o.bits.wakeup_tid.bits.expect(1.U)
      dut.dtlb_backend_reply_o.bits.wakeup_tid.valid.expect(true.B)
    }
  }

  "With Synonym" in {
    import PageDemanderDriver._
    val anno = Seq(TargetDirAnnotation("test/demander/pagefault_resolution/with_synonym"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new PageDemanderDUT(new PageDemanderParameter())).withAnnotations(anno){ dut=>
      // dut.registerThreadTable(0, 0x10)
      // dut.registerThreadTable(1, 0xEF)
      dut.sendPageFaultResponse(
        0xABC,
        -1,
        0x10,
        1,
        true,
        0xCCD,
        0xEF
      )

      timescope {
        // Okay, the page fault resolution will find 0xCCD first.
        dut.pageset_packet_i.valids.poke(1.U)
        dut.pageset_packet_i.tags(0).asid.poke(0x0EF.U)
        dut.pageset_packet_i.tags(0).vpn.poke(0x0CCD.U)
        dut.pageset_packet_i.ptes(0).modified.poke(true.B)
        dut.pageset_packet_i.ptes(0).permission.poke(0.U)
        dut.pageset_packet_i.ptes(0).ppn.poke(0x0FF.U)

        // It will fetch the synonym's set
        dut.sendPageTableSet(dut.M_AXI, (0xCC * 64 * 3).U)
      }
      
      // Then fetch the target set
      dut.sendPageTableSet(dut.M_AXI, (0xAB * 64 * 3).U)

      // and write it back
      dut.receivePageTableSet(dut.M_AXI, (0xAB * 64 * 3).U)
      dut.pageset_packet_o.ptes(0).ppn.expect(0x0FF.U)
      dut.pageset_packet_o.ptes(0).permission.expect(1.U)
      dut.pageset_packet_o.ptes(0).modified.expect(false.B)
      dut.pageset_packet_o.tags(0).asid.expect(0x10.U)
      dut.pageset_packet_o.tags(0).vpn.expect(0xABC.U)

      // response to the TLB
      dut.waitForSignalToBe(dut.dtlb_backend_reply_o.valid)
      dut.dtlb_backend_reply_o.bits.wakeup_tid.valid.expect(false.B)
      dut.dtlb_backend_reply_o.bits.tag.asid.expect(0x10.U)
      dut.dtlb_backend_reply_o.bits.tag.vpn.expect(0xABC.U)
      dut.dtlb_backend_reply_o.bits.data.modified.expect(false.B)
      dut.dtlb_backend_reply_o.bits.data.ppn.expect(0x0FF.U)
      dut.dtlb_backend_reply_o.bits.data.permission.expect(1.U)
    }
  }
}