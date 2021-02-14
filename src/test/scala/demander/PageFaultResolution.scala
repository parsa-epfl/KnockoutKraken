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
      dut.registerThreadTable(0, 0x10)
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
    import PageDemanderDriver._
    val anno = Seq(TargetDirAnnotation("test/demander/pagefault_resolution/trigger_page_eviction"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new PageDemanderDUT(new MemorySystemParameter())).withAnnotations(anno){ dut=>
      // send Page
      dut.registerThreadTable(0, 0x10)
      dut.movePageIn(0x01234567)
      dut.sendPageFaultResponse(
        0xABC,
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
        dut.pageset_packet_i.tags(i).process_id.poke(0x10.U)
        dut.pageset_packet_i.tags(i).vpn.poke((0x20 + i).U)
      }
      // 1. wait for the response of fetching PTEs and response with nothing.
      dut.sendPageTableSet(dut.M_AXI_QEMU_MISS, (0xAB * 64 * 3).U)
      // 2. It should allocate Free PPN

      // 3. The page deletor should be activated.
      // 3.1 TLB eviction
      dut.waitForSignalToBe(dut.dtlb_flush_request_o.valid)
      dut.dtlb_flush_request_o.bits.thread_id.expect(0.U)
      dut.dtlb_flush_request_o.bits.vpage.expect(0x20.U)
      dut.dtlb_flush_reply_i.bits.dirty.poke(true.B)
      dut.dtlb_flush_reply_i.bits.hit.poke(true.B)
      dut.dtlb_flush_reply_i.bits.violation.poke(false.B)
      dut.dtlb_flush_reply_i.bits.entry.modified.poke(true.B)
      dut.dtlb_flush_reply_i.bits.entry.permission.poke(1.U)
      dut.dtlb_flush_reply_i.bits.entry.pp.poke(0x10.U)
      timescope {
        dut.dtlb_flush_reply_i.valid.poke(true.B)
        dut.dtlb_flush_request_o.ready.poke(true.B)
        dut.tk()
      }
      // 3.2 Send eviction start
      dut.expectQEMUMessage(
        0,
        5,
        Seq(0x10, 1, 1)
      )
      // 3.3 Cache Eviction
      for(i <- 0 until 64){
        dut.waitForSignalToBe(dut.dcache_flush_request_o.valid)
        dut.dcache_flush_request_o.bits.addr.expect((0x10 * 64 + i).U)
        dut.dcache_flush_request_o.bits.thread_id.expect(0.U)
        timescope {
          dut.dcache_flush_request_o.ready.poke(true.B)
          dut.tk()
        }
      }

      // 3.4 Wait Cache to be empty
      dut.dcache_wb_queue_empty_i.poke(true.B)

      // 3.5 Move Page out of the DRAM
      dut.waitForSignalToBe(dut.M_AXI_PAGE.ar.arvalid)
      dut.M_AXI_PAGE.ar.araddr.expect((0x10 * 4096).U)
      timescope {
        dut.M_AXI_PAGE.ar.arready.poke(true.B)
        dut.tk()
      }

      for(i <- 0 until 64){
        dut.M_AXI_PAGE.r.rdata.poke(0x0ABCDEF.U)
        dut.M_AXI_PAGE.r.rresp.poke(0.U)
        dut.M_AXI_PAGE.r.rlast.poke((i == 63).B)
        dut.waitForSignalToBe(dut.M_AXI_PAGE.r.rready)
        timescope {
          dut.M_AXI_PAGE.r.rvalid.poke(true.B)
          dut.tk()
        }
      }

      // 3.6 Send eviction done
      dut.expectQEMUMessage(
        64, // increase by 1.
        6,
        Seq(0x10, 1, 1)
      )

      // 4. It should inserting pages.
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
      
      // 5. It should insert the PTE to page table
      dut.receivePageTableSet(dut.M_AXI_QEMU_MISS, (0xAB * 64 * 3).U)
      dut.pageset_packet_o.ptes(0).ppn.expect(0x10000.U)
      dut.pageset_packet_o.ptes(0).modified.expect(false.B)
      dut.pageset_packet_o.ptes(0).permission.expect(1.U)
      dut.pageset_packet_o.tags(0).process_id.expect(0x10.U)
      dut.pageset_packet_o.tags(0).vpn.expect(0xABC.U)
      // 6. It should response to the TLB for page arrive.
      dut.waitForSignalToBe(dut.dtlb_backend_reply_o.valid)
      dut.dtlb_backend_reply_o.bits.tag.thread_id.expect(0.U)
      dut.dtlb_backend_reply_o.bits.tag.vpage.expect(0xABC.U)
      dut.dtlb_backend_reply_o.bits.data.modified.expect(false.B)
      dut.dtlb_backend_reply_o.bits.data.pp.expect(0x10000.U)
      dut.dtlb_backend_reply_o.bits.data.permission.expect(1.U)
    }
  }

  "With Synonym" in {
    import PageDemanderDriver._
    val anno = Seq(TargetDirAnnotation("test/demander/pagefault_resolution/with_synonym"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new PageDemanderDUT(new MemorySystemParameter())).withAnnotations(anno){ dut=>
      dut.registerThreadTable(0, 0x10)
      dut.registerThreadTable(1, 0xEF)
      dut.sendPageFaultResponse(
        0xABC,
        0x10,
        1,
        true,
        0xCCD,
        0xEF
      )

      timescope {
        // Okay, the page fault resolution will find 0xCCD first.
        dut.pageset_packet_i.valids.poke(1.U)
        dut.pageset_packet_i.tags(0).process_id.poke(0x0EF.U)
        dut.pageset_packet_i.tags(0).vpn.poke(0x0CCD.U)
        dut.pageset_packet_i.ptes(0).modified.poke(true.B)
        dut.pageset_packet_i.ptes(0).permission.poke(0.U)
        dut.pageset_packet_i.ptes(0).ppn.poke(0x0FF.U)

        // It will fetch the synonym's set
        dut.sendPageTableSet(dut.M_AXI_QEMU_MISS, (0xCC * 64 * 3).U)
      }
      
      // Then fetch the target set
      dut.sendPageTableSet(dut.M_AXI_QEMU_MISS, (0xAB * 64 * 3).U)

      // and write it back
      dut.receivePageTableSet(dut.M_AXI_QEMU_MISS, (0xAB * 64 * 3).U)
      dut.pageset_packet_o.ptes(0).ppn.expect(0x0FF.U)
      dut.pageset_packet_o.ptes(0).permission.expect(1.U)
      dut.pageset_packet_o.ptes(0).modified.expect(false.B)
      dut.pageset_packet_o.tags(0).process_id.expect(0x10.U)
      dut.pageset_packet_o.tags(0).vpn.expect(0xABC.U)

      // response to the TLB
      dut.waitForSignalToBe(dut.dtlb_backend_reply_o.valid)
      dut.dtlb_backend_reply_o.bits.tag.thread_id.expect(0.U)
      dut.dtlb_backend_reply_o.bits.tag.vpage.expect(0xABC.U)
      dut.dtlb_backend_reply_o.bits.data.modified.expect(false.B)
      dut.dtlb_backend_reply_o.bits.data.pp.expect(0x0FF.U)
      dut.dtlb_backend_reply_o.bits.data.permission.expect(1.U)
    }
  }
}