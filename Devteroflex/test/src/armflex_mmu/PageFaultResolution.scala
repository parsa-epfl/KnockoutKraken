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

class PageFaultResolutionTester extends AnyFreeSpec with ChiselScalatestTester {
  "No synonym" in {
    import PageDemanderDriver._
    val anno = Seq(TargetDirAnnotation("test/demander/pagefault_resolution/no_synonym"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new MMUDUT(new MemoryHierarchyParams(pAddrW = 36))).withAnnotations(anno){ dut=>
      // send Page
      // dut.registerThreadTable(0, 0x10)
      dut.sendPageFaultResponse(
        0xABC,
        1,
        0x10,
        1,
        0x10000
      )
      // 1. wait for the response of fetching PTEs and response with nothing.
      dut.sendPageTableSet(dut.M_AXI, dut.vpn2ptSetPA(0x10, 0xABC).U)
      // 2. It should allocate Free PPN
      
      // 3. It should insert the PTE to page table
      dut.receivePageTableSet(dut.M_AXI, dut.vpn2ptSetPA(0x10, 0xABC).U)
      dut.pageset_packet_o.ptes(0).ppn.expect(0x10000.U)
      dut.pageset_packet_o.ptes(0).modified.expect(false.B)
      dut.pageset_packet_o.ptes(0).perm.expect(1.U)
      dut.pageset_packet_o.tags(0).asid.expect(0x10.U)
      dut.pageset_packet_o.tags(0).vpn.expect(0xABC.U)
      // 4. It should response to the TLB for page arrive.
      dut.waitForSignalToBe(dut.dtlb_backend_reply_o.valid)
      dut.dtlb_backend_reply_o.bits.tag.asid.expect(0x10.U)
      dut.dtlb_backend_reply_o.bits.tag.vpn.expect(0xABC.U)
      dut.dtlb_backend_reply_o.bits.data.modified.expect(false.B)
      dut.dtlb_backend_reply_o.bits.data.ppn.expect(0x10000.U)
      dut.dtlb_backend_reply_o.bits.data.perm.expect(1.U)
      dut.dtlb_backend_reply_o.bits.thid.expect(1.U)
    }
  }

  "Trigger Page Eviction following a Page Reply" in {
    import PageDemanderDriver._
    val anno = Seq(TargetDirAnnotation("test/demander/pagefault_resolution/trigger_page_eviction"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new MMUDUT(new MemoryHierarchyParams(pAddrW = 36))).withAnnotations(anno){ dut=>
      // send Page
      // dut.registerThreadTable(0, 0x10)
      dut.sendPageFaultResponse(
        0xABC,
        1,
        0x10,
        1,
        0x10000
      )
      // 0. Fake a Page table set
      dut.pageset_packet_i.valids.poke(0xFFFF.U)
      dut.pageset_packet_i.lru_bits.poke(0.U)
      for(i <- 0 until 16){
        dut.pageset_packet_i.ptes(i).modified.poke(false.B)
        dut.pageset_packet_i.ptes(i).perm.poke(1.U)
        dut.pageset_packet_i.ptes(i).ppn.poke((0x10 + i).U)
        dut.pageset_packet_i.tags(i).asid.poke(0x10.U)
        dut.pageset_packet_i.tags(i).vpn.poke((0x20 + i).U)
      }
      // 1. wait for the response of fetching PTEs and response with nothing.
      dut.sendPageTableSet(dut.M_AXI, dut.vpn2ptSetPA(0x10, 0xABC).U)
      // 2. It should allocate Free PPN

      // 3. The page deletor should be activated.
      // 3.1 TLB eviction
      dut.waitForSignalToBe(dut.dtlb_flush_request_o.valid)
      timescope { // acknowledge the flush request.
        dut.dtlb_flush_request_o.ready.poke(true.B)
        dut.itlb_flush_request_o.ready.poke(true.B)
        dut.tk()
      }
      // One cycle later, return the flush result of TLB
      dut.dtlb_flush_request_o.bits.asid.expect(0x10.U)
      dut.dtlb_flush_request_o.bits.vpn.expect(0x20.U)
      // dut.dtlb_flush_reply_i.bits.dirty.poke(true.B)
      dut.dtlb_flush_reply_i.bits.hit.poke(true.B)
      dut.dtlb_flush_reply_i.bits.violation.poke(false.B)
      dut.dtlb_flush_reply_i.bits.entry.modified.poke(true.B)
      dut.dtlb_flush_reply_i.bits.entry.perm.poke(1.U)
      dut.dtlb_flush_reply_i.bits.entry.ppn.poke(0x10.U)
      dut.dtlb_flush_reply_i.valid.poke(true.B)
      timescope {
        dut.dtlb_flush_reply_i.valid.poke(true.B)
        dut.tk()
      }
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
      dut.dcache_wb_queue_empty_i.poke(true.B)

      // 3.5 Send eviction done
      dut.expectQEMUMessage(
        6,
        Seq(0x10, 0x20, 0x0, 0x10, 1, 1)
      )

      // 4. It should insert the PTE to page table
      dut.receivePageTableSet(dut.M_AXI, dut.vpn2ptSetPA(0x10, 0xABC).U)
      dut.pageset_packet_o.ptes(0).ppn.expect(0x10000.U)
      dut.pageset_packet_o.ptes(0).modified.expect(false.B)
      dut.pageset_packet_o.ptes(0).perm.expect(1.U)
      dut.pageset_packet_o.tags(0).asid.expect(0x10.U)
      dut.pageset_packet_o.tags(0).vpn.expect(0xABC.U)
      // 5. It should response to the TLB for page arrive.
      dut.waitForSignalToBe(dut.dtlb_backend_reply_o.valid)
      dut.dtlb_backend_reply_o.bits.tag.asid.expect(0x10.U)
      dut.dtlb_backend_reply_o.bits.tag.vpn.expect(0xABC.U)
      dut.dtlb_backend_reply_o.bits.data.modified.expect(false.B)
      dut.dtlb_backend_reply_o.bits.data.ppn.expect(0x10000.U)
      dut.dtlb_backend_reply_o.bits.data.perm.expect(1.U)
      dut.dtlb_backend_reply_o.bits.thid.expect(1.U)
    }
  }
}