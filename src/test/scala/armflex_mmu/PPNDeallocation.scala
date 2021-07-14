package armflex_mmu


import chisel3._

import chiseltest._
import chiseltest.experimental._

import TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.internal.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation


import org.scalatest.FreeSpec

class PPNDeallocationTester extends FreeSpec with ChiselScalatestTester {
  "Normal case" in {
    import PageDemanderDriver._
    val anno = Seq(TargetDirAnnotation("test/demander/ppn_deallocation/normal"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new MMUDUT(new MMUParameter())).withAnnotations(anno){ dut=>
      // 1. push page.
      // dut.registerThreadTable(0, 0x10)
      dut.sendPageFaultResponse(
        0xABC,
        0x1,
        0x10,
        1,
        0x10000,
      )
      // 1.1. wait for the response of fetching PTEs and response with nothing.
      dut.sendPageTableSet(dut.M_AXI, (0xAB * 64 * 3).U)
      // 1.2. It should allocate Free PPN

      // 1.3. It should insert the PTE to page table
      dut.receivePageTableSet(dut.M_AXI, (0xAB * 64 * 3).U)
      dut.pageset_packet_o.ptes(0).ppn.expect(0x10000.U)
      dut.pageset_packet_o.ptes(0).modified.expect(false.B)
      dut.pageset_packet_o.ptes(0).permission.expect(1.U)
      dut.pageset_packet_o.tags(0).asid.expect(0x10.U)
      dut.pageset_packet_o.tags(0).vpn.expect(0xABC.U)
      dut.pageset_packet_o.valids.expect(1.U)
      // 1.5. It should response to the TLB for page arrive.
      dut.waitForSignalToBe(dut.dtlb_backend_reply_o.valid)
      dut.dtlb_backend_reply_o.bits.tag.asid.expect(0x10.U)
      dut.dtlb_backend_reply_o.bits.tag.vpn.expect(0xABC.U)
      dut.dtlb_backend_reply_o.bits.data.modified.expect(false.B)
      dut.dtlb_backend_reply_o.bits.data.ppn.expect(0x10000.U)
      dut.dtlb_backend_reply_o.bits.data.permission.expect(1.U)
      dut.dtlb_backend_reply_o.bits.tid.expect(1.U)

      // 2. evict the page.
      dut.sendQEMUMessage(
        7, 
        Seq[BigInt](
          0xABC,
          0x0,
          0x10
        )
      )

      // 3. lookup PT
      dut.pageset_packet_i.ptes(0).ppn.poke(0x10000.U)
      dut.pageset_packet_i.ptes(0).modified.poke(false.B)
      dut.pageset_packet_i.ptes(0).permission.poke(1.U)
      dut.pageset_packet_i.tags(0).asid.poke(0x10.U)
      dut.pageset_packet_i.tags(0).vpn.poke(0xABC.U)
      dut.pageset_packet_i.valids.poke(1.U)
      dut.sendPageTableSet(dut.M_AXI, (0xAB * 64 * 3).U)

      // 3. wait for TLB eviction. Let's assume not hit this time.
      dut.waitForSignalToBe(dut.dtlb_flush_request_o.valid)
      timescope {
        dut.dtlb_flush_reply_i.valid.poke(true.B)
        dut.dtlb_flush_request_o.ready.poke(true.B)
        dut.tk()
      }
      
      // 4. send start message
      dut.expectQEMUMessage(
        5,
        Seq(0xABC, 0x0, 0x10, 0x10000, 1, 0)
      )

      // 5. flush cache
      for(i <- 0 until 64){
        dut.waitForSignalToBe(dut.dcache_flush_request_o.valid)
        dut.dcache_flush_request_o.bits.addr.expect((0x10000 * 64 + i).U)
        timescope {
          dut.dcache_flush_request_o.ready.poke(true.B)
          dut.tk()
        }
      }

      // 6. Wait Cache to be empty
      dut.dcache_wb_queue_empty_i.poke(true.B)

      // 7. The PTE should be invalid.
      dut.receivePageTableSet(dut.M_AXI, (0xAB * 64 * 3).U)
      dut.pageset_packet_o.valids.expect(0.U)

    }
  }
}