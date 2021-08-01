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

class PageWalkTester extends FreeSpec with ChiselScalatestTester {
  import PageDemanderDriver._
  "Normal" in {
    val anno = Seq(TargetDirAnnotation("test/demander/pagewalk/normal"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new MMUDUT(new MemoryHierarchyParams())).withAnnotations(anno){ dut =>
      // apply a miss request
      // Set the thread table.
      // dut.registerThreadTable(0, 10)
      timescope {
        dut.itlb_backend_request_i.bits.flush_v.poke(false.B)
        dut.itlb_backend_request_i.bits.perm.poke(2.U)
        dut.itlb_backend_request_i.bits.w_v.poke(false.B)
        dut.itlb_backend_request_i.bits.tag.asid.poke(0x10.U)
        dut.itlb_backend_request_i.bits.tag.vpn.poke(0xABC.U)
        dut.itlb_backend_request_i.valid.poke(true.B)
        dut.itlb_backend_request_i.ready.expect(true.B)
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

      dut.sendPageTableSet(dut.M_AXI, (0xAB * 3 * 64).U)
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
        dut.dtlb_backend_request_i.bits.flush_v.poke(false.B)
        dut.dtlb_backend_request_i.bits.perm.poke(0.U)
        dut.dtlb_backend_request_i.bits.w_v.poke(false.B)
        dut.dtlb_backend_request_i.bits.tag.asid.poke(0x10.U)
        dut.dtlb_backend_request_i.bits.tag.vpn.poke(0xABC.U)
        dut.dtlb_backend_request_i.bits.thid.poke(1.U) // set pid.
        dut.dtlb_backend_request_i.valid.poke(true.B)
        dut.dtlb_backend_request_i.ready.expect(true.B)
        dut.tk()
      }
      dut.sendPageTableSet(dut.M_AXI, (0xAB * 3 * 64).U)
      // This will trigger an page fault.
      println("Page fault should have been triggered. Check it now.")
      
      dut.expectQEMUMessage(
        4, Seq(0xABC, 0, 0x10, 0, 1)
      )

      // dut.waitForSignalToBe(dut.M_AXI_QEMU_MQ.aw.awvalid)
      // dut.M_AXI_QEMUTX.aw.awaddr.expect(0.U)
      // // ack it
      // timescope {
      //   dut.M_AXI_QEMUTX.aw.awready.poke(true.B)
      //   dut.tk()
      // }
      // // wait for the data
      // println("Wait for the data part to be there")
      // dut.waitForSignalToBe(dut.M_AXI_QEMUTX.w.wvalid)
      // // What's the data?
      // // 0: valid 1: message_type(4.U) 2: tag.vpn(31:0), 3: tag.vpn(51, 32), 4: process_id, 5: perm
      // val data_seq = Seq[BigInt](1, 4, 0xABC, 0, 10, 0)
      // dut.M_AXI_QEMUTX.w.wdata.expect(
      //   data_seq.reverse.foldLeft(BigInt(0))({ case(res, cur) =>
      //     (res << 32) | cur
      //   }).U
      // )
      // dut.M_AXI_QEMUTX.w.wlast.expect(true.B)
      // // Mark the writeback accepted.
      // timescope {
      //   dut.M_AXI_QEMUTX.w.wready.poke(true.B)
      //   dut.tk()
      // }
      // // write response
      // timescope {
      //   dut.M_AXI_QEMUTX.b.bvalid.poke(true.B)
      //   dut.M_AXI_QEMUTX.b.bresp.poke(0.U)
      //   dut.M_AXI_QEMUTX.b.bready.expect(true.B)
      //   dut.tk()
      // }
    }
  }
}
