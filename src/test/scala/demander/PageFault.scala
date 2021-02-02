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


class PageDemanderPageFaultTester extends FreeSpec with ChiselScalatestTester {
  import PageDemanderDriver._
  "Normal" in {
    val anno = Seq(TargetDirAnnotation("test/demander/pagefault/normal"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new PageDemanderDUT(new MemorySystemParameter(), "src/main/cpp/demander.txt")).withAnnotations(anno){ dut =>
      // apply a miss request
      // Set the thread table.
      dut.registerThreadTable(0.U, 10.U)
      // dut.itlb_backend_request_i.bits.
      timescope {
        dut.itlb_backend_request_i.bits.flush_v.poke(false.B)
        dut.itlb_backend_request_i.bits.need_write_permission_v.poke(false.B)
        dut.itlb_backend_request_i.bits.w_v.poke(false.B)
        dut.itlb_backend_request_i.bits.tag.thread_id.poke(0.U)
        dut.itlb_backend_request_i.bits.tag.vpage.poke(0xABC.U)
        dut.itlb_backend_request_i.valid.poke(true.B)
        dut.itlb_backend_request_i.ready.expect(true.B)
        dut.tk()
      }
      dut.waitForSignalToBe(dut.M_AXI_PTSet.ar.arvalid)
      // Ok, how to handle the reply?
      // Maybe we should add a DRAM here? Although I can manually reply to it. (Let's do it manually.)
      dut.M_AXI_PTSet.ar.araddr.expect((0xAB * 3 * 64).U)
      dut.M_AXI_PTSet.ar.arlen.expect(2.U)
      // accept the read request
      timescope {
        dut.M_AXI_PTSet.ar.arready.poke(true.B)
        dut.tk()
      }
      // prepare for the data
      dut.pageset_packet_i.lru_bits.poke(1.U)
      dut.pageset_packet_i.valids.poke(1.U)
      dut.pageset_packet_i.tags(0).vpn.poke(0xABC.U)
      dut.pageset_packet_i.tags(0).process_id.poke(10.U)
      dut.pageset_packet_i.ptes(0).ppn.poke(0xCBA.U)
      dut.pageset_packet_i.ptes(0).modified.poke(false.B)
      dut.pageset_packet_i.ptes(0).permission.poke(2.U)
      // pushing data to the dut.
      timescope {
        for(i <- 0 until 3){
          dut.M_AXI_PTSet.r.rdata.poke(dut.pageset_converter_raw_o(i).peek)
          dut.M_AXI_PTSet.r.rid.poke(0.U)
          dut.M_AXI_PTSet.r.rlast.poke((i == 2).B)
          dut.M_AXI_PTSet.r.rresp.poke(0.U)
          dut.M_AXI_PTSet.r.rvalid.poke(true.B)
          dut.M_AXI_PTSet.r.rready.expect(true.B)
          dut.tk()
        }
      }
      // wait for the reply to the TLB
      while(!dut.tlb_backend_reply_o(0).valid.peek.litToBoolean) dut.tk()
      dut.tlb_backend_reply_o(0).bits.tag.thread_id.expect(0.U)
      dut.tlb_backend_reply_o(0).bits.tag.vpage.expect(0xABC.U)
      dut.tlb_backend_reply_o(0).bits.data.modified.expect(false.B)
      dut.tlb_backend_reply_o(0).bits.data.pp.expect(0xCBA.U)
      dut.tlb_backend_reply_o(0).bits.data.permission.expect(2.U)
    }
  }
}
