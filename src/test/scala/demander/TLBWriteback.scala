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

class TLBWritebackTester extends FreeSpec with ChiselScalatestTester {
  import PageDemanderDriver._
  "Normal" in {
    val anno = Seq(TargetDirAnnotation("test/demander/tlbwriteback/normal"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new PageDemanderDUT(new MemorySystemParameter())).withAnnotations(anno){ dut =>
      // apply a miss request
      // Set the thread table.
      dut.registerThreadTable(0.U, 10.U)
      // set TLB eviction up.
      timescope {
        dut.dtlb_backend_request_i.bits.flush_v.poke(false.B)
        dut.dtlb_backend_request_i.bits.need_write_permission_v.poke(true.B)
        dut.dtlb_backend_request_i.bits.w_v.poke(true.B)
        dut.dtlb_backend_request_i.bits.tag.thread_id.poke(0.U)
        dut.dtlb_backend_request_i.bits.tag.vpage.poke(0xABC.U)
        dut.dtlb_backend_request_i.bits.entry.modified.poke(true.B)
        dut.dtlb_backend_request_i.bits.entry.pp.poke(0xCBA.U)
        dut.dtlb_backend_request_i.bits.entry.permission.poke(1.U)
        dut.dtlb_backend_request_i.valid.poke(true.B)
        dut.dtlb_backend_request_i.ready.expect(true.B)
        dut.tk()
      }
      dut.waitForSignalToBe(dut.M_AXI_TLBWB.ar.arvalid)
      // Ok, how to handle the reply?
      // Maybe we should add a DRAM here? Although I can manually reply to it. (Let's do it manually.)
      dut.M_AXI_TLBWB.ar.araddr.expect((0xAB * 3 * 64).U)
      dut.M_AXI_TLBWB.ar.arlen.expect(2.U)
      // accept the read request
      timescope {
        dut.M_AXI_TLBWB.ar.arready.poke(true.B)
        dut.tk()
      }
      // prepare for the data
      dut.pageset_packet_i.lru_bits.poke(1.U)
      dut.pageset_packet_i.valids.poke(1.U)
      dut.pageset_packet_i.tags(0).vpn.poke(0xABC.U)
      dut.pageset_packet_i.tags(0).process_id.poke(10.U)
      dut.pageset_packet_i.ptes(0).ppn.poke(0xCBA.U)
      dut.pageset_packet_i.ptes(0).modified.poke(false.B)
      dut.pageset_packet_i.ptes(0).permission.poke(1.U)
      // pushing data to the dut.
      timescope {
        for(i <- 0 until 3){
          dut.M_AXI_TLBWB.r.rdata.poke(dut.pageset_converter_raw_o(i).peek)
          dut.M_AXI_TLBWB.r.rid.poke(0.U)
          dut.M_AXI_TLBWB.r.rlast.poke((i == 2).B)
          dut.M_AXI_TLBWB.r.rresp.poke(0.U)
          dut.M_AXI_TLBWB.r.rvalid.poke(true.B)
          dut.M_AXI_TLBWB.r.rready.expect(true.B)
          dut.tk()
        }
      }
      // wait for the update.
      dut.waitForSignalToBe(dut.M_AXI_TLBWB.aw.awvalid)
      dut.M_AXI_TLBWB.aw.awaddr.expect((0xAB * 3 * 64).U)
      dut.M_AXI_TLBWB.aw.awlen.expect(2.U)
      // reply to AW channel
      timescope {
        dut.M_AXI_TLBWB.aw.awready.poke(true.B)
        dut.tk()
      }
      // wait for the write request
      for(i <- 0 until 3){
        dut.waitForSignalToBe(dut.M_AXI_TLBWB.w.wvalid)
        dut.pageset_converter_raw_i(i).poke(dut.M_AXI_TLBWB.w.wdata.peek())
        timescope {
          dut.M_AXI_TLBWB.w.wready.poke(true.B)
          dut.tk()
        }
      }
      // AXI write reply
      timescope {
        dut.waitForSignalToBe(dut.M_AXI_TLBWB.b.bready)
        dut.M_AXI_TLBWB.b.bvalid.poke(true.B)
        dut.tk()
      }
      // verify the update
      dut.pageset_packet_o.tags(0).vpn.expect(0xABC.U)
      dut.pageset_packet_o.tags(0).process_id.expect(10.U)
      dut.pageset_packet_o.ptes(0).ppn.expect(0xCBA.U)
      dut.pageset_packet_o.ptes(0).modified.expect(true.B)
    }
  }
}
