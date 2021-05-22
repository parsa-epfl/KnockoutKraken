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

class TLBWritebackTester extends FreeSpec with ChiselScalatestTester {
  import PageDemanderDriver._
  "Normal" in {
    val anno = Seq(TargetDirAnnotation("test/demander/tlbwriteback/normal"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new PageDemanderDUT(new PageDemanderParameter())).withAnnotations(anno){ dut =>
      // apply a miss request
      // Set the thread table.
      // dut.registerThreadTable(0, 10)
      // set TLB eviction up.
      timescope {
        dut.dtlb_backend_request_i.bits.flush_v.poke(false.B)
        dut.dtlb_backend_request_i.bits.permission.poke(1.U)
        dut.dtlb_backend_request_i.bits.w_v.poke(true.B)
        dut.dtlb_backend_request_i.bits.tag.asid.poke(10.U)
        dut.dtlb_backend_request_i.bits.tag.vpn.poke(0xABC.U)
        dut.dtlb_backend_request_i.bits.entry.modified.poke(true.B)
        dut.dtlb_backend_request_i.bits.entry.ppn.poke(0xCBA.U)
        dut.dtlb_backend_request_i.bits.entry.permission.poke(1.U)
        dut.dtlb_backend_request_i.valid.poke(true.B)
        dut.dtlb_backend_request_i.ready.expect(true.B)
        dut.tk()
      }
      // prepare for the data
      dut.pageset_packet_i.lru_bits.poke(1.U)
      dut.pageset_packet_i.valids.poke(1.U)
      dut.pageset_packet_i.tags(0).vpn.poke(0xABC.U)
      dut.pageset_packet_i.tags(0).asid.poke(10.U)
      dut.pageset_packet_i.ptes(0).ppn.poke(0xCBA.U)
      dut.pageset_packet_i.ptes(0).modified.poke(false.B)
      dut.pageset_packet_i.ptes(0).permission.poke(1.U)
      dut.sendPageTableSet(dut.M_AXI, (0xAB * 3 * 64).U)
      
      dut.receivePageTableSet(dut.M_AXI, (0xAB * 3 * 64).U)
      // verify the update
      dut.pageset_packet_o.tags(0).vpn.expect(0xABC.U)
      dut.pageset_packet_o.tags(0).asid.expect(10.U)
      dut.pageset_packet_o.ptes(0).ppn.expect(0xCBA.U)
      dut.pageset_packet_o.ptes(0).modified.expect(true.B)
    }
  }
}
