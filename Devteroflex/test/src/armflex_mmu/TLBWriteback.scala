package armflex_mmu

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import chiseltest._
import chiseltest.experimental._

import chiseltest.simulator.VerilatorBackendAnnotation
import chiseltest.simulator.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation

import armflex.MemoryAccessType._

import org.scalatest.freespec.AnyFreeSpec

class TLBWritebackTester extends AnyFreeSpec with ChiselScalatestTester {
  import MMUDriver._
  "Normal" in {
    val anno = Seq(TargetDirAnnotation("test/demander/tlbwriteback/normal"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new MMUDUT(new MemoryHierarchyParams())).withAnnotations(anno){ dut =>
      // apply a miss request
      // Set the thread table.
      // dut.registerThreadTable(0, 10)
      // set TLB eviction up.
      timescope {
        dut.mmu_tlb_io.data.writebackReq.bits.tag.asid.poke(10.U)
        dut.mmu_tlb_io.data.writebackReq.bits.tag.vpn.poke(0xABC.U)
        dut.mmu_tlb_io.data.writebackReq.bits.entry.perm.poke(DATA_STORE.U)
        dut.mmu_tlb_io.data.writebackReq.bits.entry.ppn.poke(0xCBA.U)
        dut.mmu_tlb_io.data.writebackReq.bits.entry.modified.poke(true.B)
        dut.mmu_tlb_io.data.writebackReq.valid.poke(true.B)
        dut.mmu_tlb_io.data.writebackReq.ready.expect(true.B)
        dut.clock.step()
      }
      // prepare for the data
      dut.encode.packet.lru_bits.poke(1.U)
      dut.encode.packet.valids.poke(1.U)
      dut.encode.packet.entries(0).tag.vpn.poke(0xABC.U)
      dut.encode.packet.entries(0).tag.asid.poke(10.U)
      dut.encode.packet.entries(0).entry.ppn.poke(0xCBA.U)
      dut.encode.packet.entries(0).entry.modified.poke(false.B)
      dut.encode.packet.entries(0).entry.perm.poke(DATA_STORE.U)
      dut.sendPageTableSet(dut.M_AXI, dut.vpn2ptSetPA(10, 0xABC).U)
      
      dut.receivePageTableSet(dut.M_AXI, dut.vpn2ptSetPA(10, 0xABC).U)
      // verify the update
      dut.decode.packet.entries(0).tag.vpn.expect(0xABC.U)
      dut.decode.packet.entries(0).tag.asid.expect(10.U)
      dut.decode.packet.entries(0).entry.ppn.expect(0xCBA.U)
      dut.decode.packet.entries(0).entry.modified.expect(true.B)
    }
  }
}
