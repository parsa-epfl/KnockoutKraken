package armflex

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
import chiseltest._
import chiseltest.experimental._
import org.scalatest._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import firrtl.options.TargetDirAnnotation

object MemoryUnitDrivers {
  implicit class PipeTLBRequestDriver(dut: PipeTLB.PipeTLBReq) {
    def pokeReq(addr: UInt, thid: UInt, asid: UInt, perm: UInt): Unit = {
      dut.addr.poke(addr)
      dut.thid.poke(thid)
      dut.asid.poke(asid)
      dut.perm.poke(perm)
    }
    def expectReq(addr: UInt, thid: UInt, asid: UInt, perm: UInt): Unit = {
      dut.addr.expect(addr)
      dut.thid.expect(thid)
      dut.asid.expect(asid)
      dut.perm.expect(perm)
    }
  }

  implicit class PipeTLBResponseDriver(dut: PipeTLB.PipeTLBResp) {
    def pokeResp(addr: UInt, hit: Bool, miss: Bool, violation: Bool): Unit = {
      dut.addr.poke(addr)
      dut.hit.poke(hit)
      dut.miss.poke(miss)
      dut.violation.poke(violation)
    }
  }

  implicit class MemoryUnitDriver(dut: MemoryUnit) {
    implicit val clock = dut.clock
  }
}

class MemoryUnitTests extends FreeSpec with ChiselScalatestTester {
}
