package armflex

import chisel3._
import chisel3.experimental._

import org.scalatest.freespec.AnyFreeSpec
import chiseltest._
import chiseltest.internal._

import firrtl.options.TargetDirAnnotation

import arm.DEC_LITS._

class LoadStoreUnitTest extends AnyFreeSpec with ChiselScalatestTester {
  def pokeLSUImm(dut: LDSTUnit, inst: BigInt, rVal1: BigInt): Unit = {
    val imm12 = (inst >> 10) & 0xFFF
    val rn = (inst >> 0) & 0x1f
    val rt = (inst >> 5) & 0x1f
    val size = (inst >> 30) & 0x3
    val isLoad = (inst >> 22) & 0x1
    val isSigned = (inst >> 23) & 0x1
    val op = size | (isLoad << 2) | (isSigned << 3)
    dut.io.dinst.itype.poke(I_LSUImm.U)
    dut.io.dinst.rs1.poke(rt.U)
    dut.io.dinst.rs2.poke(rt.U)
    dut.io.dinst.rd.bits.poke(rn.U)
    dut.io.dinst.imm.poke(imm12.U)
    dut.io.dinst.op.poke(op.U)
    dut.io.rVal1.poke(rVal1.U)
    dut.io.rVal2.poke(0x1.U)
    dut.io.rVal3.poke(0x1.U)
  }

  val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/execute/LoadStoreUnit"))

  "3902dfe0  strb     w0, [sp, #0xb7]" in {
    test(new LDSTUnit).withAnnotations(anno) { dut =>
      pokeLSUImm(dut, BigInt("3902dfe0", 16), BigInt("0000FFFFEF981E10", 16))
      dut.clock.step(1)
      dut.io.minst.bits.exceptions.bits.unalignedExcp.expect(false.B)
      dut.io.minst.bits.exceptions.bits.unalignedExcpSP.expect(true.B)
    }
  }
}
