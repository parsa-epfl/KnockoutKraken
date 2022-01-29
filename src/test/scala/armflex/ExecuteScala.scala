package armflex

import chisel3._
import chisel3.experimental._

import org.scalatest.freespec.AnyFreeSpec
import chiseltest._
import chiseltest.internal._

import firrtl.options.TargetDirAnnotation

import arm.DEC_LITS._

class DecodeBitMasksUnitTest extends AnyFreeSpec with ChiselScalatestTester {
  // Plan: Just print the iteration value and see the maximum period.
  def decodeBitMasksPokeExpect(
      dut: DecodeBitMasks,
      inst: BigInt
  ): Unit = {
    val immn = (inst >> 22) & 0x1
    val imms = (inst >> 10) & 0x3f
    val immr = (inst >> 16) & 0x3f
    dut.io.immn.poke(immn.U)
    dut.io.imms.poke(imms.U)
    dut.io.immr.poke(immr.U)
    dut.clock.step(1)
    val (wmask, tmask) = ExecuteModels.DecodeBitMask(immn, imms, immr)
    dut.io.wmask.expect(wmask.U)
    dut.io.tmask.expect(tmask.U)
  }

  val anno = Seq(
    VerilatorBackendAnnotation,
    TargetDirAnnotation("test/execute/DecodeBitMask"),
    WriteVcdAnnotation
  )

  "b200c3e8:orr     x8, xzr, #0x101010101010101" in {
    test(new DecodeBitMasks).withAnnotations(anno) { dut =>
      decodeBitMasksPokeExpect(dut, BigInt("b200c3e8", 16))
    }
  }

  "92402c04:and     x4, x0,  #0xfff" in {
    test(new DecodeBitMasks).withAnnotations(anno) { dut =>
      decodeBitMasksPokeExpect(dut, BigInt("92402c04", 16))
    }
  }
}

class DataProcessingUnitTest extends AnyFreeSpec with ChiselScalatestTester {
  // Plan: Just print the iteration value and see the maximum period.
  def dataProcessingPokeExpect(
    dut: DataProcessing,
    inst: BigInt,
    reg: BigInt
  ): Unit = {
    val is32bit = ((inst >> 31) & 0x1) == 0
    val opcode = (inst >> 10) & 0x3f
    dut.io.a.poke(reg.U)
    dut.io.is32bit.poke(is32bit.B)
    dut.io.op.poke(opcode.U)
    dut.clock.step(1)
    val res = opcode.toInt match {
      case OP_REV   => ExecuteModels.ReverseBytes(reg, 1)
      case OP_REV32 => ExecuteModels.ReverseBytes(reg, 2)
      case OP_REV16 => ExecuteModels.ReverseBytes(reg, 4)
    }
    dut.io.res.expect(res.U)
  }

  val anno = Seq(
    VerilatorBackendAnnotation,
    TargetDirAnnotation("test/execute/DataProcessing"),
    WriteVcdAnnotation
  )

  "dac00c84:rev     x4, x4" in {
    test(new DataProcessing).withAnnotations(anno) { dut =>
      dataProcessingPokeExpect(
        dut,
        BigInt("dac00c84", 16),
        BigInt("80808080" +
               "00000000", 16)
      )
    }
  }
}

object ExecuteModels {
  def Ones(len: BigInt) = BigInt(Seq.fill(len.toInt)('1').mkString, 2)

  def HighestBitSet(bits: BigInt): Int = {
    var highestBit = -1
    var bitsLeft = bits
    while (bitsLeft != 0) {
      bitsLeft = bitsLeft >> 1
      highestBit = highestBit + 1
    }
    return highestBit
  }

  def Replicate(bits: BigInt, maxM: Int, maxN: Int): BigInt = {
    val lowerBits = bits.toString(2).reverse.padTo(64, '0')
    val pattern = lowerBits.slice(0, maxN)
    val replicate = pattern.reverse.repeat(maxM / maxN)
    BigInt(replicate, 2)
  }

  def DecodeBitMask(
      immn: BigInt,
      imms: BigInt,
      immr: BigInt
  ): (BigInt, BigInt) = {

    // Compute log2 of element size
    // 2^len must be in range [2, M]
    val len = if (immn.toInt == 1) 6 else (HighestBitSet(~imms & 0x3f))
    assert(len >= 1)

    // Determine S, R and S - R parameters
    val levels = Ones(len)

    // For logical immediates an all-ones value of S is reserved
    // since it would generate a useless all-ones result (many times)

    val S = imms & levels
    val R = immr & levels
    val diff = S - R // 6-bit subtract with borrow

    val esize = 1 << len

    val d = diff & ((1 << len) - 1);
    val welem = Ones(S + 1)
    val telem = Ones(d + 1)
    val wmask = Replicate(welem >> R.toInt, 64, esize)
    val tmask = Replicate(telem, 64, esize)
    return (wmask, tmask);
  }

  def ReverseBytes(bits: BigInt, containers: Int): BigInt = {
    if(containers == 1) {
      val bytes = bits.toByteArray.drop(1)
      assert(bytes.size == 8)
      return BigInt(bytes.reverse)
    } else {
      // TODO
      return BigInt("deadbeef", 16)
    }
  }
}
