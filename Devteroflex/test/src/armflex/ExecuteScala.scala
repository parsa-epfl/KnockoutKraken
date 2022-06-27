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
      inst: BigInt,
      reg: BigInt
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
      decodeBitMasksPokeExpect(dut, BigInt("b200c3e8", 16), 0)
    }
  }

  "b202e7e0:orr     x0, xzr, #0xcccccccccccccccc" in {
    test(new DecodeBitMasks).withAnnotations(Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/execute/DecodeBitMask/orr2"), WriteVcdAnnotation)) { dut =>
      decodeBitMasksPokeExpect(dut, BigInt("b202e7e0", 16), 0)
    }
  }

  "92402c04:and     x4, x0,  #0xfff" in {
    test(new DecodeBitMasks).withAnnotations(anno) { dut =>
      decodeBitMasksPokeExpect(dut, BigInt("92402c04", 16), 0)
    }
  }
}

class LogicALUUnitTest extends AnyFreeSpec with ChiselScalatestTester {
  // Plan: Just print the iteration value and see the maximum period.
  def logicALUPokeExpect(
      dut: LogicALU,
      inst: BigInt,
      inA: BigInt,
      inB: BigInt
  ): Unit = {
    val opcode = ((inst >> 29) & 0x3).toInt match {
      case 0 => OP_AND
      case 1 => OP_ORR
      case 2 => OP_EOR
      case 3 => OP_AND
    }

    val is32bit = ((inst >> 31) & 0x1) == 0
    val res = opcode match {
      case OP_AND => inA & inB
      case OP_ORR => inA | inB
      case OP_EOR => inA ^ inB
    }

    val resBitString = ExecuteModels.toBitString(res, 64)
    val nzcvBools = if(is32bit) {
      Seq(resBitString(31) == '1', BigInt(resBitString.reverse.drop(32).reverse, 2) == 0, false, false)
    } else {
      Seq(resBitString(63) == '1', res == 0, false, false)
    }

    val nzcvBits = nzcvBools.map {
      case false => '0'
      case true => '1'
    }
    val nzcv = BigInt(nzcvBits.mkString, 2)
    println(is32bit.toString + "nzcvBits: " + nzcvBits.mkString + "nzcv" + nzcv)
    
    dut.io.is32bit.poke(is32bit.B)
    dut.io.a.poke(inA.U)
    dut.io.b.poke(inB.U)
    dut.io.opcode.poke(opcode.U)
    dut.clock.step(1)

    dut.io.res.expect(res.U)
    dut.io.nzcv.expect(nzcv.U)
  }

  val anno = Seq(
    VerilatorBackendAnnotation,
    TargetDirAnnotation("test/execute/LogicALU"),
    WriteVcdAnnotation
  )

  "72000422:ands    w1, w2, #3" in {
    test(new LogicALU).withAnnotations(anno) { dut =>
      logicALUPokeExpect(dut, BigInt("72000422", 16), BigInt("0000000300000003", 16), BigInt("6ABCCD638373A7E4", 16))
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
      case OP_RBIT  => ExecuteModels.ReverseBit(reg, is32bit)
      case OP_REV   => ExecuteModels.ReverseBytes(reg, 1)
      case OP_REV32 => ExecuteModels.ReverseBytes(reg, 2)
      case OP_REV16 => ExecuteModels.ReverseBytes(reg, 4)
      case OP_CLZ   => if(!is32bit) ExecuteModels.CountLeadingZeroes(reg, 64) 
                       else ExecuteModels.CountLeadingZeroes(reg, 32)
      //case OP_CLS   => ExecuteModels.CountLeadingSing(reg, 64)
    }
    dut.io.res.expect(res.U)
  }

  val anno = Seq(
    VerilatorBackendAnnotation,
    TargetDirAnnotation("test/execute/DataProcessing"),
    WriteVcdAnnotation
  )

  "dac00c84:rev     x4, x4 (0x8080808000000000)" in {
    test(new DataProcessing).withAnnotations(anno) { dut =>
      dataProcessingPokeExpect(
        dut,
        BigInt("dac00c84", 16),
        BigInt("80808080" +
               "00000000", 16)
      )
    }
  }
  "dac00063:rbit    x3,x3 (0x1000000000000)" in {
    test(new DataProcessing).withAnnotations(anno) { dut =>
      dataProcessingPokeExpect(
        dut,
        BigInt("dac00063", 16),
        BigInt("1000000000000", 16)
      )
    }
  }

  "dac010e5:clz     x5, x7 (0x0000000000000000)" in {
    test(new DataProcessing).withAnnotations(anno) { dut =>
      dataProcessingPokeExpect(
        dut,
        BigInt("dac010e5", 16),
        BigInt("0000000000000000", 16)
        )
    }
  }

  "dac010e5:clz     x5, x7 (0xFFFFFFFFFFFFFFFF)" in {
    test(new DataProcessing).withAnnotations(anno) { dut =>
      dataProcessingPokeExpect(
        dut,
        BigInt("dac010e5", 16),
        BigInt("FFFFFFFFFFFFFFFF", 16)
        )
    }
  }

}

object ExecuteModels {
  def toBigInt(binary: String, bitsize: Int) = BigInt(binary.padTo(bitsize, '0'), 2)
  def toBitString(bits: BigInt, bitsize: Int) = bits.toString(2).reverse.padTo(bitsize, '0')
 
  def ROR(bits: BigInt, shift: Int, size: Int): BigInt = ((bits >> shift) | (bits << (size - shift)) & Ones(size))

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

  def ReverseBit(bits: BigInt, is32bit: Boolean): BigInt = {
    if(is32bit) {
      toBigInt(bits.toString(2).reverse, 32)
    } else {                         
      toBigInt(bits.toString(2).reverse, 64)
    }
  }

  def CountLeadingZeroes(bits: BigInt, bitsize: Int): BigInt = {
    val bitString = bits.toString(2).reverse.padTo(bitsize, '0').reverse
    val nonZero = bitString.dropWhile(_ == '0')
    bitString.size - nonZero.size
  }

  def CountLeadingSing(bits: BigInt, bitsize: Int): BigInt = {
    // TODO
    val bitString = bits.toString(2).reverse.padTo(bitsize, '0')
    val nonZero = bitString.dropWhile(_ == '1')
    bitString.size - nonZero.size
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
    val wmask = Replicate(ROR(welem, R.toInt, esize), 64, esize)
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
