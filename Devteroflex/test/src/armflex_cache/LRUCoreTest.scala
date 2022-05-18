package armflex_cache

import chisel3._
import chisel3.experimental._

import org.scalatest.freespec.AnyFreeSpec
import chiseltest._
import chiseltest.internal._

import firrtl.options.TargetDirAnnotation

class TestPseudoTreeLRU extends AnyFreeSpec with ChiselScalatestTester{
  // Plan: Just print the iteration value and see the maximum period.
  "TestPseudoLRU" in {
    val wayNumber = 16
    val verbose = false
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/Pseudo"), WriteVcdAnnotation)
    test(new PseudoTreeLRUCore(wayNumber)).withAnnotations(anno){ dut =>
      val pseudoLRU = new LRUCorePseudo(wayNumber)
      val accessList = Seq(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)
      for(access <- accessList){
        dut.io.encoding_i.poke(pseudoLRU.encode)
        val lru = pseudoLRU.getLRU(access, wayNumber)
        dut.io.lru_o.expect(lru)
        dut.io.access_i.poke(access)
        dut.io.encoding_o.expect(pseudoLRU.encode)
        dut.clock.step(1)
      }
    }
  }
  
  "TestPseudoLRU 2" in {
    val wayNumber = 16
    val verbose = false
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/Pseudo"), WriteVcdAnnotation)
    test(new PseudoTreeLRUCore(wayNumber)).withAnnotations(anno){ dut =>
      val pseudoLRU = new LRUCorePseudo(wayNumber)
      for(access <- 0 until wayNumber) {
        dut.io.encoding_i.poke(pseudoLRU.encode)
        val lru = pseudoLRU.getLRU(access, wayNumber)
        dut.io.lru_o.expect(lru)
        dut.io.access_i.poke(access)
        dut.io.encoding_o.expect(pseudoLRU.encode)
        dut.clock.step(1)
      }
    }
  }
}

class LRUCorePseudo(wayNumber: Int) {
  var encode: BigInt = 0
  
  def getLRU(access: BigInt, size: Int): Int = {
    val encodeBits = LRUCorePseudo.getBitVector(encode, size)
    val lru = LRUCorePseudo.getLRU(encodeBits, size)
    val accessPath = LRUCorePseudo.getLRUEncodedPath(access, size)
    val nextEncodeBits = LRUCorePseudo.updateBitVector(accessPath, encodeBits)
    encode = LRUCorePseudo.getBigIntFromVector(nextEncodeBits)
    println(s"0x${encodeBits} -> encoded[${nextEncodeBits}]:lru[${lru}]")
    lru
  }
}

object LRUCorePseudo {
  /**
  * Returns path corrections: inverse of the action taken
  *
  * @param access
  * @param half
  * @param currIdx
  * @param target
  * @param pathSeq
  * @return
  */
  def getLRUEncodedPath(access: BigInt, size: Int): Seq[(Int, Char)] = getLRUEncodedPath(access, size/2, 0, 0, Seq())
 
  def getLRUEncodedPath(access: BigInt, half: Int, currIdx: Int, target: Int, pathSeq: Seq[(Int, Char)]): Seq[(Int, Char)] = {
    if(half == 1) {
      if(access == target + 1) {
        return pathSeq :+ (currIdx, '1')
      } else if (access == target) {
        return pathSeq :+ (currIdx, '0')
      } else {
        assert(false)
        return Nil
      }
    } else {
      if(target + half <= access) {
        getLRUEncodedPath(access, half/2, currIdx + half, target + half, pathSeq :+ (currIdx, '1'))
      } else {
        getLRUEncodedPath(access, half/2, currIdx + 1, target, pathSeq :+ (currIdx, '0'))
      }
    }
  }

  def getLRU(bits: String, size: Int): Int = getLRU(bits, size/2, 0, 0)
  def getLRU(bits: String, half: Int, currIdx: Int, lru: Int): Int = {
    if(half == 1) {
      if(bits(0) == '1') {
        return lru + 1
      } else {
        return lru
      }
    } else if (bits(0) == '1') {
      getLRU(bits.drop(half), half/2, currIdx + half, lru + half)
    } else {
      getLRU(bits.drop(1), half/2, currIdx + 1, lru)
    }
  }
  
  
  def createBitvector(len: Int): String = BigInt(scala.util.Random.nextLong(Math.pow(2, len).toLong)).toString(2).padTo(len, '0')
  def getBitVector(value: BigInt, len: Int): String = value.toString(2).reverse.padTo(len, '0')
  
  def getBigIntFromVector(bitvector: String): BigInt = BigInt(bitvector.reverse, 2)
  def updateBitVector(path: Seq[(Int, Char)], bitvector: String): String = {
    path.foldLeft(bitvector) { 
      (currEncode, step) => currEncode.updated(step._1, step._2 match {
        case '0' => '1'
        case '1' => '0'
      })
    }
  }
}