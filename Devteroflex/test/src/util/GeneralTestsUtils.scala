package armflex.util

import chisel3._
import chiseltest._

import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._


object TestUtils {
  /**
    *
    * 
    * @param vecSize Size of the vector to return
    * @param initLit Initial values for given indexes
    * @param defaultGen Default literal for indexes not present in `initLit`
    * @return  Vector literal with `initLit` on matching indexes and `defaultGen` on other indexes
    */
  def vecLitMake[T <: Data](vecSize: Int, initLit: Seq[(Int, () => T)], defaultGen: () => T): Vec[T] =  {
    assert(initLit.size <= vecSize)
    val litGenerators = initLit.toMap
    val lits = for (i <- 0 until vecSize ) yield (i -> litGenerators.get(i).getOrElse(defaultGen)())
    val vecLits = Vec(vecSize, defaultGen().cloneType).Lit(lits:_*)
    vecLits 
  }

  /**
    *
    * 
    * @param vecSize Size of the vector to return
    * @param initLit Init generator
    * @param ignoreIdx Indexes to not initialize
    * @return  Vector literal with `initLit` on non ignored indexes
    */
  def vecLitMake[T <: Data](vecSize: Int, initLit: () => T, ignoreIdx: Seq[Int]): Vec[T] =  {
    val lits = for (i <- 0 until vecSize if(!ignoreIdx.contains(i))) yield (i -> initLit())
    val vecLits = Vec(vecSize, initLit().cloneType).Lit(lits:_*)
    vecLits 
  }

  /**
    * 
    *
    * @param vecSize Size of the vector
    * @param initLit The lit generator
    * @return All elements of vector initialized with `initLit`
    */
  def vecLitMake[T <: Data](vecSize: Int, initLit: () => T): Vec[T] = vecLitMake(vecSize, initLit, Nil)
}

object SimTools {
  def log(str: String)(verbose: Boolean) = if(verbose) {
    val withClock = true
    if(withClock) {
      val cycle = -1 // Context().backend.getClockCycle() // Need to compile modified chisel-testers2
      println(s"${"%3d".format((cycle-1)*2)}ns:" + str)
    } else {
      println(str)
    }
  }
}
