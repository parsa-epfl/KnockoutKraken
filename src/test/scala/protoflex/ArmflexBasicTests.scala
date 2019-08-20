package protoflex

import scala.collection.mutable
import scala.language.implicitConversions

import chisel3._
import chisel3.iotesters.{PeekPokeTests}

trait ArmflexBaseFlatSpec
{
  // Base ProcConfig test
  implicit val procCfg = new ProcConfig(2, true)
}

trait ArmflexBasePeekPokeTests extends PeekPokeTests
{
  implicit def bigint2boolean(b:BigInt):Boolean = if(b != 0) true else false
  def int(x: Int): BigInt = { BigInt(x) }
  def int(x: Long): BigInt = { BigInt(x) }

  // These peek's are missingin PeekPokeTests trait,
  // but are present in PeekPokeTester
  def peek(signal: Aggregate): Seq[BigInt]
  def peek(signal: Bundle): mutable.LinkedHashMap[String, BigInt]
}
