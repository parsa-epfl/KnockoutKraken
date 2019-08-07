package protoflex

import scala.collection.mutable
import scala.language.implicitConversions

import java.io.{FileInputStream, FileOutputStream, IOException}
import java.nio.ByteBuffer

import java.math.BigInteger

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester, PeekPokeTests}
import utils.AssemblyParser
import utils.SoftwareStructs

import common.PROCESSOR_TYPES.{DATA_SZ, REG_N}
import utils.PrintingTools


// base class contain utils for proc testing
trait ProcTestsBase extends PeekPokeTests {
  implicit def bigint2boolean(b:BigInt):Boolean = if(b != 0) true else false
  def int(x: Int): BigInt = { BigInt(x) }
  def int(x: Long): BigInt = { BigInt(x) }

  // These peek's are missingin PeekPokeTests trait,
  // but are present in PeekPokeTester
  def peek(signal: Aggregate): Seq[BigInt]
  def peek(signal: Bundle): mutable.LinkedHashMap[String, BigInt]

  val c: Proc
  val random = scala.util.Random

  def printState():Unit = {
    if(!c.cfg.DebugSignals) {
      println("ProcStateDBG signals are not available")
      return
    }
    val procStateDBG = c.io.procStateDBG.get
    val sFet = SoftwareStructs.dinst(peek(procStateDBG.inst_in))
    val sDec = if(peek(procStateDBG.decReg.valid))   SoftwareStructs.dinst(peek(procStateDBG.decReg.bits)) else "XXX"
    val sIss = if(peek(procStateDBG.issueReg.valid)) SoftwareStructs.dinst(peek(procStateDBG.issueReg.bits)) else "XXX"
    val sExe = if(peek(procStateDBG.exeReg.valid))   SoftwareStructs.einst(peek(procStateDBG.exeReg.bits)) else "XXX"
    val sBr  = if(peek(procStateDBG.brReg.valid))    SoftwareStructs.binst(peek(procStateDBG.brReg.bits)) else "XXX"
    val state = Seq(
      "+----------------------------------------------------------------------------------+",
      "|                                   STATE                                          |",
      "+----------------------------------------------------------------------------------+",
      "-------------------------------- DECODE STAGE --------------------------------------",
      "Inst in : \n" + sFet,
      "-------------------------------- DECODE STAGE --------------------------------------",
      "Reg_Dec : \n" + sDec,
      "-------------------------------- ISSUE STAGE ---------------------------------------",
      "Reg_Iss : \n" + sIss,
      "------------------------------- EXECUTE STAGE --------------------------------------",
      "                    |      ",
      "                    |      ",
      "             +------------+",
      "             |            |",
      "  +---------Reg_Exe :     |",
      "  |                        \n" + sExe,
      "             |            |",
      "             |            |",
      "             |            |",
      "  +----------------------Reg_Br :",
      "  |                        \n" + sBr,
      "             |            |",
      "             |            |",
      "             |            |",
      "             |------------|",
      "                    |      ",
      "                    |      ",
      "------------------------------- WB STAGE -------------------------------------------",
      "                                                                               ",
      " PC : " + peek(c.io.curr_PC) + " -> " + peek(c.io.next_PC),
      "+----------------------------------------------------------------------------------+",
      "|                                   DONE                                           |",
      "+-----------------------------------------------------------------------------------\n",
    ).mkString("\n")
    print(state)
  }

  def printCycle(cycle: Int) = {
    val cycle_str = Seq(
      "+----------------------------------------------------------------------------------+",
      "|                                Cycle : "+cycle.toString.padTo(2,' ')+ "                                        |",
      "+-----------------------------------------------------------------------------------\n").mkString("\n")
    print(cycle_str)
  }

  // helper functions

  def start_rtl() = {
    poke(c.io.tp_start, 1)
    step(1)
    poke(c.io.tp_start, 0)
  }

  def write_ppage(inst: Int, offst: Int) = {
    poke(c.io.ppage_bram.en, 1)
    poke(c.io.ppage_bram.writeEn.get, 1)
    poke(c.io.ppage_bram.addr, offst)
    poke(c.io.ppage_bram.dataIn.get, inst)
    step(1)
    poke(c.io.ppage_bram.en, 0)
    poke(c.io.ppage_bram.writeEn.get, 0)
  }

  def writeFullPPage(ppage: Array[Int], page_size: Int) = {
    for(i <- 0 to page_size) {
      write_ppage(ppage(i), i)
    }
  }

  def write32b_pstate(word: Int, offst: Int) = {
    poke(c.io.state_bram.en, 1)
    poke(c.io.state_bram.writeEn.get, 1)
    poke(c.io.state_bram.addr, offst)
    poke(c.io.state_bram.dataIn.get, word)
    step(1)
    poke(c.io.state_bram.en, 0)
    poke(c.io.state_bram.writeEn.get, 0)
  }

  // NOTE: Writes 2 words
  def write64b_pstate(lw: Long, offst: Int) = {
    val bytes = Array.fill(8)(0.toByte)
    for( i <- 0 to 7 ) bytes(i) = ((lw >> ((7-i) * 8)) & 0xFF).toByte
    val msb = ByteBuffer.wrap(bytes.slice(0, 4)).getInt
    val lsb = ByteBuffer.wrap(bytes.slice(4, 8)).getInt
    write32b_pstate(msb, offst)
    write32b_pstate(lsb, offst+1)
    println(lw.toHexString + "=" + msb.toHexString + "|" + lsb.toHexString)
  }

  def write_pstate(tag: Int, pstate: SoftwareStructs.PState): Unit ={
    var offst = 0
    for(i <- 0 until 32 ) {
      write64b_pstate(pstate.xregs(i), offst); offst += 2
    }
    write64b_pstate(pstate.pc, offst: Int); offst+=2
    write32b_pstate(0, offst); offst+=1
    write32b_pstate(0, offst); offst+=1
    write32b_pstate(pstate.nzcv, offst); offst+=1
  }

  def read32b_pstate(offst:Int):Int = {
    poke(c.io.state_bram.en, 1)
    poke(c.io.state_bram.writeEn.get, 0)
    poke(c.io.state_bram.addr, offst)
    step(1)
    poke(c.io.state_bram.en, 0)
    poke(c.io.state_bram.writeEn.get, 0)
    val uint32 = peek(c.io.state_bram.dataOut.get)
    return uint32.toInt
  }

  def read64b_pstate(offst:Int):Long = {
    val msb = read32b_pstate(offst)
    val lsb = read32b_pstate(offst+1)
    val byte_msb = Array.fill(4)(0.toByte)
    val byte_lsb = Array.fill(4)(0.toByte)
    for (i <- 0 to 3) byte_msb(i) = ((msb >> ((3-i) * 8)) & 0xFF).toByte
    for (i <- 0 to 3) byte_lsb(i) = ((lsb >> ((3-i) * 8)) & 0xFF).toByte
    ByteBuffer.wrap((byte_msb ++ byte_lsb)).getLong
  }

  def read_pstate(tag: Int): SoftwareStructs.PState ={
    var offst = 0
    val xregs = for(i <- 0 until 32 ) yield  {
      val reg = read64b_pstate(offst)
      offst += 2
      reg
    }

    val pc = read64b_pstate(offst: Int); offst+=2
    val el = read32b_pstate(offst); offst+=1
    val sp = read32b_pstate(offst); offst+=1
    val nzcv = read32b_pstate(offst); offst+=1

    new SoftwareStructs.PState(xregs.toList: List[Long], pc: Long, nzcv: Int)
  }
}

// test basic pipline TODO fix deprecated
/*
class ProcTestsPipeline(c_ : Proc) extends PeekPokeTester(c_) with ProcTestsBase
{
  override val c = c_

  val insts = AssemblyParser.parse("alu.x")
  val br_insts = AssemblyParser.parse("branch.x")
  poke(c.io.brReg.ready, 1)
  poke(c.io.exeReg.ready, 1)
  poke(c.io.valid, 0)
  poke(c.io.mem_res.valid, 0)
  poke(c.io.mem_res.bits.data, 0)
  step(1)
  for(i <- 0 until 3) {
    val itype = random.nextInt(2)
    val inst = if (itype == 0) insts(random.nextInt(insts.size)) else br_insts(random.nextInt(br_insts.size))
    poke(c.io.inst, inst.bitPat)
    poke(c.io.tag, random.nextInt(4))
    poke(c.io.valid, 1)
    printCycle(i)
    printState()
    step(1)
  }

}

class ProcTester extends ChiselFlatSpec with ArmflexBaseFlatSpec
{
  behavior of "Proc"

  backends foreach { backend =>
    "ProcTestsReadyValid" should s"test Proc pipeline (with $backend)" in {
      Driver(() => new Proc()(new ProcConfig(0)), backend)((c) => new ProcTestsPipeline(c)) should be (true)
    }
  }
}
 */
