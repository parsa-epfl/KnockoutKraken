package protoflex

import java.io.{FileInputStream, FileOutputStream, IOException}
import java.nio.ByteBuffer
import java.math.BigInteger

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester, PeekPokeTests}
import utils.{AssemblyParser, PrintingTools}
import utils.SoftwareStructs._

import common.PROCESSOR_TYPES.{DATA_SZ, REG_N}
import common.{HighLevelAxiLiteTestInterface, BRAMPortAXIHelper, BRAMPortAXI, AxiLiteSignals}

trait ProcTestsBase extends ArmflexBasePeekPokeTests with BRAMPortAXIHelper {
  val cfgProc: ProcConfig

  val INSN_SIZE = 4
  val portPPage : BRAMPortAXI
  val portPState : BRAMPortAXI
  val procStateDBG_ : Option[ProcStateDBG]

  def fireThread(tag: Int): Unit
  def procIsDone(): (Boolean, Int)

  def writeFullPPage(ppage: Array[Int], page_size: Int) = {
    for(i <- 0 to page_size) {
      wrBRAM32b(portPPage, ppage(i), i)
    }
  }

  def wrPSTATE2BRAM(tag: Int, pstate: PState): Unit ={
    println("WRITE PSTATE")
    //println(pstate.toString())
    var offst = 0
    for(i <- 0 until 32 ) {
      wrBRAM64b(portPState, pstate.xregs(i), offst); offst += 2
    }
    wrBRAM64b(portPState, pstate.pc, offst: Int); offst+=2
    // TODO Write SP, EL and NZCV as Cat(EL, SP, NZCV)
    wrBRAM32b(portPState, pstate.nzcv, offst); offst+=1
  }

  def rdBRAM2PSTATE(tag: Int): PState ={
    println("READ PSTATE")
    var offst = 0
    val xregs = for(i <- 0 until 32 ) yield  {
      val reg = rdBRAM64b(portPState, offst); offst+=2
      reg
    }

    val pc = rdBRAM64b(portPState, offst: Int); offst+=2
    val sp_el_nzcv = rdBRAM32b(portPState,offst); offst+=1

    val s_sp_el_nzcv = sp_el_nzcv.toBinaryString
    val sp = s_sp_el_nzcv.slice(5, 6)
    val el = s_sp_el_nzcv.slice(4, 5)
    val nzcv = Integer.parseInt(s_sp_el_nzcv.slice(0, 4), 2)
    val pstate = new PState(xregs.toList: List[Long], pc: Long, nzcv: Int)
    //println(pstate.toString())
    pstate
  }

  // DEBUG Functions ------------------------------------------------------------
  def getPStateInternal(cpu: Int): PState = {
    if(!cfgProc.DebugSignals) {
      println("ProportPStateDBG signals are not available: Enable DebugSignals in ProcConfig")
      return PState(List(404), 404, 404)
    }
    val procStateDBG = procStateDBG_.get
    val pstate = procStateDBG.vecPRegs(cpu)
    val rfile = procStateDBG.vecRFiles(cpu)
    val xregs = for(reg <- 0 until 32) yield peek(rfile(reg)).toLong
    val pc = peek(pstate.PC).toLong
    val nzcv = peek(pstate.NZCV).toInt
    new PState(xregs.toList: List[Long], pc: Long, nzcv: Int)
  }

  def printState():Unit = {
    if(!cfgProc.DebugSignals) {
      println("ProportPStateDBG signals are not available: Enable DebugSignals in ProcConfig")
      return
    }
    val procStateDBG = procStateDBG_.get
    val exeReg = procStateDBG.commitReg.bits.exe
    val brReg = procStateDBG.commitReg.bits.br
    val sFet = if(peek(procStateDBG.fetchReg.valid)) finst(peek(procStateDBG.fetchReg.bits)) else "XXX"
    val sDec = if(peek(procStateDBG.decReg.valid))   dinst(peek(procStateDBG.decReg.bits))   else "XXX"
    val sIss = if(peek(procStateDBG.issueReg.valid)) dinst(peek(procStateDBG.issueReg.bits)) else "XXX"
    val sExe = if(peek(exeReg.valid)) einst(peek(exeReg.bits)) else "XXX"
    val sBr  = if(peek(brReg.valid))  binst(peek(brReg.bits))  else "XXX"
    val state = Seq(
      "+----------------------------------------------------------------------------------+",
      "|                                   STATE                                          |",
      "+----------------------------------------------------------------------------------+",
      "-------------------------------- FETCH STAGE ---------------------------------------",
      "RegFetch: \n" + sFet,
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
      " PC : " + peek(procStateDBG.vecPRegs(0).PC),
      "+----------------------------------------------------------------------------------+",
      "|                                   DONE                                           |",
      "+-----------------------------------------------------------------------------------\n",
    ).mkString("\n")
    print(state)
  }

  def printCycle(cycle: Int) = {
    val cycle_str = Seq(
      "+----------------------------------------------------------------------------------+",
      "|                                Cycle : "+cycle.toString.padTo(2,' ')
        + "                                        |",
      "+-----------------------------------------------------------------------------------\n").mkString("\n")
    print(cycle_str)
  }
}

// base class contain utils for proc testing
trait ProcMainTestsBase extends ProcTestsBase {
  val cProc: Proc

  val random = scala.util.Random

  def fireThread(tag: Int) = {
    poke(cProc.io.host2tpu.fire, 1)
    poke(cProc.io.host2tpu.fireTag, tag)
    step(1)
    poke(cProc.io.host2tpu.fire, 0)
  }
  def procIsDone() : (Boolean, Int) = {
    (peek(cProc.io.host2tpu.done), peek(cProc.io.host2tpu.doneTag).toInt)
  }
}

trait ProcAxiWrapTestsBase extends ProcTestsBase with HighLevelAxiLiteTestInterface {

  val cProcAxi: ProcAxiWrap
  implicit val axiLite : AxiLiteSignals

  def readData32Bit(addr: BigInt) : BigInt = {
    setReadAddressAndGetData(addr << 2)._1
  }

  // Note: FireReg = 0; DoneReg = 1, see ProcAxi.scala
  /** Register 0 (Pulse-Only)
    * +----------------------------------------------------+
    * |                 to Transplant Cmds                 |
    * |-----------+--------------------+-------------------+
    * |   fire    |      RESERVED      |        Tag        |
    * +-----------+--------------------+-------------------+
    * |31       31|30        NB_THREADS|NB_THREADS-1      0|
    * +-----------+--------------------+-------------------+
    *
    */
  def fireThread(tag: Int): Unit = {
    val data = (1 << 31) | tag
    writeData32Bit(0: BigInt, data: BigInt)
  }

  /** Register 1 (Read-Only)
    * +----------------------------------------+
    * |          to Host Cmds                  |
    * +-----------------+----------------------+
    * |    RESERVED     |        done          |
    * +-----------------+----------------------+
    * |31      NB_THREAD|NB_THREADS-1         0|
    * +-----------------+----------+-----------+
    *
    */
  def procIsDone(): (Boolean, Int) = {
    val doneVec = readData32Bit(1).toInt
    if(doneVec != 0) {
      return (true, doneVec)
    } else {
      return (false, doneVec)
    }
  }
}
