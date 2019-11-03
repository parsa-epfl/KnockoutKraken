package protoflex

import java.io.{FileInputStream, FileOutputStream, IOException}
import java.nio.ByteBuffer
import java.math.BigInteger

import scala.language.implicitConversions

import org.scalatest._

import chisel3._
import chisel3.tester._

import utils.{AssemblyParser, PrintingTools}
import utils.SoftwareStructs._

import common.PROCESSOR_TYPES.{DATA_SZ, REG_N}
import common._
import common.AxiLite._
import common.BRAMPort.BRAMPortDriver

trait ProcTestsBase {
  implicit val clock : Clock
  implicit def bool2boolean(x: Bool): Boolean = x.litToBoolean
  val cfgProc: ProcConfig

  val INSN_SIZE = 4

  val portPPage : BRAMPortAXI
  val portPState : BRAMPortAXI
  val procStateDBG_ : Option[ProcStateDBG]

  // Operations that depend if wrapped in AxiLite or Proc is exposed
  def fireThread(tag: Int): Unit
  def procIsDone(): (Boolean, Int)

  def writePPageInst(inst: BigInt, offst: BigInt) = { portPPage.wrBRAM32b(inst, offst) }

  def wrPSTATE2BRAM(tag: Int, pstate: PState): Unit ={
    //println(pstate.toString())
    var offst = 0
    for(i <- 0 until 32 ) {
       portPState.wrBRAM64b(pstate.xregs(i), offst); offst += 2
    }
    portPState.wrBRAM64b(pstate.pc, offst: Int); offst+=2
    // TODO Write SP, EL and NZCV as Cat(EL, SP, NZCV)
    portPState.wrBRAM32b(pstate.nzcv, offst); offst+=1
  }

  def rdBRAM2PSTATE(tag: Int): PState ={
    var offst = 0
    val xregs = for(i <- 0 until 32 ) yield  {
      val reg = portPState.rdBRAM64b(offst).toLong; offst+=2
      reg
    }

    val pc = portPState.rdBRAM64b(offst: Int); offst+=2
    val sp_el_nzcv = portPState.rdBRAM32b(offst); offst+=1

    val s_sp_el_nzcv = sp_el_nzcv.toInt.toBinaryString
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
    val pstate = procStateDBG.pregsVec(cpu)
    val rfile = procStateDBG.rfileVec(cpu)

    val xregs = for(reg <- 0 until 32) yield rfile(reg).peek.litValue.toLong
    val pc = pstate.PC.peek.litValue.toLong
    val nzcv = pstate.NZCV.peek.litValue.toInt
    new PState(xregs.toList: List[Long], pc: Long, nzcv: Int)
  }

  def printState():Unit = {
    if(!cfgProc.DebugSignals) {
      println("ProportPStateDBG signals are not available: Enable DebugSignals in ProcConfig")
      return
    }
    val procStateDBG = procStateDBG_.get
    val fetchReg = procStateDBG.fetchReg
    val decReg   = procStateDBG.decReg
    val issueReg = procStateDBG.issueReg
    val exeReg   = procStateDBG.commitReg.bits.exe
    val brReg    = procStateDBG.commitReg.bits.br
    val sFet = if(fetchReg.valid.peek) finst(procStateDBG.fetchReg.bits.peek) else "XXX"
    val sDec = if(decReg.valid.peek)   dinst(procStateDBG.decReg.bits.peek)   else "XXX"
    val sIss = if(issueReg.valid.peek) dinst(procStateDBG.issueReg.bits.peek) else "XXX"
    val sExe = if(exeReg.valid.peek) einst(exeReg.bits.peek) else "XXX"
    val sBr  = if(brReg.valid.peek)  binst(brReg.bits.peek)  else "XXX"
    val PC = procStateDBG.pregsVec(0).PC.peek
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
      " PC : " + PC,
      "+----------------------------------------------------------------------------------+",
      "|                                   DONE                                           |",
      "+-----------------------------------------------------------------------------------\n",
    ).mkString("\n")
    print(state)
  }

  def printCycle(cycle: Int) = {
    val cycle_str = Seq(
      "+----------------------------------------------------------------------------------+",
      "|                                Cycle : "+ cycle.toString.padTo(2,' ')
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
      cProc.io.host2tpu.fire.valid.poke(true.B)
      cProc.io.host2tpu.fire.tag.poke(tag.U)
      clock.step(1)
      cProc.io.host2tpu.fire.valid.poke(false.B)
  }
  def procIsDone() : (Boolean, Int) = {
      (cProc.io.host2tpu.done.valid.peek.litToBoolean,
       cProc.io.host2tpu.done.tag.peek.litValue.toInt)
  }
}

trait ProcAxiWrapTestsBase extends ProcTestsBase {

  val cProcAxi: ProcAxiWrap
  val axiLite : AxiLiteSignals

  def readData32Bit(addr: BigInt) : BigInt = { axiLite.rd32B(addr << 2) }

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
    val data = (1.toLong << 31) | tag
    axiLite.wr32B(0.U, data.U)
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
    val doneVec = axiLite.rd32B(1.U)
    if(doneVec != 0) {
      return (true, doneVec)
    } else {
      return (false, doneVec)
    }
  }
}
