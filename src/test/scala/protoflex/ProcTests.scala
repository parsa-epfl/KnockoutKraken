package protoflex

import java.io.{FileInputStream, FileOutputStream, IOException}
import java.nio.ByteBuffer
import java.math.BigInteger

import scala.language.implicitConversions

import org.scalatest._
import chisel3._
import chiseltest._

import utils.{AssemblyParser, PrintingTools}
import utils.SoftwareStructs._

import common.PROCESSOR_TYPES.{DATA_SZ, REG_N}
import common._
import common.AxiLite._
import common.BRAMPort.BRAMPortDriver

object ProcDriver {
  implicit class ProcAxiDriver(target: ProcAxiWrap)(implicit val cfgProc: ProcConfig) {
    implicit def bool2boolean(x: Bool): Boolean = x.litToBoolean

    implicit val clock : Clock = target.clock

    val INSN_SIZE = 4

    val portPPage : BRAMPort = target.io.ppageBRAM
    val portPState : BRAMPort = target.io.stateBRAM
    val procStateDBG_ : Option[ProcStateDBG] = target.io.procStateDBG
    val axiLite : AxiLiteSignals = target.io.axiLite

    def readData32Bit(addr: BigInt) : BigInt = { axiLite.rd32B(addr << 2) }

    def fireThread(tag: Int): Unit = {
      val data = (1.toLong << 31) | tag
      axiLite.wr32B(0.U, data.U)
    }

    def tpuIsWorking(): Boolean = {
      procStateDBG_.get.tuWorking.valid.peek.litToBoolean
    }

    def getDone(): (Boolean, Int) = {
      val addr = 1
      val doneVec = axiLite.rd32B((addr*4).U)
      if(doneVec != 0) {
        return (true, doneVec)
      } else {
        return (false, doneVec)
      }
    }

    def writePPageInst(inst: BigInt, offst: BigInt) = { portPPage.wrBRAM32b(inst, offst) }

    def wrPSTATE2BRAM(tag: Int, pstate: PState): Unit ={
      //println(pstate.toString())
      var offst = 0
      for(i <- 0 until 32 ) {
        portPState.wrBRAM64b(pstate.xregs(i), offst); offst += 2
      }
      portPState.wrBRAM64b(pstate.pc, offst: Int); offst+=2
      portPState.wrBRAM64b(pstate.sp, offst: Int); offst+=2
      portPState.wrBRAM32b(pstate.nzcv, offst); offst+=1
    }

    def rdBRAM2PSTATE(tag: Int): PState = {
      var offst = 0

      val xregs = for(i <- 0 until 32 ) yield  {
        val reg = portPState.rdBRAM64b(offst); offst+=2
        reg
      }

      val pc = portPState.rdBRAM64b(offst: Int); offst+=2
      val sp = portPState.rdBRAM64b(offst: Int); offst+=2
      val nzcv = portPState.rdBRAM32b(offst); offst+=1

      val pstate = new PState(xregs.toList: List[BigInt], pc: BigInt, sp: BigInt, nzcv: Int)
      //println(pstate.toString())
      pstate
    }


    // DEBUG Functions ------------------------------------------------------------
    def getPStateInternal(cpu: Int): PState = {
      if(!cfgProc.DebugSignals) {
        println("ProportPStateDBG signals are not available: Enable DebugSignals in ProcConfig")
        return PState(List(404), 404, 404, 404)
      }
      val procStateDBG = procStateDBG_.get
      val pstate = procStateDBG.pregsVec(cpu)
      val rfile = procStateDBG.rfileVec(cpu)

      val xregs = for(reg <- 0 until 32) yield rfile(reg).peek.litValue
      val pc = pstate.PC.peek.litValue
      val sp = pstate.SP.peek.litValue
      val nzcv = pstate.NZCV.peek.litValue.toInt
      new PState(xregs.toList: List[BigInt], pc: BigInt, sp:BigInt, nzcv: Int)
    }

    def hasCommitedInst(): Boolean = {
      procStateDBG_.get.commitReg.valid.peek.litToBoolean &&
        !procStateDBG_.get.commitReg.bits.undef.peek.litToBoolean
    }

    def getCommitedInst(): BigInt = procStateDBG_.get.commitReg.bits.inst32.peek.litValue
    def getCommitedPC(): BigInt = procStateDBG_.get.commitReg.bits.pc.peek.litValue

    private def getMemInst = procStateDBG_.get.commitReg.bits.mem.bits
    private def getMemReq(idx: Int) = procStateDBG_.get.commitReg.bits.mem.bits.memReq(idx)
    def isCommitedMem: Boolean = procStateDBG_.get.commitReg.bits.mem.valid.peek.litToBoolean
    def isCommitedPairMem: Boolean = getMemInst.isPair.peek.litToBoolean
    def isCommitedLoad: Boolean = getMemInst.isLoad.peek.litToBoolean
    def getCommitedMemAddr(idx: Int): BigInt = getMemReq(idx).addr.peek.litValue
    def writeLD(idx: Int, data: BigInt): Unit = procStateDBG_.get.memResp(idx).poke(data.U)


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
}

// base class contain utils for proc testing
/*
trait ProcMainTestsBase extends ProcTestsBase {
  val cProc: protoflex.Proc

  val random = scala.util.Random

  def fireThread(tag: Int) = {
      cProc.io.host2tpu.fire.valid.poke(true.B)
      cProc.io.host2tpu.fire.tag.poke(tag.U)
      cProc.clock.step(1)
      cProc.io.host2tpu.fire.valid.poke(false.B)
  }
  def procIsDone() : (Boolean, Int) = {
      (cProc.io.host2tpu.done.valid.peek.litToBoolean,
       cProc.io.host2tpu.done.tag.peek.litValue.toInt)
  }
}
 */
