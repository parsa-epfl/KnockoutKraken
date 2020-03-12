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

import protoflex.TPU2STATE._

object ProcDriver {
  implicit class ProcAxiDriver(target: ProcAxiWrap)(implicit val cfgProc: ProcConfig) {
    implicit def bool2boolean(x: Bool): Boolean = x.litToBoolean

    implicit val clock : Clock = target.clock

    val WORD_SIZE = 8

    val portMem: BRAMPort = target.io.memoryBRAM
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

    def writeMem(word: BigInt, offst: BigInt, bram: BigInt) = {
      portMem.wrBRAM(word, offst + (bram << 9))
    }

    def wrPSTATE2BRAM(tag: Int, pstate: PState): Unit ={
      for(i <- ARCH_XREGS_OFFST until ARCH_XREGS_OFFST+32 ) {
        portPState.wrBRAM(pstate.xregs(i), i)
      }
      portPState.wrBRAM(pstate.pc, ARCH_PC_OFFST)
      portPState.wrBRAM(pstate.sp, ARCH_SP_OFFST)
      portPState.wrBRAM(pstate.nzcv, ARCH_PSTATE_OFFST)
    }

    def rdBRAM2PSTATE(tag: Int): PState = {
      val xregs = for(i <- ARCH_XREGS_OFFST until ARCH_XREGS_OFFST+32 ) yield {
        val reg = portPState.rdBRAM(i)
        reg
      }
      val pc = portPState.rdBRAM(ARCH_PC_OFFST)
      val sp = portPState.rdBRAM(ARCH_SP_OFFST)
      val nzcv = portPState.rdBRAM(ARCH_PSTATE_OFFST)

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

    def hasCommitedInst(): Boolean = procStateDBG_.get.commited.peek.litToBoolean
    def isCommitedUndef(): Boolean = procStateDBG_.get.commitReg.bits.undef.peek.litToBoolean

    def getCommitedInst(): BigInt = procStateDBG_.get.commitReg.bits.inst32.peek.litValue
    def getCommitedPC(): BigInt = procStateDBG_.get.commitReg.bits.pc.peek.litValue

    private def getMemInst = procStateDBG_.get.commitReg.bits.mem.bits
    private def getMemReq(idx: Int) = procStateDBG_.get.commitReg.bits.mem.bits.memReq(idx)
    def isCommitedMem: Boolean = procStateDBG_.get.commitReg.bits.mem.valid.peek.litToBoolean
    def isCommitedPairMem: Boolean = getMemInst.isPair.peek.litToBoolean
    def isCommitedLoad: Boolean = getMemInst.isLoad.peek.litToBoolean
    def getCommitedMemAddr(idx: Int): BigInt = getMemReq(idx).addr.peek.litValue

    def isMissTLB: Boolean = procStateDBG_.get.missTLB.valid.peek.litToBoolean
    def getMissTLBAddrNType: (BigInt, BigInt) = (procStateDBG_.get.missTLB.bits.get.peek.litValue, procStateDBG_.get.missTLB.tag.peek.litValue)

    def writeFillTLB(vaddr: BigInt, isWr: Boolean) : Unit = {
      procStateDBG_.get.fillTLB.bits.get.tag.poke(TLBEntry.getTLBtag(vaddr.U))
      procStateDBG_.get.fillTLB.bits.get.wrEn.poke(isWr.B)
      procStateDBG_.get.fillTLB.bits.get.valid.poke(true.B)
      procStateDBG_.get.fillTLB.tag.poke(vaddr.U)
      procStateDBG_.get.fillTLB.valid.poke(true.B)
      clock.step(1)
      procStateDBG_.get.fillTLB.valid.poke(false.B)
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
