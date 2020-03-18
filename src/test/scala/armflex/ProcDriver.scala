package armflex

import scala.language.implicitConversions

import org.scalatest._
import chisel3._
import chiseltest._

import arm.PROCESSOR_TYPES.{DATA_SZ, REG_N}

import armflex.TPU2STATE._

import util._
import util.AxiLite.AxiLiteSlaveDriver
import util.BRAMPortDriver.BRAMPortDriver
import util.SoftwareStructs._

object ProcDriver {
  val ExcpUnalignedBr   = BigInt(1 << 0)
  val ExcpUnalignedSP   = BigInt(1 << 1)
  val ExcpUnalignedData = BigInt(1 << 2)
  implicit class ProcAxiDriver(target: ProcAxiWrap)(implicit val cfgProc: ProcConfig) {
    implicit val clock : Clock = target.clock

    val WORD_SIZE = 8

    val portMem: BRAMPort = target.io.memoryBRAM
    val portPState : BRAMPort = target.io.stateBRAM
    val procStateDBG_ : Option[ProcStateDBG] = target.io.procStateDBG
    val axiLite : AxiLiteSignals = target.io.axiLite

    def readData32Bit(addr: BigInt) : BigInt = { axiLite.rd32B(addr << 2) }

    def fireThread(tag: Int): Unit = {
      val data = (1.toLong << 31) | tag
      axiLite.wr32B(0, data)
    }

    def tpuIsWorking(): Boolean = {
      procStateDBG_.get.tuWorking.valid.peek.litToBoolean
    }

    def getDone(): (Boolean, Int) = {
      val addr = 1
      val doneVec = axiLite.rd32B((addr*4)).toInt
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

      val pstate = new PState(xregs.toList: List[BigInt], pc: BigInt, sp: BigInt, nzcv.toInt: Int)
      //println(pstate.toString())
      pstate
    }


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

    def isException: Boolean = procStateDBG_.get.exception.valid.peek.litToBoolean
    def getException: BigInt = procStateDBG_.get.exception.bits.peek.litValue

    def isMissTLB: Boolean = procStateDBG_.get.missTLB.valid.peek.litToBoolean
    def getMissTLB: (Int, BigInt, BigInt) = (
      procStateDBG_.get.missTLB.tag.peek.litValue.toInt,
      procStateDBG_.get.missTLB.bits.get.vaddr.peek.litValue,
      procStateDBG_.get.missTLB.bits.get.tlbIdx.peek.litValue
    )

    def writeFillTLB(vaddr: BigInt, isWr: Boolean, tlbIdx: BigInt) : Unit = {
      procStateDBG_.get.fillTLB.bits.tlbEntry.tag.poke(TLBEntry.getTLBtag(vaddr.U))
      procStateDBG_.get.fillTLB.bits.tlbEntry.wrEn.poke(isWr.B)
      procStateDBG_.get.fillTLB.bits.tlbEntry.valid.poke(true.B)
      procStateDBG_.get.fillTLB.bits.vaddr.poke(vaddr.U)
      procStateDBG_.get.fillTLB.bits.tlbIdx.poke(tlbIdx.U)
      procStateDBG_.get.fillTLB.valid.poke(true.B)
      clock.step(1)
      procStateDBG_.get.fillTLB.valid.poke(false.B)
    }
  }
}
