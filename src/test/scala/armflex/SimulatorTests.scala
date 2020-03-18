package armflex

import java.io._
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import java.nio.ByteBuffer

import chisel3._
import chisel3.experimental._

import org.scalatest._
import chiseltest._
import chiseltest.internal._
import chiseltest.experimental.TestOptionBuilder._

import firrtl.options.TargetDirAnnotation

import arm.PROCESSOR_TYPES.REG_N

import util._
import util.ArmflexJson
import util.SoftwareStructs._

import arm._

import armflex.ProcDriver._
import java.io.BufferedWriter
import java.io.FileWriter

object FA_QflexCmds {
  // Commands SIM->QEMU
  val DATA_LOAD   = 0
  val DATA_STORE  = 1
  val INST_FETCH  = 2
  val INST_UNDEF  = 3
  val SIM_EXCP    = 4
  // Commands QEMU->SIM
  val SIM_START  = 5 // Load state from QEMU
  val SIM_STOP   = 6
  // Commands QEMU<->SIM
  val LOCK_WAIT   = 7
  val CHECK_N_STEP = 8

  def cmd_toString(cmd: Int): String = {
    val str = cmd match {
      case DATA_LOAD    => "DATA_LOAD"
      case DATA_STORE   => "DATA_STORE"
      case INST_FETCH   => "INST_FETCH"
      case INST_UNDEF   => "INST_UNDEF"
      case SIM_EXCP     => "SIM_EXCP"
      case SIM_START    => "SIM_START"
      case SIM_STOP     => "SIM_STOP"
      case LOCK_WAIT    => "LOCK_WAIT"
      case CHECK_N_STEP => "CHECK_N_STEP"
      case _ => "UNKNOWN"
    }
    str.padTo(10, ' ')
  }
}

/* sim : Chisel3 simulation
 * qemu: QEMU    simulation
 */
class SimulatorConfig (
  simStateFilename: String,
  simLockFilename: String,
  simCmdFilename : String,

  qemuStateFilename: String,
  qemuLockFilename: String,
  qemuCmdFilename: String,

  pageFilename:String,
  val pageSizeBytes:Int,
  val rootPath : String = "/dev/shm/qflex",
) {
  val simStatePath  = rootPath + "/" + simStateFilename
  val simLockPath   = rootPath + "/" + simLockFilename
  val simCmdPath    = rootPath + "/" + simCmdFilename
  val qemuStatePath = rootPath + "/" + qemuStateFilename
  val qemuLockPath  = rootPath + "/" + qemuLockFilename
  val qemuCmdPath   = rootPath + "/" + qemuCmdFilename
  val pagePath      = rootPath + "/" + pageFilename
}

class SimulatorTestsBaseDriver(val cProcAxi : ProcAxiWrap, val cfgSim : SimulatorConfig)
  (implicit val cfgProc : ProcConfig) {

  val file = new PrintWriter(new BufferedWriter(new FileWriter("/dev/shm/outputSim", true)), true)

  val WORD_SIZE = cProcAxi.WORD_SIZE

  val axiLite: AxiLiteSignals =  cProcAxi.io.axiLite

  implicit val clock = cProcAxi.clock

  val portMemory: BRAMPort = cProcAxi.io.memoryBRAM
  val portPState: BRAMPort = cProcAxi.io.stateBRAM
  val procStateDBG_ : Option[ProcStateDBG] = cProcAxi.io.procStateDBG

  var currState : PState = cProcAxi.getPStateInternal(0)

  def pageSize = cfgSim.pageSizeBytes/WORD_SIZE

  def exit: Unit = {
    file.close()
    System.exit(1)
  }

  def simLog(str: String) { file.println("RTL:" + str) }

  def getProgramPageInsns(path: String): Seq[(Int, BigInt)] = {
    val bytearray: Array[Byte] = Files.readAllBytes(Paths.get(path))
    var hex = ""
    val insns : Seq[(Int, BigInt)] = for(i <- 0 until pageSize) yield {
      val insn_LE = bytearray.slice(i*WORD_SIZE, (i+1)*WORD_SIZE)
      //val (insn_0,insn_1) = insn_LE.splitAt(4)
      val insn = BigInt(Array(0.toByte) ++ insn_LE ) //++ insn_1 ++ insn_0)
      (i, insn)
    }
    insns
  }

  def readFile(path: String): String= {
    val source = scala.io.Source.fromFile(path)
    val lines = try source.mkString finally source.close()
    lines
  }

  def writeFile(path: String, text: String): Unit = {
    Files.write(Paths.get(path), text.getBytes(StandardCharsets.UTF_8))
  }

  def writePState2File(path: String, pstate: PState): Unit = {
    val json = ArmflexJson.state2json(pstate)
    writeFile(path, json)
  }

  def updatePState(json: String, tag : Int): Unit = {
    val pstate: PState = ArmflexJson.json2state(json)
    cProcAxi.wrPSTATE2BRAM(tag, pstate)
  }

  def waitForCmd(filepath:String): (Int, BigInt) = {
    var cmd: (Int, BigInt) = (FA_QflexCmds.LOCK_WAIT, 0)
    do {
      Thread.sleep(500)
      cmd = ArmflexJson.json2cmd(readFile(filepath))
    } while(cmd._1 == FA_QflexCmds.LOCK_WAIT);
    //simLog(s"CMD is ${FA_QflexCmds.cmd_toString(cmd)} in ${filepath}")
    writeCmd((FA_QflexCmds.LOCK_WAIT, 0), filepath) // Consume Command
    cmd
  }

  def writeCmd(cmd: (Int, BigInt), path: String) = {
    val json = ArmflexJson.cmd2json(cmd._1, cmd._2)
    writeFile(path, json)
  }

  def run(pstate: PState) = {
    if(!cfgProc.DebugSignals) {
      simLog("DebugSignals not enabled, enable them to run with verification; EXIT")
      exit
    }
    Context().backend.setTimeout(clock, 10000)

    //simLog(s"START PC:" + "%016x".format(pstate.pc))
    cProcAxi.fireThread(0)

    // Wait for state to be transplanted
    do { clock.step(1) } while(!cProcAxi.tpuIsWorking);
    do { clock.step(1) } while(cProcAxi.tpuIsWorking);
    // RTL state after transplant must match initial QEMU state
    currState = cProcAxi.getPStateInternal(0)
    val matched = currState.matches(pstate)
    if(!matched._1) {
      clock.step(13)
      Context().backend.finish()
      assert(matched._1)
    }

    fork {
      do {
        if(cProcAxi.hasCommitedInst) {
          val inst = cProcAxi.getCommitedInst
          val pc = cProcAxi.getCommitedPC
          // If defined, compare state with QEMU
          if(cProcAxi.isCommitedUndef) {
            simLog(s"OUT:0x${"%016x".format(pc)}:  ${"%08x".format(inst)}      UNDEF")
            clock.step(1)
          } else if(cProcAxi.isException) {
            val excpType = cProcAxi.getException match {
              case ExcpUnalignedBr   => "Branch"
              case ExcpUnalignedSP   => "SP misaligned"
              case ExcpUnalignedData => "Data misaligned"
            }
            simLog(s"OUT:0x${"%016x".format(pc)}:  ${"%08x".format(inst)}      EXCP ${excpType}")
            clock.step(1)
          } else {

            clock.step(1) // Next clock cycle state is updated
            currState = cProcAxi.getPStateInternal(0)

            // Ask QEMU to step and write state back to compare
            writePState2File(cfgSim.simStatePath, currState)
            simLog(s"OUT:0x${"%016x".format(pc)}:  ${"%08x".format(inst)}") // Executed instruction
            writeCmd((FA_QflexCmds.CHECK_N_STEP, 0), cfgSim.qemuCmdPath)
            waitForCmd(cfgSim.simCmdPath)

            val qemuPState = ArmflexJson.json2state(readFile(cfgSim.qemuStatePath))
            val (hasMatched, diffLog) = currState.matches(qemuPState)
            if (!hasMatched) simLog(diffLog)
          }
        } else {
          clock.step(1)
        }
      } while(!cProcAxi.tpuIsWorking);
      writePState2File(cfgSim.simStatePath, cProcAxi.rdBRAM2PSTATE(0))
      writeCmd((FA_QflexCmds.INST_UNDEF, 0), cfgSim.qemuCmdPath)
    }.fork {
      val bramMask = cfgProc.TLB_NB_ENTRY - 1 //for (i <- 0 until cfgProc.TLB_NB_ENTRY_W) yield '1'
      do {
        if(cProcAxi.isMissTLB) {
          val (tag, addr, tlbIdx): (Int, BigInt, BigInt) = cProcAxi.getMissTLB
          writeCmd((tag, addr), cfgSim.qemuCmdPath)
          val cmd = waitForCmd(cfgSim.simCmdPath)
          //val bram = ((addr & (bramMask << 12)) >> 12) // mask bits after page to target TLB entry
          val bram = tlbIdx
          val insns: Seq[(Int, BigInt)] = getProgramPageInsns(cfgSim.pagePath)
          simLog(s"${"%016x".format(addr)}:BRAM:${bram}:MISSED:${FA_QflexCmds.cmd_toString(tag)}:${tag}")
          for(insn <- insns) {
            cProcAxi.writeMem(insn._2, insn._1, bram)
          }
          cProcAxi.writeFillTLB(addr, cmd._2 == FA_QflexCmds.DATA_STORE, bram)
        } else {
          clock.step(1)
        }
      } while(!cProcAxi.tpuIsWorking);
    }.join()

    do {  } while(cProcAxi.getDone._1)

    clock.step(1)
  }

  def runSimulator: Unit = {
    simLog("RUN SIMULATOR")
    while(true) {
      val cmd = waitForCmd(cfgSim.simCmdPath)._1
      cmd match {
        case FA_QflexCmds.SIM_START =>
          updatePState(readFile(cfgSim.qemuStatePath), 0)
          val pstate: PState = ArmflexJson.json2state(readFile(cfgSim.qemuStatePath))
          run(pstate)
        case FA_QflexCmds.SIM_STOP =>
          simLog("SIMULATION STOP")
          return
      }
    }
  }
}

// sim : Chisel3 simulation
// qemu: QEMU    simulation
class TestSimulatorAxi(cfgSim : SimulatorConfig, val cfgProc : ProcConfig)
    extends FlatSpec with ChiselScalatestTester {

  val annos = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/Axi"), WriteVcdAnnotation)

  behavior of "Armflex Simulator AXI interface"

  it should "Communicate between QEMU and RTL for verification" in {

    test(new ProcAxiWrap()(cfgProc))
      .withAnnotations(annos) { proc =>

      val drv = new SimulatorTestsBaseDriver(proc, cfgSim)(cfgProc)

      drv.runSimulator
    }
  }
}

object SimulatorMain extends App {
  assert(args.length == 9)
  val cfgSim = new SimulatorConfig(
    args(0), args(1), args(2),
    args(3), args(4), args(5),
    args(6), args(7).toInt,
    args(8)
  )
  implicit val cfgProc = new ProcConfig(2, true)
  new TestSimulatorAxi(cfgSim: SimulatorConfig, cfgProc).execute()
}
