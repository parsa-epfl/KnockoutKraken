package protoflex

import java.io.{BufferedInputStream, FileInputStream, FileOutputStream, IOException}
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import java.nio.ByteBuffer

import org.scalatest._

import chisel3._
import chisel3.tester._
import chisel3.tester.experimental.TestOptionBuilder._
import chisel3.tester.internal.VerilatorBackendAnnotation
import chisel3.experimental._

import common.PROCESSOR_TYPES.REG_N

import utils.ArmflexJson
import utils.SoftwareStructs._
import common.BRAMPortAXI
import common.AxiLiteSignals
import firrtl.options.TargetDirAnnotation

object FA_QflexCmds {
  // Commands SIM->QEMU
  val DATA_LOAD   = 0
  val DATA_STORE  = 1
  val INST_FETCH  = 2
  val INST_UNDEF  = 3
  val SIM_EXCP    = 4
  val CHECK_N_STEP = 8
  // Commands QEMU->SIM
  val SIM_START  = 5 // Load state from QEMU
  val SIM_STOP   = 6
  // Commands QEMU<->SIM
  val LOCK_WAIT   = 7

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

class SimulatorTestsBaseDriver(cProcAxi_ : ProcAxiWrap, val cfgSim : SimulatorConfig)
                              (implicit val cfgProc_ : ProcConfig)
    extends ProcAxiWrapTestsBase {

  val cfgProc: ProcConfig = cfgProc_

  val cProcAxi: ProcAxiWrap = cProcAxi_
  val axiLite: AxiLiteSignals =  cProcAxi.io.axiLite

  implicit val clock = cProcAxi.clock

  val portPPage: BRAMPortAXI = cProcAxi.io.ppageBRAM
  val portPState: BRAMPortAXI = cProcAxi.io.stateBRAM
  val procStateDBG_ : Option[ProcStateDBG] = cProcAxi.io.procStateDBG

  def pageSize = cfgSim.pageSizeBytes/INSN_SIZE

  var tf = System.nanoTime
  var ti = System.nanoTime
  var timeoutms = 100000

  def exit: Unit = {
    System.exit(1)
  }

  def simLog(str: String) { println("RTL:" + str) }

  def timedOut = {
    val isTimeOut = (tf - ti) / 1e6d > timeoutms
    //simLog("time_elapsed:" + (tf - ti) / 1e9d)
    if(isTimeOut) {
      simLog("TIMED OUT")
      exit
    }
    isTimeOut
  }

  def updateProgramPage(path: String): Unit = {
    val bytearray: Array[Byte] = Files.readAllBytes(Paths.get(path))
    var hex = ""
    for(i <- 0 until pageSize) {
      val insn_LE = bytearray.slice(i*INSN_SIZE, i*INSN_SIZE+INSN_SIZE)
      //val insn = ByteBuffer.wrap(insn_LE).getInt
      val insn = BigInt(Array(0.toByte) ++ insn_LE)
      writePPageInst(insn, i)
    }
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
    wrPSTATE2BRAM(tag, pstate)
  }

  def waitForCmd(filepath:String): Int = {
    ti = System.nanoTime
    var cmd: Int = FA_QflexCmds.LOCK_WAIT
    do {
      Thread.sleep(500)
      cmd = ArmflexJson.json2cmd(readFile(filepath))._1
      tf = System.nanoTime
    } while(cmd == FA_QflexCmds.LOCK_WAIT && !timedOut);
    simLog(s"IN  CMD : ${FA_QflexCmds.cmd_toString(cmd)}")
    writeCmd((FA_QflexCmds.LOCK_WAIT, 0), filepath) // Consume Command
    cmd
  }

  def writeCmd(cmd: (Int, Long), path: String) = {
    val json = ArmflexJson.cmd2json(cmd._1, cmd._2)
    simLog(s"OUT CMD ${FA_QflexCmds.cmd_toString(cmd._1)} in ${path}")
    writeFile(path, json)
  }

  def stepVerification() = {
    clock.step(1)
    if(!cfgProc.DebugSignals) {
         simLog("DebugSignals not enabled, enable them to run with verification; EXIT")
         exit
    }
    if(procStateDBG_.get.commitReg.valid.peek.litToBoolean &&
       !procStateDBG_.get.commitReg.bits.undef.peek.litToBoolean) {
      val rtlPState = getPStateInternal(0)

      writePState2File(cfgSim.simStatePath, rtlPState)
      writeCmd((FA_QflexCmds.CHECK_N_STEP, 0), cfgSim.qemuCmdPath)
      waitForCmd(cfgSim.simCmdPath)

      val qemuPState = ArmflexJson.json2state(readFile(cfgSim.qemuStatePath))
      simLog(s"Comparing States: QEMU(${"%016x".format(qemuPState.pc)})"
                + s" <-> RTL(${"%016x".format(rtlPState.pc)})")
      qemuPState.compare(rtlPState)
    }
  }

  def run() = {
    ti = System.nanoTime
    val pstate: PState = ArmflexJson.json2state(readFile(cfgSim.qemuStatePath))
    simLog(s"START PC:" + "%016x".format(pstate.pc))
    fireThread(0)
    do {
      //step(1)
      stepVerification()
      tf = System.nanoTime
    } while(!procIsDone._1 && !timedOut);
    clock.step(1)
    simLog(s"DONE  PC:" + "%016x".format(rdBRAM2PSTATE(0).pc))
  }

  def runSimulator(timeoutms_i: Int):Unit = {
    ti = System.nanoTime
    tf = System.nanoTime
    timeoutms = timeoutms_i
    while(!timedOut) {
      val cmd = waitForCmd(cfgSim.simCmdPath)
      ti = System.nanoTime
      cmd match {
        case FA_QflexCmds.SIM_START =>
          updateProgramPage(cfgSim.pagePath)
          updatePState(readFile(cfgSim.qemuStatePath), 0)
          run()
          writePState2File(cfgSim.simStatePath, rdBRAM2PSTATE(0))
          writeCmd((FA_QflexCmds.INST_UNDEF, 0), cfgSim.qemuCmdPath)
        case FA_QflexCmds.SIM_STOP =>
          simLog("SIMULATION STOP")
          return
      }
      tf = System.nanoTime
    }
  }
}

// sim : Chisel3 simulation
// qemu: QEMU    simulation
class TestSimulatorAxi(cfgSim : SimulatorConfig)(implicit val cfgProc_ : ProcConfig)
    extends FlatSpec with ChiselScalatestTester {

  val annos = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("./test/Sim/Axi"))

  behavior of "Armflex Simulator AXI interface"

  it should "Communicate between QEMU and RTL for verification" in {
    test(new ProcAxiWrap).withAnnotations(annos) { proc =>
      val drv = new SimulatorTestsBaseDriver(proc, cfgSim)
      drv.runSimulator(1000)
    }
  }
}

object SimulatorMain extends App {
  assert(args.length == 9)
  val cfg = new SimulatorConfig(
    args(0), args(1), args(2),
    args(3), args(4), args(5),
    args(6), args(7).toInt,
    args(8)
  )
  implicit val cfgProc = new ProcConfig(2, true)
  new TestSimulatorAxi(cfg: SimulatorConfig).execute()
}
