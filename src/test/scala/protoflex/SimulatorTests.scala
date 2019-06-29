package protoflex

import java.io.{BufferedInputStream, FileInputStream, FileOutputStream, IOException}
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import java.nio.ByteBuffer

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import common.PROCESSOR_TYPES.REG_N

import utils.ArmflexJson
import utils.SoftwareStructs._

object FA_QflexCmds {
  // Commands SIM->QEMU
  val DATA_LOAD   = 0
  val DATA_STORE  = 1
  val INST_FETCH  = 2
  val INST_UNDEF  = 3
  val SIM_EXCP   = 4
  // Commands QEMU->SIM
  val SIM_START  = 5 // Load state from QEMU
  val SIM_STOP   = 6
  // Commands QEMU<->SIM
  val LOCK_WAIT   = 7
}

/* sim : Chisel3 simulation
 * qemu: QEMU    simulation
 */
case class SimulatorConfig(
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

class SimulatorTests(c_ : Proc, cfg: SimulatorConfig) extends PeekPokeTester(c_) with ProcTestsBase {
  val INSN_SIZE = 4
  val pageSize = cfg.pageSizeBytes/INSN_SIZE
  override val c = c_

  val program_page = Array.ofDim[Int](pageSize)

  def readProgramPageFile(path: String) ={
    val f = new BufferedInputStream(new FileInputStream(path)) // page file in little endian
    for(i <- 0 until pageSize){
      val bytes = Array.ofDim[Byte](INSN_SIZE)
      f.read(bytes,0,INSN_SIZE)
      program_page(i) = ByteBuffer.wrap(bytes.reverse).getInt
    }
    f.close()
  }

  def updateProgramPage(path: String) = {
    val bytearray: Array[Byte] = Files.readAllBytes(Paths.get(path))
    var hex = ""
    for(i <- 0 until pageSize) {
      val insn_LE = bytearray.slice(i*INSN_SIZE, i*INSN_SIZE+INSN_SIZE)
      program_page(i) = ByteBuffer.wrap(insn_LE.reverse).getInt
      hex += ByteBuffer.wrap(insn_LE).getInt.toHexString + "\n"
    }
    writeFile(path+"_dis", hex) // Disassemble for debug
  }

  def readFile(path: String):String= {
    val source = scala.io.Source.fromFile(path)
    val lines = try source.mkString finally source.close()
    lines
  }

  def writeFile(path: String, text: String): Unit = {
    Files.write(Paths.get(path), text.getBytes(StandardCharsets.UTF_8))
  }

  def getPState(tag: Int): PState= {
    poke(c.io.tp_tag, tag)
    poke(c.io.tp_en, 1)
    step(1)
    val pc = peek(c.io.tp_pstate_in.PC).toLong
    val sp = peek(c.io.tp_pstate_in.SP).toLong
    val el = peek(c.io.tp_pstate_in.EL).toInt
    val nzcv = peek(c.io.tp_pstate_in.NZCV).toInt
    poke(c.io.tp_en, 0)
    val xregs = for (r <- 0 until REG_N) yield read_reg(tag)(r).toLong
    val pstate = PState(xregs.toList, pc, nzcv)
    pstate
  }

  def writePState(path: String, tag: Int): Unit = {
    val json = ArmflexJson.state2json(getPState(tag))
    writeFile(path, json)
  }

  def updatePState(json: String, tag : Int):Unit = {
    val pstate: PState = ArmflexJson.json2state(json)
    poke(c.io.flush,0)
    for(r <- 0 until REG_N) {
      write_reg(tag)(r, pstate.xregs(r))
    }
    write_pstate(tag)(pstate.pc, 0, 0, pstate.nzcv)
  }

  def run(insts: Int, tag: Int): Unit = {
      for(i <- 0 until insts) {
        // Load inst
        val pc = peek(c.io.curr_PC).toLong
        val inst = program_page(pc % pageSize)
        do {
          poke(c.io.inst, inst)
          poke(c.io.tag, tag)
          poke(c.io.valid, 1)
          step(1)
        } while(peek(c.io.ready) == 0);
        poke(c.io.valid, 0)
        // Wait for next inst
      }
  }

  var tf = System.nanoTime
  var ti = System.nanoTime
  val timeoutms = 10000

  def timedOut = {
    val isTimeOut = (tf - ti) / 1e9d > timeoutms
    if(isTimeOut) {
      println("TIMED OUT")
      System.exit(1)
    }
    isTimeOut
  }

  def waitForCmd(filepath:String, timeoutms : Long): Int = {
    ti = System.nanoTime
    var cmd: Int = FA_QflexCmds.LOCK_WAIT
    do {
      Thread.sleep(500)
      cmd = ArmflexJson.json2cmd(readFile(filepath))._1
      tf = System.nanoTime
    } while(cmd == FA_QflexCmds.LOCK_WAIT && !timedOut);
    cmd
  }

  def runSimulator(timeoutms: Long):Unit = {
    var ti = System.nanoTime
    var tf = System.nanoTime
    while(!timedOut) {
      val cmd = waitForCmd(cfg.simCmdPath, timeoutms)
      ti = System.nanoTime
      cmd match {
        case FA_QflexCmds.SIM_START =>
          println("SIMULATION START")
          updatePState(readFile(cfg.qemuStatePath), 0)
          updateProgramPage(cfg.pagePath)
        case FA_QflexCmds.SIM_STOP =>
          println("SIMULATION STOP")
          return
      }
      tf = System.nanoTime
    }
  }

  // Simulation routine
  runSimulator(10000)
}

class SimulatorTester(cfg: SimulatorConfig) extends ChiselFlatSpec {
  override val backends = Array("verilator")
  println("Starting Simulator")
  // Extra usefull args : --is-verbose
  val chiselArgs = Array("-tn", "proc", "-td","./test/Sim", "--backend-name", "verilator")
  iotesters.Driver.execute(chiselArgs, () => new Proc()(new ProcConfig(0))) {
      c => new SimulatorTests(c, cfg)
  } should be(true)
  println("Done Simulator")
}

object SimulatorMain extends App {
  assert(args.length == 9)
  val cfg = new SimulatorConfig(
    args(0), args(1), args(2),
    args(3), args(4), args(5),
    args(6), args(7).toInt,
    args(8)
  )
  new SimulatorTester(cfg: SimulatorConfig)
}

