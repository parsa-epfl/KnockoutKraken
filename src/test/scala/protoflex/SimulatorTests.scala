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

/* sim : Chisel3 simulation
 * qemu: QEMU    simulation
 */
case class QEMUConfig(
  simStateFilename: String,
  simLockFilename: String,
  qemuStateFilename: String,
  qemuLockFilename: String,
  qemuCmdFilename: String,
  pageFilename:String,
  val pageSize:Int,
  val rootPath : String = "/dev/shm/qflex",
) {
  val simStatePath  = rootPath + "/" + simStateFilename
  val simLockPath   = rootPath + "/" + simLockFilename
  val qemuStatePath = rootPath + "/" + qemuStateFilename
  val qemuLockPath  = rootPath + "/" + qemuLockFilename
  val qemuCmdPath   = rootPath + "/" + qemuCmdFilename
  val pagePath      = rootPath + "/" + pageFilename
}

class QEMUSimulator(c_ : Proc, config: QEMUConfig) extends PeekPokeTester(c_) with ProcTestsBase {
  val INSN_SIZE = 4
  override val c = c_

  val program_page = Array.ofDim[Int](config.pageSize/INSN_SIZE)

  def readProgramPageFile(path: String) ={
    val f = new BufferedInputStream(new FileInputStream(path)) // page file in little endian
    for(i <- 0 until config.pageSize/INSN_SIZE){
      val bytes = Array.ofDim[Byte](INSN_SIZE)
      f.read(bytes,0,INSN_SIZE)
      program_page(i) = ByteBuffer.wrap(bytes.reverse).getInt
    }
    f.close()
  }

  def updateProgramPage(path: String) = {
    val bytearray: Array[Byte] = Files.readAllBytes(Paths.get(path))
    var hex = ""
    for(i <- 0 until config.pageSize/INSN_SIZE) {
      val insn_LE = bytearray.slice(i*INSN_SIZE, i*INSN_SIZE+INSN_SIZE)
      program_page(i) = ByteBuffer.wrap(insn_LE.reverse).getInt
      hex += ByteBuffer.wrap(insn_LE).getInt.toHexString + "\n"
    }
    writeFile(path+"_dis", hex) // Disassemble for debug
  }

  def writeProgramPageDebug(path: String) = {
    for(i <- 0 until config.pageSize/INSN_SIZE) {
    }

  }

  def writeProgramPageFPGA() = {
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
    poke(c.io.tp_mode, 1)
    step(1)
    val pc = peek(c.io.tp_PC).toLong
    val sp = peek(c.io.tp_SP).toLong
    val el = peek(c.io.tp_EL).toInt
    val nzcv = peek(c.io.tp_NZCV).toInt
    poke(c.io.tp_mode, 0)
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
      val inst = program_page(pc % config.pageSize)
      println(f"instruction ${inst}%x")
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

}
