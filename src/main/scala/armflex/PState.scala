package armflex

import chisel3._

import chisel3.util.{Cat, Decoupled, Valid, Fill}
import arm.PROCESSOR_TYPES._
import chisel3.util.log2Ceil

import armflex.util._

/*
 * The 64-bit Execution state. This Execution state:
 * • Provides 31 64-bit general-purpose registers, of which X30 is used as the procedure link
 * register.
 * • Provides a 64-bit program counter (PC), stack pointers (SPs), and exception link registers
 * (ELRs).
 * • Provides 32 128-bit registers for SIMD vector and scalar floating-point support.
 * • Provides a single instruction set, A64. For more information, see The ARM instruction sets
 * on page A1-37.
 * • Defines the ARMv8 Exception model, with up to four Exception levels, EL0 - EL3, that
 * provide an execution privilege hierarchy, see Exception levels on page D1-1850.
 * • Provides support for 64-bit virtual addressing. For more information, including the limits on
 * address ranges, see Chapter D4 The AArch64 Virtual Memory System Architecture.
 * • Defines a number of Process state (PSTATE) elements that hold PE state. The A64
 * instruction set includes instructions that operate directly on various PSTATE elements.
 * • Names each System register using a suffix that indicates the lowest Exception level at which
 * the register can be accessed.
 */

class PStateRegs extends Bundle {
  val PC = DATA_T
  //val SP = DATA_T // Normaly 4 levels 32 bits
  // NOTE: QEMU uses Reg[31] as SP, so this is actually not used
  //val EL = INST_T // Normaly 4 levels 32 bits

  // PSTATE
  // Condition flags
  val NZCV = NZCV_T

  /* Execution state
   // Registers Select
   val CurrentEL = UInt(2.W) // Current Exception level
   val SPSel = UInt(2.W) // Stack pointer selection

   // Control bits
   val SS = Bool()    // Software step
   val IL = Bool()    // Illegal Execution
   val nRW = Bool()   // Current Execution
   // */
}

object PStateRegs {
  def apply(): PStateRegs = {
    val wire = Wire(new PStateRegs())
    wire.PC := DATA_X
    //wire.SP := DATA_X
    //wire.EL := DATA_X
    wire.NZCV := NZCV_X
    wire
  }
}
object RFileIO {
  class RDPort(nbThreads: Int) extends Bundle {
    val port = Vec(2, new Bundle {
        val addr = Input(REG_T)
        val data = Output(DATA_T)
      }
    )
    val tag = Input(UInt(log2Ceil(nbThreads).W))
    override def cloneType: this.type = new RDPort(nbThreads).asInstanceOf[this.type]
  }

  class WRPort(nbThreads: Int) extends Bundle {
    val addr = Input(REG_T)
    val data = Input(DATA_T)
    val en = Input(Bool())
    val tag = Input(UInt(log2Ceil(nbThreads).W))
    override def cloneType: this.type = new WRPort(nbThreads).asInstanceOf[this.type]
  }

  def wr2BRAM(port: BRAMPort, wr: WRPort) {
    port.ADDR := wr.addr
    port.DI := wr.data
    port.EN := wr.en.asUInt
    port.WE := Fill(port.cfg.NB_COL, wr.en.asUInt)
  }

  def rdBRAM(rd: RDPort, rdPort: Int, port: BRAMPort):UInt = {
    port.WE := 0.U
    port.DI := 0.U
    port.EN := true.B
    port.ADDR := Cat(rd.tag, rd.port(rdPort).addr)
    port.DO
  }
}

class RFileBRAM[T <: UInt](nbThreads: Int) extends MultiIOModule {
  val rd = IO(new RFileIO.RDPort(nbThreads))
  val wr = IO(new RFileIO.WRPort(nbThreads))

  private val bramConfig = new BRAMConfig(DATA_SZ/8, 8, nbThreads * REG_N, "", false, false, true, false)
  private val rd1_mem = Module(new BRAM()(bramConfig))
  private val rd2_mem = Module(new BRAM()(bramConfig))
  private val rd_addr = Seq(
    Cat(rd.tag, rd.port(0).addr),
    Cat(rd.tag, rd.port(1).addr)
  )
  private val wr_addr = Cat(wr.tag, wr.addr)
  private val regfile = Mem(nbThreads * REG_N, DATA_T)

  rd.port(0).data := RFileIO.rdBRAM(rd, 0, rd1_mem.portA)
  rd.port(1).data := RFileIO.rdBRAM(rd, 1, rd2_mem.portA)

  RFileIO.wr2BRAM(rd1_mem.portB, wr)
  RFileIO.wr2BRAM(rd2_mem.portB, wr)
}

class PStateRegsIO(val nbThreads: Int) extends Bundle {
  val commit = new Bundle {
    val curr = Output(new PStateRegs)
    val next = Input(ValidTag(nbThreads, new PStateRegs))
  }
  val transplant = new Bundle {
    val thread = Input(UInt(log2Ceil(nbThreads).W))
    val pregs = Output(new PStateRegs)
  }
  val issue = new Bundle {
    val thread = Input(UInt(log2Ceil(nbThreads).W))
    val pregs = Output(new PStateRegs)
  }
}

class ArchState(nbThreads: Int, withDbg: Boolean) extends MultiIOModule {
  val rfile_rd = IO(new RFileIO.RDPort(nbThreads))
  val rfile_wr = IO(new RFileIO.WRPort(nbThreads))
  val pstate = IO(new PStateRegsIO(nbThreads))

  private val rfile = Module(new RFileBRAM(nbThreads))
  rfile.rd <> rfile_rd
  rfile.wr <> rfile_wr

  private val pcMem = Mem(nbThreads, DATA_T)

  // This could be further optimized by using 2 BRAMs instead
  private val pcMem_rd1 = pcMem(pstate.issue.thread)    // Both of these can be optimized 
  private val pcMem_rd2 = pcMem(pstate.commit.next.tag) // by carring in the pipeline on fetch
  private val pcMem_rd3 = pcMem(pstate.transplant.thread)
  // pcMem_wr
  pstate.issue.pregs.PC := pcMem_rd1
  pstate.commit.curr.PC := pcMem_rd2
  pstate.transplant.pregs.PC := pcMem_rd3
  when(pstate.commit.next.valid) {
    pcMem(pstate.commit.next.tag) := pstate.commit.next.bits.get.PC
  }

  private val nzcvMem = Mem(nbThreads, NZCV_T)
  private val nzcvMem_rd1 = nzcvMem(pstate.issue.thread)    // Both of these can be optimized
  private val nzcvMem_rd2 = nzcvMem(pstate.commit.next.tag) // by carring them in the pipeline on fetch
  private val nzcvMem_rd3 = nzcvMem(pstate.transplant.thread)
  // pcMem_wr
  pstate.issue.pregs.NZCV := nzcvMem_rd1
  pstate.commit.curr.NZCV := nzcvMem_rd2
  pstate.transplant.pregs.NZCV := nzcvMem_rd3
  when(pstate.commit.next.valid) {
    nzcvMem(pstate.commit.next.tag) := pstate.commit.next.bits.get.NZCV
  }
  

  val dbg = IO(new Bundle {
    val vecState = if(withDbg) Some(Output(Vec(nbThreads, new FullStateBundle))) else None
  })
  if(withDbg) {
    val pregsVec = Wire(Vec(nbThreads, new PStateRegs))
    // Copy of BRAM RegFile states
    val dbgRFile = Mem(nbThreads * REG_N, DATA_T)
    val wrAddr = WireInit(Cat(rfile.wr.tag,rfile.wr.addr))
    when(rfile.wr.en) {
      dbgRFile(wrAddr) := rfile.wr.data
    }
    for(thread <- 0 until nbThreads) {
      for(reg <- 0 until REG_N) {
        // RegNext because BRAM has rd delay of 1
        dbg.vecState.get(thread).rfile(reg) := dbgRFile(((thread << log2Ceil(REG_N))+reg).U)
      }
      pregsVec(thread).PC := pcMem(thread)
      pregsVec(thread).NZCV := nzcvMem(thread)
      dbg.vecState.get(thread).regs := pregsVec(thread)
    }
  }
}