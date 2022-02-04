package armflex

import chisel3._

import chisel3.util.{Cat, Decoupled, Valid, Fill, Counter, OHToUInt}
import arm.PROCESSOR_TYPES._
import chisel3.util.log2Ceil

import armflex.util._

/*
 * The 64-bit Execution state. This Execution state:
 * • Provides 31 64-bit general-purpose registers, of sel X30 is used as the procedure link
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
 * • Names each System register using a suffix that indicates the lowest Exception level at sel
 * the register can be accessed.
 */

class PStateFlags extends Bundle {
  val isUndef = Bool()
  val isException = Bool()
  val NZCV = NZCV_T
}

class PStateRegs extends Bundle {
  val PC = DATA_T
  val flags = new PStateFlags
}

object PStateRegs {
  def apply(): PStateRegs = {
    val wire = Wire(new PStateRegs)
    wire.PC := DATA_X
    wire.flags.NZCV := NZCV_X
    wire.flags.isException := false.B
    wire.flags.isUndef := false.B
    wire
  }
  def getNZCV(bits: UInt): UInt = bits(3, 0)
}
object RFileIO {
  class RDPort(thidN: Int) extends Bundle {
    val port = Vec(3, new Bundle {
        val addr = Input(REG_T)
        val data = Output(DATA_T)
      }
    )
    val tag = Input(UInt(log2Ceil(thidN).W))
  }

  class WRPort(thidN: Int) extends Bundle {
    val addr = Input(REG_T)
    val data = Input(DATA_T)
    val en = Input(Bool())
    val tag = Input(UInt(log2Ceil(thidN).W))
  }

  def wr2BRAM(port: BRAMPort, wr: WRPort) = {
    port.ADDR := Cat(wr.tag, wr.addr)
    port.DI := wr.data
    port.EN := wr.en.asUInt
    port.WE := Fill(port.params.NB_COL, wr.en.asUInt)
  }

  // Offset might be different depending on the BRAM
  def wr2BRAM(port: BRAMPort, wr: WRPort, pad: Int) = {
    port.ADDR := Cat(wr.tag, 0.U(pad.W), wr.addr)
    port.DI := wr.data
    port.EN := wr.en.asUInt
    port.WE := Fill(port.params.NB_COL, wr.en.asUInt)
  }

  def rdBRAM(rd: RDPort, rdPort: Int, port: BRAMPort):UInt = {
    port.WE := 0.U
    port.DI := 0.U
    port.EN := true.B
    port.ADDR := Cat(rd.tag, rd.port(rdPort).addr)
    port.DO
  }
}

class RFileBRAM[T <: UInt](thidN: Int) extends Module {
  val rd = IO(new RFileIO.RDPort(thidN))
  val wr = IO(new RFileIO.WRPort(thidN))

  private val bramParams = new BRAMParams(DATA_SZ/8, 8, thidN * REG_N, "", false, false, true, false)
  private val rd1_mem = Module(new BRAM()(bramParams))
  private val rd2_mem = Module(new BRAM()(bramParams))
  private val rd3_mem = Module(new BRAM()(bramParams))
  private val regfile = Mem(thidN * REG_N, DATA_T)

  rd.port(0).data := RFileIO.rdBRAM(rd, 0, rd1_mem.portA)
  rd.port(1).data := RFileIO.rdBRAM(rd, 1, rd2_mem.portA)
  rd.port(2).data := RFileIO.rdBRAM(rd, 2, rd3_mem.portA)

  RFileIO.wr2BRAM(rd1_mem.portB, wr)
  RFileIO.wr2BRAM(rd2_mem.portB, wr)
  RFileIO.wr2BRAM(rd3_mem.portB, wr)
}

class PStateIO(val thidN: Int) extends Bundle {
  val commit = Flipped(new CommitArchStateIO(thidN))
  val transplant = new Bundle {
    val thread = Input(UInt(log2Ceil(thidN).W))
    val pstate = Output(new PStateRegs)
  }
  val forceTransplant = Input(UInt(thidN.W))
  val issue = new Bundle {
    val thread = Input(UInt(log2Ceil(thidN).W))
    val pstate = Output(new PStateRegs)
  }
}

class ArchState(thidN: Int, withDbg: Boolean) extends Module {
  val rfile_rd = IO(new RFileIO.RDPort(thidN))
  val rfile_wr = IO(new RFileIO.WRPort(thidN))
  val pstateIO = IO(new PStateIO(thidN))

  private val rfile = Module(new RFileBRAM(thidN))
  rfile.rd <> rfile_rd
  rfile.wr <> rfile_wr

  private val pstateMem = Mem(thidN, new PStateRegs)

  // This could be further optimized by using 2 BRAMs instead
  private val pstateMem_rd1 = pstateMem(pstateIO.issue.thread)    // Both of these can be optimized 
  private val pstateMem_rd2 = pstateMem(pstateIO.commit.tag) // by carring in the pipeline on fetch
  private val pstateMem_rd3 = pstateMem(pstateIO.transplant.thread)
  // pcMem_wr
  pstateIO.issue.pstate := pstateMem_rd1
  pstateIO.commit.pstate.curr := pstateMem_rd2
  pstateIO.transplant.pstate := pstateMem_rd3
  when(pstateIO.commit.fire) {
    pstateMem(pstateIO.commit.tag) := pstateIO.commit.pstate.next
  }
  when(pstateIO.forceTransplant =/= 0.U) {
    pstateMem(OHToUInt(pstateIO.forceTransplant)).flags.isException := true.B
  }

  pstateIO.commit.ready := DontCare // See PipelineWithTransplant
  private val icountMem = Seq.fill(thidN)(Counter(32))
  for (thid <- 0 until icountMem.size) {
    when(pstateIO.commit.fire && pstateIO.commit.tag === thid.U) {
      icountMem(thid).inc()
    }
  }

  val dbg = IO(new Bundle {
    val vecState = if(withDbg) Some(Output(Vec(thidN, new FullStateBundle))) else None
  })
  if(withDbg) {
    val pstateVec = Wire(Vec(thidN, new PStateRegs))
    // Copy of BRAM RegFile states
    val dbgRFile = Mem(thidN * REG_N, DATA_T)
    val wrAddr = WireInit(Cat(rfile.wr.tag,rfile.wr.addr))
    when(rfile.wr.en) {
      dbgRFile(wrAddr) := rfile.wr.data
    }
    for(thread <- 0 until thidN) {
      for(reg <- 0 until REG_N) {
        // RegNext because BRAM has rd delay of 1
        dbg.vecState.get(thread).rfile(reg) := dbgRFile(((thread << log2Ceil(REG_N))+reg).U)
      }
      pstateVec(thread) := pstateMem(thread)
      dbg.vecState.get(thread).pc := pstateVec(thread).PC
      dbg.vecState.get(thread).flags := pstateVec(thread).flags.asUInt
    }
  }
}

class FullStateBundle extends Bundle {
  val rfile = Vec(REG_N, DATA_T)
  val pc = DATA_T
  val flags = UInt(32.W)
}