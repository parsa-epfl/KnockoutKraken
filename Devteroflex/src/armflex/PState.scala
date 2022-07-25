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
  val execMode = UInt(2.W)
  val isICountDepleted = Bool()
  val isUndef = Bool()
  val isException = Bool()
  val NZCV = NZCV_T
}

class PStateRegs extends Bundle {
  val icountExecuted = UInt(32.W)
  val icountBudget = UInt(32.W)
  val icount = UInt(64.W)
  val asid_unused = UInt(32.W)
  val asid = UInt(32.W)
  val flags = new PStateFlags
  val PC = DATA_T
}

object PStateConsts {
  val ARCH_PSTATE_XREGS_OFFST  = (0)
  val ARCH_PSTATE_PC_OFFST     = (32)
  val ARCH_PSTATE_FLAGS_OFFST  = (33)
  val ARCH_PSTATE_ASID_OFFST   = (34)
  val ARCH_PSTATE_ICOUNT_OFFST = (35)
  val ARCH_PSTATE_ICOUNTREGS_OFFST = (36)
  val ARCH_PSTATE_TOT_REGS     = (37)

  val TRANS_STATE_PState_OFFST   = (32)  // Contains PC, flags, and icount
  val TRANS_STATE_SIZE_BYTES     = (320) // 5 512-bit blocks; Full state fits in this amount of bytes
  val TRANS_STATE_THID_MAX_BYTES = (512) // 8 512-bit blocks; Pad to next power of two 
  val TRANS_STATE_THID_MAX_REGS  = (512/8) // 8 512-bit blocks; Pad to next power of two 
  val TRANS_STATE_regsPerBlock = (512/64) // 8 64-bit regs in a 512-bit block

  val PSTATE_FLAGS_EXECUTE_SINGLESTEP = (2)
  val PSTATE_FLAGS_EXECUTE_NORMAL     = (1)
  val PSTATE_FLAGS_EXECUTE_WAIT       = (0)
}

object PStateRegs {
  def apply(): PStateRegs = {
    val wire = WireInit(new PStateRegs, 0.U.asTypeOf(new PStateRegs))
    wire
  }
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
    val thid = Input(UInt(log2Ceil(thidN).W))
    val pstate = Output(new PStateRegs)
  }
  val forceTransplant = Input(UInt(thidN.W))
  val issue = new Bundle {
    val thid = Input(UInt(log2Ceil(thidN).W))
    val pstate = Output(new PStateRegs)
  }
  val mem = Vec(2, new Bundle {
    val thid = Input(UInt(log2Ceil(thidN).W))
    val asid = Output(UInt(32.W))
  })
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
  private val pstateMem_rd1 = pstateMem(pstateIO.issue.thid)    // Both of these can be optimized 
  private val pstateMem_rd2 = pstateMem(pstateIO.commit.tag) // by carring in the pipeline on fetch
  private val pstateMem_rd3 = pstateMem(pstateIO.transplant.thid)
  private val pstateMem_rd4_asid = pstateMem(pstateIO.mem(0).thid).asid
  private val pstateMem_rd5_asid = pstateMem(pstateIO.mem(1).thid).asid
  // pcMem_wr
  pstateIO.issue.pstate := pstateMem_rd1
  pstateIO.commit.pstate.curr := pstateMem_rd2
  pstateIO.transplant.pstate := pstateMem_rd3
  pstateIO.mem(0).asid := pstateMem_rd4_asid
  pstateIO.mem(1).asid := pstateMem_rd5_asid
  when(pstateIO.commit.fire) {
    pstateMem(pstateIO.commit.tag) := pstateIO.commit.pstate.next
  }
  when(pstateIO.forceTransplant =/= 0.U) {
    pstateMem(OHToUInt(pstateIO.forceTransplant)).flags.isException := true.B
  }

  // icount management
  when(pstateIO.commit.isCommitUnit
        && pstateIO.commit.fire 
        && !pstateIO.commit.pstate.next.flags.isException 
        && !pstateIO.commit.pstate.next.flags.isUndef) {
    pstateMem(pstateIO.commit.tag).icount := pstateMem(pstateIO.commit.tag).icount + 1.U
    pstateMem(pstateIO.commit.tag).icountExecuted := pstateMem(pstateIO.commit.tag).icountExecuted + 1.U
    when(pstateIO.commit.icountLastInst) {
      pstateMem(pstateIO.commit.tag).flags.isICountDepleted := true.B
    }
  }

  when(pstateMem(pstateIO.commit.tag).icountBudget === 0.U) {
    pstateIO.commit.icountLastInst := false.B // Just execute normally, no icount
  }.otherwise {
    pstateIO.commit.icountLastInst := (pstateMem(pstateIO.commit.tag).icountExecuted + 1.U) === pstateMem(pstateIO.commit.tag).icountBudget // See PipelineWithTransplant
  }

  pstateIO.commit.ready := true.B // This signal get's multiplexed higher in the hierarchy

  if (true) {
    when(pstateIO.forceTransplant =/= 0.U && pstateIO.commit.fire) {
      assert((pstateIO.forceTransplant & 1.U << pstateIO.commit.tag) === 0.U)
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
    for(thid <- 0 until thidN) {
      for(reg <- 0 until REG_N) {
        // RegNext because BRAM has rd delay of 1
        dbg.vecState.get(thid).rfile(reg) := dbgRFile(((thid << log2Ceil(REG_N))+reg).U)
      }
      pstateVec(thid) := pstateMem(thid)
      dbg.vecState.get(thid).pc := pstateVec(thid).PC
      dbg.vecState.get(thid).flags := pstateVec(thid).flags.asUInt
    }
  }
}

class FullStateBundle extends Bundle {
  val rfile = Vec(REG_N, DATA_T)
  val pc = DATA_T
  val flags = UInt(32.W)
}