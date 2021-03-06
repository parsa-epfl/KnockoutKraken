package armflex

import chisel3._

import arm.PROCESSOR_TYPES._

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

class PStateRegs(implicit val cfg : ProcConfig) extends Bundle
{
  val PC = DATA_T
  val SP = DATA_T // Normaly 4 levels 32 bits
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
  def apply()(implicit cfg : ProcConfig): PStateRegs = {
    val wire = Wire(new PStateRegs())
    wire.PC := DATA_X
    wire.SP := DATA_X
    //wire.EL := DATA_X
    wire.NZCV := NZCV_X
    wire
  }
}

class RFileIO(implicit val cfg : ProcConfig) extends Bundle
{
  val rs1_addr = Input(REG_T)
  val rs1_data = Output(DATA_T)
  val rs2_addr = Input(REG_T)
  val rs2_data = Output(DATA_T)

  val w1_addr = Input(REG_T)
  val w1_data = Input(DATA_T)
  val w1_en   = Input(Bool())

  // ReadWritePort
  val rw_addr = Input(REG_T)
  val rw_di = Input(DATA_T)
  val rw_wen = Input(Bool())
  val rw_do = Output(DATA_T)

  val rfileVec = if(cfg.DebugSignals) Some(Output(Vec(REG_N, DATA_T))) else None
}

/**
  *  Register file for each thread.
  *  single write port and two read ports
  */
class RFile(implicit val cfg : ProcConfig) extends Module
{
  val io = IO(new RFileIO())

  val regfile = Mem(REG_N, DATA_T)

  io.rs1_data := regfile(io.rs1_addr)
  io.rs2_data := regfile(io.rs2_addr)

  when (io.w1_en) {
    regfile(io.w1_addr) := io.w1_data
  }

  // ReadWirePort
  when (io.rw_wen) {
    regfile(io.rw_addr) := io.rw_di
  }
  io.rw_do := regfile(io.rw_addr)

  // DEBUG Signals ------------------------------------------------------------
  if(cfg.DebugSignals) {
   val  vecRFile = Wire(Vec(REG_N, DATA_T))
    for(reg <- 0 until REG_N) vecRFile(reg) := regfile(reg)
    io.rfileVec.get := vecRFile
  }
}

