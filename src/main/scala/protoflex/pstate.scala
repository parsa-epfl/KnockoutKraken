package protoflex

import chisel3._

import common.PROCESSOR_TYPES._

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

class PStateRegs extends Bundle
{
  val PC = DATA_T
  val SP = DATA_T // Normaly 4 levels
  val EL = DATA_T // Normaly 4 levels

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
  def empty() = {
    PC := DATA_X
    SP := DATA_X
    EL := DATA_X
    NZCV := DATA_X
    this
  }
}

class RFileIO extends Bundle
{
  val rs1_addr = Input(REG_T)
  val rs1_data = Output(DATA_T)
  val rs2_addr = Input(REG_T)
  val rs2_data = Output(DATA_T)

  val waddr    = Input(REG_T)
  val wdata    = Input(DATA_T)
  val wen      = Input(Bool())
}

class RFile extends Module
{
  val io = IO(new RFileIO())

  val regfile = Mem(REG_N, DATA_T)

  when (io.wen)
  {
    regfile(io.waddr) := io.wdata
  }

  io.rs1_data := regfile(io.rs1_addr)
  io.rs2_data := regfile(io.rs2_addr)
}

