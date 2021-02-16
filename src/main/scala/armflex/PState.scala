package armflex

import chisel3._

import chisel3.util.{Cat, Decoupled, Valid}
import arm.PROCESSOR_TYPES._
import armflex.util.BRAMConfig
import chisel3.util.log2Ceil

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
  def apply()(implicit cfg: ProcConfig): PStateRegs = {
    val wire = Wire(new PStateRegs())
    wire.PC := DATA_X
    wire.SP := DATA_X
    //wire.EL := DATA_X
    wire.NZCV := NZCV_X
    wire
  }
}

class RFileIO(implicit val cfg: ProcConfig) extends Bundle {
  val rs1_addr = Input(REG_T)
  val rs1_data = Output(DATA_T)
  val rs2_addr = Input(REG_T)
  val rs2_data = Output(DATA_T)

  val w1_addr = Input(REG_T)
  val w1_data = Input(DATA_T)
  val w1_en = Input(Bool())

  // ReadWritePort
  val rw_addr = Input(REG_T)
  val rw_di = Input(DATA_T)
  val rw_wen = Input(Bool())
  val rw_do = Output(DATA_T)

  val rfileVec =
    if (cfg.DebugSignals) Some(Output(Vec(REG_N, DATA_T))) else None
}

/**  Register file for each thread.
  *  single write port and two read ports
  */
class RFile(implicit val cfg: ProcConfig) extends Module {
  val io = IO(new RFileIO())

  val regfile = Mem(REG_N, DATA_T)

  io.rs1_data := regfile(io.rs1_addr)
  io.rs2_data := regfile(io.rs2_addr)

  when(io.w1_en) {
    regfile(io.w1_addr) := io.w1_data
  }

  // ReadWirePort
  when(io.rw_wen) {
    regfile(io.rw_addr) := io.rw_di
  }
  io.rw_do := regfile(io.rw_addr)

  // DEBUG Signals ------------------------------------------------------------
  if (cfg.DebugSignals) {
    val vecRFile = Wire(Vec(REG_N, DATA_T))
    for (reg <- 0 until REG_N) vecRFile(reg) := regfile(reg)
    io.rfileVec.get := vecRFile
  }
}

class RFile3IO(val with3: Boolean, val withDbg: Boolean) extends Bundle {
  val rs1_addr = Input(REG_T)
  val rs1_data = Output(DATA_T)
  val rs2_addr = Input(REG_T)
  val rs2_data = Output(DATA_T)

  val w_addr = Input(REG_T)
  val w_data = Input(DATA_T)
  val w_en = Input(Bool())

  // ReadWritePort, disables RFile for a cycle
  val rw =
    if (with3) Some(new Bundle {
      val port = Flipped(Decoupled(new Bundle {
        val addr = REG_T
        val data = DATA_T
        val wen = Bool()
      }))
      val resp = Valid(DATA_T)
    })
    else None
}

/**  Register file for each thread.
  *  single write port and two read ports
  */
class RFile3Arbiter(val withDbg: Boolean) extends MultiIOModule {
  val rfile3 = IO(new RFile3IO(true, false))
  val rfile2 = IO(Flipped(new RFile3IO(false, false)))

  val port1_addr = WireInit(rfile3.rs1_addr)
  val port2_addr = WireInit(rfile3.rs2_addr)
  val port1_data = WireInit(rfile2.rs1_data)
  val port2_data = WireInit(rfile2.rs2_data)

  val portWr_en = WireInit(rfile3.w_en)
  val portWr_addr = WireInit(rfile3.w_addr)
  val portWr_data = WireInit(rfile3.w_data)

  rfile2.rs1_addr := port1_addr
  rfile2.rs2_addr := port2_addr
  rfile3.rs1_data := port1_data
  rfile3.rs2_data := port2_data
  rfile2.w_en := portWr_en
  rfile2.w_data := portWr_data
  rfile2.w_addr := portWr_addr

  // Arbiter Extra port
  val isReady = RegInit(true.B)
  isReady := true.B
  when(rfile3.rw.get.port.valid) {
    isReady := false.B
  }

  when(!isReady) {
    portWr_en := RegNext(rfile3.rw.get.port.bits.wen)
    portWr_addr := RegNext(rfile3.rw.get.port.bits.addr)
    portWr_data := RegNext(rfile3.rw.get.port.bits.data)
    port2_addr := RegNext(rfile3.rw.get.port.bits.addr)
  }
  rfile3.rw.get.port.ready := isReady
  rfile3.rw.get.resp.valid := !isReady
  rfile3.rw.get.resp.bits := port2_data
}

object RFileSingleIO {
  class RDPort[T <: Data](gen: T) extends Bundle {
    val port = Vec(2, new Bundle {
        val addr = Input(REG_T)
        val data = Output(DATA_T)
      }
    )
    val tag = Input(gen)
    override def cloneType: this.type = new RDPort(gen).asInstanceOf[this.type]

  }

  class WRPort[T <: Data](gen: T) extends Bundle {
    val addr = Input(REG_T)
    val data = Input(DATA_T)
    val en = Input(Bool())
    val tag = Input(gen)
    override def cloneType: this.type = new WRPort(gen).asInstanceOf[this.type]

  }
}

class RFileSingle[T <: Data](nbThreads: Int, gen: T, withDbg: Boolean = false) extends MultiIOModule {
  val rd = IO(new RFileSingleIO.RDPort(gen))
  val wr = IO(new RFileSingleIO.WRPort(gen))

  private val rd_addr = Seq(
    Cat(rd.tag.asUInt, rd.port(0).addr),
    Cat(rd.tag.asUInt, rd.port(1).addr)
  )
  private val wr_addr = Cat(wr.tag.asUInt, wr.addr)
  private val regfile = Mem(nbThreads * REG_N, DATA_T)

  rd.port(0).data := regfile(rd_addr(0))
  rd.port(1).data := regfile(rd_addr(1))

  when(wr.en) {
    regfile(wr_addr) := wr.data
  }

  val dbg = IO(new Bundle {
    val rfileVec =
      if (withDbg) Some(Output(Vec(nbThreads, Vec(REG_N, DATA_T))))
      else None
  })
  if (withDbg) {
    for (thread <- 0 until nbThreads) {
      val vecRFile = Wire(Vec(REG_N, DATA_T))
      for (reg <- 0 until REG_N) vecRFile(reg) := regfile(Cat(thread.U, reg.U))
      dbg.rfileVec.get(thread) := vecRFile
    }
  }
}
