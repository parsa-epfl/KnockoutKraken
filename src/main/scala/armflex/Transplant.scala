package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._

import armflex.util._

import Trans2State._

object TransplantIO extends Bundle {
  class Trans2CPU(val thidN: Int) extends Bundle {
    val thread = Output(UInt(log2Ceil(thidN).W))
    val updatingPState = Output(Bool())
    val rfile_wr = Flipped(new RFileIO.WRPort(thidN))
    val pregs = Output(new PStateRegs)
    val start = Output(Bool())
  }
  class CPU2Trans(val thidN: Int) extends Bundle {
    val rfile_wr = new RFileIO.WRPort(thidN)
    val pregs = Input(new PStateRegs)
    val done = Input(ValidTag(thidN))
  }
  class Mem2Trans(val thidN: Int) extends Bundle {
    val instFault = Input(ValidTag(thidN))
    val dataFault = Input(ValidTag(thidN))
  }
  class Host2Trans(val thidN: Int) extends Bundle {
    val pending = Input(UInt(thidN.W))
  }
  class Trans2Host(val thidN: Int, val bramCfg: BRAMParams) extends Bundle {
    val done = Output(ValidTag(thidN))
    val clear = Output(ValidTag(thidN))
  }
}
class TransplantUnit(thidN: Int) extends MultiIOModule {
  val hostBRAMParams =
    new BRAMParams(NB_COL = DATA_SZ / 8, COL_WIDTH = 8, NB_ELE = thidN * (1 << log2Ceil(ARCH_MAX_OFFST)))
  val cpu2trans = IO(new TransplantIO.CPU2Trans(thidN))
  val trans2cpu = IO(new TransplantIO.Trans2CPU(thidN))
  val host2trans = IO(new TransplantIO.Host2Trans(thidN))
  val trans2host = IO(new TransplantIO.Trans2Host(thidN, hostBRAMParams))
  val hostBramPort = IO(new BRAMPort()(hostBRAMParams))

  val mem2trans = IO(new TransplantIO.Mem2Trans(thidN))

  // Mantains always the latest state of the rfile
  private val stateBuffer = Module(new BRAM()(new BRAMParams(NB_COL = DATA_SZ / 8, COL_WIDTH = 8, NB_ELE = thidN * REG_N)))
  private val stateBufferWrPort = stateBuffer.portA
  private val stateBufferRdPort = stateBuffer.portB

  // Buffer to communicate with Host by exposing BRAM Port
  private val hostBuffer = Module(new BRAM()(hostBRAMParams))
  hostBramPort <> hostBuffer.portB
  private val hostTransPort = hostBuffer.portA

  private val s_IDLE :: s_TRANS :: Nil = Enum(2)
  private val s_BRAM2CPU :: s_CPU2BRAM :: Nil = Enum(2)
  private val state = RegInit(s_IDLE) // Reads from State, pushes to BRAM
  private val stateDir = RegInit(s_BRAM2CPU) // Reads from BRAM, pushes to State

  private val thread = RegInit(0.U(log2Ceil(thidN).W))
  private val thread_next = WireInit(thread)
  thread := thread_next

  private val stateRegType = RegInit(r_DONE)
  private val bramOFFST = RegInit(0.U(log2Ceil(hostBuffer.params.NB_ELE).W))
  private val currReg = RegInit(0.U(log2Ceil(ARCH_MAX_OFFST).W))

  private val cpu2transPending = RegInit(0.U(thidN.W))
  private val cpu2transCpuTrans = Wire(UInt(thidN.W))
  private val cpu2transInstFault = Wire(UInt(thidN.W))
  private val cpu2transDataFault = Wire(UInt(thidN.W))
  private val cpu2transInsert = WireInit(cpu2transCpuTrans | cpu2transDataFault | cpu2transInstFault)
  private val cpu2transClear = WireInit(0.U(log2Ceil(thidN).W))
  private val cpu2transClearEn = WireInit(false.B)
  private val cpu2transClearBit = WireInit(cpu2transClearEn.asUInt << cpu2transClear)

  cpu2transInstFault := Mux(mem2trans.instFault.valid, 1.U << mem2trans.instFault.tag, 0.U)
  cpu2transDataFault := Mux(mem2trans.dataFault.valid, 1.U << mem2trans.dataFault.tag, 0.U)
  cpu2transCpuTrans  := Mux(cpu2trans.done.valid, 1.U << cpu2trans.done.tag, 0.U)
  cpu2transPending := (cpu2transPending & ~cpu2transClearBit) | cpu2transInsert

  trans2host.done.valid := false.B
  trans2host.clear.valid := false.B
  trans2cpu.start := false.B
  switch(state) {
    is(s_IDLE) {
      when(host2trans.pending =/= 0.U || cpu2transPending =/= 0.U) {
        state := s_TRANS
        stateRegType := r_XREGS
        bramOFFST := thread_next << log2Ceil(ARCH_MAX_OFFST)
        currReg := 0.U
      }
      when(host2trans.pending =/= 0.U) {
        stateDir := s_BRAM2CPU
        thread_next := PriorityEncoder(host2trans.pending)
        trans2host.clear.valid := true.B
      }.elsewhen(cpu2transPending =/= 0.U) {
        stateDir := s_CPU2BRAM
        thread_next := PriorityEncoder(cpu2transPending)
        cpu2transClearEn := true.B
        cpu2transClear := thread_next
      }
    }
    is(s_TRANS) {
      bramOFFST := bramOFFST + 1.U
      currReg := currReg + 1.U
      when(currReg === (ARCH_PC_OFFST - 1).U) {
        stateRegType := r_PC
      }.elsewhen(currReg === (ARCH_SP_OFFST - 1).U) {
        stateRegType := r_SP
      }.elsewhen(currReg === (ARCH_PSTATE_OFFST - 1).U) {
        stateRegType := r_NZCV
      }.elsewhen(currReg === (ARCH_MAX_OFFST - 1).U) {
        stateRegType := r_DONE
      }
      when(stateRegType === r_DONE) {
        when(stateDir === s_CPU2BRAM) {
          trans2host.done.valid := true.B
        }.elsewhen(stateDir === s_BRAM2CPU) {
          trans2cpu.start := true.B
        }
        state := s_IDLE
      }
    }
  }

  trans2host.done.tag := thread
  trans2host.clear.tag := thread_next
  trans2cpu.thread := thread
  trans2cpu.pregs := cpu2trans.pregs
  trans2cpu.updatingPState := state === s_TRANS && stateDir === s_BRAM2CPU

  stateBufferRdPort.DI := 0.U
  stateBufferRdPort.WE := 0.U
  stateBufferRdPort.EN := true.B
  stateBufferRdPort.ADDR := Cat(thread, currReg)

  hostTransPort.EN := state === s_TRANS
  hostTransPort.WE := Fill(hostTransPort.params.NB_COL, stateDir === s_CPU2BRAM)
  hostTransPort.ADDR := Mux(stateDir === s_BRAM2CPU, bramOFFST, RegNext(bramOFFST))
  hostTransPort.DI := stateBufferRdPort.DO
  private val stateRegTypeCurr = WireInit(RegNext(stateRegType))
  when(stateRegType === r_XREGS) {
    hostTransPort.DI := stateBufferRdPort.DO
  }.elsewhen(stateRegTypeCurr === r_PC) {
    hostTransPort.DI := cpu2trans.pregs.PC
    trans2cpu.pregs.PC := hostTransPort.DO
  }.elsewhen(stateRegTypeCurr === r_SP) {
    // TODO SP is not used yet
    //hostTransPort.DI := cpu2trans.pregs.SP // 
    //trans2cpu.pregs.SP := hostTransPort.DO //
  }.elsewhen(stateRegTypeCurr === r_NZCV) {
    hostTransPort.DI := PStateGet_NZCV(cpu2trans.pregs.NZCV)
    trans2cpu.pregs.NZCV := PStateGet_NZCV(hostTransPort.DO)
  }

  // RFile response comes 1 cycle delay
  trans2cpu.rfile_wr.en := RegNext(state === s_TRANS && stateRegType === r_XREGS && stateDir === s_BRAM2CPU)
  trans2cpu.rfile_wr.addr := RegNext(Cat(thread, currReg))
  trans2cpu.rfile_wr.data := hostTransPort.DO
  trans2cpu.rfile_wr.tag := RegNext(thread)

  RFileIO.wr2BRAM(stateBufferWrPort, cpu2trans.rfile_wr, (currReg.getWidth - REG_SZ))
  when(state === s_TRANS && stateDir === s_BRAM2CPU) {
    // Forward transplant writes when getting state from host
    RFileIO.wr2BRAM(stateBufferWrPort, trans2cpu.rfile_wr, (currReg.getWidth - REG_SZ))
  }

}

// In parsa-epfl/qemu/fa-qflex
// Read fa-qflex-helper.c to get indexes of values
object Trans2State {
  val r_DONE :: r_XREGS :: r_PC :: r_SP :: r_NZCV :: Nil = Enum(5)
  def PStateGet_NZCV(word: UInt): UInt = word(3, 0)
  val ARCH_XREGS_OFFST = 0
  val ARCH_PC_OFFST = 32
  val ARCH_SP_OFFST = 33
  val ARCH_PSTATE_OFFST = 34
  val ARCH_MAX_OFFST = ARCH_PSTATE_OFFST + 1
}
