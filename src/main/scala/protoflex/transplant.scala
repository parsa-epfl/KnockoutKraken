package protoflex

import chisel3.{core, _}
import chisel3.util._
import common.{BRAMConfig, BRAMPort}
import common.PROCESSOR_TYPES._
import protoflex.TPU2STATE._

/*
 * Transplant Unit (TPU):
 * 0. Beforehand STATE -> BRAM by HOST
 * 1. When host2cpu fire
 *    1. freeze CPU (tpu2cpu.freeze)
 *    2. transplant state BRAM -> CPU
 *    3. unfreeze CPU
 *    4. fire CPU   (tpu2cpu.fire)
 * 2. When tpu2cpu done
 *    1. freeze CPU (tpu2cpu.freeze)
 *    2. transplant state CPU -> BRAM
 *    3. unfreeze CPU
 *    4. flush CPU
 *    5. Set host2cpu done
 * 3. Afterhand by HOST
 *    1. Read BRAM -> HOST
 */
class TransplantUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  private implicit val stateBRAMc = cfg.stateBRAMc
  val host2tpu = new TransplantUnitHostIO
  val tpu2cpu = new TransplantUnitCPUIO

  val tpu2cpuStateReg = Output(Valid(r_NULL.cloneType)) // Current state reg being written
  val tpu2cpuState = Output(new PStateRegs)
  val cpu2tpuState = Input(new PStateRegs)
  val rfile = Flipped(new RFileIO)

  val stateBRAM = Flipped(new BRAMPort(1))
}

/*
 * Fire    : Host prepared state in BRAM and fires to tp start the transplant,
 *           completed HOST -> BRAM, now BRAM -> CPU by TPU
 * FireTag : Which Thread to transplant.
 * Done    : CPU hit a transplant condition
 *           TPU has completed the CPU -> BRAM
 *           Now instruct HOST to perform BRAM -> HOST
 *
 * Note: Fire and FireTag are expected to be Pulsed
 */
class TransplantUnitHostIO(implicit val cfg: ProcConfig) extends Bundle
{
  val fire    = Input(Bool())
  val fireTag = Input(cfg.TAG_T)
  val done    = Output(Bool())
  val doneTag = Output(cfg.TAG_T)
}

/*
 * Freeze : Freezes thread which state's is being transplanted (CPU -> BRAM or BRAM -> CPU)
 * Fire : TPU has complted state transfer : BRAM -> CPU
 *        Now instruct CPU to start processing
 * Done : CPU has hit a transplant condition
 *        Now TPU has to bring state to BRAM : CPU -> BRAM
 * Flush : Clears the CPU pipeline
 */
class TransplantUnitCPUIO(implicit val cfg: ProcConfig) extends Bundle
{
  val flush = Output(Bool())
  val freeze = Output(Bool())
  val fire = Output(Bool())
  val done = Input(Bool())
  val flushTag = Output(cfg.TAG_T)
  val fireTag = Output(cfg.TAG_T)
  val freezeTag = Output(cfg.TAG_T)
  val doneTag = Input(cfg.TAG_T)
}

class TransplantUnit(implicit val cfg: ProcConfig) extends Module{
  val stateAddrW = cfg.stateBRAMc.addrWidthVec(1)

  val io = IO(new TransplantUnitIO)

  val bramOFFST = RegInit(0.U(log2Ceil(ARCH_MAX_OFFST).W))
  val bramOut = WireInit(io.stateBRAM.dataOut.get(31,0)) // 32 Bits read of BRAM
  // BRAM is 32b, so if we read two words in a row, we get a 64b word (usefull for XREGS and PC)
  val bramOut1CD = RegNext(bramOut) // 1 Cycle Delay
  val bramOut64b = WireInit(Cat(bramOut1CD, bramOut))

  // Only port rs1 of RFILE is used
  val regDataIn = io.rfile.rs1_data
  val regDataInMSB = regDataIn(63,32)
  val regDataInLSB = regDataIn(31, 0)
  // RFILE is 64bits, BRAM is 32bits. XREGS are first 32 words
  // By shifting lsb, we get the RFILE addr from BRAM offst
  val regAddr = bramOFFST >> 1.U
  io.rfile.rs1_addr := regAddr
  io.rfile.rs2_addr := 0.U

  val s_IDLE :: s_TRANS :: Nil = Enum(2)
  val s_BRAM2CPU :: s_CPU2BRAM :: Nil = Enum(2)
  val state = RegInit(s_IDLE)
  val stateDir = RegInit(s_BRAM2CPU)
  val stateRegType = RegInit(r_NULL)

  // Freeze toggles till transplant is done (when resetState is called)
  val freeze = RegInit(false.B)
  val freezeTag = RegInit(cfg.TAG_X)
  // Flush when transplant to host is done (when resetState is called)
  val flushSig = WireInit(false.B)
  val flushReg = RegInit(false.B)
  // Fire pulses for a cycle when fireSig is set true.
  val fireSig = WireInit(false.B)
  // Done pulses for a cycle when doneSig is set true.
  val doneSig = WireInit(false.B)
 
  def resetState = {
    bramOFFST := 0.U
    stateRegType := r_NULL
    freeze := false.B

    state := s_IDLE
  }

  switch(state) {
    is(s_IDLE) {
      when(io.host2tpu.fire) {
        freeze := true.B
        freezeTag := io.host2tpu.fireTag
        stateDir := s_BRAM2CPU
        stateRegType := r_XREGS
        bramOFFST := ARCH_XREGS_OFFST.U

        state := s_TRANS
      }.elsewhen(io.tpu2cpu.done) {
        freeze := true.B
        freezeTag := io.tpu2cpu.doneTag
        stateDir := s_CPU2BRAM
        stateRegType := r_XREGS
        bramOFFST := ARCH_XREGS_OFFST.U
        flushSig := true.B

        state := s_TRANS
      }
    }

    is(s_TRANS) {
      bramOFFST := bramOFFST + 1.U
      when(bramOFFST === ARCH_PC_OFFST.U) {
        stateRegType := r_PC
      }.elsewhen( bramOFFST === ARCH_PSTATE_OFFST.U) {
        stateRegType := r_SP_EL_NZCV
      }.elsewhen( bramOFFST === ARCH_MAX_OFFST.U ) {
        when(stateDir === s_BRAM2CPU) {
          fireSig := true.B
        }.elsewhen(stateDir === s_CPU2BRAM) {
          doneSig := true.B
        }
        resetState
      }
    }
  }

  io.stateBRAM.en := true.B
  io.stateBRAM.writeEn.get := stateDir === s_CPU2BRAM
  io.stateBRAM.addr := bramOFFST
  when(stateRegType === r_XREGS) {
    io.stateBRAM.dataIn.get := Mux(bramOFFST(0), regDataInMSB, regDataInLSB)
  }.elsewhen(stateRegType === r_PC) {
    io.stateBRAM.dataIn.get := Mux(bramOFFST(0), io.cpu2tpuState.PC(63,32), io.cpu2tpuState.PC(31,0))
  }.elsewhen(stateRegType === r_SP_EL_NZCV) {
    io.stateBRAM.dataIn.get := Cat(0.U, io.cpu2tpuState.SP(0), io.cpu2tpuState.EL(0), io.cpu2tpuState.NZCV(3,0))
  }.otherwise {
    io.stateBRAM.dataIn.get := 0.U
  }

  io.rfile.wen := stateDir === s_BRAM2CPU && stateRegType === r_XREGS
  // 1 Cycle delay from BRAM read
  io.rfile.waddr := RegNext(regAddr)
  io.rfile.wdata := bramOut64b

  io.tpu2cpuState.PC := bramOut64b
  io.tpu2cpuState.SP := TPU2STATE.PStateGet_SP(bramOut)
  io.tpu2cpuState.EL := TPU2STATE.PStateGet_EL(bramOut)
  io.tpu2cpuState.NZCV := TPU2STATE.PStateGet_NZCV(bramOut)
  io.tpu2cpuStateReg.bits := stateRegType
  io.tpu2cpuStateReg.valid := stateDir === s_BRAM2CPU

  io.tpu2cpu.fire := RegNext(fireSig)
  io.tpu2cpu.fireTag := RegNext(freezeTag) // Get the current freezeTag with the 1 cycle delay from fireReg
  io.tpu2cpu.freeze := freeze
  io.tpu2cpu.freezeTag := freezeTag
  io.tpu2cpu.flush := RegNext(flushSig)
  io.tpu2cpu.flushTag := RegNext(freezeTag)
  io.host2tpu.done := RegNext(doneSig)
  io.host2tpu.doneTag := RegNext(freezeTag)
}

// In parsa-epfl/qemu/fa-qflex
// Read fa-qflex-helper.c to get indexes of values
object TPU2STATE {
  val r_NULL :: r_XREGS :: r_PC :: r_SP_EL_NZCV :: Nil = Enum(4)
  def PStateGet_NZCV(word: UInt): UInt = word(3,0)
  def PStateGet_EL(word: UInt): UInt = word(4,4)
  def PStateGet_SP(word: UInt): UInt = word(5,5)
  val ARCH_XREGS_OFFST = 0
  val ARCH_PC_OFFST    = 64
  val ARCH_PSTATE_OFFST = 66
  val ARCH_MAX_OFFST = ARCH_PSTATE_OFFST + 1
}
