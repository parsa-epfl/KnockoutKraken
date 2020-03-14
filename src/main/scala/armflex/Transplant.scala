package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._

import util._

import TPU2STATE._

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
  val host2tpu = new TransplantUnitHostIO
  val tpu2cpu = new TransplantUnitCPUIO

  val tpu2cpuStateReg = Output(Valid(r_DONE.cloneType)) // Current state reg being written
  val tpu2cpuState = Output(new PStateRegs)
  val cpu2tpuState = Input(new PStateRegs)
  val rfile = Flipped(new RFileIO)

  val stateBRAM = Flipped(new BRAMPort()(cfg.bramConfigState))
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
  val fire = Input(ValidTag(cfg.TAG_T))
  val done = Output(ValidTag(cfg.TAG_T))
  val missTLB = Output(ValidTag(MISS_T, DATA_T))
  val fillTLB = Input(ValidTag(DATA_T, new TLBEntry))
  val getState = Input(ValidTag(cfg.TAG_T))
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
  val flush = Output(ValidTag(cfg.TAG_T))
  val fire = Output(ValidTag(cfg.TAG_T))
  val freeze = Output(ValidTag(cfg.TAG_T))
  val done = Input(ValidTag(cfg.TAG_T))

  val missTLB = Input(ValidTag(MISS_T, DATA_T))
  val fillTLB = Output(ValidTag(DATA_T, new TLBEntry))
}

class TransplantUnit(implicit val cfg: ProcConfig) extends Module{
  val io = IO(new TransplantUnitIO)

  val bramOFFST = RegInit(0.U(log2Ceil(ARCH_MAX_OFFST).W))
  val bramOut = WireInit(io.stateBRAM.DO) 

  // Only port rs1 of RFILE is used
  val regDataIn = io.rfile.rs1_data
  val regAddr = bramOFFST // First 32 word are registers
  io.rfile.rs1_addr := regAddr
  io.rfile.rs2_addr := 0.U

  val s_IDLE :: s_TRANS :: Nil = Enum(2)
  val s_BRAM2CPU :: s_CPU2BRAM :: Nil = Enum(2)
  val state = RegInit(s_IDLE)
  val stateDir = RegInit(s_BRAM2CPU)
  val stateRegType = RegInit(r_DONE)

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
    stateRegType := r_DONE
    freeze := false.B

    state := s_IDLE
  }

  switch(state) {
    is(s_IDLE) {
      when(io.host2tpu.fire.valid) {
        freeze := true.B
        freezeTag := io.host2tpu.fire.tag
        stateDir := s_BRAM2CPU
        stateRegType := r_XREGS
        bramOFFST := ARCH_XREGS_OFFST.U

        state := s_TRANS
      }.elsewhen(io.tpu2cpu.done.valid) {
        freeze := true.B
        freezeTag := io.tpu2cpu.done.tag
        stateDir := s_CPU2BRAM
        stateRegType := r_XREGS
        bramOFFST := ARCH_XREGS_OFFST.U
        flushSig := true.B

        state := s_TRANS
      }.elsewhen(io.host2tpu.getState.valid) {
        freeze := true.B
        freezeTag := io.host2tpu.getState.tag
        stateDir := s_CPU2BRAM
        stateRegType := r_XREGS
        bramOFFST := ARCH_XREGS_OFFST.U
        flushSig := true.B

        state := s_TRANS
      }
    }

    is(s_TRANS) {
      bramOFFST := bramOFFST + 1.U
      when(bramOFFST === (ARCH_PC_OFFST - 1).U) {
        stateRegType := r_PC
      }.elsewhen(bramOFFST === (ARCH_SP_OFFST - 1).U) {
        stateRegType := r_SP
      }.elsewhen( bramOFFST === (ARCH_PSTATE_OFFST - 1).U) {
        stateRegType := r_NZCV
      }.elsewhen( bramOFFST === (ARCH_MAX_OFFST - 1).U ) {
        stateRegType := r_DONE
      }.elsewhen( stateRegType === r_DONE ){
        when(stateDir === s_BRAM2CPU) {
          fireSig := true.B
        }.elsewhen(stateDir === s_CPU2BRAM) {
          doneSig := true.B
        }
        resetState
      }
    }
  }

  io.stateBRAM.EN := true.B
  io.stateBRAM.WE := Fill(8, stateDir === s_CPU2BRAM && stateRegType =/= r_DONE)
  io.stateBRAM.ADDR := bramOFFST
  when(stateRegType === r_XREGS) {
    io.stateBRAM.DI := regDataIn
  }.elsewhen(stateRegType === r_PC) {
    io.stateBRAM.DI := io.cpu2tpuState.PC
  }.elsewhen(stateRegType === r_SP) {
    io.stateBRAM.DI := io.cpu2tpuState.SP
  }.elsewhen(stateRegType === r_NZCV) {
    io.stateBRAM.DI := Cat(0.U, io.cpu2tpuState.NZCV(3,0))
  }.otherwise {
    io.stateBRAM.DI := 0.U
  }

  // One cycle delay from BRAM read
  io.rfile.w1_en := RegNext(stateDir === s_BRAM2CPU && stateRegType === r_XREGS)
  io.rfile.w1_addr := RegNext(regAddr)
  io.rfile.w1_data := bramOut

  io.rfile.w2_en   := false.B
  io.rfile.w2_addr := DontCare
  io.rfile.w2_data := DontCare

  io.tpu2cpuState.PC := bramOut
  io.tpu2cpuState.SP := bramOut
  io.tpu2cpuState.NZCV := TPU2STATE.PStateGet_NZCV(bramOut)
  // One cycle delay from BRAM read
  io.tpu2cpuStateReg.bits := RegNext(stateRegType)
  io.tpu2cpuStateReg.valid := RegNext(stateDir === s_BRAM2CPU)

  // Signals come with 1 cycle delay as cmd signals start when State machine is back to IDLE
  val freezeTag1D = RegNext(freezeTag)
  io.tpu2cpu.fire.valid := RegNext(fireSig)
  io.tpu2cpu.fire.tag := freezeTag1D
  io.tpu2cpu.freeze.valid := freeze
  io.tpu2cpu.freeze.tag := freezeTag
  io.tpu2cpu.flush.valid := RegNext(flushSig)
  io.tpu2cpu.flush.tag := freezeTag1D
  io.host2tpu.done.valid := RegNext(doneSig)
  io.host2tpu.done.tag := freezeTag1D

  // CPU <-> HOST direct communcation
  io.host2tpu.missTLB <> io.tpu2cpu.missTLB
  io.host2tpu.fillTLB <> io.tpu2cpu.fillTLB
}

// In parsa-epfl/qemu/fa-qflex
// Read fa-qflex-helper.c to get indexes of values
object TPU2STATE {
  val r_DONE :: r_XREGS :: r_PC :: r_SP :: r_NZCV :: Nil = Enum(5)
  def PStateGet_NZCV(word: UInt): UInt = word(3,0)
  val ARCH_XREGS_OFFST = 0
  val ARCH_PC_OFFST    = 32
  val ARCH_SP_OFFST    = 33
  val ARCH_PSTATE_OFFST = 34
  val ARCH_MAX_OFFST = ARCH_PSTATE_OFFST + 1
}
