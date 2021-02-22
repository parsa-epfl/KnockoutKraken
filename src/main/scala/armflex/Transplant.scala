package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._

import armflex.util._

import Trans2State._

object TransplantIO extends Bundle {
  class Trans2CPU(val nbThreads: Int) extends Bundle {
    val thread = Output(UInt(log2Ceil(nbThreads).W))
    val updatingPState = Output(Bool())
    val rfile_wr = Flipped(new RFileIO.WRPort(nbThreads))
    val pregs = Output(new PStateRegs)
    val start = Output(Bool())
  }
  class CPU2Trans(val nbThreads: Int) extends Bundle {
    val rfile_wr = new RFileIO.WRPort(nbThreads)
    val pregs = Input(new PStateRegs)
    val done = Input(ValidTag(nbThreads))
  }
  class Host2Trans(val nbThreads: Int) extends Bundle {
    val done = Input(ValidTag(nbThreads))
  }
  class Trans2Host(val nbThreads: Int, val bramCfg: BRAMConfig) extends Bundle {
    val done = Output(ValidTag(nbThreads))
  }
}
class TransplantUnit(nbThreads: Int) extends MultiIOModule {
  val hostBRAMConfig =
    new BRAMConfig(NB_COL = DATA_SZ / 8, COL_WIDTH = 8, NB_ELE = nbThreads * ARCH_MAX_OFFST)
  val cpu2trans = IO(new TransplantIO.CPU2Trans(nbThreads))
  val trans2cpu = IO(new TransplantIO.Trans2CPU(nbThreads))
  val host2trans = IO(new TransplantIO.Host2Trans(nbThreads))
  val trans2host = IO(new TransplantIO.Trans2Host(nbThreads, hostBRAMConfig))
  val hostBramPort = IO(new BRAMPort()(hostBRAMConfig))

  // Mantains always the latest state of the rfile
  private val stateBuffer = Module(
    new BRAM()(new BRAMConfig(NB_COL = DATA_SZ / 8, COL_WIDTH = 8, NB_ELE = nbThreads * REG_N))
  )
  private val stateBufferWrPort = stateBuffer.portA
  private val stateBufferRdPort = stateBuffer.portB

  // Buffer to communicate with Host by exposing BRAM Port
  private val hostBuffer = Module(new BRAM()(hostBRAMConfig))
  hostBramPort <> hostBuffer.portB
  private val hostTransPort = hostBuffer.portA

  private val s_IDLE :: s_TRANS :: Nil = Enum(2)
  private val s_BRAM2CPU :: s_CPU2BRAM :: Nil = Enum(2)
  private val state = RegInit(s_IDLE) // Reads from State, pushes to BRAM
  private val stateDir = RegInit(s_CPU2BRAM) // Reads from BRAM, pushes to State

  private val thread = RegInit(0.U(log2Ceil(nbThreads).W))

  private val stateRegType = RegInit(r_DONE)
  private val bramOFFST = RegInit(0.U(log2Ceil(hostBuffer.cfg.NB_ELE).W))
  private val currReg = RegInit(0.U(log2Ceil(ARCH_MAX_OFFST).W))

  private val hostReqQueue = Module(new Queue(UInt(log2Ceil(nbThreads).W), nbThreads, true))
  private val cpuReqQueue = Module(new Queue(UInt(log2Ceil(nbThreads).W), nbThreads, true))
  cpuReqQueue.io.enq.valid := cpu2trans.done.valid
  cpuReqQueue.io.enq.bits := cpu2trans.done.tag
  hostReqQueue.io.enq.valid := host2trans.done.valid
  hostReqQueue.io.enq.bits := host2trans.done.tag

  hostReqQueue.io.deq.ready := state === s_IDLE
  cpuReqQueue.io.deq.ready := state === s_IDLE && !hostReqQueue.io.deq.valid

  trans2host.done.valid := false.B
  trans2cpu.start := false.B
  switch(state) {
    is(s_IDLE) {
      when(hostReqQueue.io.deq.fire || cpuReqQueue.io.deq.fire) {
        state := s_TRANS
        stateRegType := r_XREGS
        bramOFFST := cpuReqQueue.io.deq.bits << log2Ceil(ARCH_MAX_OFFST)
        currReg := 0.U
      }
      when(hostReqQueue.io.deq.fire) {
        stateDir := s_BRAM2CPU
        thread := hostReqQueue.io.deq.bits
      }.elsewhen(cpuReqQueue.io.deq.fire) {
        stateDir := s_CPU2BRAM
        thread := cpuReqQueue.io.deq.bits
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
  trans2cpu.thread := thread
  trans2cpu.pregs := cpu2trans.pregs
  trans2cpu.updatingPState := state === s_TRANS && stateDir === s_BRAM2CPU

  stateBufferRdPort.DI := 0.U
  stateBufferRdPort.WE := 0.U
  stateBufferRdPort.EN := true.B
  stateBufferRdPort.ADDR := Cat(thread, currReg)

  hostTransPort.EN := state === s_TRANS
  hostTransPort.WE := Fill(hostTransPort.cfg.NB_COL, stateDir === s_CPU2BRAM)
  hostTransPort.ADDR := bramOFFST
  hostTransPort.DI := stateBufferRdPort.DO
  private val stateRegTypeCurr = WireInit(Mux(stateDir === s_CPU2BRAM, stateRegType, RegNext(stateRegType)))
  when(stateRegType === r_XREGS) {
    hostTransPort.DI := stateBufferRdPort.DO
  }.elsewhen(stateRegTypeCurr === r_PC) {
    hostTransPort.DI := cpu2trans.pregs.PC
    trans2cpu.pregs.PC := hostTransPort.DO
  }.elsewhen(stateRegTypeCurr === r_SP) {
    hostTransPort.DI := cpu2trans.pregs.SP
    trans2cpu.pregs.SP := hostTransPort.DO
  }.elsewhen(stateRegTypeCurr === r_NZCV) {
    hostTransPort.DI := PStateGet_NZCV(cpu2trans.pregs.NZCV)
    trans2cpu.pregs.NZCV := PStateGet_NZCV(hostTransPort.DO)
  }

  // RFile response comes 1 cycle delay
  trans2cpu.rfile_wr.en := RegNext(state === s_TRANS && stateRegType === r_XREGS && stateDir === s_BRAM2CPU)
  trans2cpu.rfile_wr.addr := RegNext(Cat(thread, currReg))
  trans2cpu.rfile_wr.data := hostTransPort.DO
  trans2cpu.rfile_wr.tag := RegNext(thread)

  RFileIO.wr2BRAM(stateBufferWrPort, cpu2trans.rfile_wr)
  when(state === s_TRANS && stateDir === s_BRAM2CPU) {
    // Forward transplant writes when getting state from host
    RFileIO.wr2BRAM(stateBufferWrPort, trans2cpu.rfile_wr)
  }

}

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
class TransplantUnitIO(implicit val cfg: ProcConfig) extends Bundle {
  val host2tpu = new TransplantUnitHostIO
  val tpu2cpu = new TransplantUnitCPUIO

  val tpu2cpuStateReg = Output(Valid(r_DONE.cloneType)) // Current state reg being written
  val tpu2cpuState = Output(new PStateRegs)
  val cpu2tpuState = Input(new PStateRegs)
  val rfile = new Bundle {
    val rd = Flipped(new RFileIO.RDPort(cfg.NB_THREADS))
    val wr = Flipped(new RFileIO.WRPort(cfg.NB_THREADS))
  }

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
class TransplantUnitHostIO(implicit val cfg: ProcConfig) extends Bundle {
  val fire = Input(ValidTag(cfg.TAG_T))
  val done = Output(ValidTag(cfg.TAG_T))
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
class TransplantUnitCPUIO(implicit val cfg: ProcConfig) extends Bundle {
  val flush = Output(ValidTag(cfg.TAG_T))
  val fire = Output(ValidTag(cfg.TAG_T))
  val freeze = Output(ValidTag(cfg.TAG_T))
  val done = Input(ValidTag(cfg.TAG_T))
}

class TransplantUnitLegacy(implicit val cfg: ProcConfig) extends Module {
  val io = IO(new TransplantUnitIO)

  val bramOFFST = RegInit(0.U(log2Ceil(ARCH_MAX_OFFST).W))
  val bramOut = WireInit(io.stateBRAM.DO)

  // Only port rs1 of RFILE is used
  val regDataIn = io.rfile.rd.port(0).data
  val regAddr = bramOFFST // First 32 word are registers
  io.rfile.rd.port(0).addr := regAddr
  io.rfile.rd.port(1).addr := 0.U

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
      }.elsewhen(bramOFFST === (ARCH_PSTATE_OFFST - 1).U) {
        stateRegType := r_NZCV
      }.elsewhen(bramOFFST === (ARCH_MAX_OFFST - 1).U) {
        stateRegType := r_DONE
      }.elsewhen(stateRegType === r_DONE) {
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
    io.stateBRAM.DI := Cat(0.U, io.cpu2tpuState.NZCV(3, 0))
  }.otherwise {
    io.stateBRAM.DI := 0.U
  }

  // One cycle delay from BRAM read
  io.rfile.wr.en := RegNext(stateDir === s_BRAM2CPU && stateRegType === r_XREGS)
  io.rfile.wr.addr := RegNext(regAddr)
  io.rfile.wr.data := bramOut

  io.tpu2cpuState.PC := bramOut
  io.tpu2cpuState.SP := bramOut
  io.tpu2cpuState.NZCV := PStateGet_NZCV(bramOut)
  // One cycle delay from BRAM read
  io.tpu2cpuStateReg.bits := RegNext(stateRegType)
  io.tpu2cpuStateReg.valid := RegNext(stateDir === s_BRAM2CPU)

  // Signals come with 1 cycle delay as cmd signals start when State machine is back to IDLE
  val freezeTag1D = RegNext(freezeTag)
  io.rfile.rd.tag := freezeTag1D
  io.rfile.wr.tag := freezeTag1D
  io.tpu2cpu.fire.valid := RegNext(fireSig)
  io.tpu2cpu.fire.tag := freezeTag1D
  io.tpu2cpu.freeze.valid := freeze
  io.tpu2cpu.freeze.tag := freezeTag
  io.tpu2cpu.flush.valid := RegNext(flushSig)
  io.tpu2cpu.flush.tag := freezeTag1D
  io.host2tpu.done.valid := RegNext(doneSig)
  io.host2tpu.done.tag := freezeTag1D
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
