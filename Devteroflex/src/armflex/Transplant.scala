package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._

import armflex.util._
import armflex.PStateConsts._

import antmicro.Bus.AXI4
import antmicro.CSR._

object TransplantIO extends Bundle {
  class Trans2CPU(val thidN: Int) extends Bundle { // Push state back to CPU
    // This port is used for state synchronization.
    val rfile_wr = Flipped(new RFileIO.WRPort(thidN))
    // This port is used to commit pstate. (Using this.thread to index thread.)
    val pstate = Output(Valid(new PStateRegs))

    // This port is used to restart a thread?
    val start = Output(Valid(UInt(log2Ceil(thidN).W)))

    // If this signal is true, the pipeline should be stalled, and no instruction will move forward.
    val stallPipeline = Output(Bool())
    val busyTrans2Cpu = Output(Bool())
  }
  class CPU2Trans(val thidN: Int) extends Bundle { // Get state from the CPU
    // This port is listened to update the state accordingly.
    val rfile_wr = new RFileIO.WRPort(thidN)
    // This port can be used to fetch the pstate. Whose pstate???
    val pstate = Input(new PStateRegs)
    // This port indicates that which thread is requesting transplant.
    val doneCPU = Input(ValidTag(thidN))
    // This port transfer the singlestep command to the CPU.
    val stopCPU = Output(UInt(thidN.W))

    // Mask to notify the CPU that a force transplant request is detected. Set its arch state to exception now.
    val forceTransplant = Output(UInt(thidN.W))

    // If this signal is true, the pipeline should be stalled, and no instruction will move forward.
    // It will become true when there is a structural hazard and we cannot commit the state.
    val stallPipeline = Output(Bool())
    val busyCpu2Trans = Output(Bool())
  }
  class Mem2Trans(val thidN: Int) extends Bundle {
    val instFault = Input(ValidTag(thidN))
    val dataFault = Input(ValidTag(thidN))
  }
}

object TransplantConsts {
  // These constants are shared with `fpga_interface.h`
  val TRANS_REG_OFFST_PENDING          = (0)
  val TRANS_REG_OFFST_FREE_PENDING     = (0)
  val TRANS_REG_OFFST_START            = (1)
  val TRANS_REG_OFFST_STOP_CPU         = (2)
  val TRANS_REG_OFFST_FORCE_TRANSPLANT = (3)
  val TRANS_REG_OFFST_WAITING          = (4)
  val TRANS_REG_TOTAL_REGS             = (5)
}

import armflex.TransplantConsts._

class TransplantUnit(thidN: Int) extends Module {
  private val uCpu2TransBRAM = Module(new Cpu2TransBramUnit(thidN))
  private val uTransBram2HostUnit = Module(new TransBram2HostUnit(thidN))
  uCpu2TransBRAM.transBram2Cpu <> uTransBram2HostUnit.transBram2Cpu

  val S_CSR = IO(Flipped(uCpu2TransBRAM.S_CSR))
  S_CSR <> uCpu2TransBRAM.S_CSR
  val cpu2trans = IO(uCpu2TransBRAM.cpu2trans)
  val trans2cpu = IO(uCpu2TransBRAM.trans2cpu)
  cpu2trans <> uCpu2TransBRAM.cpu2trans
  trans2cpu <> uCpu2TransBRAM.trans2cpu

  val S_AXI = IO(Flipped(uTransBram2HostUnit.S_AXI.cloneType))
  S_AXI <> uTransBram2HostUnit.S_AXI
}

class Cpu2TransBramUnit(thidN: Int) extends Module {
  val cpu2trans = IO(new TransplantIO.CPU2Trans(thidN))
  val trans2cpu = IO(new TransplantIO.Trans2CPU(thidN))

  val transBram2Cpu = IO(Flipped(new TransBram2HostUnitIO.TransBRAM2CpuIO(thidN)))

  // Wait, in this case, at most 32 threads is supported?
  assert(thidN <= 32)
  val uCSR = Module(new CSR(32, TRANS_REG_TOTAL_REGS))
  val S_CSR = IO(Flipped(uCSR.io.bus.cloneType))
  S_CSR <> uCSR.io.bus

  val cpuDoneMask = WireInit(Mux(cpu2trans.doneCPU.valid, 1.U << cpu2trans.doneCPU.tag, 0.U))

  // uCSR[0]: Check whether a thread has its state in the TBRAM and requiring a transplant back.
  val (rTrans2HostPending, setTrans2Host, clearTrans2Host) = SetClearReg(thidN)
  StatusCSR(rTrans2HostPending, uCSR.io.csr(TRANS_REG_OFFST_PENDING), thidN)
  clearTrans2Host := transBram2Cpu.ctrl.clearTrans2Host

  // uCSR[1]: Set to trigger an unpack of the architecture state to the pipeline (BRAM to CPU) and restart the thread.
  val (rTrans2CpuPending, setTrans2Cpu, clearTrans2Cpu) = SetClearReg(thidN)
  setTrans2Cpu := transBram2Cpu.ctrl.setTrans2Cpu

  // uCSR[2]: Stop executing the CPU after next instruction
  val (rStopCPURequest, setStopCpu, clearStopCpu) = SetClearReg(thidN)
  // Register was changed to read only as now PStateFlags contains execution mode
  StatusCSR(rStopCPURequest, uCSR.io.csr(TRANS_REG_OFFST_STOP_CPU), thidN)
  val setStopCpuHost = PulseCSR(uCSR.io.csr(TRANS_REG_OFFST_STOP_CPU), thidN)
  val setStopCpuTrans = WireInit(setStopCpu.cloneType, 0.U)
  setStopCpu := setStopCpuHost | setStopCpuTrans
  clearStopCpu := cpuDoneMask
  cpu2trans.stopCPU := rStopCPURequest

  // uCSR[3]: Set to enforce a thread to transplant back.
  val setCpu2TransHostForce = PulseCSR(uCSR.io.csr(TRANS_REG_OFFST_FORCE_TRANSPLANT), 32)
  cpu2trans.forceTransplant := setCpu2TransHostForce

  // Pending transplants from Cpu2Trans
  private val (rCpu2TransPending, setCpu2Trans, clearCpu2Trans) = SetClearReg(thidN)
  // All possible sources that trigger packing a state.
  private val setCpu2TransCpuDone = cpuDoneMask
  setCpu2Trans := setCpu2TransCpuDone | setCpu2TransHostForce


  private val startThread = WireInit(0.U(log2Ceil(thidN).W))
  private val sendStart = WireInit(false.B)
  // uCSR[4]: Transplant waiting for start signal
  val (rStartingCpu, setStartingCpu, clearStartingCpu) = SetClearReg(thidN)
  val (rWaitStartCpu, setWaitStartCpu, clearWaitStartCpu) = SetClearReg(thidN)
  StatusCSR(rWaitStartCpu, uCSR.io.csr(TRANS_REG_OFFST_WAITING), thidN)
  // uCSR[0]: Start a waiting thread
  setStartingCpu := PulseCSR(uCSR.io.csr(TRANS_REG_OFFST_START), thidN) & rWaitStartCpu

  // The state machine of copying data.
  // BRAM to CPU (B2C): Copy all registers, and then copy the PState
  // CPU to BRAM (C2B): Just copy the PState
  val sTIdle :: sTSyncingB2CXReg :: sTSyncingB2CPState :: sTSyncingC2BPState :: Nil = Enum(4)

  val rSyncState = RegInit(sTIdle)
  val rCurrentSyncReg = RegInit(0.U(5.W)) // 64 places most for each thread.
  val rSyncThread = RegInit(0.U(log2Ceil(thidN).W))

  // Control starting of thread
  private val doneTransplantNextCycle = WireInit(false.B)
  private val doneTransplant = RegNext(doneTransplantNextCycle)
  when(doneTransplant) {
    startThread := rSyncThread
    when(trans2cpu.pstate.bits.flags.execMode === PSTATE_FLAGS_EXECUTE_WAIT.U) {
      setWaitStartCpu := 1.U << startThread
    }.elsewhen(trans2cpu.pstate.bits.flags.execMode === PSTATE_FLAGS_EXECUTE_NORMAL.U) {
      sendStart := true.B
      clearStartingCpu := 1.U << startThread
      clearWaitStartCpu := 1.U << startThread
    }.elsewhen(trans2cpu.pstate.bits.flags.execMode === PSTATE_FLAGS_EXECUTE_SINGLESTEP.U) {
      sendStart := true.B
      setStopCpuTrans := 1.U << startThread
      clearStartingCpu := 1.U << startThread
      clearWaitStartCpu := 1.U << startThread
    }
  }.elsewhen(rStartingCpu.orR) {
    startThread := PriorityEncoder(rStartingCpu)
    clearStartingCpu := 1.U << startThread
    clearWaitStartCpu := 1.U << startThread
    sendStart := true.B
  }

  switch(rSyncState){
    is(sTIdle){
      when(rTrans2CpuPending =/= 0.U){
        rSyncState := sTSyncingB2CXReg
        rCurrentSyncReg := 0.U
        rSyncThread := PriorityEncoder(rTrans2CpuPending)
      }.elsewhen(rCpu2TransPending =/= 0.U){
        // Skip reading registers as register are already available in BRAM
        rSyncState := sTSyncingC2BPState
        rCurrentSyncReg := 0.U
        rSyncThread := PriorityEncoder(rCpu2TransPending)
      }
    }

    // Can only write a single register at a time
    is(sTSyncingB2CXReg){
      rCurrentSyncReg := rCurrentSyncReg + 1.U
      when(rCurrentSyncReg === (REG_N - 1).U) {
        rSyncState := sTSyncingB2CPState
      }
    }

    // PState fits in a single 512-bit block transaction 
    is(sTSyncingB2CPState) { 
      // Clear Trans2Cpu
      clearTrans2Cpu := 1.U << rSyncThread
      doneTransplantNextCycle := true.B
      rSyncState := sTIdle 
    }

    is(sTSyncingC2BPState) { 
      // Clear Cpu2Trans, Set Trans2Host
      clearCpu2Trans := 1.U << rSyncThread
      setTrans2Host := 1.U << rSyncThread
      rSyncState := sTIdle 
    }
  }

  // ---------------------- CPU2BRAM -----------------------
  // ----------- CPU2BRAM --------
  // - determine the write request. The data is write into tht TBRAM. 
  transBram2Cpu.wr.thid := rSyncThread
  transBram2Cpu.wr.pstate.req.bits.state := cpu2trans.pstate
  transBram2Cpu.wr.pstate.req.valid := rSyncState === sTSyncingC2BPState

  // Listen to the pipeline write request and update register inside
  transBram2Cpu.wr.xreg.req.bits.regIdx := cpu2trans.rfile_wr.addr
  transBram2Cpu.wr.xreg.req.bits.data := cpu2trans.rfile_wr.data
  transBram2Cpu.wr.xreg.req.valid := cpu2trans.rfile_wr.en

  // stall the pipeline when we decide to write the PState.
  cpu2trans.stallPipeline := transBram2Cpu.wr.pstate.req.valid
  cpu2trans.busyCpu2Trans := rSyncState === sTSyncingC2BPState

  // ----------- BRAM2CPU --------
  // BRAM2CPU Logic
  transBram2Cpu.rd.thid := rSyncThread
  // - determine the read port. It's from the TBRAM
  transBram2Cpu.rd.xreg.req.bits.regIdx := rCurrentSyncReg
  transBram2Cpu.rd.xreg.req.valid := rSyncState === sTSyncingB2CXReg

  // - Which is the write port?
  // For normal registers, it's pretty easy to update them.
  // Make the data and address aligned.
  trans2cpu.rfile_wr.en := RegNext(rSyncState === sTSyncingB2CXReg)
  trans2cpu.rfile_wr.addr := RegNext(rCurrentSyncReg)
  trans2cpu.rfile_wr.data := transBram2Cpu.rd.xreg.resp
  trans2cpu.rfile_wr.tag := rSyncThread

  // For the pstate, we have a special read port.
  transBram2Cpu.rd.pstate.req.valid := rSyncState === sTSyncingB2CPState
  trans2cpu.pstate.valid := RegNext(rSyncState === sTSyncingB2CPState)
  trans2cpu.pstate.bits := transBram2Cpu.rd.pstate.resp
  
  // During the synchronization of ArchState (or the BRAM port is occupied), the pipeline is stalled.
  trans2cpu.stallPipeline := trans2cpu.pstate.valid || trans2cpu.rfile_wr.en 
          // RegNext(rSyncState === sTSyncingB2CPState) || RegNext(rSyncState === sTSyncingB2CXReg)
  trans2cpu.busyTrans2Cpu := rSyncState === sTSyncingB2CXReg || rSyncState === sTSyncingB2CPState

  // Restart a thread after transfer is done.
  trans2cpu.start.bits := startThread
  trans2cpu.start.valid := sendStart


  if (true) { // TODO Conditional assertions
    when(rSyncState === sTSyncingB2CXReg) {
      assert(transBram2Cpu.rd.xreg.req.ready, "BRAM must be ready to receive transactions when interacting with it: pushing XRegs")
    }
    when(rSyncState === sTSyncingB2CPState) {
      assert(transBram2Cpu.rd.pstate.req.ready, "BRAM must be ready to receive transactions when interacting with it: pushing PState")
    }
    when(rSyncState ===sTSyncingC2BPState) {
      assert(transBram2Cpu.wr.pstate.req.ready, "BRAM must be ready to receive transactions when interacting with it: pulling PState")
    }
    when(doneTransplant) {
      assert(trans2cpu.pstate.valid, "Pushing PState should be the cycle that starts executing")
    }
  }
}


object TransplantUnitVerilogEmitter extends App {
  import chisel3.stage.ChiselStage
  (new ChiselStage).emitVerilog(new TransplantUnit(16))
}
