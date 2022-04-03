package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._

import armflex.util._

import antmicro.Bus.AXI4
import antmicro.CSR.CSR
import antmicro.CSR.SetCSR
import antmicro.CSR.ClearCSR

object TransplantIO extends Bundle {
  class Trans2CPU(val thidN: Int) extends Bundle { // Push state back to CPU
    val thread = Output(UInt(log2Ceil(thidN).W))
    // This port is used for state synchronization.
    val rfile_wr = Flipped(new RFileIO.WRPort(thidN))
    // This port is used to commit pstate. (Using this.thread to index thread.)
    val pstate = Output(Valid(new PStateRegs))

    // This port is used to restart a thread?
    val start = Output(Bool())

    // If this signal is true, the pipeline should be stalled, and no instruction will move forward.
    val stallPipeline = Output(Bool())
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
  }
  class Mem2Trans(val thidN: Int) extends Bundle {
    val instFault = Input(ValidTag(thidN))
    val dataFault = Input(ValidTag(thidN))
  }
}
class TransplantUnit(thidN: Int) extends Module {
  val cpu2trans = IO(new TransplantIO.CPU2Trans(thidN))
  val trans2cpu = IO(new TransplantIO.Trans2CPU(thidN))

  // Wait, in this case, at most 32 threads is supported?
  assert(thidN <= 32)
  val uCSR = Module(new CSR(32, 4))
  val S_CSR = IO(Flipped(uCSR.io.bus.cloneType))
  S_CSR <> uCSR.io.bus

  // uCSR[0]: Check whether a thread has its state in the TBRAM and requiring a transplant back.
  val wCPU2TransDoneMask = Wire(UInt(32.W))
  SetCSR(wCPU2TransDoneMask, uCSR.io.csr(0), 32)

  // uCSR[1]: Set to trigger an unpack of the architecture state to the pipeline (BRAM to CPU) and restart the thread.
  val wB2CDoneMask = Wire(UInt(32.W))
  val rUnpackRequest = ClearCSR(wB2CDoneMask, uCSR.io.csr(1), 32)

  // uCSR[2]: Set to stop a thread.
  val wStoppedThreadMask = WireInit(Mux(
    cpu2trans.doneCPU.valid,
    1.U << cpu2trans.doneCPU.tag,
    0.U
  ))
  val rStopCPURequest = ClearCSR(wStoppedThreadMask, uCSR.io.csr(2), 32)
  cpu2trans.stopCPU := rStopCPURequest

  // uCSR[3]: Set to enforce a thread to transplant back.
  val rForceTransplantRequest = ClearCSR("h_ffff_ffff".U, uCSR.io.csr(3), 32)
  cpu2trans.forceTransplant := rForceTransplantRequest

  val mem2trans = IO(new TransplantIO.Mem2Trans(thidN))

  private val rCpu2transPending = RegInit(0.U(thidN.W))
  private val wCpu2transBeingHandled = WireInit(false.B)
  // At present lower thread always has the highest priority for transplanting. 
  private val wSelectedC2TRequest = PriorityEncoder(rCpu2transPending)
  private val wCpu2transHandledReq = WireInit(wCpu2transBeingHandled.asUInt << wSelectedC2TRequest) // always clear the first one. 

  // All possible sources that trigger packing a state.
  private val wCpu2transInstFault = Mux(mem2trans.instFault.valid, 1.U << mem2trans.instFault.tag, 0.U)
  private val wCpu2transDataFault = Mux(mem2trans.dataFault.valid, 1.U << mem2trans.dataFault.tag, 0.U)
  private val wCpu2transCpuTrans = Mux(cpu2trans.doneCPU.valid, 1.U << cpu2trans.doneCPU.tag, 0.U)
  // Aggregate all the sources
  private val wCpu2transInsert = WireInit(wCpu2transCpuTrans | wCpu2transDataFault | wCpu2transInstFault)

  // The final pending register will be new inserted + force - handled.
  rCpu2transPending := (rCpu2transPending & ~wCpu2transHandledReq) | wCpu2transInsert | rForceTransplantRequest

  // The state machine of copying data.
  // BRAM to CPU (B2C): Copy all registers, and then copy the PState
  // CPU to BRAM (C2B): Just copy the PState
  val sTIdle :: sTSyncingB2CXReg :: sTSyncingB2CPState :: sTSyncingC2BPState :: Nil = Enum(4)

  val rSyncState = RegInit(sTIdle)
  val rCurrentSyncReg = RegInit(0.U(5.W)) // 64 places most for each thread.
  val rSyncThread = RegInit(0.U(log2Ceil(thidN).W))

  private val uTransplantBRAM = Module(new TransplantBRAM(thidN))

  val S_AXI = IO(Flipped(new AXI4(16, 512)))
  S_AXI <> uTransplantBRAM.S_AXI

  assert(!(uTransplantBRAM.iReadRequest.valid && uTransplantBRAM.iWriteRequest.valid), "It's impossible to see two request happens at the same time!")

  switch(rSyncState){
    is(sTIdle){
      when(rUnpackRequest =/= 0.U){
        rSyncState := sTSyncingB2CXReg
        rCurrentSyncReg := 0.U
        rSyncThread := PriorityEncoder(rUnpackRequest)
      }.elsewhen(rCpu2transPending =/= 0.U){
        rSyncState := sTSyncingC2BPState
        rCurrentSyncReg := 0.U
        rSyncThread := wSelectedC2TRequest
      }
    }

    is(sTSyncingB2CXReg){
      rCurrentSyncReg := rCurrentSyncReg + 1.U
      rSyncState := Mux(
        rCurrentSyncReg === (REG_N - 1).U,
        sTSyncingB2CPState,
        sTSyncingB2CXReg
      )
      assert(uTransplantBRAM.iReadRequest.ready)
    }

    is(sTSyncingB2CPState){
      rSyncState := sTIdle
      assert(uTransplantBRAM.iReadPStateRequest.ready)
    }

    is(sTSyncingC2BPState){
      rSyncState := sTIdle
      assert(uTransplantBRAM.iPstateWriteRequest.ready)
    }
  }

  // The CPU2BRAM request will be handed when the syncing unit is idle, and there is no request from the BRAM2CPU.
  wCpu2transBeingHandled := rSyncState === sTIdle && rUnpackRequest === 0.U && rCpu2transPending =/= 0.U
  
  // CPU2BRAM Logic.
  // - determine the write request. The data is write into tht TBRAM. 
  uTransplantBRAM.iPstateWriteRequest.bits.threadID := rSyncThread
  uTransplantBRAM.iPstateWriteRequest.bits.state := cpu2trans.pstate
  uTransplantBRAM.iPstateWriteRequest.valid := rSyncState === sTSyncingC2BPState

  // stall the pipeline when we decide to write the PState.
  cpu2trans.stallPipeline := uTransplantBRAM.iPstateWriteRequest.valid

  // When CPU2BRAM is done, we need to notify the host that data copies is finished.
  wCPU2TransDoneMask := Mux(
    RegNext(uTransplantBRAM.iPstateWriteRequest.valid), 
    1.U << rSyncThread, 
    0.U
  )

  // BRAM2CPU Logic
  // - determine the read port. It's from the TBRAM
  uTransplantBRAM.iReadRequest.bits.threadID := rSyncThread
  uTransplantBRAM.iReadRequest.bits.registerIndex := rCurrentSyncReg
  uTransplantBRAM.iReadRequest.valid := rSyncState === sTSyncingB2CXReg

  // - Which is the write port?
  // First, make the data and address aligned.
  val rB2CRegIndexAlignedWithRead = Reg(Valid(UInt(log2Ceil(REG_N).W)))
  rB2CRegIndexAlignedWithRead.bits := rCurrentSyncReg
  rB2CRegIndexAlignedWithRead.valid := rSyncState === sTSyncingB2CXReg

  // For normal registers, it's pretty easy to update them.
  trans2cpu.rfile_wr.en := rB2CRegIndexAlignedWithRead.valid
  trans2cpu.rfile_wr.addr := rB2CRegIndexAlignedWithRead.bits
  trans2cpu.rfile_wr.data := uTransplantBRAM.oReadReply
  trans2cpu.rfile_wr.tag := rSyncThread

  // For the pstate, we have a special read port.
  uTransplantBRAM.iReadPStateRequest.bits := rSyncThread
  uTransplantBRAM.iReadPStateRequest.valid := rSyncState === sTSyncingB2CPState
  trans2cpu.pstate.valid := RegNext(rSyncState === sTSyncingB2CPState)
  trans2cpu.pstate.bits := uTransplantBRAM.oReadPStateReply
  
  // During the synchronization of ArchState, the pipeline is stalled.
  trans2cpu.stallPipeline := trans2cpu.pstate.valid || trans2cpu.rfile_wr.en

  // Restart a thread after transfer is done.
  trans2cpu.thread := rSyncThread
  trans2cpu.start := RegNext(rSyncState === sTSyncingB2CPState)

  // Also clear the CSR[1]
  wB2CDoneMask := WireInit(Mux(
    rSyncState === sTSyncingB2CPState,
    1.U << rSyncThread,
    0.U
  ))

  // Listen to the pipeline write request and update register inside
  uTransplantBRAM.iWriteRequest.bits.registerIndex := cpu2trans.rfile_wr.addr
  uTransplantBRAM.iWriteRequest.bits.threadID := cpu2trans.rfile_wr.tag
  uTransplantBRAM.iWriteRequest.bits.value := cpu2trans.rfile_wr.data
  uTransplantBRAM.iWriteRequest.valid := cpu2trans.rfile_wr.en
}


object TransplantUnitVerilogEmitter extends App {
  import chisel3.stage.ChiselStage
  (new ChiselStage).emitVerilog(new TransplantUnit(16))
}
