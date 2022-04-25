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

object TransplantConsts {
  // These constants are shared with `fpga_interface.h`
  val TRANS_REG_OFFST_PENDING          = (0)
  val TRANS_REG_OFFST_FREE_PENDING     = (0)
  val TRANS_REG_OFFST_START            = (1)
  val TRANS_REG_OFFST_STOP_CPU         = (2)
  val TRANS_REG_OFFST_FORCE_TRANSPLANT = (3)
}

import armflex.TransplantConsts._

class TransplantUnit(thidN: Int) extends Module {
  val cpu2trans = IO(new TransplantIO.CPU2Trans(thidN))
  val trans2cpu = IO(new TransplantIO.Trans2CPU(thidN))

  // Wait, in this case, at most 32 threads is supported?
  assert(thidN <= 32)
  val uCSR = Module(new CSR(32, 4))
  val S_CSR = IO(Flipped(uCSR.io.bus.cloneType))
  S_CSR <> uCSR.io.bus

  // uCSR[0]: Check whether a thread has its state in the TBRAM and requiring a transplant back.
  val wCPU2TransDoneMask = WireInit(0.U(32.W))
  SetCSR(wCPU2TransDoneMask, uCSR.io.csr(TRANS_REG_OFFST_PENDING), 32)

  // uCSR[1]: Set to trigger an unpack of the architecture state to the pipeline (BRAM to CPU) and restart the thread.
  val wB2CDoneMask = WireInit(0.U(32.W))
  val rUnpackRequest = ClearCSR(wB2CDoneMask, uCSR.io.csr(TRANS_REG_OFFST_START), 32)

  // uCSR[2]: Set to stop a thread.
  val wStoppedThreadMask = WireInit(0.U)
  when(cpu2trans.doneCPU.valid) {
    wStoppedThreadMask := 1.U << cpu2trans.doneCPU.tag
  }

  val rStopCPURequest = ClearCSR(wStoppedThreadMask, uCSR.io.csr(TRANS_REG_OFFST_STOP_CPU), 32)
  cpu2trans.stopCPU := rStopCPURequest

  // uCSR[3]: Set to enforce a thread to transplant back.
  val rForceTransplantRequest = ClearCSR("h_ffff_ffff".U, uCSR.io.csr(TRANS_REG_OFFST_FORCE_TRANSPLANT), 32)
  cpu2trans.forceTransplant := rForceTransplantRequest

  // val mem2trans = IO(new TransplantIO.Mem2Trans(thidN))

  private val rCpu2TransPending = RegInit(0.U(thidN.W))
  private val wCpu2TransBeingHandled = WireInit(false.B)
  // At present lower thread always has the highest priority for transplanting. 
  private val wSelectedCpu2TransReq = PriorityEncoder(rCpu2TransPending)
  private val wCpu2TransHandledReq = WireInit(wCpu2TransBeingHandled.asUInt << wSelectedCpu2TransReq) // always clear the first one. 

  // All possible sources that trigger packing a state.
  // private val wCpu2TransInstFault = Mux(mem2trans.instFault.valid, 1.U << mem2trans.instFault.tag, 0.U)
  // private val wCpu2TransDataFault = Mux(mem2trans.dataFault.valid, 1.U << mem2trans.dataFault.tag, 0.U)
  private val wCpu2TransCpuTrans = Mux(cpu2trans.doneCPU.valid, 1.U << cpu2trans.doneCPU.tag, 0.U)
  // Aggregate all the sources
  private val wCpu2TransInsert = WireInit(wCpu2TransCpuTrans)

  // The final pending register will be new inserted + force - handled.
  rCpu2TransPending := (rCpu2TransPending & ~wCpu2TransHandledReq) | wCpu2TransInsert | rForceTransplantRequest

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

  switch(rSyncState){
    is(sTIdle){
      when(rUnpackRequest =/= 0.U){
        rSyncState := sTSyncingB2CXReg
        rCurrentSyncReg := 0.U
        rSyncThread := PriorityEncoder(rUnpackRequest)
      }.elsewhen(rCpu2TransPending =/= 0.U){
        rSyncState := sTSyncingC2BPState
        rCurrentSyncReg := 0.U
        rSyncThread := wSelectedCpu2TransReq
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
      // Clear the CSR[1] when done
      wB2CDoneMask := 1.U << rSyncThread
      rSyncState := sTIdle 
    }
    is(sTSyncingC2BPState) { 
      // Set CSR[] when done
      wCPU2TransDoneMask := 1.U << rSyncThread
      rSyncState := sTIdle 
    }
  }

  // The CPU2BRAM request will be handed when the syncing unit is idle, and there is no request from the BRAM2CPU.
  wCpu2TransBeingHandled := rSyncState === sTIdle && rUnpackRequest === 0.U && rCpu2TransPending =/= 0.U
  
  // ---------------------- CPU2BRAM -----------------------
  // ----------- CPU2BRAM --------
  // - determine the write request. The data is write into tht TBRAM. 
  uTransplantBRAM.ports.wr.thid := rSyncThread
  uTransplantBRAM.ports.wr.pstate.req.bits.state := cpu2trans.pstate
  uTransplantBRAM.ports.wr.pstate.req.valid := rSyncState === sTSyncingC2BPState

  // Listen to the pipeline write request and update register inside
  uTransplantBRAM.ports.wr.xreg.req.bits.regIdx := cpu2trans.rfile_wr.addr
  uTransplantBRAM.ports.wr.xreg.req.bits.data := cpu2trans.rfile_wr.data
  uTransplantBRAM.ports.wr.xreg.req.valid := cpu2trans.rfile_wr.en

  // stall the pipeline when we decide to write the PState.
  cpu2trans.stallPipeline := uTransplantBRAM.ports.wr.pstate.req.valid

  // ----------- BRAM2CPU --------
  // BRAM2CPU Logic
  uTransplantBRAM.ports.rd.thid := rSyncThread
  // - determine the read port. It's from the TBRAM
  uTransplantBRAM.ports.rd.xreg.req.bits.regIdx := rCurrentSyncReg
  uTransplantBRAM.ports.rd.xreg.req.valid := rSyncState === sTSyncingB2CXReg

  // - Which is the write port?
  // For normal registers, it's pretty easy to update them.
  // Make the data and address aligned.
  trans2cpu.rfile_wr.en := RegNext(rSyncState === sTSyncingB2CXReg)
  trans2cpu.rfile_wr.addr := RegNext(rCurrentSyncReg)
  trans2cpu.rfile_wr.data := uTransplantBRAM.ports.rd.xreg.resp
  trans2cpu.rfile_wr.tag := rSyncThread

  // For the pstate, we have a special read port.
  uTransplantBRAM.ports.rd.pstate.req.valid := rSyncState === sTSyncingB2CPState
  trans2cpu.pstate.valid := RegNext(rSyncState === sTSyncingB2CPState)
  trans2cpu.pstate.bits := uTransplantBRAM.ports.rd.pstate.resp
  
  // During the synchronization of ArchState (or the BRAM port is occupied), the pipeline is stalled.
  trans2cpu.stallPipeline := trans2cpu.pstate.valid || trans2cpu.rfile_wr.en 
          // RegNext(rSyncState === sTSyncingB2CPState) || RegNext(rSyncState === sTSyncingB2CXReg)

  // Restart a thread after transfer is done.
  trans2cpu.thread := rSyncThread
  trans2cpu.start := RegNext(rSyncState === sTSyncingB2CPState)


  if (true) { // TODO Conditional assertions
    when(rSyncState === sTSyncingB2CXReg) {
      assert(uTransplantBRAM.ports.rd.xreg.req.ready, "BRAM must be ready to receive transactions when interacting with it: pushing XRegs")
    }
    when(rSyncState === sTSyncingB2CPState) {
      assert(uTransplantBRAM.ports.rd.pstate.req.ready, "BRAM must be ready to receive transactions when interacting with it: pushing PState")
    }
    when(rSyncState ===sTSyncingC2BPState) {
      assert(uTransplantBRAM.ports.wr.pstate.req.ready, "BRAM must be ready to receive transactions when interacting with it: pulling PState")
    }
  }
}


object TransplantUnitVerilogEmitter extends App {
  import chisel3.stage.ChiselStage
  (new ChiselStage).emitVerilog(new TransplantUnit(16))
}
