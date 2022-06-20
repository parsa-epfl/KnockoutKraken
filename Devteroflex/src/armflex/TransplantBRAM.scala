package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._

import antmicro.Bus.AXI4
import armflex.util.BRAM
import armflex.util.BRAMParams

import armflex.PStateConsts._
import armflex.TransplantConsts._
import armflex.TransBram2HostUnitIO._

object TransBram2HostUnitIO {
  // Given that registers are 64-bit wide, but that 
  // a bram block is 512-bits we align with the block granularity
  def bramAlignAddr(thid: UInt, regIdx: UInt) = Cat(thid, regIdx >> 3)
  def bramAlignMask(regIdx: UInt) = (0xFF.U(8.W)) << ((regIdx(2, 0)) * 8.U)
  def bramAlignData(regIdx: UInt, data: UInt) = data << ((regIdx(2, 0) * 64.U))

  def bramBlockGetReg(block: UInt, regIdx: Int): UInt = block((regIdx + 1)*64 - 1, regIdx*64)
  def bramBlockUnpackPState(block: UInt): PStateRegs = {
    val pstate = Wire(new PStateRegs)
    pstate.PC := bramBlockGetReg(block, ARCH_PSTATE_PC_OFFST % regsPerBlock)
    pstate.flags := bramBlockGetReg(block, ARCH_PSTATE_FLAGS_OFFST % regsPerBlock).asTypeOf(new PStateFlags)
    val asidReg = bramBlockGetReg(block, ARCH_PSTATE_ASID_OFFST % regsPerBlock)
    pstate.asid := asidReg(31, 0)
    pstate.asid_unused := asidReg(63, 32)
    pstate.icount := bramBlockGetReg(block, ARCH_PSTATE_ICOUNT_OFFST % regsPerBlock)
    val icountRegs = bramBlockGetReg(block, ARCH_PSTATE_ICOUNTREGS_OFFST % regsPerBlock)
    pstate.icountExecuted := icountRegs(31, 0) // 34[31:0] -> icountExecuted
    pstate.icountBudget := icountRegs(63, 32) // 34[63:32] -> icountBudget
    pstate
  }
  def bramBlockPackPState(pstate: PStateRegs): UInt = {
    val block = Wire(Vec(regsPerBlock, DATA_T))
    block := DontCare
    block(ARCH_PSTATE_PC_OFFST % regsPerBlock) := pstate.PC
    block(ARCH_PSTATE_FLAGS_OFFST % regsPerBlock) := pstate.flags.asUInt
    block(ARCH_PSTATE_ASID_OFFST % regsPerBlock) := Cat(pstate.asid_unused, pstate.asid)
    block(ARCH_PSTATE_ICOUNT_OFFST % regsPerBlock) := pstate.icount
    block(ARCH_PSTATE_ICOUNTREGS_OFFST % regsPerBlock) := Cat(pstate.icountBudget, pstate.icountExecuted)
    return block.asUInt()
  }

  class RdPort(thidN: Int) extends Bundle {
    val thid = Input(UInt(log2Ceil(thidN).W))
    val xreg = new Bundle {
      val req = Flipped(Decoupled(new RdReq))
      val resp = Output(DATA_T)
    }
    val pstate = new Bundle {
      val req = Flipped(Decoupled())
      val resp = Output(new PStateRegs)
    }
  }

  class RdReq extends Bundle {
    val regIdx = UInt(log2Ceil(TRANS_STATE_THID_MAX_REGS).W)
  }

  class WrPort(thidN: Int) extends Bundle {
    val thid = Input(UInt(log2Ceil(thidN).W))
    val xreg = new Bundle {
      val req = Flipped(Decoupled(new WrReq))
    }
    val pstate = new Bundle {
      val req = Flipped(Decoupled(new WrPStateReq))
    }
  }

  class WrReq extends Bundle {
    val regIdx = UInt(log2Ceil(TRANS_STATE_THID_MAX_REGS).W)
    val data = DATA_T
  }

  class TransBRAM2CpuIO(thidN: Int) extends Bundle {
    val rd = new RdPort(thidN)
    val wr = new WrPort(thidN)
    val ctrl = new Trans2CpuCtrlIO(thidN)
  }

  // Use the 2nd BRAM ports to sync data with the CPU.
  // This port is for submitting the pstate. It has the highest priority.
  class WrPStateReq extends Bundle {
    val state = new PStateRegs()
  }

  class Trans2CpuCtrlIO(thidN: Int) extends Bundle {
    val clearTrans2Host = Output(UInt(thidN.W)) // Clear pending bit BRAM->HOST
    val setTrans2Cpu = Output(UInt(thidN.W)) // Raise transplant BRAM->CPU
  }

}

// This registers store the ArchState that should be sent to the host and update by the host.
// - Each thread has 32 64bit registers, and PC, the flag, and the stack pointer.
// - We allocate 512 byte (64*64bit) space for each thread. The first 256 bytes are for the x0 - x31 registers, and then one 64byte for PC + Flag + Stack, and the following position are preserved for future reuse.
// - At present we support at most 128 threads, so 16bit address at most. (Not sure if we have such a large space from the shell.)

class TransBram2HostUnit(thidN: Int) extends Module {
  assert(thidN <= 128)
  assert(DATA_SZ == 64)
  assert(REG_N == 32)
  val addrW = log2Ceil(thidN * TRANS_STATE_THID_MAX_BYTES)

  val S_AXI = IO(Flipped(new AXI4(addrW, BLOCK_SZ)))
  val sAXIIdle :: sAXIReading :: sAXIReadWaitComplete :: sAXIWriting :: sAXIWriteResponse :: Nil = Enum(5)

  val uBRAM = Module(new BRAM()(new BRAMParams(64, 8, 1024, "", false)))

  val transBram2Cpu = IO(new TransBRAM2CpuIO(thidN))
  val rdPort = transBram2Cpu.rd
  val wrPort = transBram2Cpu.wr

  // ---------------------- AXI -------------------------
 
  // Two AXI transactions use the same BRAM ports, and cannot happen at the same time.
  // the AXI contexts.
  val rAXIState = RegInit(sAXIIdle)
  // val rAXIID = RegInit(0.U(AXI4.idWidth.W))
  val rAXIAddr = RegInit(0.U(S_AXI.addrWidth.W))
  val rAXIAddrStep = RegInit(0.U(AXI4.sizeWidth.W))
  val rAXICounter = RegInit(0.U(AXI4.lenWidth.W))
  val rAXICounterEnd = RegInit(0.U(AXI4.lenWidth.W))

  val thid = RegInit(0.U(S_AXI.addrWidth.W))

  class axi_read_meta_info_t extends Bundle {
    // val id = UInt(AXI4.idWidth.W)
    val isLast = Bool()
  }

  val wAXIReadSync = Wire(Decoupled(new axi_read_meta_info_t))
  wAXIReadSync.bits.isLast := false.B

  val clearTrans2Host = WireInit(transBram2Cpu.ctrl.clearTrans2Host.cloneType, 0.U)
  val setTrans2Cpu = WireInit(transBram2Cpu.ctrl.setTrans2Cpu.cloneType, 0.U)
  transBram2Cpu.ctrl.clearTrans2Host := clearTrans2Host
  transBram2Cpu.ctrl.setTrans2Cpu := setTrans2Cpu

  switch(rAXIState){
    is(sAXIIdle){
      when(S_AXI.ar.fire){
        // rAXIID := S_AXI.ar.arid
        rAXIAddr := S_AXI.ar.araddr
        rAXICounter := 0.U
        rAXICounterEnd := S_AXI.ar.arlen
        rAXIAddrStep := S_AXI.ar.arsize
        // only increase burst is supported.
        assert(S_AXI.ar.arburst === 1.U)
        rAXIState := sAXIReading

        thid := S_AXI.ar.araddr(addrW-1, log2Ceil(TRANS_STATE_THID_MAX_BYTES))
      }.elsewhen(S_AXI.aw.fire){
        // rAXIID := S_AXI.aw.awid
        rAXIAddr := S_AXI.aw.awaddr
        rAXICounter := 0.U
        rAXICounterEnd := S_AXI.aw.awlen
        rAXIAddrStep := S_AXI.aw.awsize
        assert(S_AXI.aw.awburst === 1.U)
        rAXIState := sAXIWriting

        thid := S_AXI.aw.awaddr(addrW-1, log2Ceil(TRANS_STATE_THID_MAX_BYTES))
      }
    }
    is(sAXIReading){
      when(wAXIReadSync.fire){
        rAXIAddr := rAXIAddr + (1.U << (rAXIAddrStep))
        rAXICounter := rAXICounter + 1.U
        when(rAXICounter === rAXICounterEnd) {
          wAXIReadSync.bits.isLast := true.B
          rAXIState := sAXIReadWaitComplete
        }
      }
    }
    is(sAXIWriting){
      when(S_AXI.w.fire){
        rAXIAddr := rAXIAddr + (1.U << (rAXIAddrStep))
        rAXICounter := rAXICounter + 1.U
        when(rAXICounter === rAXICounterEnd){
          rAXIState := sAXIWriteResponse
        }
      }
    }
    is(sAXIReadWaitComplete){ // wait for the last read transactions to be complete
      when(S_AXI.r.fire){
        clearTrans2Host := 1.U << thid
        rAXIState := sAXIIdle
      }
    }
    is(sAXIWriteResponse){
      when(S_AXI.b.fire){
        setTrans2Cpu := 1.U << thid
        rAXIState := sAXIIdle
      }
    }
  }

  // BRAM port
  uBRAM.portA.ADDR := rAXIAddr >> log2Ceil(BLOCK_SZ/8)
  uBRAM.portA.DI := S_AXI.w.wdata
  when(rAXIState === sAXIReading){
    uBRAM.portA.EN := true.B
    uBRAM.portA.WE := 0.U
  }.elsewhen(rAXIState === sAXIWriting){
    uBRAM.portA.EN := S_AXI.w.fire
    uBRAM.portA.WE := S_AXI.w.wstrb
  }.otherwise {
    uBRAM.portA.EN := false.B
    uBRAM.portA.WE := 0.U
  }

  // BRAM sync
  // wAXIReadSync.bits.id := rAXIID
  wAXIReadSync.valid := rAXIState === sAXIReading

  // Sync data is valid together with the read output from the BRAM.
  val wAXIReadSyncOut = Queue(wAXIReadSync, 1, true);

  S_AXI.ar.arready := rAXIState === sAXIIdle

  S_AXI.r.rdata := uBRAM.portA.DO
  // S_AXI.r.rid := wAXIReadSyncOut.bits.id
  S_AXI.r.rlast := wAXIReadSyncOut.bits.isLast
  S_AXI.r.rresp := 0.U
  S_AXI.r.rvalid := wAXIReadSyncOut.valid
  wAXIReadSyncOut.ready := S_AXI.r.rready

  S_AXI.aw.awready := rAXIState === sAXIIdle && !S_AXI.ar.arvalid // write has lower priority.

  S_AXI.w.wready := rAXIState === sAXIWriting

  // S_AXI.b.bid := rAXIID
  S_AXI.b.bresp := 0.U
  S_AXI.b.bvalid := rAXIState === sAXIWriteResponse


  // ---------------------- CPU -------------------------

  // Register read port. It has the 2nd priority.
  val rRdBias = RegEnable(rdPort.xreg.req.bits.regIdx(2, 0), rdPort.xreg.req.fire)
  rdPort.xreg.req.ready := wrPort.pstate.req.ready && !wrPort.pstate.req.valid
  rdPort.pstate.req.ready := rdPort.xreg.req.ready && !rdPort.xreg.req.valid
  // This port is for the normal register writing.
  wrPort.xreg.req.ready := rdPort.pstate.req.ready && !rdPort.pstate.req.valid
  wrPort.pstate.req.ready := true.B
 
  rdPort.xreg.resp := uBRAM.portB.DO >> (rRdBias * BYTES_PER_BLOCK.U)
  rdPort.pstate.resp := bramBlockUnpackPState(uBRAM.portB.DO)

  // determine the BRAM address
  when(wrPort.xreg.req.fire){
    val xregIdx = wrPort.xreg.req.bits.regIdx
    uBRAM.portB.ADDR := bramAlignAddr(wrPort.thid, xregIdx)
    uBRAM.portB.DI   := bramAlignData(xregIdx, wrPort.xreg.req.bits.data)
    uBRAM.portB.WE   := bramAlignMask(xregIdx)
  }.elsewhen(wrPort.pstate.req.fire){
    uBRAM.portB.ADDR := bramAlignAddr(wrPort.thid, TRANS_STATE_PState_OFFST.U)
    uBRAM.portB.DI := bramBlockPackPState(wrPort.pstate.req.bits.state)
    uBRAM.portB.WE := Fill(48, 1.U) // Write 3/4 Block
  }.elsewhen(rdPort.xreg.req.fire){
    uBRAM.portB.ADDR := bramAlignAddr(rdPort.thid, rdPort.xreg.req.bits.regIdx)
    uBRAM.portB.DI := DontCare
    uBRAM.portB.WE := 0.U
  }.elsewhen(rdPort.pstate.req.fire) {
    uBRAM.portB.ADDR := bramAlignAddr(wrPort.thid, TRANS_STATE_PState_OFFST.U)
    uBRAM.portB.DI := DontCare
    uBRAM.portB.WE := 0.U
  }.otherwise {
    uBRAM.portB.ADDR := DontCare
    uBRAM.portB.DI := DontCare
    uBRAM.portB.WE := 0.U
  }

  uBRAM.portB.EN := true.B

  if (true) { // TODO Conditional asserts
    when(rAXIState === sAXIReadWaitComplete && S_AXI.r.fire) {
      assert(S_AXI.r.rlast, "Transplant:RD: rlast flag must be raised on last block")
    }
    when(rAXIState === sAXIWriting && rAXICounter === rAXICounterEnd && S_AXI.w.fire) {
      assert(S_AXI.w.wlast, "Transplant:WR: wlast flag must be raised on last block")
    }
    when(rAXIState === sAXIIdle) {
      when(S_AXI.ar.fire) {
        assert(S_AXI.ar.arburst === 1.U, "Transplant:RD: arburst must be one")
      }.elsewhen(S_AXI.aw.fire){
        assert(S_AXI.aw.awburst === 1.U, "Transplant:WR: awburst must be one")
      }
    }
    assert((wrPort.xreg.req.fire.asUInt + wrPort.pstate.req.fire.asUInt +
           rdPort.xreg.req.fire.asUInt + rdPort.pstate.req.fire.asUInt) <= 1.U, "Can fire at most 1 request at the same time")
  }
}

object TransBram2HostUnitVerilogEmitter extends App {
  import chisel3.stage.ChiselStage

  val c = new ChiselStage;
  c.emitVerilog(new TransBram2HostUnit(128))
}
