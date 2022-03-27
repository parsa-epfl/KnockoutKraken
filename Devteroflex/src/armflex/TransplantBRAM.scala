package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._

import antmicro.Bus.AXI4
import armflex.util.BRAM
import armflex.util.BRAMParams


// This registers store the ArchState that should be sent to the host and update by the host.
// - Each thread has 32 64bit registers, and PC, the flag, and the stack pointer.
// - We allocate 512 byte (64*64bit) space for each thread. The first 256 bytes are for the x0 - x31 registers, and then one 64byte for PC + Flag + Stack, and the following position are preserved for future reuse.
// - At present we support at most 128 threads, so 16bit address at most. (Not sure if we have such a large space from the shell.)

class TransplantBRAM(
  threadNumber: Int
) extends Module {
  assert(threadNumber <= 128)
  assert(DATA_SZ == 64)
  assert(REG_N == 32)

  val S_AXI = IO(Flipped(new AXI4(16, 512)))
  val sAXIIdle :: sAXIReading :: sAXIReadWaitComplete :: sAXIWriting :: sAXIWriteResponse :: Nil = Enum(5)

  val uBRAM = Module(new BRAM()(new BRAMParams(
    64, 8, 1024, "", true
  )))

  // Two AXI transactions use the same BRAM ports, and cannot happen at the same time.
  // the AXI contexts.
  val rAXIState = RegInit(sAXIIdle)
  val rAXIID = RegInit(0.U(AXI4.idWidth.W))
  val rAXIAddr = RegInit(0.U(16.W))
  val rAXIAddrStep = RegInit(0.U(AXI4.sizeWidth.W))
  val rAXICounter = RegInit(0.U(AXI4.lenWidth.W))
  val rAXICounterEnd = RegInit(0.U(AXI4.lenWidth.W))

  class axi_read_meta_info_t extends Bundle {
    val id = UInt(AXI4.idWidth.W)
    val isLast = Bool()
  }

  val wAXIReadSync = Wire(Decoupled(new axi_read_meta_info_t))

  switch(rAXIState){
    is(sAXIIdle){
      when(S_AXI.ar.fire){
        rAXIID := S_AXI.ar.arid
        rAXIAddr := S_AXI.ar.araddr
        rAXICounter := 0.U
        rAXICounterEnd := S_AXI.ar.arlen
        rAXIAddrStep := S_AXI.ar.arsize
        // only increase burst is supported.
        assert(S_AXI.ar.arburst === 1.U)
        rAXIState := sAXIReading
      }.elsewhen(S_AXI.aw.fire){
        rAXIID := S_AXI.aw.awid
        rAXIAddr := S_AXI.aw.awaddr
        rAXICounter := 0.U
        rAXICounterEnd := S_AXI.aw.awlen
        rAXIAddrStep := S_AXI.aw.awsize
        assert(S_AXI.aw.awburst === 1.U)
        rAXIState := sAXIWriting
      }
    }
    is(sAXIReading){
      when(wAXIReadSync.fire){
        rAXIAddr := rAXIAddr + (1.U << (rAXIAddrStep))
        rAXICounter := rAXICounter + 1.U
        rAXIState := Mux(
          rAXICounter === rAXICounterEnd,
          sAXIReadWaitComplete,
          sAXIReading
        )
      }
    }
    is(sAXIReadWaitComplete){ // wait for the last read transactions to be complete
      when(S_AXI.r.fire){
        rAXIState := sAXIIdle
      }
    }
    is(sAXIWriting){
      when(S_AXI.w.fire){
        rAXIAddr := rAXIAddr + (1.U << (rAXIAddrStep))
        rAXICounter := rAXICounter + 1.U
        when(rAXICounter === rAXICounterEnd){
          assert(S_AXI.w.wlast)
          rAXIState := sAXIWriteResponse
        }
      }
    }
    is(sAXIWriteResponse){
      when(S_AXI.b.fire){
        rAXIState := sAXIIdle
      }
    }
  }

  // BRAM port
  uBRAM.portA.ADDR := rAXIAddr >> 6
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
  wAXIReadSync.bits.id := rAXIID
  wAXIReadSync.bits.isLast := rAXICounter === rAXICounterEnd
  wAXIReadSync.valid := rAXIState === sAXIReading

  // Sync data is valid together with the read output from the BRAM.
  val wAXIReadSyncOut = Queue(wAXIReadSync, 1, true);

  S_AXI.ar.arready := rAXIState === sAXIIdle

  S_AXI.r.rdata := uBRAM.portA.DO
  S_AXI.r.rid := wAXIReadSyncOut.bits.id
  S_AXI.r.rlast := wAXIReadSyncOut.bits.isLast
  S_AXI.r.rresp := 0.U
  S_AXI.r.rvalid := wAXIReadSyncOut.valid

  wAXIReadSyncOut.ready := S_AXI.r.rready

  S_AXI.aw.awready := rAXIState === sAXIIdle && !S_AXI.ar.arvalid // write has lower priority.

  S_AXI.w.wready := rAXIState === sAXIWriting

  S_AXI.b.bid := rAXIID
  S_AXI.b.bresp := 0.U
  S_AXI.b.bvalid := rAXIState === sAXIWriteResponse

  
  // Use the 2nd BRAM ports to sync data with the CPU.
  class TransplantBRAMWriteRequest extends Bundle {
    val threadID = UInt(log2Ceil(threadNumber).W)
    val registerIndex = UInt(6.W) // up to 64 64bit place is available.

    def bramAddr = Cat(threadID, registerIndex >> 3)
    def bramMask = (0xFF.U(8.W)) << ((registerIndex(2, 0)) * 8.U)

    val value = UInt(64.W)

    def bramWriteData = value << ((registerIndex(2, 0) * 64.U))
  }

  // This port is for the normal register writing.
  val iWriteRequest = IO(Flipped(Decoupled(new TransplantBRAMWriteRequest)))
  iWriteRequest.ready := true.B

  // This port is for submitting the pstate.
  class TransplantBRAMPStateSubmitRequest extends Bundle {
    val threadID = UInt(log2Ceil(threadNumber).W)
    val state = new PStateRegs()

    def bramAddr = Cat(threadID, 4.U) // Cat(threadID, 0b100.U)

    // 32 -> PC, 33 -> SP(0), 34 -> FLGAS, 35 -> ICount
    def writeData: UInt = {
      val res = Wire(Vec(64, UInt(64.W)))
      res(0) := state.PC
      res(1) := 0.U
      res(2) := state.flags
      res(3) := Cat(state.icountBudget, state.icount)
      for (i <- 4 to 64){
        res(i) := DontCare
      }

      return res.asUInt()
    }

    def writeMask = Cat(0xFFFF.U(16.W), 0xFFFF.U(16.W))
  }

  val iPstateWriteRequest = IO(Flipped(Decoupled(new TransplantBRAMPStateSubmitRequest)))
  iPstateWriteRequest.ready := !iWriteRequest.valid


  class TransplantBRAMReadRequest extends Bundle {
    val threadID = UInt(log2Ceil(threadNumber).W)
    val registerIndex = UInt(6.W)

    def bramAddr = Cat(threadID, registerIndex >> 3)
  }

  val iReadRequest = IO(Flipped(Decoupled(new TransplantBRAMReadRequest)))
  iReadRequest.ready := iPstateWriteRequest.ready && !iPstateWriteRequest.valid

  val rReadBias = RegNext(iReadRequest.bits.registerIndex(2, 0))
  val oReadReply = IO(Output(UInt(64.W)))

  val iReadPStateRequest = IO(Flipped(Decoupled(UInt(6.W))))
  iReadPStateRequest.ready := iReadRequest.ready && !iReadRequest.valid

  val oReadPStateReply = IO(Output(new PStateRegs))

  // determine the BRAM address
  when(iWriteRequest.valid){
    uBRAM.portB.ADDR := iWriteRequest.bits.bramAddr
    uBRAM.portB.DI := iWriteRequest.bits.bramWriteData
    uBRAM.portB.WE := iWriteRequest.bits.bramMask
  }.elsewhen(iPstateWriteRequest.fire){
    uBRAM.portB.ADDR := iPstateWriteRequest.bits.bramAddr
    uBRAM.portB.DI := iPstateWriteRequest.bits.writeData
    uBRAM.portB.WE := iPstateWriteRequest.bits.writeMask
  }.elsewhen(iReadRequest.fire){
    uBRAM.portB.ADDR := iReadRequest.bits.bramAddr
    uBRAM.portB.DI := DontCare
    uBRAM.portB.WE := 0.U
  }.otherwise {
    uBRAM.portB.ADDR := Cat(iReadPStateRequest.bits, 4.U) // Cat(threadID, 0b100.U)
    uBRAM.portB.DI := DontCare
    uBRAM.portB.WE := 0.U
  }

  uBRAM.portB.EN := true.B

  oReadReply := uBRAM.portB.DO >> (rReadBias * 64.U)
  // Fixed position.
  oReadPStateReply.PC := uBRAM.portB.DO(63, 0)
  oReadPStateReply.flags := uBRAM.portB.DO(127, 64)
  oReadPStateReply.icount := uBRAM.portB.DO(159, 128)
  oReadPStateReply.icountBudget := uBRAM.portB.DO(191, 160)
}

object TransplantBRAMVerilogEmitter extends App {
  import chisel3.stage.ChiselStage

  val c = new ChiselStage;
  c.emitVerilog(new TransplantBRAM(128))
}