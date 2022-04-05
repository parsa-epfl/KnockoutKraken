package armflex.util

import chisel3._
import chisel3.util._
import antmicro.Bus.AXI4Lite

import antmicro.Bus._
import antmicro.CSR.CSR

// Refactor this big module
class AXIControlledMessageQueue extends Module {
  val fifo_i = IO(Flipped(Decoupled(UInt(512.W))))
  val fifo_o = IO(Decoupled(UInt(512.W)))

  val uCSR = Module(new CSR(32, 4))
  val S_CSR = IO(Flipped(uCSR.io.bus.cloneType))
  S_CSR <> uCSR.io.bus

  // uCSR[0]: Whether there is a place to hold a new message (the host can send message out)
  uCSR.io.csr(0).dataIn := fifo_o.ready

  // uCSR[1]: Whether there is a message pending in the queue. (the host can read a message out)
  uCSR.io.csr(1).dataIn := fifo_i.valid

  // uCSR[2]: Write to send out the message in the buffer.
  uCSR.io.csr(2).dataIn := 0.U
  val rMessagePushValid = RegNext(uCSR.io.csr(2).dataWrite)
  fifo_o.valid := rMessagePushValid

  // uCSR[3]: Write to pop out one message in the buffer.
  uCSR.io.csr(3).dataIn := 0.U
  val rMessagePopValid = RegNext(uCSR.io.csr(3).dataWrite)
  fifo_i.ready := rMessagePopValid

  // Minimal AXI4 address space is 128 bytes
  val S_AXI = IO(Flipped(new AXI4(log2Ceil(128), 512)))

  // Read logic. Reading from 0x00 means it get the message, but not pop out.
  // TODO: Make writing to 0x00 and reading to 0x40 meaningful. 
  val rAXIReadValid = RegInit(false.B)
  val rAXIRid = RegInit(0.U(AXI4.idWidth.W))
  switch(rAXIReadValid){
    is(false.B){
      when(S_AXI.ar.fire && (S_AXI.ar.araddr(6)) === false.B){
        rAXIRid := S_AXI.ar.arid
        // no burst is allowed!
        assert(S_AXI.ar.arlen === 0.U)
        rAXIReadValid := true.B
      }
    }
    is(true.B){
      rAXIReadValid := Mux(
        S_AXI.r.fire,
        false.B,
        true.B
      )
    }
  }

  S_AXI.ar.arready := rAXIReadValid === false.B

  S_AXI.r.rvalid := rAXIReadValid
  S_AXI.r.rdata := fifo_i.bits
  S_AXI.r.rresp := 0.U
  S_AXI.r.rlast := true.B
  S_AXI.r.rid := rAXIRid

  // Write logic. Write to 0x40 means place a message into the buffer.
  val rAXIWriteId = RegInit(0.U(AXI4.idWidth.W))
  val sAXI_AW :: sAXI_W :: sAXI_B :: Nil = Enum(3)
  val rAXIWriteState = RegInit(sAXI_AW)
  val rAXIWriteBuffer = RegInit(0.U(512.W))

  S_AXI.aw.awready := rAXIWriteState === sAXI_AW && S_AXI.aw.awaddr(6) === true.B

  S_AXI.w.wready := rAXIWriteState === sAXI_W

  S_AXI.b.bresp := 0.U
  S_AXI.b.bid := rAXIWriteId
  S_AXI.b.bvalid := rAXIWriteState === sAXI_B

  switch(rAXIWriteState){
    is(sAXI_AW){
      when(S_AXI.aw.fire && (S_AXI.aw.awaddr(6) === true.B)) {
        rAXIWriteState := sAXI_W
        rAXIWriteId := S_AXI.aw.awid
        // no burst is allowed!
        assert(S_AXI.aw.awlen === 0.U)
      }
    }
    is(sAXI_W){
      when(S_AXI.w.fire){
        rAXIWriteState := sAXI_B
        // only one element should be transferred.
        assert(S_AXI.w.wlast)
        rAXIWriteBuffer := S_AXI.w.wdata
      }
    }
    is(sAXI_B){
      when(S_AXI.b.fire){
        rAXIWriteState := sAXI_AW
      }
    }
  }

  fifo_o.bits := rAXIWriteBuffer
}

