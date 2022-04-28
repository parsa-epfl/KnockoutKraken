package armflex.util

import chisel3._
import chisel3.util._
import antmicro.Bus.AXI4Lite

import antmicro.Bus._
import antmicro.CSR.CSR

class AXIControlledMessageQueue(val axiW: Int, val csrRegSize: Int, val queueS: Int) extends Module {
  val rdFifo = IO(new Bundle {
    val deq = Flipped(Decoupled(UInt(axiW.W)))
    val msgCnt = Input(UInt(log2Ceil(queueS+1).W))
  })

  val wrFifo = IO(new Bundle {
    val enq = Decoupled(UInt(axiW.W))
    val freeCnt = Input(UInt(log2Ceil(queueS+1).W))
  })

  val S_AXI = IO(Flipped(new AXI4(log2Ceil(axiW/8) + 1, axiW)))

  val uCSR = Module(new CSR(csrRegSize, 2))
  val S_CSR = IO(Flipped(uCSR.io.bus.cloneType))
  S_CSR <> uCSR.io.bus

  // uCSR[0]: How many requests are pending
  // uCSR[1]: How many free spots does the Queue has
  uCSR.io.csr(0).dataIn := rdFifo.msgCnt
  uCSR.io.csr(1).dataIn := wrFifo.freeCnt

  val sIdle :: sData :: sDone :: sB :: Nil = Enum(4)
  val rdState = RegInit(sIdle)
  val wrState = RegInit(sIdle)

  val rdAllowed = RegInit(false.B)
  val rdAddr = RegInit(S_AXI.ar.araddr.cloneType, 0.U)
  val rdSize = RegInit(S_AXI.ar.arsize.cloneType, 0.U)
  val rdLen = RegInit(S_AXI.ar.arlen.cloneType, 0.U)

  val rdAddrCorrect = WireInit(S_AXI.ar.araddr(log2Ceil(axiW/8)) === 0.U)
  S_AXI.ar.arready := rdState === sIdle

  S_AXI.r.rresp := Mux(rdAllowed, AXI4.OKAY.U, AXI4.SLVERR.U)
  S_AXI.r.rlast := rdLen === 0.U
  S_AXI.r.rdata := rdFifo.deq.bits
  S_AXI.r.rvalid := rdState === sData
  rdFifo.deq.ready := S_AXI.r.fire && rdAllowed

  switch(rdState){
    is(sIdle){
      when(S_AXI.ar.fire) {
        rdAllowed := rdAddrCorrect && S_AXI.ar.arlen < rdFifo.msgCnt
        rdAddr := S_AXI.ar.araddr
        rdLen := S_AXI.ar.arlen
        rdSize := S_AXI.ar.arsize
        rdState := sData
      }
    }
    
    is(sData) {
      when(S_AXI.r.fire) {
        rdLen := rdLen - 1.U
        when(S_AXI.r.rlast) {
          rdState := sDone
        }
      }
    }

    is(sDone){
      rdState := sIdle
    }
  }

  val wrAllowed = RegInit(false.B)
  val wrAddr = RegInit(S_AXI.aw.awaddr.cloneType, 0.U)
  val wrSize = RegInit(S_AXI.aw.awsize.cloneType, 0.U)
  val wrLen = RegInit(S_AXI.aw.awlen.cloneType, 0.U)
 
  val wrAddrCorrect = WireInit(S_AXI.aw.awaddr(log2Ceil(axiW/8)) === 1.U)
  S_AXI.aw.awready := wrState === sIdle

  S_AXI.w.wready := wrState === sData
  wrFifo.enq.valid := S_AXI.w.fire && wrAllowed
  wrFifo.enq.bits := S_AXI.w.wdata

  S_AXI.b.bresp  := Mux(wrAllowed, AXI4.OKAY.U, AXI4.SLVERR.U)
  S_AXI.b.bvalid := wrState === sB

  switch(wrState){
    is(sIdle){
      when(S_AXI.aw.fire) {
        wrAllowed := wrAddrCorrect && S_AXI.aw.awlen < wrFifo.freeCnt
        wrAddr := S_AXI.aw.awaddr
        wrLen := S_AXI.aw.awlen
        wrSize := S_AXI.aw.awsize
        wrState := sData
      }
    }
    
    is(sData) {
      when(S_AXI.w.fire) {
        wrLen := wrLen - 1.U
        when(S_AXI.w.wlast) {
          wrState := sB
        }
      }
    }

    is(sB) {
      when(S_AXI.b.fire) {
        wrState := sDone
      }
    }

    is(sDone){
      wrState := sIdle
    }
  }


  dontTouch(S_CSR)
  dontTouch(uCSR.io)
  // Conditional asserts
  val brespExpect = RegInit(AXI4.OKAY.U)
  when(S_AXI.aw.fire && !(S_AXI.aw.awlen < wrFifo.freeCnt )) {
    brespExpect := AXI4.SLVERR.U
  }
  when(S_AXI.b.fire) {
    assert(S_AXI.b.bresp === brespExpect, "If fifo can't receive transactions, send error on write")
  }
  val rrespExpect = RegInit(AXI4.OKAY.U)
  when(S_AXI.ar.fire && !(S_AXI.ar.arlen < rdFifo.msgCnt)) {
    rrespExpect := AXI4.SLVERR.U
  }
  when(S_AXI.r.fire) {
    assert(S_AXI.r.rresp === rrespExpect, "If fifo is empty, send AXI error on read")
  }
  when(S_AXI.w.fire) {
    assert(S_AXI.w.wstrb.andR, "Strobe must have all bytes enabled")
  }
  when(wrState === sData && S_AXI.w.wlast) {
    assert(wrLen === 0.U, "Last write must be last element")
  }
  when(rdState === sData && S_AXI.r.rlast) {
    assert(rdLen === 0.U, "Last write must be last element")
  }
}

