package armflex.util

import chisel3._
import chisel3.util._

import antmicro.Bus._
import antmicro.Bus._

class AXILInterconnector(
  addrSegment: Seq[BigInt],
  addrMask: Seq[BigInt],
  addrW: Int = 32,
  dataW: Int = 32
) extends Module {
  assert(addrMask.length == addrSegment.length)
  val S_AXIL = IO(Flipped(new AXI4Lite(addrW, dataW)))
  val M_AXIL = IO(Vec(addrMask.length, new AXI4Lite(addrW, dataW)))
  // AR
  val read_which = OHToUInt(addrSegment.zip(addrMask).map { case (addr, mask) =>
    (S_AXIL.ar.araddr & mask.U) === addr.U
  })

  for(i <- 0 until addrSegment.length){
    M_AXIL(i).ar.araddr := S_AXIL.ar.araddr
    M_AXIL(i).ar.arprot := S_AXIL.ar.arprot
    M_AXIL(i).ar.arvalid := S_AXIL.ar.arvalid && read_which === i.U
  }
  S_AXIL.ar.arready := M_AXIL(read_which).ar.arready

  val read_which_enq = Wire(Decoupled(UInt(log2Ceil(addrSegment.length).W)))
  read_which_enq.bits := read_which
  read_which_enq.valid := S_AXIL.ar.arready && S_AXIL.ar.arvalid

  val read_which_queue = Queue(read_which_enq, 2)

  // R
  S_AXIL.r.rdata := M_AXIL(read_which_queue.bits).r.rdata
  S_AXIL.r.rresp := M_AXIL(read_which_queue.bits).r.rresp
  S_AXIL.r.rvalid := M_AXIL(read_which_queue.bits).r.rvalid && read_which_queue.valid
  for(i <- 0 until addrSegment.length){
    M_AXIL(i).r.rready := S_AXIL.r.rready && read_which_queue.bits === i.U && read_which_queue.valid
  }

  read_which_queue.ready := S_AXIL.r.rvalid && S_AXIL.r.rready

  // AW
  val write_which = OHToUInt(addrSegment.zip(addrMask).map { case (addr, mask) =>
    (S_AXIL.aw.awaddr & mask.U) === addr.U
  })

  for(i <- 0 until addrSegment.length){
    M_AXIL(i).aw.awaddr := S_AXIL.aw.awaddr
    M_AXIL(i).aw.awprot := S_AXIL.aw.awprot
    M_AXIL(i).aw.awvalid := S_AXIL.aw.awvalid && i.U === write_which
  }
  S_AXIL.aw.awready := M_AXIL(write_which).aw.awready

  val write_which_enq = Wire(Decoupled(UInt(log2Ceil(addrSegment.length).W)))
  write_which_enq.bits := write_which
  write_which_enq.valid := S_AXIL.aw.awvalid && S_AXIL.aw.awready

  val write_which_queue = Queue(write_which_enq, 2)

  // W
  for(i <- 0 until addrSegment.length){
    M_AXIL(i).w.wdata := S_AXIL.w.wdata
    M_AXIL(i).w.wstrb := S_AXIL.w.wstrb
    M_AXIL(i).w.wvalid := S_AXIL.w.wvalid && write_which_queue.bits === i.U && write_which_queue.valid
  }
  S_AXIL.w.wready := M_AXIL(write_which_queue.bits).w.wready && write_which_queue.valid
  write_which_queue.ready := S_AXIL.w.wready && S_AXIL.w.wvalid

  val response_which_enq = Wire(Decoupled(UInt(log2Ceil(addrSegment.length).W)))
  response_which_enq.bits := write_which_queue.bits
  response_which_enq.valid := write_which_queue.fire

  val response_which_queue = Queue(response_which_enq, 2)

  // B
  S_AXIL.b.bresp := M_AXIL(response_which_queue.bits).b.bresp
  S_AXIL.b.bvalid := M_AXIL(response_which_queue.bits).b.bvalid && response_which_queue.valid
  for(i <- 0 until addrSegment.length){
    M_AXIL(i).b.bready := S_AXIL.b.bready && i.U === response_which_queue.bits && response_which_queue.valid
  }

  response_which_queue.ready := S_AXIL.b.bvalid && S_AXIL.b.bready
}

class AXIInterconnector(
  addrSegment: Seq[BigInt],
  addrMask: Seq[BigInt],
  addrWidth: Int = 64,
  dataW: Int = 512
) extends Module {
  assert(addrMask.length == addrSegment.length)
  val S_AXI = IO(Flipped(new AXI4(addrWidth, dataW)))
  val M_AXI = IO(Vec(addrMask.length, new AXI4(addrWidth, dataW)))

  // AR
  val read_which = OHToUInt(addrSegment.zip(addrMask).map { case (addr, mask) =>
    (S_AXI.ar.araddr & mask.U) === addr.U
  })

  for(i <- 0 until addrSegment.length){
    M_AXI(i).ar.araddr := S_AXI.ar.araddr
    M_AXI(i).ar.arburst := S_AXI.ar.arburst
    M_AXI(i).ar.arcache := S_AXI.ar.arcache
    M_AXI(i).ar.arid := S_AXI.ar.arid
    M_AXI(i).ar.arlen := S_AXI.ar.arlen
    M_AXI(i).ar.arlock := S_AXI.ar.arlock
    M_AXI(i).ar.arprot := S_AXI.ar.arprot
    M_AXI(i).ar.arqos := S_AXI.ar.arqos
    M_AXI(i).ar.arsize := S_AXI.ar.arsize
    M_AXI(i).ar.arvalid := S_AXI.ar.arvalid && read_which === i.U
  }
  S_AXI.ar.arready := M_AXI(read_which).ar.arready

  val read_which_enq = Wire(Decoupled(UInt(log2Ceil(addrSegment.length).W)))
  read_which_enq.bits := read_which
  read_which_enq.valid := S_AXI.ar.arready && S_AXI.ar.arvalid

  val read_which_queue = Queue(read_which_enq, addrMask.length)

  // R
  S_AXI.r.rdata := M_AXI(read_which_queue.bits).r.rdata
  S_AXI.r.rid := M_AXI(read_which_queue.bits).r.rid
  S_AXI.r.rlast := M_AXI(read_which_queue.bits).r.rlast
  S_AXI.r.rresp := M_AXI(read_which_queue.bits).r.rresp
  S_AXI.r.rvalid := M_AXI(read_which_queue.bits).r.rvalid && read_which_queue.valid
  
  for(i <- 0 until addrSegment.length){
    M_AXI(i).r.rready := S_AXI.r.rready && read_which_queue.bits === i.U && read_which_queue.valid
  }

  read_which_queue.ready := S_AXI.r.rvalid && S_AXI.r.rready && S_AXI.r.rlast

  // AW
  val write_which = OHToUInt(addrSegment.zip(addrMask).map { case (addr, mask) =>
    (S_AXI.aw.awaddr & mask.U) === addr.U
  })

  for(i <- 0 until addrSegment.length){
    M_AXI(i).aw.awaddr := S_AXI.aw.awaddr
    M_AXI(i).aw.awburst := S_AXI.aw.awburst
    M_AXI(i).aw.awcache := S_AXI.aw.awcache
    M_AXI(i).aw.awid := S_AXI.aw.awid
    M_AXI(i).aw.awlen := S_AXI.aw.awlen
    M_AXI(i).aw.awlock := S_AXI.aw.awlock
    M_AXI(i).aw.awprot := S_AXI.aw.awprot
    M_AXI(i).aw.awqos := S_AXI.aw.awqos
    M_AXI(i).aw.awsize := S_AXI.aw.awsize
    M_AXI(i).aw.awvalid := S_AXI.aw.awvalid && i.U === write_which
  }
  S_AXI.aw.awready := M_AXI(write_which).aw.awready

  val write_which_enq = Wire(Decoupled(UInt(log2Ceil(addrSegment.length).W)))
  write_which_enq.bits := write_which
  write_which_enq.valid := S_AXI.aw.awvalid && S_AXI.aw.awready

  val write_which_queue = Queue(write_which_enq, addrMask.length)

  // W
  for(i <- 0 until addrSegment.length){
    M_AXI(i).w.wdata := S_AXI.w.wdata
    M_AXI(i).w.wlast := S_AXI.w.wlast
    M_AXI(i).w.wstrb := S_AXI.w.wstrb
    M_AXI(i).w.wvalid := S_AXI.w.wvalid && write_which_queue.bits === i.U && write_which_queue.valid
  }
  S_AXI.w.wready := M_AXI(write_which_queue.bits).w.wready && write_which_queue.valid
  write_which_queue.ready := S_AXI.w.wready && S_AXI.w.wvalid && S_AXI.w.wlast

  val response_which_enq = Wire(Decoupled(UInt(log2Ceil(addrSegment.length).W)))
  response_which_enq.bits := write_which_queue.bits
  response_which_enq.valid := write_which_queue.fire

  val response_which_queue = Queue(response_which_enq, addrMask.length)

  // B
  S_AXI.b.bid := M_AXI(response_which_enq.bits).b.bid
  S_AXI.b.bresp := M_AXI(response_which_queue.bits).b.bresp
  S_AXI.b.bvalid := M_AXI(response_which_queue.bits).b.bvalid && response_which_queue.valid
  for(i <- 0 until addrSegment.length){
    M_AXI(i).b.bready := S_AXI.b.bready && i.U === response_which_queue.bits && response_which_queue.valid
  }

  response_which_queue.ready := S_AXI.b.bvalid && S_AXI.b.bready
}

object AXIInterconnectorVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  c.emitVerilog(new AXIInterconnector(
    Seq(0x1000, 0x2000, 0x3000), Seq(0xF000, 0xF000, 0xF000), 16, 32
  ))
  c.emitVerilog(new AXILInterconnector(
    Seq(0x1000, 0x2000, 0x3000), Seq(0xF000, 0xF000, 0xF000), 16, 32
  ))
}

class AXILInterconnectorNonOptimized(
  addrSegments: Seq[BigInt],
  addrW: Int = 32,
  dataW: Int = 32
) extends MultiIOModule {
  val S_AXIL = IO(Flipped(new AXI4Lite(addrW, dataW)))
  val M_AXIL = IO(Vec(addrSegments.length, new AXI4Lite(addrW, dataW)))

  val sIdle :: sReadData :: sWriteData :: sWriteResp :: Nil = Enum(4)
  val state = RegInit(sIdle)

  val addrPort = WireInit(0.U(log2Ceil(addrSegments.length).W))
  val selectedPort = RegInit(0.U(log2Ceil(addrSegments.length).W))


  for(segment <- 0 until addrSegments.length){
    // AR
    M_AXIL(segment).ar.araddr := S_AXIL.ar.araddr
    M_AXIL(segment).ar.arprot := S_AXIL.ar.arprot
    M_AXIL(segment).ar.arvalid := false.B
    S_AXIL.ar.arready := false.B
 
    // AW
    M_AXIL(segment).aw.awaddr := S_AXIL.aw.awaddr
    M_AXIL(segment).aw.awprot := S_AXIL.aw.awprot
    M_AXIL(segment).aw.awvalid := false.B
    S_AXIL.aw.awready := false.B

    // R
    M_AXIL(segment).r.rready := false.B
    S_AXIL.r.rvalid := false.B

    // W
    M_AXIL(segment).w.wdata := S_AXIL.w.wdata
    M_AXIL(segment).w.wstrb := S_AXIL.w.wstrb
    M_AXIL(segment).w.wvalid := false.B
    S_AXIL.w.wready := false.B

    // B
    M_AXIL(segment).b.bready := false.B
  }

  // Handshakes
  addrPort := (addrSegments.length - 1).U
  for (port <- 0 until addrSegments.length - 1) {
    when(S_AXIL.aw.awvalid && addrSegments(port).U <= S_AXIL.aw.awaddr && 
         S_AXIL.aw.awaddr < addrSegments(port + 1).U) {
      addrPort := port.U
    }.elsewhen(S_AXIL.ar.arvalid && addrSegments(port).U <= S_AXIL.ar.araddr && 
               S_AXIL.ar.araddr < addrSegments(port + 1).U) {
      addrPort := port.U
    }
  }

  // AW
  M_AXIL(addrPort).aw.awvalid := S_AXIL.aw.awvalid && state === sIdle
  S_AXIL.aw.awready := M_AXIL(addrPort).aw.awready && state === sIdle

  // AR
  M_AXIL(addrPort).ar.arvalid := S_AXIL.ar.arvalid && state === sIdle
  S_AXIL.ar.arready := M_AXIL(addrPort).ar.arready && state === sIdle

  // R
  S_AXIL.r.rresp := M_AXIL(selectedPort).r.rresp
  S_AXIL.r.rdata := M_AXIL(selectedPort).r.rdata
  S_AXIL.r.rvalid := M_AXIL(selectedPort).r.rvalid && state === sReadData
  M_AXIL(selectedPort).r.rready := S_AXIL.r.rready && state === sReadData

  // W
  M_AXIL(selectedPort).w.wvalid := S_AXIL.w.wvalid && state === sWriteData
  S_AXIL.w.wready := M_AXIL(selectedPort).w.wready && state === sWriteData

  // B
  S_AXIL.b.bresp := M_AXIL(selectedPort).b.bresp
  S_AXIL.b.bvalid := M_AXIL(selectedPort).b.bvalid && state === sWriteResp
  M_AXIL(selectedPort).b.bready := S_AXIL.b.bready && state === sWriteResp
  
  val awFire = WireInit(S_AXIL.aw.awvalid && S_AXIL.aw.awready)
  val arFire = WireInit(S_AXIL.ar.arvalid && S_AXIL.ar.arready)
  switch(state){
    is(sIdle){
      when(awFire) {
        state := sWriteData
        selectedPort := addrPort
      }.elsewhen(arFire) {
        state := sReadData
        selectedPort := addrPort
      }
    }
    is(sReadData){
      when(S_AXIL.r.rready && S_AXIL.r.rvalid){
        state := sIdle
      }
    }
    is(sWriteData){
      when(S_AXIL.w.wvalid && S_AXIL.w.wready){
        state := sWriteResp
      }
    }
    is(sWriteResp){
      when(S_AXIL.b.bready && S_AXIL.b.bvalid){
        state := sIdle
      }
    }
  }
}