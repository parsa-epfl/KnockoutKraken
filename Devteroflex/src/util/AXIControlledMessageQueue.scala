package armflex.util

import chisel3._
import chisel3.util._
import antmicro.Bus.AXI4Lite

import antmicro.Bus._

class AXIControlledMessageQueue extends Module {
  val fifo_i = IO(Flipped(Decoupled(UInt(512.W))))
  val fifo_o = IO(Decoupled(UInt(512.W)))

  /**
   *  Address | Meaning
   *  0x0     | out queue is not full (can send)
   *  0x4     | in queue is not empty (can read message)
   */ 
  // TODO: add interrupt here when there is a message available
  val S_AXIL = IO(Flipped(new AXI4Lite(32, 32)))
  // write signals are not included.
  S_AXIL.aw.awready := false.B
  S_AXIL.w.wready := false.B
  S_AXIL.b.bresp := 0.U
  S_AXIL.b.bvalid := false.B

  val addr_r = RegInit(0.U(3.W))
  val addr_vr = RegInit(false.B)
  
  when(S_AXIL.ar.arready && S_AXIL.ar.arvalid){
    addr_r := S_AXIL.ar.araddr(2, 0)
    addr_vr := true.B
  }.elsewhen(S_AXIL.r.rready && S_AXIL.r.rvalid){
    addr_vr := false.B
  }

  S_AXIL.ar.arready := !addr_vr
  
  S_AXIL.r.rdata := Mux(addr_r(2), fifo_i.valid, fifo_o.ready)
  S_AXIL.r.rresp := 0.U
  S_AXIL.r.rvalid := addr_vr

  // Read from any address will trigger a pop
  // write to any address will trigger a insersion.
  // Minimal AXI4 address spacce is 128 bytes

  val S_AXI = IO(Flipped(new AXI4(log2Ceil(128), 512)))

  // read logic
  val axi_rid_r = RegEnable(S_AXI.ar.arid, S_AXI.ar.arvalid && S_AXI.ar.arready)
  val axi_rv = RegInit(false.B) // whether the bus is in read state.
  S_AXI.ar.arready := !axi_rv

  S_AXI.r.rvalid := fifo_i.valid && axi_rv
  S_AXI.r.rdata := fifo_i.bits
  S_AXI.r.rresp := 0.U
  S_AXI.r.rlast := true.B
  S_AXI.r.rid := axi_rid_r
  fifo_i.ready := axi_rv && S_AXI.r.rready

  when(S_AXI.r.rvalid && S_AXI.r.rready){
    axi_rv := false.B
  }.elsewhen(S_AXI.ar.arready && S_AXI.ar.arvalid){
    axi_rv := true.B
  }

  // write logic
  val axi_wid_r = RegEnable(S_AXI.aw.awid, S_AXI.aw.awvalid && S_AXI.aw.awready)
  val sAXI_AW :: sAXI_W :: sAXI_B :: Nil = Enum(3)
  val axi_w_state_r = RegInit(sAXI_AW)

  S_AXI.aw.awready := axi_w_state_r === sAXI_AW

  S_AXI.w.wready := axi_w_state_r === sAXI_W && fifo_o.ready
  fifo_o.valid := S_AXI.w.wvalid && axi_w_state_r === sAXI_W
  fifo_o.bits := S_AXI.w.wdata

  S_AXI.b.bresp := 0.U
  S_AXI.b.bid := axi_wid_r
  S_AXI.b.bvalid := axi_w_state_r === sAXI_B

  switch(axi_w_state_r){
    is(sAXI_AW){
      when(S_AXI.aw.awready && S_AXI.aw.awvalid) {
        axi_w_state_r := sAXI_W
      }
    }
    is(sAXI_W){
      when(S_AXI.w.wready && S_AXI.w.wvalid && S_AXI.w.wlast){
        axi_w_state_r := sAXI_B
      }
    }
    is(sAXI_B){
      when(S_AXI.b.bvalid && S_AXI.b.bready){
        axi_w_state_r := sAXI_AW
      }
    }
  }



}

