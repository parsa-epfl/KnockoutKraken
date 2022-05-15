/*
MIT License

Copyright (c) 2019 Antmicro

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this perm notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
package antmicro.Bus

import chisel3._

class AXI4AW(val addrWidth : Int) extends Bundle{
  // val awid = Output(UInt(AXI4.idWidth.W))
  val awaddr = Output(UInt(addrWidth.W))
  val awlen = Output(UInt(AXI4.lenWidth.W))
  val awsize = Output(UInt(AXI4.sizeWidth.W))
  val awburst = Output(UInt(AXI4.burstWidth.W))
  // val awlock = Output(Bool())
  // val awcache = Output(UInt(AXI4.cacheWidth.W))
  // val awprot = Output(UInt(AXI4.protWidth.W))
  // val awqos = Output(UInt(AXI4.qosWidth.W))
  val awvalid = Output(Bool())
  val awready = Input(Bool())

  def fire = this.awready && this.awvalid
}

object AXI4AW {
  def apply(addr : UInt, len : UInt, size : UInt, valid : UInt) : AXI4AW = {
    val aw = Wire(new AXI4AW(addr.getWidth))
    // aw.awid := 0.U
    aw.awburst := 1.U
    // aw.awlock := 0.U
    // aw.awcache := 2.U
    // aw.awprot := 0.U
    // aw.awqos := 0.U
    aw.awaddr := addr
    aw.awlen := len
    aw.awsize := size
    aw.awvalid := valid
    aw
  }
  def tieOff(addrWidth : Int): AXI4AW = {
    val aw = Wire(new AXI4AW(addrWidth))
    // aw.awid := 0.U
    aw.awaddr := 0.U
    aw.awlen := 0.U
    aw.awsize := 0.U
    aw.awburst := 0.U
    // aw.awlock := 0.U
    // aw.awcache := 0.U
    // aw.awprot := 0.U
    // aw.awqos := 0.U
    aw.awvalid := 0.U
    aw
  }

  def stub(addrWidth: Int): AXI4AW = {
    val aw = Wire(new AXI4AW(addrWidth))
    aw.awready := false.B
    aw
  }
}

class AXI4W(val dataWidth : Int) extends Bundle{
  val wdata = Output(UInt(dataWidth.W))
  val wstrb = Output(UInt((dataWidth/8).W))
  val wlast = Output(Bool())
  val wvalid = Output(Bool())
  val wready = Input(Bool())

  def fire = this.wready && this.wvalid
}

object AXI4W{
  def apply(data : UInt, strb : UInt, last : UInt, valid : UInt): AXI4W = {
    val w = Wire(new AXI4W(data.getWidth))
    w.wdata := data
    w.wstrb := strb
    w.wlast := last
    w.wvalid := valid
    w
  }
  def tieOff(dataWidth : Int): AXI4W = {
    val w = Wire(new AXI4W(dataWidth))
    w.wdata := 0.U
    w.wstrb := 0.U
    w.wlast := 0.U
    w.wvalid := 0.U
    w
  }

  def stub(dataWidth: Int): AXI4W = {
    val w = Wire(new AXI4W(dataWidth))
    w.wready := false.B
    w
  }
}

class AXI4B extends Bundle{
  // val bid = Input(UInt(AXI4.idWidth.W))
  val bresp = Input(UInt(AXI4.respWidth.W))
  val bvalid = Input(Bool())
  val bready = Output(Bool())

  def fire = this.bready && this.bvalid
}

object AXI4B{
  def apply(ready : UInt): AXI4B = {
    val b = Wire(new AXI4B())
    b.bready := ready
    b
  }

  def tieOff(): AXI4B = {
    val b = Wire(new AXI4B())
    b.bready := 0.U
    b
  }

  def stub(): AXI4B = {
    val b = Wire(new AXI4B())
    // b.bid := DontCare
    b.bresp := DontCare
    b.bvalid := false.B
    b
  }
}

class AXI4AR(val addrWidth : Int) extends Bundle{
  // val arid = Output(UInt(AXI4.idWidth.W))
  val araddr = Output(UInt(addrWidth.W))
  val arlen = Output(UInt(AXI4.lenWidth.W))
  val arsize = Output(UInt(AXI4.sizeWidth.W))
  val arburst = Output(UInt(AXI4.burstWidth.W))
  // val arlock = Output(Bool())
  // val arcache = Output(UInt(AXI4.cacheWidth.W))
  // val arprot = Output(UInt(AXI4.protWidth.W))
  // val arqos = Output(UInt(AXI4.qosWidth.W))
  val arvalid = Output(Bool())
  val arready = Input(Bool())

  def fire = this.arready && this.arvalid
}

object AXI4AR{
  def apply(addr : UInt, len : UInt, size : UInt, valid : UInt) : AXI4AR = {
    val ar = Wire(new AXI4AR(addr.getWidth))
    // ar.arid := 0.U
    ar.arburst := 1.U
    // ar.arlock := 0.U
    // ar.arcache := 2.U
    // ar.arprot := 0.U
    // ar.arqos := 0.U
    ar.araddr := addr
    ar.arlen := len
    ar.arsize := size
    ar.arvalid := valid
    ar
  }
  def tieOff(addrWidth: Int): AXI4AR = {
    val ar = Wire(new AXI4AR(addrWidth))
    // ar.arid := 0.U
    ar.araddr := 0.U
    ar.arlen := 0.U
    ar.arsize := 0.U
    ar.arburst := 0.U
    // ar.arlock := 0.U
    // ar.arcache := 0.U
    // ar.arprot := 0.U
    // ar.arqos := 0.U
    ar.arvalid := 0.U
    ar
  }

  def stub(addrWidth: Int): AXI4AR = {
    val ar = Wire(new AXI4AR(addrWidth))
    ar.arready := false.B
    ar
  }
}

class AXI4R(val dataWidth : Int) extends Bundle{
  // val rid = Input(UInt(AXI4.idWidth.W))
  val rdata = Input(UInt(dataWidth.W))
  val rresp = Input(UInt(AXI4.respWidth.W))
  val rlast = Input(Bool())
  val rvalid = Input(Bool())
  val rready = Output(Bool())

  def fire = this.rready && this.rvalid
}

object AXI4R{
  def apply(dataWidth : Int, ready : UInt): AXI4R = {
    val r = Wire(new AXI4R(dataWidth))
    r.rready := ready
    r
  }
  def tieOff(dataWidth : Int): AXI4R = {
    val r = Wire(new AXI4R(dataWidth))
    r.rready := 0.U
    r
  }

  def stub(dataWidth: Int): AXI4R = {
    val r = Wire(new AXI4R(dataWidth))
    r.rdata := DontCare
    // r.rid := DontCare
    r.rlast := DontCare
    r.rresp := DontCare
    r.rvalid := false.B
    r
  }
}

class AXI4(val addrWidth : Int, val dataWidth : Int) extends Bundle{
  val aw = new AXI4AW(addrWidth)
  val w = new AXI4W(dataWidth)
  val b = new AXI4B()
  val ar = new AXI4AR(addrWidth)
  val r = new AXI4R(dataWidth)
}

object AXI4 {
  val idWidth = 0
  val lenWidth = 8
  val sizeWidth = 3
  val burstWidth = 2
  val cacheWidth = 0
  val protWidth = 0
  val qosWidth = 0
  val respWidth = 2
  val OKAY   = 0
  val EXOKAY = 1
  val SLVERR = 2
  val DECERR = 3
}
