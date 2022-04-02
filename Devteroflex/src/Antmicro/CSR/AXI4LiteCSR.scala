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
package antmicro.CSR

import antmicro.Bus.AXI4Lite
import chisel3._
import chisel3.util._

class AXI4LiteCSR(dataWidth: Int, regCount: Int) extends Module {
  val addrWidth = log2Ceil(regCount) + 2 // Byte addressed
  assert(dataWidth == 32) // Since we already add 2 to the address, the dataWidth must be 32bit.
  val io = IO(new Bundle{
    val ctl = Flipped(new AXI4Lite(addrWidth, dataWidth))
    val bus = new CSRBusBundle(dataWidth, regCount)
  })

  val sIdle :: sReadAddr :: sReadData :: sWriteAddr :: sWriteData :: sWriteResp :: Nil = Enum(6)
  val state = RegInit(sIdle)

  val awready = RegInit(false.B)
  val wready = RegInit(false.B)
  val bvalid = RegInit(false.B)
  val bresp = WireInit(0.U(AXI4Lite.respWidth.W))

  val arready = RegInit(false.B)
  val rvalid = RegInit(false.B)
  val rresp = WireInit(0.U(AXI4Lite.respWidth.W))

  val addr = RegInit(0.U(addrWidth.W))

  io.ctl.r.rdata := io.bus.dataIn
  io.bus.dataOut := io.ctl.w.wdata

  io.ctl.aw.awready := awready
  io.ctl.w.wready := wready
  io.ctl.b.bvalid := bvalid
  io.ctl.b.bresp := bresp

  io.ctl.ar.arready := arready
  io.ctl.r.rvalid := rvalid
  io.ctl.r.rresp := rresp

  io.bus.read := (io.ctl.r.rready && rvalid) || (io.ctl.ar.arvalid && arready)  // '||' for setting up the read address in BRAM before the read access
  io.bus.write := io.ctl.w.wvalid && wready
  io.bus.addr := addr

  switch(state){
    is(sIdle){
      when(io.ctl.aw.awvalid){
        state := sWriteAddr
        addr := io.ctl.aw.awaddr >> log2Ceil(dataWidth/8).U
        awready := true.B // TODO: I don't think awready should wait for the awvalid :facepalm:

      }.elsewhen(io.ctl.ar.arvalid){
        state := sReadAddr
        addr := io.ctl.ar.araddr >> log2Ceil(dataWidth/8).U
        arready := true.B
      }
    }
    is(sReadAddr){
      when(io.ctl.ar.arvalid && arready){
        state := sReadData
        arready := false.B
        rvalid := true.B
      }
    }
    is(sReadData){
      when(io.ctl.r.rready && rvalid){
        state := sIdle
        rvalid := false.B
      }
    }
    is(sWriteAddr){
      when(io.ctl.aw.awvalid && awready){
        state := sWriteData
        awready := false.B
        wready := true.B
      }
    }
    is(sWriteData){
      when(io.ctl.w.wvalid && wready){
        state := sWriteResp
        wready := false.B
        bvalid := true.B
      }
    }
    is(sWriteResp){
      when(io.ctl.b.bready && bvalid){
        state := sIdle
        bvalid := false.B
      }
    }
  }
}
