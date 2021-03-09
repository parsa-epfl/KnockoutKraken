/*
MIT License

Copyright (c) 2019 Antmicro

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
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

import chisel3._
import chisel3.util.log2Ceil

class CSR(val dataWidth: Int, val regCount: Int) extends Module{
  val io = IO(new Bundle{
    val csr: Vec[CSRRegBundle] = Vec(regCount, new CSRRegBundle(dataWidth))
    val bus = Flipped(new CSRBusBundle(dataWidth, regCount))
  })

  val data = WireInit(0.U(dataWidth.W))

  io.bus.dataIn := data

  for(i <- 0 until regCount){
    when(io.bus.addr === i.U && io.bus.read){
      data := io.csr(i).dataIn
      io.csr(i).dataRead := true.B
    }.otherwise{
      io.csr(i).dataRead := false.B
    }

    when(io.bus.addr === i.U && io.bus.write){
      io.csr(i).dataOut := io.bus.dataOut
      io.csr(i).dataWrite := true.B
    }.otherwise{
      io.csr(i).dataWrite := false.B
      io.csr(i).dataOut := 0.U
    }
  }
}

import armflex.util.{BRAMPort, BRAMConfig}
import chisel3.util.{Cat, Fill}
class CSR2BRAM(val cfg: BRAMConfig) extends Module{
  assert(cfg.NB_COL == 8)
  val io = IO(new Bundle{
    val bus = Flipped(new CSRBusBundle(32, cfg.RAM_DEPTH))
    val port = Flipped(new BRAMPort()(cfg))
  })

  io.port.ADDR := io.bus.addr >> 1.U // Take upper bits
  io.port.EN := io.bus.read || io.bus.write
  io.port.DI := Cat(io.bus.dataOut, io.bus.dataOut)
  val mask = Fill(4, io.bus.write.asUInt)
  io.port.WE := Mux(io.bus.addr(0), mask << 4, mask)
  io.bus.dataIn := Mux(RegNext(io.bus.addr(0)), io.port.DO(63,32) , io.port.DO(31, 0))
}