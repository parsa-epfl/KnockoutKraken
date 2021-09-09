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

import chisel3._
import chisel3.util.{log2Ceil, MixedVec, PopCount}

class CSRBusBundle(val dataWidth: Int, val regCount: Int) extends Bundle {
  val addr = Output(UInt(log2Ceil(regCount).W))
  val dataOut = Output(UInt(dataWidth.W))
  val dataIn = Input(UInt(dataWidth.W))
  val write = Output(Bool())
  val read = Output(Bool())
}

// Below here it does not belong to Antmicro
case class CSRBusSlave(val addrStart: Int, val regCount: Int)

class CSRBusMasterToNSlaves(val dataWidth: Int, val slaves: Seq[CSRBusSlave], val addrRange: (Int, Int)) extends MultiIOModule {
  val slavesBus = IO(MixedVec(slaves.map(slave => new CSRBusBundle(dataWidth, slave.regCount))))
  val masterBus = IO(Flipped(new CSRBusBundle(dataWidth, addrRange._2 - addrRange._1)))
  private val dataIn = WireInit(masterBus.dataOut.cloneType, 0xDEAD2BAD.S.asUInt)
  private val matches = Wire(Vec(slaves.length, Bool()))

  masterBus.dataIn := dataIn
  for((slave, idx) <- slaves.zipWithIndex) {
    slavesBus(idx).dataOut := masterBus.dataOut
    slavesBus(idx).addr := masterBus.addr - slave.addrStart.U
    when(slave.addrStart.U <= masterBus.addr && masterBus.addr < (slave.addrStart + slave.regCount).U) {
      dataIn := slavesBus(idx).dataIn
      slavesBus(idx).read := masterBus.read
      slavesBus(idx).write := masterBus.write
      matches(idx) := true.B
    }.otherwise {
      slavesBus(idx).read := false.B
      slavesBus(idx).write := false.B
      matches(idx) := false.B
    }
  }

  if(true) { // TODO Conditional assertions
    for(slave <- slaves) { assert(addrRange._1 <= slave.addrStart && slave.addrStart + slave.regCount <= addrRange._2, "Addressed must be between allocated range") }
    assert(PopCount(matches.asUInt) <= 1.U, "It's impossible to hit multiple entries for CSR bus.")
    when(masterBus.read || masterBus.write) {
      assert(PopCount(matches.asUInt) === 1.U, "If an operation is performed, the accessed address must match one and only one of the slaves") 
    }
  }
}
