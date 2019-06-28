package protoflex

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import common.{BRAMConfig, constBRAM, BRAMTestHelper}



class TransplantTest(c: TransplantUnit) extends PeekPokeTester(c) {
  // testing write back
  poke(c.io.bram_port.dataOut.get, 0)
  println(" not req")
  poke(c.io.tp_req, 0)
  step(2)
  println(" tp req ")
  poke(c.io.tp_req, 1)
  step(1)
  poke(c.io.start, 0)
  step(100)

  // testing initializatoin
  //  println(" not start")
  //  poke(c.io.start, 0)
  //  step(2)
  //  println(" starting ")
  //  poke(c.io.start, 1)
  //  step(1)
  //  poke(c.io.start, 0)
  //  step(100)



}

class TransplantTester extends ChiselFlatSpec{
  implicit val stateBRAMc = new BRAMConfig(Seq(constBRAM.TDPBRAM36ParamDict(36), constBRAM.TDPBRAM36ParamDict(36)))

  behavior of "transplant unit"

  backends foreach {backend =>
    it should s"Transplant testing with ${backend}" in {
      Driver(() => new TransplantUnit, backend)((c)=> new TransplantTest(c)) should be (true)
    }
  }
}

object TransplantMain extends App {
  implicit val stateBRAMc = new BRAMConfig(Seq(constBRAM.TDPBRAM36ParamDict(36), constBRAM.TDPBRAM36ParamDict(36)))

  val chiselArgs = Array("-tn", "TransplantUnit", "-td","./test_result", "--backend-name", "verilator")
  iotesters.Driver.execute(chiselArgs, () => new TransplantUnit) {
    c => new TransplantTest(c)
  }
}

