package trial

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class TrialUnitTester(c: Trial) extends PeekPokeTester(c) {
  poke(c.io.in, 0xFF)
  step(1)
  val sObj = c.io.out
  expect(sObj.a, 0x0F)
  expect(sObj.b, 0x00)
}

class TrialTester extends ChiselFlatSpec {
  private val backendNames = if(firrtl.FileUtils.isCommandAvailable("verilator")) {
    Array("firrtl", "verilator")
  }
  else {
    Array("firrtl")
  }
  for ( backendName <- backendNames ) {
    "Trial" should s"test some stuff (with $backendName)" in {
      Driver(() => new Trial, backendName) {
        c => new TrialUnitTester(c)
      } should be (true)
    }
  }

//  "Basic test using Driver.execute" should "be used as an alternative way to run specification" in {
//    iotesters.Driver.execute(Array(), () => new Trial) {
//      c => new TrialUnitTester(c)
//    } should be (true)
//  }
//
//  "using --backend-name verilator" should "be an alternative way to run using verilator" in {
//    if(backendNames.contains("verilator")) {
//      iotesters.Driver.execute(Array("--backend-name", "verilator"), () => new Trial) {
//        c => new TrialUnitTester(c)
//      } should be(true)
//    }
//  }
//
//  "running with --is-verbose" should "show more about what's going on in your tester" in {
//    iotesters.Driver.execute(Array("--is-verbose"), () => new Trial) {
//      c => new TrialUnitTester(c)
//    } should be(true)
//  }
//
//  "running with --fint-write-vcd" should "create a vcd file from your test" in {
//    iotesters.Driver.execute(Array("--fint-write-vcd"), () => new Trial) {
//      c => new TrialUnitTester(c)
//    } should be(true)
//  }
//
//  "using --help" should s"show the many options available" in {
//    iotesters.Driver.execute(Array("--help"), () => new Trial) {
//      c => new TrialUnitTester(c)
//    } should be (true)
//  }
}
