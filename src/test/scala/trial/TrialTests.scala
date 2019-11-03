/* LEGACY TESTS
package trial

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import utils.AssemblyParser

class TrialUnitTesterRotate(c: TrialRotate) extends PeekPokeTester(c)
{
  poke(c.io.in, 0x1)
  poke(c.io.rot, 0)
  step(1)
  expect(c.io.out, 0x1)

  poke(c.io.in, 0x1)
  poke(c.io.rot, 1)
  step(1)
  expect(c.io.out, 0x8)

  poke(c.io.in, 0x01)
  poke(c.io.rot, 2)
  step(1)
  expect(c.io.out, 0x4)

  poke(c.io.in, 0x01)
  poke(c.io.rot, 3)
  step(1)
  expect(c.io.out, 0x2)
}

class TrialUnitTesterVecs(c: TrialVecs) extends PeekPokeTester(c)
{
  for(i <- 0 to 10 ) {
    expect(c.io.out, i * (i%4))
    expect(c.io.out0, i * 0)
    expect(c.io.out1, i * 1)
    expect(c.io.out2, i * 2)
    expect(c.io.out3, i * 3)
    step(1)
  }
}

class TrialUnitPriority(c: TrialPriority) extends PeekPokeTester(c)
{

  for(i <- 0 to 3 ) {
    poke(c.io.in, i)
    step(1)
    expect(c.io.out, 0)
    expect(c.io.out0, if(i==0) 0 else (0 + 1))
    expect(c.io.out1, if(i==1) 0 else (1 + 1))
    expect(c.io.out2, if(i==2) 0 else (2 + 1))
    expect(c.io.out3, if(i==3) 0 else (3 + 1))
  }
}

class TrialTester extends ChiselFlatSpec {
  private val backendNames = if(firrtl.FileUtils.isCommandAvailable("verilator")) {
    Array("firrtl", "verilator")
  }
  else {
    Array("firrtl")
  }
  for ( backendName <- backendNames ) {
    "TrialRotate" should s"test right rotate function from alu and priority enconder (with $backendName)" in {
      Driver(() => new TrialRotate, backendName) {
        c => new TrialUnitTesterRotate(c)
      } should be (true)
    }
  }
  for ( backendName <- backendNames ) {
    "TrialVecs" should s"test how to create registered vectors and index (with $backendName)" in {
      Driver(() => new TrialVecs, backendName) {
        c => new TrialUnitTesterVecs(c)
      } should be (true)
    }
  }
  for ( backendName <- backendNames ) {
    "TrialPriority" should s"test how chisel assignement works with linear overwriting assignements (with $backendName)" in {
      Driver(() => new TrialPriority, backendName) {
        c => new TrialUnitPriority(c)
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
} // */
