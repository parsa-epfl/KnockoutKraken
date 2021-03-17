package armflex.demander

import peripheral._
import armflex.util._

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import antmicro.Bus._

import chiseltest._
import chiseltest.experimental._

import TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.internal.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation


import org.scalatest.FreeSpec
import armflex.cache.MemorySystemParameter

class FreeListDUT(param: PageDemanderParameter) extends MultiIOModule {
  val u_fl = Module(new FreeList(param))
  val u_axi_read = Module(new AXIReadMultiplexer(
    param.dramAddrWidth,
    param.dramDataWidth,
    1
  ))

  u_fl.M_DMA_R <> u_axi_read.S_IF(0)

  val u_axi_write = Module(new AXIWriteMultiplexer(
    param.dramAddrWidth,
    param.dramDataWidth,
    1
  ))

  u_fl.M_DMA_W <> u_axi_write.S_IF(0)

  val push_i = IO(Flipped(Decoupled(UInt(32.W))))
  u_fl.push_i <> push_i
  val pop_o = IO(Decoupled(UInt(32.W)))
  u_fl.pop_o <> pop_o
  val full_o = IO(Output(Bool()))
  u_fl.full_o <> full_o
  val empty_o = IO(Output(Bool()))
  u_fl.empty_o <> empty_o

  val M_AXI = IO(new AXI4(
    param.dramAddrWidth, 
    param.dramDataWidth
  ))

  M_AXI.ar <> u_axi_read.M_AXI.ar
  M_AXI.r <> u_axi_read.M_AXI.r
  u_axi_read.M_AXI.aw <> AXI4AW.stub(param.dramAddrWidth)
  u_axi_read.M_AXI.w <> AXI4W.stub(param.dramDataWidth)
  u_axi_read.M_AXI.b <> AXI4B.stub()

  M_AXI.aw <> u_axi_write.M_AXI.aw
  M_AXI.w <> u_axi_write.M_AXI.w
  M_AXI.b <> u_axi_write.M_AXI.b
  u_axi_write.M_AXI.ar <> AXI4AR.stub(param.dramAddrWidth)
  u_axi_write.M_AXI.r <> AXI4R.stub(param.dramDataWidth)

}

class FreeListTest extends FreeSpec with ChiselScalatestTester {
  import scala.collection.mutable.Queue

  def waitToBeTrue(clock: Clock, signal: Bool) = {
    println(s"Waiting for ${signal.pathName} to be true")
    while(!signal.peek.litToBoolean){
      clock.step()
    }
  }

  "Fetch" in {
    val anno = Seq(TargetDirAnnotation("test/freelist/fetch"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new FreeListDUT(new PageDemanderParameter())).withAnnotations(anno){ dut =>
      // extract 64 elements
      val elementQueue = new Queue[BigInt]()
      dut.clock.step(5)
      dut.full_o.expect(true.B)
      for(i <- 0 until 64){
        waitToBeTrue(
          dut.clock,
          dut.pop_o.valid
        )
        timescope {
          elementQueue.enqueue(dut.pop_o.bits.peek().litValue())
          dut.pop_o.ready.poke(true.B)
          dut.clock.step()
        }
      }

      // No the set is full.
      // push the element back
      dut.push_i.bits.poke(elementQueue.dequeue().U)
      timescope {
        dut.push_i.valid.poke(true.B)
        waitToBeTrue(dut.clock, dut.push_i.ready)
        dut.clock.step()
      }
      // This should trigger a writeback to the DRAM
      waitToBeTrue(dut.clock, dut.M_AXI.aw.awvalid)
      dut.M_AXI.aw.awaddr.expect((0xC040000 + 4 * 64).U)
      timescope {
        dut.M_AXI.aw.awready.poke(true.B)
        dut.clock.step()
      }
      // wait for the data
      waitToBeTrue(dut.clock, dut.M_AXI.w.wvalid)
      val targetData = Seq.tabulate(16)({i => BigInt(0x10000 + i + 64)}).reverse.reduce {
        (result: BigInt, current: BigInt) => (result << 32) | current
      }
      dut.M_AXI.w.wdata.expect(targetData.U)
      timescope {
        dut.M_AXI.w.wready.poke(true.B)
        dut.clock.step()
      }
      // write reply
      timescope {
        dut.M_AXI.b.bvalid.poke(true.B)
        waitToBeTrue(dut.clock, dut.M_AXI.b.bready)
        dut.clock.step()
      }

      // pop one element
      waitToBeTrue(dut.clock, dut.pop_o.valid)
      dut.pop_o.bits.expect((0x10000).U)
      timescope {
        dut.pop_o.ready.poke(true.B)
        dut.clock.step()
      }

      // It should now fetch the new chunk
      waitToBeTrue(dut.clock, dut.M_AXI.ar.arvalid)
      dut.M_AXI.ar.araddr.expect((0xC040000 + 4 * 64).U)
      timescope {
        dut.M_AXI.ar.arready.poke(true.B)
        dut.clock.step()
      }

      // Reply with data
      dut.M_AXI.r.rdata.poke(targetData.U)
      dut.M_AXI.r.rlast.poke(true.B)
      timescope {
        dut.M_AXI.r.rvalid.poke(true.B)
        waitToBeTrue(dut.clock, dut.M_AXI.r.rready)
        dut.clock.step()
      }

      // pop one
      waitToBeTrue(dut.clock, dut.pop_o.valid)
      dut.pop_o.bits.expect((0x10000 + 64).U)


    }
  }

  
}
