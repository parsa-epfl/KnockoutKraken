package armflex.util

import chisel3._
import chisel3.util._

import chiseltest._
import chiseltest.experimental._

import TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.internal.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation


import org.scalatest.FreeSpec

import DMAController.Bus._
import DMAController.Frontend._

class AXIRAMControllerDUT extends MultiIOModule {
  val u_read_dma = Module(new AXI4Reader(32, 32))
  val u_write_dma = Module(new AXI4Writer(32, 32))
  val u_dut = Module(new AXIRAMController(32, 32))

  val ram_read_request_o = IO(u_dut.read_request_o.cloneType)
  ram_read_request_o <> u_dut.read_request_o

  val ram_read_reply_i = IO(Flipped(u_dut.read_reply_i.cloneType))
  ram_read_reply_i <> u_dut.read_reply_i

  val ram_write_request_o = IO(u_dut.write_request_o.cloneType)
  ram_write_request_o <> u_dut.write_request_o

  u_read_dma.io.bus.ar <> u_dut.S_AXI.ar
  u_read_dma.io.bus.aw <> AXI4AW.stub(32)
  u_read_dma.io.bus.r <> u_dut.S_AXI.r
  u_read_dma.io.bus.w <> AXI4W.stub(32)
  u_read_dma.io.bus.b <> AXI4B.stub()
  

  val read_dma_data_o = IO(u_read_dma.io.dataOut.cloneType)
  read_dma_data_o <> Queue(u_read_dma.io.dataOut, 2)

  val read_dma_config_i = IO(Flipped(u_read_dma.io.xfer.cloneType))
  read_dma_config_i <> u_read_dma.io.xfer

  u_write_dma.io.bus.ar <> AXI4AR.stub(32)
  u_write_dma.io.bus.aw <> u_dut.S_AXI.aw
  u_write_dma.io.bus.r <> AXI4R.stub(32)
  u_write_dma.io.bus.w <> u_dut.S_AXI.w
  u_write_dma.io.bus.b <> u_dut.S_AXI.b

  val write_dma_data_i = IO(Flipped(u_write_dma.io.dataIn.cloneType))
  u_write_dma.io.dataIn <> Queue(write_dma_data_i, 2)

  val write_dma_config_i = IO(Flipped(u_write_dma.io.xfer.cloneType))
  write_dma_config_i <> u_write_dma.io.xfer
}

class AXIRAMControllerTester extends FreeSpec with ChiselScalatestTester {
  "Read" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/util/axi_memory/read"), WriteVcdAnnotation)
    test(new AXIRAMControllerDUT).withAnnotations(anno){ dut =>
      timescope {
        dut.read_dma_config_i.address.poke(0x10.U)
        dut.read_dma_config_i.length.poke(8.U)
        dut.read_dma_config_i.valid.poke(true.B)
        dut.clock.step()
      }
      
      // Wait for the read request from the data.
      while(dut.ram_read_request_o.valid.peek.litToBoolean != true){
        dut.clock.step()
      }

      timescope {
        dut.ram_read_request_o.ready.poke(true.B)
        for (i <- 0 until 8){
          // check the 8 requests.
          dut.ram_read_request_o.valid.expect(true.B)
          dut.ram_read_request_o.bits.expect((0x10 + i * 4).U)
          
          dut.clock.step()
        }
      }

      fork {
        timescope {
          dut.read_dma_data_o.ready.poke(true.B)
          dut.clock.step(8)
        }
      }

      // 8 requests are checked. Now it's time to return the data.
      fork {
        timescope {
          dut.ram_read_reply_i.valid.poke(true.B)
          for (i <- 0 until 8){
            dut.ram_read_reply_i.ready.expect(true.B)
            dut.ram_read_reply_i.bits.poke((i * 32).U)
            dut.clock.step()
          }
        }
      }
      
      // Let's check the result from the DMA
      fork {
        dut.clock.step()
        for(i <- 0 until 8){
          dut.read_dma_data_o.valid.expect(true.B)
          dut.read_dma_data_o.bits.expect((i * 32).U)
          dut.clock.step()
        }
      }.join()
    }
  }

  "Write" in {
    // Configure
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/util/axi_memory/write"), WriteVcdAnnotation)
    test(new AXIRAMControllerDUT).withAnnotations(anno){ dut =>
      // 1. config
      timescope {
        dut.write_dma_config_i.address.poke(0xA0.U)
        dut.write_dma_config_i.length.poke(16.U)
        dut.write_dma_config_i.valid.poke(true.B)
        dut.clock.step()
      }

      dut.write_dma_data_i.initSource()
      dut.write_dma_data_i.setSourceClock(dut.clock)
      // 2. place the data.
      fork {
        dut.write_dma_data_i.enqueueSeq(Seq.tabulate(16)({ i =>
          (i * 16).U
        }))
      }

      // 3. monitor the write request
      fork {
        while(!dut.ram_write_request_o.valid.peek.litToBoolean) dut.clock.step()
        timescope {
          dut.ram_write_request_o.ready.poke(true.B)
          for(i <- 0 until 16){
            dut.ram_write_request_o.bits.addr.expect((0xA0 + i * 4).U)
            dut.ram_write_request_o.bits.data.expect((i * 16).U)
            dut.ram_write_request_o.bits.mask.expect(0xF.U)
            dut.ram_write_request_o.valid.expect(true.B)
            dut.clock.step()
          }
        }
      }.join()
    }
  }
}
