package armflex

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental._
import org.scalatest._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.internal.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation
import TestOptionBuilder._

//import armflex.util.SimTools._

class RequestAdaptorTester extends FreeSpec with ChiselScalatestTester {
  // val param = new MemorySystemParameter()
  import CacheInterfaceAdaptors._
  import arm.DECODE_CONTROL_SIGNALS._

  "Normal read" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/memory_request_adaptor/normal"), WriteVcdAnnotation)
    test(new CacheRequestAdaptor(2, 64, 512)).withAnnotations(anno){ dut =>

      dut.o.ready.poke(true.B)

      // a normal read request
      dut.i.bits.isLoad.poke(true.B)
      dut.i.bits.isPair.poke(false.B)
      dut.i.bits.size.poke(SIZE64)
      dut.i.bits.tag.poke(0.U)
      dut.i.bits.memReq(0).addr.poke(0.U)
      dut.i.bits.memReq(0).data.poke(0.U)
      dut.i.bits.memReq(1).addr.poke(0.U)
      dut.i.bits.memReq(1).data.poke(0.U)
      dut.i.valid.poke(true.B)
      dut.i.ready.expect(true.B)

      dut.clock.step()

      dut.i.valid.poke(false.B)

      // check o
      dut.o.valid.expect(false.B)
      
      // check sync_message_o
      dut.sync_message_o.valid.expect(true.B)
      
      dut.clock.step()
    }
  }

  "Pair Load" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/memory_request_adaptor/pair"), WriteVcdAnnotation)
    test(new CacheRequestAdaptor(2, 64, 512)).withAnnotations(anno){ dut =>
      dut.o.ready.poke(true.B)

      dut.i.bits.isLoad.poke(true.B)
      dut.i.bits.isPair.poke(true.B)
      dut.i.bits.size.poke(SIZE64)
      dut.i.bits.memReq(0).addr.poke(56.U)
      dut.i.bits.memReq(1).addr.poke(64.U)

      dut.i.valid.poke(true.B)
      dut.i.ready.expect(true.B)

      dut.o.valid.expect(true.B)
      dut.o.bits.addr.expect(0.U)

      dut.clock.step()
      dut.i.valid.poke(false.B)
      dut.i.ready.expect(false.B)

      dut.o.valid.expect(true.B)
      dut.o.bits.permission.expect(0.U)
      dut.o.bits.addr.expect(1.U)

      dut.clock.step()

      dut.sync_message_o.valid.expect(true.B)
      dut.sync_message_o.bits.bias_addr.expect(56.U)
      dut.sync_message_o.bits.order.expect(0.U)
      dut.sync_message_o.bits.pair_v.expect(true.B)
      dut.sync_message_o.ready.poke(true.B)

      dut.clock.step()

      dut.sync_message_o.valid.expect(true.B)
      dut.sync_message_o.bits.bias_addr.expect(0.U)

      dut.clock.step()

      dut.sync_message_o.valid.expect(false.B)

    }
  }

  "Data placement" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/memory_request_adaptor/data_placement"), WriteVcdAnnotation)
    test(new CacheRequestAdaptor(2, 64, 512)).withAnnotations(anno){ dut =>
      dut.o.ready.poke(true.B)

      // a normal read request
      dut.i.bits.isLoad.poke(false.B)
      dut.i.bits.isPair.poke(false.B)
      dut.i.bits.size.poke(SIZEB)
      dut.i.bits.tag.poke(0.U)
      dut.i.bits.memReq(0).addr.poke(3.U)
      dut.i.bits.memReq(0).data.poke(128.U)
      dut.i.bits.memReq(1).addr.poke(0.U)
      dut.i.bits.memReq(1).data.poke(0.U)
      dut.i.valid.poke(true.B)
      dut.i.ready.expect(true.B)

      dut.o.valid.expect(true.B)
      dut.o.bits.permission.expect(1.U)
      dut.o.bits.wData.expect((128l << 24).U)
      dut.o.bits.wMask.expect((255l << 24).U)
    }
  }
}

// Cases:
// 1 | 2
// M | M
// H | M
// M | H
// M | M
class ReplyAdaptorTester extends FreeSpec with ChiselScalatestTester {
  import CacheInterfaceAdaptors._
  import arm.DECODE_CONTROL_SIGNALS._

  "Pair" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/memory_reply_adaptor/pair"), WriteVcdAnnotation)
    test(new CacheReplyAdaptor(2, 64, 512)).withAnnotations(anno){ dut =>
      // setup the sync_message_i
      //dut.sync_message_i.setSinkClock(dut.clock)
      dut.sync_message_i.bits.bias_addr.poke(2.U)
      dut.sync_message_i.bits.order.poke(0.U)
      dut.sync_message_i.bits.pair_v.poke(true.B)
      dut.sync_message_i.bits.size.poke(SIZEB)
      dut.sync_message_i.valid.poke(true.B)

      dut.cache_reply_i.bits.data.poke(0x09ABCDEF.U)
      dut.cache_reply_i.bits.hit.poke(true.B)
      dut.cache_reply_i.valid.poke(true.B)

      dut.clock.step()

      dut.data_o.valid.expect(true.B)
      dut.data_o.bits.data.expect(0x00AB.U)

      dut.sync_message_i.bits.bias_addr.poke(3.U)
      dut.sync_message_i.bits.order.poke(1.U)
      dut.sync_message_i.bits.pair_v.poke(true.B)
      dut.sync_message_i.bits.size.poke(SIZEB)
      dut.sync_message_i.valid.poke(true.B)

      dut.cache_reply_i.bits.data.poke(0x09ABCDEF.U)
      dut.cache_reply_i.bits.hit.poke(true.B)
      dut.cache_reply_i.valid.poke(true.B)

      // expect result?
      dut.data_o.valid.expect(true.B)
      dut.data_o.bits.data.expect(0x0009.U)
    }
  }
}
