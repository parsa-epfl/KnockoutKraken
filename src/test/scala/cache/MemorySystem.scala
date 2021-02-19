package armflex.cache

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
  val param = new MemorySystemParameter()
  import CacheInterfaceAdaptors._
  import arm.DECODE_CONTROL_SIGNALS._

  "Normal read" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/memory_request_adaptor/normal"), WriteVcdAnnotation)
    test(new CacheRequestAdaptor(param)).withAnnotations(anno){ dut =>

      dut.o.ready.poke(true.B)

      // a normal read request
      dut.i.bits.isLoad.poke(true.B)
      dut.i.bits.isPair.poke(false.B)
      dut.i.bits.size.poke(SIZE64)
      dut.i.bits.tag.get.poke(0.U)
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
    test(new CacheRequestAdaptor(param)).withAnnotations(anno){ dut =>
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
      dut.o.bits.w_v.expect(false.B)
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
    test(new CacheRequestAdaptor(param)).withAnnotations(anno){ dut =>
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
      dut.o.bits.w_v.expect(true.B)
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
  val param = new MemorySystemParameter()
  import CacheInterfaceAdaptors._
  import arm.DECODE_CONTROL_SIGNALS._

  "Pair" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/memory_reply_adaptor/pair"), WriteVcdAnnotation)
    test(new CacheReplyAdaptor(param)).withAnnotations(anno){ dut =>
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

object DataMemorySystemTestUtility {

class DelayChain[T <: Data](t: T, level: Integer) extends MultiIOModule {
  val i = IO(Flipped(Decoupled(t)))
  val o = IO(Decoupled(t))
  val connections = Wire(Vec(level + 1, Decoupled(t)))
  connections(0) <> i
  for(i <- 1 to level){
    connections(i) <> Queue(connections(i-1), 1)
  }
  o <> connections(level)
}


class DataMemorySystemDUT extends MultiIOModule {

  val param = new MemorySystemParameter(
    20,
    16,
    4096,
    4,
    16,
    8,
    1024,
    4,
    512
  )

  val u_system = Module(new DataMemorySystem(param))

  val u_cache_request_delay_chain = Module(new DelayChain(u_system.cache_backend_request_o.bits.cloneType, 10))
  u_system.cache_backend_request_o <> u_cache_request_delay_chain.i
  val u_cache_reply_delay_chain = Module(new DelayChain(u_system.cache_backend_reply_i.bits.cloneType, 10))
  u_system.cache_backend_reply_i <> u_cache_reply_delay_chain.o
  val u_tlb_request_delay_chain = Module(new DelayChain(u_system.tlb_backend_request_o.bits.cloneType, 20))
  u_system.tlb_backend_request_o <> u_tlb_request_delay_chain.i
  val u_tlb_reply_delay_chain = Module(new DelayChain(u_system.tlb_backend_reply_i.bits.cloneType, 20))
  u_system.tlb_backend_reply_i <> u_tlb_reply_delay_chain.o

  // Cache mapping rule: a simple memory with everything empty.
  val mem = Mem(1024, UInt(512.W))
  u_cache_reply_delay_chain.i.bits.addr := u_cache_request_delay_chain.o.bits.addr
  u_cache_reply_delay_chain.i.bits.data := mem(u_cache_request_delay_chain.o.bits.addr)
  u_cache_reply_delay_chain.i.bits.not_sync_with_data_v := false.B
  u_cache_reply_delay_chain.i.bits.thread_id := u_cache_request_delay_chain.o.bits.thread_id
  u_cache_reply_delay_chain.i.valid := u_cache_request_delay_chain.o.valid && !u_cache_request_delay_chain.o.bits.w_v
  u_cache_request_delay_chain.o.ready := true.B
  when(u_cache_request_delay_chain.o.valid && u_cache_request_delay_chain.o.bits.w_v){
    mem(u_cache_request_delay_chain.o.bits.addr) := u_cache_request_delay_chain.o.bits.data
  }

  // TLB Rule: xor the higher part and lower part. (How to handle the Thread ID?)
  def tlbMappingFunction(tag: TLBTagPacket): UInt = {
    assert(tag.thread_id.getWidth == 2)
    assert(tag.vpage.getWidth == 8)
    (tag.vpage(7, 4) ^ tag.vpage(3, 0)) + tag.thread_id
  }

  val tlb_modified = Mem(1024, Bool())

  u_tlb_reply_delay_chain.i.bits.tag := u_tlb_request_delay_chain.o.bits.tag
  u_tlb_reply_delay_chain.i.bits.data.pp := tlbMappingFunction(u_tlb_request_delay_chain.o.bits.tag)
  u_tlb_reply_delay_chain.i.bits.data.modified := tlb_modified(Cat(
    u_tlb_request_delay_chain.o.bits.tag.thread_id,
    u_tlb_request_delay_chain.o.bits.tag.vpage
  ))
  u_tlb_reply_delay_chain.i.bits.data.permission := 1.U
  
}

implicit class DataMemorySystemInterface(dut: DataMemorySystemDUT) {
  
}


}

class DataMemorySystemTester extends FreeSpec with ChiselScalatestTester {
  val param = new MemorySystemParameter()
  "Generate Verilog" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/memory_system/for_data"), WriteVcdAnnotation)
    test(new DataMemorySystem(param)).withAnnotations(anno){ dut =>
      
    }
  }
}