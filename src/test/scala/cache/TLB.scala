package armflex.cache

import chisel3._
import chisel3.util._

import chiseltest._
import chiseltest.experimental._
import org.scalatest.FreeSpec
import chisel3.util.experimental.loadMemoryFromFile

object TLBTestUtility{

class DelayChain[T <: Data](in: T, level: Integer) extends MultiIOModule {
  val i = IO(Flipped(Decoupled(in.cloneType)))
  val o = IO(Decoupled(in.cloneType))
  val connections = Wire(Vec(level + 1, Decoupled(in.cloneType)))
  connections(0) <> i
  for(i <- 1 to level){
    connections(i) <> Queue(connections(i-1), 1)
  }
  o <> connections(level)
}

class DUTTLB(
  parent: () => BaseTLB,
  initialMem: String = ""
) extends MultiIOModule {
  val u_tlb = Module(parent())

  val delay_chain_req = Module(new DelayChain(u_tlb.backend_request_o.bits.cloneType, 4))
  delay_chain_req.i <> u_tlb.backend_request_o
  
  val delay_chain_rep = Module(new DelayChain(u_tlb.backend_reply_i.bits.cloneType, 4))
  delay_chain_rep.o <> u_tlb.backend_reply_i


  val frontendRequest_i = IO(Flipped(u_tlb.frontend_request_i.cloneType))
  frontendRequest_i <> u_tlb.frontend_request_i

  val flushRequest_i = IO(Flipped(u_tlb.flush_request_i.cloneType))
  flushRequest_i <> u_tlb.flush_request_i

  val frontendReply_o = IO(u_tlb.frontend_reply_o.cloneType)
  frontendReply_o <> u_tlb.frontend_reply_o

  val packetArrive_o = IO(u_tlb.packet_arrive_o.cloneType)
  packetArrive_o <> u_tlb.packet_arrive_o

  val violation_o = IO(u_tlb.violation_o.cloneType)
  violation_o <> u_tlb.violation_o

  delay_chain_rep.i.valid := delay_chain_req.o.valid && !delay_chain_req.o.bits.w_v
  delay_chain_rep.i.bits.tag := delay_chain_req.o.bits.tag

  delay_chain_req.o.ready := delay_chain_rep.i.ready

  val u_mem = Mem((1l << u_tlb.param.addressWidth), new TLBEntryPacket(u_tlb.param))

  if(initialMem.nonEmpty) loadMemoryFromFile(u_mem, initialMem)

  val mem_port = u_mem(delay_chain_req.o.bits.tag.asUInt())
  val mod_mem_value = WireInit(mem_port)
  mod_mem_value.modified := true.B
  when(delay_chain_req.o.valid && delay_chain_req.o.bits.w_v){
    mem_port := mod_mem_value
  }
  delay_chain_rep.i.bits.data := mem_port
}

implicit class BaseTLBDriver(target: DUTTLB){
  def setReadRequest(vpage: UInt, threadID: UInt) = {
    target.frontendRequest_i.bits.tag.vpage.poke(vpage)
    target.frontendRequest_i.bits.tag.thread_id.poke(threadID)
    target.frontendRequest_i.bits.w_v.poke(false.B)
    target.frontendRequest_i.valid.poke(true.B)
    target.frontendRequest_i.ready.expect(true.B)
  }

  def setWriteRequest(vpage: UInt, threadID: UInt) = {
    target.frontendRequest_i.bits.tag.vpage.poke(vpage)
    target.frontendRequest_i.bits.tag.thread_id.poke(threadID)
    target.frontendRequest_i.bits.w_v.poke(true.B)
    target.frontendRequest_i.valid.poke(true.B)
    target.frontendRequest_i.ready.expect(true.B)
  }

  def setFlushRequest(vpage: UInt, threadID: UInt) = {
    target.flushRequest_i.bits.vpage.poke(vpage)
    target.flushRequest_i.bits.thread_id.poke(threadID)
    target.flushRequest_i.valid.poke(true.B)
    target.flushRequest_i.ready.expect(true.B)
  }

  def clearRequest() = {
    target.frontendRequest_i.valid.poke(false.B)
    target.flushRequest_i.valid.poke(false.B)
  }

  def waitForArrive(expectThreadID: UInt) = {
    do{
      target.tick()
    } while(!target.packetArrive_o.valid.peek.litToBoolean)
    target.packetArrive_o.bits.expect(expectThreadID)
    target.tick()
  }

  def expectReply(violation: Boolean, hit: Boolean, ppn: UInt) = {
    target.frontendReply_o.valid.expect(true.B)
    target.frontendReply_o.bits.violation.expect(violation.B)
    target.frontendReply_o.bits.hit.expect(hit.B)
    if(!violation && hit)
      target.frontendReply_o.bits.entry.pp.expect(ppn)
  }

  def tick(step: Int = 1){
    //import armflex.util.SimTools._
    //logWithCycle("Tick.")
    target.clock.step(step)
  }
}

class TLBPlusCache(
  parentTLB: () => BaseTLB,
  parentCache: () => BaseCache,
  initialMemFile: String = "",
  initialMapping: String = ""
) extends MultiIOModule {

}


}


import TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.internal.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation

class TLBTester extends FreeSpec with ChiselScalatestTester {
  val param = new TLBParameter(
    8, 4, 2, 1, 32, true
  )

  import TLBTestUtility._

  "Normal Access" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/tlb/normal_read"), WriteVcdAnnotation)
    test(new DUTTLB(
      () => new BaseTLB(param, () => new PseudoTreeLRUCore(param.associativity)), ""
    )).withAnnotations(anno){ dut =>
      dut.setReadRequest(0.U, 0.U)
      dut.expectReply(false, false, 0.U)
      dut.tick()
      dut.clearRequest()
      dut.waitForArrive(0.U)

      dut.setReadRequest(0.U, 0.U)
      dut.expectReply(false, true, 0.U)
    }
  }
}