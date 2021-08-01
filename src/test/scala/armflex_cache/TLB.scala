package armflex_cache

import armflex.PTEntryPacket
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
  parent: () => BRAMTLB,
  initialMem: String = ""
) extends MultiIOModule {
  val u_tlb = Module(parent())

  val delay_chain_req = Module(new DelayChain(u_tlb.miss_request_o.bits.cloneType, 4))
  delay_chain_req.i <> u_tlb.miss_request_o
  
  val delay_chain_rep = Module(new DelayChain(u_tlb.refill_request_i.bits.cloneType, 4))
  delay_chain_rep.o <> u_tlb.refill_request_i


  val frontendRequest_i = IO(Flipped(u_tlb.frontend_request_i.cloneType))
  frontendRequest_i <> u_tlb.frontend_request_i

  val flushRequest_i = IO(Flipped(u_tlb.flush_request_i.cloneType))
  flushRequest_i <> u_tlb.flush_request_i

  val frontendReply_o = IO(u_tlb.frontend_reply_o.cloneType)
  frontendReply_o <> u_tlb.frontend_reply_o

  val packetArrive_o = IO(u_tlb.packet_arrive_o.cloneType)
  packetArrive_o <> u_tlb.packet_arrive_o

  delay_chain_rep.i.valid := delay_chain_req.o.valid && !delay_chain_req.o.bits.w_v
  delay_chain_rep.i.bits.thid := delay_chain_req.o.bits.thid
  delay_chain_rep.i.bits.tag := delay_chain_req.o.bits.tag

  delay_chain_req.o.ready := delay_chain_rep.i.ready

  val u_mem = Mem(1L << u_tlb.params.vPageW, new PTEntryPacket(u_tlb.params))

  if(initialMem.nonEmpty) loadMemoryFromFile(u_mem, initialMem)

  val mem_port = u_mem(delay_chain_req.o.bits.tag.asUInt)
  val mod_mem_value = WireInit(mem_port)
  mod_mem_value.modified := true.B
  when(delay_chain_req.o.valid && delay_chain_req.o.bits.w_v){
    mem_port := mod_mem_value
  }
  delay_chain_rep.i.bits.data := mem_port
}

implicit class BaseTLBDriver(target: DUTTLB){
  def setReadRequest(vpage: UInt, asid: UInt) = {
    target.frontendRequest_i.bits.tag.vpn.poke(vpage)
    target.frontendRequest_i.bits.thid.poke(asid)
    target.frontendRequest_i.bits.tag.asid.poke(asid)
    target.frontendRequest_i.bits.perm.poke(0.U)
    target.frontendRequest_i.valid.poke(true.B)
    target.frontendRequest_i.ready.expect(true.B)
  }

  def setWriteRequest(vpage: UInt, asid: UInt) = {
    target.frontendRequest_i.bits.tag.vpn.poke(vpage)
    target.frontendRequest_i.bits.thid.poke(vpage)
    target.frontendRequest_i.bits.tag.asid.poke(asid)
    target.frontendRequest_i.bits.perm.poke(1.U)
    target.frontendRequest_i.valid.poke(true.B)
    target.frontendRequest_i.ready.expect(true.B)
  }

  def setFlushRequest(vpage: UInt, asid: UInt) = {
    target.flushRequest_i.bits.vpn.poke(vpage)
    target.flushRequest_i.bits.asid.poke(asid)
    target.flushRequest_i.valid.poke(true.B)
    target.flushRequest_i.ready.expect(true.B)
  }

  def clearRequest() = {
    target.frontendRequest_i.valid.poke(false.B)
    target.flushRequest_i.valid.poke(false.B)
  }

  def waitForArrive(expectAsid: UInt) = {
    do{
      target.tick()
    } while(!target.packetArrive_o.valid.peek.litToBoolean)
    target.packetArrive_o.bits.expect(expectAsid)
    target.tick()
  }

  def expectReply(violation: Boolean, hit: Boolean, ppn: UInt) = {
    target.frontendReply_o.valid.expect(true.B)
    target.frontendReply_o.bits.violation.expect(violation.B)
    target.frontendReply_o.bits.hit.expect(hit.B)
    if(!violation && hit)
      target.frontendReply_o.bits.entry.ppn.expect(ppn)
  }

  def tick(step: Int = 1){
    //import armflex.util.SimTools._
    //logWithCycle("Tick.")
    target.clock.step(step)
  }
}
}


import TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.internal.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation

class TLBTester extends FreeSpec with ChiselScalatestTester {
  val param = new PageTableParams(
    8, 4, 2, 15, 1, 32, 32
  )

  import TLBTestUtility._

  "Normal Access" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/tlb/normal_read"), WriteVcdAnnotation)
    test(new DUTTLB(
      () => new BRAMTLB(param, () => new PseudoTreeLRUCore(param.tlbAssociativity)), ""
      )).withAnnotations(anno){ dut =>
      dut.setReadRequest(0.U, 0.U)
      dut.tick()
      dut.expectReply(violation = false, hit = false, 0.U)
      dut.tick()
      dut.clearRequest()
      dut.waitForArrive(0.U)

      dut.setReadRequest(0.U, 0.U)
      dut.tick()
      dut.clearRequest()
      dut.expectReply(violation = false, hit = true, 0.U)
    }
  }
}