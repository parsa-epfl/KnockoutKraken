package armflex_cache

import armflex.PTEntryPacket
import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.util.experimental.loadMemoryFromFile

import armflex.MemoryAccessType._
import armflex_mmu.peripheral.PageTableOps

object TLBTestUtility {

class DelayChain[T <: Data](in: T, level: Integer) extends Module {
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
  parent: () => TLB,
  initialMem: String = ""
) extends Module {
  val u_tlb = Module(parent())

  val delay_chain_pt_req = Module(new DelayChain(u_tlb.mmu_io.pageTableReq.bits.cloneType, 4))
  delay_chain_pt_req.i <> u_tlb.mmu_io.pageTableReq

  val delay_chain_rep = Module(new DelayChain(u_tlb.mmu_io.refillResp.bits.cloneType, 4))
  delay_chain_rep.o <> u_tlb.mmu_io.refillResp


  val frontendRequest_i = IO(Flipped(u_tlb.pipeline_io.translationReq.cloneType))
  frontendRequest_i <> u_tlb.pipeline_io.translationReq

  val flushRequest_i = IO(Flipped(u_tlb.mmu_io.flush.req.cloneType))
  flushRequest_i <> u_tlb.mmu_io.flush.req

  val frontendReply_o = IO(u_tlb.pipeline_io.translationResp.cloneType)
  frontendReply_o <> u_tlb.pipeline_io.translationResp

  val packetArrive_o = IO(u_tlb.pipeline_io.wakeAfterMiss.cloneType)
  packetArrive_o <> u_tlb.pipeline_io.wakeAfterMiss

  val u_mem = Mem(1L << (u_tlb.params.vPageW + u_tlb.params.asidW), new PTEntryPacket(u_tlb.params))

  if(initialMem.nonEmpty) loadMemoryFromFile(u_mem, initialMem)

  delay_chain_rep.i.bits.dest := Mux(delay_chain_pt_req.o.bits.entry.entry.perm === INST_FETCH.U, PageTableOps.destITLB, PageTableOps.destITLB)
  delay_chain_rep.i.bits.thid := delay_chain_pt_req.o.bits.thid
  delay_chain_rep.i.bits.tag := delay_chain_pt_req.o.bits.entry.tag
  delay_chain_rep.i.bits.data := delay_chain_pt_req.o.bits.entry.entry
  delay_chain_rep.i.valid := delay_chain_pt_req.o.valid && delay_chain_pt_req.o.bits.op === PageTableOps.opLookup
  delay_chain_rep.i.ready <> delay_chain_pt_req.o.ready

  // Handle opEvict expected value
}

implicit class BaseTLBDriver(target: DUTTLB){
  def setReadRequest(vpage: UInt, asid: UInt) = {
    target.frontendRequest_i.bits.tag.vpn.poke(vpage)
    target.frontendRequest_i.bits.thid.poke(asid)
    target.frontendRequest_i.bits.tag.asid.poke(asid)
    target.frontendRequest_i.bits.perm.poke(DATA_LOAD.U)
    target.frontendRequest_i.valid.poke(true.B)
    target.frontendRequest_i.ready.expect(true.B)
  }

  def setWriteRequest(vpage: UInt, asid: UInt) = {
    target.frontendRequest_i.bits.tag.vpn.poke(vpage)
    target.frontendRequest_i.bits.thid.poke(vpage)
    target.frontendRequest_i.bits.tag.asid.poke(asid)
    target.frontendRequest_i.bits.perm.poke(DATA_STORE.U)
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
    } while(!target.packetArrive_o.valid.peek().litToBoolean)
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

  def tick(step: Int = 1) = {
    //import armflex.util.SimTools._
    //logWithCycle("Tick.")
    target.clock.step(step)
  }
}
}


import chiseltest.simulator.VerilatorBackendAnnotation
import chiseltest.simulator.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation

class TLBTester extends AnyFreeSpec with ChiselScalatestTester {
  val param = new PageTableParams(
    12, 8, 4, 2, 4, 1, 32, 32
  )

  import TLBTestUtility._

  "Normal Access" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/tlb/normal_read"), WriteVcdAnnotation)
    test(new DUTTLB(
      () => new TLB(param, () => new PseudoTreeLRUCore(param.tlbAssociativity)), ""
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