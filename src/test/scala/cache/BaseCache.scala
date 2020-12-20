package armflex.cache

/**
 * 
 * ! 1. Frontend RAW Hazard
 * ! 2. Backend RAW Hazard. (Fetch a block that in the WB queue)
 * ! 3. Synonyms. (Duplicated entries in the same set.)
 * ! 4. Permission check
 * ! 5. Back pressure
 * 
 */ 

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
import chiseltest._
import chiseltest.experimental._
import org.scalatest._
import chiseltest.internal.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import chiseltest.internal.WriteVcdAnnotation

import armflex.util.SimTools._

/**
 * Useful testing functions and prototypes for testing a cache.
 */ 
object CacheTestUtility{

class BackendMemorySimulator(
  param: CacheParameter,
  srcFile: String = ""
) extends MultiIOModule{
  val mem = SyncReadMem((1 << param.addressWidth), UInt(param.backendPortSize.W))
  val request_i = IO(Flipped(Decoupled(new MergedBackendRequestPacket(param))))
  val reply_o = IO(Decoupled(UInt(param.backendPortSize.W)))
  if(srcFile.nonEmpty) loadMemoryFromFile(mem, srcFile)

  request_i.ready := true.B

  val port = mem(request_i.bits.addr)
  var shouldReply_v = Wire(Bool())
  when(request_i.valid && request_i.bits.w_v){
    port := request_i.bits.data
  }
  shouldReply_v := !request_i.bits.w_v && request_i.valid
  val reply_vr = RegNext(shouldReply_v)
  reply_o.valid := reply_vr
  reply_o.bits := port
}

class DelayChain(param: CacheParameter, n: Int) extends MultiIOModule{
  val cacheRequest_i = IO(Flipped(Decoupled(new MergedBackendRequestPacket(param))))
  val cacheReply_o = IO(Decoupled(UInt(param.backendPortSize.W)))

  val backendRequest_o = IO(Decoupled(new MergedBackendRequestPacket(param)))
  val backendReply_i = IO(Flipped(Decoupled(UInt(param.backendPortSize.W))))

  val requestChain = Wire(Vec(n+1, cacheRequest_i.cloneType))
  requestChain(0) <> cacheRequest_i
  val replyChain = Wire(Vec(n+1, cacheReply_o.cloneType))
  replyChain(0) <> backendReply_i

  for(i <- 1 to n){
    requestChain(i) <> Queue(requestChain(i-1), 1)
    replyChain(i) <> Queue(replyChain(i-1), 1)
  }
  cacheReply_o <> replyChain(n)
  backendRequest_o <> requestChain(n)
}

class DTUCache(
  parent: () => BaseCache,
  initialFile: String = ""
) extends MultiIOModule{
  val u_cache = Module(parent())
  //? why delay chain? To simulate the latency of DRAM access.
  val u_delayChain = Module(new DelayChain(u_cache.param, 4))
  val u_backend = Module(new BackendMemorySimulator(u_cache.param, initialFile))

  u_cache.backendReadReply_i <> u_delayChain.cacheReply_o
  u_cache.backendRequest_o <> u_delayChain.cacheRequest_i
  
  u_backend.request_i <> u_delayChain.backendRequest_o
  u_backend.reply_o <> u_delayChain.backendReply_i

  // export the frontend ports of cache
  val frontendRequest_i = IO(Flipped(u_cache.frontendRequest_i.cloneType))
  frontendRequest_i <> u_cache.frontendRequest_i
  val flushRequest_i = IO(Flipped(u_cache.flushRequest_i.cloneType))
  flushRequest_i <> u_cache.flushRequest_i
  val frontendReply_o = IO(u_cache.frontendReply_o.cloneType)
  frontendReply_o <> u_cache.frontendReply_o
  val packetArrive_o = IO(u_cache.packetArrive_o.cloneType)
  packetArrive_o <> u_cache.packetArrive_o
}

implicit class CacheDriver(target: DTUCache){
  def setReadRequest(addr: UInt, threadID: UInt, groupedFlag: Bool = false.B):Unit = {
    target.frontendRequest_i.bits.addr.poke(addr)
    target.frontendRequest_i.bits.thread_id.poke(threadID)
    target.frontendRequest_i.bits.w_v.poke(false.B)
    target.frontendRequest_i.valid.poke(true.B)
    target.frontendRequest_i.ready.expect(true.B) // make sure this is selected.
  }

  def setWriteRequest(addr: UInt, threadID: UInt, data: UInt, mask: UInt, groupedFlag: Bool = false.B): Unit = {
    //assert(target.u_cache.param.writable, "Can not set a write request to a read-only cache")
    target.frontendRequest_i.bits.addr.poke(addr)
    target.frontendRequest_i.bits.thread_id.poke(threadID)
    target.frontendRequest_i.bits.w_v.poke(true.B)
    target.frontendRequest_i.bits.wData.poke(data)
    target.frontendRequest_i.bits.wMask.poke(mask)
    target.frontendRequest_i.valid.poke(true.B)
    target.frontendRequest_i.ready.expect(true.B) // make sure this is selected.
  }

  def setFlushRequest(addr: UInt, threadID: UInt): Unit = {
    target.flushRequest_i.bits.addr.poke(addr)
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
    target.packetArrive_o.bits.thread_id.expect(expectThreadID)
    target.tick()
  }

  def expectReply(hit: Boolean, threadID: UInt, data: UInt) = {
    target.frontendReply_o.valid.expect(true.B)
    target.frontendReply_o.bits.hit.expect(hit.B)
    target.frontendReply_o.bits.thread_id.expect(threadID)
    if(hit){
      target.frontendReply_o.bits.data.expect(data)
    }
  }

  def tick(step: Int = 1){
    logWithCycle("Tick.")
    target.clock.step(step)
  }
}

}

import TestOptionBuilder._

/**
 * How to test a read-only tester?
 * 
 * - Flushing
 * - Read correctness
 * - LRU replacement (at least 5 transaction)
 * 
 */ 

class CacheTester extends FreeSpec with ChiselScalatestTester {
  val param = new CacheParameter(
    64, 2, 32, 10, 2
  )

  import CacheTestUtility._

  "Normal Access" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/cache/normal_read"), WriteVcdAnnotation)
    test(new DTUCache(
      () => BaseCache.generateCache(param, () => new PseudoTreeLRUCore(param.associativity)),
      "test/cache/memory.txt"
    )).withAnnotations(anno){ dut =>
      //dut.frontendRequest_i.bits.
      //dut.
      dut.setReadRequest(108.U, 0.U, false.B)
      dut.tick()
      dut.expectReply(false, 0.U, 0.U)
      dut.frontendRequest_i.valid.poke(false.B)
      dut.waitForArrive(0.U)
      dut.tick()
      dut.setReadRequest(108.U, 0.U, false.B)
      dut.tick()
      dut.expectReply(true, 0.U, 108.U)
      dut.frontendRequest_i.valid.poke(false.B)
    }
  }
  "Synonyms" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/cache/synonyms"), WriteVcdAnnotation)
    test(new DTUCache(
      () => BaseCache.generateCache(param, () => new PseudoTreeLRUCore(param.associativity)),
      "test/cache/memory.txt"
    )).withAnnotations(anno){ dut =>
      // the first transaction
      dut.setReadRequest(108.U, 0.U)
      dut.tick()
      dut.expectReply(false, 0.U, 0.U)
      dut.clearRequest()
      dut.tick(4)

      dut.setReadRequest(108.U, 1.U)
      dut.tick()
      dut.expectReply(false, 1.U, 0.U);
      dut.clearRequest()

      dut.waitForArrive(0.U)
      dut.tick()
      dut.setWriteRequest(108.U, 0.U, 110.U, ((1l << 8) - 1).U)
      dut.tick()
      dut.clearRequest()
      dut.expectReply(true, 0.U, 110.U)

      dut.waitForArrive(1.U)
      dut.tick()
      dut.setReadRequest(108.U, 0.U)
      dut.tick()
      dut.clearRequest()
      dut.expectReply(true, 0.U, 110.U)
    }
  }

  "RAW" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/cache/RAW"), WriteVcdAnnotation)
    test(new DTUCache(
      () => BaseCache.generateCache(param, () => new PseudoTreeLRUCore(param.associativity)),
      "test/cache/memory.txt"
    )).withAnnotations(anno){ dut =>
      dut.setWriteRequest(
        108.U, 0.U, 114.U, ((1l << 8) - 1).U // no full mask here in order not to trigger the full write.
      )
      dut.tick()
      dut.expectReply(false, 0.U, 0.U)
      dut.frontendRequest_i.valid.poke(false.B)

      dut.waitForArrive(0.U)
      dut.tick()

      dut.setWriteRequest(108.U, 0.U, 112.U, ((1l << 32) - 1).U)
      dut.tick()
      dut.expectReply(true, 0.U, 112.U) // reply its previous value?
      dut.setReadRequest(108.U, 0.U)
      dut.tick()
      dut.frontendRequest_i.valid.poke(false.B)
      dut.expectReply(true, 0.U, 112.U)
    }
  }

  "Full write must hit" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/cache/full_writing"), WriteVcdAnnotation)
    test(new DTUCache(
      () => BaseCache.generateCache(param, () => new PseudoTreeLRUCore(param.associativity)),
      "test/cache/memory.txt"
    )).withAnnotations(anno){ dut =>
      dut.setWriteRequest(111.U, 0.U, 10.U, ((1l << 32) - 1).U)
      dut.tick()
      dut.expectReply(true, 0.U, 10.U)
      dut.clearRequest()
      dut.tick()
      dut.setReadRequest(111.U, 0.U)
      dut.tick()
      dut.expectReply(true, 0.U, 10.U)
      dut.clearRequest()

      dut.setWriteRequest(122.U, 0.U, 133.U, ((1l << 32) - 1).U)
      dut.tick()
      dut.setReadRequest(122.U, 0.U)
      dut.tick()
      dut.clearRequest()
      dut.expectReply(true, 0.U, 133.U)
    }
  }

  "Flush" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/cache/flushing"), WriteVcdAnnotation)
    test(new DTUCache(
      () => BaseCache.generateCache(param, () => new PseudoTreeLRUCore(param.associativity)),
      "test/cache/memory.txt"
    )).withAnnotations(anno){ dut =>
      dut.setReadRequest(101.U, 0.U)
      dut.tick()
      dut.expectReply(false, 0.U, 0.U)
      dut.clearRequest()
      dut.waitForArrive(0.U)
      dut.tick()

      dut.setReadRequest(101.U, 0.U)
      dut.tick()
      dut.expectReply(true, 0.U, 101.U)
      dut.clearRequest()

      dut.setFlushRequest(101.U, 0.U)
      dut.tick()
      dut.expectReply(true, 0.U, 0.U)
      dut.clearRequest()

      dut.setReadRequest(101.U, 0.U)
      dut.tick()
      dut.expectReply(false, 0.U, 101.U)
      dut.clearRequest()
    }
  }
}

class CacheLRUTester extends FreeSpec with ChiselScalatestTester {
  val param = new CacheParameter(
    64, 2, 32, 10, 2
  )
  import CacheTestUtility._

  "LRU Replacement" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/cache/LRU"), WriteVcdAnnotation)
    test(new DTUCache(
      () => BaseCache.generateCache(param, () => new PseudoTreeLRUCore(param.associativity)),
      "test/cache/memory.txt"
    )).withAnnotations(anno){ dut =>
      // set number is 64, associativity is 2.
      dut.setReadRequest(0.U, 0.U)
      dut.tick()
      dut.clearRequest()
      dut.waitForArrive(0.U)
      dut.tick()

      dut.setReadRequest(64.U, 0.U)
      dut.tick()
      dut.clearRequest()
      dut.waitForArrive(0.U)
      dut.tick()

      dut.setReadRequest(0.U, 0.U)
      dut.tick()
      dut.clearRequest()
      dut.expectReply(true, 0.U, 0.U)

      dut.setReadRequest(128.U, 0.U)
      dut.tick()
      dut.clearRequest()
      dut.waitForArrive(0.U)
      dut.tick()

      dut.setReadRequest(128.U, 0.U)
      dut.tick()
      dut.clearRequest()
      dut.expectReply(true, 0.U, 128.U)

      dut.setReadRequest(256.U, 0.U)
      dut.tick()
      dut.clearRequest()
      dut.waitForArrive(0.U)
      dut.tick()

      dut.setReadRequest(128.U, 0.U)
      dut.tick()
      dut.clearRequest()
      dut.expectReply(true, 0.U, 128.U)
    }
  }

  "Replacement restricted by the Pending" in {
    
  }

  "Invalid position must be available" in {

  }
}