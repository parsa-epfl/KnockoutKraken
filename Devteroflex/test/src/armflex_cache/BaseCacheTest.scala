package armflex_cache

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
import chisel3.util.experimental.loadMemoryFromFileInline
import chiseltest._
import chiseltest.experimental._
import org.scalatest.freespec.AnyFreeSpec
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.options.TargetDirAnnotation
import chiseltest.simulator.WriteVcdAnnotation

//import armflex.util.SimTools._

/**
* Useful testing functions and prototypes for testing a cache.
*/ 
object CacheDrivers {
  
  class BackendMemorySimulator(param: CacheParams, srcFile: String = "") extends Module {
    val mem = SyncReadMem(1 << (param.pAddrWidth), UInt(param.blockSize.W))
    val request_i = IO(Flipped(Decoupled(new MergedBackendRequestPacket(param.databankParameter))))
    val reply_o = IO(Decoupled(UInt(param.blockSize.W)))
    if(srcFile.nonEmpty) loadMemoryFromFileInline(mem, srcFile)
    
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
  
  class DelayChain(param: CacheParams, n: Int) extends Module {
    val cacheRequest_i = IO(Flipped(Decoupled(new MergedBackendRequestPacket(param.databankParameter))))
    val cacheReply_o = IO(Decoupled(UInt(param.blockSize.W)))
    
    val backendRequest_o = IO(Decoupled(new MergedBackendRequestPacket(param.databankParameter)))
    val backendReply_i = IO(Flipped(Decoupled(UInt(param.blockSize.W))))
    
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
  
  class DTUCache(parent: () => BaseCache, initialFile: String = "") extends Module {
    val u_cache = Module(parent())
    //? why delay chain? To simulate the latency of DRAM access.
    val u_delayChain = Module(new DelayChain(u_cache.params, 4))
    val u_backend = Module(new BackendMemorySimulator(u_cache.params, initialFile))
    
    u_cache.axiMem_io.resp <> u_delayChain.cacheReply_o
    u_cache.axiMem_io.req <> u_delayChain.cacheRequest_i
    
    u_backend.request_i <> u_delayChain.backendRequest_o
    u_backend.reply_o <> u_delayChain.backendReply_i
    
    // export the frontend ports of cache
    val frontendRequest_i = IO(Flipped(u_cache.pipeline_io.req.cloneType))
    frontendRequest_i <> u_cache.pipeline_io.req
    val flushRequest_i = IO(Flipped(u_cache.mmu_i.flushReq.cloneType))
    flushRequest_i <> u_cache.mmu_i.flushReq
    val frontendReply_o = IO(u_cache.pipeline_io.resp.cloneType)
    frontendReply_o <> u_cache.pipeline_io.resp
  }
  
  implicit class CacheDriver(target: DTUCache){
    
    def blockOffsetWidth = log2Ceil(target.u_cache.params.blockSize / 8)
    
    def setReadRequest(blockAddr: Int):Unit = {
      target.frontendRequest_i.bits.addr.poke((blockAddr << blockOffsetWidth).U)
      target.frontendRequest_i.valid.poke(true.B)
      target.frontendRequest_i.ready.expect(true.B) // make sure this is selected.
    }
    
    def setWriteRequest(blockAddr: Int, data: BigInt, wEn: BigInt): Unit = {
      //assert(target.u_cache.params.writable, "Can not set a write request to a read-only cache")
      target.frontendRequest_i.bits.addr.poke((blockAddr << blockOffsetWidth).U)
      target.frontendRequest_i.bits.data.poke(data.U)
      target.frontendRequest_i.bits.w_en.poke(wEn.U)
      target.frontendRequest_i.valid.poke(true.B)
      target.frontendRequest_i.ready.expect(true.B) // make sure this is selected.
    }
    
    def setFlushRequest(blockAddr: Int): Unit = {
      target.flushRequest_i.bits.addr.poke((blockAddr << blockOffsetWidth).U)
      target.flushRequest_i.valid.poke(true.B)
      target.flushRequest_i.ready.expect(true.B)
    }
    
    def clearRequest() = {
      target.frontendRequest_i.valid.poke(false.B)
      target.flushRequest_i.valid.poke(false.B)
    }
    
    def waitForArrive(data: BigInt) = {
      do{
        target.tick()
      } while(!target.frontendReply_o.valid.peek().litToBoolean)
      target.frontendReply_o.bits.miss2hit.expect(true.B)
      target.frontendReply_o.bits.data.expect(data.U)
    }
    
    def expectReply(hit: Boolean, data: BigInt, dataDontCare: Boolean = false) = {
      target.frontendReply_o.valid.expect(true.B)
      target.frontendReply_o.bits.hit.expect(hit.B)
      target.frontendReply_o.bits.miss2hit.expect(false.B)
      if(hit && !dataDontCare){
        target.frontendReply_o.bits.data.expect(data.U)
      }
    }
    
    def tick(step: Int = 1): Unit = {
      //logWithCycle("Tick.")
      target.clock.step(step)
    }
  }
  
}

/**
* How to test a read-only tester?
* 
* - Flushing
* - Read correctness
* - LRU replacement (at least 5 transaction)
* 
*/ 

class CacheTester extends AnyFreeSpec with ChiselScalatestTester {
  val param = CacheParams(12, 64, 2, 32, 32)
  import CacheDrivers._
  
  "Normal Access" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/cache/normal_read"), WriteVcdAnnotation)
    test(new DTUCache(
    () => BaseCache(param, () => new PseudoTreeLRUCore(param.associativity)),
    "Devteroflex/test/content/memory.txt"
    )).withAnnotations(anno){ dut =>
      //dut.frontendRequest_i.bits.
      //dut.
      dut.setReadRequest(108)
      dut.tick()
      dut.expectReply(hit = false, 0)
      dut.frontendRequest_i.valid.poke(false.B)
      dut.waitForArrive(108)
      dut.tick()
      dut.setReadRequest(108)
      dut.tick()
      dut.expectReply(hit = true, data = 108)
      dut.frontendRequest_i.valid.poke(false.B)
    }
  }
  "Refilling keeps latest result" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/cache/synonyms"), WriteVcdAnnotation)
    test(new DTUCache(
    () => BaseCache(param, () => new PseudoTreeLRUCore(param.associativity)),
    "Devteroflex/test/content/memory.txt"
    )).withAnnotations(anno){ dut =>
      // the first transaction
      dut.setReadRequest(108)
      dut.tick()
      dut.expectReply(hit = false, 0)
      dut.clearRequest()
      dut.tick(4)
      
      dut.setWriteRequest(108, 110, 0x1)
      dut.tick()
      dut.expectReply(hit = false, 0)
      dut.clearRequest()
      
      dut.waitForArrive(108) // first read
      dut.tick()
      dut.waitForArrive(110) // second write
    }
  }
  
  "RAW" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/cache/RAW"), WriteVcdAnnotation)
    test(new DTUCache(
    () => BaseCache(param, () => new PseudoTreeLRUCore(param.associativity)),
    "Devteroflex/test/content/memory.txt"
    )).withAnnotations(anno){ dut =>
      // warm up the cache
      dut.setWriteRequest(
      108, 114, 0x1 // no full mask here in order not to trigger the full write.
      )
      dut.tick()
      dut.expectReply(hit = false, 0, dataDontCare = true)
      dut.frontendRequest_i.valid.poke(false.B)
      
      dut.waitForArrive(114)
      dut.tick()
      dut.tick()
      
      // bring the RAW hazard
      
      dut.setWriteRequest(108,112, 1)
      dut.tick()
      dut.expectReply(hit = true, 114, dataDontCare = true) // hit always reply its value read from cache
      dut.setReadRequest(108)
      dut.tick()
      dut.frontendRequest_i.valid.poke(false.B)
      dut.expectReply(hit = true, 112)
    }
  }
  
  "Full write must hit" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/cache/full_writing"), WriteVcdAnnotation)
    test(new DTUCache(
    () => BaseCache(param, () => new PseudoTreeLRUCore(param.associativity)),
    "Devteroflex/test/content/memory.txt"
    )).withAnnotations(anno){ dut =>
      dut.setWriteRequest(111, 10, 0xF)
      dut.tick()
      dut.expectReply(hit = true, 10, dataDontCare = true)
      dut.clearRequest()
      dut.tick()
      dut.setReadRequest(111)
      dut.tick()
      dut.expectReply(hit = true, 10)
      dut.clearRequest()
      
      dut.setWriteRequest(122, 133, 0xF)
      dut.tick()
      dut.setReadRequest(122)
      dut.tick()
      dut.clearRequest()
      dut.expectReply(hit = true, 133)
    }
  }
  
  "Flush" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/cache/flushing"), WriteVcdAnnotation)
    test(new DTUCache(
    () => BaseCache(param, () => new PseudoTreeLRUCore(param.associativity)),
    "Devteroflex/test/content/memory.txt"
    )).withAnnotations(anno){ dut =>
      dut.setReadRequest(101)
      dut.tick()
      dut.expectReply(hit = false, 0)
      dut.clearRequest()
      dut.waitForArrive(101)
      dut.tick()
      
      dut.setReadRequest(101)
      dut.tick()
      dut.expectReply(hit = true, 101)
      dut.clearRequest()
      
      dut.setFlushRequest(101)
      dut.tick()
      //dut.expectReply(true, 0.U, 0.U, true)
      dut.clearRequest()
      
      dut.setReadRequest(101)
      dut.tick()
      dut.expectReply(hit = false, 101)
      dut.clearRequest()
      dut.waitForArrive(101)
      dut.tick()
      
      // What if we flush an entry sel is not in the cache?
      dut.setFlushRequest(99)
      dut.tick()
      // There should be no backend request
      for(_ <- 0 until 30){
        //dut.expectReply(false, 0.U, 0.U, true)
        dut.tick() // keep flushing and never hit
      }
    }
  }
  
  "Flush dirty triggers writing back" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/cache/write_flushing"), WriteVcdAnnotation)
    test(new DTUCache(
    () => BaseCache(param, () => new PseudoTreeLRUCore(param.associativity)),
    "Devteroflex/test/content/memory.txt"
    )).withAnnotations(anno){ dut =>
      dut.setWriteRequest(10, 100, 1)
      dut.tick()
      dut.expectReply(hit = false, 0, dataDontCare = true)
      dut.clearRequest()
      dut.waitForArrive(100)
      dut.tick()
      
      dut.setFlushRequest(10)
      dut.tick()
      //dut.expectReply(true, 0.U, 0.U, true)
      dut.clearRequest()
      dut.tick()
      dut.tick()
      dut.tick()
      dut.tick()
      
      dut.setReadRequest(10)
      dut.tick()
      dut.expectReply(hit = false, 0, dataDontCare = true)
      dut.clearRequest()
      dut.waitForArrive(100)
      dut.tick()
    }
  }
  
  "Hit incomplete forwarding block" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/cache/forwarding_chunk"), WriteVcdAnnotation)
    test(new DTUCache(
    () => BaseCache(param, () => new PseudoTreeLRUCore(param.associativity)),
    "Devteroflex/test/content/memory.txt"
    )).withAnnotations(anno){ dut =>
      dut.setWriteRequest(10, 0xA, 0x1)
      dut.tick()
      dut.expectReply(hit = false, 0)
      dut.clearRequest()
      dut.waitForArrive(0xA)
      dut.tick(10)
      dut.setWriteRequest(10, 0xA000, 0x2)
      dut.tick()
      dut.expectReply(hit = true, 0, dataDontCare = true)
      dut.setReadRequest(10)
      dut.tick()
      dut.clearRequest()
      dut.expectReply(hit = true, 0xA00A)
    }
  }
}

class CacheLRUTester extends AnyFreeSpec with ChiselScalatestTester {
  val param = CacheParams(12, 64, 2, 32, 32)
  import CacheDrivers._
  
  // TODO: RAW Hazard for LRU.
  
  "LRU Replacement" in {
    val anno = Seq(VerilatorBackendAnnotation, TargetDirAnnotation("test/cache/LRU"), WriteVcdAnnotation)
    test(new DTUCache(
    () => BaseCache(param, () => new PseudoTreeLRUCore(param.associativity)),
    "Devteroflex/test/content/memory.txt"
    )).withAnnotations(anno){ dut =>
      // set number is 64, tlbAssociativity is 2.
      dut.setReadRequest(0)
      dut.tick()
      dut.clearRequest()
      dut.waitForArrive(0)
      dut.tick()
      
      dut.setReadRequest(64)
      dut.tick()
      dut.clearRequest()
      dut.waitForArrive(64)
      dut.tick()
      
      dut.setReadRequest(0)
      dut.tick()
      dut.clearRequest()
      dut.expectReply(hit = true, 0)
      
      dut.setReadRequest(128)
      dut.tick()
      dut.clearRequest()
      dut.waitForArrive(128)
      dut.tick()
      
      dut.setReadRequest(128)
      dut.tick()
      dut.clearRequest()
      dut.expectReply(hit = true, 128)
      
      dut.setReadRequest(256)
      dut.tick()
      dut.clearRequest()
      dut.waitForArrive(256)
      dut.tick()
      
      dut.setReadRequest(128)
      dut.tick()
      dut.clearRequest()
      dut.expectReply(hit = true, 128)
    }
  }
}