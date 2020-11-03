package armflex.cache

import chisel3._
import chisel3.util._

import chisel3.stage.ChiselStage

import armflex.util.Diverter
import armflex.util.BRAMConfig
import armflex.util.BRAMPort
import armflex.util.BRAM

case class CacheParameter(
  // Cache properties
  setNumber: Int = 1024,
  associativity: Int = 4,
  blockBit: Int = 512,  
  // Address
  addressWidth: Int = 55, // address to access the whole block instead of one byte
  // Thread id
  threadNumber: Int = 4,
  // Permission recordings. A D cache needs one bit to record the permission of the data.
  writable: Boolean = false,
  // Permission checking. 
  permissionIsChecked: Boolean = false
){
  assert (isPow2(setNumber))
  assert (isPow2(associativity))

  def tagWidth(): Int = {
    addressWidth - log2Ceil(setNumber)
  }

  def threadIDWidth(): Int = {
    if(threadNumber > 0)
      log2Ceil(threadNumber)
    else
      0
  }

  def wayWidth(): Int = {
    log2Ceil(associativity)
  }
  
}

class BRAMorRegister(implementedWithRegister: Boolean = true)(implicit cfg: BRAMConfig) extends MultiIOModule{
  val portA = IO(new BRAMPort)
  val portB = IO(new BRAMPort)

  if(implementedWithRegister){
    val pAdo = Wire(Vec(cfg.NB_COL, UInt(cfg.COL_WIDTH.W)))
    val pBdo = Wire(Vec(cfg.NB_COL, UInt(cfg.COL_WIDTH.W)))
    for(col <- 0 until cfg.NB_COL){
      val regBank_r = RegInit(VecInit(Seq.fill(cfg.NB_ELE)(0.U(cfg.COL_WIDTH.W))))
      when(portA.EN && portA.WE(col)){
        regBank_r(portA.ADDR) := portA.DI((col+1)*cfg.COL_WIDTH-1, col*cfg.COL_WIDTH)
      }
      pAdo(col) := regBank_r(portA.ADDR)

      when(portB.EN && portB.WE(col)){
        regBank_r(portA.ADDR) := portB.DI((col+1)*cfg.COL_WIDTH-1, col*cfg.COL_WIDTH)
      }
      pBdo(col) := regBank_r(portB.ADDR)
    }
    portA.DO := pAdo.asUInt()
    portB.DO := pBdo.asUInt()

  } else {
    val bram = Module(new BRAM())
    bram.portA <> portA
    bram.portB <> portB
  }
}

class DataBankFrontendPort(
  addressWidth: Int,
  threadIDWidth: Int,
  blockSize: Int,
  writable: Boolean,
) extends Bundle{
  val addr = UInt(addressWidth.W) // access address.
  val threadID = UInt(threadIDWidth.W)
  val groupedFlag = Bool()
  val w_v = if(writable) Some(Bool()) else None // write valid?
  val wData = if(writable) Some(UInt(blockSize.W)) else None
  val wMask = if(writable) Some(UInt(blockSize.W)) else None
  val wpermission = Bool() // 0 is read-only and 1 is read-write.

  // overload constructor to quickly build a frontend port from the cache parameter.
  def this(param: CacheParameter) = this(
    param.addressWidth,
    param.threadIDWidth(),
    param.blockBit,
    param.writable
  )

  override def cloneType(): this.type = new DataBankFrontendPort(addressWidth, threadIDWidth, blockSize, writable).asInstanceOf[this.type]
}


/**
 * The memory and its controlling logic in a cache to store all data entries.
 * 
 * Signals in the DataBank are divided into 4 channels:
 * - Frontend:  The request / response ports to the pipeline
 * - Backend:   The request / response ports to the DRAM & high-level controller
 * - LRU:       The port to activate the LRU module
 * - Notify:    The ports to notify a waking up, sleeping, and kill of one thread due to the data arrival, missing, and permission mismatching
 * 
 * Since we use SyncMem in this module, we need an extra cycle to judge whether to hit and another cycle to write back.
 * - Latch the frontend request as well as access memory.
 * -------------------- Register --------------------
 * - With the data entry, we can judge the request is hit or miss. 
 *  - if hit, a read response is immediately available. A write request will be pushed to the writing queue.
 *  - if miss, a request to access the DRAM is pushed to the queue
 * 
 * Two-ported BRAM is used in this module, the two ports are designed to:
 *  - Access the data entries for the frontend request (Read-only)
 *  - Write back the data packet from the high-level response or the frontend write request.
 *    - RRArbiter to prevent starving
 * 
 * Write requests from the frontend will be broadcast to two queues:
 *  - DRAM / high-level write queue, and its capacity is 4 * threadNumber
 *  - Write port of the BRAM to update data in the cache
 * Therefore we employ a diverter to make sure two requests are pushed to the two queues only once.
 * 
 * @param param the parameter of cache.
 * @param t the Data entry Chisel type. See Entry.scala for all options.
 * @param implementedWithRegister build the bank with register, which is useful and necessary for the L1 TLB.
 * 
 */ 
class DataBank[T <: DataBankEntry](
  param: CacheParameter, 
  t: T,
  implementedWithRegister: Boolean = false
) extends Module{
  val io = IO(new Bundle{
    // two ports, one for the frontend and one for the backend.
    val frontend = new Bundle{
      val req_i = Flipped(Decoupled(new DataBankFrontendPort(param)))
      val rep_o = Decoupled(new Bundle{
        val data = UInt(param.blockBit.W)
        val threadID = UInt(param.threadIDWidth().W)
        val hit = Bool()
      })
    }

    val backend = new Bundle{
      val req_o = Decoupled(new Bundle{
        val addr = UInt(param.addressWidth.W)
        val wreq = if(param.writable) Some(new Bundle{
          val data = UInt(param.blockBit.W)
          val v = Bool()
        }) else None
      })
      val reply_i = Flipped(Decoupled(UInt(param.blockBit.W)))
    }

    val lru = new Bundle{
      val addr_o = Output(UInt(param.addressWidth.W))
      val addr_vo = Output(Bool())

      val index_o = Output(UInt(param.wayWidth().W))
      val index_vo = Output(Bool())

      val lru_i = Input(UInt(param.wayWidth().W))
    }

    val packet_arrive_o = Valid(new Bundle{
      val threadID = UInt(param.threadIDWidth().W)
      val groupedFlag = Bool()
    }) // a data block from the cache
    val thread_permission_violated_o = ValidIO(UInt(param.threadIDWidth().W))

    // flush the cache.
    val flush_i = Flipped(Decoupled(UInt(param.threadNumber.W)))

    // we have a flag to indicate that there're pending writing request.
    val writing_is_busy_vo = if(param.writable) Some(Output(Bool())) else None
  })

  val bramRowType = Vec(param.associativity, t)
  implicit val bramConfig = new BRAMConfig(
    param.associativity,
    t.getWidth,
    param.setNumber
  )
  val mem = Module(new BRAMorRegister(implementedWithRegister))

  mem.portA.DI := 0.U

  // Flushing logic
  val flushing_r = RegInit(false.B)
  val flush_cnt = Counter(param.setNumber)
  val flush_mask_r = RegNext(io.flush_i.bits)

  when(io.flush_i.fire()){
    flush_cnt.reset()
    flushing_r := true.B // start to flush
    flush_mask_r := io.flush_i.bits
  }.elsewhen(flushing_r){
    flushing_r := !(flush_cnt.inc())
  }

  /**
   * Helper function to correctly add register or directly bypass to union reading from the register and block memory.
   */ 
  private def regOrNot[T <: Data](t: T): T = {
    if(implementedWithRegister) WireInit(t) else RegNext(t)
  }

  // ######## FRONTEND ########
  val setWidth = log2Ceil(param.setNumber)
  val s0_context_n = Wire(Valid(new DataBankFrontendPort(param)))
  val s0_context_r = regOrNot(s0_context_n) // Why s0: state zero

  s0_context_n.bits.addr := io.frontend.req_i.bits.addr
  s0_context_n.bits.threadID := io.frontend.req_i.bits.threadID
  s0_context_n.bits.wpermission := io.frontend.req_i.bits.wpermission
  if(param.writable){
    s0_context_n.bits.wData.get := io.frontend.req_i.bits.wData.get
    s0_context_n.bits.wMask.get := io.frontend.req_i.bits.wMask.get
    s0_context_n.bits.w_v.get := io.frontend.req_i.bits.w_v.get
  }
  s0_context_n.valid := io.frontend.req_i.fire()

  // priority arbiter. Flushing is the highest priority.
  when(!flushing_r){ 
    mem.portA.ADDR := io.frontend.req_i.bits.addr(setWidth-1, 0)
    mem.portA.EN := io.frontend.req_i.valid
    mem.portA.WE := 0.U
  }.otherwise{
    mem.portA.ADDR := flush_cnt.value
    mem.portA.EN := flushing_r
    mem.portA.WE := 0.U
  }
  
  // ----- CYCLE 0: Access BRAM & store the request information -----
  val fAccess = mem.portA.DO.asTypeOf(bramRowType)
  
  io.lru.addr_o := io.frontend.req_i.bits.addr // forward to LRU
  io.lru.addr_vo := io.frontend.req_i.valid

  // ----- CYCLE 1: Read out or trigger state machine -----
  val matchBits = fAccess.map({ x=>
    x.checkHit(s0_context_r.bits.addr, s0_context_r.bits.threadID) && x.v
  }) // get all the comparison results
  val hitsWhich = OHToUInt(matchBits) // encode from the comparison
  val isHit: Bool = PopCount(matchBits) === 1.U // this value should only be 1 or zero.

  //! TBD here for how to handle the permission check of bits.
  val isValid = fAccess(hitsWhich).valid(s0_context_r.bits.wpermission)

  io.frontend.rep_o.valid := s0_context_r.valid && isValid
  io.frontend.rep_o.bits.data := fAccess(hitsWhich).read() // read
  io.frontend.rep_o.bits.threadID := s0_context_r.bits.threadID
  io.frontend.rep_o.bits.hit := isHit // if not head, also return.

  io.lru.index_o := hitsWhich
  io.lru.index_vo := isHit

  class WritebackPacket extends Bundle{
    val addr = UInt(param.addressWidth.W)  // address
    val threadID = UInt(param.threadIDWidth().W) // tid
    val wayIndex = UInt(param.wayWidth().W) // which position to replace
    val dataBlock = UInt(param.blockBit.W)
    val groupedFlag = Bool()
  }

  // Write request from the frontend
  val frontendWBPort = Wire(Decoupled(new WritebackPacket))

  val s1_wlatch_r = Reg(new Bundle{ // forward latch of writing request
    val addr = UInt(param.addressWidth.W) 
    val dataBlock = UInt(param.blockBit.W)
    val v = Bool()
  })

  if(param.writable){
    frontendWBPort.valid := s0_context_r.bits.w_v.get && isHit && isValid && s0_context_r.valid
    frontendWBPort.bits.addr := s0_context_r.bits.addr
    frontendWBPort.bits.dataBlock := VecInit(Seq.tabulate(param.blockBit)({ i =>
      Mux(s0_context_r.bits.wMask.get(i), s0_context_r.bits.wData.get(i), fAccess(hitsWhich).read()(i))
    })).asUInt()
    frontendWBPort.bits.threadID := s0_context_r.bits.threadID
    frontendWBPort.bits.groupedFlag := s0_context_r.bits.groupedFlag
    frontendWBPort.bits.wayIndex := Mux(isHit, hitsWhich, io.lru.lru_i) 
    io.frontend.req_i.ready := Mux(
      io.frontend.req_i.bits.w_v.get,
      frontendWBPort.ready,
      !flushing_r
    ) // new write request will not be handled until the writing back is processed.
    // we need to forward the result back to the reading part for RAW

    s1_wlatch_r.addr := frontendWBPort.bits.addr
    s1_wlatch_r.dataBlock := frontendWBPort.bits.dataBlock
    s1_wlatch_r.v := frontendWBPort.fire()

    io.frontend.rep_o.bits.data := Mux(
      s1_wlatch_r.v && s1_wlatch_r.addr === s0_context_r.bits.addr && isHit, // when the last writing result is valid, shares same address, and has not been evicted
      s1_wlatch_r.dataBlock,
      fAccess(hitsWhich).read() // read
    ) 
    io.frontend.rep_o.bits.hit := isHit // if not head, also return.
  } else {
    frontendWBPort.bits := DontCare
    frontendWBPort.valid := false.B
    io.frontend.req_i.ready := !flushing_r
    s1_wlatch_r := DontCare
  }

  // -------- Push missing request to the backend --------
  class MissRequestPacket extends Bundle{
    val addr = UInt(param.addressWidth.W)
    val threadID = UInt(param.threadIDWidth().W)
    val lru = UInt(param.wayWidth().W)
    val groupedFlag = Bool()
  }

  val frontFetchRequest = Wire(Decoupled(new MissRequestPacket))
  frontFetchRequest.bits.addr := s0_context_r.bits.addr
  frontFetchRequest.bits.threadID := s0_context_r.bits.threadID
  frontFetchRequest.bits.groupedFlag := s0_context_r.bits.groupedFlag
  frontFetchRequest.bits.lru := io.lru.lru_i
  frontFetchRequest.valid := !isHit && s0_context_r.valid && isValid // when miss occurs, push request to the backend.

  val missQueue = Queue(frontFetchRequest, param.threadNumber * 2) // FIFO to transmit the missing request from front to back.

  val s1_miss_r = Reg(Valid(new MissRequestPacket))
  s1_miss_r.bits := frontFetchRequest.bits
  s1_miss_r.valid := frontFetchRequest.valid

  val backendWBPort = Wire(Decoupled(new WritebackPacket))
  backendWBPort.bits.addr := missQueue.bits.addr
  backendWBPort.bits.dataBlock := io.backend.reply_i.bits
  backendWBPort.bits.threadID := missQueue.bits.threadID
  backendWBPort.bits.wayIndex := missQueue.bits.lru
  backendWBPort.bits.groupedFlag := missQueue.bits.groupedFlag
  backendWBPort.valid := missQueue.valid && io.backend.reply_i.valid

  missQueue.ready := backendWBPort.ready
  io.backend.reply_i.ready := backendWBPort.ready


  // BRAM Write port arbiter 
  val arb = Module(new Arbiter(new WritebackPacket, 2))
  arb.io.in(0) <> frontendWBPort
  arb.io.in(1) <> backendWBPort

  val writeBackPacket = arb.io.out // arbiter result.
  

  // full write back data. We only modify the relevant part.
  val memWBEntry = 0.U.asTypeOf(bramRowType) // if update, we need to read it out first?
  val updaterMemWBEntry = memWBEntry(writeBackPacket.bits.wayIndex).buildFrom(writeBackPacket.bits.addr, writeBackPacket.bits.threadID, writeBackPacket.bits.dataBlock)

  // flushing logic in the write port.
  val flushing_s1_r = regOrNot(flushing_r)
  val flushing_s1_addr_r = regOrNot(flush_cnt.value)

  val flushed_data = VecInit(fAccess.map({x => 
    x.flush(flush_mask_r)
  }))

  when(!flushing_s1_r){
    writeBackPacket.ready := true.B
    mem.portB.EN := writeBackPacket.valid
    mem.portB.WE := UIntToOH(writeBackPacket.bits.wayIndex)
    mem.portB.ADDR := writeBackPacket.bits.addr
    mem.portB.DI := updaterMemWBEntry.asUInt()
  }.otherwise{
    writeBackPacket.ready := false.B
    mem.portB.EN := true.B
    mem.portB.WE := Fill(param.associativity, true.B)
    mem.portB.ADDR := flushing_s1_addr_r
    mem.portB.DI := flushed_data.asUInt()
  }

  io.flush_i.ready := !missQueue.valid // wait for the missing handling to be complete.

  val fetchMissFromDRAM = Wire(Decoupled(io.backend.req_o.bits.cloneType))
  val writeThroughToDRAM = Wire(Decoupled(io.backend.req_o.bits.cloneType))
  fetchMissFromDRAM.bits.addr := s1_miss_r.bits.addr
  fetchMissFromDRAM.valid := s1_miss_r.valid
  if(param.writable){
    writeThroughToDRAM.bits.addr := s1_wlatch_r.addr
    fetchMissFromDRAM.bits.wreq.get := DontCare
    writeThroughToDRAM.bits.wreq.get.data := s1_wlatch_r.dataBlock
    writeThroughToDRAM.bits.wreq.get.v := s1_wlatch_r.v
    writeThroughToDRAM.valid := s1_wlatch_r.v
  } else {
    writeThroughToDRAM.bits := DontCare
    writeThroughToDRAM.valid := false.B
  }

  // Arbiter
  assert(fetchMissFromDRAM.valid && writeThroughToDRAM.valid === 0.B, "Come on. Do you think for one isntruction missing and writing-through could happen at the same time?")

  val writeThroughArb = Module(new Arbiter(io.backend.req_o.bits.cloneType, 2))
  writeThroughArb.io.in(0) <> fetchMissFromDRAM
  writeThroughArb.io.in(1) <> writeThroughToDRAM

  val writeThroughQueue = Queue(writeThroughArb.io.out, 4 * param.threadNumber)
  io.backend.req_o <> writeThroughQueue
  
  // to be determined:
  // - io.frontend.req_i.ready
  // - io.flush_i.ready 
  // - io.writing_is_busy_vo.get

  // kill
  io.thread_permission_violated_o.bits := !isValid && s0_context_r.valid
  io.thread_permission_violated_o.valid := !isValid

  // wake up
  io.packet_arrive_o.valid := backendWBPort.fire() // when the write back is committed, wake up the thread.
  io.packet_arrive_o.bits.threadID := backendWBPort.bits.threadID
  io.packet_arrive_o.bits.groupedFlag := backendWBPort.bits.groupedFlag
}

// Separation plan for the DataBank.
// Break into three modules:
//  - Frontend. (frontend request, miss & hit)
//  - BRAM (store and flushing)
//  - Backend: write-through and fetch data
//  - Thread management. (wake up, kill, and sleep)


/**
 *  base class of LRU module.
 *  @param lruCore the LRU updating logic
 *  @param param Cache parameters
 *  @param implementedWithRegister use register as the buffer to store the encoding if true.
 */ 
class LRU[T <: LRUCore](
  param: CacheParameter,
  lruCore: () => T,
  implementedWithRegister: Boolean
) extends Module{
  val io = IO(new Bundle {
    // First cycle: give me the address for me to fetch the LRU
    val addr_i = Input(UInt(param.addressWidth.W))
    val addr_vi = Input(Bool())
    // Second cycle: give me whether or not you hits a specific position
    val index_i = Input(UInt(param.wayWidth().W))
    val index_vi = Input(Bool())
    // I will also tell you which position is available for replacement.
    val lru_o = Output(UInt(param.wayWidth().W))
    // The update of lru_o is valid in the third cycle.
  })
  // add an extra stage to store the addr. They will be used for the LRU bits writing back.
  val addr_s1_r = RegNext(io.addr_i)
  val addr_s1_vr = RegNext(io.addr_vi)

  // Connected to the LRU Core
  val core = Module(lruCore())

  implicit val bramConfig = new BRAMConfig(
    1,
    core.encodingWidth(),
    param.setNumber
  )

  val bram = Module(new BRAMorRegister(implementedWithRegister))

  bram.portA.EN := io.addr_vi
  bram.portA.ADDR := io.addr_i
  bram.portA.WE := false.B
  bram.portA.DI := 0.U


  core.io.encoding_i := bram.portA.DO

  core.io.index_i := io.index_i
  core.io.index_vi := io.index_vi
  core.io.vi := addr_s1_vr // if true, there is a request.
  io.lru_o := core.io.lru_o

  // write back
  bram.portB.EN := addr_s1_vr
  bram.portB.ADDR := addr_s1_r
  bram.portB.WE := addr_s1_vr
  bram.portB.DI := core.io.encoding_o
}

/**
 * The generator for BRAM-based cache.
 * 
 * The size of all operands is `param.blockBit`, which is typically 512bit
 * for a cache. If you hope to access the cache by 32bit, please partition 
 * the block manually. 
 * 
 * @param param the parameters of the cache
 * @param entry the entry Chisel Type. See Entry.scala for all options.
 * @param lruCore an generator of the LRU updating logic. See LRUCore.scala for all options.
 */ 
class BaseCache[EntryType <: DataBankEntry](
  param: CacheParameter,
  entry: EntryType,
  lruCore: () => LRUCore,
  implementedWithRegister: Boolean = false
) extends Module{
  val dataBank = Module(new DataBank(param, entry, implementedWithRegister))
  val lru = Module(new LRU(param, lruCore, implementedWithRegister))
  val io = IO(new Bundle{
    val frontend = dataBank.io.frontend.cloneType
    val backend = dataBank.io.backend.cloneType
    val packet_arrive_o = dataBank.io.packet_arrive_o.cloneType
    val thread_permission_violated_o = dataBank.io.thread_permission_violated_o.cloneType
    val flush_i = Flipped(dataBank.io.flush_i.cloneType)
    val writing_is_busy_vo = if(param.writable) Some(dataBank.io.writing_is_busy_vo.get.cloneType) else None
  })

  io.frontend <> dataBank.io.frontend
  io.backend <> dataBank.io.backend
  //io. <> dataBank.io.threadNotify
  io.packet_arrive_o <> dataBank.io.packet_arrive_o
  io.thread_permission_violated_o <> dataBank.io.thread_permission_violated_o
  dataBank.io.flush_i <> io.flush_i
  if(param.writable){
    io.writing_is_busy_vo.get <> dataBank.io.writing_is_busy_vo.get
  }

  lru.io.addr_i := dataBank.io.lru.addr_o
  lru.io.addr_vi := dataBank.io.lru.addr_vo
  lru.io.index_i := dataBank.io.lru.index_o
  lru.io.index_vi := dataBank.io.lru.index_vo
  dataBank.io.lru.lru_i := lru.io.lru_o
}

class ICache extends Module{
  val cacheParam = new CacheParameter(
    1024, 2, 512, 27, 4, false // 36 - 9 = 27
  )
  val entryType = new CacheEntry(cacheParam)
  val base = Module(new BaseCache(cacheParam, entryType, () => new PseudoTreeLRUCore(2)))
  val io = IO(base.io.cloneType)
  base.io <> io
}

class DCache extends Module{
  val cacheParam = new CacheParameter(
    1024, 2, 512, 27, 4, true // 36 - 9 = 27
  )
  val entryType = new CacheEntry(cacheParam)
  val base = Module(new BaseCache(cacheParam, entryType, () => new PseudoTreeLRUCore(2)))
  val io = IO(base.io.cloneType)
  base.io <> io
}

class L1ITLB extends Module{
  val cacheParam = new CacheParameter(
    16, 8, 24, 52, 4, false, true
  )
  val entryType = new CacheEntry(cacheParam)
  val base = Module(new BaseCache(cacheParam, entryType, () => new PseudoTreeLRUCore(2), true))
  val io = IO(base.io.cloneType)
  base.io <> io
}

class L1DTLB extends Module{
  val cacheParam = new CacheParameter(
    16, 4, 24, 52, 4, false, true
  )
  val entryType = new CacheEntry(cacheParam)
  val base = Module(new BaseCache(cacheParam, entryType, () => new PseudoTreeLRUCore(2), true))
  val io = IO(base.io.cloneType)
  base.io <> io
}

class L2TLB extends Module{
  val cacheParam = new CacheParameter(
    1024, 2, 24, 52, 4, false, true
  )
  val entryType = new CacheEntry(cacheParam)
  val base = Module(new BaseCache(cacheParam, entryType, () => new PseudoTreeLRUCore(2)))
  val io = IO(base.io.cloneType)
  base.io <> io
}

object CacheVerilogEmitter extends App{
  println((new ChiselStage).emitVerilog(new ICache))
}

