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
      val req_i = Flipped(Decoupled(new Bundle{
        val addr = UInt(param.addressWidth.W) // access address.
        val threadID = UInt(log2Ceil(param.threadNumber).W)
        val w_v = if(param.writable) Some(Bool()) else None // write valid?
        val wData = if(param.writable) Some(UInt(param.blockBit.W)) else None
        val wMask = if(param.writable) Some(UInt(param.blockBit.W)) else None
        val permission = if(param.writable) Some(Bool()) else None // 0 is read, 1 is read and write.
      }))
      val rep_o = Decoupled(new Bundle{
        val data = UInt(param.blockBit.W)
      })
    }

    val backend = new Bundle{
      val rAddr_o = Decoupled(UInt(param.addressWidth.W))
      val rData_i = Flipped(Decoupled(UInt(param.blockBit.W)))
      val wReq_o = if(param.writable) Some(Decoupled(new Bundle{
        val addr = UInt(param.addressWidth.W)
        val data = UInt(param.blockBit.W)
      })) else None
    }

    val lru = new Bundle{
      val addr_o = Output(UInt(param.addressWidth.W))
      val addr_vo = Output(Bool())

      val index_o = Output(UInt(param.wayWidth().W))
      val index_vo = Output(Bool())

      val lru_i = Input(UInt(param.wayWidth().W))
    }

    val threadNotify = new Bundle{
      val wakeup_o = ValidIO(UInt(param.threadIDWidth().W))
      val sleep_o = ValidIO(UInt(param.threadIDWidth().W))
      val kill_o = ValidIO(UInt(param.threadIDWidth().W))
    }
  })

  val bramRowType = Vec(param.associativity, t)
  implicit val bramConfig = new BRAMConfig(
    param.associativity,
    t.getWidth,
    param.setNumber
  )
  val mem = Module(new BRAMorRegister(implementedWithRegister))

  mem.portA.DI := 0.U

  /**
   * Helper function to correctly add register or directly bypass to union reading from the register and block memory.
   */ 
  def regOrNot[T <: Data](t: T): T = {
    if(implementedWithRegister) t else RegNext(t)
  }

  // ######## FRONTEND ########
  val setWidth = log2Ceil(param.setNumber)
  val fAddr = io.frontend.req_i.bits.addr
  val fThread = io.frontend.req_i.bits.threadID
  val f_v_s0 =  regOrNot(io.frontend.req_i.valid)
  val fAddr_s0 = regOrNot(fAddr)
  val fThread_s0 = regOrNot(fThread)
  val wv_r = if(param.writable){
    val d_value = io.frontend.req_i.valid && io.frontend.req_i.bits.w_v.get
    regOrNot(d_value)
   } else false.B
  val permission_r = if(param.permissionIsChecked) regOrNot(io.frontend.req_i.bits.permission.get) else false.B

  mem.portA.ADDR := fAddr(setWidth-1, 0)
  mem.portA.EN := io.frontend.req_i.valid
  mem.portA.WE := 0.U

  // ----- CYCLE 0: Access BRAM & store the request information -----
  val fAccess = mem.portA.DO.asTypeOf(bramRowType)
  
  io.lru.addr_o := fAddr // forward to LRU
  io.lru.addr_vo := io.frontend.req_i.valid

  // ----- CYCLE 1: Read out or trigger state machine -----
  val matchBits = fAccess.map({ x=>
    x.checkHit(fAddr, fThread) && x.v
  }) // get all the comparison results
  val hitsWhich = OHToUInt(matchBits) // encode from the comparison
  val isHit: Bool = PopCount(matchBits) === 1.U // this value should only be 1 or zero.

  //! TBD here for how to handle the permission check of bits.
  val isValid = fAccess(hitsWhich).valid(permission_r)

  io.frontend.rep_o.valid := f_v_s0 && isHit && isValid
  io.frontend.rep_o.bits.data := fAccess(hitsWhich).read() // read

  io.lru.index_o := hitsWhich
  io.lru.index_vo := isHit

  class WritebackPacket extends Bundle{
    val addr = UInt(param.addressWidth.W)  // address
    val threadID = UInt(param.threadIDWidth().W) // tid
    val wayIndex = UInt(param.wayWidth().W) // which position to replace
    val oldDataBlock = UInt(param.blockBit.W)
    val dataBlock = UInt(param.blockBit.W)
    val mask = UInt(param.blockBit.W)
  }

  // Write request from the frontend
  val frontendWBPort = Wire(Decoupled(new WritebackPacket))

  if(param.writable){
    val wData_r = regOrNot(io.frontend.req_i.bits.wData.get)
    val wMask_r = regOrNot(io.frontend.req_i.bits.wMask.get)
    frontendWBPort.valid := wv_r && isHit && isValid
    frontendWBPort.bits.addr := fAddr_s0
    frontendWBPort.bits.dataBlock := wData_r
    frontendWBPort.bits.threadID := fThread_s0
    frontendWBPort.bits.wayIndex := Mux(isHit, hitsWhich, io.lru.lru_i) 
    frontendWBPort.bits.mask := wMask_r
    frontendWBPort.bits.oldDataBlock := fAccess(hitsWhich).read()
    io.frontend.req_i.ready := Mux(io.frontend.req_i.bits.w_v.get, frontendWBPort.ready, true.B) // new write request will not be handled until the writing back is processed.
  } else {
    frontendWBPort.bits := DontCare
    frontendWBPort.valid := false.B
    io.frontend.req_i.ready := true.B
  }

  // -------- Push missing request to the backend --------
  class F2BData extends Bundle{
    val addr = UInt(param.addressWidth.W)
    val threadID = UInt(param.threadIDWidth().W)
    val lru = UInt(param.wayWidth().W)
  }

  val frontFetchRequest = Wire(Decoupled(new F2BData))
  frontFetchRequest.bits.addr := fAddr_s0
  frontFetchRequest.bits.threadID := fThread_s0
  frontFetchRequest.bits.lru := io.lru.lru_i
  frontFetchRequest.valid := !isHit && f_v_s0 && isValid // when miss occurs, push request to the backend.

  val frontToBackFIFO = Queue(frontFetchRequest, param.threadNumber) // FIFO to transmit the missing request from front to back.


  // ######## BACKEND ########
  // ---- Read from the DRAM ----
  val eB_IDLE :: eB_WAIT :: eB_WRITE :: Nil = Enum(3)
  val backendState_r = RegInit(eB_IDLE)
  val backendReady = backendState_r === eB_IDLE && io.backend.rAddr_o.ready

  io.backend.rAddr_o.valid := backendState_r === eB_IDLE && frontToBackFIFO.valid
  io.backend.rAddr_o.bits := frontToBackFIFO.bits.addr
  frontToBackFIFO.ready := backendReady // when backend is ready, eject one term from the frontToBackFIFO.

  val backendAcceptData = backendState_r === eB_WAIT // the backend is ready to receive the data from the DRAM
  io.backend.rData_i.ready := backendAcceptData

  // val backend_context_t =  // the bundle of the working context of the backend.
  val backendWB_r = Reg(new WritebackPacket) 
  backendWB_r.oldDataBlock := 0.U
  // update logic of backend_context
  when(frontToBackFIFO.valid && backendReady){
    // save address and thread from the FIFO
    backendWB_r.addr := frontToBackFIFO.bits.addr
    backendWB_r.threadID := frontToBackFIFO.bits.threadID
    backendWB_r.wayIndex := frontToBackFIFO.bits.lru
  }.elsewhen(io.backend.rData_i.valid && backendAcceptData){
    backendWB_r.dataBlock := io.backend.rData_i.bits
    backendWB_r.mask := Fill(param.blockBit, 1.U(1.W)) // update from the backend is undoubtedly a replacement.
  }
  val backendWBPort = Wire(Decoupled(new WritebackPacket)) // from DRAM to Data bank.
  backendWBPort.bits := backendWB_r
  backendWBPort.valid := backendState_r === eB_WRITE

  // State machine of the backend logic
  switch(backendState_r){
    is(eB_IDLE){
      backendState_r := Mux(frontToBackFIFO.valid && backendReady, eB_WAIT, eB_IDLE)
    }
    is(eB_WAIT){ // wait the data response from the backend port
      backendState_r := Mux(io.backend.rData_i.valid && backendAcceptData, eB_WRITE, eB_WAIT)
    }
    is(eB_WRITE){ // Wait write to complete.
      backendState_r := Mux(backendWBPort.ready, eB_IDLE, eB_WRITE)
    }
  }

  // diverter the frontendWBPort. It should point to both the data block as well as the DRAM.
  var diversion: Vec[DecoupledIO[WritebackPacket]] = null
  if(param.writable){
    diversion = Diverter(2, frontendWBPort)
  } else {
    diversion = Diverter(1, frontendWBPort)
  }

  // Write port arbiter 
  val arb = Module(new RRArbiter(new WritebackPacket, 2))
  arb.io.in(0) <> backendWBPort
  arb.io.in(1) <> diversion(0)

  val writeBackPacket = arb.io.out // arbiter result.
  writeBackPacket.ready := true.B

  // full write back data. We only modify the relevant part.
  val memWBEntry = 0.U.asTypeOf(bramRowType) // if update, we need to read it out first?
  val memWBUpdatedDataBlock = VecInit(Seq.tabulate(param.blockBit)({ i =>
    Mux(writeBackPacket.bits.mask(i), writeBackPacket.bits.dataBlock(i), writeBackPacket.bits.oldDataBlock(i))
  })).asUInt()
  val updaterMemWBEntry = memWBEntry(writeBackPacket.bits.wayIndex).buildFrom(writeBackPacket.bits.addr, writeBackPacket.bits.threadID, memWBUpdatedDataBlock)

  mem.portB.EN := writeBackPacket.valid
  mem.portB.WE := UIntToOH(writeBackPacket.bits.wayIndex)
  mem.portB.ADDR := writeBackPacket.bits.addr
  mem.portB.DI := updaterMemWBEntry.asUInt()

  // ######## BACKEND Write ########
  if(param.writable){
    val backendWriteThrough = Wire(Decoupled(new WritebackPacket))
    backendWriteThrough <> diversion(1)
    val b2mem_fifo = Queue(backendWriteThrough, 4*param.threadNumber)
    val w_req = io.backend.wReq_o.get
    w_req.valid := b2mem_fifo.valid
    b2mem_fifo.ready := w_req.ready
    w_req.bits.addr := b2mem_fifo.bits.addr
    w_req.bits.data := b2mem_fifo.bits.dataBlock
  }

  // ######## Notify ########
  // Wake up
  io.threadNotify.wakeup_o.bits := backendWB_r.threadID
  io.threadNotify.wakeup_o.valid := backendWBPort.valid && backendWBPort.ready // when the write back is committed, wake up the thread.
  // Sleep
  io.threadNotify.sleep_o.bits := frontFetchRequest.bits.threadID
  io.threadNotify.sleep_o.valid := frontFetchRequest.valid && frontFetchRequest.ready // when the miss request is committed.

  io.threadNotify.kill_o.bits := fThread_s0
  io.threadNotify.kill_o.valid := !isValid
}


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
 * @param lruCore the LRU updating logic. See LRUCore.scala for all options.
 */ 
class BaseCache[T_ENTRY <: DataBankEntry, T_LRU_CORE <: LRUCore](
  param: CacheParameter,
  entry: T_ENTRY,
  lruCore: () => LRUCore,
  implementedWithRegister: Boolean = false
) extends Module{
  val dataBank = Module(new DataBank(param, entry, implementedWithRegister))
  val lru = Module(new LRU(param, lruCore, implementedWithRegister))
  val io = IO(new Bundle{
    val frontend = dataBank.io.frontend.cloneType
    val backend = dataBank.io.backend.cloneType
    val threadNotify = dataBank.io.threadNotify.cloneType
  })

  io.frontend <> dataBank.io.frontend
  io.backend <> dataBank.io.backend
  io.threadNotify <> dataBank.io.threadNotify

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