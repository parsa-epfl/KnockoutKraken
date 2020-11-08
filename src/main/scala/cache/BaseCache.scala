package armflex.cache

import chisel3._
import chisel3.util._

import chisel3.stage.ChiselStage

import armflex.util.Diverter
import armflex.util.BRAMConfig
import armflex.util.BRAMPort
import armflex.util.BRAM
import armflex.util.FlushQueue
//import firrtl.PrimOps.Mul

import scala.collection.mutable
import treadle.executable.DataType
import scala.xml.dtd.impl.Base

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
  // backend data width. Sometimes this is not equal to the block size, for instance, TLB.
  backendPortSize: Int = 512,
  // the data bank is implemented in register?
  implementedWithRegister: Boolean = false
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

  def setWidth(): Int = {
    log2Ceil(setNumber)
  }
  
}

object DataBankManager{
class FrontendRequestPacket(
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

  override def cloneType(): this.type = new FrontendRequestPacket(addressWidth, threadIDWidth, blockSize, writable).asInstanceOf[this.type]
}

class FrontendReplyPacket(param: CacheParameter) extends Bundle{
  val data = UInt(param.blockBit.W)
  val threadID = UInt(param.threadIDWidth().W)
  val hit = Bool()

  override def cloneType(): this.type = new FrontendReplyPacket(param).asInstanceOf[this.type]
}

class MissQueuePacket(param: CacheParameter) extends Bundle{
  val addr = UInt(param.addressWidth.W)
  val threadID = UInt(param.threadIDWidth().W)
  val lru = UInt(param.wayWidth().W)
  val grouped_v = Bool()

  override def cloneType(): this.type = new MissQueuePacket(param).asInstanceOf[this.type]
}

class BackendRequestPacket(param: CacheParameter) extends Bundle{
  val addr = UInt(param.addressWidth.W)
  val wreq = if(param.writable) Some(new Bundle{
    val data = UInt(param.backendPortSize.W)
    val v = Bool()
  }) else None

  override def cloneType(): this.type = new BackendRequestPacket(param).asInstanceOf[this.type]
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

class BankWriteRequestPacket[ENTRY <: DataBankEntry](
  param: CacheParameter,
  tEntryGenerator: ENTRY
) extends Bundle{
  val addr = UInt(param.setWidth().W)
  val which = UInt(param.wayWidth().W)
  val data = tEntryGenerator.cloneType

  override def cloneType: this.type = new BankWriteRequestPacket(param, tEntryGenerator).asInstanceOf[this.type]
}

class BRAMPortAdapter[ENTRY <: DataBankEntry](
  param: CacheParameter,
  tEntryGenerator: ENTRY,
) extends MultiIOModule{
  val set_t = Vec(param.associativity, tEntryGenerator.cloneType)

  val frontendReadRequest_i = IO(Flipped(Decoupled(UInt(param.setWidth().W))))
  val frontendReadReplyData_o = IO(Decoupled(set_t.cloneType))

  implicit val bramCfg = new BRAMConfig(
    param.associativity, tEntryGenerator.getWidth, param.setNumber
  )

  val bramPortA = IO(Flipped(new BRAMPort()))
  bramPortA.ADDR := frontendReadRequest_i.bits
  bramPortA.EN := true.B
  bramPortA.WE := 0.U
  bramPortA.DI := 0.U
  frontendReadReplyData_o.bits := bramPortA.DO.asTypeOf(set_t.cloneType)
  if(param.implementedWithRegister){
    frontendReadReplyData_o.valid := frontendReadRequest_i.valid
    frontendReadRequest_i.ready := true.B
  } else {
    frontendReadReplyData_o.valid := RegNext(frontendReadRequest_i.valid)
    frontendReadRequest_i.ready := true.B
  }

  val frontendWriteRequest_i = IO(Flipped(Decoupled(new BankWriteRequestPacket(param, tEntryGenerator))))
  val bramPortB = IO(Flipped(new BRAMPort()))
  bramPortB.ADDR := frontendWriteRequest_i.bits.addr

  val writeValue = Wire(set_t.cloneType)
  writeValue := 0.U.asTypeOf(set_t.cloneType)
  writeValue(frontendWriteRequest_i.bits.which) := frontendWriteRequest_i.bits.data

  bramPortB.DI := writeValue.asUInt()
  bramPortB.EN := frontendWriteRequest_i.valid
  bramPortB.WE := UIntToOH(frontendWriteRequest_i.bits.which) & Fill(param.associativity, frontendWriteRequest_i.valid)
  frontendWriteRequest_i.ready := true.B
}

class DataBankFrontend[ENTRY <: DataBankEntry](
  param: CacheParameter,
  tEntryGenerator: ENTRY
) extends MultiIOModule{
  // Ports to frontend
  val frontendRequest_i = IO(Flipped(Decoupled(new FrontendRequestPacket(param))))
  val frontendReply_o = IO(Valid(new FrontendReplyPacket(param)))

  // Ports to Bank RAM (Read port)
  val set_t = Vec(param.associativity, tEntryGenerator.cloneType)

  val bankRamRequestAddr_o = IO(Decoupled(UInt(param.setWidth().W)))
  val bankRamReplyData_i = IO(Flipped(Decoupled(set_t.cloneType)))

  val bankRamWriteRequest_o = IO(Decoupled(new BankWriteRequestPacket(param, tEntryGenerator)))

  // Port to Backend (Read Request)
  val backendRequest_o = IO(Decoupled(new BackendRequestPacket(param)))
  val backendReadReply_i = IO(Flipped(Decoupled(UInt(param.backendPortSize.W))))

  // Port to LRU
  val lruPort = IO(new Bundle{
    val addr_o = Output(UInt(param.addressWidth.W))
    val addr_vo = Output(Bool())

    val index_o = Output(UInt(param.wayWidth().W))
    val index_vo = Output(Bool())

    val lru_i = Input(UInt(param.wayWidth().W))
  })

  val pipelineStateReady = Wire(Vec(3, Bool()))

  frontendRequest_i.ready := pipelineStateReady(0)

  // Store request
  val s1_frontendRequest_n = Wire(Decoupled(new FrontendRequestPacket(param)))
  s1_frontendRequest_n.bits := frontendRequest_i.bits
  s1_frontendRequest_n.valid := frontendRequest_i.fire()
  val s1_frontendRequest_r = if(param.implementedWithRegister) FlushQueue(s1_frontendRequest_n, 0, true)
  else FlushQueue(s1_frontendRequest_n, 1, true) 
  s1_frontendRequest_r.ready := pipelineStateReady(1)

  // pass to the bram
  bankRamRequestAddr_o.bits := frontendRequest_i.bits.addr(param.setWidth-1, 0)
  bankRamRequestAddr_o.valid := frontendRequest_i.fire()

  // pass to the LRU
  lruPort.addr_o := frontendRequest_i.bits.addr(param.setWidth-1, 0)
  lruPort.addr_vo := bankRamRequestAddr_o.fire()

  // fetch data from the bram
  bankRamReplyData_i.ready := s1_frontendRequest_r.valid // If transaction is valid, then we accept the result.
  
  val matchBits = bankRamReplyData_i.bits.map({ x=>
    x.checkHit(s1_frontendRequest_r.bits.addr, s1_frontendRequest_r.bits.threadID) && x.v
  }) // get all the comparison results
  val hitsWhich = OHToUInt(matchBits) // encode from the comparison
  val isHit: Bool = PopCount(matchBits) === 1.U // this value should only be 1 or zero.
  val hitEntry = bankRamReplyData_i.bits(hitsWhich)
  
  val isValid = hitEntry.valid(s1_frontendRequest_r.bits.wpermission)

  frontendReply_o.valid := s1_frontendRequest_r.fire() && isValid
  frontendReply_o.bits.data := hitEntry.read() // read
  frontendReply_o.bits.threadID := s1_frontendRequest_r.bits.threadID
  frontendReply_o.bits.hit := isHit // if not head, also return.

  lruPort.index_o := hitsWhich
  lruPort.index_vo := isHit

  // forwarding path from last writing to this read. (RAW)

  class WritingContext extends Bundle{
    val addr = UInt(param.addressWidth.W)
    val threadID = UInt(param.threadIDWidth().W)
    val dataBlock = UInt(param.backendPortSize.W)
    def toEntry(): DataBankEntry = {
      tEntryGenerator.buildFrom(addr, threadID, dataBlock)
    }
  }
  val s2_writing_n = Wire(Decoupled(new WritingContext))
  val s2_writing_r = FlushQueue(s2_writing_n, 1, true)
  s2_writing_r.ready := pipelineStateReady(2)
  val frontendWriteToBank = Wire(Decoupled(new BankWriteRequestPacket(param, tEntryGenerator)))
  if(param.writable){
    val forwardHitEntry = Mux(
      s2_writing_r.valid &&  // valid
      s2_writing_r.bits.addr === s1_frontendRequest_r.bits.addr && // hit
      isHit, // the entry could be found in the BRAM
      s2_writing_r.bits.toEntry(),
      hitEntry
    )

    val updatedEntry = forwardHitEntry.updateData(
      s1_frontendRequest_r.bits.wData.get, 
      s1_frontendRequest_r.bits.wMask.get
    )

    frontendReply_o.bits.data := forwardHitEntry.read()

    frontendWriteToBank.bits.data := updatedEntry
    frontendWriteToBank.bits.addr := s1_frontendRequest_r.bits.addr(param.setWidth()-1, 0)
    frontendWriteToBank.bits.which := hitsWhich
    frontendWriteToBank.valid := s1_frontendRequest_r.valid && isHit && isValid && s1_frontendRequest_r.bits.w_v.get
    assert(
      frontendWriteToBank.valid === frontendWriteToBank.ready,
      "frontend write back request must be put in the first priority."
    )

    s2_writing_n.bits.addr := s1_frontendRequest_r.bits.addr
    s2_writing_n.bits.threadID := s1_frontendRequest_r.bits.threadID
    s2_writing_n.bits.dataBlock := updatedEntry.read()
    s2_writing_n.valid := frontendWriteToBank.valid
  } else {
    frontendWriteToBank.bits := DontCare
    frontendWriteToBank.valid := false.B
    s2_writing_n.bits := DontCare
    s2_writing_n.valid := false.B
  }
  
  // Missed request will also be pushed to the miss queue for sync with backend reply

  class MissRequestPacket extends Bundle{
    val addr = UInt(param.addressWidth.W)
    val threadID = UInt(param.threadIDWidth().W)
    val lru = UInt(param.wayWidth().W)
    val groupedFlag = Bool()
  }

  val frontFetchRequest = Wire(Decoupled(new MissRequestPacket))
  frontFetchRequest.bits.addr := s1_frontendRequest_r.bits.addr
  frontFetchRequest.bits.threadID := s1_frontendRequest_r.bits.threadID
  frontFetchRequest.bits.groupedFlag := s1_frontendRequest_r.bits.groupedFlag
  frontFetchRequest.bits.lru := lruPort.lru_i
  frontFetchRequest.valid := !isHit && s1_frontendRequest_r.valid && isValid && pipelineStateReady(2) //! listen to the s1_ready
  assert(frontFetchRequest.ready, "missQueue should never be full. ")

  val missQueue = Queue(frontFetchRequest, param.threadNumber * 2) // FIFO to transmit the missing request from front to back.
  val backendUpdateBankRequest = Wire(Decoupled(new BankWriteRequestPacket(param, tEntryGenerator)))
  backendUpdateBankRequest.bits.addr := missQueue.bits.addr(param.setWidth()-1, 0)
  backendUpdateBankRequest.bits.which := missQueue.bits.lru
  backendUpdateBankRequest.bits.data := tEntryGenerator.buildFrom(
    missQueue.bits.addr,
    missQueue.bits.threadID,
    backendReadReply_i.bits
  )
  backendUpdateBankRequest.valid := missQueue.valid && backendReadReply_i.valid
  missQueue.ready := backendUpdateBankRequest.ready
  backendReadReply_i.ready := backendUpdateBankRequest.ready

  val u_writeToBankArb = Module(new Arbiter(new BankWriteRequestPacket(param, tEntryGenerator), 2))
  u_writeToBankArb.io.in(0) <> frontendWriteToBank
  u_writeToBankArb.io.in(1) <> backendUpdateBankRequest
  bankRamWriteRequest_o <> u_writeToBankArb.io.out

  //missRequest_o <> missQueue

  // If Miss, then push the request to backend
  val s2_miss_n = Wire(Decoupled(new BackendRequestPacket(param)))
  s2_miss_n.bits.addr := s1_frontendRequest_r.bits.addr
  if(param.writable){
    s2_miss_n.bits.wreq.get.data := 0.U;
    s2_miss_n.bits.wreq.get.v := 0.U
  }
  s2_miss_n.valid := frontFetchRequest.valid
  val s2_miss_r = FlushQueue(s2_miss_n, 1, true)
  s2_miss_r.ready := pipelineStateReady(2)
  val backendReadRequest = Wire(Decoupled(new BackendRequestPacket(param)))
  backendReadRequest.bits := s2_miss_r.bits
  backendReadRequest.valid := s2_miss_r.valid
  val backendWriteRequest = Wire(Decoupled(new BackendRequestPacket(param)))
  backendWriteRequest.bits.addr := s2_writing_r.bits.addr
  backendWriteRequest.valid := s2_writing_r.fire()
  if(param.writable){
    backendReadRequest.bits.wreq.get.data := 0.U
    backendReadRequest.bits.wreq.get.v := false.B
    backendWriteRequest.bits.wreq.get.data := s2_writing_r.bits.dataBlock
    backendWriteRequest.bits.wreq.get.v := true.B
  }

  val u_backendArb = Module(new Arbiter(new BackendRequestPacket(param), 2))
  u_backendArb.io.in(0) <> backendReadRequest
  u_backendArb.io.in(1) <> backendWriteRequest
  assert(backendReadRequest.valid && backendWriteRequest.valid, "It's impossible for one transaction hit and miss at the same time.")
  backendRequest_o <> u_backendArb.io.out  

  pipelineStateReady(2) := backendRequest_o.ready
  pipelineStateReady(1) := s2_miss_n.ready && s2_writing_n.ready &&
  bankRamReplyData_i.valid === s1_frontendRequest_r.valid // BRAM response arrives.
  pipelineStateReady(0) := s1_frontendRequest_n.ready && bankRamRequestAddr_o.ready
}

class DataBankReseter extends MultiIOModule{
  
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
      val req_i = Flipped(Decoupled(new DataBankManager.FrontendRequestPacket(param)))
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
          val data = UInt(param.backendPortSize.W)
          val v = Bool()
        }) else None
      })
      val reply_i = Flipped(Decoupled(UInt(param.backendPortSize.W)))
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
    // we have a flag to indicate that there're pending backend request. Please directly use flush_i.ready
  })

  val bramRowType = Vec(param.associativity, t)
  implicit val bramConfig = new BRAMConfig(
    param.associativity,
    t.getWidth,
    param.setNumber
  )
  val mem = Module(new DataBankManager.BRAMorRegister(implementedWithRegister))

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

  // ######## FRONTEND ########
  val setWidth = log2Ceil(param.setNumber)
  val s0_context_n = Wire(Valid(new DataBankManager.FrontendRequestPacket(param)))
  var s0_context_r = Reg(Valid(new DataBankManager.FrontendRequestPacket(param)))
  val s1_ready = Wire(Bool())
  if(!implementedWithRegister){
    s0_context_r := Mux(
      flushing_r && flush_mask_r(s0_context_r.bits.threadID), // flush current register.
      0.U.asTypeOf(s0_context_n.cloneType),
      Mux(s1_ready, s0_context_n, s0_context_r)
    )

  } else {
    s0_context_r = s0_context_n
  }

  s0_context_n.bits.addr := io.frontend.req_i.bits.addr
  s0_context_n.bits.threadID := io.frontend.req_i.bits.threadID
  s0_context_n.bits.wpermission := io.frontend.req_i.bits.wpermission
  s0_context_n.bits.groupedFlag := io.frontend.req_i.bits.groupedFlag
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
    val dataBlock = UInt(param.backendPortSize.W)
    val groupedFlag = Bool()
  }

  // Write request from the frontend
  val frontendWBPort = Wire(Decoupled(new WritebackPacket))
  val s2_ready = Wire(Bool()) // whether you can update s1_*_r
  val s1_wlatch_r = Reg(new Bundle{ // forward latch of writing request
    val addr = UInt(param.addressWidth.W)
    val threadID = UInt(param.threadIDWidth().W)
    val dataBlock = UInt(param.backendPortSize.W)
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
    when(flushing_r && flush_mask_r(s1_wlatch_r.threadID)){
      s1_wlatch_r := 0.U
    }.elsewhen(s2_ready){
      s1_wlatch_r.addr := frontendWBPort.bits.addr
      s1_wlatch_r.dataBlock := frontendWBPort.bits.dataBlock
      s1_wlatch_r.v := frontendWBPort.fire()
      s1_wlatch_r.threadID := frontendWBPort.bits.threadID

      io.frontend.rep_o.bits.data := Mux(
        s1_wlatch_r.v && s1_wlatch_r.addr === s0_context_r.bits.addr && isHit, // when the last writing result is valid, shares same address, and has not been evicted
        s1_wlatch_r.dataBlock,
        fAccess(hitsWhich).read() // read
      )
    }
    io.frontend.rep_o.bits.hit := isHit // if not head, also return.
  } else {
    frontendWBPort.bits := DontCare
    frontendWBPort.valid := false.B
    io.frontend.req_i.ready := !flushing_r
    s1_wlatch_r := DontCare
    s1_wlatch_r.v := false.B
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
  frontFetchRequest.valid := !isHit && s0_context_r.valid && isValid && s1_ready // when miss occurs, push request to the backend.

  val missQueue = Queue(frontFetchRequest, param.threadNumber * 2) // FIFO to transmit the missing request from front to back.
  // question: Do we need to clear missQueue when flushing? It's sync with io.backend_reply_i

  val s2_miss_r = Reg(Valid(new MissRequestPacket))
  when(flushing_r && flush_mask_r(s2_miss_r.bits.threadID)){
    s2_miss_r := 0.U.asTypeOf(s2_miss_r.cloneType)
  }.elsewhen(s2_ready){
    s2_miss_r.bits := frontFetchRequest.bits
    s2_miss_r.valid := frontFetchRequest.valid
  }
  

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
  val flushing_s1_r = if(implementedWithRegister) RegNext(flushing_r) else flushing_r
  val flushing_s1_addr_r = if(implementedWithRegister) RegNext(flush_cnt.value) else flush_cnt.value

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
  fetchMissFromDRAM.bits.addr := s2_miss_r.bits.addr
  fetchMissFromDRAM.valid := s2_miss_r.valid
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
  assert(fetchMissFromDRAM.valid && writeThroughToDRAM.valid === 0.B, "Come on. Do you think for one instruction missing and writing-through could happen at the same time?")

  val writeThroughArb = Module(new Arbiter(io.backend.req_o.bits.cloneType, 2))
  writeThroughArb.io.in(0) <> fetchMissFromDRAM
  writeThroughArb.io.in(1) <> writeThroughToDRAM

  val writeThroughQueue = Queue(writeThroughArb.io.out, 4 * param.threadNumber)
  io.backend.req_o <> writeThroughQueue
  
  // to be determined:
  // - io.frontend.req_i.ready
  s2_ready := writeThroughArb.io.out.ready // the backend Q must be ready
  s1_ready := missQueue.ready && s2_ready && !flushing_s1_r // s2_ready and missQ ready and flushing WB is not busy
  io.frontend.req_i.ready := s1_ready && !flushing_r // s1 ready and flush RD is not busy

  // - io.flush_i.ready 
  io.flush_i.ready := !writeThroughQueue.valid // queue must be empty when flushing

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
 */ 
class LRU[T <: LRUCore](
  param: CacheParameter,
  lruCore: () => T
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

  val bram = Module(new DataBankManager.BRAMorRegister(param.implementedWithRegister))

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
class BaseCache[ENTRY <: DataBankEntry](
  val param: CacheParameter,
  entry: ENTRY,
  lruCore: () => LRUCore,
) extends MultiIOModule{
  val bankFrontend = Module(new DataBankManager.DataBankFrontend(param, entry))
  val bramAdapter = Module(new DataBankManager.BRAMPortAdapter(param, entry))

  implicit val cfg = bramAdapter.bramCfg
  val bram = Module(new DataBankManager.BRAMorRegister(param.implementedWithRegister))
  bramAdapter.bramPortA <> bram.portA
  bramAdapter.bramPortB <> bram.portB

  bramAdapter.frontendReadReplyData_o <> bankFrontend.bankRamReplyData_i
  bramAdapter.frontendReadRequest_i <> bankFrontend.bankRamRequestAddr_o
  bramAdapter.frontendWriteRequest_i <> bankFrontend.bankRamWriteRequest_o

  val frontendRequest_i = IO(Flipped(bankFrontend.frontendRequest_i.cloneType))
  frontendRequest_i <> bankFrontend.frontendRequest_i
  val frontendReply_o = IO(bankFrontend.frontendReply_o.cloneType)
  frontendReply_o <> bankFrontend.frontendReply_o

  val u_lruCore = Module(new LRU(param, lruCore))
  u_lruCore.io.addr_i := bankFrontend.lruPort.addr_o
  u_lruCore.io.addr_vi := bankFrontend.lruPort.addr_vo

  u_lruCore.io.index_i := bankFrontend.lruPort.index_o
  u_lruCore.io.index_vi := bankFrontend.lruPort.index_vo
  bankFrontend.lruPort.lru_i := u_lruCore.io.lru_o

  // Backend Queue
  val backendRequest_o = IO(bankFrontend.backendRequest_o.cloneType)
  backendRequest_o <> bankFrontend.backendRequest_o

  val backendReadReply_i = IO(Flipped(bankFrontend.backendReadReply_i.cloneType)) //! When clone a Flipped Decoupled type, remember to use Flipped to make it correct direction
  backendReadReply_i <> bankFrontend.backendReadReply_i
}

object BaseCache {
  def generateCache(param: CacheParameter, lruCore: () => LRUCore): BaseCache[CacheEntry] = {
    return new BaseCache(param, new CacheEntry(param), lruCore)
  }
  def generateL1TLB(param: CacheParameter, lruCore: () => LRUCore): BaseCache[TLBEntry] = {
    assert(!param.writable, "You're forbidden to create a writable TLB.")
    assert(!param.implementedWithRegister, "L1 TLB is implemented with registers.")
    return new BaseCache(param, new TLBEntry(param), lruCore)
  }
  def generateL2TLB(param: CacheParameter, lruCore: () => LRUCore): BaseCache[TLBEntry] = {
    assert(!param.writable, "You're forbidden to create a writable TLB.")
    return new BaseCache(param, new TLBEntry(param), lruCore)
  }
}

object BaseCacheVerilogGenerator extends App{
  println((new ChiselStage).emitVerilog(BaseCache.generateCache(
    new CacheParameter(64, 2, 512, 24, 4, true, 512, false),
    () => new PseudoTreeLRUCore(4)
  )))
}
