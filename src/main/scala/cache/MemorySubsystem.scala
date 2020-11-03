package armflex.cache

import chisel3._
import chisel3.util._
import chisel3.experimental._

import armflex.{
  MInst,
  MemReq,
  ProcConfig,
  CommitInst,
  TLBMiss,
  ValidTag,
  TLBFill,
  RFileIO,
}

import arm.PROCESSOR_TYPES
import arm.DECODE_CONTROL_SIGNALS._

class MemorySubsystem(
  param: CacheParameter,
  pageNumberWidth: Int,
  pageBiasWidth: Int = 12, // log2(4k)
)(implicit val cfg: ProcConfig) extends Module{
  val io = IO(new Bundle{
    // true if currently we are handing TLB missing.
    val selHost = Output(Bool())
    // true if currently we are not handling missing.
    val selMem = Output(Bool())
    // ports to ask transplanter to fetch new entry to TLB.
    val tlbMiss_o = Output(ValidTag(PROCESSOR_TYPES.MISS_T, new TLBMiss))
    // ports to receive the new TLB entries
    val tlb_entry_i = Input(Valid(new TLBFill))
    // listen to the D port of the commitReg
    val commitEnq_i = Input(Decoupled(new CommitInst))
    // listen to the Q port of the commitReg
    val commitDeq_i = Input(Decoupled(new CommitInst))
    
    // Register file port
    val rfile_o = Flipped(new RFileIO) // connect to the register
    val rfile_w_vo = Output(Bool()) // write is enabled
    val rfile_r_vo = Output(Bool()) // read is enabled

    // busy
    val busy_o = Output(Bool())

    // exception
    val unalignedException_vo = Output(Bool())
  })
}

/**
 * This module plays two roles:
 * 
 * 1. Convert the cache port to the `minst` port of the pipeline.
 * 2. Handle the pairLoad / Store.
 * 
 */ 

// Handle pair instruction.
class MemoryInterfaceAdaptor(
  param: CacheParameter
)(
  implicit val cfg: ProcConfig
) extends MultiIOModule{
  val blockAddrWidth = log2Ceil(param.blockBit)
  val frontend = IO(new Bundle{
    val req_i = Flipped(Decoupled(new Bundle{
      val vaddr = UInt(PROCESSOR_TYPES.VADDR.W)
      val size = UInt(2.W)
      val threadID = UInt(cfg.NB_THREADS_W.W)
      val pair_v = Bool()
      // write part.
      val w_v = if(param.writable) Some(Bool()) else None
      val wdata = if(param.writable) Some(Vec(2, PROCESSOR_TYPES.DATA_T)) else None
    }))
    val reply_o = Decoupled(new Bundle{
      val pair_v = Output(Bool())
      val threadID = Output(UInt(param.threadIDWidth().W))
      val rdata = Output(Vec(2, PROCESSOR_TYPES.DATA_T))
    })
  })

  val toCache = IO(new Bundle{
    val req_o = Decoupled(new DataBankFrontendPort(
      (PROCESSOR_TYPES.VADDR - blockAddrWidth),
      cfg.NB_THREADS_W,
      param.blockBit,
      param.writable
    ))
    val reply_i = Flipped(Decoupled(new Bundle{
      val data = UInt(param.blockBit.W)
      val threadID = UInt(param.threadIDWidth().W)
      val hit = Bool()
    }))
    val packet_arrive_i = Flipped(Valid(new Bundle{
      val threadID = UInt(param.threadIDWidth().W)
      val groupedFlag = Bool()
    }))
  })

  val threadNotify = IO(new Bundle{
    val wake_o = Valid(UInt(param.threadIDWidth().W))
    //val kill_o = Valid(UInt(param.threadIDWidth().W))
    val sleep_o = Valid(UInt(param.threadIDWidth().W))
  })

  private def placeData(data: UInt, size: UInt, addrBias: UInt, isRecovery: Boolean = false): (UInt, UInt) = { // (data, mask)
    val accessMaskingInByte = MuxLookup(size, 0.U, Array(
      SIZEB -> 1.U((PROCESSOR_TYPES.DATA_SZ / 8).W),
      SIZEH -> 3.U((PROCESSOR_TYPES.DATA_SZ / 8).W),
      SIZE32 -> 15.U((PROCESSOR_TYPES.DATA_SZ / 8).W),
      SIZE64 -> Fill((PROCESSOR_TYPES.DATA_SZ / 8), true.B)
    ))
    val accessMaskingInBit = VecInit(accessMaskingInByte.asBools().map(Fill(8, _))).asUInt()
    if(isRecovery){
      assert(data.getWidth == param.blockBit)
      ((data >> (addrBias << 3)) & accessMaskingInBit, accessMaskingInBit)
    } else {
      assert(data.getWidth == PROCESSOR_TYPES.DATA_SZ)
      val res = WireInit(UInt(param.blockBit.W), data) // apply mask before logic extending to 512bit.
      (res << (addrBias << 3), accessMaskingInBit << (addrBias << 3))
    }
  }

  // Judge whether this is out of bounds.
  val vaddrOfBlock = frontend.req_i.bits.vaddr(blockAddrWidth - 1, 0)
  val vaddrPairBias = MuxLookup(frontend.req_i.bits.size, 4.U, Array(
    SIZEB -> 1.U,
    SIZEH -> 2.U,
    SIZE32 -> 4.U,
    SIZE64 -> 8.U
  ))
  val vaddrOfPair = vaddrOfBlock +& vaddrPairBias
  val pairDisassemble_v = vaddrOfPair(blockAddrWidth) && frontend.req_i.bits.pair_v
  val pairNotDisassemble_v = frontend.req_i.bits.pair_v && !(vaddrOfPair(blockAddrWidth))

  // alignment exception. make sure that related bits are zero.
  val aligned = Vec(3, Wire(Bool())) // half-word, word, and double-word.
  for(i <- 0 until 3){
    aligned(i) := (frontend.req_i.bits.vaddr & Fill(i+1, 1.B)) === 0.U // 16: 0, 32: 00, 64: 000
  }
  val unalignedException_o = IO(Bool())
  unalignedException_o := MuxLookup(frontend.req_i.bits.size, 0.B, Array(
    SIZEB -> 0.B,
    SIZEH -> !aligned(0),
    SIZE32 -> !aligned(1),
    SIZE64 -> !aligned(2)
  ))

  // the second request in the transaction group.
  val secondTrans_r = Reg(new Bundle{
    val vaddr = UInt(PROCESSOR_TYPES.VADDR.W)
    val size = UInt(2.W)
    val threadID = UInt(cfg.NB_THREADS_W.W)
    val pair_v = Bool()
    val v = Bool()

    val w_v = if(param.writable) Some(Bool()) else None
    val wdata = if(param.writable) Some(PROCESSOR_TYPES.DATA_T.cloneType) else None
    val wmask = if(param.writable) Some(UInt((PROCESSOR_TYPES.DATA_SZ / 8).W)) else None
  })

  secondTrans_r.vaddr := frontend.req_i.bits.vaddr + vaddrPairBias
  secondTrans_r.size := frontend.req_i.bits.size
  secondTrans_r.threadID := frontend.req_i.bits.threadID
  secondTrans_r.pair_v := frontend.req_i.bits.pair_v
  secondTrans_r.v := frontend.req_i.fire() && pairDisassemble_v && !unalignedException_o

  if(param.writable){
    secondTrans_r.w_v.get := frontend.req_i.bits.w_v.get
    val wplacement_result = placeData(frontend.req_i.bits.wdata.get(1), frontend.req_i.bits.size, vaddrOfPair(blockAddrWidth-1, 0))
    secondTrans_r.wdata.get := wplacement_result._1
    secondTrans_r.wmask.get := wplacement_result._2
  }

  // To handle reply, we need a bridge that could forward the incoming information from the request part to the reply part.
  class Requester2ReceiverPacket extends Bundle{
    val blockAddr = Vec(2, UInt(blockAddrWidth.W)) // address for the block
    val size = UInt(2.W)
    val threadID = UInt(param.threadIDWidth().W)
    val pair_v = Bool()
    val transactionGroupID = UInt(2.W) // 00: not in the group; 01: the first transaction; 10： the second element; 11: don't care
  }

  val requestToReceiverPacket = Wire(Decoupled(new Requester2ReceiverPacket)) // For non grouped request.

  requestToReceiverPacket.bits.blockAddr(0) := vaddrOfBlock
  requestToReceiverPacket.bits.blockAddr(1) := vaddrOfPair(blockAddrWidth-1, 0)
  requestToReceiverPacket.bits.size := frontend.req_i.bits.size
  requestToReceiverPacket.bits.threadID := frontend.req_i.bits.threadID
  requestToReceiverPacket.bits.pair_v := frontend.req_i.bits.pair_v
  requestToReceiverPacket.bits.transactionGroupID := Mux(pairDisassemble_v, 1.U, 0.U)
  
  requestToReceiverPacket.valid := frontend.req_i.valid && !unalignedException_o
  frontend.req_i.ready := receiverPartPacket.ready

  val requestToReceiverPacketSecond = Wire(DecoupledIO(new Requester2ReceiverPacket)) // For grouped request.
  requestToReceiverPacketSecond.bits.blockAddr(0) := secondTrans_r.vaddr
  requestToReceiverPacketSecond.bits.blockAddr(1) := 0.U
  requestToReceiverPacketSecond.bits.size := secondTrans_r.size
  requestToReceiverPacketSecond.bits.threadID := secondTrans_r.threadID
  requestToReceiverPacketSecond.bits.pair_v := secondTrans_r.pair_v
  requestToReceiverPacketSecond.bits.transactionGroupID := 2.U

  requestToReceiverPacketSecond.valid := secondTrans_r.v

  val arb = Module(new Arbiter(new Requester2ReceiverPacket, 2))
  arb.io.in(0) <> requestToReceiverPacketSecond // the replicator is in high priority.
  arb.io.in(1) <> requestToReceiverPacket

  /**
   * Convert the arbiter output to the transaction of cache.
   * TODO: Add the logic to access TLB.
   */
  toCache.req_o.bits.addr := Mux(secondTrans_r.v, secondTrans_r.vaddr, frontend.req_i.bits.vaddr) //! ADDRESS TRANSLATION!!!
  toCache.req_o.bits.threadID := Mux(secondTrans_r.v, secondTrans_r.threadID, frontend.req_i.bits.threadID)
  toCache.req_o.valid := arb.io.out.valid
  if(param.writable){
    toCache.req_o.bits.wpermission := Mux(secondTrans_r.v, secondTrans_r.w_v.get, frontend.req_i.bits.w_v.get)
    val wplacement_result = placeData(frontend.req_i.bits.wdata.get(0), frontend.req_i.bits.size, vaddrOfBlock)
    toCache.req_o.bits.wData.get := wplacement_result._1
    toCache.req_o.bits.wMask.get := wplacement_result._2
    toCache.req_o.bits.w_v.get := Mux(secondTrans_r.v, secondTrans_r.w_v.get, frontend.req_i.bits.w_v.get)
  } else {
    toCache.req_o.bits.wpermission := false.B
  }
  //toCache.req_o.bits.permission :

  val receiverPartPacket = Queue(arb.io.out, 2)

  assert(receiverPartPacket.valid === toCache.reply_i.valid, "cache reply and receiver packet should be valid at the same time!")
  // pop out the queue as long as a new requirement is get.
  receiverPartPacket.ready := true.B
  toCache.reply_i.ready := true.B //? always ready?

  val shiftedRes = receiverPartPacket.bits.blockAddr.map(placeData(
    toCache.reply_i.bits.data, 
    receiverPartPacket.bits.size,
    _,
    true
  )._1)

  // return result.
  class ReplyToCachePacket extends Bundle{
    val resultPart = frontend.reply_o.cloneType
    val wakeup = Valid(UInt(param.threadIDWidth().W))
    val sleep = Valid(UInt(param.threadIDWidth().W))
  }

  val directlyHitResultPass = new ReplyToCachePacket // for single and hit transaction
  val groupedResultPass = new ReplyToCachePacket

  // Result: directed transaction
  directlyHitResultPass.resultPart.bits.pair_v := receiverPartPacket.bits.pair_v
  directlyHitResultPass.resultPart.bits.rdata := VecInit(shiftedRes)
  directlyHitResultPass.resultPart.bits.threadID := receiverPartPacket.bits.threadID
  directlyHitResultPass.resultPart.valid := receiverPartPacket.valid && receiverPartPacket.bits.transactionGroupID === 0.U && toCache.reply_i.bits.hit


  // Result: grouped transaction
  val firstDataPacket = Reg(Vec(param.threadNumber, Valid(PROCESSOR_TYPES.DATA_T)))
  for(i <- 0 until param.threadNumber){
    when(receiverPartPacket.bits.threadID === i.U && receiverPartPacket.valid){
      firstDataPacket(i).valid := MuxLookup(receiverPartPacket.bits.transactionGroupID, 0.B, Array(
        1.U -> toCache.reply_i.bits.hit, // reserve the first packet
        2.U -> 0.B  // clean the result regardless of whether the second transaction miss.
      ))
      firstDataPacket(i).bits := shiftedRes(0) // always keep the first packet.
    }
  }
  groupedResultPass.resultPart.bits.pair_v := receiverPartPacket.bits.pair_v
  groupedResultPass.resultPart.bits.rdata(0) := firstDataPacket(receiverPartPacket.bits.threadID).bits
  groupedResultPass.resultPart.bits.rdata(1) := shiftedRes(1)
  groupedResultPass.resultPart.bits.threadID := receiverPartPacket.bits.threadID
  groupedResultPass.resultPart.valid := receiverPartPacket.valid &&
    firstDataPacket(receiverPartPacket.bits.threadID).valid && // the first data packet is valid
    receiverPartPacket.bits.transactionGroupID === 2.U && // the second data packet is valid
    toCache.reply_i.bits.hit // the second data packet is hit

  
  val frontendReplyArb = Module(new Arbiter(frontend.reply_o.bits.cloneType, 2))
  frontendReplyArb.io.in(0) <> directlyHitResultPass
  frontendReplyArb.io.in(1) <> groupedResultPass

  frontend.reply_o <> frontendReplyArb.io.out
  when(frontend.reply_o.valid){
    assert(frontend.reply_o.ready === true.B, "The frontend should always ready to accept the requests from the memory subsystem.")
  }

  // wake up: directed transaction
  class GroupedWakeUpTable extends Bundle{
    val v = Bool()
    val arrive_v = Vec(2, Bool())
  }

  val wakeupCounter = Reg(Vec(2, UInt(2.W))) // counters for unhandled missing packet

  // wake up: grouped transaction
  for(i <- 0 until param.threadNumber){
    // monitor three points, arrive, hit, and miss.
    val newGroupedMiss = receiverPartPacket.valid && 
                        receiverPartPacket.bits.transactionGroupID =/= 0.U && 
                        !toCache.reply_i.bits.hit && receiverPartPacket.bits.threadID === i.U
    val newGroupedArrive = toCache.packet_arrive_i.valid && 
                        toCache.packet_arrive_i.bits.groupedFlag && 
                        toCache.packet_arrive_i.bits.threadID === i.U

    assert(newGroupedMiss =/= newGroupedArrive, "It's impossible for the module to handle a grouped miss and a grouped arrival at the same time.")

    when(newGroupedMiss){
      wakeupCounter(i) := wakeupCounter(i) + 1.U
    }.elsewhen(newGroupedArrive){
      wakeupCounter(i) := wakeupCounter(i) - 1.U
    }
  }

  threadNotify.wake_o.bits := toCache.packet_arrive_i.bits.threadID
  threadNotify.wake_o.valid := toCache.packet_arrive_i.valid &&  Mux(
    toCache.packet_arrive_i.bits.groupedFlag, 
    wakeupCounter(toCache.packet_arrive_i.bits.threadID) === 1.U, // only one packet left, which is arrival one.
    true.B
  )

  threadNotify.sleep_o.valid := receiverPartPacket.valid && !toCache.reply_i.bits.hit
  threadNotify.sleep_o.bits := receiverPartPacket.bits.threadID
}