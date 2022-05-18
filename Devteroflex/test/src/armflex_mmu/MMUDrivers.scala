package armflex_mmu

import antmicro.Bus._
import armflex.util._
import armflex_cache.PageTableParams
import peripheral.PageTableSetPacket

import chisel3._
import chisel3.util.{log2Ceil, Cat}
import chiseltest._

import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._

import antmicro.CSR.AXI4LiteCSR
import armflex.util.AXIDrivers._
import armflex._
import armflex_cache._
import armflex.util.TestUtils
import MMUBundleDrivers._
import antmicro.util.CSRDrivers._
import com.google.protobuf.Extension.MessageType

// import armflex_mmu.MMUBundleDrivers.PageTableSetPacket._

object MMUHelpers {

  /**
   * This helper will convert a pageset packet to three 512bits raw data.
   *
   */
  class PageSetConverter(implicit params: PageTableParams) extends Module {
    val encode = IO(new Bundle {
      val packet = Input(new PageTableSetPacket(params))
      val vector = Output(Vec(3, UInt(512.W)))
    })
    val decode = IO(new Bundle {
      val vector = Input(Vec(3, UInt(512.W)))
      val packet = Output(new PageTableSetPacket(params))
    })
    encode.vector := VecInit(encode.packet.asUInt.asBools().grouped(512).map(VecInit(_).asUInt).toSeq)
    decode.packet := decode.vector.asUInt.asTypeOf(decode.packet.cloneType)
  }

  class RawMessageHelperIO(implicit params: PageTableParams) extends Bundle {
    val decode = new RawMessageHelperDecodeIO
    val encode = new RawMessageHelperEncodeIO
    val getType = new RawMessageHelperGetTypeIO
  }

  class RawMessageHelperDecodeIO(implicit params: PageTableParams) extends Bundle {
    val pack = Input(UInt(512.W))
    val msgPageEvictReq = Output(new QEMUPageEvictRequest(params))
    val msgMissReply = Output(new QEMUMissReply(params))
    val msgEvictReply = Output(new QEMUEvictReply(params))
    val msgPageEvictNotif = Output(new PageEvictNotification(QEMUMessagesType.sEvictNotify, params))
    val msgPageEvictDone = Output(new PageEvictNotification(QEMUMessagesType.sEvictDone, params))
    val msgPageFaultNotif = Output(new PageFaultNotification(params))
  }

  class RawMessageHelperGetTypeIO(implicit params: PageTableParams) extends Bundle {
    val msg = Input(UInt(512.W))
    val msgType = Output(UInt(QEMUMessagesType.encodingW.W))
  }

  class RawMessageHelperEncodeIO(implicit params: PageTableParams) extends Bundle {
    val message_type = Input(UInt(QEMUMessagesType.encodingW.W))
    val msgPageEvictReq = Input(new QEMUPageEvictRequest(params))
    val msgMissReply = Input(new QEMUMissReply(params))
    val msgEvictReply = Input(new QEMUEvictReply(params))
    val msgPageEvictNotif = Input(new PageEvictNotification(QEMUMessagesType.sEvictNotify, params))
    val msgPageEvictDone = Input(new PageEvictNotification(QEMUMessagesType.sEvictDone, params))
    val msgPageFaultNotif = Input(new PageFaultNotification(params))
    val unpack = Output(UInt(512.W))
  }

  class RawMessageHelper(implicit params: PageTableParams) extends Module {
    val io = IO(new RawMessageHelperIO)

    val vectorized = (new TxMessage).parseFromVec(VecInit(io.decode.pack.asBools().grouped(32).map{x=> Cat(x.reverse)}.toSeq))
    io.decode.msgPageEvictReq   := io.decode.msgPageEvictReq.parseFromVec(vectorized.data)
    io.decode.msgMissReply      := io.decode.msgMissReply.parseFromVec(vectorized.data)
    io.decode.msgEvictReply     := io.decode.msgEvictReply.parseFromVec(vectorized.data)
    io.decode.msgPageEvictNotif := io.decode.msgPageEvictNotif.parseFromVec(vectorized.data)
    io.decode.msgPageEvictDone  := io.decode.msgPageEvictDone.parseFromVec(vectorized.data)
    io.decode.msgPageFaultNotif := io.decode.msgPageFaultNotif.parseFromVec(vectorized.data)

    val isRx = WireInit(false.B)
    val txMessage = Wire(Vec(16, UInt(32.W)))
    val rxMessage = Wire(Vec(9, UInt(32.W)))
    txMessage.foreach(x => x := 0xDEADBEEF.S.asUInt)
    rxMessage.foreach(x => x := 0xDEADBEEF.S.asUInt)
    when(io.encode.message_type === QEMUMessagesType.sPageFaultNotify.U) {
      txMessage := io.encode.msgPageFaultNotif.getRawMessage.asVec
    }.elsewhen(io.encode.message_type === QEMUMessagesType.sEvictNotify.U) {
      txMessage := io.encode.msgPageEvictNotif.getRawMessage.asVec
    }.elsewhen(io.encode.message_type === QEMUMessagesType.sEvictDone.U) {
      txMessage := io.encode.msgPageEvictDone.getRawMessage.asVec
    }.elsewhen(io.encode.message_type === QEMUMessagesType.sPageEvict.U) {
      rxMessage := io.encode.msgPageEvictReq.getRawMessage.asVec
      isRx := true.B
    }.elsewhen(io.encode.message_type === QEMUMessagesType.sMissReply.U) {
      rxMessage := io.encode.msgMissReply.getRawMessage.asVec
      isRx := true.B
    }.elsewhen(io.encode.message_type === QEMUMessagesType.sEvictReply.U) {
      rxMessage := io.encode.msgEvictReply.getRawMessage.asVec
      isRx := true.B
    }
    io.encode.unpack := Mux(isRx, rxMessage.asUInt, txMessage.asUInt)

    val raw_message = (new TxMessage).parseFromVec(VecInit(io.getType.msg.asBools().grouped(32).map{x=> Cat(x.reverse)}.toSeq))
    io.getType.msgType := raw_message.message_type
  }

  implicit class RawHelperIOFunctions(target: RawMessageHelperIO) {
    def getMsgMMUType(msg: UInt) = { target.getType.msg.poke(msg); target.getType.msgType.peek().litValue }
    def encodeMsgPageEvictReq(msg: QEMUPageEvictRequest): UInt = { target.encode.message_type.poke(QEMUMessagesType.sPageEvict.U); target.encode.msgPageEvictReq.poke(msg); target.encode.unpack.peek() }
    def encodeMsgMissReply(msg: QEMUMissReply): UInt = { target.encode.message_type.poke(QEMUMessagesType.sMissReply.U); target.encode.msgMissReply.poke(msg); target.encode.unpack.peek() }
    def encodeMsgEvictReply(msg: QEMUEvictReply): UInt = { target.encode.message_type.poke(QEMUMessagesType.sEvictReply.U); target.encode.msgEvictReply.poke(msg); target.encode.unpack.peek() }
    def encodeMsgPageEvictNotifStart(msg: PageEvictNotification): UInt = { target.encode.message_type.poke(QEMUMessagesType.sEvictNotify.U);  target.encode.msgPageEvictNotif.poke(msg); target.encode.unpack.peek() }
    def encodeMsgPageEvictNotifDone(msg: PageEvictNotification): UInt = { target.encode.message_type.poke(QEMUMessagesType.sEvictDone.U);  target.encode.msgPageEvictDone.poke(msg); target.encode.unpack.peek() }
    def encodeMsgPageFaultNotif(msg: PageFaultNotification): UInt = { target.encode.message_type.poke(QEMUMessagesType.sPageFaultNotify.U); target.encode.msgPageFaultNotif.poke(msg); target.encode.unpack.peek() }
 
    def expectMsgPageEvictReq(msg: UInt, expect: QEMUPageEvictRequest) = { target.decode.pack.poke(msg); target.decode.msgPageEvictReq.expect(expect) }
    def expectMsgMissReply(msg: UInt, expect: QEMUMissReply) = { target.decode.pack.poke(msg); target.decode.msgMissReply.expect(expect) }
    def expectMsgEvictReply(msg: UInt, expect: QEMUEvictReply) = { target.decode.pack.poke(msg); target.decode.msgEvictReply.expect(expect) }
    def expectMsgPageEvictNotifStart(msg: UInt, expect: PageEvictNotification) = { target.decode.pack.poke(msg); target.decode.msgPageEvictNotif.expect(expect) }
    def expectMsgPageEvictNotifDone(msg: UInt, expect: PageEvictNotification) = { target.decode.pack.poke(msg); target.decode.msgPageEvictDone.expect(expect) }
    def expectMsgPageFaultNotif(msg: UInt, expect: PageFaultNotification) = { target.decode.pack.poke(msg); target.decode.msgPageFaultNotif.expect(expect) }
 
    def decodeMsgPageEvictReq(msg: UInt) : QEMUPageEvictRequest  = { target.decode.pack.poke(msg); target.decode.msgPageEvictReq.peek() }
    def decodeMsgMissReply(msg: UInt) : QEMUMissReply  = { target.decode.pack.poke(msg); target.decode.msgMissReply.peek() }
    def decodeMsgEvictReply(msg: UInt) : QEMUEvictReply  = { target.decode.pack.poke(msg); target.decode.msgEvictReply.peek() }
    def decodeMsgPageEvictNotifStart(msg: UInt) : PageEvictNotification  = { target.decode.pack.poke(msg); target.decode.msgPageEvictNotif.peek() }
    def decodeMsgPageEvictNotifDone(msg: UInt) : PageEvictNotification  = { target.decode.pack.poke(msg); target.decode.msgPageEvictDone.peek() }
    def decodeMsgPageFaultNotif(msg: UInt) : PageFaultNotification  = { target.decode.pack.poke(msg); target.decode.msgPageFaultNotif.peek() }
  }
}

object MMUBundleDrivers {
  object CacheFlushRequest {
    def apply(addr: UInt)(implicit params: CacheParams) = 
      new CacheFlushRequest(params).Lit(_.addr -> addr)
  }
  object TLBPipelineResp {
    def apply(entry : PTEntryPacket, hit : Bool, violation : Bool, thid: UInt)(implicit params: PageTableParams) = {
      new TLBPipelineResp(params).Lit(_.entry -> PTEntryPacket(entry.ppn, entry.perm, entry.modified), _.hit -> hit, _.violation -> violation, _.thid -> thid)
    }
  }

  object TLBMissRequestMessage {
    def apply(thid: UInt, asid: UInt, vpn: UInt, perm: UInt)(implicit params: PageTableParams): TLBMissRequestMessage = 
      new TLBMissRequestMessage(params).Lit(_.perm -> perm, _.tag.asid -> asid, _.tag.vpn -> vpn, _.thid -> thid)
  }

  object TLBMMURespPacket {
    def apply(thid: UInt, asid: UInt, vpn: UInt, ppn: UInt, perm: UInt, modified: Bool)(implicit params: PageTableParams): TLBMMURespPacket =
      new TLBMMURespPacket(params).Lit(
        _.tag.asid -> asid, _.tag.vpn -> vpn,
        _.data.modified -> modified, _.data.perm -> perm, _.data.ppn -> ppn,
        _.thid -> thid
      )
  }
  object PTTagPacket {
    def apply(implicit params: PageTableParams): PTTagPacket =  new PTTagPacket(params).Lit(_.vpn -> 0.U,_.asid -> 0.U)
    def apply(vpn: UInt, asid: UInt)(implicit params: PageTableParams): PTTagPacket =  new PTTagPacket(params).Lit(_.vpn -> vpn, _.asid -> asid)
  }

  object PTEntryPacket {
    def apply(implicit params: PageTableParams): PTEntryPacket = new PTEntryPacket(params).Lit(_.ppn -> 0.U, _.perm -> 0.U, _.modified -> false.B)
    def apply(ppn: UInt, perm: UInt, modified: Bool)(implicit params: PageTableParams): PTEntryPacket = new PTEntryPacket(params).Lit(_.ppn -> ppn, _.perm -> perm, _.modified -> modified)
  }

  object QEMUPageEvictRequest {
    def apply(tag: PTTagPacket)(implicit params: PageTableParams): QEMUPageEvictRequest = 
      new QEMUPageEvictRequest(params).Lit(_.tag -> PTTagPacket(tag.vpn, tag.asid))
  }

  object QEMUMissReply {
    def apply(tag: PTTagPacket, perm: UInt, thid: UInt, ppn: UInt)(implicit params: PageTableParams): QEMUMissReply = 
      new QEMUMissReply(params).Lit( _.tag -> PTTagPacket(tag.vpn, tag.asid), _.perm -> perm, _.thid -> thid, _.thid_v -> (thid.litValue != BigInt("FFFFFFFF", 16)).B, _.ppn -> ppn)
  }

  object QEMUEvictReply { 
    def apply(tag: PTTagPacket, old_ppn: UInt)(implicit params: PageTableParams): QEMUEvictReply = 
      new QEMUEvictReply(params).Lit(_.tag -> PTTagPacket(tag.vpn, tag.asid), _.old_ppn -> old_ppn)
  }

  object PageEvictNotif {
    def apply(item: PageTableItem)(implicit params: PageTableParams): PageEvictNotification = 
      new PageEvictNotification(QEMUMessagesType.sEvictNotify, params).Lit(_.item -> PageTableItem(item.tag, item.entry))
  }

  object PageEvictNotifDone {
    def apply(item: PageTableItem)(implicit params: PageTableParams): PageEvictNotification = 
      new PageEvictNotification(QEMUMessagesType.sEvictDone, params).Lit(_.item -> PageTableItem(item.tag, item.entry))
  }

  object PageFaultNotification {
    def apply(tag: PTTagPacket, perm: UInt, thid: UInt)(implicit params: PageTableParams): PageFaultNotification = 
      new PageFaultNotification(params).Lit(_.tag -> PTTagPacket(tag.vpn, tag.asid), _.perm -> perm, _.thid -> thid)
  }

  object PageTableItem {
    def apply(tag: PTTagPacket, entry: PTEntryPacket)(implicit params: PageTableParams): PageTableItem =
      new PageTableItem(params).Lit(_.tag -> PTTagPacket(tag.vpn, tag.asid), _.entry -> PTEntryPacket(entry.ppn, entry.perm, entry.modified))
    def apply(implicit params: PageTableParams): PageTableItem = 
      new PageTableItem(params).Lit(_.tag -> PTTagPacket(params), _.entry -> PTEntryPacket(params))
  }
 
  object PageTableSetPacket {
    def apply(validVec: BigInt, packet: PageTableSetPacket)(implicit params: PageTableParams): PageTableSetPacket = {
      new PageTableSetPacket(params).Lit(
        _.entries -> TestUtils.vecLitMake(params.ptAssociativity, packet.entries.zipWithIndex.map {
          case (set, idx) => (idx,
          () => PageTableItem(set.tag, set.entry)(params))},
          () => PageTableItem(params)), 
        _.lru_bits -> packet.lru_bits, _.valids -> validVec.U)
    }

    def apply(entries: Seq[(Int, PageTableItem)], lru: UInt)(implicit params: PageTableParams): PageTableSetPacket = {
      val valids =  entries.map(_._1).foldLeft(0)((bitvec, idx) => bitvec | 1 << idx)
      new PageTableSetPacket(params).Lit(
        _.entries -> TestUtils.vecLitMake(params.ptAssociativity, 
          entries.map { case (idx, entry) => (idx, 
            () => PageTableItem(PTTagPacket(entry.tag.vpn, entry.tag.asid), PTEntryPacket(entry.entry.ppn, entry.entry.perm, entry.entry.modified)))
            }, () => PageTableItem(params)),
        _.lru_bits -> lru, _.valids -> valids.U)
    }
 
    def apply(sets: Seq[(Int, PageTableItem)])(implicit params: PageTableParams): PageTableSetPacket = apply(sets, 0.U)
    def apply(port: Int, set: PageTableItem)(implicit params: PageTableParams): PageTableSetPacket = apply(Seq((port, set)), 0.U)
    def makeEmptySet(implicit params: PageTableParams): Seq[(Int, PageTableItem)] = 
      for (entryIdx <- 0 until params.ptAssociativity) yield (entryIdx, PageTableItem(PTTagPacket(params), PTEntryPacket(params)))
  }
}

/**
 * This is just a simple wrapper of the module PageDemander. 
 * It also contains some utility functions that simplify the verification process.
 *
 * @params params Parameter of the memory system.
 *
 */
class MMUDUT(
  val params: MemoryHierarchyParams
) extends Module {
  val uMMU = Module(new MMU(params, 2))
  implicit val ptParams = params.getPageTableParams
  implicit val cacheParams = params.getCacheParams

  // Load Store Unit: Page deleting acknowledgement
  uMMU.mmu_pipe_io.data.flushPermReq.ready := true.B
  uMMU.mmu_pipe_io.inst.flushPermReq.ready := true.B
  uMMU.mmu_pipe_io.data.flushCompled.ready := true.B
  uMMU.mmu_pipe_io.inst.flushCompled.ready := true.B

  // TLB Requests
  val mmu_tlb_io = IO(uMMU.mmu_tlb_io.cloneType)
  val mmu_cache_io = IO(uMMU.mmu_cache_io.cloneType)
  uMMU.mmu_tlb_io <> mmu_tlb_io
  uMMU.mmu_cache_io <> mmu_cache_io
 
  // AXI slave of the page buffer
  val S_AXI = IO(Flipped(uMMU.S_AXI.cloneType))
  val M_AXI = IO(new AXI4(params.dramAddrW, params.dramdataW))
  S_AXI <> uMMU.S_AXI


  // val uAXIL2CSR = Module(new AXI4LiteCSR(32, 4))
  // val S_AXIL = IO(Flipped(uAXIL2CSR.io.ctl.cloneType))
  // uAXIL2CSR.io.bus <> uMMU.S_CSR
  // S_AXIL <> uAXIL2CSR.io.ctl
  val S_CSR = IO(Flipped(uMMU.S_CSR.cloneType))
  S_CSR <> uMMU.S_CSR

  // Manage M_AXI ports
  val rdPorts = 4
  val wrPorts = 3
  val axiMulti_R = Module(new AXIReadMultiplexer(params.dramAddrW, params.dramdataW, rdPorts))
  val axiMulti_W = Module(new AXIWriteMultiplexer(params.dramAddrW, params.dramdataW, wrPorts))
  for(i <- 0 until 4) axiMulti_R.S_IF(i) <> uMMU.axiShell_io.M_DMA_R(i)
  for(i <- 0 until 3) axiMulti_W.S_IF(i) <> uMMU.axiShell_io.M_DMA_W(i)

  // Tie Read port
  M_AXI.ar <> axiMulti_R.M_AXI.ar
  M_AXI.r <> axiMulti_R.M_AXI.r

  // Tie Write port
  M_AXI.aw <> axiMulti_W.M_AXI.aw
  M_AXI.w <> axiMulti_W.M_AXI.w
  M_AXI.b <> axiMulti_W.M_AXI.b

  // Tie off Write Port
  axiMulti_R.M_AXI.aw <> AXI4AW.stub(params.dramAddrW)
  axiMulti_R.M_AXI.w <> AXI4W.stub(params.dramdataW)
  axiMulti_R.M_AXI.b <> AXI4B.stub()
  // Tie off Read Port 
  axiMulti_W.M_AXI.ar <> AXI4AR.stub(params.dramAddrW)
  axiMulti_W.M_AXI.r <> AXI4R.stub(params.dramdataW)
  
  // Helper: decode and encode messages from a raw UInt
  val uHelperEncodeDecodePageSet = Module(new MMUHelpers.PageSetConverter)
  val encode = IO(uHelperEncodeDecodePageSet.encode.cloneType)
  val decode = IO(uHelperEncodeDecodePageSet.decode.cloneType)
  encode <> uHelperEncodeDecodePageSet.encode
  decode <> uHelperEncodeDecodePageSet.decode

  val uHelperEncodeRawMessage = Module(new MMUHelpers.RawMessageHelper()(params.getPageTableParams))
  val rawMessageHelper = IO(uHelperEncodeRawMessage.io.cloneType)
  rawMessageHelper <> uHelperEncodeRawMessage.io
}


object MMUDriver {

  implicit class MMUDriver(target: MMUDUT){
    implicit val clock: Clock = target.clock
    implicit val ptparams = target.params.getPageTableParams
    implicit val cacheparams = target.params.getCacheParams

    def init() = {
      target.M_AXI.initMaster()
      target.S_AXI.initSlave()
      target.S_CSR.init()
      target.mmu_tlb_io.inst.missReq.initSource()
      target.mmu_tlb_io.inst.missReq.setSourceClock(clock)
      target.mmu_tlb_io.data.missReq.initSource()
      target.mmu_tlb_io.data.missReq.setSourceClock(clock)

      target.mmu_tlb_io.inst.refillResp.initSink()
      target.mmu_tlb_io.inst.refillResp.setSinkClock(clock)
      target.mmu_tlb_io.data.refillResp.initSink()
      target.mmu_tlb_io.data.refillResp.setSinkClock(clock)

      target.mmu_tlb_io.inst.flushReq.initSink()
      target.mmu_tlb_io.data.flushReq.initSink()
      target.mmu_tlb_io.inst.flushReq.setSinkClock(clock)
      target.mmu_tlb_io.data.flushReq.setSinkClock(clock)
      target.mmu_tlb_io.inst.flushResp.initSource()
      target.mmu_tlb_io.data.flushResp.initSource()
      target.mmu_tlb_io.inst.flushResp.setSourceClock(clock)
      target.mmu_tlb_io.data.flushResp.setSourceClock(clock)

      target.mmu_cache_io.data.flushReq.initSink()
      target.mmu_cache_io.inst.flushReq.initSink()
      target.mmu_cache_io.data.flushReq.setSinkClock(clock)
      target.mmu_cache_io.inst.flushReq.setSinkClock(clock)

      target.mmu_tlb_io.inst.writebackReq.initSource()
      target.mmu_tlb_io.inst.writebackReq.setSourceClock(clock)
      target.mmu_tlb_io.data.writebackReq.initSource()
      target.mmu_tlb_io.data.writebackReq.setSourceClock(clock)

      target.mmu_cache_io.inst.wbEmpty.poke(true.B) 
      target.mmu_cache_io.data.wbEmpty.poke(true.B) 
    }

    /**
     * Wait the handshake port (boolean type) equal to the value.
     *
     * @params port the source port
     * @params value the target value, usually and default true
     *
     * @return the number of cycles (interval) it waits for.
     *
     */
    def waitForSignalToBe(port: Bool, value: Boolean = true): Int = {
      println(s"wait ${port.pathName} to be $value.")
      var interval = 0
      while(port.peek().litToBoolean != value){
        clock.step()
        interval += 1
      }
      return interval
    }

    def vpn2ptSetPA(asid: BigInt, vpn: BigInt) = {
      val PTEsPerLine = target.params.pAddrW - log2Ceil(target.params.pageSize) - log2Ceil(16);
      val mask = (BigInt(1) << PTEsPerLine) - 1
      // Waring: Sync this function with MemoryHierarchyParams.vpn2ptSetPA
      (((((vpn >> 6) << target.params.asidW) | asid) & mask) * 3) << 6
    }

    def sendQEMUMessage(message_type: BigInt, rawMessage: Seq[BigInt]) = {}
    def sendPageFaultResponse(vpn: BigInt, thid: Int, asid: Int, perm: Int, ppn: Int) = {}
    def expectQEMUMessage(message_type: BigInt, rawMessage: Seq[BigInt]) = {}
    def receivePageTableSet(master_bus: AXI4, expectedAddr: UInt) = {}
    def sendPageTableSet(master_bus: AXI4, expectedAddr: UInt) = {}
    
    def waitTillPendingMMUMsg() = { var timeout = 0; while(target.S_CSR.readReg(0) == 0 && timeout < 10) { clock.step(5); timeout += 1} }
    def waitTillFreeMMUMsg() = { var timeout = 0; while(target.S_CSR.readReg(1) == 0 && timeout < 10) { clock.step(5); timeout += 1} }
    def getMMUMsg(): UInt = { 
      val ret = target.S_AXI.rd(0.U, 1); 
      assert(ret.size == 1); 
      ret(0) 
    }
    def sendMMUMsg(msg: UInt) = {
      assert(target.S_CSR.readReg(1) != 0)
      target.S_AXI.wr((512/8).U, Seq(msg))
    }
 
    // -------------- Page Table (M_AXI) -----------------
    def encodePageTableSet(set: PageTableSetPacket): Vec[UInt] = {
      target.encode.packet.poke(set)
      target.encode.vector.peek()
    }

    def encodePageTableSet(port: Int, set: PageTableItem, lru: UInt): Vec[UInt] = {
      target.encode.packet.poke(MMUBundleDrivers.PageTableSetPacket(Seq((port, set)), lru))
      target.encode.vector.peek()
    }

    def encodePageTableSet(port: Int, set: PageTableItem): Vec[UInt] = {
      val pageTableSet = MMUBundleDrivers.PageTableSetPacket(Seq((port, set)), 0.U)
      target.encode.packet.poke(pageTableSet)
      target.encode.vector.peek()
    }

    def encodePageTableSet(validVec: BigInt, pageTableSet: PageTableSetPacket) = {
      target.encode.packet.poke(MMUBundleDrivers.PageTableSetPacket(validVec, pageTableSet))
      target.encode.vector.peek()
    }

    def encodePageTableSet(sets: Seq[(Int, PageTableItem)]): Vec[UInt] = encodePageTableSet(sets, 0.U)
    def encodePageTableSet(sets: Seq[(Int, PageTableItem)], lru: UInt): Vec[UInt] = {
      target.encode.packet.poke(MMUBundleDrivers.PageTableSetPacket(sets, lru))
      target.encode.vector.peek()
    }

    def decodePageTableSet(vector: Vec[UInt]): PageTableSetPacket = {
      target.decode.vector.poke(vector)
      target.decode.packet.peek()
    }

    def sendMissReq(accessType: Int, thid: UInt, asid: UInt, vpn: UInt, perm: UInt) = {
      if(accessType == 2) 
        target.mmu_tlb_io.inst.missReq.enqueue(TLBMissRequestMessage(thid, asid, vpn, perm))
      else 
        target.mmu_tlb_io.data.missReq.enqueue(TLBMissRequestMessage(thid, asid, vpn, perm))
    }

    
    def expectMissResp(accessType: Int, thid: UInt, set: PageTableItem) = {
      if(accessType == MemoryAccessType.INST_FETCH) {
        target.mmu_tlb_io.inst.refillResp.expectDequeue(TLBMMURespPacket(thid, set.tag.asid, set.tag.vpn, set.entry.ppn, set.entry.perm, set.entry.modified))
      } else  {
        target.mmu_tlb_io.data.refillResp.expectDequeue(TLBMMURespPacket(thid, set.tag.asid, set.tag.vpn, set.entry.ppn, set.entry.perm, set.entry.modified))
      }
    }

    def expectFlushReqCache(accessType: Int, thid: UInt, set: PageTableItem) = {
      val cacheBlockBytes = target.params.cacheBlockSize/8
      val ppageAddr = set.entry.ppn.litValue << log2Ceil(target.params.pageSize)
      if(accessType == MemoryAccessType.INST_FETCH) {
        for (block <- 0 until target.params.pageSize/cacheBlockBytes) {
          val addr = ppageAddr + (block << log2Ceil(cacheBlockBytes))
          target.mmu_cache_io.inst.flushReq.expectDequeue(CacheFlushRequest(addr.U))
        }
      } else {
        target.mmu_cache_io.data.wbEmpty.poke(false.B) 
        for (block <- 0 until target.params.pageSize/cacheBlockBytes) {
          val addr = ppageAddr + (block << log2Ceil(cacheBlockBytes))
          target.mmu_cache_io.data.flushReq.expectDequeue(CacheFlushRequest(addr.U))
        }
        clock.step(4)
        target.mmu_cache_io.data.wbEmpty.poke(true.B) 
      }
      clock.step()
    }

    def expectFlushReqTLB(accessType: Int, thid: UInt, set: PageTableItem) = {
      fork {
        target.mmu_tlb_io.inst.flushReq.expectDequeue(set.tag)
        target.mmu_tlb_io.inst.flushResp.enqueueNow(TLBPipelineResp(set.entry, (accessType == MemoryAccessType.INST_FETCH).B, false.B, thid))
      }.fork {
        target.mmu_tlb_io.data.flushReq.expectDequeue(set.tag)
        target.mmu_tlb_io.data.flushResp.enqueueNow(TLBPipelineResp(set.entry, (accessType != MemoryAccessType.INST_FETCH).B, false.B, thid))
      }.join()
    }

    def vpn2ptSetPA(tag: PTTagPacket): BigInt = vpn2ptSetPA(tag.asid.litValue, tag.vpn.litValue)

    def expectWrPageTableSetPacket(packet: PageTableSetPacket, expectAddr: UInt): Unit = {
      target.encode.packet.poke(packet)
      val vectorPacket = target.encode.vector.peek()
      target.M_AXI.expectWr(vectorPacket, expectAddr)
    }

    def expectRdPageTablePacket(packet: PageTableSetPacket, expectAddr: UInt) = {
      target.encode.packet.poke(packet)
      val vectorPacket = target.encode.vector.peek()
      target.M_AXI.expectRd(vectorPacket, expectAddr)
    }

    def respEmptyPageTableSet(asid: UInt, vpn: UInt) = {
      val expectAddr = vpn2ptSetPA(asid.litValue, vpn.litValue)
      val vectorPacket = encodePageTableSet(Nil)
      target.M_AXI.expectRd(vectorPacket, expectAddr.U)
    }

    def respFullPageTableSet(asid: UInt, vpn: UInt) = {
      val expectAddr = vpn2ptSetPA(asid.litValue, vpn.litValue)
      val entries = for (entryIdx <- 0 until ptparams.tlbSetNumber) yield (entryIdx -> PageTableItem(PTTagPacket(ptparams), PTEntryPacket(ptparams)))
      val vectorPacket = encodePageTableSet(entries)
      target.M_AXI.expectRd(vectorPacket, expectAddr.U)
    }

  }
}

