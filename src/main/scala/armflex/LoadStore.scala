// See LICENSE.txt for license details.
package armflex

import chisel3._
import chisel3.util._
import arm.PROCESSOR_TYPES._
import arm.DECODE_CONTROL_SIGNALS._
import armflex.util.CreditQueueController

class MemReq extends Bundle {
  val addr = DATA_T
  val data = DATA_T
  val reg = REG_T
}

class MInst extends Bundle {

  val size = Output(UInt(2.W))
  val isPair = Output(Bool())
  val isLoad = Output(Bool())
  val is32bit = Output(Bool())
  val isSigned = Output(Bool())
  val req = Output(Vec(2, new MemReq))

  // With Write Back
  val rd_res = Output(DATA_T)
  val rd = Valid(REG_T)
  val exceptions = Valid(new Bundle {
    val unalignedExcp = Bool()
    val unalignedExcpSP = Bool()
  })
}

class MInstTag[T <: UInt](gen: T) extends MInst {
  val tag = Output(gen)

  def :=(target: MInstTag[T]) = {
    this.size     := target.size
    this.isPair   := target.isPair
    this.isLoad   := target.isLoad
    this.is32bit  := target.is32bit
    this.isSigned := target.isSigned
    this.req      := target.req
    this.rd_res   := target.rd_res
    this.rd       := target.rd
    this.exceptions := target.exceptions
    this.tag      := target.tag
  }
  def :=(target: MInst) = {
    this.size     := target.size
    this.isPair   := target.isPair
    this.isLoad   := target.isLoad
    this.is32bit  := target.is32bit
    this.isSigned := target.isSigned
    this.req      := target.req
    this.rd_res   := target.rd_res
    this.rd       := target.rd
    this.exceptions := target.exceptions
    this.tag := DontCare
  }
  override def cloneType: this.type = new MInstTag(gen).asInstanceOf[this.type]
}

class LDSTUnitIO extends Bundle {
  val dinst = Input(new DInst)
  val rVal1 = Input(DATA_T) // Rn
  val rVal2 = Input(DATA_T) // Rt in some cases
  val rVal3 = Input(DATA_T) // Rt in some cases
  val pstate = Input(new PStateRegs)

  val minst = Output(Valid(new MInst))
}

class ExtendReg extends Module {
  val io = IO(new Bundle {
    val value = Input(DATA_T)
    val option = Input(UInt(3.W))
    val shift = Input(UInt(2.W))
    val res = Output(DATA_T)
  })
  val value = io.value
  val isSigned = io.option(2)
  val size = io.option(1, 0) // NOTE: option(1) == 1
  val valSExt = WireInit(
    MuxLookup(
      size,
      value.asSInt,
      Array(
        0.U -> value(7, 0).asSInt.pad(64),
        1.U -> value(15, 0).asSInt.pad(64),
        2.U -> value(31, 0).asSInt.pad(64),
        3.U -> value.asSInt
      )
    )
  )
  val valUExt = WireInit(
    MuxLookup(
      size,
      value,
      Array(
        0.U -> value(7, 0).pad(64),
        1.U -> value(15, 0).pad(64),
        2.U -> value(31, 0).pad(64),
        3.U -> value
      )
    )
  )
  val res = WireInit(Mux(isSigned, valSExt.asUInt, valUExt))
  io.res := res << io.shift
}

class LDSTUnit extends Module {

  val io = IO(new LDSTUnitIO)

  // WZR, XZR, and SP
  val rVal1 = WireInit(Mux(io.dinst.rs1 === 31.U, 0.U, io.rVal1))
  val rVal2 = WireInit(Mux(io.dinst.rs2 === 31.U, 0.U, io.rVal2))
  val rVal3 = WireInit(Mux(io.dinst.imm(4,0) === 31.U, 0.U, io.rVal3))

  // Decode All variants
  val size = WireInit(io.dinst.op(1, 0))
  val isLoad = WireInit(io.dinst.op(2))
  val isSigned = WireInit(io.dinst.op(3))
  val offstSignExt7 = io.dinst.imm(6, 0).asSInt.pad(64).asUInt
  val offstSignExt9 = io.dinst.imm(8, 0).asSInt.pad(64).asUInt
  val offst = WireInit(DATA_T, io.dinst.imm)

  // Decode for LD/ST with Post/Pre-index and Signed offset variants
  val wback = WireInit(false.B)
  val postindex = WireInit(false.B)

  // Decode LSReg variants
  val option = io.dinst.shift_val.bits
  val shift = Mux(io.dinst.shift_val.valid, size, 0.U)
  val extendReg = Module(new ExtendReg)
  extendReg.io.value := rVal2
  extendReg.io.option := option
  extendReg.io.shift := shift

  private val isPair = WireInit(
    io.dinst.itype === I_LSPairPr ||
    io.dinst.itype === I_LSPair   ||
    io.dinst.itype === I_LSPairPo)
 
  when(io.dinst.itype === I_LSUImm) {
    wback := false.B
    postindex := false.B
    offst := io.dinst.imm(11, 0) << size
  }.elsewhen(io.dinst.itype === I_LSUReg) {
    wback := false.B
    postindex := false.B
    offst := offstSignExt9
  }.elsewhen(io.dinst.itype === I_LSPairPo) {
    wback := true.B
    postindex := true.B
    offst := offstSignExt7 << size
  }.elsewhen(io.dinst.itype === I_LSPair) {
    wback := false.B
    postindex := false.B
    offst := offstSignExt7 << size
  }.elsewhen(io.dinst.itype === I_LSPairPr) {
    wback := true.B
    postindex := false.B
    offst := offstSignExt7 << size
  }.elsewhen(io.dinst.itype === I_LSRegPo) {
    wback := true.B
    postindex := true.B
    offst := offstSignExt9
  }.elsewhen(io.dinst.itype === I_LSReg) {
    wback := false.B
    postindex := false.B
    offst := extendReg.io.res
  }.elsewhen(io.dinst.itype === I_LSRegPr) {
    wback := true.B
    postindex := false.B
    offst := offstSignExt9
  }
  //  if n == 31 then
  //     address = SP[]; // SP[] == X[31]
  //  else
  //     address = X[n];
  //
  when(io.dinst.rs1 === 31.U) {
    // CheckSPAAligment(); Checked in MemoryArbiter - DataAligner
    rVal1 := io.rVal1
  }
  val base_address = WireInit(rVal1)

  //  if !postindex then
  //     address = address + offset
  // ==> implicitly
  //  if postindex then
  //     address = address
  //  else
  //     address = address + offset
  val ldst_address = WireInit(base_address + offst)
  when(postindex) {
    ldst_address := base_address
  }

  // integer datasize = 8 << size
  // bits(datasize) data

  // For WRITES
  // if rt_unknown
  //   data = bits(datasize) UNKNOWN
  // else
  //   data = X[t]
  //
  // Mem[address, datasize DIV 8, AccType_NORMAL] = data

  // LD's
  // data = Mem[address, datasize DIV 8, AccType_NORMAL]
  // if signed then
  //   X[t] = SignExtend(data, regsize)
  // else
  //   X[t] = ZeroExtend(data, regsize)

  // ST's
  // data = X[t]
  // Mem[address, datasize DIV 8, AccType_NORMAL] = data

  val data_1 = WireInit(rVal2) // data_1 = Rt = rVal2
  val data_2 = WireInit(rVal3) // data_2 = Rt2 = rVal3

  // Prepare MInst
  val minst = Wire(new MInst)
  minst.size := size
  minst.isSigned := isSigned
  minst.is32bit := size =/= SIZE64 && !(io.dinst.itype === I_LSUImm && io.dinst.op === OP_LDRSW)
  minst.isLoad := isLoad

  minst.req(0).addr := ldst_address
  minst.req(0).data := data_1
  minst.req(0).reg := io.dinst.rs2
  // For Pair LD/ST
  val dbytes = 1.U << size
  minst.isPair := isPair
  minst.req(1).addr := ldst_address + dbytes
  minst.req(1).data := data_1
  minst.req(1).reg := io.dinst.rs2

  when(isPair) {
    // Pair stores changes destination
    minst.req(0).data := data_2
    minst.req(0).reg := io.dinst.rd.bits
  }


  // if wback then
  //   if wb_unknown then
  //     address = bits(64) UNKNOWN;
  //   elsif postindex then
  //     address = address + offset; => base_address + offset
  //   if n == 31 then
  //     SP[] = address;
  //   else
  //     X[n] = address;
  val wback_addr = WireInit(base_address + offst)
  minst.rd_res := wback_addr
  minst.rd.bits := io.dinst.rs1
  minst.rd.valid := wback

  // Check for Exceptions
  // NOTE: Read Manual section B2.5 for alignment support -> Possible performance improvements
  val isAlignedMem_0 = WireInit(MuxLookup(minst.size, false.B, Array(
    SIZEB -> true.B,
    SIZEH -> (minst.req(0).addr(0) === 0.U),
    SIZE32 -> (minst.req(0).addr(1, 0) === 0.U),
    SIZE64 -> (minst.req(0).addr(2, 0) === 0.U)
    )))

  val isAlignedMem_1 = WireInit(MuxLookup(minst.size, false.B, Array(
    SIZEB -> true.B,
    SIZEH -> (minst.req(1).addr(0) === 0.U),
    SIZE32 -> (minst.req(1).addr(1, 0) === 0.U),
    SIZE64 -> (minst.req(1).addr(2, 0) === 0.U)
    )))

  minst.exceptions.bits.unalignedExcpSP := io.dinst.rs1 === 31.U && wback_addr(1, 0) =/= 0.U
  minst.exceptions.bits.unalignedExcp := !isAlignedMem_0 || (minst.isPair && !isAlignedMem_1)
  minst.exceptions.valid := minst.exceptions.bits.asUInt.orR

  // Output
  io.minst.bits := minst
  io.minst.valid := MuxLookup(io.dinst.itype, false.B, Array(
    I_LSPairPr -> true.B,
    I_LSPair -> true.B,
    I_LSPairPo -> true.B,
    I_LSUReg -> true.B,
    I_LSRegPr -> true.B,
    I_LSReg -> true.B,
    I_LSRegPo -> true.B,
    I_LSUImm -> true.B
    ))

  // TODO
  // boolean tag_checked = memop != MemOp_PREFETCH && (n != 31); // itype == LSUReg
  // boolean tag_checked = wback || (n != 31);                   // itype == LSUImm
  val tag_checked = WireInit(false.B)

  // if HaveMTEExt() then
  //   SetNotTagCheckedInstruction(!tag_checked); // itype == LSUImm
  //   SetNotTagCheckedInstruction(!tag_checked); // itype == LSUReg
  //   SetNotTagCheckedInstruction(FALSE);        // itype == LSReg

  // NOTE itype == LSUImm
  // Checking for Transplant corner cases
  // if wback && n == t && n != 31 then
  //     c = ConstrainUnpredictable();
  //     assert c IN {Constraint_NONE, Constraint_UNKNOWN, Constraint_UNDEF, Constraint_NOP};
  //     case c of
  //         when Constraint_NONE rt_unknown = FALSE; // value stored is original value
  //         when Constraint_UNKNOWN rt_unknown = TRUE; // value stored is UNKNOWN => Transplant (?)
  //         when Constraint_UNDEF UNDEFINED;        => Transplant
  //         when Constraint_NOP EndOfInstruction(); => NOP

  // NOTE itype == LSPair
  // if t == t2 then
  //   Constraint c = ConstrainUnpredictable();
  //   assert c IN {Constraint_UNKNOWN, Constraint_UNDEF, Constraint_NOP};
  //   case c of
  //     when Constraint_UNKNOWN rt_unknown = TRUE; // result is UNKNOWN
  //     when Constraint_UNDEF UNDEFINED;
  //     when Constraint_NOP EndOfInstruction();
  val rt_unkown = WireInit(false.B)
}

import armflex.util.ExtraUtils._

class MemoryUnit(
  params: PipelineParams,
  tlbQ_entries: Int = 2,
  cacheReqQ_entries: Int = 3,
  cacheReqQMisalignement_entries: Int = 3,
  doneInsts_entries: Int = 4
) extends MultiIOModule {
  val pipe = IO(new Bundle {
    val req = Flipped(Decoupled(new MInstTag(params.thidT)))
    val resp = Decoupled(new CommitInst(params.thidN))
  })
  val mem_io = IO(new PipeMemPortIO(DATA_SZ, params.pAddrW, params.thidW, 0, params.blockSize))
  val mmu_io = IO(new PipeMMUPortIO)

  private class PendingCacheReq extends Bundle {
    val inst = new MInstTag(params.thidT)
    // For Pair LoadStores, to execute second address, while keeping first access information
    val blockMisaligned = Output(Bool())
    val firstIsCompleted = Output(Bool())
    val paddr = Output(Vec(2, mem_io.cache.req.bits.addr.cloneType))
  }

  private val cacheInstsFlight_entries = doneInsts_entries + cacheReqQMisalignement_entries
  private val tlbReqQ = Module(new Queue(new MInstTag(params.thidT), tlbQ_entries, true, false))
  private val tlbMeta = RegEnable(tlbReqQ.io.deq.bits, tlbReqQ.io.deq.fire)
  private val cacheReq = Wire(new PendingCacheReq) // Pack TLB response to Cache
  private val cacheAdaptor = Module(new PipeCache.CacheInterface(cacheReq.cloneType, cacheInstsFlight_entries, params.pAddrW, params.blockSize))
  private val cacheReqQ = Module(new Queue(new PendingCacheReq, cacheReqQ_entries, true, false))
  // This queue is here to send second transaction following a pair instruction block misaligned access
  private val cacheReqMisalignedQ = Module(new Queue(new PendingCacheReq, cacheReqQMisalignement_entries, true, false))
  private val doneInst = Module(new Queue(new CommitInst(params.thidN), doneInsts_entries, true, false))
  // Makes sure receiver has enough entries left
  private val tlb2cache_credits = Module(new CreditQueueController(cacheReqQ_entries))
  private val cache2cacheMisaligned_credits = Module(new CreditQueueController(cacheReqQMisalignement_entries))
  private val cache2doneInst_credits = Module(new CreditQueueController(doneInsts_entries))

  // -----------------------------------------
  // -------------- TLB   Access -------------
  // -----------------------------------------

  tlbReqQ.io.enq <> pipe.req

  // Manage instruction TLB translation of Pair Instruction
  private val sTLB_req :: sTLB_intermediateResp :: sTLB_pair_req :: Nil = Enum(3)
  private val sTLB_state = RegInit(sTLB_req)

  private def isPairPageMisaligned(minst: MInstTag[UInt]) = minst.isPair && // Is Pair
    minst.req(0).addr(12) =/= minst.req(1).addr(12) // Page is different

  private def isBlockMisaligned(addr1: UInt, addr2: UInt) = params.getBlockAddrBits(addr1) =/= params.getBlockAddrBits(addr2)

  private val tlbPair_paddr1 = RegEnable(mem_io.tlb.resp.bits.addr, mem_io.tlb.resp.fire)
  private val tlbDropInst = WireInit(false.B) // Drop instruction on miss

  // Handshakes
  // Producer needs to ensure receiver has enough entries
  mem_io.tlb.req.valid := false.B
  mem_io.tlb.resp.ready := false.B
  tlbReqQ.io.deq.ready := false.B
  cacheReqQ.io.enq.valid := false.B
  mem_io.tlb.req.handshake(tlbReqQ.io.deq, tlb2cache_credits.ready)
  // Receive only when not intermediate response
  cacheReqQ.io.enq.handshake(mem_io.tlb.resp, sTLB_state =/= sTLB_intermediateResp)

  // Backpressure Management
  tlb2cache_credits.trans.in := tlbReqQ.io.deq.fire
  tlb2cache_credits.trans.out := cacheReqQ.io.deq.fire
  tlb2cache_credits.trans.dropped := tlbDropInst

  // Data
  mem_io.tlb.req.bits.addr := Mux(sTLB_state === sTLB_pair_req,
                                  tlbReqQ.io.deq.bits.req(1).addr,
                                  tlbReqQ.io.deq.bits.req(0).addr)
  mem_io.tlb.req.bits.thid := tlbReqQ.io.deq.bits.tag
  mem_io.tlb.req.bits.perm := Mux(tlbReqQ.io.deq.bits.isLoad, DATA_LOAD.U, DATA_STORE.U)
  mem_io.tlb.req.bits.asid := DontCare // Defined higher in the hierarchy
  cacheReq.inst := tlbMeta
  cacheReq.paddr(0) := mem_io.tlb.resp.bits.addr | tlbReqQ.io.deq.bits.req(0).addr(11,0) // PAGE_SIZE
  cacheReq.paddr(1) := mem_io.tlb.resp.bits.addr | tlbReqQ.io.deq.bits.req(1).addr(11,0)
  cacheReq.firstIsCompleted := false.B
  cacheReqQ.io.enq.bits := cacheReq

  // Req TLB management -----
  // Notably Load Store Pair instructions
  // TODO: violation here ditches the instruction, we might want to push an exception
  //       to the commit instruction queue to trigger a transplanted later
  switch(sTLB_state) {
    is(sTLB_req) {
      sTLB_state := sTLB_req
      when(mem_io.tlb.req.fire) {
        when(isPairPageMisaligned(tlbReqQ.io.deq.bits)) {
          sTLB_state := sTLB_intermediateResp
          tlbReqQ.io.deq.ready := false.B // Wait for second translation if misaligned
        }
      }
    }
    is(sTLB_intermediateResp) {
      // Don't send second translation yet
      mem_io.tlb.req.valid := false.B
      tlbReqQ.io.deq.ready := false.B

      // If not hit, or violation on first address, ditch instruction
      when(mem_io.tlb.resp.fire) {
        sTLB_state := sTLB_pair_req
        when(!mem_io.tlb.resp.bits.hit || mem_io.tlb.resp.bits.violation) {
          // Ditch request, Memory Hierarchy will manage miss
          tlbReqQ.io.deq.ready := true.B
          tlbDropInst := true.B
          sTLB_state := sTLB_req
        }
      }
    }
    is(sTLB_pair_req) {
      when(mem_io.tlb.req.fire) {
        sTLB_state := sTLB_req
      }
    }
  }

  // Resp TLB management -----
  cacheReq.blockMisaligned := isBlockMisaligned(cacheReq.paddr(0), cacheReq.paddr(1))
  when(mem_io.tlb.resp.fire && sTLB_state =/= sTLB_intermediateResp) {
    cacheReq.paddr(0) := mem_io.tlb.resp.bits.addr                            | tlbReqQ.io.deq.bits.req(0).addr(11,0)
    cacheReq.paddr(1) := mem_io.tlb.resp.bits.addr + (1.U << tlbMeta.size)    | tlbReqQ.io.deq.bits.req(1).addr(11,0)
    when(isPairPageMisaligned(tlbMeta)) {
      cacheReq.paddr(0) := tlbPair_paddr1            | tlbReqQ.io.deq.bits.req(0).addr(11,0)
      cacheReq.paddr(1) := mem_io.tlb.resp.bits.addr | tlbReqQ.io.deq.bits.req(1).addr(11,0)
      cacheReq.blockMisaligned := true.B
    }

    when(!mem_io.tlb.resp.bits.hit || mem_io.tlb.resp.bits.violation) {
      cacheReqQ.io.enq.valid := false.B // Ditch instruction on miss/violation
      tlbDropInst := true.B
    }
  }

  // -----------------------------------------
  // -------------- Cache Access -------------
  // -----------------------------------------

  // Data -------
  private val cacheAdaptorMInstInput = WireInit(cacheAdaptor.pipe_io.req.meta)
  // singleAccessPair can perform the pair operation in a single cache access
  private val singleAccessPair = WireInit(cacheAdaptor.pipe_io.req.meta.inst.isPair && !cacheAdaptor.pipe_io.req.meta.blockMisaligned)
  // secondAccessPair implies the first access has completed already
  private val secondAccessPair = WireInit(cacheAdaptor.pipe_io.req.meta.inst.isPair && cacheAdaptor.pipe_io.req.meta.firstIsCompleted)

  private val cacheAdaptorReqData = WireInit(cacheAdaptor.pipe_io.req.port.bits.data.cloneType, 0.U)
  private val cacheAdaptorReqW_en = WireInit(cacheAdaptor.pipe_io.req.port.bits.w_en.cloneType, 0.U)

  // Align store data with byte and address for block
  private val maskSize = WireInit(cacheAdaptorMInstInput.inst.size +& singleAccessPair.asUInt)
  private val maskByteEn: UInt = MuxLookup(maskSize, 1.U, Array(
    // Mask for size = 2**(bytes in maskSize+1) - 1
    SIZEB -> ((1 << 1) - 1).U, // 0b1
    SIZEH -> ((1 << 2) - 1).U, // 0b11
    SIZE32 -> ((1 << 4) - 1).U, // 0b1111
    SIZE64 -> ((1 << 8) - 1).U, // 0b11111111
    SIZE128 -> ((1 << 16) - 1).U // 0b1111111111111111 // Only for pair 64 bit instructions
    ))

  // Select, align and mask bytes in block
  private val selReqIdx = secondAccessPair.asUInt // Select second one in case it is the second access
  private val maskEn_shift = WireInit(params.getBlockAddrBits(cacheAdaptorMInstInput.inst.req(selReqIdx).addr))
  when(cacheAdaptorMInstInput.inst.isLoad) {
    cacheAdaptorReqW_en := 0.U
  }.otherwise {
    cacheAdaptorReqW_en := maskByteEn << maskEn_shift
  }

 cacheAdaptorReqData := cacheAdaptorMInstInput.inst.req(selReqIdx).data << Cat(params.getBlockAddrBits(cacheAdaptorMInstInput.inst.req(selReqIdx).addr), 0.U(3.W))
  when(singleAccessPair) {
    cacheAdaptorReqData :=
      cacheAdaptorMInstInput.inst.req(1).data << Cat(params.getBlockAddrBits(cacheAdaptorMInstInput.inst.req(1).addr), 0.U(3.W)) |
        cacheAdaptorMInstInput.inst.req(0).data << Cat(params.getBlockAddrBits(cacheAdaptorMInstInput.inst.req(0).addr), 0.U(3.W))
  }

  cacheAdaptor.pipe_io.req.port.bits.data := cacheAdaptorReqData
  cacheAdaptor.pipe_io.req.port.bits.w_en := cacheAdaptorReqW_en
  cacheAdaptor.pipe_io.req.port.bits.addr := cacheReqQ.io.deq.bits.paddr(selReqIdx)
  cacheAdaptor.pipe_io.req.meta := cacheReqQ.io.deq.bits
  when(cacheReqMisalignedQ.io.deq.valid) {
    cacheAdaptor.pipe_io.req.port.bits.addr := cacheReqMisalignedQ.io.deq.bits.paddr(selReqIdx)
    cacheAdaptor.pipe_io.req.meta := cacheReqMisalignedQ.io.deq.bits
  }

  // Manage Control signals ---------
  // Send and Receive Cache Requests to the Pipeline
  mem_io.cache <> cacheAdaptor.cache_io

  // Arbiter for Cache Adaptor, prioritise requests that need second run
  // Send Cache Block
  cacheReqQ.io.deq.ready := false.B
  cacheReqMisalignedQ.io.deq.ready := false.B
  cacheAdaptor.pipe_io.req.port.valid := false.B
  // Receive Cache Block
  cacheReqMisalignedQ.io.enq.valid := false.B
  doneInst.io.enq.valid := false.B

  // Credits for backpressure
  cache2doneInst_credits.trans.in := false.B
  cache2cacheMisaligned_credits.trans.in := false.B
  cache2doneInst_credits.trans.dropped := false.B
  cache2cacheMisaligned_credits.trans.dropped := false.B
  when(cacheReqQ.io.deq.fire) {
    when(cacheReqQ.io.deq.bits.blockMisaligned) {
      cache2cacheMisaligned_credits.trans.in := true.B
    }.otherwise {
      cache2doneInst_credits.trans.in := true.B
    }
  }.elsewhen(cacheReqMisalignedQ.io.deq.fire) {
    cache2doneInst_credits.trans.in := true.B
  }
  cache2cacheMisaligned_credits.trans.out := cacheReqMisalignedQ.io.deq.fire
  cache2doneInst_credits.trans.out := doneInst.io.deq.fire

  // Manage requests to the cache, if pair pending, it has priority for execution
  when(cacheAdaptor.pipe_io.req.port.ready) {
    when(cacheReqMisalignedQ.io.deq.valid && cache2doneInst_credits.ready) {
      cacheReqMisalignedQ.io.deq.handshake(cacheAdaptor.pipe_io.req.port)
    }.otherwise {
      when(cacheReqQ.io.deq.bits.blockMisaligned) {
        cacheReqQ.io.deq.handshake(cacheAdaptor.pipe_io.req.port, cache2cacheMisaligned_credits.ready)
      }.otherwise {
        cacheReqQ.io.deq.handshake(cacheAdaptor.pipe_io.req.port, cache2doneInst_credits.ready)
      }
    }
  }

  // Manage responses from the cache, if pair not done, push it to be re-executed
  private val cachePipeResp = WireInit(cacheAdaptor.pipe_io.resp)
  private val respPairIdx = cachePipeResp.meta.firstIsCompleted.asUInt
  private val alignedDataResp: UInt = WireInit(cachePipeResp.port.bits.data >> Cat(cachePipeResp.meta.inst.req(respPairIdx).addr(log2Ceil(params.blockSize / 8), 0), 0.U(3.W)))
  // If blockMisaligned, save initial load in req data
  // when(cachePipeResp.meta.inst.isLoad) { } // Not necessary, overwrite store data given already executed
  cacheAdaptor.pipe_io.resp.port.ready := true.B // Backpressure is managed by ensuring enough
  cacheReqMisalignedQ.io.enq.bits := cachePipeResp.meta
  cacheReqMisalignedQ.io.enq.bits.inst.req(0).data := alignedDataResp
  when(cachePipeResp.meta.inst.isPair) {
    when(cachePipeResp.meta.blockMisaligned
           && !cachePipeResp.meta.firstIsCompleted) {
      cacheReqMisalignedQ.io.enq.bits.firstIsCompleted := true.B
      cacheReqMisalignedQ.io.enq.valid := cacheAdaptor.pipe_io.resp.port.valid
    }.otherwise {
      doneInst.io.enq.valid := cacheAdaptor.pipe_io.resp.port.valid
    }
  }.otherwise {
    doneInst.io.enq.valid := cacheAdaptor.pipe_io.resp.port.valid
  }

  // Pack final response
  val metaInfo = cachePipeResp.meta.inst
  val memData = Seq.fill(2)(WireInit(DATA_X))
  when(metaInfo.isPair) {
    when(cachePipeResp.meta.blockMisaligned) {
      // Pair instruction in two blocks, fetch intermediate result saved previously
      memData(0) := cachePipeResp.meta.inst.req(0).data
      memData(1) := alignedDataResp
    }.otherwise {
      // Pair instruction in same block
      memData(0) := alignedDataResp
      memData(1) := alignedDataResp >> Cat((1.U << metaInfo.size), 0.U(3.W))
    }
  }.otherwise {
    // Normal instruction
    memData(0) := alignedDataResp
    memData(1) := DontCare
  }

  def signExtendData(bits: UInt, size: UInt, sign: Bool): UInt = {
    val bitsSExt: SInt = MuxLookup(size, bits.asSInt, Array(
      SIZEB -> bits(7, 0).asSInt.pad(DATA_SZ),
      SIZEH -> bits(15, 0).asSInt.pad(DATA_SZ),
      SIZE32 -> bits(31, 0).asSInt.pad(DATA_SZ),
      SIZE64 -> bits.asSInt
      ))
    val bitsUExt: UInt = MuxLookup(size, bits, Array(
      SIZEB -> bits(7, 0).pad(DATA_SZ),
      SIZEH -> bits(15, 0).pad(DATA_SZ),
      SIZE32 -> bits(31, 0).pad(DATA_SZ),
      SIZE64 -> bits
      ))
    val res = WireInit(Mux(sign, bitsSExt.asUInt, bitsUExt))
    res
  }

  doneInst.io.enq.bits := 0.U.asTypeOf(doneInst.gen) // Init all false
  doneInst.io.enq.bits.rd(0).valid := metaInfo.rd.valid
  doneInst.io.enq.bits.rd(1).valid := metaInfo.isLoad
  doneInst.io.enq.bits.rd(2).valid := metaInfo.isLoad && metaInfo.isPair
  doneInst.io.enq.bits.res(0) := metaInfo.rd_res
  doneInst.io.enq.bits.res(1) := signExtendData(memData(0), metaInfo.size, metaInfo.isSigned)
  doneInst.io.enq.bits.res(2) := signExtendData(memData(1), metaInfo.size, metaInfo.isSigned)
  doneInst.io.enq.bits.rd(0).bits := metaInfo.rd.bits
  doneInst.io.enq.bits.rd(1).bits := metaInfo.req(0).reg
  doneInst.io.enq.bits.rd(2).bits := metaInfo.req(1).reg
  doneInst.io.enq.bits.is32bit := metaInfo.is32bit
  doneInst.io.enq.bits.tag := metaInfo.tag

  pipe.resp <> doneInst.io.deq

  // -----------------------------------------
  // -------------- Flush Request ------------
  // -----------------------------------------
  private val flushController = Module(new PipeCache.CacheFlushingController)
  private val haveCacheReq = WireInit(cacheReqQ.io.deq.valid || cacheReqMisalignedQ.io.deq.valid)
  private val havePendingCacheReq = WireInit(cacheAdaptor.pending =/= 0.U)
  when(flushController.ctrl.stopTransactions){
    tlbReqQ.io.deq.ready := false.B
    mem_io.tlb.req.valid := false.B
  }
  flushController.ctrl.hasPendingWork := haveCacheReq || havePendingCacheReq

  mmu_io <> flushController.mmu_io

  val location = "Pipeline:MemoryUnit"
  if(true) { // TODO Conditional asserts
    // --- TLB Stage ---
    when(mem_io.tlb.resp.fire && sTLB_state =/= sTLB_intermediateResp) {
      assert(RegNext(mem_io.tlb.req.fire), "Hit response of TLB should arrive in 1 cycle")
    }

    when(!tlb2cache_credits.ready) {
      assert(!tlbReqQ.io.deq.fire, "Can't fire requests if not enough credits left")
    }
    when(!cache2doneInst_credits.ready) {
      when(!cacheAdaptor.pipe_io.req.meta.blockMisaligned ||
             (cacheAdaptor.pipe_io.req.meta.blockMisaligned && cacheAdaptor.pipe_io.req.meta.firstIsCompleted)) {
        assert(!cacheAdaptor.pipe_io.req.port.fire, "Can't fire new request if instruction commit receiver doesn't have enough credits")
      }
    }
    when(!cache2cacheMisaligned_credits.ready) {
      when(cacheAdaptor.pipe_io.req.meta.blockMisaligned && !cacheAdaptor.pipe_io.req.meta.firstIsCompleted) {
        assert(!cacheAdaptor.pipe_io.req.port.fire, "Can't fire new request if misaligned qeue doesn't have enough credits")
      }
    }
    when(cacheReqQ.io.enq.valid) {
      assert(cacheReqQ.io.enq.ready, "Credit system should ensure that receiver has always enough entries left")
    }
    when(cacheReqMisalignedQ.io.enq.valid) {
      assert(cacheReqMisalignedQ.io.enq.ready, "Credit system should ensure that receiver has always enough entries left")
    }
    when(doneInst.io.enq.valid) {
      assert(doneInst.io.enq.ready, "Credit system should ensure that receiver has always enough entries left")
    }
    when(mem_io.cache.resp.valid) {
      when(mem_io.cache.resp.bits.hit) {
        assert(cacheAdaptor.pipe_io.resp.port.valid, "Cache Adaptor only acts as a module to forward meta data and has no latency")
      }.elsewhen(mem_io.cache.resp.bits.miss) {
        assert(!cacheAdaptor.pipe_io.resp.port.valid, "Cache Adaptor should not forward transaction on miss")
      }
      assert(doneInst.io.enq.ready || cacheReqMisalignedQ.io.enq.ready, "Credit system should ensure that receiver has always enough entries left")
    }
    when(mem_io.cache.req.valid) {
      assert(cacheAdaptor.pipe_io.req.port.valid, "Cache Adaptor only acts as a module to forward meta data and has no latency")
      assert(cacheReqMisalignedQ.io.deq.valid || cacheReqQ.io.deq.valid, "Cache requests must come from one of two queues")
    }
    when(cacheAdaptor.pipe_io.resp.port.fire) {
      when(cacheAdaptor.pipe_io.resp.meta.inst.isPair) {
        when(cacheAdaptor.pipe_io.resp.meta.blockMisaligned && !cacheAdaptor.pipe_io.resp.meta.firstIsCompleted) {
          assert(!doneInst.io.enq.fire, "On misaligned first pair access, instruction must not be pushed to completed insts")
          assert(cacheReqMisalignedQ.io.enq.fire, "On misaligned first pair access, instruction must be pushed to rerun queue")
          assert(cacheReqMisalignedQ.io.enq.bits.firstIsCompleted, "On misaligned first pair access, firstIsCompleted flag must be set")
        }.elsewhen(cacheAdaptor.pipe_io.resp.meta.blockMisaligned && cacheAdaptor.pipe_io.resp.meta.firstIsCompleted) {
          assert(!cacheReqMisalignedQ.io.enq.fire, "On misalgined second pair access, instruction must no be pushed to rerun queue")
          assert(doneInst.io.enq.fire, "On misaligned second pair access, instruction must be pushed to completed insts")
        }
      }.otherwise {
        assert(doneInst.io.enq.fire, "On normal access, instruction must be pushed to completed insts")
      }
    }

    // --- Flush assertions ---
    when(flushController.ctrl.stopTransactions) {
      assert(!mem_io.tlb.req.fire, "A new translation should never be sent once starting to fill flushing permissions")
    }
    when(flushController.ctrl.waitingForMMU) {
      assert(!haveCacheReq && !havePendingCacheReq, "No new cache requests should ever appear given that we stopped translating")
    }
  }
  if(false) { // TODO Conditional printing
    when(mem_io.tlb.req.fire) {
      printf(p"${location}:iTLB:Req:thid[${mem_io.tlb.req.bits.thid}]:PC[0x${Hexadecimal(mem_io.tlb.req.bits.addr)}]\n")
    }
    when(mem_io.tlb.resp.fire) {
      printf(p"${location}:iTLB:Resp:thid[${tlbMeta.tag}]:VA[0x${Hexadecimal(tlbMeta.req(0).addr)}]:PA[0x${Hexadecimal(mem_io.tlb.resp.bits.addr)}]\n")
    }
    when(mem_io.cache.req.fire) {
      printf(p"${location}:iCache:Req:thid[${cacheAdaptor.pipe_io.req.meta.inst.tag}]:PA[0x${Hexadecimal(mem_io.cache.req.bits.addr)}]\n")
    }
    when(mem_io.cache.resp.fire) {
      printf(p"${location}:iCache:Resp:thid[${cacheAdaptor.pipe_io.resp.meta.inst.tag}]\n" +
             p"   Hit[${mem_io.cache.resp.bits.hit}]:DATA[0x${Hexadecimal(cacheAdaptor.pipe_io.resp.port.bits.data)}]\n")
    }
  }

}