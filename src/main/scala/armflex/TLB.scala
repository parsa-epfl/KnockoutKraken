package armflex
// See LICENSE.txt for license details.
import chisel3._
import chisel3.util.{Valid, Cat}

import arm.PROCESSOR_TYPES._
import util.PseudoLRU
import util.PseudoBitmapLRU

object TLBEntry {
  def getTLBtag(bits: UInt)(implicit cfg: ProcConfig): UInt =
    bits(DATA_SZ-1, PAGE_SZ)
}

class TLBEntry(implicit val cfg: ProcConfig) extends Bundle {
  val valid = Bool()
  val wrEn = Bool()
  val tag = UInt((DATA_SZ-PAGE_SZ).W)
}

class TLBMiss(implicit val cfg: ProcConfig) extends Bundle {
  val vaddr = DATA_T
  val tlbIdx = UInt(cfg.TLB_NB_ENTRY_W.W)
}
class TLBFill(implicit val cfg: ProcConfig) extends Bundle {
  val tlbEntry = new TLBEntry
  val vaddr = DATA_T
  val tlbIdx = UInt(cfg.TLB_NB_ENTRY_W.W)
}

/* Full Associative - Least Recently Used policy
 */
class TLBUnit(implicit val cfg: ProcConfig) extends Module {
  import TLBEntry._
  val io = IO(new Bundle {
    val fillTLB = Input(Valid(new TLBFill))

    val iPort = new Bundle {
      val isWr = Input(Bool())
      val vaddr = Input(Valid(DATA_T))
      val paddr = Output(DATA_T)
      val miss = Output(Valid(new TLBMiss))
    }
    val dPort = new Bundle {
      val isWr = Input(Bool())
      val vaddr = Input(Valid(DATA_T))
      val paddr = Output(DATA_T)
      val miss = Output(Valid(new TLBMiss))
    }
    val excpWrProt = Output(Bool())
  })

  val tlb = Mem(cfg.TLB_NB_ENTRY, io.fillTLB.bits.tlbEntry.asUInt.cloneType)
  //RegInit(VecInit(Seq.fill(cfg.TLB_NB_ENTRY)(TLBEntry(false))))
  val iPortTagIn = WireInit(getTLBtag(io.iPort.vaddr.bits))
  val dPortTagIn = WireInit(getTLBtag(io.dPort.vaddr.bits))

  val iPortIdx = WireInit(0.U(cfg.TLB_NB_ENTRY_W.W))
  val dPortIdx = WireInit(0.U(cfg.TLB_NB_ENTRY_W.W))
  val iPortHit = WireInit(false.B)
  val dPortHit = WireInit(false.B)

  val excpWrProtData = WireInit(false.B)
  val excpWrProtInst = WireInit(false.B)
  for(i <- 0 until cfg.TLB_NB_ENTRY) {
    val tlbEntry = tlb(i).asTypeOf(new TLBEntry)
    when(tlbEntry.valid) {
      when(tlbEntry.tag === iPortTagIn) {
        iPortIdx := i.U
        iPortHit := true.B
        excpWrProtInst := !tlbEntry.wrEn && io.iPort.isWr
      }
      when(tlbEntry.tag === dPortTagIn) {
        dPortIdx := i.U
        dPortHit := true.B
        excpWrProtData := !tlbEntry.wrEn && io.dPort.isWr
      }
    }
  }

  val lru = Module(new PseudoBitmapLRU(cfg.TLB_NB_ENTRY, cfg.TLB_NB_ENTRY_W))
  lru.io.idx_1.bits := iPortIdx
  lru.io.idx_2.bits := dPortIdx
  lru.io.idx_1.valid := iPortHit && io.iPort.vaddr.valid
  lru.io.idx_2.valid := dPortHit && io.dPort.vaddr.valid

  io.iPort.paddr := Cat(iPortIdx, (io.iPort.vaddr.bits >> 3.U)(PAGE_SZ-4, 0))
  io.dPort.paddr := Cat(dPortIdx, (io.dPort.vaddr.bits >> 3.U)(PAGE_SZ-4, 0))
  // NOTE This is a hack in the exceptional case both would miss same cycle
  val pseudoLRU_idx2 = Cat(~lru.io.lru_idx(cfg.TLB_NB_ENTRY_W-1), lru.io.lru_idx(cfg.TLB_NB_ENTRY_W-2, 0))
  io.iPort.miss.bits.tlbIdx := Mux(io.dPort.miss.valid, pseudoLRU_idx2, lru.io.lru_idx)
  io.dPort.miss.bits.tlbIdx := lru.io.lru_idx
  io.iPort.miss.bits.vaddr := io.iPort.vaddr.bits
  io.dPort.miss.bits.vaddr := io.dPort.vaddr.bits
  io.iPort.miss.valid := !iPortHit && io.iPort.vaddr.valid
  io.dPort.miss.valid := !dPortHit && io.dPort.vaddr.valid

  when(io.fillTLB.valid) {
    tlb(io.fillTLB.bits.tlbIdx) := io.fillTLB.bits.tlbEntry.asUInt
  }

  io.excpWrProt := (excpWrProtData && io.dPort.vaddr.valid) || (excpWrProtInst && io.iPort.vaddr.valid)
}
