package protoflex
// See LICENSE.txt for license details.
import chisel3._
import chisel3.util.{Valid}
import common.PROCESSOR_TYPES._

object TLBEntry {
  def apply()(implicit cfg: ProcConfig): TLBEntry = {
    val wire = Wire(new TLBEntry)
    wire.tag := 0.U
    wire.valid := false.B
    wire.wrEn := false.B
    wire
  }
  def getTLBidx(bits: UInt)(implicit cfg: ProcConfig): UInt =
    bits(PAGE_SZ + cfg.TLB_NB_ENTRY_W - 1, PAGE_SZ)
  def getTLBtag(bits: UInt)(implicit cfg: ProcConfig): UInt =
    bits(DATA_SZ-1, PAGE_SZ + cfg.TLB_NB_ENTRY_W)
  def vaddr2paddr(bits: UInt): UInt = bits >> 3.U
}

class TLBEntry(implicit val cfg: ProcConfig) extends Bundle {
  val tag = UInt((DATA_SZ-PAGE_SZ-cfg.TLB_NB_ENTRY_W).W)
  val wrEn = Bool()
  val valid = Bool()
}

/* Full Associative
 *    LRU -> Least Recently Used
 */
class TLBUnit(implicit val cfg: ProcConfig) extends Module {
  import TLBEntry._
  val io = IO(new Bundle {
    val fillTLB = Input(ValidTag(DATA_T, new TLBEntry))
    val iPort = new Bundle {
      val isWr = Input(Bool())
      val vaddr = Input(Valid(DATA_T))
      val paddr = Output(DATA_T)
      val miss = Output(Valid(DATA_T))
    }
    val dPort = new Bundle {
      val isWr = Input(Bool())
      val vaddr = Input(Valid(DATA_T))
      val paddr = Output(DATA_T)
      val miss = Output(Valid(DATA_T))
    }
    val excpWrProt = Output(Bool())
  })

  val tlb = Mem(cfg.TLB_NB_ENTRY, new TLBEntry)
  //RegInit(VecInit(Seq.fill(cfg.TLB_NB_ENTRY)(TLBEntry(false))))
  val iPortIdx = WireInit(getTLBidx(io.iPort.vaddr.bits))
  val dPortIdx = WireInit(getTLBidx(io.dPort.vaddr.bits))
  val iPortTagIn = WireInit(getTLBtag(io.iPort.vaddr.bits))
  val dPortTagIn = WireInit(getTLBtag(io.dPort.vaddr.bits))

  val tlb_iPort = tlb(iPortIdx)
  val tlb_dPort = tlb(dPortIdx)
  val iPortOut = WireInit(tlb_iPort)
  val dPortOut = WireInit(tlb_dPort)

  val iPortDirty = iPortOut.tag =/= iPortTagIn
  val dPortDirty = dPortOut.tag =/= dPortTagIn

  val iPortMiss = !iPortOut.valid || iPortDirty
  val dPortMiss = !dPortOut.valid || dPortDirty

  val excpWrProt = WireInit(io.dPort.vaddr.valid && dPortOut.valid && !dPortOut.wrEn && io.dPort.isWr) ||
    (io.iPort.vaddr.valid && iPortOut.valid && !iPortOut.wrEn && io.iPort.isWr)

  io.iPort.paddr := vaddr2paddr(io.iPort.vaddr.bits)
  io.dPort.paddr := vaddr2paddr(io.dPort.vaddr.bits)
  io.iPort.miss.valid := io.iPort.vaddr.valid && iPortMiss
  io.dPort.miss.valid := io.dPort.vaddr.valid && dPortMiss
  io.iPort.miss.bits  := io.iPort.vaddr.bits
  io.dPort.miss.bits  := io.dPort.vaddr.bits

  val fPortIdx = WireInit(getTLBidx(io.fillTLB.tag))
  val tlb_fPort = tlb(fPortIdx)
  when(io.fillTLB.valid) {
    tlb(fPortIdx) := io.fillTLB.bits.get
  }

  io.excpWrProt := excpWrProt
}
