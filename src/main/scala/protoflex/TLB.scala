package protoflex
// See LICENSE.txt for license details.
import chisel3._
import chisel3.util.{Valid}
import common.PROCESSOR_TYPES._

class TLBEntry extends Bundle {
  val vpageAddr = DATA_T
  val ppageAddr = DATA_T
}

object TLBEntry {
  def apply(): TLBEntry = {
    val wire = Wire(new TLBEntry)
    wire.vpageAddr := 0.U
    wire.ppageAddr := 0.U
    wire
  }
  def apply(valid: Boolean): Valid[TLBEntry] = {
    val wire = Wire(Valid(new TLBEntry))
    wire.bits.vpageAddr := 0.U
    wire.bits.ppageAddr := 0.U
    wire.valid := valid.B
    wire
  }
}

class TLBUnit(implicit val cfg: ProcConfig) extends Module {
  private def getTLBEntry(bits: UInt): UInt = bits(PAGE_SZ + cfg.TLB_NB_ENTRY_W - 1, PAGE_SZ)
  private def maskPage(bits: UInt): UInt = bits(DATA_SZ-1,PAGE_SZ)
  val io = IO(new Bundle {
    val fillTLB = Input(Valid(new TLBEntry))
    val vaddr = Input(Valid(DATA_T))
    val paddr = Output(DATA_T)
    val miss = Output(Valid(DATA_T))
  })

  val tlbLUT = RegInit(VecInit(Seq.fill(cfg.TLB_NB_ENTRY)(TLBEntry(false))))

  val vpageEntryIdx = WireInit(getTLBEntry(io.vaddr.bits))
  val fillTLBEntryIdx = WireInit(getTLBEntry(io.fillTLB.bits.vpageAddr))

  when(io.fillTLB.valid) {
    tlbLUT(fillTLBEntryIdx) := io.fillTLB
  }

  io.paddr := io.vaddr.bits >> 2.U

  val vpageTLBEntry = WireInit(tlbLUT(vpageEntryIdx))
  val pageTLB = WireInit(maskPage(vpageTLBEntry.bits.vpageAddr))
  val pagePC  = WireInit(maskPage(io.vaddr.bits))
  val dirtyEntry = WireInit(vpageTLBEntry.valid && pageTLB =/= pagePC)
  val invalidEntry = WireInit(!tlbLUT(vpageEntryIdx).valid)
  io.miss.bits := io.vaddr.bits
  io.miss.valid := io.vaddr.valid && (invalidEntry || dirtyEntry)
}
