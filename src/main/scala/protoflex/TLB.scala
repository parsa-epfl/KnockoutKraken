package protoflex
// See LICENSE.txt for license details.
import chisel3._
import chisel3.util.{Valid}
import common.PROCESSOR_TYPES._

class TLBUnitIO(implicit val cfg: ProcConfig) extends Bundle {
  val fillTLB = Input(Valid(new TLBEntry))

  val vaddr = Input(DATA_T)
  val paddr = Output(DATA_T)

  val miss = Output(Valid(DATA_T))
}

class TLBEntry extends Bundle {
  val vpageAddr = UInt((DATA_SZ - PAGE_SZ).W)
  val ppageAddr = UInt((DATA_SZ - PAGE_SZ).W)
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
  val pageNbr_SZ = DATA_SZ - 12 // -2 for Byte addressable to 32b
  val io = IO(new TLBUnitIO)
  val tlbLUT = RegInit(VecInit(Seq.fill(cfg.TLB_NB_ENTRY)(TLBEntry(false))))

  val vpage = io.vaddr >> PAGE_SZ

  when(io.fillTLB.valid) {
    tlbLUT(0) := io.fillTLB
  }

  io.paddr := io.vaddr >> 2.U

  io.miss.bits := io.vaddr
  io.miss.valid := tlbLUT(0).valid && tlbLUT(0).bits.vpageAddr =/= vpage && false.B
}
