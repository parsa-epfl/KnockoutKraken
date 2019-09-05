package protoflex
// See LICENSE.txt for license details.
import chisel3._
import chisel3.util.{Valid}
import common.PROCESSOR_TYPES._

class TLBUnitIO(implicit val cfg: ProcConfig) extends Bundle {
  val vaddrFill = Input(DATA_T)
  val fill = Input(Bool())

  val vaddr = Input(Valid(DATA_T))
  val miss = Output(Bool())
  val hit = Output(Bool())
  val paddr = Output(Valid(DATA_T))
}

class TLBUnit(implicit val cfg: ProcConfig) extends Module {
  val io = IO(new TLBUnitIO)

  val vpage = io.vaddr.bits >> cfg.ppageBRAMc.addrWidthVec(0)
  val vpageTLB = vpage(cfg.TLB_NB_ENTRY_W-1,0)
  val vpageTAG = vpage >> cfg.TLB_NB_ENTRY_W

  val vpageFill = io.vaddrFill >> cfg.ppageBRAMc.addrWidthVec(0)
  val vpageFillTLB = vpageFill(cfg.TLB_NB_ENTRY_W-1,0)
  val vpageFillTAG = vpageFill >> cfg.TLB_NB_ENTRY_W

  val dataInit = {
    val wire = Wire(Valid(UInt((DATA_SZ - cfg.TLB_NB_ENTRY_W - cfg.ppageBRAMc.addrWidthVec(0)).W)))
    wire.valid := false.B
    wire.bits := DATA_X
    wire
  }

  val tlb = RegInit(VecInit(Seq.fill(cfg.TLB_NB_ENTRY)(dataInit)))
  when(io.fill) {
    tlb(vpageFillTLB).bits := vpageFillTAG
    tlb(vpageFillTLB).valid := true.B
  }

  io.miss := tlb(vpageTLB).valid && tlb(vpageTLB).bits =/= vpageTAG
  io.hit := tlb(vpageTLB).valid && tlb(vpageTLB).bits === vpageTAG
  io.paddr.valid := io.vaddr.valid
  io.paddr.bits := io.vaddr.bits >> 2.U
}
