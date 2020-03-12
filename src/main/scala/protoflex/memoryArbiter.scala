package protoflex

import chisel3._
import chisel3.util._

import common.PROCESSOR_TYPES._
import common._

class MemArbiterData(implicit val cfg: ProcConfig) extends MultiIOModule
{
  val io = IO(new Bundle {

    val selHost = Output(Bool())
    val selMem = Output(Bool())

    val commitEnq = Input(Decoupled(new CommitInst))
    val commitDeq = Input(Decoupled(new CommitInst))

    val fillTLB = Input(ValidTag(DATA_T, new TLBEntry))
    val memPort = Flipped(new BRAMPort()(cfg.bramConfigMem))
    val tlbPort = Flipped(new Bundle {
      val isWr = Input(Bool())
      val vaddr = Input(Valid(DATA_T))
      val paddr = Output(DATA_T)
      val miss = Output(Valid(DATA_T))
    })

    val rfile = Flipped(new RFileIO)
    val rfileWr = Output(Bool())
    val rfileRd = Output(Bool())

    val busy = Output(Bool())
    val reqMiss = Output(Valid(DATA_T))
    val unalignedExcp = Output(Bool())
  })

  val s_Commiting :: s_MemCommiting :: s_MemCommitingPair :: s_GettingPage :: s_RestartMem :: Nil = Enum(5)
  val commitingStage = RegInit(s_Commiting)

  val minst = WireInit(io.commitDeq.bits.mem.bits)
  val commitNext = WireInit(io.commitEnq.bits)
  val missAddr = RegInit(DATA_X)

  // Commit Memory takes more cycles
  val dataAligner = Module(new DataAlignByte)
  dataAligner.io.minst := minst
  dataAligner.io.currReq := DontCare
  dataAligner.io.data := Mux(minst.isLoad, io.memPort.DO, io.rfile.rs1_data)

  io.selHost := commitingStage === s_GettingPage
  io.selMem := !(commitingStage === s_GettingPage)

  io.tlbPort <> DontCare
  io.tlbPort.isWr := false.B
  io.tlbPort.vaddr.valid := false.B

  io.rfile <> DontCare
  io.rfile.w1_en := false.B
  io.rfile.w2_en := false.B

  io.busy := !(commitingStage === s_Commiting)

  io.reqMiss.valid := commitingStage === s_GettingPage
  io.reqMiss.bits := missAddr

  io.rfileWr := false.B
  io.rfileRd := false.B

  io.unalignedExcp := dataAligner.io.unalignedExcp

  io.memPort.EN := true.B
  io.memPort.DI := DontCare
  io.memPort.WE := 0.U
  io.memPort.ADDR := DontCare


  switch(commitingStage) {
    is(s_Commiting) {

      // Pre-Load from memory
      io.tlbPort.isWr := !commitNext.mem.bits.isLoad
      io.tlbPort.vaddr.bits := commitNext.mem.bits.memReq(0).addr
      io.tlbPort.vaddr.valid := commitNext.mem.valid && io.commitEnq.fire()

      io.memPort.EN := true.B
      io.memPort.DI := DontCare
      io.memPort.WE := false.B
      io.memPort.ADDR := io.tlbPort.paddr

      when(io.commitEnq.fire() && commitNext.mem.valid) {
        commitingStage := s_MemCommiting
      }

      when(io.tlbPort.miss.valid) {
        commitingStage := s_GettingPage
        missAddr := io.tlbPort.miss.bits
      }
    }
    // Load is available
    is(s_MemCommiting) {
      dataAligner.io.currReq := 0.U

      io.rfileWr := true.B
      io.rfileRd := !minst.isLoad

      // WriteBack
      io.rfile.w2_addr := minst.rd.bits
      io.rfile.w2_data := minst.rd_res
      io.rfile.w2_en := minst.rd.valid

      // Load
      io.rfile.w1_addr := minst.memReq(0).reg
      io.rfile.w1_data := dataAligner.io.aligned
      io.rfile.w1_en := minst.isLoad

      // Stores
      io.rfile.rs1_addr := minst.memReq(0).reg
      // Write to memory if Store else read Pair Load
      io.memPort.EN := true.B
      io.memPort.DI := dataAligner.io.aligned
      io.memPort.WE := dataAligner.io.byteEn
      io.memPort.ADDR := Mux(minst.isLoad,
        TLBEntry.vaddr2paddr(minst.memReq(1).addr), // Load -> Load next address if Pair
        TLBEntry.vaddr2paddr(minst.memReq(0).addr)) // Store -> Store current address

      commitingStage := Mux(minst.isPair, s_MemCommitingPair, s_Commiting)

      // Check for TLB miss if Pair Memory inst
      io.tlbPort.isWr := !minst.isLoad
      io.tlbPort.vaddr.bits := minst.memReq(1).addr
      io.tlbPort.vaddr.valid := minst.isPair

      // ABORT Commit: Pair instruction missed
      when(io.tlbPort.miss.valid) {
        io.rfile.w1_en := false.B
        io.rfile.w2_en := false.B
        io.memPort.WE := 0.U
        missAddr := io.tlbPort.miss.bits
        commitingStage := s_GettingPage
      }
    }
    // No need to check for TLB dPort miss, both addresses already checked
    is(s_MemCommitingPair) {
      dataAligner.io.currReq := 1.U

      io.rfileWr := true.B
      io.rfileRd := !minst.isLoad

      io.tlbPort.isWr := DontCare
      io.tlbPort.vaddr.bits := DontCare
      io.tlbPort.vaddr.valid := false.B

      io.rfile.w1_addr := minst.memReq(1).reg
      io.rfile.w1_data := dataAligner.io.aligned
      io.rfile.w1_en := minst.isLoad


      // Write to memory if Store
      io.memPort.EN := true.B
      io.memPort.DI := dataAligner.io.aligned
      io.memPort.WE := dataAligner.io.byteEn
      io.memPort.ADDR := TLBEntry.vaddr2paddr(minst.memReq(1).addr)

      commitingStage := s_Commiting
    }
    is(s_GettingPage) {
      when(io.fillTLB.valid && io.fillTLB.tag === missAddr) {
        commitingStage := s_RestartMem
      }
    }
    // Pre-load: similar to s_Commiting, but take commitMem instead of commitNext
    is(s_RestartMem) {

      io.tlbPort.isWr := !minst.isLoad
      io.tlbPort.vaddr.bits := minst.memReq(0).addr
      io.tlbPort.vaddr.valid := true.B

      io.memPort.EN := true.B
      io.memPort.DI := DontCare
      io.memPort.WE := 0.U
      io.memPort.ADDR := io.tlbPort.paddr

      commitingStage := s_MemCommiting

      when(io.tlbPort.miss.valid) {
        commitingStage := s_GettingPage
      }
    }
  }
}

class MemArbiterInst(implicit val cfg: ProcConfig) extends MultiIOModule
{
  val io = IO(new Bundle {
    val vaddr = Input(Valid(DATA_T))

    val selHost = Output(Bool())
    val selMem = Output(Bool())

    val fillTLB = Input(ValidTag(DATA_T, new TLBEntry))
    val memPort = Flipped(new BRAMPort()(cfg.bramConfigMem))
    val tlbPort = Flipped(new Bundle {
      val isWr = Input(Bool())
      val vaddr = Input(Valid(DATA_T))
      val paddr = Output(DATA_T)
      val miss = Output(Valid(DATA_T))
    })

    val busy = Output(Bool())
    val reqMiss = Output(Valid(DATA_T))
  })

  val s_Fetching :: s_GettingPage :: Nil = Enum(2)
  val fetchStage = RegInit(s_Fetching)

  val missAddr = RegInit(DATA_X)

  io.selHost := fetchStage === s_GettingPage
  io.selMem := !(fetchStage === s_GettingPage)

  io.tlbPort.isWr := false.B
  io.tlbPort.vaddr := io.vaddr

  io.busy := !(fetchStage === s_Fetching)
  io.reqMiss.valid := fetchStage === s_GettingPage
  io.reqMiss.bits := missAddr

  io.memPort.EN := true.B
  io.memPort.DI := 0.U
  io.memPort.WE := 0.U
  io.memPort.ADDR := io.tlbPort.paddr

  switch(fetchStage) {
    is(s_Fetching) {
      when(io.tlbPort.miss.valid) {
        fetchStage := s_GettingPage
        missAddr := io.tlbPort.miss.bits
      }
    }

    is(s_GettingPage) {
      when(io.fillTLB.valid && io.fillTLB.tag === missAddr) {
        fetchStage := s_Fetching
      }
    }
  }
}
