package armflex

import chisel3._
import chisel3.util._

import arm.PROCESSOR_TYPES._
import arm.DECODE_CONTROL_SIGNALS._

import util._

class MemPort(implicit val cfg: ProcConfig) extends Bundle {
  val port = new BRAMPort()(cfg.bramConfigMem)
  val busy = Output(Bool())
  val miss = Output(Bool())
}

class MemorySystem(implicit val cfg: ProcConfig) extends MultiIOModule
{
  implicit val bramCfg: BRAMConfig = cfg.bramConfigMem
  val ctrl = IO(new Bundle {
    val fillTLB = Flipped(Decoupled(new TLBFill))
    val reqMiss = Decoupled(new TLBMiss)
  })

  val fillQueue = Module(new Queue(new TLBFill, 2))
  val missQueue = Module(new Queue(new TLBMiss, 8))

  val ports = IO(new Bundle {
    val fetch = new MemPort
    val ls    = new MemPort
    val isWRProt = Output(Bool())
    val host  = new MemPort
  }) 

  val memory = Module(new BRAM()(cfg.bramConfigMem))

  val tlb = Module(new TLBUnit())
  tlb.io.iPort.isWr := false.B
  tlb.io.iPort.vaddr.bits := ports.fetch.port.ADDR
  tlb.io.iPort.vaddr.valid := ports.fetch.port.EN

  tlb.io.dPort.isWr := true.B
  tlb.io.dPort.vaddr.bits := ports.ls.port.ADDR
  tlb.io.dPort.vaddr.valid := ports.ls.port.EN
  // Only dPort has WB so has WrProt Excp
  val isWRProt = RegNext(tlb.io.excpWrProt)

  val arbiter = Module(new PortArbiter)
  arbiter.io.ports.fetch <> ports.fetch.port
  arbiter.io.ports.fetch.ADDR := tlb.io.iPort.paddr

  arbiter.io.ports.ls <> ports.ls.port
  arbiter.io.ports.ls.ADDR := tlb.io.dPort.paddr
  ports.isWRProt := isWRProt

  arbiter.io.ports.host <> ports.host.port

  memory.portA <> arbiter.io.bram.portA
  memory.portB <> arbiter.io.bram.portB

  ports.fetch.busy := arbiter.io.busy.fetch
  ports.ls.busy := arbiter.io.busy.ls
  ports.host.busy := arbiter.io.busy.host

  // Handle Miss and Fetch Page
  ports.fetch.miss := tlb.io.iPort.miss.valid
  ports.ls.miss := tlb.io.dPort.miss.valid
  ports.host.miss := false.B

  // TODO: MissQueue dPort race condition
  missQueue.io.enq.bits := tlb.io.iPort.miss.bits
  missQueue.io.enq.valid := tlb.io.iPort.miss.valid

  ctrl.fillTLB <> fillQueue.io.enq
  ctrl.reqMiss <> missQueue.io.deq

  tlb.io.fillTLB.bits := fillQueue.io.deq.bits
  tlb.io.fillTLB.valid := fillQueue.io.deq.valid
  fillQueue.io.deq.ready := true.B
}

class PortArbiter(implicit val cfg: ProcConfig) extends MultiIOModule {
  implicit val bramCfg: BRAMConfig = cfg.bramConfigMem
  val io = IO(new Bundle {
    val ports = new Bundle {
      val fetch = new BRAMPort
      val ls    = new BRAMPort
      val host  = new BRAMPort
    } 

    val busy = new Bundle {
      val fetch = Output(Bool())
      val ls    = Output(Bool())
      val host  = Output(Bool())
    }

    val bram = new Bundle {
      val portA = Flipped(new BRAMPort)
      val portB = Flipped(new BRAMPort)
    }
  })

  io.busy.fetch := false.B
  io.busy.host := false.B
  io.busy.ls := false.B
  io.bram.portA <> io.ports.host
  io.bram.portB <> io.ports.host 

  when(io.ports.fetch.EN && io.ports.ls.EN && io.ports.host.EN) {
    // Triple port request, stall fetch
    io.bram.portA <> io.ports.host
    io.bram.portB <> io.ports.ls
    io.busy.fetch := true.B
  }.elsewhen(!io.ports.fetch.EN && !io.ports.ls.EN) {
    // Don't double map host port
    io.bram.portB.EN := false.B
  }.otherwise {
    when(io.ports.fetch.EN) {
      io.bram.portA <> io.ports.fetch
    }
    when(io.ports.ls.EN) { 
      io.bram.portB <> io.ports.ls 
    }
  }
  io.ports.fetch.DO := io.bram.portA.DO
  io.ports.ls.DO := io.bram.portB.DO
}

class MemArbiterData(implicit val cfg: ProcConfig) extends MultiIOModule
{
  val io = IO(new Bundle {

    val selHost = Output(Bool())
    val selMem = Output(Bool())

    val commitEnq = Input(Decoupled(new CommitInst))
    val commitDeq = Input(Decoupled(new CommitInst))

    val fillTLB = Input(Valid(new TLBFill))
    val memPort = Flipped(new BRAMPort()(cfg.bramConfigMem))
    val tlbPort = Flipped(new TLBPort)

    val rfile = Flipped(new RFileIO)
    val rfileWr = Output(Bool())
    val rfileRd = Output(Bool())

    val busy = Output(Bool())
    val reqMiss = Output(ValidTag(MISS_T, new TLBMiss))
    val unalignedExcp = Output(Bool())
  })

  val s_Commiting :: s_MemCommiting :: s_MemCommitingPair :: s_GettingPage :: s_RestartMem :: Nil = Enum(5)
  val commitingStage = RegInit(s_Commiting)

  val minst = WireInit(io.commitDeq.bits.mem.bits)
  val commitNext = WireInit(io.commitEnq.bits)
  val tlbpaddr = RegInit(DATA_X)
  val missTLB = RegInit(new TLBMiss, 0.U.asTypeOf(new TLBMiss))

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
  io.rfile.rw_wen := false.B

  io.busy := !(commitingStage === s_Commiting)

  io.reqMiss.valid := commitingStage === s_GettingPage
  io.reqMiss.tag := Mux(minst.isLoad, DATA_LOAD.U, DATA_STORE.U)
  io.reqMiss.bits.get := missTLB

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
        tlbpaddr := io.tlbPort.paddr
        commitingStage := s_MemCommiting
      }

      when(io.tlbPort.miss.valid) {
        commitingStage := s_GettingPage
        missTLB := io.tlbPort.miss.bits
      }
    }
    // Load is available
    is(s_MemCommiting) {
      dataAligner.io.currReq := 0.U

      io.rfileWr := true.B
      io.rfileRd := !minst.isLoad

      // Check for TLB miss if Pair Memory inst
      io.tlbPort.isWr := !minst.isLoad
      io.tlbPort.vaddr.bits := minst.memReq(1).addr
      io.tlbPort.vaddr.valid := minst.isPair

      // WriteBack
      io.rfile.rw_addr := minst.rd.bits
      io.rfile.rw_di := minst.rd_res
      io.rfile.rw_wen := minst.rd.valid

      // Load
      io.rfile.w1_addr := minst.memReq(0).reg
      io.rfile.w1_data := dataAligner.io.aligned
      io.rfile.w1_en := minst.isLoad

      // Stores
      io.rfile.rs1_addr := minst.memReq(0).reg
      // Write to memory if Store else read Pair Load
      io.memPort.EN := true.B
      io.memPort.DI := dataAligner.io.aligned
      // ST wzr, addr NOTE Might be missing cases
      when(minst.memReq(0).reg === 31.U) {
        io.memPort.DI := 0.U
      }
      io.memPort.WE := dataAligner.io.byteEn
      io.memPort.ADDR := Mux(minst.isLoad,
        io.tlbPort.paddr, // Load -> Load next address if Pair
        tlbpaddr)         // Store -> Store last cycle preread memReq(0) address

      tlbpaddr := io.tlbPort.paddr
      commitingStage := Mux(minst.isPair, s_MemCommitingPair, s_Commiting)

      // ABORT Commit: Pair instruction missed
      when(io.tlbPort.miss.valid) {
        io.rfile.w1_en := false.B
        io.rfile.rw_wen := false.B
        io.memPort.WE := 0.U
        missTLB := io.tlbPort.miss.bits
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

      // Load
      io.rfile.w1_addr := minst.memReq(1).reg
      io.rfile.w1_data := dataAligner.io.aligned
      io.rfile.w1_en := minst.isLoad

      // Stores
      io.rfile.rs1_addr := minst.memReq(1).reg
      // Write to memory if Store
      io.memPort.EN := true.B
      io.memPort.DI := dataAligner.io.aligned
      when(minst.memReq(1).reg === 31.U) {
        io.memPort.DI := 0.U
      }
      io.memPort.WE := dataAligner.io.byteEn
      io.memPort.ADDR := tlbpaddr

      commitingStage := s_Commiting
    }
    is(s_GettingPage) {
      when(io.fillTLB.valid && io.fillTLB.bits.vaddr === missTLB.vaddr) {
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

      tlbpaddr := io.tlbPort.paddr
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

    val fillTLB = Input(Valid(new TLBFill))
    val memPort = Flipped(new BRAMPort()(cfg.bramConfigMem))
    val tlbPort = Flipped(new TLBPort)

    val busy = Output(Bool())
    val reqMiss = Output(Valid(new TLBMiss))
  })

  val s_Fetching :: s_GettingPage :: Nil = Enum(2)
  val fetchStage = RegInit(s_Fetching)

  val missTLB = RegInit(new TLBMiss, 0.U.asTypeOf(new TLBMiss))

  io.selHost := fetchStage === s_GettingPage
  io.selMem := !(fetchStage === s_GettingPage)

  io.tlbPort.isWr := false.B
  io.tlbPort.vaddr := io.vaddr

  io.busy := !(fetchStage === s_Fetching)
  io.reqMiss.valid := fetchStage === s_GettingPage
  io.reqMiss.bits := missTLB

  io.memPort.EN := true.B
  io.memPort.DI := 0.U
  io.memPort.WE := 0.U
  io.memPort.ADDR := io.tlbPort.paddr

  switch(fetchStage) {
    is(s_Fetching) {
      when(io.tlbPort.miss.valid) {
        fetchStage := s_GettingPage
        missTLB := io.tlbPort.miss.bits
      }
    }

    is(s_GettingPage) {
      when(io.fillTLB.valid && io.fillTLB.bits.vaddr === missTLB.vaddr) {
        fetchStage := s_Fetching
      }
    }
  }
}
