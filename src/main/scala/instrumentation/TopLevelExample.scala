package instrumentation

import chisel3._
import chisel3.util._

import antmicro.CSR._
import antmicro.Bus._
import armflex.util._
import armflex.util.ExtraUtils._
import javax.script.Bindings

class TopLevelExampleParams(
  val dramParams: DRAMExampleParams,
  val csrParams: CSRExampleParams,
  val computeParams: ComputeExampleParams
)

class TopLevelExample(val params: TopLevelExampleParams) extends MultiIOModule {
  private val csr = Module(new CSRExample(params.csrParams, params.computeParams))
  private val compute = Module(new ComputeExample(params.computeParams, csr.bramParams))
  compute.bramPort <> csr.bramPort
  compute.ctrl <> csr.ctrl

  val dram_io = IO(compute.dram_io.cloneType)
  val S_AXIL = IO(Flipped(csr.S_AXIL.cloneType))
  compute.dram_io <> dram_io
  csr.S_AXIL <> S_AXIL
}


/**
 * In this example we showcase an accesible control register flow from AXIL.
 * This can be used to receive and send low frequency commands to the host.
 * 
 * Here we have 1024 32-bit entries  stored in BRAM, accesible from both the PL
 * and from the host.
 * We also have 16 32-bit registers stored in LUT, which can be configured to
 * either a CSR Clean/Set/Status/Simple type of registers.
 * 
 * CSR: Control and Status Register
 * 
 * We showcase two examples of CSR registers: Set, which serves as rising a flag from the
 * FPGA to be consumed by the host. And Clear, which servers as rising a flag from the host 
 * to be consumed by the FPGA.
 * 
 */
class CSRExampleParams(
  val axilDataW: Int = 32
) {
  val bramWords: Int = 1024
  val ctrlRegWords: Int = 4
  val csrRegsN: Int = bramWords + ctrlRegWords

  val bramBaseAddr: Int = 0x0

  // Base addr for CSR Regs
  val ctrlRegsBaseAddr: Int = bramWords
  // CSR Regs
  val ctrlRegDoneAddr: Int = 0
  val ctrlRegStartAddr: Int = 1
  val ctrlRegAddrAddr: Int = 2
  val ctrlRegBurstAddr: Int = 3
  
  def getAddrAXIL(addr: Int) = addr << log2Ceil(axilDataW/8)
  def getAddrCSR(addr: Int) =  addr >> log2Ceil(axilDataW/8)
}

class CSRExample(val params: CSRExampleParams, val computeParams: ComputeExampleParams) extends MultiIOModule {
  val bramParams = new BRAMParams(
    NB_COL = 4, COL_WIDTH = 8, NB_ELE = params.bramWords, 
    INIT_FILE = "", false, false, true, false)
  

  // Generate AXIL accesible control registers
  
  // This module transforms AXIL transactions to CSR bus
  private val uAxilToCSR = Module(new AXI4LiteCSR(params.axilDataW, params.getAddrAXIL(params.csrRegsN)))

  // Here we have the base address and number of registers for the modules
  // addressable by AXIL
  private val cfgBusCSR_stateregs = new CSRBusSlave(params.bramBaseAddr, params.bramWords)
  private val cfgBusCSR_ctrlregs = new CSRBusSlave(params.ctrlRegsBaseAddr, params.ctrlRegWords)

  // We have two CSR modules, one translate transactions to access BRAM 
  // and the other one allows you to use one of the multiple CSR modules 
  private val uCSR_stateregs = Module(new CSR2BRAM32(new BRAMParams()))
  private val uCSR_ctrlregs = Module(new CSR(params.axilDataW, params.ctrlRegWords))

  // We connect the AXIL Master -> N CSR Slaves
  private val uCSRmux = Module(new CSRBusMasterToNSlaves(params.axilDataW, 
    Seq(cfgBusCSR_stateregs, cfgBusCSR_ctrlregs), (0x0, params.csrRegsN)))
  uCSRmux.masterBus <> uAxilToCSR.io.bus
  uCSRmux.slavesBus(0) <> uCSR_stateregs.io.bus
  uCSRmux.slavesBus(1) <> uCSR_ctrlregs.io.bus

  // Generate Control registers

  // This register will set bits when `wireSetBits` is raised.
  // You can read the bits either from `setBits` or from the AXIL interface
  // To lower the bits, just write to the register with a mask.
  private val wireSetBits = WireInit(WireInit(VecInit(0.U(32.W).asBools)))
  private val setBits = SetCSR(wireSetBits.asUInt, uCSR_ctrlregs.io.csr(0), params.axilDataW)
  
  // This register will set bits when a mask is written by AXIL interface.
  // You can read the bits pending bits `pendingBitsToClear` or from the AXIL interface
  // To lower the bits, just rise the mask with `wireClearBits`.
  private val wireClearBits = WireInit(VecInit(0.U(32.W).asBools))
  private val pendingBitsToClear = ClearCSR(wireClearBits.asUInt, uCSR_ctrlregs.io.csr(1), params.axilDataW)

  private val addrCSRReg = SimpleCSR(uCSR_ctrlregs.io.csr(2), params.axilDataW)
  private val burstCSRReg = SimpleCSR(uCSR_ctrlregs.io.csr(3), params.axilDataW)

  // Connect to BRAM for a wide array of addressable registers
  private val bram = Module(new BRAM()(bramParams))
  bram.portA <> uCSR_stateregs.io.port


  // SHELL IO
  val S_AXIL = IO(Flipped(uAxilToCSR.io.ctl.cloneType))
  S_AXIL <> uAxilToCSR.io.ctl

  // Exposed BRAM port
  val bramPort = IO(new BRAMPort()(bramParams))
  bram.portB <> bramPort

  // Control registers for module example
  val ctrl = IO(Flipped(new ComputeExampleCTRL(computeParams.addrW)))
  ctrl.addr := addrCSRReg
  ctrl.burstS := burstCSRReg
  ctrl.start.valid := pendingBitsToClear(0).asBool
  ctrl.start.bits := pendingBitsToClear(1).asUInt
  when(ctrl.start.valid) {
    wireClearBits(0) := ctrl.start.ready.asUInt
    wireClearBits(1) := ctrl.start.ready.asUInt
  }
  ctrl.done.ready := true.B
  wireSetBits(0) := ctrl.done.valid.asUInt
}

/**
  * The following compute example takes commands, and has a state machine to
  * pack words from a 32-bit buffer into a 512-bit buffer for sending burst
  * requests to memory.
  */
object CommandTypes {
  val cWrite :: cRead ::  Nil = Enum(2)
  def cType = cRead.cloneType
}

import CommandTypes._


class ComputeExampleParams(
  val addrW: Int = 12,
  val dataW: Int = 512
)
class ComputeExampleCTRL(val addrW: Int) extends Bundle {
  val burstS = Input(UInt(8.W))
  val addr = Input(UInt(addrW.W))
  val start =  Flipped(Decoupled(cType))
  val done = Decoupled()
}
class ComputeExample(val params: ComputeExampleParams, val toPack: BRAMParams) extends MultiIOModule {
  private val bramBufferData = new BRAMParams(
    NB_COL = params.dataW/8, COL_WIDTH = 8, NB_ELE = 1024, 
    INIT_FILE = "", false, false, true, false)
  private val bram = Module(new BRAM()(bramBufferData))

  val bramPort = IO(Flipped(new BRAMPort()(toPack)))
  val ctrl = IO(new ComputeExampleCTRL(params.addrW))

  // The write port allows for burst data writes
  val dram_io = IO(new Bundle{
    val read = Flipped(new ReadPort(params.addrW, params.dataW))
    val write = Flipped(new WritePort(params.addrW, params.dataW))
  })


  private val s_Idle :: s_Packing :: s_Address :: s_WaitLastWord :: s_WaitBufferWrite :: s_Bursting :: s_WaitAck :: s_PrepareFirst :: Nil = Enum(8)
  private val step = RegInit(s_Idle)
  private val dramCmd = RegInit(cType, 0.U.asTypeOf(cRead))
  private val burstSize = RegInit(0.U.asTypeOf(ctrl.burstS))
  private val addr = RegInit(0.U.asTypeOf(ctrl.addr))

  ctrl.start.ready := (step === s_Idle)
  ctrl.done.valid := (step === s_WaitAck)

  private val wordsPerBlock = bramBufferData.NB_COL/toPack.NB_COL
  private val currBlock = RegInit(0.U.asTypeOf(ctrl.burstS))
  private val currWord = RegInit(0.U(log2Ceil(wordsPerBlock).W))
  private val bufferBlock = RegInit(VecInit(Seq.fill(wordsPerBlock)(0.U((toPack.NB_COL*toPack.COL_WIDTH).W))))

  private val firstBlock = RegInit(false.B)

  bramPort.ADDR := Cat(currBlock, currWord)
  bramPort.EN := true.B
  bramPort.WE := Fill(bramPort.WE.getWidth, step === s_Packing && dramCmd === cRead)
  bramPort.DI := bufferBlock(currBlock)
  when(RegNext(step === s_Packing)) {
    bufferBlock(RegNext(currWord)) := bramPort.DO
  }
  val getWords = bramPort.DO.asBools.sliding(params.dataW).map(seq => VecInit(seq).asUInt).toSeq

  //.map(seq => seq.asUInt)
  when(firstBlock) {
    bufferBlock.zip(getWords).map( ele => ele._1 := ele._2)
  }

  when(dramCmd === cWrite) {
    // It takes 2 cycles from address to buffering the block
    // ADDR -> BRAM SMALL DATA -> REG BUFFER
    bram.portA.ADDR := RegNext(RegNext(currBlock))
    bram.portA.DI := bufferBlock.asUInt
    bram.portA.EN := true.B
    bram.portA.WE := Fill(bram.portA.WE.getWidth, RegNext(RegNext(step === s_Packing)))
  }.elsewhen(dramCmd === cRead) {
    // TODO support for Read
    bram.portA <> DontCare
    bram.portA.EN := false.B
  }.otherwise {
    bram.portA <> DontCare
    bram.portA.EN := false.B
  }

  when(dramCmd === cWrite) {
    bram.portB.ADDR := currBlock
    bram.portB.EN := true.B
    bram.portB.WE := false.B
    bram.portB.DI := DontCare
  }.elsewhen(dramCmd === cRead) {
    // TODO support for Read
    bram.portB <> DontCare
    bram.portB.EN := false.B
  }.otherwise {
    bram.portB <> DontCare
    bram.portB.EN := false.B
  }

  // TODO support for Read
  dram_io.read.data.ready := step === s_Bursting
  dram_io.read.req.valid := step === s_Address && dramCmd === cRead
  dram_io.read.req.bits.w_en := 0.U
  dram_io.read.req.bits.addr := addr
  dram_io.read.req.bits.burst := burstSize
  
  dram_io.write.data.valid := step === s_Bursting
  dram_io.write.data.bits := Mux(firstBlock, bufferBlock.asUInt, bram.portB.DO)
  dram_io.write.req.valid := step === s_Address && dramCmd === cWrite
  dram_io.write.req.bits.w_en := Fill(dram_io.read.req.bits.w_en.getWidth, (dramCmd === cWrite).asUInt)
  dram_io.write.req.bits.addr := addr
  dram_io.write.req.bits.burst := burstSize

  switch(step) {
    is(s_Idle) {
      when(ctrl.start.fire) {
        step := s_Packing
        burstSize := ctrl.burstS
        dramCmd := ctrl.start.bits
        addr := ctrl.addr
      }
    }
    is(s_Packing) {
      currWord := currWord + 1.U
      when(currWord === (wordsPerBlock - 1).U) {
        currWord := 0.U
        currBlock := currBlock + 1.U
        when((currBlock+1.U) === burstSize) {
          currBlock := 0.U
          currWord := 0.U
          step := s_WaitLastWord
        }
      }
    }
    is(s_WaitLastWord) {
      step := s_WaitBufferWrite
    }
    is(s_WaitBufferWrite) {
      step := s_Address
    }
    is(s_Address) {
      when(dram_io.read.req.fire || dram_io.write.req.fire) {
        step := s_PrepareFirst
      }
    }
    is(s_PrepareFirst) {
      step := s_Bursting
      currBlock := currBlock + 1.U
    }
    is(s_Bursting) {
      when(dram_io.read.data.fire || dram_io.write.data.fire) {
        firstBlock := false.B
        currBlock := currBlock + 1.U
        when(currBlock === burstSize) {
          step := s_WaitAck
        }
      }
    }
    is(s_WaitAck) {
      when(ctrl.done.fire) {
        step := s_Idle
      }
    }
  }

}


/**
  * This generates the verilog code.
  */
object ARMFlexTopVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  import java.io._
  import firrtl.options.TargetDirAnnotation

  c.emitVerilog(
    new TopLevelExample(new TopLevelExampleParams(
      new DRAMExampleParams(),
      new CSRExampleParams(),
      new ComputeExampleParams()
    )), annotations = Seq(TargetDirAnnotation("test/example/")))
}


/**
  * This class wraps up the module and gives access to the DRAM ports.
  * Not necessary for testing.
  */
class TopLevelExampleDRAM(val params: TopLevelExampleParams) extends MultiIOModule {
  private val system = Module(new TopLevelExample(params))
  private val dram = Module(new DRAMWrapper(params.dramParams))
  system.dram_io.read <> dram.read
  system.dram_io.write <> dram.write
  private val axiMulti_R = Module(new AXIReadMultiplexer(params.dramParams.pAddrW, params.dramParams.pDataW, 1))
  private val axiMulti_W = Module(new AXIWriteMultiplexer(params.dramParams.pAddrW, params.dramParams.pDataW, 1))

  val SHELL_IO = IO(new Bundle {
    val M_AXI = new AXI4(params.dramParams.pAddrW, params.dramParams.pDataW)
    val S_AXIL = system.S_AXIL.cloneType
  })
  SHELL_IO.S_AXIL <> system.S_AXIL

  dram.SHELL_IO_AXI.M_DMA_R <> axiMulti_R.S_IF(0)
  dram.SHELL_IO_AXI.M_DMA_W <> axiMulti_W.S_IF(0)
  // Interconnect Read ports
  SHELL_IO.M_AXI.ar <> axiMulti_R.M_AXI.ar
  SHELL_IO.M_AXI.r <> axiMulti_R.M_AXI.r
  // Disable Write ports
  axiMulti_R.M_AXI.aw <> AXI4AW.stub(params.dramParams.pAddrW)
  axiMulti_R.M_AXI.w <> AXI4W.stub(params.dramParams.pDataW)
  axiMulti_R.M_AXI.b <> AXI4B.stub()
  // Interconnect Write ports
  SHELL_IO.M_AXI.aw <> axiMulti_W.M_AXI.aw
  SHELL_IO.M_AXI.w <> axiMulti_W.M_AXI.w
  SHELL_IO.M_AXI.b <> axiMulti_W.M_AXI.b
   // Disable Read ports
  axiMulti_W.M_AXI.ar <> AXI4AR.stub(params.dramParams.pAddrW)
  axiMulti_W.M_AXI.r <> AXI4R.stub(params.dramParams.pDataW)
}
class DRAMExampleParams(
  val pAddrW: Int = 12,
  val pDataW: Int = 512
)
class DRAMWrapper(val params: DRAMExampleParams) extends MultiIOModule {
  assert(params.pDataW == 512, "Assumes that we match AWS F1 DRAM width.")
  val SHELL_IO_AXI = IO(new Bundle {
    val M_DMA_R = new AXIReadMasterIF(params.pAddrW, params.pDataW)
    val M_DMA_W = new AXIWriteMasterIF(params.pAddrW, params.pDataW)
  })

  // The read port allows for burst data writes
  val read = IO(new ReadPort(params.pAddrW, params.pDataW))
  SHELL_IO_AXI.M_DMA_R.req.bits.address := read.req.bits.addr
  SHELL_IO_AXI.M_DMA_R.req.bits.length := read.req.bits.burst
  SHELL_IO_AXI.M_DMA_R.req.handshake(read.req)
  read.data <> SHELL_IO_AXI.M_DMA_R.data

  // The write port allows for burst data writes
  val write = IO(new WritePort(params.pAddrW, params.pDataW))

  SHELL_IO_AXI.M_DMA_W.req.bits.address := write.req.bits.addr
  SHELL_IO_AXI.M_DMA_W.req.bits.length := write.req.bits.burst
  SHELL_IO_AXI.M_DMA_W.req.handshake(write.req)
  SHELL_IO_AXI.M_DMA_W.data <> write.data
}