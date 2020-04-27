package armflex

import chisel3._
import chisel3.util.{Cat, Valid}

import util._

import arm.PROCESSOR_TYPES._

object AxiDriver extends App {
  chisel3.Driver.execute(Array("-tn", "ProcAxi", "-td", "verilog", "-fsm"), () =>
    new ProcAxiWrap()(new ProcConfig(2, false, 16)))
}

class ProcAxiWrap(implicit val cfg: ProcConfig) extends MultiIOModule {
  def str2bool(c:Char):Boolean = c match {
    case '0' => false
    case '1' => true
  }

  def reverseEndianess(bits: UInt, wordSize: Integer, wordNb: Integer): UInt = {
    val splitted = VecInit.tabulate(wordNb) { idx => bits((idx+1)*wordSize-1, idx*wordSize) }
    val reversed = VecInit(splitted.reverse)
    reversed.asUInt
  }

  // Register file
  // 1 Pulse Reg, 7 ReadOnly regs, 8 Rd-Write Regs, 
  // 16 ReadOnly Regs, 1 Pulse reg
  // 
  val readOnly  = "011111110000000011111111111111110".map(str2bool)
  val pulseOnly = "100000000000000000000000000000000".map(str2bool)
  val cfgAxiMM = new AxiMemoryMappedRegFileConfig(33, readOnly, pulseOnly)
  val regFile  = Module(new AxiMemoryMappedRegFile()(cfgAxiMM))

  val proc = Module(new Proc())

  val io = IO(new Bundle {
    val axiLite = AxiLiteSlave(cfgAxiMM.axiLiteConfig)

    val memoryBRAM = new BRAMPort()(cfg.bramConfigMem(true))
    val stateBRAM = new BRAMPort()(cfg.bramConfigState(true))

    /* To Infer bram port, add these attributes to ports in the following manner

     (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 memoryBRAM CLK" *)
     input         io_memoryBRAM_clk, // @[:@3430.4]
     (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 memoryBRAM RST" *)
     input         io_memoryBRAM_rst, // @[:@3430.4]
     (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 memoryBRAM WE" *)
     input         io_memoryBRAM_writeEn, // @[:@3430.4]
     (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 memoryBRAM EN" *)
     input         io_memoryBRAM_en, // @[:@3430.4]
     (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 memoryBRAM ADDR" *)
     input [9:0]   io_memoryBRAM_addr, // @[:@3430.4]
     (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 memoryBRAM DIN" *)
     input [35:0]  io_memoryBRAM_dataIn, // @[:@3430.4]
     (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 memoryBRAM DOUT" *)
     output [35:0] io_memoryBRAM_dataOut, // @[:@3430.4]
     (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 stateBRAM CLK" *)
     input         io_stateBRAM_clk, // @[:@3430.4]
     (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 stateBRAM RST" *)
     input         io_stateBRAM_rst // @[:@3430.4]
     (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 stateBRAM WE" *)
     input         io_stateBRAM_writeEn, // @[:@3430.4]
     (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 stateBRAM EN" *)
     input         io_stateBRAM_en, // @[:@3430.4]
     (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 stateBRAM ADDR" *)
     input [9:0]   io_stateBRAM_addr, // @[:@3430.4]
     (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 stateBRAM DIN" *)
     input [35:0]  io_stateBRAM_dataIn, // @[:@3430.4]
     (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 stateBRAM DOUT" *)
     output [35:0] io_stateBRAM_dataOut, // @[:@3430.4]
     */
    // Debug
    val procStateDBG = if(cfg.DebugSignals) Some(new ProcStateDBG) else None
  })

  io.memoryBRAM <> proc.io.memoryBRAM
  val revDI = reverseEndianess(io.memoryBRAM.DI,      cfg.bramConfigMem.COL_WIDTH, cfg.bramConfigMem.NB_COL)
  val revDO = reverseEndianess(proc.io.memoryBRAM.DO, cfg.bramConfigMem.COL_WIDTH, cfg.bramConfigMem.NB_COL)
  proc.io.memoryBRAM.DI := revDI
  io.memoryBRAM.DO := revDO

  io.stateBRAM <> proc.io.stateBRAM
  io.axiLite <> regFile.io.axiLite

  // 0 -> PulseOnly, 1-7 -> ReadOnly, 8-16->Rd/Wr
  val regValues = WireInit(VecInit(Seq.fill(cfgAxiMM.nbrReg)(0.U(AxiLiteConsts.dataWidth.W))))
  def regOut(reg:Int)(msb:Int, lsb:Int) = regFile.io.regsOutput(reg)(msb,lsb)
  def regIn(reg:Int) = regValues(reg)
  regFile.io.regsInput := regValues

  /** Register 0 (Pulse-Only)
    * +-----------------------+-------------------------------------------------+
    * |                   Transplant Commands HOST->FPGA                        |
    * |-----------+-----------+---------------+-------------+-------------------+
    * |   fire    | getState  |   fillTLB     |   TLB.isWr  |        Tag        |
    * +-----------+-----------+---------------+-------------+-------------------+
    * |31       31|30       30|29           29|28         28| NB_THREADS-1     0|
    * +-----------+-----------+---------------+-------------+-------------------+
    *
    */
  val tagReg = RegNext(regOut(0)(cfg.NB_THREADS, 0))
  val fireReg = RegNext(regOut(0)(31, 31).asBool)
  proc.io.host2tpu.fire.valid := fireReg
  proc.io.host2tpu.fire.tag := tagReg

  val getState = RegNext(regOut(0)(30, 30).asBool)
  proc.io.host2tpu.getState.valid := getState
  proc.io.host2tpu.getState.tag := tagReg

  val fillTLB_fire = regOut(0)(29, 29).asBool
  val fillTLB_isWr = regOut(0)(28, 28).asBool

  /** Register 1 (Read-Only)
    * +---------------------------------+
    * |      to Host Cmds               |
    * +-------------------+-------------+
    * |    TLB Miss       |   done      |
    * +-------------------+-------------+
    * |31              16 | 15         0|
    * +-------------------+-------------+
    *
    */
  val doneVec = RegInit(VecInit(Seq.fill(cfg.NB_THREADS)(false.B)))
  val tlbMiss = WireInit(proc.io.host2tpu.missTLB.valid)
  regIn(1) := Cat(0.U, tlbMiss.asUInt, 0.U((16-cfg.NB_THREADS).W), doneVec.asUInt)

  when(fireReg) {
    doneVec(tagReg) := false.B
  }
  when(proc.io.host2tpu.done.valid) {
    doneVec(proc.io.host2tpu.done.tag) := true.B
  }

  /** Register 2 (Read-Only)
    * +----------------------------------------+
    * |                                        |
    * +----------------------------------------+
    * |             RESERVED                   |
    * +----------------------------------------+
    * |31                                     0|
    * +----------------------------------------+
    *
    */

  /** Register 4 (Read-Only)            Register 3 (Read-Only)
    * +------------------------------++-----------------------+
    * |                              ||                       |
    * +------------------------------++-----------------------+
    * |       Miss vaddr(63,32)      ||     Miss vaddr(31,0)  |
    * +------------------------------++-----------------------+
    * |31                           0||31                    0|
    * +------------------------------++-----------------------+
    *
    */
  val missTLB_vaddr = RegInit(DATA_X)
  regIn(3) := missTLB_vaddr(31,0)
  regIn(4) := missTLB_vaddr(63,32)

  when(proc.io.host2tpu.missTLB.valid) {
    missTLB_vaddr := proc.io.host2tpu.missTLB.bits.get.vaddr
  }

  /** Register 5 (ReadOnly)
    * +----------------------------------------+
    * |                                        |
    * +----------------------------------------+
    * |             TLB Entry                  |
    * +----------------------------------------+
    * |31                                     0|
    * +----------------------------------------+
    *
    */
  val missTLB_tlbIdx = RegInit(0.U(32.W))
  regIn(5) := missTLB_tlbIdx
  when(proc.io.host2tpu.missTLB.valid) {
    missTLB_tlbIdx := proc.io.host2tpu.missTLB.bits.get.tlbIdx
  }

  /** Register 9            Register 8
    * +----------------++---------------------+
    * |                ||                     |
    * +----------------++---------------------+
    * | vaddr(63,32)   ||    vaddr(31,0)      |
    * +----------------++---------------------+
    * |31             0||31                  0|
    * +----------------++---------------------+
    *
    */ // reg(3, 0, 1)
  /** Register 10
    * +----------------------------+
    * |             tlbIndex       |
    * +----------------------------+
    * |                            |
    * +----------------------------+
    * |31                         0|
    * +----------------------------+
    *
    */
  val fillTLB_valid = fillTLB_fire // Check Reg 0 Pulse Only
  val fillTLB_vaddr = WireInit(Cat(regOut(9)(31, 0), regOut(8)(31,0)))
  val fillTLB_tlbIdx = WireInit(regOut(10)(31,0))
  val fillTLB_wire = Wire(Valid(new TLBFill))
  fillTLB_wire.valid := fillTLB_valid
  fillTLB_wire.bits.vaddr := fillTLB_vaddr.asUInt
  fillTLB_wire.bits.tlbIdx := fillTLB_tlbIdx
  fillTLB_wire.bits.tlbEntry.valid := true.B
  fillTLB_wire.bits.tlbEntry.wrEn := fillTLB_isWr
  fillTLB_wire.bits.tlbEntry.tag := TLBEntry.getTLBtag(fillTLB_vaddr)
  val fillTLB = RegNext(fillTLB_wire)

  proc.io.host2tpu.fillTLB := fillTLB

  // Performance Counters
  /**     Reg 16        Reg 17         Reg 18       Reg 19    
    * +------------++------------++------------++------------+
    * |   Commit   || Inst Stall ||  Mem Stall ||   Branch   |
    * +------------++------------++------------++------------+
    * |31         0||31         0||31         0||31         0|
    * +------------++------------++------------++------------+
    *
    */ 
  /**     Reg 20        Reg 21         Reg 22       Reg 23    
    * +------------++------------++------------++------------+
    * |   Commit   || Inst Stall ||  Mem Stall ||   Branch   |
    * +------------++------------++------------++------------+
    * |31         0||31         0||31         0||31         0|
    * +------------++------------++------------++------------+
    *
    */ 
   
   val perfStats = Reg(new PerfStats)
   perfStats := proc.io.perfStats
   regIn(16) := perfStats.cntCommit      
   regIn(17) := perfStats.cntInstStall   
   regIn(18) := perfStats.cntMemStall    
   regIn(19) := perfStats.cntBranch      
   regIn(20) := perfStats.cntFlush       
   regIn(21) := perfStats.cntTransplants 
   regIn(22) := perfStats.cntUndefInsts  
   regIn(23) := perfStats.cntException   

   /**     Reg 32
    *  +------------+
    *  |   Reset    |
    *  +----+-------+
    *  |31  |7     0|
    *  +----+-------+
    */
   proc.io.resetStats := regOut(24)(7,0)

  // DEBUG Signals ------------------------------------------------------------
  if(cfg.DebugSignals) {
    proc.io.procStateDBG.get <> io.procStateDBG.get
  }
}
