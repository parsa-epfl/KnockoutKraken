package protoflex

import chisel3._
import chisel3.util.{Cat}
import common.AxiLite._
import common.PROCESSOR_TYPES._
import common._

object AxiDriver extends App {
  chisel3.Driver.execute(Array("-tn", "ProcAxi", "-td", "Verilog", "-fsm"), () => new ProcAxiWrap()(new ProcConfig(2, false)))
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

  val readOnly  = "0100".map(str2bool)
  val pulseOnly = "1000".map(str2bool)
  val cfgAxiMM = new AxiMemoryMappedRegFileConfig(4, readOnly, pulseOnly)

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

  // 0 -> RDONLY, 1 -> Pulse, 2 -> CTRL, 3 -> CTRL
  val regValues = WireInit(VecInit(Seq.fill(cfgAxiMM.nbrReg)(0.U(AxiLiteConsts.dataWidth.W))))
  def regOut(reg:Int, offst:Int, size:Int) = regFile.io.regsOutput(reg)(offst+(size-1),offst)
  def regIn(reg:Int) = regValues(reg)
  regFile.io.regsInput := regValues

  /** Register 0 (Pulse-Only)
    * +-----------------------+----------------------------------------+
    * |                             to Transplant Cmds                 |
    * |-----------+-----------+--------------------+-------------------+
    * |   fire    | getState  |      RESERVED      |        Tag        |
    * +-----------+-----------+--------------------+-------------------+
    * |31       31|30       30|30        NB_THREADS|NB_THREADS-1      0|
    * +-----------+-----------+--------------------+-------------------+
    *
    */
  val tagReg = regOut(0, 0, cfg.NB_THREADS)

  val fireReg    = regOut(0, 31, 1).asBool
  proc.io.host2tpu.fire.valid := fireReg
  proc.io.host2tpu.fire.tag := tagReg

  val getState = regOut(0, 30, 1).asBool()
  proc.io.host2tpu.getState.valid := getState
  proc.io.host2tpu.getState.tag := tagReg



  /** Register 1 (Read-Only)
    * +----------------------------------------+
    * |          to Host Cmds                  |
    * +-----------------+----------------------+
    * |    RESERVED     |        done          |
    * +-----------------+----------------------+
    * |31      NB_THREAD|NB_THREADS-1         0|
    * +-----------------+----------+-----------+
    *
    */
  val doneVec = RegInit(VecInit(Seq.fill(cfg.NB_THREADS)(false.B)))
  regIn(1):= Cat(0.U, doneVec.asUInt)

  when(fireReg) {
    doneVec(tagReg) := false.B
  }
  when(proc.io.host2tpu.done.valid) {
    doneVec(proc.io.host2tpu.done.tag) := true.B
  }

  /** Register 2 TODO, CMD get TLB paddr
    * +----------------------------------------+
    * |                                        |
    * +----------------------------------------+
    * |             RESERVED                   |
    * +----------------------------------------+
    * |31                                     0|
    * +----------------------------------------+
    *
    */
  // reg(2, 0, 1)
  proc.io.host2tpu.fillTLB := DontCare

  /** Register 3
    * +----------------------------------------+
    * |                                        |
    * +----------------------------------------+
    * |             PC(31,0)                   |
    * +----------------------------------------+
    * |31                                     0|
    * +----------------------------------------+
    *
    */
  /** Register 3
    * +----------------------------------------+
    * |                                        |
    * +----------------------------------------+
    * |             PC(63,32)                  |
    * +----------------------------------------+
    * |31                                     0|
    * +----------------------------------------+
    *
    */ // reg(3, 0, 1)
  /** Register 3
    * +----------------------------------|-----+
    * |                                  | is  |
    * +----------------------------------|-----+
    * |             IS MISS              | Miss|
    * +----------------------------------|-----+
    * |31                                |  0  |
    * +----------------------------------|-----+
    *
    */

  // DEBUG Signals ------------------------------------------------------------
  if(cfg.DebugSignals) {
    proc.io.procStateDBG.get <> io.procStateDBG.get
    proc.io.host2tpu.fillTLB <> io.procStateDBG.get.fillTLB
    proc.io.host2tpu.missTLB <> io.procStateDBG.get.missTLB
  }
}
