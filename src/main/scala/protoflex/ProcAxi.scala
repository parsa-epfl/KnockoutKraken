package protoflex

import chisel3._
import chisel3.util.{Cat}
import common.{AxiMemoryMappedRegFile, AxiMemoryMappedRegFileConfig}
import common.AxiLite._
import common.{BRAM, BRAMConfig, BRAMPort, BRAMPortAXI}
import common.PROCESSOR_TYPES._
import common.AxiLiteConsts

object AxiDriver extends App {
  chisel3.Driver.execute(Array("-tn", "ProcAxi", "-td", "Verilog", "-fsm"), () => new ProcAxiWrap()(new ProcConfig(2, false)))
}

class ProcAxiWrap(implicit val cfg: ProcConfig) extends MultiIOModule {
  def str2bool(c:Char):Boolean = c match {
    case '0' => false
    case '1' => true
  }

  val readOnly  = "0100".map(str2bool)
  val pulseOnly = "1000".map(str2bool)
  val cfgAxiMM = new AxiMemoryMappedRegFileConfig(4, readOnly, pulseOnly)

  val regFile  = Module(new AxiMemoryMappedRegFile()(cfgAxiMM))
  val proc = Module(new Proc())

  val io = IO(new Bundle {
                val axiLite = AxiLiteSlave(cfgAxiMM.axiLiteConfig)

                val ppageBRAM = new BRAMPortAXI(0)(cfg.ppageBRAMc)
                val stateBRAM = new BRAMPortAXI(0)(cfg.stateBRAMc)

                /* To Infer bram port, add these attributes to ports in the following manner

                 (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 ppageBRAM CLK" *)
                 input         io_ppageBRAM_clk, // @[:@3430.4]
                 (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 ppageBRAM RST" *)
                 input         io_ppageBRAM_rst, // @[:@3430.4]
                 (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 ppageBRAM WE" *)
                 input         io_ppageBRAM_writeEn, // @[:@3430.4]
                 (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 ppageBRAM EN" *)
                 input         io_ppageBRAM_en, // @[:@3430.4]
                 (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 ppageBRAM ADDR" *)
                 input [9:0]   io_ppageBRAM_addr, // @[:@3430.4]
                 (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 ppageBRAM DIN" *)
                 input [35:0]  io_ppageBRAM_dataIn, // @[:@3430.4]
                 (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 ppageBRAM DOUT" *)
                 output [35:0] io_ppageBRAM_dataOut, // @[:@3430.4]
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

  io.ppageBRAM <> proc.io.ppageBRAM
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
  proc.io.host2tpu.fillTLB.valid    := false.B
  proc.io.host2tpu.fillTLB.tag      := tagReg
  proc.io.host2tpu.fillTLB.data.get := TLBEntry()
  // reg(2, 0, 1)

  /** Register 3
    * +----------------------------------------+
    * |                                        |
    * +----------------------------------------+
    * |             RESERVED                   |
    * +----------------------------------------+
    * |31                                     0|
    * +----------------------------------------+
    *
    */
  // reg(3, 0, 1)

  // DEBUG Signals ------------------------------------------------------------
  if(cfg.DebugSignals) {
    io.procStateDBG.get <> proc.io.procStateDBG.get
  }
}
