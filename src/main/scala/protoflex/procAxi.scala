package protoflex

import chisel3._
import common.{AxiLiteSlave,AxiMemoryMappedConfig, AxiMemoryMappedWithRegs}
import common.{BRAM, BRAMConfig, BRAMPort}
import common.PROCESSOR_TYPES._

class procAxiWrap(implicit val cfg: ProcConfig) extends Module {
  def str2bool(c:Char):Boolean = c match {
    case '0' => false
    case '1' => true
  }

  val readOnly  = "0100".map(str2bool)
  val pulseOnly = "1000".map(str2bool)
  val cfgAxiMM = new AxiMemoryMappedConfig(4, readOnly, pulseOnly)
  val regFile  = Module(new AxiMemoryMappedWithRegs()(cfgAxiMM))
  val proc = Module(new Proc())

  val io = IO(new Bundle {
                val axiLite = AxiLiteSlave(cfgAxiMM.axiLiteConfig)
                val ppageBRAM = new BRAMPort(0)(cfg.ppageBRAMc)
                val stateBRAM = new BRAMPort(0)(cfg.stateBRAMc)
              })
  io.ppageBRAM <> proc.io.ppage_bram
  io.stateBRAM <> proc.io.state_bram

  // 0 -> CTRL, 1 -> CTRL, 2 -> RDONLY, 3 -> Pulse
  def reg(reg:Int, offst:Int, size:Int) = regFile.io.regsValues(reg)(offst+(size-1),offst)

  /**
    * +--------------+----------------------+
    * | RESERVED     |         COMP CTRL    |
    * |              +----------+-----------+
    * |              |   cmd2   |   start   |
    * +--------------+----------+-----------+
    * |31           1|x        x|0         0|
    * +--------------+----------+-----------+
    *
    */
  proc.io.tp_start := RegNext(reg(0,0,1))

  /**
    * +--------------+----------------------+
    * | RESERVED     |         COMP CTRL    |
    * |              +----------+-----------+
    * |              |   cmd2   |   start   |
    * +--------------+----------+-----------+
    * |31           1|x        x|0         0|
    * +--------------+----------+-----------+
    *
    */
  reg(3,0,1) := proc.io.tp_done
}
