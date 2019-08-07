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

  val readOnly  = "0001".map(str2bool)
  val pulseOnly = "0010".map(str2bool)
  val cfgAxiMM = new AxiMemoryMappedConfig(4, readOnly, pulseOnly)
  val regFile  = Module(new AxiMemoryMappedWithRegs()(cfgAxiMM))
  val proc = Module(new Proc())

  val io = IO(new Bundle {
                val axiLite = AxiLiteSlave(cfgAxiMM.axiLiteConfig)
                val ppageBRAM = new BRAMPort(0)(cfg.ppageBRAMc)
                val stateBRAM = new BRAMPort(0)(cfg.stateBRAMc)
              })
  io.ppageBRAM <> proc.io.ppageBRAM
  io.stateBRAM <> proc.io.stateBRAM

  // 0 -> RDONLY, 1 -> Pulse, 2 -> CTRL, 3 -> CTRL
  def reg(reg:Int, offst:Int, size:Int) = regFile.io.regsValues(reg)(offst+(size-1),offst)

  val host2tp = Wire(new TransplantUnitHostIO)
  /** Register 0 (Pulse-Only)
    * +----------------------------------------------------+
    * |                 to Transplant Cmds                 |
    * |-----------+--------------------+-------------------+
    * |   fire    |      RESERVED      |        Tag        |
    * +-----------+--------------------+-------------------+
    * |31       31|30        NB_THREADS|NB_THREADS-1      0|
    * +-----------+--------------------+-------------------+
    *
    */
  proc.io.host2tp.fire    := RegNext(reg(0, 31, 1))
  proc.io.host2tp.fireTag := RegNext(reg(0, 0, cfg.NB_THREADS))

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
  reg(1, 0, cfg.NB_THREADS) := proc.io.host2tp.done

  /** Register 2
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
}
