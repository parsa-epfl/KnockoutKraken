package common

import chisel3._
import chiseltest._

object BRAMPort {
  implicit class BRAMPortDriver(target: BRAMPort)(implicit clock: Clock) {
    def wrBRAM32b(bits:BigInt, offst: BigInt) = {
      target.EN.poke(true.B)
      target.WE.poke(true.B)
      if(target.cfg.isAXI) {
        target.ADDR.poke((offst << 2).U)
      } else {
        target.ADDR.poke(offst.U)
      }
      target.DI.poke(bits.U)
      clock.step()
      target.EN.poke(false.B)
      target.WE.poke(false.B)
      target.ADDR.poke(0.U)
      target.DI.poke(0.U)
    }

    def rdBRAM32b(offst:BigInt): BigInt = {
      target.EN.poke(true.B)
      if(target.cfg.isAXI) {
        target.ADDR.poke((offst << 2).U)
      } else {
        target.ADDR.poke(offst.U)
      }
      clock.step()
      target.EN.poke(false.B)
      target.ADDR.poke(0.U)
      val uint32 = target.DO.peek
      return uint32.litValue()
    }

    def wrBRAM64b(lw: BigInt, offst: BigInt) = {
      val bytes = Array.fill(8)(0.toByte)
      for( i <- 0 to 7 ) bytes(i) = ((lw >> ((7-i) * 8)) & 0xFF).toByte
      val msb = BigInt(Array(0.toByte) ++ bytes.slice(0, 4))
      val lsb = BigInt(Array(0.toByte) ++ bytes.slice(4, 8))
      wrBRAM32b(msb, offst)
      wrBRAM32b(lsb, offst+1)
    }

    def rdBRAM64b(offst:BigInt): BigInt = {
      val msb = rdBRAM32b(offst)
      val lsb = rdBRAM32b(offst+1)
      val byte_msb = Array.fill(4)(0.toByte)
      val byte_lsb = Array.fill(4)(0.toByte)
      for (i <- 0 to 3) byte_msb(i) = ((msb >> ((3-i) * 8)) & 0xFF).toByte
      for (i <- 0 to 3) byte_lsb(i) = ((lsb >> ((3-i) * 8)) & 0xFF).toByte
      val uint64 = BigInt(Array(0.toByte) ++ byte_msb ++ byte_lsb) // Unsigned long
      return uint64
    }
  }
}

case class BRAMConfig(
  C_ADDR_WIDTH: Int = 10,
  val DATA_WIDTH: Int = 36,
  val SIZE: Int = 1024,
  val isAXI: Boolean = false
) {
  val ADDR_WIDTH = if(isAXI) (C_ADDR_WIDTH + 2) else C_ADDR_WIDTH

  // Clone function with set AXI ports
  def apply(isAXI: Boolean): BRAMConfig = new BRAMConfig(C_ADDR_WIDTH, DATA_WIDTH, SIZE, isAXI)
}

class BRAMPort(implicit val cfg: BRAMConfig) extends Bundle {
  val CLK = if(cfg.isAXI) Some(Input(Bool())) else None
  val RST = if(cfg.isAXI) Some(Input(Bool())) else None
  val EN = Input(Bool())
  val WE = Input(Bool())
  val ADDR = Input(UInt(cfg.ADDR_WIDTH.W))
  val DI = Input(UInt(cfg.DATA_WIDTH.W))
  val DO = Output(UInt(cfg.DATA_WIDTH.W))
  def <>(other: BRAMPort) = {
    if(cfg.isAXI) CLK.get <> other.CLK.getOrElse(DontCare)
    if(cfg.isAXI) RST.get <> other.RST.getOrElse(DontCare)
    EN <> other.EN
    WE <> other.WE
    DI <> other.DI
    DO <> other.DO
    (cfg.isAXI, other.cfg.isAXI) match {
      case (true,false) => (ADDR >> 2.U) <> other.ADDR
      case (false,true) => ADDR <> (other.ADDR >> 2.U)
      case (true,true)  => ADDR <> other.ADDR
      case (false,false) => ADDR <> other.ADDR
    }
  }
}

class BRAM(implicit cfg: BRAMConfig) extends MultiIOModule {
  val portA = IO(new BRAMPort)
  val portB = IO(new BRAMPort)
  private val bramTDP = Module(new BRAMTDP(cfg.ADDR_WIDTH, cfg.DATA_WIDTH, cfg.SIZE))

  bramTDP.io.enA <> portA.EN
  bramTDP.io.weA <> portA.WE
  bramTDP.io.addrA <> portA.ADDR
  bramTDP.io.diA <> portA.DI
  bramTDP.io.doA <> portA.DO

  bramTDP.io.enB <> portB.EN
  bramTDP.io.weB <> portB.WE
  bramTDP.io.addrB <> portB.ADDR
  bramTDP.io.diB <> portB.DI
  bramTDP.io.doB <> portB.DO
}

// Inspired from inline string
class BRAMTDP(val ADDR_WIDTH: Int = 10, val DATA_WIDTH: Int = 36, val SIZE: Int = 1024) extends Module {
  val io = IO(new Bundle(){
    val enA = Input(Bool())
    val enB = Input(Bool())
    val weA = Input(Bool())
    val weB = Input(Bool())
    val addrA = Input(UInt(ADDR_WIDTH.W))
    val addrB = Input(UInt(ADDR_WIDTH.W))
    val diA = Input(UInt(DATA_WIDTH.W))
    val diB = Input(UInt(DATA_WIDTH.W))
    val doA = Output(UInt(DATA_WIDTH.W))
    val doB = Output(UInt(DATA_WIDTH.W))
  })

  val ram = Mem(SIZE, UInt(DATA_WIDTH.W))

  val readA = RegInit(0.U(DATA_WIDTH.W));
  val readB = RegInit(0.U(DATA_WIDTH.W));

  when(io.enA) {
    readA := ram(io.addrA)
    when(io.weA) {
      ram(io.addrA) := io.diA
    }
  }

  when(io.enB) {
    readB := ram(io.addrB)
    when(io.weB) {
      ram(io.addrB) := io.diB
    }
  }

  io.doA := readA;
  io.doB := readB;

  private val setInLineBlackBoxTDPInfer = (
    s"BlackBoxBRAMTDP.v",
    s"""
      |// Dual-Port Block RAM with Two Write Ports
      |
      |module BlackBoxBRAMTDP #(
      |                         parameter ADDR_WIDTH_A = ${ADDR_WIDTH},
      |                         parameter DATA_WIDTH_A = ${DATA_WIDTH},
      |                         parameter ADDR_WIDTH_B = ${ADDR_WIDTH},
      |                         parameter DATA_WIDTH_B = ${DATA_WIDTH},
      |                         parameter SIZE_A       = ${SIZE},
      |                         parameter SIZE_B       = ${SIZE})
      |   (clk,enA,enB,weA,weB,addrA,addrB,diA,diB,doA,doB);
      |
      |   input clk,enA,enB,weA,weB;
      |   input [ADDR_WIDTH_A-1:0] addrA;
      |   input [ADDR_WIDTH_B-1:0] addrB;
      |   input [DATA_WIDTH_A-1:0] diA;
      |   input [DATA_WIDTH_B-1:0] diB;
      |   output [DATA_WIDTH_A-1:0] doA;
      |   output [DATA_WIDTH_B-1:0] doB;
      |
      |`define max(a,b) {(a) > (b) ? (a) : (b)}
      |`define min(a,b) {(a) < (b) ? (a) : (b)}
      |
      |   function integer log2;
      |      input integer          value;
      |      reg [31:0]             shifted;
      |      integer                res;
      |      begin
      |         if (value < 2)
      |  	       log2 = value;
      |         else
      |           begin
      |  	          shifted = value-1;
      |  	          for (res=0; shifted>0; res=res+1)
      |  		          shifted = shifted>>1;
      |  	          log2 = res;
      |           end
      |      end
      |   endfunction
      |
      |   localparam maxSIZE  = `max(SIZE_A, SIZE_B);
      |   localparam maxWIDTH = `max(DATA_WIDTH_A, DATA_WIDTH_B);
      |   localparam minWIDTH = `min(DATA_WIDTH_A, DATA_WIDTH_B);
      |
      |   localparam RATIO = maxWIDTH / minWIDTH;
      |   localparam log2RATIO = log2(RATIO);
      |
      |   reg[minWIDTH-1:0] ram [0:maxSIZE-1];
      |   reg [DATA_WIDTH_A-1:0] readA;
      |   reg [DATA_WIDTH_B-1:0] readB;
      |
      |   always @(posedge clk)
      |     begin
      |        if (enB)
      |          begin
      |             if (weB)
      |               ram[addrB] <= diB;
      |             readB <= ram[addrB];
      |          end
      |     end
      |
      |   always @(posedge clk)
      |     begin
      |        if (enA)
      |          begin
      |             if (weA)
      |               ram[addrA] <= diA;
      |             readA <= ram[addrA];
      |          end
      |     end
      |
      |   assign doA = readA;
      |   assign doB = readB;
      |
      |endmodule
  """.stripMargin)
}

object BRAMConfig {
  // Possible configurations Xilinx Ultrascale BRAM
  // 18 TDP
  //  (ADDR_WIDTH, DATA_WIDTH, SIZE)
  val BRAM18TD1A14     = BRAMConfig(1,  14, 16384)
  val BRAM18TD2A13     = BRAMConfig(2,  13, 8192)
  val BRAM18TD4A12     = BRAMConfig(4,  12, 4096)
  val BRAM18TD9A11     = BRAMConfig(9,  11, 2048)
  val BRAM18TD18A10    = BRAMConfig(18, 10, 1024)

  // 36 TDP
  val BRAM36TD1A15     = BRAMConfig(1,  15, 32768)
  val BRAM36TD2A14     = BRAMConfig(2,  14, 16384)
  val BRAM36TD4A13     = BRAMConfig(4,  13, 8192)
  val BRAM36TD9A12     = BRAMConfig(9,  12, 4096)
  val BRAM36TD18A11    = BRAMConfig(18, 11, 2048)
  val BRAM36TD36A10    = BRAMConfig(36, 10, 1024)
}

