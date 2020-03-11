package legacy

import chisel3._
import chisel3.experimental._
import chisel3.util._
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
    }

    def rdBRAM32b(offst:BigInt): BigInt = {
      target.EN.poke(true.B)
      target.WE.poke(false.B)
      if(target.cfg.isAXI) {
        target.ADDR.poke((offst << 2).U)
      } else {
        target.ADDR.poke(offst.U)
      }
      clock.step()
      target.EN.poke(false.B)
      val uint32 = target.DO.peek
      return uint32.litValue()
    }

    def wrBRAM64b(lw: BigInt, offst: BigInt) = if(target.cfg.DATA_WIDTH == 72) {
      target.EN.poke(true.B)
      target.WE.poke(true.B)
      if(target.cfg.isAXI) {
        target.ADDR.poke((offst << 2).U)
      } else {
        target.ADDR.poke(offst.U)
      }
      target.DI.poke(lw.U)
      clock.step()
      target.EN.poke(false.B)
      target.WE.poke(false.B)
    } else {
      val bytes = Array.fill(8)(0.toByte)
      for( i <- 0 until 8 ) bytes(i) = ((lw >> ((7-i) * 8)) & 0xFF).toByte
      val msb = BigInt(Array(0.toByte) ++ bytes.slice(0, 4))
      val lsb = BigInt(Array(0.toByte) ++ bytes.slice(4, 8))
      wrBRAM32b(msb, offst)
      wrBRAM32b(lsb, offst+1)
    }

    def rdBRAM64b(offst:BigInt): BigInt = if(target.cfg.DATA_WIDTH == 72) {
      target.EN.poke(true.B)
      target.WE.poke(false.B)
      if(target.cfg.isAXI) {
        target.ADDR.poke((offst << 2).U)
      } else {
        target.ADDR.poke(offst.U)
      }
      clock.step()
      target.EN.poke(false.B)
      target.ADDR.poke(0.U)
      val uint64 = target.DO.peek
      return uint64.litValue()
    } else {
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

  implicit class BRAMPortDriver_FULL(target: BRAMPort_FULL)(implicit clock: Clock) {
    def wrBRAM(bits:BigInt, offst: BigInt) = {
      target.EN.poke(true.B)
      target.WE.poke("b11111111".U) // Assumes 64bit
      if(target.cfg.isAXI) {
        target.ADDR.poke((offst << 2).U)
      } else {
        target.ADDR.poke(offst.U)
      }
      target.DI.poke(bits.U)
      clock.step()
      target.EN.poke(false.B)
      target.WE.poke(0.U)
    }

    def wrBRAM(bits:BigInt, offst: BigInt, strobe: BigInt) = {
      target.EN.poke(true.B)
      target.WE.poke(strobe.U)
      if(target.cfg.isAXI) {
        target.ADDR.poke((offst << 2).U)
      } else {
        target.ADDR.poke(offst.U)
      }
      target.DI.poke(bits.U)
      clock.step()
      target.EN.poke(false.B)
      target.WE.poke(0.U)
    }

    def rdBRAM(offst:BigInt): BigInt = {
      target.EN.poke(true.B)
      target.WE.poke(false.B)
      if(target.cfg.isAXI) {
        target.ADDR.poke((offst << 2).U)
      } else {
        target.ADDR.poke(offst.U)
      }
      clock.step()
      target.EN.poke(false.B)
      val uint32 = target.DO.peek
      return uint32.litValue()
    }
  }
}

case class BRAMConfig(
  C_ADDR_WIDTH: Int = 10,
  val DATA_WIDTH: Int = 36,
  val isAXI: Boolean = false, // AXI is byte addressed
  val isRegistered: Boolean = true
) {
  private val ratio = (DATA_WIDTH/36).ceil.toInt
  private val extraBits = log2Floor(ratio)
  val ADDR_WIDTH = if(isAXI) (C_ADDR_WIDTH + 2) - extraBits else C_ADDR_WIDTH - extraBits

  // Clone function with set AXI ports
  def apply(isAXI: Boolean): BRAMConfig = new BRAMConfig(C_ADDR_WIDTH, DATA_WIDTH, isAXI, isRegistered)
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
  private val bramTDP = Module(new BRAMTDP(cfg.ADDR_WIDTH, cfg.DATA_WIDTH))

  bramTDP.io.clk <> this.clock.asUInt
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
class BRAMTDP(val ADDR_WIDTH: Int = 10, val DATA_WIDTH: Int = 36, outputReg: Boolean = true) //extends Module {
    extends BlackBox(Map(
      "ADDR_SIZE"-> ADDR_WIDTH,
      "DATA_SIZE" -> DATA_WIDTH))
    with HasBlackBoxInline {
  val SIZE = (1 << ADDR_WIDTH)*36/DATA_WIDTH
  val io = IO(new Bundle(){
    val clk = Input(Bool())
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

  /* This logic would enable simulations with Treadle, but it's too slow for full system
  val ram = Mem(SIZE, UInt(DATA_WIDTH.W))

  def genReg[T <: Data] (isReg: Boolean, gen: T) = if(isReg) Reg(gen) else Wire(gen)
  val readA = genReg(outputReg, UInt(DATA_WIDTH.W));
  val readB = genReg(outputReg, UInt(DATA_WIDTH.W));

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
  // */
  //*
  setInline("BRAMTDP.v",
    s"""
      |// Dual-Port Block RAM with Two Write Ports
      |
      |module BRAMTDP #(
      |                         parameter ADDR_SIZE = ${ADDR_WIDTH},
      |                         parameter DATA_SIZE = ${DATA_WIDTH})
      |   (clk,enA,enB,weA,weB,addrA,addrB,diA,diB,doA,doB);
      |
      |   input clk,enA,enB,weA,weB;
      |   input [ADDR_SIZE-1:0] addrA;
      |   input [ADDR_SIZE-1:0] addrB;
      |   input [DATA_SIZE-1:0] diA;
      |   input [DATA_SIZE-1:0] diB;
      |   output [DATA_SIZE-1:0] doA;
      |   output [DATA_SIZE-1:0] doB;
      |
      |   (* ram_style = "block" *) reg [DATA_SIZE-1:0] ram [0:2**ADDR_SIZE-1];
      |   reg [DATA_SIZE-1:0] readA;
      |   reg [DATA_SIZE-1:0] readB;
      |   wire [DATA_SIZE-1:0] readingA;
      |   wire [DATA_SIZE-1:0] readingB;
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
      |   assign readingA = ram[addrA];
      |   assign readingB = ram[addrB];
      |   assign doA = readA;
      |   assign doB = readB;
      |
      |endmodule
  """.stripMargin)
 // */
}
