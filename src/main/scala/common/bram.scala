/** Credits to my friend Laurier Loiselle
  *  source: https://github.com/laurierloi/fpgabits/
  */

package common

import scala.language.reflectiveCalls

import java.nio.ByteBuffer

import scala.language.reflectiveCalls

import chisel3._
import chisel3.tester._
import chisel3.util._
//import chisel3.experimental._

object constBRAM {
  /** From table 1-7,1-8,1-9,1-10 in UG573
    * Nomenclature: 18 or 36 for BRAM type
    *               S/T,  for (S)imple/(T)rue Dual Port
    *               Dn, where n is data width
    *               An, where n is address width
    *    (optional) Wn, where n is the alternate port witdth(only for SDP)
    *    (optional) C,  for cascade implementation
    */

  /* Fix the index of the write and read ports for the sdp */
  val sdpWritePort :: sdpReadPort = Enum(2)

  /* PORT PARAM DEFINITIONS */
  /* 18 TDP */
  //  (addrWidth, dataWidth, size)
  val BRAM18TD1A14     = BRAMPortParam(14, 1,  16384)
  val BRAM18TD2A13     = BRAMPortParam(13, 2,  8192)
  val BRAM18TD4A12     = BRAMPortParam(12, 4,  4096)
  val BRAM18TD9A11     = BRAMPortParam(11, 9,  2048)
  val BRAM18TD18A10    = BRAMPortParam(10, 18, 1024)

  /* 36 TDP */
  val BRAM36TD1A15     = BRAMPortParam(15, 1,  32768)
  val BRAM36TD2A14     = BRAMPortParam(14, 2,  16384)
  val BRAM36TD4A13     = BRAMPortParam(13, 4,  8192)
  val BRAM36TD9A12     = BRAMPortParam(12, 9,  4096)
  val BRAM36TD18A11    = BRAMPortParam(11, 18, 2048)
  val BRAM36TD36A10    = BRAMPortParam(10, 36, 1024)
  //val BRAM36TD1A16C    = BRAMPortParam(16, 1,  65546)

  // Dict key is the data width
  val TDPBRAM18AvailableWidths = Seq(1, 2, 4, 9, 18)
  val TDPBRAM36AvailableWidths = Seq(1, 2, 4, 9, 18, 36)
  val TDPBRAM18ParamDict = Map[Int , BRAMPortParam](
    1 -> BRAM18TD1A14 , 2   -> BRAM18TD2A13  , 4  -> BRAM18TD4A12 ,
    9 -> BRAM18TD9A11 , 18  -> BRAM18TD18A10)
  val TDPBRAM36ParamDict = Map[Int , BRAMPortParam](
    1 -> BRAM36TD1A15 , 2   -> BRAM36TD2A14  , 4  ->  BRAM36TD4A13 ,
    9 -> BRAM36TD9A12 , 18  -> BRAM36TD18A11 , 36 -> BRAM36TD36A10)

  def getTDPClosestWidth(width: Int, isBRAM18: Boolean): Int = {
    if(isBRAM18){ TDPBRAM18AvailableWidths.filter(_ >= width)(0) }
    else {        TDPBRAM36AvailableWidths.filter(_ >= width)(0) } }
  def getTDPBRAMPortParam(width: Int, isBRAM18: Boolean): BRAMPortParam = {
    if(isBRAM18) { TDPBRAM18ParamDict(width) }
    else         { TDPBRAM36ParamDict(width) } }


  /* 18 SDP */
  val BRAM18SD32A14W1  = new BRAMConfig(false, Seq(9, 14), Seq(32, 1 ), Seq(512, 16384))
  val BRAM18SD32A13W2  = new BRAMConfig(false, Seq(9, 13), Seq(32, 2 ), Seq(512, 8192 ))
  val BRAM18SD32A12W4  = new BRAMConfig(false, Seq(9, 12), Seq(32, 4 ), Seq(512, 4096 ))
  val BRAM18SD36A11W9  = new BRAMConfig(false, Seq(9, 11), Seq(36, 9 ), Seq(512, 2048 ))
  val BRAM18SD36A10W18 = new BRAMConfig(false, Seq(9, 10), Seq(36, 18), Seq(512, 1024 ))
  val BRAM18SD36A10W36 = new BRAMConfig(false, Seq(9, 10), Seq(36, 36), Seq(512, 512  ))

  /* 36 SDP */
  val BRAM36SD64A15W1  = new BRAMConfig(false, Seq(9, 15), Seq(64, 1 ), Seq(512, 32768))
  val BRAM36SD64A14W2  = new BRAMConfig(false, Seq(9, 14), Seq(64, 2 ), Seq(512, 16384))
  val BRAM36SD64A13W4  = new BRAMConfig(false, Seq(9, 13), Seq(64, 4 ), Seq(512, 8192 ))
  val BRAM36SD72A12W9  = new BRAMConfig(false, Seq(9, 12), Seq(72, 9 ), Seq(512, 4096 ))
  val BRAM36SD72A11W18 = new BRAMConfig(false, Seq(9, 11), Seq(72, 18), Seq(512, 2048 ))
  val BRAM36SD72A10W36 = new BRAMConfig(false, Seq(9, 10), Seq(72, 36), Seq(512, 1024 ))
  val BRAM36SD72A9W72  = new BRAMConfig(false, Seq(9, 9 ), Seq(72, 72), Seq(512, 512  ))

  val SDPBRAM18AvailableWidths = Seq(1, 2, 4, 9, 18, 36)
  val SDPBRAM36AvailableWidths = Seq(1, 2, 4, 9, 18, 36, 72)
  val SDPBRAM18ConfigDict = Map[Int, BRAMConfig](
    1  -> BRAM18SD32A14W1, 2  -> BRAM18SD32A13W2, 4  -> BRAM18SD32A12W4,
    9  -> BRAM18SD36A11W9, 18 -> BRAM18SD36A10W18, 36 -> BRAM18SD36A10W36)
  val SDPBRAM36ConfigDict = Map[Int, BRAMConfig](
    1  -> BRAM36SD64A15W1 , 2  -> BRAM36SD64A14W2 , 4  -> BRAM36SD64A13W4 ,
    9  -> BRAM36SD72A12W9 , 18 -> BRAM36SD72A11W18, 36 -> BRAM36SD72A10W36,
    72 -> BRAM36SD72A9W72)

}

case class BRAMPortParam(
  val addrWidth : Int,
  val dataWidth : Int,
  val size      : Int,
  val writePort : Boolean = true,
  val readPort  : Boolean = true
)

case class BRAMConfig(
  val portParamVec : Seq[BRAMPortParam],
  val debug        : Boolean = false
){
  def this(isTrueDualPort: Boolean, addrWidthVec: Seq[Int],
           dataWidthVec: Seq[Int], sizeVec: Seq[Int]) = {
    this(Seq(BRAMPortParam(addrWidthVec(0), dataWidthVec(0), sizeVec(0), true, !isTrueDualPort),
             BRAMPortParam(addrWidthVec(1), dataWidthVec(1), sizeVec(1), !isTrueDualPort, true)))
  }
  val isTrueDualPort  = portParamVec.map{case x => x.writePort & x.readPort}.foldLeft(true)(_ && _)
  val addrWidthVec    = portParamVec.map{_.addrWidth}
  val dataWidthVec    = portParamVec.map{_.dataWidth}
  val sizeVec         = portParamVec.map{_.size}

  val readLatency = 2
  val nbrPort     = 2
  val sdpWritePortIdx = 0
  val sdpReadPortIdx  = 1
  val writePortVec = portParamVec.map{_.writePort}
  val readPortVec  = portParamVec.map{_.readPort}
  val widthRatio = dataWidthVec(1) / dataWidthVec(0)

  def dataGen(idx: Int): UInt = UInt(dataWidthVec(idx).W)
  def addrGen(idx: Int): UInt = UInt(addrWidthVec(idx).W)

  override def toString: String =
    f"(tdp: $isTrueDualPort" +
      f", addr0W: ${addrWidthVec(0)}%2s, data0W: ${dataWidthVec(0)}%2s, size0: ${sizeVec(0)}%6s" +
      f", addr1W: ${addrWidthVec(1)}%2s, data1W: ${dataWidthVec(1)}%2s, size1: ${sizeVec(1)}%6s)"
}

class BRAMPort(private val idx: Int)(implicit val c: BRAMConfig) extends Bundle {
  val writeEn = if(c.writePortVec(idx)) Some(Input(Bool())) else None
  val en      = Input(Bool())
  val addr    = Input(c.addrGen(idx))
  val dataIn  = if(c.writePortVec(idx)) Some(Input( c.dataGen(idx))) else None
  val dataOut = if(c.readPortVec(idx))  Some(Output(c.dataGen(idx))) else None
}

class BRAMPortAXI(private val idx: Int)(implicit val c: BRAMConfig) extends Bundle {
  val CLK = Input(Bool())
  val RST = Input(Bool())
  val WE = Input(Bool())
  val EN = Input(Bool())
  val ADDR = Input(c.addrGen(idx))
  val DI = Input( c.dataGen(idx))
  val DO = Output(c.dataGen(idx))

  def <>(port_ : BRAMPort) {
    WE <> port_.writeEn.get
    EN <> port_.en
    DI <> port_.dataIn.get
    DO <> port_.dataOut.get
    ADDR <> port_.addr
  }
}

/** Note: This BRAM port is expected to be 32 bit wide for data
  *       And address of 1024 words
  */
object BRAMPort {
  implicit class BRAMPortDriver(target: BRAMPortAXI)(implicit clock: Clock) {
    def wrBRAM32b(bits:BigInt, offst: BigInt) = {
      target.EN.poke(true.B)
      target.WE.poke(true.B)
      target.ADDR.poke(offst.U)
      target.DI.poke(bits.U)
      clock.step()
      target.EN.poke(false.B)
      target.WE.poke(false.B)
      target.ADDR.poke(0.U)
      target.DI.poke(0.U)
    }

    def rdBRAM32b(offst:BigInt): BigInt = {
      target.EN.poke(true.B)
      target.ADDR.poke(offst.U)
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

class BRAMIO(implicit val c: BRAMConfig) extends Bundle {
  val portA = new BRAMPort(0)
  val portB = new BRAMPort(1)

  def getPort(idx: Int): BRAMPort = {
    if (idx == 0) { portA }
    else { portB }
  }
}

class BRAM(implicit val c: BRAMConfig) extends MultiIOModule {
  val io = IO(new BRAMIO)

  val bram = Module(
    //if(c.isTrueDualPort) {
      new BRAMTDP
    //} else {
    //  new BRAMSDP}
  )

  io <> bram.io
}

/* TRUE DUAL PORT */
class BRAMTDP(implicit val c: BRAMConfig) extends MultiIOModule {
  val io = IO(new BRAMIO)

  assert((c.dataWidthVec(0) >= c.dataWidthVec(1)),
         "To use Asymetric TDP BRAM, the size of port0" +
           " must be greater or equal to the size of port1")

  val blackBoxBRAMTDP = Module(new BlackBoxBRAMTDP)
  blackBoxBRAMTDP.io.clk := this.clock

  val portA = io.portA
  blackBoxBRAMTDP.io.weA   <> portA.writeEn.get
  blackBoxBRAMTDP.io.enA   <> portA.en
  blackBoxBRAMTDP.io.addrA <> portA.addr
  blackBoxBRAMTDP.io.diA   <> portA.dataIn.get
  blackBoxBRAMTDP.io.doA   <> portA.dataOut.get

  val portB = io.portB
  blackBoxBRAMTDP.io.weB   <> portB.writeEn.get
  blackBoxBRAMTDP.io.enB   <> portB.en
  blackBoxBRAMTDP.io.addrB <> portB.addr
  blackBoxBRAMTDP.io.diB   <> portB.dataIn.get
  blackBoxBRAMTDP.io.doB   <> portB.dataOut.get

  /*
   printf( p"weA: ${io.getPort(0).writeEn.get}" +
   p", enA: ${io.getPort(0).en}" +
   p", addrA: ${io.getPort(0).addr}" +
   p", diA: ${io.getPort(0).dataIn.get}" +
   p", doA: ${io.getPort(0).dataOut.get}" + "\n")
   printf( p"weB: ${io.getPort(1).writeEn.get}" +
   p", enB: ${io.getPort(1).en}" +
   p", addrB: ${io.getPort(1).addr}" +
   p", diB: ${io.getPort(1).dataIn.get}" +
   p", doB: ${io.getPort(1).dataOut.get}" + "\n")
   */
}

class BlackBoxBRAMTDP(implicit val c: BRAMConfig) extends BlackBox(
  Map("ADDR_WIDTH_A"   -> c.addrWidthVec(0),
      "DATA_WIDTH_A"   -> c.dataWidthVec(0),
      "ADDR_WIDTH_B"   -> c.addrWidthVec(1),
      "DATA_WIDTH_B"   -> c.dataWidthVec(1),
      "SIZE_A"         -> c.sizeVec(0),
      "SIZE_B"         -> c.sizeVec(1)))
    with HasBlackBoxInline {
  val io = IO(new Bundle(){
                val clk   = Input(Clock())
                val enA   = Input(Bool())
                val enB   = Input(Bool())
                val weA   = Input(Bool())
                val weB   = Input(Bool())
                val addrA = Input(UInt(c.addrWidthVec(0).W))
                val addrB = Input(UInt(c.addrWidthVec(1).W))
                val diA   = Input(UInt(c.dataWidthVec(0).W))
                val diB   = Input(UInt(c.dataWidthVec(1).W))
                val doA   = Output(UInt(c.dataWidthVec(0).W))
                val doB   = Output(UInt(c.dataWidthVec(1).W))
              })

  setInline(s"BlackBoxBRAMTDP.v",
            s"""
      |// Dual-Port Block RAM with Two Write Ports
      |
      |module BlackBoxBRAMTDP #(
      |                         parameter ADDR_WIDTH_A = ${c.addrWidthVec(0)},
      |                         parameter DATA_WIDTH_A = ${c.dataWidthVec(0)},
      |                         parameter ADDR_WIDTH_B = ${c.addrWidthVec(1)},
      |                         parameter DATA_WIDTH_B = ${c.dataWidthVec(1)},
      |                         parameter SIZE_A       = ${c.sizeVec(0)},
      |                         parameter SIZE_B       = ${c.sizeVec(1)})
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

/* SIMPLE DUAL PORT */
class BRAMSDP(implicit val c: BRAMConfig) extends MultiIOModule {
  val io = IO(new BRAMIO)

  assert((c.dataWidthVec(0) >= c.dataWidthVec(1)),
         "To use SDP BRAM, the size of write port (port0)" +
           " must be greater to the size of read port (port1)." +
           "\nThis module could support readPort > writePort" +
           ", but no use was found for it in the design")

  val blackBoxBRAMSDP = Module(new BlackBoxBRAMSDP)

  blackBoxBRAMSDP.io.clkA := this.clock
  blackBoxBRAMSDP.io.clkB := this.clock

  val portA = io.portA
  blackBoxBRAMSDP.io.weA   <> portA.writeEn.get
  blackBoxBRAMSDP.io.enA   <> portA.en
  blackBoxBRAMSDP.io.addrA <> portA.addr
  blackBoxBRAMSDP.io.diA   <> portA.dataIn.get

  val portB = io.portB
  //portB.writeEn  N/C
  blackBoxBRAMSDP.io.enB   <> portB.en
  blackBoxBRAMSDP.io.addrB <> portB.addr
  //portB.dataIn N/C
  blackBoxBRAMSDP.io.doB   <> portB.dataOut.get

  /*
   printf( p"weA:   {${io.ports(0).writeEn}} " +
   p"enA:   {${io.ports(0).en}} " +
   p"addrA: {${io.ports(0).addr}} " +
   p"diA:   {${io.ports(0).dataIn}} " +
   p"doA:   {${io.ports(0).dataOut}} " + "\n")
   // */
}

class BlackBoxBRAMSDP(implicit val c: BRAMConfig) extends BlackBox(
  Map("ADDR_WIDTH_A"   -> c.addrWidthVec(0),
      "DATA_WIDTH_A"   -> c.dataWidthVec(0),
      "ADDR_WIDTH_B"   -> c.addrWidthVec(1),
      "DATA_WIDTH_B"   -> c.dataWidthVec(1),
      "SIZE_A"         -> c.sizeVec(0),
      "SIZE_B"         -> c.sizeVec(1)))
    with HasBlackBoxInline {
  val io = IO(new Bundle(){
                val clkA  = Input(Clock())
                val clkB  = Input(Clock())
                val enA   = Input(Bool())
                val enB   = Input(Bool())
                val weA   = Input(Bool())
                val addrA = Input(UInt(c.addrWidthVec(0).W))
                val addrB = Input(UInt(c.addrWidthVec(1).W))
                val diA   = Input(UInt(c.dataWidthVec(0).W))
                val doB   = Output(UInt(c.dataWidthVec(1).W))
              })

  setInline("BlackBoxBRAMSDP.v",
            s"""
      |// Simple Dual-Port Block RAM
      |// one port acts as read and the other acts as write
      |
      |module BlackBoxBRAMSDP #(
      |    parameter ADDR_WIDTH_A = ${c.addrWidthVec(0)},
      |    parameter DATA_WIDTH_A = ${c.dataWidthVec(0)},
      |    parameter ADDR_WIDTH_B = ${c.addrWidthVec(1)},
      |    parameter DATA_WIDTH_B = ${c.dataWidthVec(1)},
      |    parameter SIZE_A       = ${c.sizeVec(0)},
      |    parameter SIZE_B       = ${c.sizeVec(1)})
      |(clkA,clkB,enA,enB,weA,addrA,addrB,diA,doB);
      |
      |
      |input clkA,clkB,enA,enB,weA;
      |input [ADDR_WIDTH_A-1:0] addrA;
      |input [ADDR_WIDTH_B-1:0] addrB;
      |input [DATA_WIDTH_A-1:0] diA;
      |output [DATA_WIDTH_B-1:0] doB;
      |
      |`define max(a,b) {(a) > (b) ? (a) : (b)}
      |`define min(a,b) {(a) < (b) ? (a) : (b)}
      |
      |function integer log2;
      |input integer value;
      |reg [31:0] shifted;
      |integer res;
      |begin
      |  if (value < 2)
      |  	 log2 = value;
      |  else
      |  begin
      |  	 shifted = value-1;
      |  	 for (res=0; shifted>0; res=res+1)
      |  		 shifted = shifted>>1;
      |  	 log2 = res;
      |  end
      |end
      |endfunction
      |
      |localparam maxSIZE  = `max(SIZE_A, SIZE_B);
      |localparam maxWIDTH = `max(DATA_WIDTH_A, DATA_WIDTH_B);
      |localparam minWIDTH = `min(DATA_WIDTH_A, DATA_WIDTH_B);
      |
      |localparam RATIO = maxWIDTH / minWIDTH;
      |localparam log2RATIO = log2(RATIO);
      |
      |reg [minWIDTH-1:0] ram [0:maxSIZE-1];
      |reg [DATA_WIDTH_B-1:0] readB;
      |always @(posedge clkB)
      |begin
      |  if (enB) begin
      |      readB <= ram[addrB];
      |  end
      |end
      |assign doB = readB;
      |
      |generate
      |if(RATIO > 1)
      |  always @(posedge clkA)
      |  begin : portA
      |    integer i;
      |    reg [log2RATIO-1:0] lsbaddr;
      |    for (i=0; i < RATIO; i = i + 1) begin
      |      lsbaddr = i;
      |      if (enA) begin
      |        if (weA)
      |          ram[{addrA, lsbaddr}] <= diA[(i+1)*minWIDTH-1 -: minWIDTH];
      |      end
      |    end
      |  end
      |else
      |  always @(posedge clkA)
      |  begin : portA
      |    if (enA) begin
      |      if (weA)
      |        ram[addrA] <= diA;
      |    end
      |  end
      |endgenerate
      |
      |
      |endmodule
  """.stripMargin)
}

