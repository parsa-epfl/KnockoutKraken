///** Credits to my friend Laurier Loiselle
//  *  source: https://github.com/laurierloi/fpgabits/
//  */
//
//package common
//
//import scala.language.reflectiveCalls
//
//import java.nio.ByteBuffer
//
//import scala.language.reflectiveCalls
//
//import chisel3._
//import chisel3.tester._
//import chisel3.util._
////import chisel3.experimental._
//
//object constLegacyBRAM {
//  /** From table 1-7,1-8,1-9,1-10 in UG573
//    * Nomenclature: 18 or 36 for LegacyBRAM type
//    *               S/T,  for (S)imple/(T)rue Dual Port
//    *               Dn, where n is data width
//    *               An, where n is address width
//    *    (optional) Wn, where n is the alternate port witdth(only for SDP)
//    *    (optional) C,  for cascade implementation
//    */
//
//  /* Fix the index of the write and read ports for the sdp */
//  val sdpWritePort :: sdpReadPort = Enum(2)
//
//  /* PORT PARAM DEFINITIONS */
//  /* 18 TDP */
//  //  (addrWidth, dataWidth, size)
//  val LegacyBRAM18TD1A14     = LegacyBRAMPortParam(14, 1,  16384)
//  val LegacyBRAM18TD2A13     = LegacyBRAMPortParam(13, 2,  8192)
//  val LegacyBRAM18TD4A12     = LegacyBRAMPortParam(12, 4,  4096)
//  val LegacyBRAM18TD9A11     = LegacyBRAMPortParam(11, 9,  2048)
//  val LegacyBRAM18TD18A10    = LegacyBRAMPortParam(10, 18, 1024)
//
//  /* 36 TDP */
//  val LegacyBRAM36TD1A15     = LegacyBRAMPortParam(15, 1,  32768)
//  val LegacyBRAM36TD2A14     = LegacyBRAMPortParam(14, 2,  16384)
//  val LegacyBRAM36TD4A13     = LegacyBRAMPortParam(13, 4,  8192)
//  val LegacyBRAM36TD9A12     = LegacyBRAMPortParam(12, 9,  4096)
//  val LegacyBRAM36TD18A11    = LegacyBRAMPortParam(11, 18, 2048)
//  val LegacyBRAM36TD36A10    = LegacyBRAMPortParam(10, 36, 1024)
//  //val LegacyBRAM36TD1A16C    = LegacyBRAMPortParam(16, 1,  65546)
//
//  // Dict key is the data width
//  val TDPLegacyBRAM18AvailableWidths = Seq(1, 2, 4, 9, 18)
//  val TDPLegacyBRAM36AvailableWidths = Seq(1, 2, 4, 9, 18, 36)
//  val TDPLegacyBRAM18ParamDict = Map[Int , LegacyBRAMPortParam](
//    1 -> LegacyBRAM18TD1A14 , 2   -> LegacyBRAM18TD2A13  , 4  -> LegacyBRAM18TD4A12 ,
//    9 -> LegacyBRAM18TD9A11 , 18  -> LegacyBRAM18TD18A10)
//  val TDPLegacyBRAM36ParamDict = Map[Int , LegacyBRAMPortParam](
//    1 -> LegacyBRAM36TD1A15 , 2   -> LegacyBRAM36TD2A14  , 4  ->  LegacyBRAM36TD4A13 ,
//    9 -> LegacyBRAM36TD9A12 , 18  -> LegacyBRAM36TD18A11 , 36 -> LegacyBRAM36TD36A10)
//
//  def getTDPClosestWidth(width: Int, isLegacyBRAM18: Boolean): Int = {
//    if(isLegacyBRAM18){ TDPLegacyBRAM18AvailableWidths.filter(_ >= width)(0) }
//    else {        TDPLegacyBRAM36AvailableWidths.filter(_ >= width)(0) } }
//  def getTDPLegacyBRAMPortParam(width: Int, isLegacyBRAM18: Boolean): LegacyBRAMPortParam = {
//    if(isLegacyBRAM18) { TDPLegacyBRAM18ParamDict(width) }
//    else         { TDPLegacyBRAM36ParamDict(width) } }
//
//
//  /* 18 SDP */
//  val LegacyBRAM18SD32A14W1  = new LegacyBRAMConfig(false, Seq(9, 14), Seq(32, 1 ), Seq(512, 16384))
//  val LegacyBRAM18SD32A13W2  = new LegacyBRAMConfig(false, Seq(9, 13), Seq(32, 2 ), Seq(512, 8192 ))
//  val LegacyBRAM18SD32A12W4  = new LegacyBRAMConfig(false, Seq(9, 12), Seq(32, 4 ), Seq(512, 4096 ))
//  val LegacyBRAM18SD36A11W9  = new LegacyBRAMConfig(false, Seq(9, 11), Seq(36, 9 ), Seq(512, 2048 ))
//  val LegacyBRAM18SD36A10W18 = new LegacyBRAMConfig(false, Seq(9, 10), Seq(36, 18), Seq(512, 1024 ))
//  val LegacyBRAM18SD36A10W36 = new LegacyBRAMConfig(false, Seq(9, 10), Seq(36, 36), Seq(512, 512  ))
//
//  /* 36 SDP */
//  val LegacyBRAM36SD64A15W1  = new LegacyBRAMConfig(false, Seq(9, 15), Seq(64, 1 ), Seq(512, 32768))
//  val LegacyBRAM36SD64A14W2  = new LegacyBRAMConfig(false, Seq(9, 14), Seq(64, 2 ), Seq(512, 16384))
//  val LegacyBRAM36SD64A13W4  = new LegacyBRAMConfig(false, Seq(9, 13), Seq(64, 4 ), Seq(512, 8192 ))
//  val LegacyBRAM36SD72A12W9  = new LegacyBRAMConfig(false, Seq(9, 12), Seq(72, 9 ), Seq(512, 4096 ))
//  val LegacyBRAM36SD72A11W18 = new LegacyBRAMConfig(false, Seq(9, 11), Seq(72, 18), Seq(512, 2048 ))
//  val LegacyBRAM36SD72A10W36 = new LegacyBRAMConfig(false, Seq(9, 10), Seq(72, 36), Seq(512, 1024 ))
//  val LegacyBRAM36SD72A9W72  = new LegacyBRAMConfig(false, Seq(9, 9 ), Seq(72, 72), Seq(512, 512  ))
//
//  val SDPLegacyBRAM18AvailableWidths = Seq(1, 2, 4, 9, 18, 36)
//  val SDPLegacyBRAM36AvailableWidths = Seq(1, 2, 4, 9, 18, 36, 72)
//  val SDPLegacyBRAM18ConfigDict = Map[Int, LegacyBRAMConfig](
//    1  -> LegacyBRAM18SD32A14W1, 2  -> LegacyBRAM18SD32A13W2, 4  -> LegacyBRAM18SD32A12W4,
//    9  -> LegacyBRAM18SD36A11W9, 18 -> LegacyBRAM18SD36A10W18, 36 -> LegacyBRAM18SD36A10W36)
//  val SDPLegacyBRAM36ConfigDict = Map[Int, LegacyBRAMConfig](
//    1  -> LegacyBRAM36SD64A15W1 , 2  -> LegacyBRAM36SD64A14W2 , 4  -> LegacyBRAM36SD64A13W4 ,
//    9  -> LegacyBRAM36SD72A12W9 , 18 -> LegacyBRAM36SD72A11W18, 36 -> LegacyBRAM36SD72A10W36,
//    72 -> LegacyBRAM36SD72A9W72)
//
//}
//
//case class LegacyBRAMPortParam(
//  val addrWidth : Int,
//  val dataWidth : Int,
//  val size      : Int,
//  val writePort : Boolean = true,
//  val readPort  : Boolean = true
//)
//
//case class LegacyBRAMConfig(
//  val portParamVec : Seq[LegacyBRAMPortParam],
//  val debug        : Boolean = false
//){
//  def this(isTrueDualPort: Boolean, addrWidthVec: Seq[Int],
//           dataWidthVec: Seq[Int], sizeVec: Seq[Int]) = {
//    this(Seq(LegacyBRAMPortParam(addrWidthVec(0), dataWidthVec(0), sizeVec(0), true, !isTrueDualPort),
//             LegacyBRAMPortParam(addrWidthVec(1), dataWidthVec(1), sizeVec(1), !isTrueDualPort, true)))
//  }
//  val isTrueDualPort  = portParamVec.map{case x => x.writePort & x.readPort}.foldLeft(true)(_ && _)
//  val addrWidthVec    = portParamVec.map{_.addrWidth}
//  val dataWidthVec    = portParamVec.map{_.dataWidth}
//  val sizeVec         = portParamVec.map{_.size}
//
//  val readLatency = 2
//  val nbrPort     = 2
//  val sdpWritePortIdx = 0
//  val sdpReadPortIdx  = 1
//  val writePortVec = portParamVec.map{_.writePort}
//  val readPortVec  = portParamVec.map{_.readPort}
//  val widthRatio = dataWidthVec(1) / dataWidthVec(0)
//
//  def dataGen(idx: Int): UInt = UInt(dataWidthVec(idx).W)
//  def addrGen(idx: Int): UInt = UInt(addrWidthVec(idx).W)
//
//  override def toString: String =
//    f"(tdp: $isTrueDualPort" +
//      f", addr0W: ${addrWidthVec(0)}%2s, data0W: ${dataWidthVec(0)}%2s, size0: ${sizeVec(0)}%6s" +
//      f", addr1W: ${addrWidthVec(1)}%2s, data1W: ${dataWidthVec(1)}%2s, size1: ${sizeVec(1)}%6s)"
//}
//
//class LegacyBRAMPort(private val idx: Int)(implicit val c: LegacyBRAMConfig) extends Bundle {
//  val writeEn = if(c.writePortVec(idx)) Some(Input(Bool())) else None
//  val en      = Input(Bool())
//  val addr    = Input(c.addrGen(idx))
//  val dataIn  = if(c.writePortVec(idx)) Some(Input( c.dataGen(idx))) else None
//  val dataOut = if(c.readPortVec(idx))  Some(Output(c.dataGen(idx))) else None
//}
//
//class LegacyBRAMPortAXI(private val idx: Int)(implicit val c: LegacyBRAMConfig) extends Bundle {
//  val CLK = Input(Bool())
//  val RST = Input(Bool())
//  val WE = Input(Bool())
//  val EN = Input(Bool())
//  val ADDR = Input(c.addrGen(idx))
//  val DI = Input( c.dataGen(idx))
//  val DO = Output(c.dataGen(idx))
//
//  def <>(port_ : LegacyBRAMPort) {
//    WE <> port_.writeEn.get
//    EN <> port_.en
//    DI <> port_.dataIn.get
//    DO <> port_.dataOut.get
//    ADDR <> port_.addr
//  }
//}
//
///** Note: This LegacyBRAM port is expected to be 32 bit wide for data
//  *       And address of 1024 words
//  */
//object LegacyBRAMPort {
//  implicit class LegacyBRAMPortDriver(target: LegacyBRAMPortAXI)(implicit clock: Clock) {
//    def wrLegacyBRAM32b(bits:BigInt, offst: BigInt) = {
//      target.EN.poke(true.B)
//      target.WE.poke(true.B)
//      target.ADDR.poke(offst.U)
//      target.DI.poke(bits.U)
//      clock.step()
//      target.EN.poke(false.B)
//      target.WE.poke(false.B)
//      target.ADDR.poke(0.U)
//      target.DI.poke(0.U)
//    }
//
//    def rdLegacyBRAM32b(offst:BigInt): BigInt = {
//      target.EN.poke(true.B)
//      target.ADDR.poke(offst.U)
//      clock.step()
//      target.EN.poke(false.B)
//      target.ADDR.poke(0.U)
//      val uint32 = target.DO.peek
//      return uint32.litValue()
//    }
//
//    def wrLegacyBRAM64b(lw: BigInt, offst: BigInt) = {
//      val bytes = Array.fill(8)(0.toByte)
//      for( i <- 0 to 7 ) bytes(i) = ((lw >> ((7-i) * 8)) & 0xFF).toByte
//      val msb = BigInt(Array(0.toByte) ++ bytes.slice(0, 4))
//      val lsb = BigInt(Array(0.toByte) ++ bytes.slice(4, 8))
//      wrLegacyBRAM32b(msb, offst)
//      wrLegacyBRAM32b(lsb, offst+1)
//    }
//
//    def rdLegacyBRAM64b(offst:BigInt): BigInt = {
//      val msb = rdLegacyBRAM32b(offst)
//      val lsb = rdLegacyBRAM32b(offst+1)
//      val byte_msb = Array.fill(4)(0.toByte)
//      val byte_lsb = Array.fill(4)(0.toByte)
//      for (i <- 0 to 3) byte_msb(i) = ((msb >> ((3-i) * 8)) & 0xFF).toByte
//      for (i <- 0 to 3) byte_lsb(i) = ((lsb >> ((3-i) * 8)) & 0xFF).toByte
//      val uint64 = BigInt(Array(0.toByte) ++ byte_msb ++ byte_lsb) // Unsigned long
//      return uint64
//    }
//  }
//}
//
//class LegacyBRAMIO(implicit val c: LegacyBRAMConfig) extends Bundle {
//  val portA = new LegacyBRAMPort(0)
//  val portB = new LegacyBRAMPort(1)
//
//  def getPort(idx: Int): LegacyBRAMPort = {
//    if (idx == 0) { portA }
//    else { portB }
//  }
//}
//
//class LegacyBRAM(implicit val c: LegacyBRAMConfig) extends MultiIOModule {
//  val io = IO(new LegacyBRAMIO)
//
//  val bram = Module(
//    //if(c.isTrueDualPort) {
//      new LegacyBRAMTDP
//    //} else {
//    //  new LegacyBRAMSDP}
//  )
//
//  io <> bram.io
//}
//
///* TRUE DUAL PORT */
//class LegacyBRAMTDP(implicit val c: LegacyBRAMConfig) extends MultiIOModule {
//  val io = IO(new LegacyBRAMIO)
//
//  assert((c.dataWidthVec(0) >= c.dataWidthVec(1)),
//         "To use Asymetric TDP LegacyBRAM, the size of port0" +
//           " must be greater or equal to the size of port1")
//
//  val blackBoxLegacyBRAMTDP = Module(new NonBlackBoxLegacyBRAMTDP)
//  blackBoxLegacyBRAMTDP.io.clk := this.clock
//
//  val portA = io.portA
//  blackBoxLegacyBRAMTDP.io.weA   <> portA.writeEn.get
//  blackBoxLegacyBRAMTDP.io.enA   <> portA.en
//  blackBoxLegacyBRAMTDP.io.addrA <> portA.addr
//  blackBoxLegacyBRAMTDP.io.diA   <> portA.dataIn.get
//  blackBoxLegacyBRAMTDP.io.doA   <> portA.dataOut.get
//
//  val portB = io.portB
//  blackBoxLegacyBRAMTDP.io.weB   <> portB.writeEn.get
//  blackBoxLegacyBRAMTDP.io.enB   <> portB.en
//  blackBoxLegacyBRAMTDP.io.addrB <> portB.addr
//  blackBoxLegacyBRAMTDP.io.diB   <> portB.dataIn.get
//  blackBoxLegacyBRAMTDP.io.doB   <> portB.dataOut.get
//
//  /*
//   printf( p"weA: ${io.getPort(0).writeEn.get}" +
//   p", enA: ${io.getPort(0).en}" +
//   p", addrA: ${io.getPort(0).addr}" +
//   p", diA: ${io.getPort(0).dataIn.get}" +
//   p", doA: ${io.getPort(0).dataOut.get}" + "\n")
//   printf( p"weB: ${io.getPort(1).writeEn.get}" +
//   p", enB: ${io.getPort(1).en}" +
//   p", addrB: ${io.getPort(1).addr}" +
//   p", diB: ${io.getPort(1).dataIn.get}" +
//   p", doB: ${io.getPort(1).dataOut.get}" + "\n")
//   */
//}
//
//class NonBlackBoxLegacyBRAMTDP(implicit val c: LegacyBRAMConfig) extends Module {
//  val io = IO(new Bundle(){
//                val clk   = Input(Clock())
//                val enA   = Input(Bool())
//                val enB   = Input(Bool())
//                val weA   = Input(Bool())
//                val weB   = Input(Bool())
//                val addrA = Input(UInt(c.addrWidthVec(0).W))
//                val addrB = Input(UInt(c.addrWidthVec(1).W))
//                val diA   = Input(UInt(c.dataWidthVec(0).W))
//                val diB   = Input(UInt(c.dataWidthVec(1).W))
//                val doA   = Output(UInt(c.dataWidthVec(0).W))
//                val doB   = Output(UInt(c.dataWidthVec(1).W))
//              })
//
//  val ADDR_WIDTH_A = c.addrWidthVec(0)
//  val DATA_WIDTH_A = c.dataWidthVec(0)
//  val ADDR_WIDTH_B = c.addrWidthVec(1)
//  val DATA_WIDTH_B = c.dataWidthVec(1)
//  val SIZE_A       = c.sizeVec(0)
//  val SIZE_B       = c.sizeVec(1)
//
//  val maxSIZE  = math.max(SIZE_A, SIZE_B);
//  val maxWIDTH = math.max(DATA_WIDTH_A, DATA_WIDTH_B);
//  val minWIDTH = math.min(DATA_WIDTH_A, DATA_WIDTH_B);
//
//  val RATIO = maxWIDTH / minWIDTH;
//  val log2RATIO = math.log10(RATIO)/math.log10(2);
//
//  val ram = Mem(maxSIZE, UInt(minWIDTH.W))
//  val ramPortA = ram(io.addrA)
//  val ramPortB = ram(io.addrB)
//
//  val readA = Reg(UInt((c.dataWidthVec(0)).W));
//  val readB = Reg(UInt((c.dataWidthVec(1)).W));
//
//  when(io.enA) {
//    when(io.weA) {
//      ramPortA := io.diA
//    }
//    readA := ramPortA
//  }
//
//  when(io.enB) {
//    when(io.weB) {
//      ramPortB := io.diB
//    }
//    readB := ramPortB
//  }
//
//  io.doA := readA;
//  io.doB := readB;
//}
//
//class BlackBoxLegacyBRAMTDP(implicit val c: LegacyBRAMConfig) extends BlackBox(
//  Map("ADDR_WIDTH_A"   -> c.addrWidthVec(0),
//      "DATA_WIDTH_A"   -> c.dataWidthVec(0),
//      "ADDR_WIDTH_B"   -> c.addrWidthVec(1),
//      "DATA_WIDTH_B"   -> c.dataWidthVec(1),
//      "SIZE_A"         -> c.sizeVec(0),
//      "SIZE_B"         -> c.sizeVec(1)))
//    with HasBlackBoxInline {
//  val io = IO(new Bundle(){
//                val clk   = Input(Clock())
//                val enA   = Input(Bool())
//                val enB   = Input(Bool())
//                val weA   = Input(Bool())
//                val weB   = Input(Bool())
//                val addrA = Input(UInt(c.addrWidthVec(0).W))
//                val addrB = Input(UInt(c.addrWidthVec(1).W))
//                val diA   = Input(UInt(c.dataWidthVec(0).W))
//                val diB   = Input(UInt(c.dataWidthVec(1).W))
//                val doA   = Output(UInt(c.dataWidthVec(0).W))
//                val doB   = Output(UInt(c.dataWidthVec(1).W))
//              })
//
//  setInline(s"BlackBoxLegacyBRAMTDP.v",
//            s"""
//      |// Dual-Port Block RAM with Two Write Ports
//      |
//      |module BlackBoxLegacyBRAMTDP #(
//      |                         parameter ADDR_WIDTH_A = ${c.addrWidthVec(0)},
//      |                         parameter DATA_WIDTH_A = ${c.dataWidthVec(0)},
//      |                         parameter ADDR_WIDTH_B = ${c.addrWidthVec(1)},
//      |                         parameter DATA_WIDTH_B = ${c.dataWidthVec(1)},
//      |                         parameter SIZE_A       = ${c.sizeVec(0)},
//      |                         parameter SIZE_B       = ${c.sizeVec(1)})
//      |   (clk,enA,enB,weA,weB,addrA,addrB,diA,diB,doA,doB);
//      |
//      |   input clk,enA,enB,weA,weB;
//      |   input [ADDR_WIDTH_A-1:0] addrA;
//      |   input [ADDR_WIDTH_B-1:0] addrB;
//      |   input [DATA_WIDTH_A-1:0] diA;
//      |   input [DATA_WIDTH_B-1:0] diB;
//      |   output [DATA_WIDTH_A-1:0] doA;
//      |   output [DATA_WIDTH_B-1:0] doB;
//      |
//      |`define max(a,b) {(a) > (b) ? (a) : (b)}
//      |`define min(a,b) {(a) < (b) ? (a) : (b)}
//      |
//      |   function integer log2;
//      |      input integer          value;
//      |      reg [31:0]             shifted;
//      |      integer                res;
//      |      begin
//      |         if (value < 2)
//      |  	       log2 = value;
//      |         else
//      |           begin
//      |  	          shifted = value-1;
//      |  	          for (res=0; shifted>0; res=res+1)
//      |  		          shifted = shifted>>1;
//      |  	          log2 = res;
//      |           end
//      |      end
//      |   endfunction
//      |
//      |   localparam maxSIZE  = `max(SIZE_A, SIZE_B);
//      |   localparam maxWIDTH = `max(DATA_WIDTH_A, DATA_WIDTH_B);
//      |   localparam minWIDTH = `min(DATA_WIDTH_A, DATA_WIDTH_B);
//      |
//      |   localparam RATIO = maxWIDTH / minWIDTH;
//      |   localparam log2RATIO = log2(RATIO);
//      |
//      |   reg[minWIDTH-1:0] ram [0:maxSIZE-1];
//      |   reg [DATA_WIDTH_A-1:0] readA;
//      |   reg [DATA_WIDTH_B-1:0] readB;
//      |
//      |   always @(posedge clk)
//      |     begin
//      |        if (enB)
//      |          begin
//      |             if (weB)
//      |               ram[addrB] <= diB;
//      |             readB <= ram[addrB];
//      |          end
//      |     end
//      |
//      |   always @(posedge clk)
//      |     begin
//      |        if (enA)
//      |          begin
//      |             if (weA)
//      |               ram[addrA] <= diA;
//      |             readA <= ram[addrA];
//      |          end
//      |     end
//      |
//      |   assign doA = readA;
//      |   assign doB = readB;
//      |
//      |endmodule
//  """.stripMargin)
//}
//
///* SIMPLE DUAL PORT */
//class LegacyBRAMSDP(implicit val c: LegacyBRAMConfig) extends MultiIOModule {
//  val io = IO(new LegacyBRAMIO)
//
//  assert((c.dataWidthVec(0) >= c.dataWidthVec(1)),
//         "To use SDP LegacyBRAM, the size of write port (port0)" +
//           " must be greater to the size of read port (port1)." +
//           "\nThis module could support readPort > writePort" +
//           ", but no use was found for it in the design")
//
//  val blackBoxLegacyBRAMSDP = Module(new BlackBoxLegacyBRAMSDP)
//
//  blackBoxLegacyBRAMSDP.io.clkA := this.clock
//  blackBoxLegacyBRAMSDP.io.clkB := this.clock
//
//  val portA = io.portA
//  blackBoxLegacyBRAMSDP.io.weA   <> portA.writeEn.get
//  blackBoxLegacyBRAMSDP.io.enA   <> portA.en
//  blackBoxLegacyBRAMSDP.io.addrA <> portA.addr
//  blackBoxLegacyBRAMSDP.io.diA   <> portA.dataIn.get
//
//  val portB = io.portB
//  //portB.writeEn  N/C
//  blackBoxLegacyBRAMSDP.io.enB   <> portB.en
//  blackBoxLegacyBRAMSDP.io.addrB <> portB.addr
//  //portB.dataIn N/C
//  blackBoxLegacyBRAMSDP.io.doB   <> portB.dataOut.get
//
//  /*
//   printf( p"weA:   {${io.ports(0).writeEn}} " +
//   p"enA:   {${io.ports(0).en}} " +
//   p"addrA: {${io.ports(0).addr}} " +
//   p"diA:   {${io.ports(0).dataIn}} " +
//   p"doA:   {${io.ports(0).dataOut}} " + "\n")
//   // */
//}
//
//class BlackBoxLegacyBRAMSDP(implicit val c: LegacyBRAMConfig) extends BlackBox(
//  Map("ADDR_WIDTH_A"   -> c.addrWidthVec(0),
//      "DATA_WIDTH_A"   -> c.dataWidthVec(0),
//      "ADDR_WIDTH_B"   -> c.addrWidthVec(1),
//      "DATA_WIDTH_B"   -> c.dataWidthVec(1),
//      "SIZE_A"         -> c.sizeVec(0),
//      "SIZE_B"         -> c.sizeVec(1)))
//    with HasBlackBoxInline {
//  val io = IO(new Bundle(){
//                val clkA  = Input(Clock())
//                val clkB  = Input(Clock())
//                val enA   = Input(Bool())
//                val enB   = Input(Bool())
//                val weA   = Input(Bool())
//                val addrA = Input(UInt(c.addrWidthVec(0).W))
//                val addrB = Input(UInt(c.addrWidthVec(1).W))
//                val diA   = Input(UInt(c.dataWidthVec(0).W))
//                val doB   = Output(UInt(c.dataWidthVec(1).W))
//              })
//
//  setInline("BlackBoxLegacyBRAMSDP.v",
//            s"""
//      |// Simple Dual-Port Block RAM
//      |// one port acts as read and the other acts as write
//      |
//      |module BlackBoxLegacyBRAMSDP #(
//      |    parameter ADDR_WIDTH_A = ${c.addrWidthVec(0)},
//      |    parameter DATA_WIDTH_A = ${c.dataWidthVec(0)},
//      |    parameter ADDR_WIDTH_B = ${c.addrWidthVec(1)},
//      |    parameter DATA_WIDTH_B = ${c.dataWidthVec(1)},
//      |    parameter SIZE_A       = ${c.sizeVec(0)},
//      |    parameter SIZE_B       = ${c.sizeVec(1)})
//      |(clkA,clkB,enA,enB,weA,addrA,addrB,diA,doB);
//      |
//      |
//      |input clkA,clkB,enA,enB,weA;
//      |input [ADDR_WIDTH_A-1:0] addrA;
//      |input [ADDR_WIDTH_B-1:0] addrB;
//      |input [DATA_WIDTH_A-1:0] diA;
//      |output [DATA_WIDTH_B-1:0] doB;
//      |
//      |`define max(a,b) {(a) > (b) ? (a) : (b)}
//      |`define min(a,b) {(a) < (b) ? (a) : (b)}
//      |
//      |function integer log2;
//      |input integer value;
//      |reg [31:0] shifted;
//      |integer res;
//      |begin
//      |  if (value < 2)
//      |  	 log2 = value;
//      |  else
//      |  begin
//      |  	 shifted = value-1;
//      |  	 for (res=0; shifted>0; res=res+1)
//      |  		 shifted = shifted>>1;
//      |  	 log2 = res;
//      |  end
//      |end
//      |endfunction
//      |
//      |localparam maxSIZE  = `max(SIZE_A, SIZE_B);
//      |localparam maxWIDTH = `max(DATA_WIDTH_A, DATA_WIDTH_B);
//      |localparam minWIDTH = `min(DATA_WIDTH_A, DATA_WIDTH_B);
//      |
//      |localparam RATIO = maxWIDTH / minWIDTH;
//      |localparam log2RATIO = log2(RATIO);
//      |
//      |reg [minWIDTH-1:0] ram [0:maxSIZE-1];
//      |reg [DATA_WIDTH_B-1:0] readB;
//      |always @(posedge clkB)
//      |begin
//      |  if (enB) begin
//      |      readB <= ram[addrB];
//      |  end
//      |end
//      |assign doB = readB;
//      |
//      |generate
//      |if(RATIO > 1)
//      |  always @(posedge clkA)
//      |  begin : portA
//      |    integer i;
//      |    reg [log2RATIO-1:0] lsbaddr;
//      |    for (i=0; i < RATIO; i = i + 1) begin
//      |      lsbaddr = i;
//      |      if (enA) begin
//      |        if (weA)
//      |          ram[{addrA, lsbaddr}] <= diA[(i+1)*minWIDTH-1 -: minWIDTH];
//      |      end
//      |    end
//      |  end
//      |else
//      |  always @(posedge clkA)
//      |  begin : portA
//      |    if (enA) begin
//      |      if (weA)
//      |        ram[addrA] <= diA;
//      |    end
//      |  end
//      |endgenerate
//      |
//      |
//      |endmodule
//  """.stripMargin)
//}

