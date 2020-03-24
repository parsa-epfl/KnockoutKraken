package armflex.util

import chisel3._
import chisel3.experimental._
import chisel3.util._

class BRAMTDP(implicit cfg: BRAMConfig) extends BlackBox(Map(
  "NB_COL"    -> cfg.NB_COL,                    // Specify number of columns (number of bytes)
  "COL_WIDTH" -> cfg.COL_WIDTH,              // Specify column width (byte width, typically 8 or 9)
  "RAM_DEPTH" -> cfg.RAM_DEPTH,              // Specify RAM depth (number of entries)
  "RAM_PERFORMANCE" -> cfg.RAM_PERFORMANCE,  // Select "HIGH_PERFORMANCE" or "LOW_LATENCY"
  "INIT_FILE" ->  cfg.INIT_FILE              // Specify name/location of RAM initialization file if using one (leave blank if not)
)) with HasBlackBoxInline {
  val io = IO(new Bundle(){
    val clk = Input(Bool())
    val rst = Input(Bool())
    val addra = Input(UInt(log2Ceil(cfg.RAM_DEPTH).W))
    val addrb = Input(UInt(log2Ceil(cfg.RAM_DEPTH).W))
    val dina = Input(UInt((cfg.NB_COL*cfg.COL_WIDTH).W))
    val dinb = Input(UInt((cfg.NB_COL*cfg.COL_WIDTH).W))
    val wea = Input(UInt(cfg.NB_COL.W))
    val web = Input(UInt(cfg.NB_COL.W))
    val ena = Input(Bool())
    val enb = Input(Bool())
    val regcea = Input(Bool())
    val regceb = Input(Bool())
    val douta = Output(UInt((cfg.NB_COL*cfg.COL_WIDTH).W))
    val doutb = Output(UInt((cfg.NB_COL*cfg.COL_WIDTH).W))
  })

  setInline("BRAMTDP.v",
    s"""
     | //  Xilinx True Dual Port RAM Byte Write, Write First Single Clock RAM
     | //  This code implements a parameterizable true dual port memory (both ports can read and write).
     | //  The behavior of this RAM is when data is written, the new memory contents at the write
     | //  address are presented on the output port.
     |
     | module BRAMTDP #(
     |   parameter NB_COL = 4,                           // Specify number of columns (number of bytes)
     |   parameter COL_WIDTH = 9,                        // Specify column width (byte width, typically 8 or 9)
     |   parameter RAM_DEPTH = 1024,                     // Specify RAM depth (number of entries)
     |   parameter RAM_PERFORMANCE = "HIGH_PERFORMANCE", // Select "HIGH_PERFORMANCE" or "LOW_LATENCY"
     |   parameter INIT_FILE = ""                        // Specify name/location of RAM initialization file if using one (leave blank if not)
     | ) (
     |   input clk,                             // Clock
     |   input rst,                             // output reset (does not affect memory contents)
     |   input [clogb2(RAM_DEPTH-1)-1:0] addra, // Port A address bus, width determined from RAM_DEPTH
     |   input [clogb2(RAM_DEPTH-1)-1:0] addrb, // Port B address bus, width determined from RAM_DEPTH
     |   input [(NB_COL*COL_WIDTH)-1:0] dina,   // Port A RAM input data
     |   input [(NB_COL*COL_WIDTH)-1:0] dinb,   // Port B RAM input data
     |   input [NB_COL-1:0] wea,                // Port A write enable
     |   input [NB_COL-1:0] web,                // Port B write enable
     |   input ena,                             // Port A RAM Enable, for additional power savings, disable BRAM when not in use
     |   input enb,                             // Port B RAM Enable, for additional power savings, disable BRAM when not in use
     |   input regcea,                          // Port A output register enable
     |   input regceb,                          // Port B output register enable
     |   output [(NB_COL*COL_WIDTH)-1:0] douta, // Port A RAM output data
     |   output [(NB_COL*COL_WIDTH)-1:0] doutb  // Port B RAM output data
     | );
     |
     |   reg [(NB_COL*COL_WIDTH)-1:0] BRAM [RAM_DEPTH-1:0];
     |   reg [(NB_COL*COL_WIDTH)-1:0] ram_data_a = {(NB_COL*COL_WIDTH){1'b0}};
     |   reg [(NB_COL*COL_WIDTH)-1:0] ram_data_b = {(NB_COL*COL_WIDTH){1'b0}};
     |
     |   // The following code either initializes the memory values to a specified file or to all zeros to match hardware
     |   generate
     |     if (INIT_FILE != "") begin: use_init_file
     |       initial
     |         ${"$"}readmemh(INIT_FILE, BRAM, 0, RAM_DEPTH-1);
     |     end else begin: init_bram_to_zero
     |       integer ram_index;
     |       initial
     |         for (ram_index = 0; ram_index < RAM_DEPTH; ram_index = ram_index + 1)
     |           BRAM[ram_index] = {(NB_COL*COL_WIDTH){1'b0}};
     |     end
     |   endgenerate
     |
     |   generate
     |   genvar i;
     |      for (i = 0; i < NB_COL; i = i+1) begin: byte_write
     |        always @(posedge clk)
     |          if (ena)
     |            if (wea[i]) begin
     |              BRAM[addra][(i+1)*COL_WIDTH-1:i*COL_WIDTH] <= dina[(i+1)*COL_WIDTH-1:i*COL_WIDTH];
     |              ram_data_a[(i+1)*COL_WIDTH-1:i*COL_WIDTH] <= dina[(i+1)*COL_WIDTH-1:i*COL_WIDTH];
     |            end else begin
     |              ram_data_a[(i+1)*COL_WIDTH-1:i*COL_WIDTH] <= BRAM[addra][(i+1)*COL_WIDTH-1:i*COL_WIDTH];
     |            end
     |
     |        always @(posedge clk)
     |          if (enb)
     |            if (web[i]) begin
     |              BRAM[addrb][(i+1)*COL_WIDTH-1:i*COL_WIDTH] <= dinb[(i+1)*COL_WIDTH-1:i*COL_WIDTH];
     |              ram_data_b[(i+1)*COL_WIDTH-1:i*COL_WIDTH] <= dinb[(i+1)*COL_WIDTH-1:i*COL_WIDTH];
     |            end else begin
     |              ram_data_b[(i+1)*COL_WIDTH-1:i*COL_WIDTH] <= BRAM[addrb][(i+1)*COL_WIDTH-1:i*COL_WIDTH];
     |            end
     |      end
     |   endgenerate
     |
     |   //  The following code generates HIGH_PERFORMANCE (use output register) or LOW_LATENCY (no output register)
     |   generate
     |     if (RAM_PERFORMANCE == "LOW_LATENCY") begin: no_output_register
     |
     |       // The following is a 1 clock cycle read latency at the cost of a longer clock-to-out timing
     |        assign douta = ram_data_a;
     |        assign doutb = ram_data_b;
     |
     |     end else begin: output_register
     |
     |       // The following is a 2 clock cycle read latency with improve clock-to-out timing
     |
     |       reg [(NB_COL*COL_WIDTH)-1:0] douta_reg = {(NB_COL*COL_WIDTH){1'b0}};
     |       reg [(NB_COL*COL_WIDTH)-1:0] doutb_reg = {(NB_COL*COL_WIDTH){1'b0}};
     |
     |       always @(posedge clk)
     |         if (rst)
     |           douta_reg <= {(NB_COL*COL_WIDTH){1'b0}};
     |         else if (regcea)
     |           douta_reg <= ram_data_a;
     |
     |       always @(posedge clk)
     |         if (rst)
     |           doutb_reg <= {(NB_COL*COL_WIDTH){1'b0}};
     |         else if (regceb)
     |           doutb_reg <= ram_data_b;
     |
     |       assign douta = douta_reg;
     |       assign doutb = doutb_reg;
     |
     |     end
     |   endgenerate
     |
     |   //  The following function calculates the address width based on specified RAM depth
     |   function integer clogb2;
     |     input integer depth;
     |       for (clogb2=0; depth>0; clogb2=clogb2+1)
     |         depth = depth >> 1;
     |   endfunction
     |
     | endmodule
     |
     | // The following is an instantiation template for xilinx_true_dual_port_write_first_byte_write_1_clock_ram
     | /*
     |   //  Xilinx True Dual Port RAM Byte Write Write-First Single Clock RAM
     |   BRAMTDP #(
     |     .NB_COL(4),                           // Specify number of columns (number of bytes)
     |     .COL_WIDTH(9),                        // Specify column width (byte width, typically 8 or 9)
     |     .RAM_DEPTH(1024),                     // Specify RAM depth (number of entries)
     |     .RAM_PERFORMANCE("HIGH_PERFORMANCE"), // Select "HIGH_PERFORMANCE" or "LOW_LATENCY"
     |     .INIT_FILE("")                        // Specify name/location of RAM initialization file if using one (leave blank if not)
     |   ) your_instance_name (
     |     .clk(clk),       // clock
     |     .rst(rst),       // output reset (does not affect memory contents)
     |     .addra(addra),   // Port A address bus, width determined from RAM_DEPTH
     |     .addrb(addrb),   // Port B address bus, width determined from RAM_DEPTH
     |     .dina(dina),     // Port A RAM input data, width determined from NB_COL*COL_WIDTH
     |     .dinb(dinb),     // Port B RAM input data, width determined from NB_COL*COL_WIDTH
     |     .wea(wea),       // Port A write enable, width determined from NB_COL
     |     .web(web),       // Port B write enable, width determined from NB_COL
     |     .ena(ena),       // Port A RAM Enable, for additional power savings, disable port when not in use
     |     .enb(enb),       // Port B RAM Enable, for additional power savings, disable port when not in use
     |     .regcea(regcea), // Port A output register enable
     |     .regceb(regceb), // Port B output register enable
     |     .douta(douta),   // Port A RAM output data, width determined from NB_COL*COL_WIDTH
     |     .doutb(doutb)    // Port B RAM output data, width determined from NB_COL*COL_WIDTH
     |   );
     | */
  """.stripMargin)
  // */
}

class BRAMConfig(
  val NB_COL: Int = 4,
  val COL_WIDTH: Int = 9,
  I_RAM_DEPTH: Int = 1024,
  val INIT_FILE: String = "",
  val isRegistered: Boolean = false, // True => 2 Cycles
  val isAXI: Boolean = false // AXI is byte addressed
) {
  assert(I_RAM_DEPTH % 1024 == 0)
  val RAM_DEPTH = if(isAXI) (I_RAM_DEPTH << log2Ceil(NB_COL)) else I_RAM_DEPTH
  val RAM_PERFORMANCE = if(isRegistered) "HIGH_PERFORMANCE" else "LOW_LATENCY"
  // Clone function with set AXI ports
  def apply(isAXI: Boolean): BRAMConfig =
    new BRAMConfig(NB_COL, COL_WIDTH, I_RAM_DEPTH, INIT_FILE, isRegistered, isAXI)
}


class BRAMPort(implicit val cfg: BRAMConfig) extends Bundle {

  val CLK = if(cfg.isAXI) Some(Input(Bool())) else None
  val RST = if(cfg.isAXI) Some(Input(Bool())) else None
  val EN = Input(Bool())
  val WE = Input(UInt(cfg.NB_COL.W))
  val ADDR = Input(UInt(log2Ceil(cfg.RAM_DEPTH).W))
  val DI = Input(UInt((cfg.NB_COL*cfg.COL_WIDTH).W))
  val DO = Output(UInt((cfg.NB_COL*cfg.COL_WIDTH).W))
  def <>(other: BRAMPort) = {
    if(cfg.isAXI) CLK.get <> other.CLK.getOrElse(DontCare)
    if(cfg.isAXI) RST.get <> other.RST.getOrElse(DontCare)
    EN <> other.EN
    WE <> other.WE
    DI <> other.DI
    DO <> other.DO
      (cfg.isAXI, other.cfg.isAXI) match {
      case (true,false) => (ADDR >> log2Ceil(cfg.NB_COL).U) <> other.ADDR
      case (false,true) => ADDR <> (other.ADDR >> log2Ceil(cfg.NB_COL))
      case (true,true)  => ADDR <> other.ADDR
      case (false,false) => ADDR <> other.ADDR
    }
  }
}

class BRAM(implicit cfg: BRAMConfig) extends MultiIOModule {
  val portA = IO(new BRAMPort)
  val portB = IO(new BRAMPort)
  private val bramTDP = Module(new BRAMTDP)

  bramTDP.io.clk <> this.clock.asUInt
  bramTDP.io.rst <> this.reset.asUInt

  bramTDP.io.addra <> portA.ADDR
  bramTDP.io.dina <> portA.DI
  bramTDP.io.wea <> portA.WE
  bramTDP.io.ena <> portA.EN
  bramTDP.io.regcea <> true.B
  bramTDP.io.douta <> portA.DO

  bramTDP.io.addrb <> portB.ADDR
  bramTDP.io.dinb <> portB.DI
  bramTDP.io.web <> portB.WE
  bramTDP.io.enb <> portB.EN
  bramTDP.io.regceb <> true.B
  bramTDP.io.doutb <> portB.DO
}

