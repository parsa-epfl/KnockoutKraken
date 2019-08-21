module BRAMTDP( // @[:@301.2]
  input         clock, // @[:@302.4]
  input         io_portA_writeEn, // @[:@304.4]
  input         io_portA_en, // @[:@304.4]
  input  [9:0]  io_portA_addr, // @[:@304.4]
  input  [35:0] io_portA_dataIn, // @[:@304.4]
  output [35:0] io_portA_dataOut, // @[:@304.4]
  input         io_portB_writeEn, // @[:@304.4]
  input         io_portB_en, // @[:@304.4]
  input  [9:0]  io_portB_addr, // @[:@304.4]
  input  [35:0] io_portB_dataIn, // @[:@304.4]
  output [35:0] io_portB_dataOut // @[:@304.4]
);
  wire [35:0] blackBoxBRAMTDP_doB; // @[bram.scala 169:31:@306.4]
  wire [35:0] blackBoxBRAMTDP_doA; // @[bram.scala 169:31:@306.4]
  wire [35:0] blackBoxBRAMTDP_diB; // @[bram.scala 169:31:@306.4]
  wire [35:0] blackBoxBRAMTDP_diA; // @[bram.scala 169:31:@306.4]
  wire [9:0] blackBoxBRAMTDP_addrB; // @[bram.scala 169:31:@306.4]
  wire [9:0] blackBoxBRAMTDP_addrA; // @[bram.scala 169:31:@306.4]
  wire  blackBoxBRAMTDP_weB; // @[bram.scala 169:31:@306.4]
  wire  blackBoxBRAMTDP_weA; // @[bram.scala 169:31:@306.4]
  wire  blackBoxBRAMTDP_enB; // @[bram.scala 169:31:@306.4]
  wire  blackBoxBRAMTDP_enA; // @[bram.scala 169:31:@306.4]
  wire  blackBoxBRAMTDP_clk; // @[bram.scala 169:31:@306.4]
  BlackBoxBRAMTDP #(.SIZE_B(1024), .ADDR_WIDTH_A(10), .DATA_WIDTH_A(36), .DATA_WIDTH_B(36), .SIZE_A(1024), .ADDR_WIDTH_B(10)) blackBoxBRAMTDP ( // @[bram.scala 169:31:@306.4]
    .doB(blackBoxBRAMTDP_doB),
    .doA(blackBoxBRAMTDP_doA),
    .diB(blackBoxBRAMTDP_diB),
    .diA(blackBoxBRAMTDP_diA),
    .addrB(blackBoxBRAMTDP_addrB),
    .addrA(blackBoxBRAMTDP_addrA),
    .weB(blackBoxBRAMTDP_weB),
    .weA(blackBoxBRAMTDP_weA),
    .enB(blackBoxBRAMTDP_enB),
    .enA(blackBoxBRAMTDP_enA),
    .clk(blackBoxBRAMTDP_clk)
  );
  assign io_portA_dataOut = blackBoxBRAMTDP_doA; // @[bram.scala 177:28:@323.4]
  assign io_portB_dataOut = blackBoxBRAMTDP_doB; // @[bram.scala 184:28:@328.4]
  assign blackBoxBRAMTDP_diB = io_portB_dataIn; // @[bram.scala 183:28:@327.4]
  assign blackBoxBRAMTDP_diA = io_portA_dataIn; // @[bram.scala 176:28:@322.4]
  assign blackBoxBRAMTDP_addrB = io_portB_addr; // @[bram.scala 182:28:@326.4]
  assign blackBoxBRAMTDP_addrA = io_portA_addr; // @[bram.scala 175:28:@321.4]
  assign blackBoxBRAMTDP_weB = io_portB_writeEn; // @[bram.scala 180:28:@324.4]
  assign blackBoxBRAMTDP_weA = io_portA_writeEn; // @[bram.scala 173:28:@319.4]
  assign blackBoxBRAMTDP_enB = io_portB_en; // @[bram.scala 181:28:@325.4]
  assign blackBoxBRAMTDP_enA = io_portA_en; // @[bram.scala 174:28:@320.4]
  assign blackBoxBRAMTDP_clk = clock; // @[bram.scala 170:26:@318.4]
endmodule
