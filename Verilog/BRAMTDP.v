module BRAMTDP( // @[:@139.2]
  input         clock, // @[:@140.4]
  input         io_portA_writeEn, // @[:@142.4]
  input         io_portA_en, // @[:@142.4]
  input  [9:0]  io_portA_addr, // @[:@142.4]
  input  [35:0] io_portA_dataIn, // @[:@142.4]
  output [35:0] io_portA_dataOut, // @[:@142.4]
  input         io_portB_writeEn, // @[:@142.4]
  input         io_portB_en, // @[:@142.4]
  input  [9:0]  io_portB_addr, // @[:@142.4]
  input  [35:0] io_portB_dataIn, // @[:@142.4]
  output [35:0] io_portB_dataOut // @[:@142.4]
);
  wire [35:0] blackBoxBRAMTDP_doB; // @[bram.scala 187:31:@144.4]
  wire [35:0] blackBoxBRAMTDP_doA; // @[bram.scala 187:31:@144.4]
  wire [35:0] blackBoxBRAMTDP_diB; // @[bram.scala 187:31:@144.4]
  wire [35:0] blackBoxBRAMTDP_diA; // @[bram.scala 187:31:@144.4]
  wire [9:0] blackBoxBRAMTDP_addrB; // @[bram.scala 187:31:@144.4]
  wire [9:0] blackBoxBRAMTDP_addrA; // @[bram.scala 187:31:@144.4]
  wire  blackBoxBRAMTDP_weB; // @[bram.scala 187:31:@144.4]
  wire  blackBoxBRAMTDP_weA; // @[bram.scala 187:31:@144.4]
  wire  blackBoxBRAMTDP_enB; // @[bram.scala 187:31:@144.4]
  wire  blackBoxBRAMTDP_enA; // @[bram.scala 187:31:@144.4]
  wire  blackBoxBRAMTDP_clk; // @[bram.scala 187:31:@144.4]
  BlackBoxBRAMTDP #(.SIZE_B(1024), .ADDR_WIDTH_A(10), .DATA_WIDTH_A(36), .DATA_WIDTH_B(36), .SIZE_A(1024), .ADDR_WIDTH_B(10)) blackBoxBRAMTDP ( // @[bram.scala 187:31:@144.4]
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
  assign io_portA_dataOut = blackBoxBRAMTDP_doA; // @[bram.scala 195:28:@161.4]
  assign io_portB_dataOut = blackBoxBRAMTDP_doB; // @[bram.scala 202:28:@166.4]
  assign blackBoxBRAMTDP_diB = io_portB_dataIn; // @[bram.scala 201:28:@165.4]
  assign blackBoxBRAMTDP_diA = io_portA_dataIn; // @[bram.scala 194:28:@160.4]
  assign blackBoxBRAMTDP_addrB = io_portB_addr; // @[bram.scala 200:28:@164.4]
  assign blackBoxBRAMTDP_addrA = io_portA_addr; // @[bram.scala 193:28:@159.4]
  assign blackBoxBRAMTDP_weB = io_portB_writeEn; // @[bram.scala 198:28:@162.4]
  assign blackBoxBRAMTDP_weA = io_portA_writeEn; // @[bram.scala 191:28:@157.4]
  assign blackBoxBRAMTDP_enB = io_portB_en; // @[bram.scala 199:28:@163.4]
  assign blackBoxBRAMTDP_enA = io_portA_en; // @[bram.scala 192:28:@158.4]
  assign blackBoxBRAMTDP_clk = clock; // @[bram.scala 188:26:@156.4]
endmodule
