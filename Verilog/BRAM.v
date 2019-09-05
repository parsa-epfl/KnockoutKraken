module BRAM( // @[:@168.2]
  input         clock, // @[:@169.4]
  input         io_portA_writeEn, // @[:@171.4]
  input         io_portA_en, // @[:@171.4]
  input  [9:0]  io_portA_addr, // @[:@171.4]
  input  [35:0] io_portA_dataIn, // @[:@171.4]
  output [35:0] io_portA_dataOut, // @[:@171.4]
  input         io_portB_writeEn, // @[:@171.4]
  input  [9:0]  io_portB_addr, // @[:@171.4]
  input  [35:0] io_portB_dataIn, // @[:@171.4]
  output [35:0] io_portB_dataOut // @[:@171.4]
);
  wire  bram_clock; // @[bram.scala 169:20:@173.4]
  wire  bram_io_portA_writeEn; // @[bram.scala 169:20:@173.4]
  wire  bram_io_portA_en; // @[bram.scala 169:20:@173.4]
  wire [9:0] bram_io_portA_addr; // @[bram.scala 169:20:@173.4]
  wire [35:0] bram_io_portA_dataIn; // @[bram.scala 169:20:@173.4]
  wire [35:0] bram_io_portA_dataOut; // @[bram.scala 169:20:@173.4]
  wire  bram_io_portB_writeEn; // @[bram.scala 169:20:@173.4]
  wire  bram_io_portB_en; // @[bram.scala 169:20:@173.4]
  wire [9:0] bram_io_portB_addr; // @[bram.scala 169:20:@173.4]
  wire [35:0] bram_io_portB_dataIn; // @[bram.scala 169:20:@173.4]
  wire [35:0] bram_io_portB_dataOut; // @[bram.scala 169:20:@173.4]
  BRAMTDP bram ( // @[bram.scala 169:20:@173.4]
    .clock(bram_clock),
    .io_portA_writeEn(bram_io_portA_writeEn),
    .io_portA_en(bram_io_portA_en),
    .io_portA_addr(bram_io_portA_addr),
    .io_portA_dataIn(bram_io_portA_dataIn),
    .io_portA_dataOut(bram_io_portA_dataOut),
    .io_portB_writeEn(bram_io_portB_writeEn),
    .io_portB_en(bram_io_portB_en),
    .io_portB_addr(bram_io_portB_addr),
    .io_portB_dataIn(bram_io_portB_dataIn),
    .io_portB_dataOut(bram_io_portB_dataOut)
  );
  assign io_portA_dataOut = bram_io_portA_dataOut; // @[bram.scala 176:6:@181.4]
  assign io_portB_dataOut = bram_io_portB_dataOut; // @[bram.scala 176:6:@176.4]
  assign bram_clock = clock; // @[:@174.4]
  assign bram_io_portA_writeEn = io_portA_writeEn; // @[bram.scala 176:6:@185.4]
  assign bram_io_portA_en = io_portA_en; // @[bram.scala 176:6:@184.4]
  assign bram_io_portA_addr = io_portA_addr; // @[bram.scala 176:6:@183.4]
  assign bram_io_portA_dataIn = io_portA_dataIn; // @[bram.scala 176:6:@182.4]
  assign bram_io_portB_writeEn = io_portB_writeEn; // @[bram.scala 176:6:@180.4]
  assign bram_io_portB_en = 1'h1; // @[bram.scala 176:6:@179.4]
  assign bram_io_portB_addr = io_portB_addr; // @[bram.scala 176:6:@178.4]
  assign bram_io_portB_dataIn = io_portB_dataIn; // @[bram.scala 176:6:@177.4]
endmodule
