module BRAM( // @[:@330.2]
  input         clock, // @[:@331.4]
  input         io_portA_writeEn, // @[:@333.4]
  input         io_portA_en, // @[:@333.4]
  input  [9:0]  io_portA_addr, // @[:@333.4]
  input  [35:0] io_portA_dataIn, // @[:@333.4]
  output [35:0] io_portA_dataOut, // @[:@333.4]
  input         io_portB_writeEn, // @[:@333.4]
  input  [9:0]  io_portB_addr, // @[:@333.4]
  input  [35:0] io_portB_dataIn, // @[:@333.4]
  output [35:0] io_portB_dataOut // @[:@333.4]
);
  wire  bram_clock; // @[bram.scala 151:20:@335.4]
  wire  bram_io_portA_writeEn; // @[bram.scala 151:20:@335.4]
  wire  bram_io_portA_en; // @[bram.scala 151:20:@335.4]
  wire [9:0] bram_io_portA_addr; // @[bram.scala 151:20:@335.4]
  wire [35:0] bram_io_portA_dataIn; // @[bram.scala 151:20:@335.4]
  wire [35:0] bram_io_portA_dataOut; // @[bram.scala 151:20:@335.4]
  wire  bram_io_portB_writeEn; // @[bram.scala 151:20:@335.4]
  wire  bram_io_portB_en; // @[bram.scala 151:20:@335.4]
  wire [9:0] bram_io_portB_addr; // @[bram.scala 151:20:@335.4]
  wire [35:0] bram_io_portB_dataIn; // @[bram.scala 151:20:@335.4]
  wire [35:0] bram_io_portB_dataOut; // @[bram.scala 151:20:@335.4]
  BRAMTDP bram ( // @[bram.scala 151:20:@335.4]
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
  assign io_portA_dataOut = bram_io_portA_dataOut; // @[bram.scala 158:6:@343.4]
  assign io_portB_dataOut = bram_io_portB_dataOut; // @[bram.scala 158:6:@338.4]
  assign bram_clock = clock; // @[:@336.4]
  assign bram_io_portA_writeEn = io_portA_writeEn; // @[bram.scala 158:6:@347.4]
  assign bram_io_portA_en = io_portA_en; // @[bram.scala 158:6:@346.4]
  assign bram_io_portA_addr = io_portA_addr; // @[bram.scala 158:6:@345.4]
  assign bram_io_portA_dataIn = io_portA_dataIn; // @[bram.scala 158:6:@344.4]
  assign bram_io_portB_writeEn = io_portB_writeEn; // @[bram.scala 158:6:@342.4]
  assign bram_io_portB_en = 1'h1; // @[bram.scala 158:6:@341.4]
  assign bram_io_portB_addr = io_portB_addr; // @[bram.scala 158:6:@340.4]
  assign bram_io_portB_dataIn = io_portB_dataIn; // @[bram.scala 158:6:@339.4]
endmodule
