module SRAM( // @[:@2887.2]
  input         clock, // @[:@2888.4]
  input  [7:0]  io_addr_r, // @[:@2890.4]
  input  [7:0]  io_addr_w, // @[:@2890.4]
  input  [27:0] io_data_in, // @[:@2890.4]
  output [27:0] io_data_out // @[:@2890.4]
);
  reg [27:0] mem [0:255]; // @[TLBLegacy.scala 124:24:@2892.4]
  reg [31:0] _RAND_0;
  wire [27:0] mem__T_24_data; // @[TLBLegacy.scala 124:24:@2892.4]
  wire [7:0] mem__T_24_addr; // @[TLBLegacy.scala 124:24:@2892.4]
  wire [27:0] mem__T_17_data; // @[TLBLegacy.scala 124:24:@2892.4]
  wire [7:0] mem__T_17_addr; // @[TLBLegacy.scala 124:24:@2892.4]
  wire  mem__T_17_mask; // @[TLBLegacy.scala 124:24:@2892.4]
  wire  mem__T_17_en; // @[TLBLegacy.scala 124:24:@2892.4]
  wire  _GEN_3; // @[TLBLegacy.scala 125:17:@2893.4]
  reg [7:0] mem__T_24_addr_pipe_0;
  reg [31:0] _RAND_1;
  assign mem__T_24_addr = mem__T_24_addr_pipe_0;
  assign mem__T_24_data = mem[mem__T_24_addr]; // @[TLBLegacy.scala 124:24:@2892.4]
  assign mem__T_17_data = io_data_in;
  assign mem__T_17_addr = io_addr_w;
  assign mem__T_17_mask = 1'h1;
  assign mem__T_17_en = 1'h0;
  assign _GEN_3 = 1'h1; // @[TLBLegacy.scala 125:17:@2893.4]
  assign io_data_out = mem__T_24_data; // @[TLBLegacy.scala 128:15:@2905.4]
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE
  integer initvar;
  initial begin
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      #0.002 begin end
    `endif
  _RAND_0 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 256; initvar = initvar+1)
    mem[initvar] = _RAND_0[27:0];
  `endif // RANDOMIZE_MEM_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  mem__T_24_addr_pipe_0 = _RAND_1[7:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if(mem__T_17_en & mem__T_17_mask) begin
      mem[mem__T_17_addr] <= mem__T_17_data; // @[TLBLegacy.scala 124:24:@2892.4]
    end
    if (_GEN_3) begin
      mem__T_24_addr_pipe_0 <= io_addr_r;
    end
  end
endmodule
