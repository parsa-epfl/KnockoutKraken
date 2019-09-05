module FReg_1( // @[:@2562.2]
  input         clock, // @[:@2563.4]
  input         reset, // @[:@2564.4]
  input         io_enq_valid, // @[:@2565.4]
  input         io_enq_bits_rd_valid, // @[:@2565.4]
  input  [4:0]  io_enq_bits_rd_bits, // @[:@2565.4]
  input         io_enq_bits_nzcv_valid, // @[:@2565.4]
  input  [3:0]  io_enq_bits_nzcv_bits, // @[:@2565.4]
  input         io_enq_bits_tag, // @[:@2565.4]
  input  [63:0] io_enq_bits_res, // @[:@2565.4]
  output        io_deq_valid, // @[:@2565.4]
  output        io_deq_bits_rd_valid, // @[:@2565.4]
  output [4:0]  io_deq_bits_rd_bits, // @[:@2565.4]
  output        io_deq_bits_nzcv_valid, // @[:@2565.4]
  output [3:0]  io_deq_bits_nzcv_bits, // @[:@2565.4]
  output        io_deq_bits_tag, // @[:@2565.4]
  output [63:0] io_deq_bits_res, // @[:@2565.4]
  input         io_flush // @[:@2565.4]
);
  reg  _T_100_rd_valid; // @[FReg.scala 45:24:@2567.4]
  reg [31:0] _RAND_0;
  reg [4:0] _T_100_rd_bits; // @[FReg.scala 45:24:@2567.4]
  reg [31:0] _RAND_1;
  reg  _T_100_nzcv_valid; // @[FReg.scala 45:24:@2567.4]
  reg [31:0] _RAND_2;
  reg [3:0] _T_100_nzcv_bits; // @[FReg.scala 45:24:@2567.4]
  reg [31:0] _RAND_3;
  reg  _T_100_tag; // @[FReg.scala 45:24:@2567.4]
  reg [31:0] _RAND_4;
  reg [63:0] _T_100_res; // @[FReg.scala 45:24:@2567.4]
  reg [63:0] _RAND_5;
  reg  _T_109; // @[FReg.scala 46:30:@2568.4]
  reg [31:0] _RAND_6;
  wire  _T_117; // @[FReg.scala 57:28:@2590.4]
  assign _T_117 = io_flush == 1'h0; // @[FReg.scala 57:28:@2590.4]
  assign io_deq_valid = _T_109 & _T_117; // @[FReg.scala 57:16:@2592.4]
  assign io_deq_bits_rd_valid = _T_100_rd_valid; // @[FReg.scala 56:15:@2589.4]
  assign io_deq_bits_rd_bits = _T_100_rd_bits; // @[FReg.scala 56:15:@2588.4]
  assign io_deq_bits_nzcv_valid = _T_100_nzcv_valid; // @[FReg.scala 56:15:@2587.4]
  assign io_deq_bits_nzcv_bits = _T_100_nzcv_bits; // @[FReg.scala 56:15:@2586.4]
  assign io_deq_bits_tag = _T_100_tag; // @[FReg.scala 56:15:@2585.4]
  assign io_deq_bits_res = _T_100_res; // @[FReg.scala 56:15:@2584.4]
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
  `ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  _T_100_rd_valid = _RAND_0[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  _T_100_rd_bits = _RAND_1[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  _T_100_nzcv_valid = _RAND_2[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  _T_100_nzcv_bits = _RAND_3[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  _T_100_tag = _RAND_4[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {2{`RANDOM}};
  _T_100_res = _RAND_5[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  _T_109 = _RAND_6[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    _T_100_rd_valid <= io_enq_bits_rd_valid;
    _T_100_rd_bits <= io_enq_bits_rd_bits;
    _T_100_nzcv_valid <= io_enq_bits_nzcv_valid;
    _T_100_nzcv_bits <= io_enq_bits_nzcv_bits;
    _T_100_tag <= io_enq_bits_tag;
    _T_100_res <= io_enq_bits_res;
    if (reset) begin
      _T_109 <= 1'h0;
    end else begin
      _T_109 <= io_enq_valid;
    end
  end
endmodule
