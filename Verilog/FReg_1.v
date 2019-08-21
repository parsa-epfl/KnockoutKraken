module FReg_1( // @[:@2560.2]
  input         clock, // @[:@2561.4]
  input         reset, // @[:@2562.4]
  input         io_enq_valid, // @[:@2563.4]
  input  [63:0] io_enq_bits_res, // @[:@2563.4]
  input  [4:0]  io_enq_bits_rd, // @[:@2563.4]
  input         io_enq_bits_rd_en, // @[:@2563.4]
  input         io_enq_bits_tag, // @[:@2563.4]
  input  [3:0]  io_enq_bits_nzcv, // @[:@2563.4]
  input         io_enq_bits_nzcv_en, // @[:@2563.4]
  output        io_deq_valid, // @[:@2563.4]
  output [63:0] io_deq_bits_res, // @[:@2563.4]
  output [4:0]  io_deq_bits_rd, // @[:@2563.4]
  output        io_deq_bits_rd_en, // @[:@2563.4]
  output        io_deq_bits_tag, // @[:@2563.4]
  output [3:0]  io_deq_bits_nzcv, // @[:@2563.4]
  output        io_deq_bits_nzcv_en, // @[:@2563.4]
  input         io_flush // @[:@2563.4]
);
  reg [63:0] _T_34_res; // @[FReg.scala 45:24:@2565.4]
  reg [63:0] _RAND_0;
  reg [4:0] _T_34_rd; // @[FReg.scala 45:24:@2565.4]
  reg [31:0] _RAND_1;
  reg  _T_34_rd_en; // @[FReg.scala 45:24:@2565.4]
  reg [31:0] _RAND_2;
  reg  _T_34_tag; // @[FReg.scala 45:24:@2565.4]
  reg [31:0] _RAND_3;
  reg [3:0] _T_34_nzcv; // @[FReg.scala 45:24:@2565.4]
  reg [31:0] _RAND_4;
  reg  _T_34_nzcv_en; // @[FReg.scala 45:24:@2565.4]
  reg [31:0] _RAND_5;
  reg  _T_37; // @[FReg.scala 46:30:@2566.4]
  reg [31:0] _RAND_6;
  wire  _T_45; // @[FReg.scala 57:28:@2588.4]
  assign _T_45 = io_flush == 1'h0; // @[FReg.scala 57:28:@2588.4]
  assign io_deq_valid = _T_37 & _T_45; // @[FReg.scala 57:16:@2590.4]
  assign io_deq_bits_res = _T_34_res; // @[FReg.scala 56:15:@2587.4]
  assign io_deq_bits_rd = _T_34_rd; // @[FReg.scala 56:15:@2586.4]
  assign io_deq_bits_rd_en = _T_34_rd_en; // @[FReg.scala 56:15:@2585.4]
  assign io_deq_bits_tag = _T_34_tag; // @[FReg.scala 56:15:@2584.4]
  assign io_deq_bits_nzcv = _T_34_nzcv; // @[FReg.scala 56:15:@2583.4]
  assign io_deq_bits_nzcv_en = _T_34_nzcv_en; // @[FReg.scala 56:15:@2582.4]
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
  _RAND_0 = {2{`RANDOM}};
  _T_34_res = _RAND_0[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  _T_34_rd = _RAND_1[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  _T_34_rd_en = _RAND_2[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  _T_34_tag = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  _T_34_nzcv = _RAND_4[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  _T_34_nzcv_en = _RAND_5[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  _T_37 = _RAND_6[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    _T_34_res <= io_enq_bits_res;
    _T_34_rd <= io_enq_bits_rd;
    _T_34_rd_en <= io_enq_bits_rd_en;
    _T_34_tag <= io_enq_bits_tag;
    _T_34_nzcv <= io_enq_bits_nzcv;
    _T_34_nzcv_en <= io_enq_bits_nzcv_en;
    if (reset) begin
      _T_37 <= 1'h0;
    end else begin
      _T_37 <= io_enq_valid;
    end
  end
endmodule
