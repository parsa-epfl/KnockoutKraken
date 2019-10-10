module FlushReg_2( // @[:@3131.2]
  input         clock, // @[:@3132.4]
  input         reset, // @[:@3133.4]
  input         io_enq_valid, // @[:@3134.4]
  input         io_enq_bits_exe_valid, // @[:@3134.4]
  input         io_enq_bits_exe_bits_rd_valid, // @[:@3134.4]
  input  [4:0]  io_enq_bits_exe_bits_rd_bits, // @[:@3134.4]
  input         io_enq_bits_exe_bits_nzcv_valid, // @[:@3134.4]
  input  [3:0]  io_enq_bits_exe_bits_nzcv_bits, // @[:@3134.4]
  input  [63:0] io_enq_bits_exe_bits_res, // @[:@3134.4]
  input         io_enq_bits_br_valid, // @[:@3134.4]
  input  [63:0] io_enq_bits_br_bits_offset, // @[:@3134.4]
  input         io_enq_bits_undef, // @[:@3134.4]
  input         io_enq_bits_tag, // @[:@3134.4]
  output        io_deq_valid, // @[:@3134.4]
  output        io_deq_bits_exe_valid, // @[:@3134.4]
  output        io_deq_bits_exe_bits_rd_valid, // @[:@3134.4]
  output [4:0]  io_deq_bits_exe_bits_rd_bits, // @[:@3134.4]
  output        io_deq_bits_exe_bits_nzcv_valid, // @[:@3134.4]
  output [3:0]  io_deq_bits_exe_bits_nzcv_bits, // @[:@3134.4]
  output [63:0] io_deq_bits_exe_bits_res, // @[:@3134.4]
  output        io_deq_bits_br_valid, // @[:@3134.4]
  output [63:0] io_deq_bits_br_bits_offset, // @[:@3134.4]
  output        io_deq_bits_undef, // @[:@3134.4]
  output        io_deq_bits_tag // @[:@3134.4]
);
  reg  _T_190_exe_valid; // @[FlushReg.scala 23:24:@3136.4]
  reg [31:0] _RAND_0;
  reg  _T_190_exe_bits_rd_valid; // @[FlushReg.scala 23:24:@3136.4]
  reg [31:0] _RAND_1;
  reg [4:0] _T_190_exe_bits_rd_bits; // @[FlushReg.scala 23:24:@3136.4]
  reg [31:0] _RAND_2;
  reg  _T_190_exe_bits_nzcv_valid; // @[FlushReg.scala 23:24:@3136.4]
  reg [31:0] _RAND_3;
  reg [3:0] _T_190_exe_bits_nzcv_bits; // @[FlushReg.scala 23:24:@3136.4]
  reg [31:0] _RAND_4;
  reg [63:0] _T_190_exe_bits_res; // @[FlushReg.scala 23:24:@3136.4]
  reg [63:0] _RAND_5;
  reg  _T_190_br_valid; // @[FlushReg.scala 23:24:@3136.4]
  reg [31:0] _RAND_6;
  reg [63:0] _T_190_br_bits_offset; // @[FlushReg.scala 23:24:@3136.4]
  reg [63:0] _RAND_7;
  reg  _T_190_undef; // @[FlushReg.scala 23:24:@3136.4]
  reg [31:0] _RAND_8;
  reg  _T_190_tag; // @[FlushReg.scala 23:24:@3136.4]
  reg [31:0] _RAND_9;
  reg  _T_217; // @[FlushReg.scala 24:30:@3137.4]
  reg [31:0] _RAND_10;
  assign io_deq_valid = _T_217; // @[FlushReg.scala 35:16:@3177.4]
  assign io_deq_bits_exe_valid = _T_190_exe_valid; // @[FlushReg.scala 34:15:@3174.4]
  assign io_deq_bits_exe_bits_rd_valid = _T_190_exe_bits_rd_valid; // @[FlushReg.scala 34:15:@3173.4]
  assign io_deq_bits_exe_bits_rd_bits = _T_190_exe_bits_rd_bits; // @[FlushReg.scala 34:15:@3172.4]
  assign io_deq_bits_exe_bits_nzcv_valid = _T_190_exe_bits_nzcv_valid; // @[FlushReg.scala 34:15:@3171.4]
  assign io_deq_bits_exe_bits_nzcv_bits = _T_190_exe_bits_nzcv_bits; // @[FlushReg.scala 34:15:@3170.4]
  assign io_deq_bits_exe_bits_res = _T_190_exe_bits_res; // @[FlushReg.scala 34:15:@3169.4]
  assign io_deq_bits_br_valid = _T_190_br_valid; // @[FlushReg.scala 34:15:@3168.4]
  assign io_deq_bits_br_bits_offset = _T_190_br_bits_offset; // @[FlushReg.scala 34:15:@3167.4]
  assign io_deq_bits_undef = _T_190_undef; // @[FlushReg.scala 34:15:@3162.4]
  assign io_deq_bits_tag = _T_190_tag; // @[FlushReg.scala 34:15:@3161.4]
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
  _T_190_exe_valid = _RAND_0[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  _T_190_exe_bits_rd_valid = _RAND_1[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  _T_190_exe_bits_rd_bits = _RAND_2[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  _T_190_exe_bits_nzcv_valid = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  _T_190_exe_bits_nzcv_bits = _RAND_4[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {2{`RANDOM}};
  _T_190_exe_bits_res = _RAND_5[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  _T_190_br_valid = _RAND_6[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {2{`RANDOM}};
  _T_190_br_bits_offset = _RAND_7[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  _T_190_undef = _RAND_8[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  _T_190_tag = _RAND_9[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  _T_217 = _RAND_10[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    _T_190_exe_valid <= io_enq_bits_exe_valid;
    _T_190_exe_bits_rd_valid <= io_enq_bits_exe_bits_rd_valid;
    _T_190_exe_bits_rd_bits <= io_enq_bits_exe_bits_rd_bits;
    _T_190_exe_bits_nzcv_valid <= io_enq_bits_exe_bits_nzcv_valid;
    _T_190_exe_bits_nzcv_bits <= io_enq_bits_exe_bits_nzcv_bits;
    _T_190_exe_bits_res <= io_enq_bits_exe_bits_res;
    _T_190_br_valid <= io_enq_bits_br_valid;
    _T_190_br_bits_offset <= io_enq_bits_br_bits_offset;
    _T_190_undef <= io_enq_bits_undef;
    _T_190_tag <= io_enq_bits_tag;
    if (reset) begin
      _T_217 <= 1'h0;
    end else begin
      _T_217 <= io_enq_valid;
    end
  end
endmodule
