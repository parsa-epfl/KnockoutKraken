module FetchUnit( // @[:@614.2]
  input         clock, // @[:@615.4]
  input         reset, // @[:@616.4]
  input         io_en, // @[:@617.4]
  input  [63:0] io_PC, // @[:@617.4]
  input         io_tagIn, // @[:@617.4]
  output        io_incr, // @[:@617.4]
  input         io_flush, // @[:@617.4]
  output [9:0]  io_ppageBRAM_addr, // @[:@617.4]
  input  [35:0] io_ppageBRAM_dataOut, // @[:@617.4]
  input         io_deq_ready, // @[:@617.4]
  output        io_deq_valid, // @[:@617.4]
  output [31:0] io_deq_bits_inst, // @[:@617.4]
  output        io_deq_bits_tag // @[:@617.4]
);
  wire [63:0] _T_57; // @[fetch.scala 37:30:@620.4]
  reg  valid; // @[fetch.scala 42:22:@624.4]
  reg [31:0] _RAND_0;
  reg  tag; // @[fetch.scala 43:20:@625.4]
  reg [31:0] _RAND_1;
  wire  readIns; // @[fetch.scala 45:30:@627.4]
  wire  _GEN_0; // @[fetch.scala 46:17:@628.4]
  wire  _GEN_1; // @[fetch.scala 46:17:@628.4]
  reg  instV_valid; // @[fetch.scala 64:22:@641.4]
  reg [31:0] _RAND_2;
  reg [31:0] instV_bits; // @[fetch.scala 64:22:@641.4]
  reg [31:0] _RAND_3;
  wire  _T_77; // @[fetch.scala 65:8:@642.4]
  reg  _T_79; // @[fetch.scala 65:32:@643.4]
  reg [31:0] _RAND_4;
  wire  _T_80; // @[fetch.scala 65:22:@645.4]
  wire  _GEN_3; // @[fetch.scala 65:48:@646.4]
  wire [35:0] _GEN_4; // @[fetch.scala 65:48:@646.4]
  wire  _GEN_5; // @[fetch.scala 70:22:@650.4]
  wire  _T_85; // @[fetch.scala 75:28:@655.4]
  wire [35:0] _T_87; // @[fetch.scala 78:26:@660.4]
  assign _T_57 = io_PC >> 2'h2; // @[fetch.scala 37:30:@620.4]
  assign readIns = io_deq_ready | io_flush; // @[fetch.scala 45:30:@627.4]
  assign _GEN_0 = readIns ? io_en : valid; // @[fetch.scala 46:17:@628.4]
  assign _GEN_1 = readIns ? io_tagIn : tag; // @[fetch.scala 46:17:@628.4]
  assign _T_77 = io_deq_ready == 1'h0; // @[fetch.scala 65:8:@642.4]
  assign _T_80 = _T_77 & _T_79; // @[fetch.scala 65:22:@645.4]
  assign _GEN_3 = _T_80 ? 1'h1 : instV_valid; // @[fetch.scala 65:48:@646.4]
  assign _GEN_4 = _T_80 ? io_ppageBRAM_dataOut : {{4'd0}, instV_bits}; // @[fetch.scala 65:48:@646.4]
  assign _GEN_5 = io_deq_ready ? 1'h0 : _GEN_3; // @[fetch.scala 70:22:@650.4]
  assign _T_85 = io_flush == 1'h0; // @[fetch.scala 75:28:@655.4]
  assign _T_87 = instV_valid ? {{4'd0}, instV_bits} : io_ppageBRAM_dataOut; // @[fetch.scala 78:26:@660.4]
  assign io_incr = io_deq_ready & io_en; // @[fetch.scala 74:11:@654.4]
  assign io_ppageBRAM_addr = _T_57[9:0]; // @[fetch.scala 37:21:@621.4]
  assign io_deq_valid = valid & _T_85; // @[fetch.scala 75:16:@657.4]
  assign io_deq_bits_inst = _T_87[31:0]; // @[fetch.scala 78:20:@661.4]
  assign io_deq_bits_tag = tag; // @[fetch.scala 76:19:@658.4]
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
  valid = _RAND_0[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  tag = _RAND_1[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  instV_valid = _RAND_2[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  instV_bits = _RAND_3[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  _T_79 = _RAND_4[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (reset) begin
      valid <= 1'h0;
    end else begin
      if (readIns) begin
        valid <= io_en;
      end
    end
    if (reset) begin
      tag <= 1'h0;
    end else begin
      if (readIns) begin
        tag <= io_tagIn;
      end
    end
    if (reset) begin
      instV_valid <= 1'h0;
    end else begin
      if (io_deq_ready) begin
        instV_valid <= 1'h0;
      end else begin
        if (_T_80) begin
          instV_valid <= 1'h1;
        end
      end
    end
    if (reset) begin
      instV_bits <= 32'h0;
    end else begin
      instV_bits <= _GEN_4[31:0];
    end
    _T_79 <= io_deq_ready;
  end
endmodule
