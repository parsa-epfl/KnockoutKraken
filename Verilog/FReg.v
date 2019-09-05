module FReg( // @[:@962.2]
  input         clock, // @[:@963.4]
  input         reset, // @[:@964.4]
  output        io_enq_ready, // @[:@965.4]
  input         io_enq_valid, // @[:@965.4]
  input         io_enq_bits_rd_valid, // @[:@965.4]
  input  [4:0]  io_enq_bits_rd_bits, // @[:@965.4]
  input  [4:0]  io_enq_bits_rs1_bits, // @[:@965.4]
  input         io_enq_bits_rs2_valid, // @[:@965.4]
  input  [4:0]  io_enq_bits_rs2_bits, // @[:@965.4]
  input  [25:0] io_enq_bits_imm_bits, // @[:@965.4]
  input         io_enq_bits_shift_val_valid, // @[:@965.4]
  input  [5:0]  io_enq_bits_shift_val_bits, // @[:@965.4]
  input  [1:0]  io_enq_bits_shift_type, // @[:@965.4]
  input  [3:0]  io_enq_bits_cond_bits, // @[:@965.4]
  input  [2:0]  io_enq_bits_itype, // @[:@965.4]
  input  [2:0]  io_enq_bits_op, // @[:@965.4]
  input         io_enq_bits_nzcv_en, // @[:@965.4]
  input         io_enq_bits_tag, // @[:@965.4]
  input  [63:0] io_enq_bits_pc, // @[:@965.4]
  input         io_deq_ready, // @[:@965.4]
  output        io_deq_valid, // @[:@965.4]
  output        io_deq_bits_rd_valid, // @[:@965.4]
  output [4:0]  io_deq_bits_rd_bits, // @[:@965.4]
  output [4:0]  io_deq_bits_rs1_bits, // @[:@965.4]
  output        io_deq_bits_rs2_valid, // @[:@965.4]
  output [4:0]  io_deq_bits_rs2_bits, // @[:@965.4]
  output [25:0] io_deq_bits_imm_bits, // @[:@965.4]
  output        io_deq_bits_shift_val_valid, // @[:@965.4]
  output [5:0]  io_deq_bits_shift_val_bits, // @[:@965.4]
  output [1:0]  io_deq_bits_shift_type, // @[:@965.4]
  output [3:0]  io_deq_bits_cond_bits, // @[:@965.4]
  output [2:0]  io_deq_bits_itype, // @[:@965.4]
  output [2:0]  io_deq_bits_op, // @[:@965.4]
  output        io_deq_bits_nzcv_en, // @[:@965.4]
  output        io_deq_bits_tag, // @[:@965.4]
  output [63:0] io_deq_bits_pc, // @[:@965.4]
  input         io_flush // @[:@965.4]
);
  reg  _T_254_rd_valid; // @[FReg.scala 45:24:@967.4]
  reg [31:0] _RAND_0;
  reg [4:0] _T_254_rd_bits; // @[FReg.scala 45:24:@967.4]
  reg [31:0] _RAND_1;
  reg [4:0] _T_254_rs1_bits; // @[FReg.scala 45:24:@967.4]
  reg [31:0] _RAND_2;
  reg  _T_254_rs2_valid; // @[FReg.scala 45:24:@967.4]
  reg [31:0] _RAND_3;
  reg [4:0] _T_254_rs2_bits; // @[FReg.scala 45:24:@967.4]
  reg [31:0] _RAND_4;
  reg [25:0] _T_254_imm_bits; // @[FReg.scala 45:24:@967.4]
  reg [31:0] _RAND_5;
  reg  _T_254_shift_val_valid; // @[FReg.scala 45:24:@967.4]
  reg [31:0] _RAND_6;
  reg [5:0] _T_254_shift_val_bits; // @[FReg.scala 45:24:@967.4]
  reg [31:0] _RAND_7;
  reg [1:0] _T_254_shift_type; // @[FReg.scala 45:24:@967.4]
  reg [31:0] _RAND_8;
  reg [3:0] _T_254_cond_bits; // @[FReg.scala 45:24:@967.4]
  reg [31:0] _RAND_9;
  reg [2:0] _T_254_itype; // @[FReg.scala 45:24:@967.4]
  reg [31:0] _RAND_10;
  reg [2:0] _T_254_op; // @[FReg.scala 45:24:@967.4]
  reg [31:0] _RAND_11;
  reg  _T_254_nzcv_en; // @[FReg.scala 45:24:@967.4]
  reg [31:0] _RAND_12;
  reg  _T_254_tag; // @[FReg.scala 45:24:@967.4]
  reg [31:0] _RAND_13;
  reg [63:0] _T_254_pc; // @[FReg.scala 45:24:@967.4]
  reg [63:0] _RAND_14;
  reg  _T_277; // @[FReg.scala 46:30:@968.4]
  reg [31:0] _RAND_15;
  wire  _T_279; // @[FReg.scala 48:33:@969.4]
  wire  _T_280; // @[FReg.scala 48:40:@970.4]
  wire  _T_281; // @[FReg.scala 48:56:@971.4]
  wire  _GEN_20; // @[FReg.scala 50:17:@974.4]
  wire  _T_285; // @[FReg.scala 57:28:@1018.4]
  assign _T_279 = _T_277 == 1'h0; // @[FReg.scala 48:33:@969.4]
  assign _T_280 = _T_279 | io_deq_ready; // @[FReg.scala 48:40:@970.4]
  assign _T_281 = _T_280 | io_flush; // @[FReg.scala 48:56:@971.4]
  assign _GEN_20 = _T_281 ? io_enq_valid : _T_277; // @[FReg.scala 50:17:@974.4]
  assign _T_285 = io_flush == 1'h0; // @[FReg.scala 57:28:@1018.4]
  assign io_enq_ready = _T_280 | io_flush; // @[FReg.scala 54:16:@997.4]
  assign io_deq_valid = _T_277 & _T_285; // @[FReg.scala 57:16:@1020.4]
  assign io_deq_bits_rd_valid = _T_254_rd_valid; // @[FReg.scala 56:15:@1017.4]
  assign io_deq_bits_rd_bits = _T_254_rd_bits; // @[FReg.scala 56:15:@1016.4]
  assign io_deq_bits_rs1_bits = _T_254_rs1_bits; // @[FReg.scala 56:15:@1014.4]
  assign io_deq_bits_rs2_valid = _T_254_rs2_valid; // @[FReg.scala 56:15:@1013.4]
  assign io_deq_bits_rs2_bits = _T_254_rs2_bits; // @[FReg.scala 56:15:@1012.4]
  assign io_deq_bits_imm_bits = _T_254_imm_bits; // @[FReg.scala 56:15:@1010.4]
  assign io_deq_bits_shift_val_valid = _T_254_shift_val_valid; // @[FReg.scala 56:15:@1009.4]
  assign io_deq_bits_shift_val_bits = _T_254_shift_val_bits; // @[FReg.scala 56:15:@1008.4]
  assign io_deq_bits_shift_type = _T_254_shift_type; // @[FReg.scala 56:15:@1007.4]
  assign io_deq_bits_cond_bits = _T_254_cond_bits; // @[FReg.scala 56:15:@1005.4]
  assign io_deq_bits_itype = _T_254_itype; // @[FReg.scala 56:15:@1004.4]
  assign io_deq_bits_op = _T_254_op; // @[FReg.scala 56:15:@1003.4]
  assign io_deq_bits_nzcv_en = _T_254_nzcv_en; // @[FReg.scala 56:15:@1002.4]
  assign io_deq_bits_tag = _T_254_tag; // @[FReg.scala 56:15:@1001.4]
  assign io_deq_bits_pc = _T_254_pc; // @[FReg.scala 56:15:@998.4]
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
  _T_254_rd_valid = _RAND_0[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  _T_254_rd_bits = _RAND_1[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  _T_254_rs1_bits = _RAND_2[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  _T_254_rs2_valid = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  _T_254_rs2_bits = _RAND_4[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  _T_254_imm_bits = _RAND_5[25:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  _T_254_shift_val_valid = _RAND_6[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  _T_254_shift_val_bits = _RAND_7[5:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  _T_254_shift_type = _RAND_8[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  _T_254_cond_bits = _RAND_9[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  _T_254_itype = _RAND_10[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {1{`RANDOM}};
  _T_254_op = _RAND_11[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_12 = {1{`RANDOM}};
  _T_254_nzcv_en = _RAND_12[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_13 = {1{`RANDOM}};
  _T_254_tag = _RAND_13[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_14 = {2{`RANDOM}};
  _T_254_pc = _RAND_14[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_15 = {1{`RANDOM}};
  _T_277 = _RAND_15[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (_T_281) begin
      _T_254_rd_valid <= io_enq_bits_rd_valid;
    end
    if (_T_281) begin
      _T_254_rd_bits <= io_enq_bits_rd_bits;
    end
    if (_T_281) begin
      _T_254_rs1_bits <= io_enq_bits_rs1_bits;
    end
    if (_T_281) begin
      _T_254_rs2_valid <= io_enq_bits_rs2_valid;
    end
    if (_T_281) begin
      _T_254_rs2_bits <= io_enq_bits_rs2_bits;
    end
    if (_T_281) begin
      _T_254_imm_bits <= io_enq_bits_imm_bits;
    end
    if (_T_281) begin
      _T_254_shift_val_valid <= io_enq_bits_shift_val_valid;
    end
    if (_T_281) begin
      _T_254_shift_val_bits <= io_enq_bits_shift_val_bits;
    end
    if (_T_281) begin
      _T_254_shift_type <= io_enq_bits_shift_type;
    end
    if (_T_281) begin
      _T_254_cond_bits <= io_enq_bits_cond_bits;
    end
    if (_T_281) begin
      _T_254_itype <= io_enq_bits_itype;
    end
    if (_T_281) begin
      _T_254_op <= io_enq_bits_op;
    end
    if (_T_281) begin
      _T_254_nzcv_en <= io_enq_bits_nzcv_en;
    end
    if (_T_281) begin
      _T_254_tag <= io_enq_bits_tag;
    end
    if (_T_281) begin
      _T_254_pc <= io_enq_bits_pc;
    end
    if (reset) begin
      _T_277 <= 1'h0;
    end else begin
      if (_T_281) begin
        _T_277 <= io_enq_valid;
      end
    end
  end
endmodule
