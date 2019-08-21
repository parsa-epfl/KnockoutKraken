module FReg( // @[:@960.2]
  input         clock, // @[:@961.4]
  input         reset, // @[:@962.4]
  output        io_enq_ready, // @[:@963.4]
  input         io_enq_valid, // @[:@963.4]
  input  [4:0]  io_enq_bits_rd, // @[:@963.4]
  input  [4:0]  io_enq_bits_rs1, // @[:@963.4]
  input  [4:0]  io_enq_bits_rs2, // @[:@963.4]
  input  [25:0] io_enq_bits_imm, // @[:@963.4]
  input  [5:0]  io_enq_bits_shift_val, // @[:@963.4]
  input  [1:0]  io_enq_bits_shift_type, // @[:@963.4]
  input  [3:0]  io_enq_bits_cond, // @[:@963.4]
  input  [2:0]  io_enq_bits_itype, // @[:@963.4]
  input  [2:0]  io_enq_bits_op, // @[:@963.4]
  input         io_enq_bits_rd_en, // @[:@963.4]
  input         io_enq_bits_rs2_en, // @[:@963.4]
  input         io_enq_bits_shift_en, // @[:@963.4]
  input         io_enq_bits_nzcv_en, // @[:@963.4]
  input         io_enq_bits_tag, // @[:@963.4]
  input         io_deq_ready, // @[:@963.4]
  output        io_deq_valid, // @[:@963.4]
  output [4:0]  io_deq_bits_rd, // @[:@963.4]
  output [4:0]  io_deq_bits_rs1, // @[:@963.4]
  output [4:0]  io_deq_bits_rs2, // @[:@963.4]
  output [25:0] io_deq_bits_imm, // @[:@963.4]
  output [5:0]  io_deq_bits_shift_val, // @[:@963.4]
  output [1:0]  io_deq_bits_shift_type, // @[:@963.4]
  output [3:0]  io_deq_bits_cond, // @[:@963.4]
  output [2:0]  io_deq_bits_itype, // @[:@963.4]
  output [2:0]  io_deq_bits_op, // @[:@963.4]
  output        io_deq_bits_rd_en, // @[:@963.4]
  output        io_deq_bits_rs2_en, // @[:@963.4]
  output        io_deq_bits_shift_en, // @[:@963.4]
  output        io_deq_bits_nzcv_en, // @[:@963.4]
  output        io_deq_bits_tag, // @[:@963.4]
  input         io_flush // @[:@963.4]
);
  reg [4:0] _T_34_rd; // @[FReg.scala 45:24:@965.4]
  reg [31:0] _RAND_0;
  reg [4:0] _T_34_rs1; // @[FReg.scala 45:24:@965.4]
  reg [31:0] _RAND_1;
  reg [4:0] _T_34_rs2; // @[FReg.scala 45:24:@965.4]
  reg [31:0] _RAND_2;
  reg [25:0] _T_34_imm; // @[FReg.scala 45:24:@965.4]
  reg [31:0] _RAND_3;
  reg [5:0] _T_34_shift_val; // @[FReg.scala 45:24:@965.4]
  reg [31:0] _RAND_4;
  reg [1:0] _T_34_shift_type; // @[FReg.scala 45:24:@965.4]
  reg [31:0] _RAND_5;
  reg [3:0] _T_34_cond; // @[FReg.scala 45:24:@965.4]
  reg [31:0] _RAND_6;
  reg [2:0] _T_34_itype; // @[FReg.scala 45:24:@965.4]
  reg [31:0] _RAND_7;
  reg [2:0] _T_34_op; // @[FReg.scala 45:24:@965.4]
  reg [31:0] _RAND_8;
  reg  _T_34_rd_en; // @[FReg.scala 45:24:@965.4]
  reg [31:0] _RAND_9;
  reg  _T_34_rs2_en; // @[FReg.scala 45:24:@965.4]
  reg [31:0] _RAND_10;
  reg  _T_34_shift_en; // @[FReg.scala 45:24:@965.4]
  reg [31:0] _RAND_11;
  reg  _T_34_nzcv_en; // @[FReg.scala 45:24:@965.4]
  reg [31:0] _RAND_12;
  reg  _T_34_tag; // @[FReg.scala 45:24:@965.4]
  reg [31:0] _RAND_13;
  reg  _T_37; // @[FReg.scala 46:30:@966.4]
  reg [31:0] _RAND_14;
  wire  _T_39; // @[FReg.scala 48:33:@967.4]
  wire  _T_40; // @[FReg.scala 48:40:@968.4]
  wire  _T_41; // @[FReg.scala 48:56:@969.4]
  wire  _GEN_20; // @[FReg.scala 50:17:@972.4]
  wire  _T_45; // @[FReg.scala 57:28:@1016.4]
  assign _T_39 = _T_37 == 1'h0; // @[FReg.scala 48:33:@967.4]
  assign _T_40 = _T_39 | io_deq_ready; // @[FReg.scala 48:40:@968.4]
  assign _T_41 = _T_40 | io_flush; // @[FReg.scala 48:56:@969.4]
  assign _GEN_20 = _T_41 ? io_enq_valid : _T_37; // @[FReg.scala 50:17:@972.4]
  assign _T_45 = io_flush == 1'h0; // @[FReg.scala 57:28:@1016.4]
  assign io_enq_ready = _T_40 | io_flush; // @[FReg.scala 54:16:@995.4]
  assign io_deq_valid = _T_37 & _T_45; // @[FReg.scala 57:16:@1018.4]
  assign io_deq_bits_rd = _T_34_rd; // @[FReg.scala 56:15:@1015.4]
  assign io_deq_bits_rs1 = _T_34_rs1; // @[FReg.scala 56:15:@1014.4]
  assign io_deq_bits_rs2 = _T_34_rs2; // @[FReg.scala 56:15:@1013.4]
  assign io_deq_bits_imm = _T_34_imm; // @[FReg.scala 56:15:@1012.4]
  assign io_deq_bits_shift_val = _T_34_shift_val; // @[FReg.scala 56:15:@1011.4]
  assign io_deq_bits_shift_type = _T_34_shift_type; // @[FReg.scala 56:15:@1010.4]
  assign io_deq_bits_cond = _T_34_cond; // @[FReg.scala 56:15:@1009.4]
  assign io_deq_bits_itype = _T_34_itype; // @[FReg.scala 56:15:@1008.4]
  assign io_deq_bits_op = _T_34_op; // @[FReg.scala 56:15:@1007.4]
  assign io_deq_bits_rd_en = _T_34_rd_en; // @[FReg.scala 56:15:@1006.4]
  assign io_deq_bits_rs2_en = _T_34_rs2_en; // @[FReg.scala 56:15:@1004.4]
  assign io_deq_bits_shift_en = _T_34_shift_en; // @[FReg.scala 56:15:@1002.4]
  assign io_deq_bits_nzcv_en = _T_34_nzcv_en; // @[FReg.scala 56:15:@1000.4]
  assign io_deq_bits_tag = _T_34_tag; // @[FReg.scala 56:15:@998.4]
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
  _T_34_rd = _RAND_0[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  _T_34_rs1 = _RAND_1[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  _T_34_rs2 = _RAND_2[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  _T_34_imm = _RAND_3[25:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  _T_34_shift_val = _RAND_4[5:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  _T_34_shift_type = _RAND_5[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  _T_34_cond = _RAND_6[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  _T_34_itype = _RAND_7[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  _T_34_op = _RAND_8[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  _T_34_rd_en = _RAND_9[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  _T_34_rs2_en = _RAND_10[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {1{`RANDOM}};
  _T_34_shift_en = _RAND_11[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_12 = {1{`RANDOM}};
  _T_34_nzcv_en = _RAND_12[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_13 = {1{`RANDOM}};
  _T_34_tag = _RAND_13[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_14 = {1{`RANDOM}};
  _T_37 = _RAND_14[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (_T_41) begin
      _T_34_rd <= io_enq_bits_rd;
    end
    if (_T_41) begin
      _T_34_rs1 <= io_enq_bits_rs1;
    end
    if (_T_41) begin
      _T_34_rs2 <= io_enq_bits_rs2;
    end
    if (_T_41) begin
      _T_34_imm <= io_enq_bits_imm;
    end
    if (_T_41) begin
      _T_34_shift_val <= io_enq_bits_shift_val;
    end
    if (_T_41) begin
      _T_34_shift_type <= io_enq_bits_shift_type;
    end
    if (_T_41) begin
      _T_34_cond <= io_enq_bits_cond;
    end
    if (_T_41) begin
      _T_34_itype <= io_enq_bits_itype;
    end
    if (_T_41) begin
      _T_34_op <= io_enq_bits_op;
    end
    if (_T_41) begin
      _T_34_rd_en <= io_enq_bits_rd_en;
    end
    if (_T_41) begin
      _T_34_rs2_en <= io_enq_bits_rs2_en;
    end
    if (_T_41) begin
      _T_34_shift_en <= io_enq_bits_shift_en;
    end
    if (_T_41) begin
      _T_34_nzcv_en <= io_enq_bits_nzcv_en;
    end
    if (_T_41) begin
      _T_34_tag <= io_enq_bits_tag;
    end
    if (reset) begin
      _T_37 <= 1'h0;
    end else begin
      if (_T_41) begin
        _T_37 <= io_enq_valid;
      end
    end
  end
endmodule
