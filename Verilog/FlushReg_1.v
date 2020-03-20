module FlushReg_1( // @[:@1178.2]
  input         clock, // @[:@1179.4]
  input         reset, // @[:@1180.4]
  output        io_enq_ready, // @[:@1181.4]
  input         io_enq_valid, // @[:@1181.4]
  input         io_enq_bits_rd_valid, // @[:@1181.4]
  input  [4:0]  io_enq_bits_rd_bits, // @[:@1181.4]
  input  [4:0]  io_enq_bits_rs1_bits, // @[:@1181.4]
  input         io_enq_bits_rs2_valid, // @[:@1181.4]
  input  [4:0]  io_enq_bits_rs2_bits, // @[:@1181.4]
  input  [25:0] io_enq_bits_imm_bits, // @[:@1181.4]
  input         io_enq_bits_shift_val_valid, // @[:@1181.4]
  input  [5:0]  io_enq_bits_shift_val_bits, // @[:@1181.4]
  input  [1:0]  io_enq_bits_shift_type, // @[:@1181.4]
  input  [3:0]  io_enq_bits_cond_bits, // @[:@1181.4]
  input  [2:0]  io_enq_bits_itype, // @[:@1181.4]
  input  [2:0]  io_enq_bits_op, // @[:@1181.4]
  input         io_enq_bits_nzcv_en, // @[:@1181.4]
  input         io_enq_bits_tag, // @[:@1181.4]
  input  [63:0] io_enq_bits_pc, // @[:@1181.4]
  input         io_deq_ready, // @[:@1181.4]
  output        io_deq_valid, // @[:@1181.4]
  output        io_deq_bits_rd_valid, // @[:@1181.4]
  output [4:0]  io_deq_bits_rd_bits, // @[:@1181.4]
  output [4:0]  io_deq_bits_rs1_bits, // @[:@1181.4]
  output        io_deq_bits_rs2_valid, // @[:@1181.4]
  output [4:0]  io_deq_bits_rs2_bits, // @[:@1181.4]
  output [25:0] io_deq_bits_imm_bits, // @[:@1181.4]
  output        io_deq_bits_shift_val_valid, // @[:@1181.4]
  output [5:0]  io_deq_bits_shift_val_bits, // @[:@1181.4]
  output [1:0]  io_deq_bits_shift_type, // @[:@1181.4]
  output [3:0]  io_deq_bits_cond_bits, // @[:@1181.4]
  output [2:0]  io_deq_bits_itype, // @[:@1181.4]
  output [2:0]  io_deq_bits_op, // @[:@1181.4]
  output        io_deq_bits_nzcv_en, // @[:@1181.4]
  output        io_deq_bits_tag, // @[:@1181.4]
  output [63:0] io_deq_bits_pc, // @[:@1181.4]
  input         io_flush // @[:@1181.4]
);
  reg  _T_162_rd_valid; // @[FlushReg.scala 23:24:@1183.4]
  reg [31:0] _RAND_0;
  reg [4:0] _T_162_rd_bits; // @[FlushReg.scala 23:24:@1183.4]
  reg [31:0] _RAND_1;
  reg [4:0] _T_162_rs1_bits; // @[FlushReg.scala 23:24:@1183.4]
  reg [31:0] _RAND_2;
  reg  _T_162_rs2_valid; // @[FlushReg.scala 23:24:@1183.4]
  reg [31:0] _RAND_3;
  reg [4:0] _T_162_rs2_bits; // @[FlushReg.scala 23:24:@1183.4]
  reg [31:0] _RAND_4;
  reg [25:0] _T_162_imm_bits; // @[FlushReg.scala 23:24:@1183.4]
  reg [31:0] _RAND_5;
  reg  _T_162_shift_val_valid; // @[FlushReg.scala 23:24:@1183.4]
  reg [31:0] _RAND_6;
  reg [5:0] _T_162_shift_val_bits; // @[FlushReg.scala 23:24:@1183.4]
  reg [31:0] _RAND_7;
  reg [1:0] _T_162_shift_type; // @[FlushReg.scala 23:24:@1183.4]
  reg [31:0] _RAND_8;
  reg [3:0] _T_162_cond_bits; // @[FlushReg.scala 23:24:@1183.4]
  reg [31:0] _RAND_9;
  reg [2:0] _T_162_itype; // @[FlushReg.scala 23:24:@1183.4]
  reg [31:0] _RAND_10;
  reg [2:0] _T_162_op; // @[FlushReg.scala 23:24:@1183.4]
  reg [31:0] _RAND_11;
  reg  _T_162_nzcv_en; // @[FlushReg.scala 23:24:@1183.4]
  reg [31:0] _RAND_12;
  reg  _T_162_tag; // @[FlushReg.scala 23:24:@1183.4]
  reg [31:0] _RAND_13;
  reg [63:0] _T_162_pc; // @[FlushReg.scala 23:24:@1183.4]
  reg [63:0] _RAND_14;
  reg  _T_185; // @[FlushReg.scala 24:30:@1184.4]
  reg [31:0] _RAND_15;
  wire  _T_187; // @[FlushReg.scala 26:33:@1185.4]
  wire  _T_188; // @[FlushReg.scala 26:40:@1186.4]
  wire  _T_189; // @[FlushReg.scala 26:56:@1187.4]
  wire  _GEN_20; // @[FlushReg.scala 28:17:@1190.4]
  wire  _T_193; // @[FlushReg.scala 35:28:@1234.4]
  assign _T_187 = _T_185 == 1'h0; // @[FlushReg.scala 26:33:@1185.4]
  assign _T_188 = _T_187 | io_deq_ready; // @[FlushReg.scala 26:40:@1186.4]
  assign _T_189 = _T_188 | io_flush; // @[FlushReg.scala 26:56:@1187.4]
  assign _GEN_20 = _T_189 ? io_enq_valid : _T_185; // @[FlushReg.scala 28:17:@1190.4]
  assign _T_193 = io_flush == 1'h0; // @[FlushReg.scala 35:28:@1234.4]
  assign io_enq_ready = _T_188 | io_flush; // @[FlushReg.scala 32:16:@1213.4]
  assign io_deq_valid = _T_185 & _T_193; // @[FlushReg.scala 35:16:@1236.4]
  assign io_deq_bits_rd_valid = _T_162_rd_valid; // @[FlushReg.scala 34:15:@1233.4]
  assign io_deq_bits_rd_bits = _T_162_rd_bits; // @[FlushReg.scala 34:15:@1232.4]
  assign io_deq_bits_rs1_bits = _T_162_rs1_bits; // @[FlushReg.scala 34:15:@1230.4]
  assign io_deq_bits_rs2_valid = _T_162_rs2_valid; // @[FlushReg.scala 34:15:@1229.4]
  assign io_deq_bits_rs2_bits = _T_162_rs2_bits; // @[FlushReg.scala 34:15:@1228.4]
  assign io_deq_bits_imm_bits = _T_162_imm_bits; // @[FlushReg.scala 34:15:@1226.4]
  assign io_deq_bits_shift_val_valid = _T_162_shift_val_valid; // @[FlushReg.scala 34:15:@1225.4]
  assign io_deq_bits_shift_val_bits = _T_162_shift_val_bits; // @[FlushReg.scala 34:15:@1224.4]
  assign io_deq_bits_shift_type = _T_162_shift_type; // @[FlushReg.scala 34:15:@1223.4]
  assign io_deq_bits_cond_bits = _T_162_cond_bits; // @[FlushReg.scala 34:15:@1221.4]
  assign io_deq_bits_itype = _T_162_itype; // @[FlushReg.scala 34:15:@1220.4]
  assign io_deq_bits_op = _T_162_op; // @[FlushReg.scala 34:15:@1219.4]
  assign io_deq_bits_nzcv_en = _T_162_nzcv_en; // @[FlushReg.scala 34:15:@1218.4]
  assign io_deq_bits_tag = _T_162_tag; // @[FlushReg.scala 34:15:@1217.4]
  assign io_deq_bits_pc = _T_162_pc; // @[FlushReg.scala 34:15:@1214.4]
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
  _T_162_rd_valid = _RAND_0[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  _T_162_rd_bits = _RAND_1[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  _T_162_rs1_bits = _RAND_2[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  _T_162_rs2_valid = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  _T_162_rs2_bits = _RAND_4[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  _T_162_imm_bits = _RAND_5[25:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  _T_162_shift_val_valid = _RAND_6[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  _T_162_shift_val_bits = _RAND_7[5:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  _T_162_shift_type = _RAND_8[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  _T_162_cond_bits = _RAND_9[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  _T_162_itype = _RAND_10[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {1{`RANDOM}};
  _T_162_op = _RAND_11[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_12 = {1{`RANDOM}};
  _T_162_nzcv_en = _RAND_12[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_13 = {1{`RANDOM}};
  _T_162_tag = _RAND_13[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_14 = {2{`RANDOM}};
  _T_162_pc = _RAND_14[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_15 = {1{`RANDOM}};
  _T_185 = _RAND_15[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (_T_189) begin
      _T_162_rd_valid <= io_enq_bits_rd_valid;
    end
    if (_T_189) begin
      _T_162_rd_bits <= io_enq_bits_rd_bits;
    end
    if (_T_189) begin
      _T_162_rs1_bits <= io_enq_bits_rs1_bits;
    end
    if (_T_189) begin
      _T_162_rs2_valid <= io_enq_bits_rs2_valid;
    end
    if (_T_189) begin
      _T_162_rs2_bits <= io_enq_bits_rs2_bits;
    end
    if (_T_189) begin
      _T_162_imm_bits <= io_enq_bits_imm_bits;
    end
    if (_T_189) begin
      _T_162_shift_val_valid <= io_enq_bits_shift_val_valid;
    end
    if (_T_189) begin
      _T_162_shift_val_bits <= io_enq_bits_shift_val_bits;
    end
    if (_T_189) begin
      _T_162_shift_type <= io_enq_bits_shift_type;
    end
    if (_T_189) begin
      _T_162_cond_bits <= io_enq_bits_cond_bits;
    end
    if (_T_189) begin
      _T_162_itype <= io_enq_bits_itype;
    end
    if (_T_189) begin
      _T_162_op <= io_enq_bits_op;
    end
    if (_T_189) begin
      _T_162_nzcv_en <= io_enq_bits_nzcv_en;
    end
    if (_T_189) begin
      _T_162_tag <= io_enq_bits_tag;
    end
    if (_T_189) begin
      _T_162_pc <= io_enq_bits_pc;
    end
    if (reset) begin
      _T_185 <= 1'h0;
    end else begin
      if (_T_189) begin
        _T_185 <= io_enq_valid;
      end
    end
  end
endmodule
