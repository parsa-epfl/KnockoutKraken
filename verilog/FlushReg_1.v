module FlushReg_1(
  input         clock,
  input         reset,
  output        io_enq_ready,
  input         io_enq_valid,
  input         io_enq_bits_rd_valid,
  input  [4:0]  io_enq_bits_rd_bits,
  input  [4:0]  io_enq_bits_rs1,
  input  [4:0]  io_enq_bits_rs2,
  input  [25:0] io_enq_bits_imm,
  input         io_enq_bits_shift_val_valid,
  input  [5:0]  io_enq_bits_shift_val_bits,
  input  [1:0]  io_enq_bits_shift_type,
  input         io_enq_bits_cond_valid,
  input  [3:0]  io_enq_bits_cond_bits,
  input         io_enq_bits_is32bit,
  input  [4:0]  io_enq_bits_itype,
  input  [3:0]  io_enq_bits_op,
  input         io_enq_bits_nzcv_valid,
  input  [3:0]  io_enq_bits_nzcv_bits,
  input         io_enq_bits_tag,
  input         io_enq_bits_inst32_valid,
  input  [31:0] io_enq_bits_inst32_bits,
  input  [63:0] io_enq_bits_pc,
  input         io_deq_ready,
  output        io_deq_valid,
  output        io_deq_bits_rd_valid,
  output [4:0]  io_deq_bits_rd_bits,
  output [4:0]  io_deq_bits_rs1,
  output [4:0]  io_deq_bits_rs2,
  output [25:0] io_deq_bits_imm,
  output        io_deq_bits_shift_val_valid,
  output [5:0]  io_deq_bits_shift_val_bits,
  output [1:0]  io_deq_bits_shift_type,
  output        io_deq_bits_cond_valid,
  output [3:0]  io_deq_bits_cond_bits,
  output        io_deq_bits_is32bit,
  output [4:0]  io_deq_bits_itype,
  output [3:0]  io_deq_bits_op,
  output        io_deq_bits_nzcv_valid,
  output [3:0]  io_deq_bits_nzcv_bits,
  output        io_deq_bits_tag,
  output        io_deq_bits_inst32_valid,
  output [31:0] io_deq_bits_inst32_bits,
  output [63:0] io_deq_bits_pc,
  input         io_flush
);
  reg  reg_rd_valid; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_0;
  reg [4:0] reg_rd_bits; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_1;
  reg [4:0] reg_rs1; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_2;
  reg [4:0] reg_rs2; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_3;
  reg [25:0] reg_imm; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_4;
  reg  reg_shift_val_valid; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_5;
  reg [5:0] reg_shift_val_bits; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_6;
  reg [1:0] reg_shift_type; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_7;
  reg  reg_cond_valid; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_8;
  reg [3:0] reg_cond_bits; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_9;
  reg  reg_is32bit; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_10;
  reg [4:0] reg_itype; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_11;
  reg [3:0] reg_op; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_12;
  reg  reg_nzcv_valid; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_13;
  reg [3:0] reg_nzcv_bits; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_14;
  reg  reg_tag; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_15;
  reg  reg_inst32_valid; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_16;
  reg [31:0] reg_inst32_bits; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_17;
  reg [63:0] reg_pc; // @[FlushReg.scala 23:16]
  reg [63:0] _RAND_18;
  reg  valid; // @[FlushReg.scala 24:22]
  reg [31:0] _RAND_19;
  wire  _T; // @[FlushReg.scala 26:25]
  wire  _T_1; // @[FlushReg.scala 26:32]
  wire  do_enq; // @[FlushReg.scala 26:48]
  wire  _T_3; // @[FlushReg.scala 35:28]
  assign _T = ~valid; // @[FlushReg.scala 26:25]
  assign _T_1 = _T | io_deq_ready; // @[FlushReg.scala 26:32]
  assign do_enq = _T_1 | io_flush; // @[FlushReg.scala 26:48]
  assign _T_3 = ~io_flush; // @[FlushReg.scala 35:28]
  assign io_enq_ready = _T_1 | io_flush; // @[FlushReg.scala 32:16]
  assign io_deq_valid = valid & _T_3; // @[FlushReg.scala 35:16]
  assign io_deq_bits_rd_valid = reg_rd_valid; // @[FlushReg.scala 34:15]
  assign io_deq_bits_rd_bits = reg_rd_bits; // @[FlushReg.scala 34:15]
  assign io_deq_bits_rs1 = reg_rs1; // @[FlushReg.scala 34:15]
  assign io_deq_bits_rs2 = reg_rs2; // @[FlushReg.scala 34:15]
  assign io_deq_bits_imm = reg_imm; // @[FlushReg.scala 34:15]
  assign io_deq_bits_shift_val_valid = reg_shift_val_valid; // @[FlushReg.scala 34:15]
  assign io_deq_bits_shift_val_bits = reg_shift_val_bits; // @[FlushReg.scala 34:15]
  assign io_deq_bits_shift_type = reg_shift_type; // @[FlushReg.scala 34:15]
  assign io_deq_bits_cond_valid = reg_cond_valid; // @[FlushReg.scala 34:15]
  assign io_deq_bits_cond_bits = reg_cond_bits; // @[FlushReg.scala 34:15]
  assign io_deq_bits_is32bit = reg_is32bit; // @[FlushReg.scala 34:15]
  assign io_deq_bits_itype = reg_itype; // @[FlushReg.scala 34:15]
  assign io_deq_bits_op = reg_op; // @[FlushReg.scala 34:15]
  assign io_deq_bits_nzcv_valid = reg_nzcv_valid; // @[FlushReg.scala 34:15]
  assign io_deq_bits_nzcv_bits = reg_nzcv_bits; // @[FlushReg.scala 34:15]
  assign io_deq_bits_tag = reg_tag; // @[FlushReg.scala 34:15]
  assign io_deq_bits_inst32_valid = reg_inst32_valid; // @[FlushReg.scala 34:15]
  assign io_deq_bits_inst32_bits = reg_inst32_bits; // @[FlushReg.scala 34:15]
  assign io_deq_bits_pc = reg_pc; // @[FlushReg.scala 34:15]
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
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
  `ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  reg_rd_valid = _RAND_0[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  reg_rd_bits = _RAND_1[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  reg_rs1 = _RAND_2[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  reg_rs2 = _RAND_3[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  reg_imm = _RAND_4[25:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  reg_shift_val_valid = _RAND_5[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  reg_shift_val_bits = _RAND_6[5:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  reg_shift_type = _RAND_7[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  reg_cond_valid = _RAND_8[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  reg_cond_bits = _RAND_9[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  reg_is32bit = _RAND_10[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {1{`RANDOM}};
  reg_itype = _RAND_11[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_12 = {1{`RANDOM}};
  reg_op = _RAND_12[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_13 = {1{`RANDOM}};
  reg_nzcv_valid = _RAND_13[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_14 = {1{`RANDOM}};
  reg_nzcv_bits = _RAND_14[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_15 = {1{`RANDOM}};
  reg_tag = _RAND_15[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_16 = {1{`RANDOM}};
  reg_inst32_valid = _RAND_16[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_17 = {1{`RANDOM}};
  reg_inst32_bits = _RAND_17[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_18 = {2{`RANDOM}};
  reg_pc = _RAND_18[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_19 = {1{`RANDOM}};
  valid = _RAND_19[0:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (do_enq) begin
      reg_rd_valid <= io_enq_bits_rd_valid;
    end
    if (do_enq) begin
      reg_rd_bits <= io_enq_bits_rd_bits;
    end
    if (do_enq) begin
      reg_rs1 <= io_enq_bits_rs1;
    end
    if (do_enq) begin
      reg_rs2 <= io_enq_bits_rs2;
    end
    if (do_enq) begin
      reg_imm <= io_enq_bits_imm;
    end
    if (do_enq) begin
      reg_shift_val_valid <= io_enq_bits_shift_val_valid;
    end
    if (do_enq) begin
      reg_shift_val_bits <= io_enq_bits_shift_val_bits;
    end
    if (do_enq) begin
      reg_shift_type <= io_enq_bits_shift_type;
    end
    if (do_enq) begin
      reg_cond_valid <= io_enq_bits_cond_valid;
    end
    if (do_enq) begin
      reg_cond_bits <= io_enq_bits_cond_bits;
    end
    if (do_enq) begin
      reg_is32bit <= io_enq_bits_is32bit;
    end
    if (do_enq) begin
      reg_itype <= io_enq_bits_itype;
    end
    if (do_enq) begin
      reg_op <= io_enq_bits_op;
    end
    if (do_enq) begin
      reg_nzcv_valid <= io_enq_bits_nzcv_valid;
    end
    if (do_enq) begin
      reg_nzcv_bits <= io_enq_bits_nzcv_bits;
    end
    if (do_enq) begin
      reg_tag <= io_enq_bits_tag;
    end
    if (do_enq) begin
      reg_inst32_valid <= io_enq_bits_inst32_valid;
    end
    if (do_enq) begin
      reg_inst32_bits <= io_enq_bits_inst32_bits;
    end
    if (do_enq) begin
      reg_pc <= io_enq_bits_pc;
    end
    if (reset) begin
      valid <= 1'h0;
    end else if (do_enq) begin
      valid <= io_enq_valid;
    end
  end
endmodule
