module FlushReg_2(
  input         clock,
  input         reset,
  output        io_enq_ready,
  input         io_enq_valid,
  input         io_enq_bits_exe_valid,
  input         io_enq_bits_exe_bits_rd_valid,
  input  [4:0]  io_enq_bits_exe_bits_rd_bits,
  input         io_enq_bits_exe_bits_nzcv_valid,
  input  [3:0]  io_enq_bits_exe_bits_nzcv_bits,
  input  [63:0] io_enq_bits_exe_bits_res,
  input         io_enq_bits_br_valid,
  input  [63:0] io_enq_bits_br_bits_pc,
  input         io_enq_bits_br_bits_unalignedExcp,
  input         io_enq_bits_pcrel_valid,
  input  [4:0]  io_enq_bits_pcrel_bits_rd,
  input  [63:0] io_enq_bits_pcrel_bits_res,
  input         io_enq_bits_mem_valid,
  input  [1:0]  io_enq_bits_mem_bits_size,
  input         io_enq_bits_mem_bits_isPair,
  input         io_enq_bits_mem_bits_isLoad,
  input  [63:0] io_enq_bits_mem_bits_memReq_0_addr,
  input  [4:0]  io_enq_bits_mem_bits_memReq_0_reg,
  input  [63:0] io_enq_bits_mem_bits_memReq_1_addr,
  input  [4:0]  io_enq_bits_mem_bits_memReq_1_reg,
  input  [63:0] io_enq_bits_mem_bits_rd_res,
  input         io_enq_bits_mem_bits_rd_valid,
  input  [4:0]  io_enq_bits_mem_bits_rd_bits,
  input         io_enq_bits_mem_bits_unalignedExcpSP,
  input         io_enq_bits_undef,
  input         io_enq_bits_tag,
  input         io_deq_ready,
  output        io_deq_valid,
  output        io_deq_bits_exe_valid,
  output        io_deq_bits_exe_bits_rd_valid,
  output [4:0]  io_deq_bits_exe_bits_rd_bits,
  output        io_deq_bits_exe_bits_nzcv_valid,
  output [3:0]  io_deq_bits_exe_bits_nzcv_bits,
  output [63:0] io_deq_bits_exe_bits_res,
  output        io_deq_bits_br_valid,
  output [63:0] io_deq_bits_br_bits_pc,
  output        io_deq_bits_br_bits_unalignedExcp,
  output        io_deq_bits_pcrel_valid,
  output [4:0]  io_deq_bits_pcrel_bits_rd,
  output [63:0] io_deq_bits_pcrel_bits_res,
  output        io_deq_bits_mem_valid,
  output [1:0]  io_deq_bits_mem_bits_size,
  output        io_deq_bits_mem_bits_isPair,
  output        io_deq_bits_mem_bits_isLoad,
  output [63:0] io_deq_bits_mem_bits_memReq_0_addr,
  output [4:0]  io_deq_bits_mem_bits_memReq_0_reg,
  output [63:0] io_deq_bits_mem_bits_memReq_1_addr,
  output [4:0]  io_deq_bits_mem_bits_memReq_1_reg,
  output [63:0] io_deq_bits_mem_bits_rd_res,
  output        io_deq_bits_mem_bits_rd_valid,
  output [4:0]  io_deq_bits_mem_bits_rd_bits,
  output        io_deq_bits_mem_bits_unalignedExcpSP,
  output        io_deq_bits_undef,
  output        io_deq_bits_tag,
  input         io_flush
);
  reg  reg_exe_valid; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_0;
  reg  reg_exe_bits_rd_valid; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_1;
  reg [4:0] reg_exe_bits_rd_bits; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_2;
  reg  reg_exe_bits_nzcv_valid; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_3;
  reg [3:0] reg_exe_bits_nzcv_bits; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_4;
  reg [63:0] reg_exe_bits_res; // @[FlushReg.scala 23:16]
  reg [63:0] _RAND_5;
  reg  reg_br_valid; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_6;
  reg [63:0] reg_br_bits_pc; // @[FlushReg.scala 23:16]
  reg [63:0] _RAND_7;
  reg  reg_br_bits_unalignedExcp; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_8;
  reg  reg_pcrel_valid; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_9;
  reg [4:0] reg_pcrel_bits_rd; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_10;
  reg [63:0] reg_pcrel_bits_res; // @[FlushReg.scala 23:16]
  reg [63:0] _RAND_11;
  reg  reg_mem_valid; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_12;
  reg [1:0] reg_mem_bits_size; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_13;
  reg  reg_mem_bits_isPair; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_14;
  reg  reg_mem_bits_isLoad; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_15;
  reg [63:0] reg_mem_bits_memReq_0_addr; // @[FlushReg.scala 23:16]
  reg [63:0] _RAND_16;
  reg [4:0] reg_mem_bits_memReq_0_reg; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_17;
  reg [63:0] reg_mem_bits_memReq_1_addr; // @[FlushReg.scala 23:16]
  reg [63:0] _RAND_18;
  reg [4:0] reg_mem_bits_memReq_1_reg; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_19;
  reg [63:0] reg_mem_bits_rd_res; // @[FlushReg.scala 23:16]
  reg [63:0] _RAND_20;
  reg  reg_mem_bits_rd_valid; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_21;
  reg [4:0] reg_mem_bits_rd_bits; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_22;
  reg  reg_mem_bits_unalignedExcpSP; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_23;
  reg  reg_undef; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_24;
  reg  reg_tag; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_25;
  reg  valid; // @[FlushReg.scala 24:22]
  reg [31:0] _RAND_26;
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
  assign io_deq_bits_exe_valid = reg_exe_valid; // @[FlushReg.scala 34:15]
  assign io_deq_bits_exe_bits_rd_valid = reg_exe_bits_rd_valid; // @[FlushReg.scala 34:15]
  assign io_deq_bits_exe_bits_rd_bits = reg_exe_bits_rd_bits; // @[FlushReg.scala 34:15]
  assign io_deq_bits_exe_bits_nzcv_valid = reg_exe_bits_nzcv_valid; // @[FlushReg.scala 34:15]
  assign io_deq_bits_exe_bits_nzcv_bits = reg_exe_bits_nzcv_bits; // @[FlushReg.scala 34:15]
  assign io_deq_bits_exe_bits_res = reg_exe_bits_res; // @[FlushReg.scala 34:15]
  assign io_deq_bits_br_valid = reg_br_valid; // @[FlushReg.scala 34:15]
  assign io_deq_bits_br_bits_pc = reg_br_bits_pc; // @[FlushReg.scala 34:15]
  assign io_deq_bits_br_bits_unalignedExcp = reg_br_bits_unalignedExcp; // @[FlushReg.scala 34:15]
  assign io_deq_bits_pcrel_valid = reg_pcrel_valid; // @[FlushReg.scala 34:15]
  assign io_deq_bits_pcrel_bits_rd = reg_pcrel_bits_rd; // @[FlushReg.scala 34:15]
  assign io_deq_bits_pcrel_bits_res = reg_pcrel_bits_res; // @[FlushReg.scala 34:15]
  assign io_deq_bits_mem_valid = reg_mem_valid; // @[FlushReg.scala 34:15]
  assign io_deq_bits_mem_bits_size = reg_mem_bits_size; // @[FlushReg.scala 34:15]
  assign io_deq_bits_mem_bits_isPair = reg_mem_bits_isPair; // @[FlushReg.scala 34:15]
  assign io_deq_bits_mem_bits_isLoad = reg_mem_bits_isLoad; // @[FlushReg.scala 34:15]
  assign io_deq_bits_mem_bits_memReq_0_addr = reg_mem_bits_memReq_0_addr; // @[FlushReg.scala 34:15]
  assign io_deq_bits_mem_bits_memReq_0_reg = reg_mem_bits_memReq_0_reg; // @[FlushReg.scala 34:15]
  assign io_deq_bits_mem_bits_memReq_1_addr = reg_mem_bits_memReq_1_addr; // @[FlushReg.scala 34:15]
  assign io_deq_bits_mem_bits_memReq_1_reg = reg_mem_bits_memReq_1_reg; // @[FlushReg.scala 34:15]
  assign io_deq_bits_mem_bits_rd_res = reg_mem_bits_rd_res; // @[FlushReg.scala 34:15]
  assign io_deq_bits_mem_bits_rd_valid = reg_mem_bits_rd_valid; // @[FlushReg.scala 34:15]
  assign io_deq_bits_mem_bits_rd_bits = reg_mem_bits_rd_bits; // @[FlushReg.scala 34:15]
  assign io_deq_bits_mem_bits_unalignedExcpSP = reg_mem_bits_unalignedExcpSP; // @[FlushReg.scala 34:15]
  assign io_deq_bits_undef = reg_undef; // @[FlushReg.scala 34:15]
  assign io_deq_bits_tag = reg_tag; // @[FlushReg.scala 34:15]
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
  reg_exe_valid = _RAND_0[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  reg_exe_bits_rd_valid = _RAND_1[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  reg_exe_bits_rd_bits = _RAND_2[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  reg_exe_bits_nzcv_valid = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  reg_exe_bits_nzcv_bits = _RAND_4[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {2{`RANDOM}};
  reg_exe_bits_res = _RAND_5[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  reg_br_valid = _RAND_6[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {2{`RANDOM}};
  reg_br_bits_pc = _RAND_7[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  reg_br_bits_unalignedExcp = _RAND_8[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  reg_pcrel_valid = _RAND_9[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  reg_pcrel_bits_rd = _RAND_10[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {2{`RANDOM}};
  reg_pcrel_bits_res = _RAND_11[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_12 = {1{`RANDOM}};
  reg_mem_valid = _RAND_12[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_13 = {1{`RANDOM}};
  reg_mem_bits_size = _RAND_13[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_14 = {1{`RANDOM}};
  reg_mem_bits_isPair = _RAND_14[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_15 = {1{`RANDOM}};
  reg_mem_bits_isLoad = _RAND_15[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_16 = {2{`RANDOM}};
  reg_mem_bits_memReq_0_addr = _RAND_16[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_17 = {1{`RANDOM}};
  reg_mem_bits_memReq_0_reg = _RAND_17[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_18 = {2{`RANDOM}};
  reg_mem_bits_memReq_1_addr = _RAND_18[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_19 = {1{`RANDOM}};
  reg_mem_bits_memReq_1_reg = _RAND_19[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_20 = {2{`RANDOM}};
  reg_mem_bits_rd_res = _RAND_20[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_21 = {1{`RANDOM}};
  reg_mem_bits_rd_valid = _RAND_21[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_22 = {1{`RANDOM}};
  reg_mem_bits_rd_bits = _RAND_22[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_23 = {1{`RANDOM}};
  reg_mem_bits_unalignedExcpSP = _RAND_23[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_24 = {1{`RANDOM}};
  reg_undef = _RAND_24[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_25 = {1{`RANDOM}};
  reg_tag = _RAND_25[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_26 = {1{`RANDOM}};
  valid = _RAND_26[0:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (do_enq) begin
      reg_exe_valid <= io_enq_bits_exe_valid;
    end
    if (do_enq) begin
      reg_exe_bits_rd_valid <= io_enq_bits_exe_bits_rd_valid;
    end
    if (do_enq) begin
      reg_exe_bits_rd_bits <= io_enq_bits_exe_bits_rd_bits;
    end
    if (do_enq) begin
      reg_exe_bits_nzcv_valid <= io_enq_bits_exe_bits_nzcv_valid;
    end
    if (do_enq) begin
      reg_exe_bits_nzcv_bits <= io_enq_bits_exe_bits_nzcv_bits;
    end
    if (do_enq) begin
      reg_exe_bits_res <= io_enq_bits_exe_bits_res;
    end
    if (do_enq) begin
      reg_br_valid <= io_enq_bits_br_valid;
    end
    if (do_enq) begin
      reg_br_bits_pc <= io_enq_bits_br_bits_pc;
    end
    if (do_enq) begin
      reg_br_bits_unalignedExcp <= io_enq_bits_br_bits_unalignedExcp;
    end
    if (do_enq) begin
      reg_pcrel_valid <= io_enq_bits_pcrel_valid;
    end
    if (do_enq) begin
      reg_pcrel_bits_rd <= io_enq_bits_pcrel_bits_rd;
    end
    if (do_enq) begin
      reg_pcrel_bits_res <= io_enq_bits_pcrel_bits_res;
    end
    if (do_enq) begin
      reg_mem_valid <= io_enq_bits_mem_valid;
    end
    if (do_enq) begin
      reg_mem_bits_size <= io_enq_bits_mem_bits_size;
    end
    if (do_enq) begin
      reg_mem_bits_isPair <= io_enq_bits_mem_bits_isPair;
    end
    if (do_enq) begin
      reg_mem_bits_isLoad <= io_enq_bits_mem_bits_isLoad;
    end
    if (do_enq) begin
      reg_mem_bits_memReq_0_addr <= io_enq_bits_mem_bits_memReq_0_addr;
    end
    if (do_enq) begin
      reg_mem_bits_memReq_0_reg <= io_enq_bits_mem_bits_memReq_0_reg;
    end
    if (do_enq) begin
      reg_mem_bits_memReq_1_addr <= io_enq_bits_mem_bits_memReq_1_addr;
    end
    if (do_enq) begin
      reg_mem_bits_memReq_1_reg <= io_enq_bits_mem_bits_memReq_1_reg;
    end
    if (do_enq) begin
      reg_mem_bits_rd_res <= io_enq_bits_mem_bits_rd_res;
    end
    if (do_enq) begin
      reg_mem_bits_rd_valid <= io_enq_bits_mem_bits_rd_valid;
    end
    if (do_enq) begin
      reg_mem_bits_rd_bits <= io_enq_bits_mem_bits_rd_bits;
    end
    if (do_enq) begin
      reg_mem_bits_unalignedExcpSP <= io_enq_bits_mem_bits_unalignedExcpSP;
    end
    if (do_enq) begin
      reg_undef <= io_enq_bits_undef;
    end
    if (do_enq) begin
      reg_tag <= io_enq_bits_tag;
    end
    if (reset) begin
      valid <= 1'h0;
    end else if (do_enq) begin
      valid <= io_enq_valid;
    end
  end
endmodule
