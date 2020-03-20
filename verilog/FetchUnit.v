module FetchUnit(
  input         clock,
  input         reset,
  input         io_flush_tag,
  input         io_flush_valid,
  input         io_fire_tag,
  input         io_fire_valid,
  input         io_commitReg_valid,
  input         io_commitReg_bits_br_valid,
  input         io_commitReg_bits_tag,
  input  [63:0] io_nextPC,
  input         io_fetchEn_0,
  input         io_fetchEn_1,
  input  [63:0] io_pcVec_0,
  input  [63:0] io_pcVec_1,
  output        io_pc_tag,
  output [63:0] io_pc_bits,
  output        io_pc_valid,
  input         io_hit,
  input  [31:0] io_insn,
  input         io_deq_ready,
  output        io_deq_valid,
  output [31:0] io_deq_bits_inst,
  output        io_deq_bits_tag,
  output [63:0] io_deq_bits_pc
);
  wire  arbiter_clock; // @[Fetch.scala 38:23]
  wire  arbiter_reset; // @[Fetch.scala 38:23]
  wire [1:0] arbiter_io_ready; // @[Fetch.scala 38:23]
  wire  arbiter_io_next_ready; // @[Fetch.scala 38:23]
  wire  arbiter_io_next_valid; // @[Fetch.scala 38:23]
  wire  arbiter_io_next_bits; // @[Fetch.scala 38:23]
  wire  fetchReg_clock; // @[Fetch.scala 41:24]
  wire  fetchReg_reset; // @[Fetch.scala 41:24]
  wire  fetchReg_io_enq_ready; // @[Fetch.scala 41:24]
  wire  fetchReg_io_enq_valid; // @[Fetch.scala 41:24]
  wire [31:0] fetchReg_io_enq_bits_inst; // @[Fetch.scala 41:24]
  wire  fetchReg_io_enq_bits_tag; // @[Fetch.scala 41:24]
  wire [63:0] fetchReg_io_enq_bits_pc; // @[Fetch.scala 41:24]
  wire  fetchReg_io_deq_ready; // @[Fetch.scala 41:24]
  wire  fetchReg_io_deq_valid; // @[Fetch.scala 41:24]
  wire [31:0] fetchReg_io_deq_bits_inst; // @[Fetch.scala 41:24]
  wire  fetchReg_io_deq_bits_tag; // @[Fetch.scala 41:24]
  wire [63:0] fetchReg_io_deq_bits_pc; // @[Fetch.scala 41:24]
  wire  fetchReg_io_flush; // @[Fetch.scala 41:24]
  reg [63:0] prefetchPC_0; // @[Fetch.scala 37:27]
  reg [63:0] _RAND_0;
  reg [63:0] prefetchPC_1; // @[Fetch.scala 37:27]
  reg [63:0] _RAND_1;
  wire  _T_5; // @[Fetch.scala 43:48]
  wire  _GEN_29; // @[Fetch.scala 88:32]
  wire  readyThreads_1; // @[Fetch.scala 86:24]
  wire  _GEN_28; // @[Fetch.scala 88:32]
  wire  readyThreads_0; // @[Fetch.scala 86:24]
  wire  _T_19; // @[Fetch.scala 87:31]
  wire  _GEN_27; // @[Fetch.scala 87:49]
  wire  insnHit; // @[Fetch.scala 86:24]
  wire  _T_7; // @[Fetch.scala 50:16]
  wire [63:0] _GEN_1; // @[Fetch.scala 51:32]
  wire [63:0] _T_9; // @[Fetch.scala 51:32]
  wire [63:0] _GEN_2; // @[Fetch.scala 51:17]
  wire [63:0] _GEN_3; // @[Fetch.scala 51:17]
  wire [63:0] _GEN_4; // @[Fetch.scala 50:42]
  wire [63:0] _GEN_5; // @[Fetch.scala 50:42]
  reg  insnReg_valid; // @[Fetch.scala 58:20]
  reg [31:0] _RAND_2;
  reg [31:0] insnReg_bits_inst; // @[Fetch.scala 58:20]
  reg [31:0] _RAND_3;
  reg  insnReg_bits_tag; // @[Fetch.scala 58:20]
  reg [31:0] _RAND_4;
  reg [63:0] insnReg_bits_pc; // @[Fetch.scala 58:20]
  reg [63:0] _RAND_5;
  reg  insnReq_valid; // @[Fetch.scala 59:27]
  reg [31:0] _RAND_6;
  reg  insnReq_bits_tag; // @[Fetch.scala 61:30]
  reg [31:0] _RAND_7;
  reg [63:0] insnReq_bits_pc; // @[Fetch.scala 62:29]
  reg [63:0] _RAND_8;
  wire  _T_13; // @[Fetch.scala 64:8]
  wire  _T_14; // @[Fetch.scala 64:34]
  wire  _T_15; // @[Fetch.scala 64:31]
  wire  _T_17; // @[Fetch.scala 71:31]
  wire  _T_18; // @[Fetch.scala 73:27]
  wire  _T_20; // @[Fetch.scala 92:24]
  wire  _GEN_30; // @[Fetch.scala 94:28]
  wire  _T_21; // @[Fetch.scala 95:51]
  RRArbiter arbiter ( // @[Fetch.scala 38:23]
    .clock(arbiter_clock),
    .reset(arbiter_reset),
    .io_ready(arbiter_io_ready),
    .io_next_ready(arbiter_io_next_ready),
    .io_next_valid(arbiter_io_next_valid),
    .io_next_bits(arbiter_io_next_bits)
  );
  FlushReg fetchReg ( // @[Fetch.scala 41:24]
    .clock(fetchReg_clock),
    .reset(fetchReg_reset),
    .io_enq_ready(fetchReg_io_enq_ready),
    .io_enq_valid(fetchReg_io_enq_valid),
    .io_enq_bits_inst(fetchReg_io_enq_bits_inst),
    .io_enq_bits_tag(fetchReg_io_enq_bits_tag),
    .io_enq_bits_pc(fetchReg_io_enq_bits_pc),
    .io_deq_ready(fetchReg_io_deq_ready),
    .io_deq_valid(fetchReg_io_deq_valid),
    .io_deq_bits_inst(fetchReg_io_deq_bits_inst),
    .io_deq_bits_tag(fetchReg_io_deq_bits_tag),
    .io_deq_bits_pc(fetchReg_io_deq_bits_pc),
    .io_flush(fetchReg_io_flush)
  );
  assign _T_5 = arbiter_io_next_valid & io_hit; // @[Fetch.scala 43:48]
  assign _GEN_29 = io_flush_tag ? 1'h0 : io_fetchEn_1; // @[Fetch.scala 88:32]
  assign readyThreads_1 = io_flush_valid ? _GEN_29 : io_fetchEn_1; // @[Fetch.scala 86:24]
  assign _GEN_28 = ~io_flush_tag ? 1'h0 : io_fetchEn_0; // @[Fetch.scala 88:32]
  assign readyThreads_0 = io_flush_valid ? _GEN_28 : io_fetchEn_0; // @[Fetch.scala 86:24]
  assign _T_19 = arbiter_io_next_bits == io_flush_tag; // @[Fetch.scala 87:31]
  assign _GEN_27 = _T_19 ? 1'h0 : _T_5; // @[Fetch.scala 87:49]
  assign insnHit = io_flush_valid ? _GEN_27 : _T_5; // @[Fetch.scala 86:24]
  assign _T_7 = insnHit & fetchReg_io_enq_ready; // @[Fetch.scala 50:16]
  assign _GEN_1 = arbiter_io_next_bits ? prefetchPC_1 : prefetchPC_0; // @[Fetch.scala 51:32]
  assign _T_9 = _GEN_1 + 64'h4; // @[Fetch.scala 51:32]
  assign _GEN_2 = ~arbiter_io_next_bits ? _T_9 : prefetchPC_0; // @[Fetch.scala 51:17]
  assign _GEN_3 = arbiter_io_next_bits ? _T_9 : prefetchPC_1; // @[Fetch.scala 51:17]
  assign _GEN_4 = _T_7 ? _GEN_2 : prefetchPC_0; // @[Fetch.scala 50:42]
  assign _GEN_5 = _T_7 ? _GEN_3 : prefetchPC_1; // @[Fetch.scala 50:42]
  assign _T_13 = ~fetchReg_io_enq_ready; // @[Fetch.scala 64:8]
  assign _T_14 = ~insnReg_valid; // @[Fetch.scala 64:34]
  assign _T_15 = _T_13 & _T_14; // @[Fetch.scala 64:31]
  assign _T_17 = insnReg_valid ? insnReg_valid : insnReq_valid; // @[Fetch.scala 71:31]
  assign _T_18 = io_commitReg_valid & io_commitReg_bits_br_valid; // @[Fetch.scala 73:27]
  assign _T_20 = insnReq_bits_tag == io_flush_tag; // @[Fetch.scala 92:24]
  assign _GEN_30 = _T_20 ? 1'h0 : _T_17; // @[Fetch.scala 94:28]
  assign _T_21 = fetchReg_io_deq_bits_tag == io_flush_tag; // @[Fetch.scala 95:51]
  assign io_pc_tag = arbiter_io_next_bits; // @[Fetch.scala 55:13]
  assign io_pc_bits = arbiter_io_next_bits ? prefetchPC_1 : prefetchPC_0; // @[Fetch.scala 54:18]
  assign io_pc_valid = arbiter_io_next_valid; // @[Fetch.scala 56:15]
  assign io_deq_valid = fetchReg_io_deq_valid; // @[Fetch.scala 82:10]
  assign io_deq_bits_inst = fetchReg_io_deq_bits_inst; // @[Fetch.scala 82:10]
  assign io_deq_bits_tag = fetchReg_io_deq_bits_tag; // @[Fetch.scala 82:10]
  assign io_deq_bits_pc = fetchReg_io_deq_bits_pc; // @[Fetch.scala 82:10]
  assign arbiter_clock = clock;
  assign arbiter_reset = reset;
  assign arbiter_io_ready = {readyThreads_1,readyThreads_0}; // @[Fetch.scala 46:20]
  assign arbiter_io_next_ready = fetchReg_io_enq_ready; // @[Fetch.scala 47:25]
  assign fetchReg_clock = clock;
  assign fetchReg_reset = reset;
  assign fetchReg_io_enq_valid = io_flush_valid ? _GEN_30 : _T_17; // @[Fetch.scala 71:25 Fetch.scala 94:52]
  assign fetchReg_io_enq_bits_inst = insnReg_valid ? insnReg_bits_inst : io_insn; // @[Fetch.scala 70:25]
  assign fetchReg_io_enq_bits_tag = insnReg_valid ? insnReg_bits_tag : insnReq_bits_tag; // @[Fetch.scala 70:25]
  assign fetchReg_io_enq_bits_pc = insnReg_valid ? insnReg_bits_pc : insnReq_bits_pc; // @[Fetch.scala 70:25]
  assign fetchReg_io_deq_ready = io_deq_ready; // @[Fetch.scala 82:10]
  assign fetchReg_io_flush = io_flush_valid & _T_21; // @[Fetch.scala 85:21 Fetch.scala 95:23]
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
  _RAND_0 = {2{`RANDOM}};
  prefetchPC_0 = _RAND_0[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {2{`RANDOM}};
  prefetchPC_1 = _RAND_1[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  insnReg_valid = _RAND_2[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  insnReg_bits_inst = _RAND_3[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  insnReg_bits_tag = _RAND_4[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {2{`RANDOM}};
  insnReg_bits_pc = _RAND_5[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  insnReq_valid = _RAND_6[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  insnReq_bits_tag = _RAND_7[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {2{`RANDOM}};
  insnReq_bits_pc = _RAND_8[63:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (reset) begin
      prefetchPC_0 <= 64'h0;
    end else if (io_fire_valid) begin
      if (~io_fire_tag) begin
        if (io_fire_tag) begin
          prefetchPC_0 <= io_pcVec_1;
        end else begin
          prefetchPC_0 <= io_pcVec_0;
        end
      end else if (_T_18) begin
        if (~io_commitReg_bits_tag) begin
          prefetchPC_0 <= io_nextPC;
        end else if (_T_7) begin
          if (~arbiter_io_next_bits) begin
            prefetchPC_0 <= _T_9;
          end
        end
      end else if (io_flush_valid) begin
        if (~io_flush_tag) begin
          if (io_flush_tag) begin
            prefetchPC_0 <= io_pcVec_1;
          end else begin
            prefetchPC_0 <= io_pcVec_0;
          end
        end else if (_T_7) begin
          if (~arbiter_io_next_bits) begin
            prefetchPC_0 <= _T_9;
          end
        end
      end else if (_T_7) begin
        if (~arbiter_io_next_bits) begin
          prefetchPC_0 <= _T_9;
        end
      end
    end else if (_T_18) begin
      if (~io_commitReg_bits_tag) begin
        prefetchPC_0 <= io_nextPC;
      end else if (_T_7) begin
        if (~arbiter_io_next_bits) begin
          prefetchPC_0 <= _T_9;
        end
      end
    end else if (io_flush_valid) begin
      if (~io_flush_tag) begin
        if (io_flush_tag) begin
          prefetchPC_0 <= io_pcVec_1;
        end else begin
          prefetchPC_0 <= io_pcVec_0;
        end
      end else begin
        prefetchPC_0 <= _GEN_4;
      end
    end else begin
      prefetchPC_0 <= _GEN_4;
    end
    if (reset) begin
      prefetchPC_1 <= 64'h0;
    end else if (io_fire_valid) begin
      if (io_fire_tag) begin
        if (io_fire_tag) begin
          prefetchPC_1 <= io_pcVec_1;
        end else begin
          prefetchPC_1 <= io_pcVec_0;
        end
      end else if (_T_18) begin
        if (io_commitReg_bits_tag) begin
          prefetchPC_1 <= io_nextPC;
        end else if (_T_7) begin
          if (arbiter_io_next_bits) begin
            prefetchPC_1 <= _T_9;
          end
        end
      end else if (io_flush_valid) begin
        if (io_flush_tag) begin
          if (io_flush_tag) begin
            prefetchPC_1 <= io_pcVec_1;
          end else begin
            prefetchPC_1 <= io_pcVec_0;
          end
        end else if (_T_7) begin
          if (arbiter_io_next_bits) begin
            prefetchPC_1 <= _T_9;
          end
        end
      end else if (_T_7) begin
        if (arbiter_io_next_bits) begin
          prefetchPC_1 <= _T_9;
        end
      end
    end else if (_T_18) begin
      if (io_commitReg_bits_tag) begin
        prefetchPC_1 <= io_nextPC;
      end else if (_T_7) begin
        if (arbiter_io_next_bits) begin
          prefetchPC_1 <= _T_9;
        end
      end
    end else if (io_flush_valid) begin
      if (io_flush_tag) begin
        if (io_flush_tag) begin
          prefetchPC_1 <= io_pcVec_1;
        end else begin
          prefetchPC_1 <= io_pcVec_0;
        end
      end else begin
        prefetchPC_1 <= _GEN_5;
      end
    end else begin
      prefetchPC_1 <= _GEN_5;
    end
    if (_T_15) begin
      insnReg_valid <= insnReq_valid;
    end else if (fetchReg_io_enq_ready) begin
      insnReg_valid <= 1'h0;
    end
    if (_T_15) begin
      insnReg_bits_inst <= io_insn;
    end
    if (_T_15) begin
      insnReg_bits_tag <= insnReq_bits_tag;
    end
    if (_T_15) begin
      insnReg_bits_pc <= insnReq_bits_pc;
    end
    if (io_flush_valid) begin
      if (_T_19) begin
        insnReq_valid <= 1'h0;
      end else begin
        insnReq_valid <= _T_5;
      end
    end else begin
      insnReq_valid <= _T_5;
    end
    insnReq_bits_tag <= arbiter_io_next_bits;
    if (arbiter_io_next_bits) begin
      insnReq_bits_pc <= prefetchPC_1;
    end else begin
      insnReq_bits_pc <= prefetchPC_0;
    end
  end
endmodule
