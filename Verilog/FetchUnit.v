module FetchUnit( // @[:@768.2]
  input         clock, // @[:@769.4]
  input         reset, // @[:@770.4]
  input         io_flush_tag, // @[:@771.4]
  input         io_flush_valid, // @[:@771.4]
  input         io_fire_tag, // @[:@771.4]
  input         io_fire_valid, // @[:@771.4]
  input         io_commitReg_valid, // @[:@771.4]
  input         io_commitReg_bits_br_valid, // @[:@771.4]
  input         io_commitReg_bits_tag, // @[:@771.4]
  input  [63:0] io_nextPC, // @[:@771.4]
  input         io_fetchEn_0, // @[:@771.4]
  input         io_fetchEn_1, // @[:@771.4]
  input  [63:0] io_pcVec_0, // @[:@771.4]
  input  [63:0] io_pcVec_1, // @[:@771.4]
  output [63:0] io_pc_data, // @[:@771.4]
  output        io_pc_valid, // @[:@771.4]
  input  [31:0] io_insn, // @[:@771.4]
  input         io_deq_ready, // @[:@771.4]
  output        io_deq_valid, // @[:@771.4]
  output [31:0] io_deq_bits_inst, // @[:@771.4]
  output        io_deq_bits_tag, // @[:@771.4]
  output [63:0] io_deq_bits_pc // @[:@771.4]
);
  wire  arbiter_clock; // @[fetch.scala 38:23:@777.4]
  wire  arbiter_reset; // @[fetch.scala 38:23:@777.4]
  wire [1:0] arbiter_io_ready; // @[fetch.scala 38:23:@777.4]
  wire  arbiter_io_next_ready; // @[fetch.scala 38:23:@777.4]
  wire  arbiter_io_next_valid; // @[fetch.scala 38:23:@777.4]
  wire  arbiter_io_next_bits; // @[fetch.scala 38:23:@777.4]
  wire  fetchReg_clock; // @[fetch.scala 41:24:@781.4]
  wire  fetchReg_reset; // @[fetch.scala 41:24:@781.4]
  wire  fetchReg_io_enq_ready; // @[fetch.scala 41:24:@781.4]
  wire  fetchReg_io_enq_valid; // @[fetch.scala 41:24:@781.4]
  wire [31:0] fetchReg_io_enq_bits_inst; // @[fetch.scala 41:24:@781.4]
  wire  fetchReg_io_enq_bits_tag; // @[fetch.scala 41:24:@781.4]
  wire [63:0] fetchReg_io_enq_bits_pc; // @[fetch.scala 41:24:@781.4]
  wire  fetchReg_io_deq_ready; // @[fetch.scala 41:24:@781.4]
  wire  fetchReg_io_deq_valid; // @[fetch.scala 41:24:@781.4]
  wire [31:0] fetchReg_io_deq_bits_inst; // @[fetch.scala 41:24:@781.4]
  wire  fetchReg_io_deq_bits_tag; // @[fetch.scala 41:24:@781.4]
  wire [63:0] fetchReg_io_deq_bits_pc; // @[fetch.scala 41:24:@781.4]
  wire  fetchReg_io_flush; // @[fetch.scala 41:24:@781.4]
  reg [63:0] prefetchPC_0; // @[fetch.scala 37:27:@776.4]
  reg [63:0] _RAND_0;
  reg [63:0] prefetchPC_1; // @[fetch.scala 37:27:@776.4]
  reg [63:0] _RAND_1;
  wire  _T_242; // @[fetch.scala 42:67:@784.4]
  wire  _T_245; // @[fetch.scala 44:48:@788.4]
  wire  _T_248; // @[fetch.scala 45:25:@791.4]
  reg  insnReq_valid; // @[fetch.scala 61:27:@811.4]
  reg [31:0] _RAND_2;
  wire  _T_249; // @[fetch.scala 45:22:@792.4]
  wire  _T_253; // @[fetch.scala 52:30:@801.4]
  wire [63:0] _GEN_2; // @[fetch.scala 53:12:@803.6]
  wire [64:0] _T_264; // @[fetch.scala 54:74:@804.6]
  wire [63:0] _T_265; // @[fetch.scala 54:74:@805.6]
  wire [63:0] _GEN_3; // @[fetch.scala 54:38:@806.6]
  wire [63:0] _GEN_4; // @[fetch.scala 54:38:@806.6]
  wire [63:0] _GEN_6; // @[fetch.scala 52:56:@802.4]
  wire [63:0] _GEN_7; // @[fetch.scala 52:56:@802.4]
  reg  insnReq_bits_tag; // @[fetch.scala 63:30:@815.4]
  reg [31:0] _RAND_3;
  reg [63:0] insnReq_bits_pc; // @[fetch.scala 64:29:@818.4]
  reg [63:0] _RAND_4;
  wire  _T_272; // @[fetch.scala 69:27:@825.4]
  wire [63:0] _GEN_8; // @[fetch.scala 70:39:@827.6]
  wire [63:0] _GEN_9; // @[fetch.scala 70:39:@827.6]
  wire [63:0] _GEN_10; // @[fetch.scala 69:58:@826.4]
  wire [63:0] _GEN_11; // @[fetch.scala 69:58:@826.4]
  wire [63:0] _GEN_15; // @[fetch.scala 74:29:@830.6]
  wire [63:0] _GEN_12; // @[fetch.scala 74:29:@830.6]
  wire [63:0] _GEN_13; // @[fetch.scala 74:29:@830.6]
  wire [63:0] _GEN_16; // @[fetch.scala 73:23:@829.4]
  wire [63:0] _GEN_17; // @[fetch.scala 73:23:@829.4]
  wire [63:0] _GEN_21; // @[fetch.scala 77:30:@833.6]
  wire [63:0] _GEN_18; // @[fetch.scala 77:30:@833.6]
  wire [63:0] _GEN_19; // @[fetch.scala 77:30:@833.6]
  wire [63:0] _GEN_22; // @[fetch.scala 76:24:@832.4]
  wire [63:0] _GEN_23; // @[fetch.scala 76:24:@832.4]
  RRArbiter arbiter ( // @[fetch.scala 38:23:@777.4]
    .clock(arbiter_clock),
    .reset(arbiter_reset),
    .io_ready(arbiter_io_ready),
    .io_next_ready(arbiter_io_next_ready),
    .io_next_valid(arbiter_io_next_valid),
    .io_next_bits(arbiter_io_next_bits)
  );
  FlushReg fetchReg ( // @[fetch.scala 41:24:@781.4]
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
  assign _T_242 = fetchReg_io_deq_bits_tag == io_flush_tag; // @[fetch.scala 42:67:@784.4]
  assign _T_245 = io_pc_valid & fetchReg_io_enq_ready; // @[fetch.scala 44:48:@788.4]
  assign _T_248 = io_deq_ready == 1'h0; // @[fetch.scala 45:25:@791.4]
  assign _T_249 = insnReq_valid & _T_248; // @[fetch.scala 45:22:@792.4]
  assign _T_253 = arbiter_io_next_valid & fetchReg_io_enq_ready; // @[fetch.scala 52:30:@801.4]
  assign _GEN_2 = arbiter_io_next_bits ? prefetchPC_1 : prefetchPC_0; // @[fetch.scala 53:12:@803.6]
  assign _T_264 = _GEN_2 + 64'h4; // @[fetch.scala 54:74:@804.6]
  assign _T_265 = _GEN_2 + 64'h4; // @[fetch.scala 54:74:@805.6]
  assign _GEN_3 = 1'h0 == arbiter_io_next_bits ? _T_265 : prefetchPC_0; // @[fetch.scala 54:38:@806.6]
  assign _GEN_4 = arbiter_io_next_bits ? _T_265 : prefetchPC_1; // @[fetch.scala 54:38:@806.6]
  assign _GEN_6 = _T_253 ? _GEN_3 : prefetchPC_0; // @[fetch.scala 52:56:@802.4]
  assign _GEN_7 = _T_253 ? _GEN_4 : prefetchPC_1; // @[fetch.scala 52:56:@802.4]
  assign _T_272 = io_commitReg_valid & io_commitReg_bits_br_valid; // @[fetch.scala 69:27:@825.4]
  assign _GEN_8 = 1'h0 == io_commitReg_bits_tag ? io_nextPC : _GEN_6; // @[fetch.scala 70:39:@827.6]
  assign _GEN_9 = io_commitReg_bits_tag ? io_nextPC : _GEN_7; // @[fetch.scala 70:39:@827.6]
  assign _GEN_10 = _T_272 ? _GEN_8 : _GEN_6; // @[fetch.scala 69:58:@826.4]
  assign _GEN_11 = _T_272 ? _GEN_9 : _GEN_7; // @[fetch.scala 69:58:@826.4]
  assign _GEN_15 = io_fire_tag ? io_pcVec_1 : io_pcVec_0; // @[fetch.scala 74:29:@830.6]
  assign _GEN_12 = 1'h0 == io_fire_tag ? _GEN_15 : _GEN_10; // @[fetch.scala 74:29:@830.6]
  assign _GEN_13 = io_fire_tag ? _GEN_15 : _GEN_11; // @[fetch.scala 74:29:@830.6]
  assign _GEN_16 = io_fire_valid ? _GEN_12 : _GEN_10; // @[fetch.scala 73:23:@829.4]
  assign _GEN_17 = io_fire_valid ? _GEN_13 : _GEN_11; // @[fetch.scala 73:23:@829.4]
  assign _GEN_21 = io_flush_tag ? io_pcVec_1 : io_pcVec_0; // @[fetch.scala 77:30:@833.6]
  assign _GEN_18 = 1'h0 == io_flush_tag ? _GEN_21 : _GEN_16; // @[fetch.scala 77:30:@833.6]
  assign _GEN_19 = io_flush_tag ? _GEN_21 : _GEN_17; // @[fetch.scala 77:30:@833.6]
  assign _GEN_22 = io_flush_valid ? _GEN_18 : _GEN_16; // @[fetch.scala 76:24:@832.4]
  assign _GEN_23 = io_flush_valid ? _GEN_19 : _GEN_17; // @[fetch.scala 76:24:@832.4]
  assign io_pc_data = _T_253 ? _GEN_2 : prefetchPC_0; // @[fetch.scala 57:18:@808.4]
  assign io_pc_valid = arbiter_io_next_valid; // @[fetch.scala 59:15:@810.4]
  assign io_deq_valid = fetchReg_io_deq_valid; // @[fetch.scala 80:10:@838.4]
  assign io_deq_bits_inst = fetchReg_io_deq_bits_inst; // @[fetch.scala 80:10:@837.4]
  assign io_deq_bits_tag = fetchReg_io_deq_bits_tag; // @[fetch.scala 80:10:@836.4]
  assign io_deq_bits_pc = fetchReg_io_deq_bits_pc; // @[fetch.scala 80:10:@835.4]
  assign arbiter_clock = clock; // @[:@778.4]
  assign arbiter_reset = reset; // @[:@779.4]
  assign arbiter_io_ready = {io_fetchEn_1,io_fetchEn_0}; // @[fetch.scala 50:20:@799.4]
  assign arbiter_io_next_ready = fetchReg_io_enq_ready; // @[fetch.scala 51:25:@800.4]
  assign fetchReg_clock = clock; // @[:@782.4]
  assign fetchReg_reset = reset; // @[:@783.4]
  assign fetchReg_io_enq_valid = insnReq_valid; // @[fetch.scala 67:25:@824.4]
  assign fetchReg_io_enq_bits_inst = io_insn; // @[fetch.scala 66:24:@823.4]
  assign fetchReg_io_enq_bits_tag = insnReq_bits_tag; // @[fetch.scala 66:24:@822.4]
  assign fetchReg_io_enq_bits_pc = insnReq_bits_pc; // @[fetch.scala 66:24:@821.4]
  assign fetchReg_io_deq_ready = io_deq_ready; // @[fetch.scala 80:10:@839.4]
  assign fetchReg_io_flush = io_flush_valid & _T_242; // @[fetch.scala 42:21:@786.4]
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
  prefetchPC_0 = _RAND_0[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {2{`RANDOM}};
  prefetchPC_1 = _RAND_1[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  insnReq_valid = _RAND_2[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  insnReq_bits_tag = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {2{`RANDOM}};
  insnReq_bits_pc = _RAND_4[63:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (reset) begin
      prefetchPC_0 <= 64'h0;
    end else begin
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          if (io_flush_tag) begin
            prefetchPC_0 <= io_pcVec_1;
          end else begin
            prefetchPC_0 <= io_pcVec_0;
          end
        end else begin
          if (io_fire_valid) begin
            if (1'h0 == io_fire_tag) begin
              if (io_fire_tag) begin
                prefetchPC_0 <= io_pcVec_1;
              end else begin
                prefetchPC_0 <= io_pcVec_0;
              end
            end else begin
              if (_T_272) begin
                if (1'h0 == io_commitReg_bits_tag) begin
                  prefetchPC_0 <= io_nextPC;
                end else begin
                  if (_T_253) begin
                    if (1'h0 == arbiter_io_next_bits) begin
                      prefetchPC_0 <= _T_265;
                    end
                  end
                end
              end else begin
                if (_T_253) begin
                  if (1'h0 == arbiter_io_next_bits) begin
                    prefetchPC_0 <= _T_265;
                  end
                end
              end
            end
          end else begin
            if (_T_272) begin
              if (1'h0 == io_commitReg_bits_tag) begin
                prefetchPC_0 <= io_nextPC;
              end else begin
                if (_T_253) begin
                  if (1'h0 == arbiter_io_next_bits) begin
                    prefetchPC_0 <= _T_265;
                  end
                end
              end
            end else begin
              if (_T_253) begin
                if (1'h0 == arbiter_io_next_bits) begin
                  prefetchPC_0 <= _T_265;
                end
              end
            end
          end
        end
      end else begin
        if (io_fire_valid) begin
          if (1'h0 == io_fire_tag) begin
            if (io_fire_tag) begin
              prefetchPC_0 <= io_pcVec_1;
            end else begin
              prefetchPC_0 <= io_pcVec_0;
            end
          end else begin
            if (_T_272) begin
              if (1'h0 == io_commitReg_bits_tag) begin
                prefetchPC_0 <= io_nextPC;
              end else begin
                prefetchPC_0 <= _GEN_6;
              end
            end else begin
              prefetchPC_0 <= _GEN_6;
            end
          end
        end else begin
          if (_T_272) begin
            if (1'h0 == io_commitReg_bits_tag) begin
              prefetchPC_0 <= io_nextPC;
            end else begin
              prefetchPC_0 <= _GEN_6;
            end
          end else begin
            prefetchPC_0 <= _GEN_6;
          end
        end
      end
    end
    if (reset) begin
      prefetchPC_1 <= 64'h0;
    end else begin
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          if (io_flush_tag) begin
            prefetchPC_1 <= io_pcVec_1;
          end else begin
            prefetchPC_1 <= io_pcVec_0;
          end
        end else begin
          if (io_fire_valid) begin
            if (io_fire_tag) begin
              if (io_fire_tag) begin
                prefetchPC_1 <= io_pcVec_1;
              end else begin
                prefetchPC_1 <= io_pcVec_0;
              end
            end else begin
              if (_T_272) begin
                if (io_commitReg_bits_tag) begin
                  prefetchPC_1 <= io_nextPC;
                end else begin
                  if (_T_253) begin
                    if (arbiter_io_next_bits) begin
                      prefetchPC_1 <= _T_265;
                    end
                  end
                end
              end else begin
                if (_T_253) begin
                  if (arbiter_io_next_bits) begin
                    prefetchPC_1 <= _T_265;
                  end
                end
              end
            end
          end else begin
            if (_T_272) begin
              if (io_commitReg_bits_tag) begin
                prefetchPC_1 <= io_nextPC;
              end else begin
                if (_T_253) begin
                  if (arbiter_io_next_bits) begin
                    prefetchPC_1 <= _T_265;
                  end
                end
              end
            end else begin
              if (_T_253) begin
                if (arbiter_io_next_bits) begin
                  prefetchPC_1 <= _T_265;
                end
              end
            end
          end
        end
      end else begin
        if (io_fire_valid) begin
          if (io_fire_tag) begin
            if (io_fire_tag) begin
              prefetchPC_1 <= io_pcVec_1;
            end else begin
              prefetchPC_1 <= io_pcVec_0;
            end
          end else begin
            if (_T_272) begin
              if (io_commitReg_bits_tag) begin
                prefetchPC_1 <= io_nextPC;
              end else begin
                prefetchPC_1 <= _GEN_7;
              end
            end else begin
              prefetchPC_1 <= _GEN_7;
            end
          end
        end else begin
          if (_T_272) begin
            if (io_commitReg_bits_tag) begin
              prefetchPC_1 <= io_nextPC;
            end else begin
              prefetchPC_1 <= _GEN_7;
            end
          end else begin
            prefetchPC_1 <= _GEN_7;
          end
        end
      end
    end
    if (_T_249) begin
      insnReq_valid <= 1'h0;
    end else begin
      insnReq_valid <= _T_245;
    end
    insnReq_bits_tag <= arbiter_io_next_bits;
    if (_T_253) begin
      if (arbiter_io_next_bits) begin
        insnReq_bits_pc <= prefetchPC_1;
      end else begin
        insnReq_bits_pc <= prefetchPC_0;
      end
    end else begin
      insnReq_bits_pc <= prefetchPC_0;
    end
  end
endmodule
