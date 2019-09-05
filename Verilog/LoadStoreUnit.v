module LoadStoreUnit( // @[:@2865.2]
  input         clock, // @[:@2866.4]
  input         reset, // @[:@2867.4]
  input         io_dinst_valid, // @[:@2868.4]
  input  [25:0] io_dinst_bits_imm_bits, // @[:@2868.4]
  input  [2:0]  io_dinst_bits_itype, // @[:@2868.4]
  input  [63:0] io_dinst_bits_pc // @[:@2868.4]
);
  wire  tlb_clock; // @[lsu.scala 80:19:@2890.4]
  wire  tlb_reset; // @[lsu.scala 80:19:@2890.4]
  wire  tlb_io_vaddr_valid; // @[lsu.scala 80:19:@2890.4]
  wire [27:0] tlb_io_vaddr_bits_tag; // @[lsu.scala 80:19:@2890.4]
  wire [7:0] tlb_io_vaddr_bits_set; // @[lsu.scala 80:19:@2890.4]
  wire  tlb_io_hit_valid; // @[lsu.scala 80:19:@2890.4]
  wire  tlb_io_hit_bits; // @[lsu.scala 80:19:@2890.4]
  reg [63:0] dinst_reg_pc; // @[lsu.scala 55:22:@2870.4]
  reg [63:0] _RAND_0;
  wire [64:0] base; // @[lsu.scala 58:31:@2871.4]
  wire [25:0] _T_237; // @[lsu.scala 62:46:@2873.4]
  wire [63:0] imm_sign_extened; // @[lsu.scala 61:30:@2872.4 lsu.scala 62:20:@2874.4]
  wire [64:0] _GEN_54; // @[lsu.scala 68:18:@2876.4]
  wire [65:0] _T_239; // @[lsu.scala 68:18:@2876.4]
  wire [64:0] _T_240; // @[lsu.scala 68:18:@2877.4]
  wire [64:0] _T_241; // @[lsu.scala 68:18:@2878.4]
  wire [64:0] _T_242; // @[lsu.scala 68:34:@2879.4]
  wire [47:0] vaddr; // @[lsu.scala 67:19:@2875.4 lsu.scala 68:9:@2880.4]
  reg [1:0] state; // @[lsu.scala 102:22:@2911.4]
  reg [31:0] _RAND_1;
  wire  _T_254; // @[Conditional.scala 37:30:@2912.4]
  wire  _T_255; // @[lsu.scala 105:32:@2914.6]
  wire [63:0] _GEN_1; // @[lsu.scala 105:45:@2915.6]
  wire  _GEN_21; // @[lsu.scala 105:45:@2915.6]
  wire [1:0] _GEN_22; // @[lsu.scala 105:45:@2915.6]
  wire  _T_256; // @[Conditional.scala 37:30:@2941.6]
  wire  _T_257; // @[lsu.scala 113:36:@2944.10]
  wire [1:0] _GEN_24; // @[lsu.scala 113:39:@2945.10]
  wire [1:0] _GEN_26; // @[lsu.scala 112:30:@2943.8]
  wire [1:0] _GEN_30; // @[Conditional.scala 39:67:@2942.6]
  wire [1:0] _GEN_52; // @[Conditional.scala 40:58:@2913.4]
  TLBUnitLegacy tlb ( // @[lsu.scala 80:19:@2890.4]
    .clock(tlb_clock),
    .reset(tlb_reset),
    .io_vaddr_valid(tlb_io_vaddr_valid),
    .io_vaddr_bits_tag(tlb_io_vaddr_bits_tag),
    .io_vaddr_bits_set(tlb_io_vaddr_bits_set),
    .io_hit_valid(tlb_io_hit_valid),
    .io_hit_bits(tlb_io_hit_bits)
  );
  assign base = {1'b0,$signed(dinst_reg_pc)}; // @[lsu.scala 58:31:@2871.4]
  assign _T_237 = $signed(io_dinst_bits_imm_bits); // @[lsu.scala 62:46:@2873.4]
  assign imm_sign_extened = {{38{_T_237[25]}},_T_237}; // @[lsu.scala 61:30:@2872.4 lsu.scala 62:20:@2874.4]
  assign _GEN_54 = {{1{imm_sign_extened[63]}},imm_sign_extened}; // @[lsu.scala 68:18:@2876.4]
  assign _T_239 = $signed(base) + $signed(_GEN_54); // @[lsu.scala 68:18:@2876.4]
  assign _T_240 = $signed(base) + $signed(_GEN_54); // @[lsu.scala 68:18:@2877.4]
  assign _T_241 = $signed(_T_240); // @[lsu.scala 68:18:@2878.4]
  assign _T_242 = $unsigned(_T_241); // @[lsu.scala 68:34:@2879.4]
  assign vaddr = _T_242[47:0]; // @[lsu.scala 67:19:@2875.4 lsu.scala 68:9:@2880.4]
  assign _T_254 = 2'h0 == state; // @[Conditional.scala 37:30:@2912.4]
  assign _T_255 = io_dinst_bits_itype == 3'h5; // @[lsu.scala 105:32:@2914.6]
  assign _GEN_1 = _T_255 ? io_dinst_bits_pc : dinst_reg_pc; // @[lsu.scala 105:45:@2915.6]
  assign _GEN_21 = _T_255 ? io_dinst_valid : 1'h0; // @[lsu.scala 105:45:@2915.6]
  assign _GEN_22 = _T_255 ? 2'h1 : state; // @[lsu.scala 105:45:@2915.6]
  assign _T_256 = 2'h1 == state; // @[Conditional.scala 37:30:@2941.6]
  assign _T_257 = tlb_io_hit_bits; // @[lsu.scala 113:36:@2944.10]
  assign _GEN_24 = _T_257 ? 2'h2 : 2'h0; // @[lsu.scala 113:39:@2945.10]
  assign _GEN_26 = tlb_io_hit_valid ? _GEN_24 : state; // @[lsu.scala 112:30:@2943.8]
  assign _GEN_30 = _T_256 ? _GEN_26 : state; // @[Conditional.scala 39:67:@2942.6]
  assign _GEN_52 = _T_254 ? _GEN_22 : _GEN_30; // @[Conditional.scala 40:58:@2913.4]
  assign tlb_clock = clock; // @[:@2891.4]
  assign tlb_reset = reset; // @[:@2892.4]
  assign tlb_io_vaddr_valid = _T_254 ? _GEN_21 : 1'h0; // @[lsu.scala 86:22:@2899.4 lsu.scala 107:28:@2936.8]
  assign tlb_io_vaddr_bits_tag = vaddr[47:20]; // @[lsu.scala 84:21:@2898.4]
  assign tlb_io_vaddr_bits_set = vaddr[19:12]; // @[lsu.scala 84:21:@2897.4]
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
  dinst_reg_pc = _RAND_0[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  state = _RAND_1[1:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (_T_254) begin
      if (_T_255) begin
        dinst_reg_pc <= io_dinst_bits_pc;
      end
    end
    if (reset) begin
      state <= 2'h0;
    end else begin
      if (_T_254) begin
        if (_T_255) begin
          state <= 2'h1;
        end
      end else begin
        if (_T_256) begin
          if (tlb_io_hit_valid) begin
            if (_T_257) begin
              state <= 2'h2;
            end else begin
              state <= 2'h0;
            end
          end
        end
      end
    end
  end
endmodule
