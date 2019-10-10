module LoadStoreUnit( // @[:@3022.2]
  input         clock, // @[:@3023.4]
  input         reset, // @[:@3024.4]
  input         io_dinst_valid, // @[:@3025.4]
  input  [25:0] io_dinst_bits_imm_bits, // @[:@3025.4]
  input  [2:0]  io_dinst_bits_itype, // @[:@3025.4]
  input  [63:0] io_dinst_bits_pc // @[:@3025.4]
);
  wire  tlb_clock; // @[lsu.scala 79:19:@3047.4]
  wire  tlb_reset; // @[lsu.scala 79:19:@3047.4]
  wire  tlb_io_vaddr_valid; // @[lsu.scala 79:19:@3047.4]
  wire [27:0] tlb_io_vaddr_bits_tag; // @[lsu.scala 79:19:@3047.4]
  wire [7:0] tlb_io_vaddr_bits_set; // @[lsu.scala 79:19:@3047.4]
  wire  tlb_io_hit_valid; // @[lsu.scala 79:19:@3047.4]
  wire  tlb_io_hit_bits; // @[lsu.scala 79:19:@3047.4]
  reg [63:0] dinst_reg_pc; // @[lsu.scala 54:22:@3027.4]
  reg [63:0] _RAND_0;
  wire [64:0] base; // @[lsu.scala 57:31:@3028.4]
  wire [25:0] _T_231; // @[lsu.scala 61:46:@3030.4]
  wire [63:0] imm_sign_extened; // @[lsu.scala 60:30:@3029.4 lsu.scala 61:20:@3031.4]
  wire [64:0] _GEN_54; // @[lsu.scala 67:18:@3033.4]
  wire [65:0] _T_233; // @[lsu.scala 67:18:@3033.4]
  wire [64:0] _T_234; // @[lsu.scala 67:18:@3034.4]
  wire [64:0] _T_235; // @[lsu.scala 67:18:@3035.4]
  wire [64:0] _T_236; // @[lsu.scala 67:34:@3036.4]
  wire [47:0] vaddr; // @[lsu.scala 66:19:@3032.4 lsu.scala 67:9:@3037.4]
  reg [1:0] state; // @[lsu.scala 101:22:@3068.4]
  reg [31:0] _RAND_1;
  wire  _T_248; // @[Conditional.scala 37:30:@3069.4]
  wire  _T_249; // @[lsu.scala 104:32:@3071.6]
  wire [63:0] _GEN_1; // @[lsu.scala 104:45:@3072.6]
  wire  _GEN_21; // @[lsu.scala 104:45:@3072.6]
  wire [1:0] _GEN_22; // @[lsu.scala 104:45:@3072.6]
  wire  _T_250; // @[Conditional.scala 37:30:@3098.6]
  wire  _T_251; // @[lsu.scala 112:36:@3101.10]
  wire [1:0] _GEN_24; // @[lsu.scala 112:39:@3102.10]
  wire [1:0] _GEN_26; // @[lsu.scala 111:30:@3100.8]
  wire [1:0] _GEN_30; // @[Conditional.scala 39:67:@3099.6]
  wire [1:0] _GEN_52; // @[Conditional.scala 40:58:@3070.4]
  TLBUnitLegacy tlb ( // @[lsu.scala 79:19:@3047.4]
    .clock(tlb_clock),
    .reset(tlb_reset),
    .io_vaddr_valid(tlb_io_vaddr_valid),
    .io_vaddr_bits_tag(tlb_io_vaddr_bits_tag),
    .io_vaddr_bits_set(tlb_io_vaddr_bits_set),
    .io_hit_valid(tlb_io_hit_valid),
    .io_hit_bits(tlb_io_hit_bits)
  );
  assign base = {1'b0,$signed(dinst_reg_pc)}; // @[lsu.scala 57:31:@3028.4]
  assign _T_231 = $signed(io_dinst_bits_imm_bits); // @[lsu.scala 61:46:@3030.4]
  assign imm_sign_extened = {{38{_T_231[25]}},_T_231}; // @[lsu.scala 60:30:@3029.4 lsu.scala 61:20:@3031.4]
  assign _GEN_54 = {{1{imm_sign_extened[63]}},imm_sign_extened}; // @[lsu.scala 67:18:@3033.4]
  assign _T_233 = $signed(base) + $signed(_GEN_54); // @[lsu.scala 67:18:@3033.4]
  assign _T_234 = $signed(base) + $signed(_GEN_54); // @[lsu.scala 67:18:@3034.4]
  assign _T_235 = $signed(_T_234); // @[lsu.scala 67:18:@3035.4]
  assign _T_236 = $unsigned(_T_235); // @[lsu.scala 67:34:@3036.4]
  assign vaddr = _T_236[47:0]; // @[lsu.scala 66:19:@3032.4 lsu.scala 67:9:@3037.4]
  assign _T_248 = 2'h0 == state; // @[Conditional.scala 37:30:@3069.4]
  assign _T_249 = io_dinst_bits_itype == 3'h5; // @[lsu.scala 104:32:@3071.6]
  assign _GEN_1 = _T_249 ? io_dinst_bits_pc : dinst_reg_pc; // @[lsu.scala 104:45:@3072.6]
  assign _GEN_21 = _T_249 ? io_dinst_valid : 1'h0; // @[lsu.scala 104:45:@3072.6]
  assign _GEN_22 = _T_249 ? 2'h1 : state; // @[lsu.scala 104:45:@3072.6]
  assign _T_250 = 2'h1 == state; // @[Conditional.scala 37:30:@3098.6]
  assign _T_251 = tlb_io_hit_bits; // @[lsu.scala 112:36:@3101.10]
  assign _GEN_24 = _T_251 ? 2'h2 : 2'h0; // @[lsu.scala 112:39:@3102.10]
  assign _GEN_26 = tlb_io_hit_valid ? _GEN_24 : state; // @[lsu.scala 111:30:@3100.8]
  assign _GEN_30 = _T_250 ? _GEN_26 : state; // @[Conditional.scala 39:67:@3099.6]
  assign _GEN_52 = _T_248 ? _GEN_22 : _GEN_30; // @[Conditional.scala 40:58:@3070.4]
  assign tlb_clock = clock; // @[:@3048.4]
  assign tlb_reset = reset; // @[:@3049.4]
  assign tlb_io_vaddr_valid = _T_248 ? _GEN_21 : 1'h0; // @[lsu.scala 85:22:@3056.4 lsu.scala 106:28:@3093.8]
  assign tlb_io_vaddr_bits_tag = vaddr[47:20]; // @[lsu.scala 83:21:@3055.4]
  assign tlb_io_vaddr_bits_set = vaddr[19:12]; // @[lsu.scala 83:21:@3054.4]
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
    if (_T_248) begin
      if (_T_249) begin
        dinst_reg_pc <= io_dinst_bits_pc;
      end
    end
    if (reset) begin
      state <= 2'h0;
    end else begin
      if (_T_248) begin
        if (_T_249) begin
          state <= 2'h1;
        end
      end else begin
        if (_T_250) begin
          if (tlb_io_hit_valid) begin
            if (_T_251) begin
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
