module LoadStoreUnit( // @[:@2863.2]
  input         clock, // @[:@2864.4]
  input         reset, // @[:@2865.4]
  input         io_dinst_valid, // @[:@2866.4]
  input  [25:0] io_dinst_bits_imm, // @[:@2866.4]
  input  [2:0]  io_dinst_bits_itype, // @[:@2866.4]
  input  [63:0] io_pc // @[:@2866.4]
);
  wire  tlb_clock; // @[lsu.scala 82:19:@2888.4]
  wire  tlb_reset; // @[lsu.scala 82:19:@2888.4]
  wire  tlb_io_vaddr_valid; // @[lsu.scala 82:19:@2888.4]
  wire [27:0] tlb_io_vaddr_bits_tag; // @[lsu.scala 82:19:@2888.4]
  wire [7:0] tlb_io_vaddr_bits_set; // @[lsu.scala 82:19:@2888.4]
  wire  tlb_io_hit_valid; // @[lsu.scala 82:19:@2888.4]
  wire  tlb_io_hit_bits; // @[lsu.scala 82:19:@2888.4]
  wire [64:0] base; // @[lsu.scala 60:24:@2869.4]
  wire [25:0] _T_55; // @[lsu.scala 64:41:@2871.4]
  wire [63:0] imm_sign_extened; // @[lsu.scala 63:30:@2870.4 lsu.scala 64:20:@2872.4]
  wire [64:0] _GEN_54; // @[lsu.scala 70:18:@2874.4]
  wire [65:0] _T_57; // @[lsu.scala 70:18:@2874.4]
  wire [64:0] _T_58; // @[lsu.scala 70:18:@2875.4]
  wire [64:0] _T_59; // @[lsu.scala 70:18:@2876.4]
  wire [64:0] _T_60; // @[lsu.scala 70:34:@2877.4]
  wire [47:0] vaddr; // @[lsu.scala 69:19:@2873.4 lsu.scala 70:9:@2878.4]
  reg [1:0] state; // @[lsu.scala 104:22:@2909.4]
  reg [31:0] _RAND_0;
  wire  _T_72; // @[Conditional.scala 37:30:@2910.4]
  wire  _T_73; // @[lsu.scala 107:32:@2912.6]
  wire  _GEN_21; // @[lsu.scala 107:45:@2913.6]
  wire [1:0] _GEN_22; // @[lsu.scala 107:45:@2913.6]
  wire  _T_74; // @[Conditional.scala 37:30:@2939.6]
  wire  _T_75; // @[lsu.scala 115:36:@2942.10]
  wire [1:0] _GEN_24; // @[lsu.scala 115:39:@2943.10]
  wire [1:0] _GEN_26; // @[lsu.scala 114:30:@2941.8]
  wire [1:0] _GEN_30; // @[Conditional.scala 39:67:@2940.6]
  wire [1:0] _GEN_52; // @[Conditional.scala 40:58:@2911.4]
  TLBUnit tlb ( // @[lsu.scala 82:19:@2888.4]
    .clock(tlb_clock),
    .reset(tlb_reset),
    .io_vaddr_valid(tlb_io_vaddr_valid),
    .io_vaddr_bits_tag(tlb_io_vaddr_bits_tag),
    .io_vaddr_bits_set(tlb_io_vaddr_bits_set),
    .io_hit_valid(tlb_io_hit_valid),
    .io_hit_bits(tlb_io_hit_bits)
  );
  assign base = {1'b0,$signed(io_pc)}; // @[lsu.scala 60:24:@2869.4]
  assign _T_55 = $signed(io_dinst_bits_imm); // @[lsu.scala 64:41:@2871.4]
  assign imm_sign_extened = {{38{_T_55[25]}},_T_55}; // @[lsu.scala 63:30:@2870.4 lsu.scala 64:20:@2872.4]
  assign _GEN_54 = {{1{imm_sign_extened[63]}},imm_sign_extened}; // @[lsu.scala 70:18:@2874.4]
  assign _T_57 = $signed(base) + $signed(_GEN_54); // @[lsu.scala 70:18:@2874.4]
  assign _T_58 = $signed(base) + $signed(_GEN_54); // @[lsu.scala 70:18:@2875.4]
  assign _T_59 = $signed(_T_58); // @[lsu.scala 70:18:@2876.4]
  assign _T_60 = $unsigned(_T_59); // @[lsu.scala 70:34:@2877.4]
  assign vaddr = _T_60[47:0]; // @[lsu.scala 69:19:@2873.4 lsu.scala 70:9:@2878.4]
  assign _T_72 = 2'h0 == state; // @[Conditional.scala 37:30:@2910.4]
  assign _T_73 = io_dinst_bits_itype == 3'h5; // @[lsu.scala 107:32:@2912.6]
  assign _GEN_21 = _T_73 ? io_dinst_valid : 1'h0; // @[lsu.scala 107:45:@2913.6]
  assign _GEN_22 = _T_73 ? 2'h1 : state; // @[lsu.scala 107:45:@2913.6]
  assign _T_74 = 2'h1 == state; // @[Conditional.scala 37:30:@2939.6]
  assign _T_75 = tlb_io_hit_bits; // @[lsu.scala 115:36:@2942.10]
  assign _GEN_24 = _T_75 ? 2'h2 : 2'h0; // @[lsu.scala 115:39:@2943.10]
  assign _GEN_26 = tlb_io_hit_valid ? _GEN_24 : state; // @[lsu.scala 114:30:@2941.8]
  assign _GEN_30 = _T_74 ? _GEN_26 : state; // @[Conditional.scala 39:67:@2940.6]
  assign _GEN_52 = _T_72 ? _GEN_22 : _GEN_30; // @[Conditional.scala 40:58:@2911.4]
  assign tlb_clock = clock; // @[:@2889.4]
  assign tlb_reset = reset; // @[:@2890.4]
  assign tlb_io_vaddr_valid = _T_72 ? _GEN_21 : 1'h0; // @[lsu.scala 88:22:@2897.4 lsu.scala 109:28:@2934.8]
  assign tlb_io_vaddr_bits_tag = vaddr[47:20]; // @[lsu.scala 86:21:@2896.4]
  assign tlb_io_vaddr_bits_set = vaddr[19:12]; // @[lsu.scala 86:21:@2895.4]
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
  state = _RAND_0[1:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (reset) begin
      state <= 2'h0;
    end else begin
      if (_T_72) begin
        if (_T_73) begin
          state <= 2'h1;
        end
      end else begin
        if (_T_74) begin
          if (tlb_io_hit_valid) begin
            if (_T_75) begin
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
