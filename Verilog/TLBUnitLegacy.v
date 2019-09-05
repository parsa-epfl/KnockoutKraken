module TLBUnitLegacy( // @[:@2770.2]
  input         clock, // @[:@2771.4]
  input         reset, // @[:@2772.4]
  input         io_vaddr_valid, // @[:@2773.4]
  input  [27:0] io_vaddr_bits_tag, // @[:@2773.4]
  input  [7:0]  io_vaddr_bits_set, // @[:@2773.4]
  output        io_hit_valid, // @[:@2773.4]
  output        io_hit_bits // @[:@2773.4]
);
  wire  tag_mem_clock; // @[TLBLegacy.scala 53:23:@2783.4]
  wire [7:0] tag_mem_io_addr_r; // @[TLBLegacy.scala 53:23:@2783.4]
  wire [7:0] tag_mem_io_addr_w; // @[TLBLegacy.scala 53:23:@2783.4]
  wire [27:0] tag_mem_io_data_in; // @[TLBLegacy.scala 53:23:@2783.4]
  wire [27:0] tag_mem_io_data_out; // @[TLBLegacy.scala 53:23:@2783.4]
  reg [7:0] set_reg; // @[TLBLegacy.scala 42:20:@2775.4]
  reg [31:0] _RAND_0;
  reg [27:0] tag_reg; // @[TLBLegacy.scala 43:20:@2776.4]
  reg [31:0] _RAND_1;
  wire  _T_44; // @[TLBLegacy.scala 73:11:@2801.6]
  reg  state; // @[TLBLegacy.scala 84:22:@2816.4]
  reg [31:0] _RAND_2;
  wire  _T_55; // @[TLBLegacy.scala 88:19:@2817.4]
  wire [255:0] _T_56; // @[TLBLegacy.scala 88:40:@2818.4]
  wire  _T_57; // @[TLBLegacy.scala 88:40:@2819.4]
  wire  _T_60; // @[Conditional.scala 37:30:@2823.4]
  wire  _GEN_4; // @[TLBLegacy.scala 93:49:@2827.6]
  wire  _GEN_10; // @[Conditional.scala 39:67:@2833.6]
  wire  _GEN_13; // @[Conditional.scala 40:58:@2824.4]
  wire  _GEN_17; // @[TLBLegacy.scala 100:15:@2839.12]
  wire  _GEN_18; // @[TLBLegacy.scala 100:15:@2839.12]
  wire  _GEN_19; // @[TLBLegacy.scala 100:15:@2839.12]
  wire  _GEN_22; // @[TLBLegacy.scala 108:15:@2859.12]
  wire  _GEN_23; // @[TLBLegacy.scala 108:15:@2859.12]
  SRAM tag_mem ( // @[TLBLegacy.scala 53:23:@2783.4]
    .clock(tag_mem_clock),
    .io_addr_r(tag_mem_io_addr_r),
    .io_addr_w(tag_mem_io_addr_w),
    .io_data_in(tag_mem_io_data_in),
    .io_data_out(tag_mem_io_data_out)
  );
  assign _T_44 = reset == 1'h0; // @[TLBLegacy.scala 73:11:@2801.6]
  assign _T_55 = tag_mem_io_data_out == tag_reg; // @[TLBLegacy.scala 88:19:@2817.4]
  assign _T_56 = 256'h0 >> set_reg; // @[TLBLegacy.scala 88:40:@2818.4]
  assign _T_57 = _T_56[0]; // @[TLBLegacy.scala 88:40:@2819.4]
  assign _T_60 = 1'h0 == state; // @[Conditional.scala 37:30:@2823.4]
  assign _GEN_4 = io_vaddr_valid ? 1'h1 : state; // @[TLBLegacy.scala 93:49:@2827.6]
  assign _GEN_10 = state ? 1'h0 : state; // @[Conditional.scala 39:67:@2833.6]
  assign _GEN_13 = _T_60 ? _GEN_4 : _GEN_10; // @[Conditional.scala 40:58:@2824.4]
  assign io_hit_valid = _T_60 ? 1'h0 : state; // @[TLBLegacy.scala 89:16:@2822.4 TLBLegacy.scala 98:20:@2834.8]
  assign io_hit_bits = _T_55 & _T_57; // @[TLBLegacy.scala 88:7:@2821.4]
  assign tag_mem_clock = clock; // @[:@2784.4]
  assign tag_mem_io_addr_r = io_vaddr_bits_set; // @[TLBLegacy.scala 54:21:@2786.4]
  assign tag_mem_io_addr_w = io_vaddr_bits_set; // @[TLBLegacy.scala 55:21:@2787.4]
  assign tag_mem_io_data_in = io_vaddr_bits_tag; // @[TLBLegacy.scala 56:22:@2788.4]
  assign _GEN_17 = _T_60 == 1'h0; // @[TLBLegacy.scala 100:15:@2839.12]
  assign _GEN_18 = _GEN_17 & state; // @[TLBLegacy.scala 100:15:@2839.12]
  assign _GEN_19 = _GEN_18 & io_hit_bits; // @[TLBLegacy.scala 100:15:@2839.12]
  assign _GEN_22 = io_hit_bits == 1'h0; // @[TLBLegacy.scala 108:15:@2859.12]
  assign _GEN_23 = _GEN_18 & _GEN_22; // @[TLBLegacy.scala 108:15:@2859.12]
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
  set_reg = _RAND_0[7:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  tag_reg = _RAND_1[27:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  state = _RAND_2[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (io_vaddr_valid) begin
      set_reg <= io_vaddr_bits_set;
    end
    if (io_vaddr_valid) begin
      tag_reg <= io_vaddr_bits_tag;
    end
    if (reset) begin
      state <= 1'h0;
    end else begin
      if (_T_60) begin
        if (io_vaddr_valid) begin
          state <= 1'h1;
        end
      end else begin
        if (state) begin
          state <= 1'h0;
        end
      end
    end
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (1'h0) begin
          $fwrite(32'h80000002,"write tlb tag %x set %x \n",io_vaddr_bits_tag,io_vaddr_bits_set); // @[TLBLegacy.scala 73:11:@2803.8]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_19 & _T_44) begin
          $fwrite(32'h80000002,"** TLB hit \n"); // @[TLBLegacy.scala 100:15:@2839.12]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_23 & _T_44) begin
          $fwrite(32'h80000002,"** TLB miss \n"); // @[TLBLegacy.scala 108:15:@2859.12]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
  end
endmodule
