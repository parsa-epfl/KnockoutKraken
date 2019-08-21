module TLBUnit( // @[:@2768.2]
  input         clock, // @[:@2769.4]
  input         reset, // @[:@2770.4]
  input         io_vaddr_valid, // @[:@2771.4]
  input  [27:0] io_vaddr_bits_tag, // @[:@2771.4]
  input  [7:0]  io_vaddr_bits_set, // @[:@2771.4]
  output        io_hit_valid, // @[:@2771.4]
  output        io_hit_bits // @[:@2771.4]
);
  wire  tag_mem_clock; // @[TLB.scala 52:23:@2781.4]
  wire [7:0] tag_mem_io_addr_r; // @[TLB.scala 52:23:@2781.4]
  wire [7:0] tag_mem_io_addr_w; // @[TLB.scala 52:23:@2781.4]
  wire [27:0] tag_mem_io_data_in; // @[TLB.scala 52:23:@2781.4]
  wire [27:0] tag_mem_io_data_out; // @[TLB.scala 52:23:@2781.4]
  reg [7:0] set_reg; // @[TLB.scala 41:20:@2773.4]
  reg [31:0] _RAND_0;
  reg [27:0] tag_reg; // @[TLB.scala 42:20:@2774.4]
  reg [31:0] _RAND_1;
  wire  _T_44; // @[TLB.scala 72:11:@2799.6]
  reg  state; // @[TLB.scala 83:22:@2814.4]
  reg [31:0] _RAND_2;
  wire  _T_55; // @[TLB.scala 87:19:@2815.4]
  wire [255:0] _T_56; // @[TLB.scala 87:40:@2816.4]
  wire  _T_57; // @[TLB.scala 87:40:@2817.4]
  wire  _T_60; // @[Conditional.scala 37:30:@2821.4]
  wire  _GEN_4; // @[TLB.scala 92:49:@2825.6]
  wire  _GEN_10; // @[Conditional.scala 39:67:@2831.6]
  wire  _GEN_13; // @[Conditional.scala 40:58:@2822.4]
  wire  _GEN_17; // @[TLB.scala 99:15:@2837.12]
  wire  _GEN_18; // @[TLB.scala 99:15:@2837.12]
  wire  _GEN_19; // @[TLB.scala 99:15:@2837.12]
  wire  _GEN_22; // @[TLB.scala 107:15:@2857.12]
  wire  _GEN_23; // @[TLB.scala 107:15:@2857.12]
  SRAM tag_mem ( // @[TLB.scala 52:23:@2781.4]
    .clock(tag_mem_clock),
    .io_addr_r(tag_mem_io_addr_r),
    .io_addr_w(tag_mem_io_addr_w),
    .io_data_in(tag_mem_io_data_in),
    .io_data_out(tag_mem_io_data_out)
  );
  assign _T_44 = reset == 1'h0; // @[TLB.scala 72:11:@2799.6]
  assign _T_55 = tag_mem_io_data_out == tag_reg; // @[TLB.scala 87:19:@2815.4]
  assign _T_56 = 256'h0 >> set_reg; // @[TLB.scala 87:40:@2816.4]
  assign _T_57 = _T_56[0]; // @[TLB.scala 87:40:@2817.4]
  assign _T_60 = 1'h0 == state; // @[Conditional.scala 37:30:@2821.4]
  assign _GEN_4 = io_vaddr_valid ? 1'h1 : state; // @[TLB.scala 92:49:@2825.6]
  assign _GEN_10 = state ? 1'h0 : state; // @[Conditional.scala 39:67:@2831.6]
  assign _GEN_13 = _T_60 ? _GEN_4 : _GEN_10; // @[Conditional.scala 40:58:@2822.4]
  assign io_hit_valid = _T_60 ? 1'h0 : state; // @[TLB.scala 88:16:@2820.4 TLB.scala 97:20:@2832.8]
  assign io_hit_bits = _T_55 & _T_57; // @[TLB.scala 87:7:@2819.4]
  assign tag_mem_clock = clock; // @[:@2782.4]
  assign tag_mem_io_addr_r = io_vaddr_bits_set; // @[TLB.scala 53:21:@2784.4]
  assign tag_mem_io_addr_w = io_vaddr_bits_set; // @[TLB.scala 54:21:@2785.4]
  assign tag_mem_io_data_in = io_vaddr_bits_tag; // @[TLB.scala 55:22:@2786.4]
  assign _GEN_17 = _T_60 == 1'h0; // @[TLB.scala 99:15:@2837.12]
  assign _GEN_18 = _GEN_17 & state; // @[TLB.scala 99:15:@2837.12]
  assign _GEN_19 = _GEN_18 & io_hit_bits; // @[TLB.scala 99:15:@2837.12]
  assign _GEN_22 = io_hit_bits == 1'h0; // @[TLB.scala 107:15:@2857.12]
  assign _GEN_23 = _GEN_18 & _GEN_22; // @[TLB.scala 107:15:@2857.12]
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
          $fwrite(32'h80000002,"write tlb tag %x set %x \n",io_vaddr_bits_tag,io_vaddr_bits_set); // @[TLB.scala 72:11:@2801.8]
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
          $fwrite(32'h80000002,"** TLB hit \n"); // @[TLB.scala 99:15:@2837.12]
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
          $fwrite(32'h80000002,"** TLB miss \n"); // @[TLB.scala 107:15:@2857.12]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
  end
endmodule
