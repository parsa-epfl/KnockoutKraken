module FReg_2( // @[:@2704.2]
  input         clock, // @[:@2705.4]
  input         reset, // @[:@2706.4]
  input         io_enq_valid, // @[:@2707.4]
  input  [63:0] io_enq_bits_offset, // @[:@2707.4]
  input         io_enq_bits_tag, // @[:@2707.4]
  output        io_deq_valid, // @[:@2707.4]
  output [63:0] io_deq_bits_offset, // @[:@2707.4]
  output        io_deq_bits_tag, // @[:@2707.4]
  input         io_flush // @[:@2707.4]
);
  reg [63:0] _T_34_offset; // @[FReg.scala 45:24:@2709.4]
  reg [63:0] _RAND_0;
  reg  _T_34_tag; // @[FReg.scala 45:24:@2709.4]
  reg [31:0] _RAND_1;
  reg  _T_37; // @[FReg.scala 46:30:@2710.4]
  reg [31:0] _RAND_2;
  wire  _T_45; // @[FReg.scala 57:28:@2724.4]
  assign _T_45 = io_flush == 1'h0; // @[FReg.scala 57:28:@2724.4]
  assign io_deq_valid = _T_37 & _T_45; // @[FReg.scala 57:16:@2726.4]
  assign io_deq_bits_offset = _T_34_offset; // @[FReg.scala 56:15:@2723.4]
  assign io_deq_bits_tag = _T_34_tag; // @[FReg.scala 56:15:@2722.4]
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
  _T_34_offset = _RAND_0[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  _T_34_tag = _RAND_1[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  _T_37 = _RAND_2[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    _T_34_offset <= io_enq_bits_offset;
    _T_34_tag <= io_enq_bits_tag;
    if (reset) begin
      _T_37 <= 1'h0;
    end else begin
      _T_37 <= io_enq_valid;
    end
  end
endmodule
