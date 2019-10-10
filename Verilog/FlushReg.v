module FlushReg( // @[:@742.2]
  input         clock, // @[:@743.4]
  input         reset, // @[:@744.4]
  output        io_enq_ready, // @[:@745.4]
  input         io_enq_valid, // @[:@745.4]
  input  [31:0] io_enq_bits_inst, // @[:@745.4]
  input         io_enq_bits_tag, // @[:@745.4]
  input  [63:0] io_enq_bits_pc, // @[:@745.4]
  input         io_deq_ready, // @[:@745.4]
  output        io_deq_valid, // @[:@745.4]
  output [31:0] io_deq_bits_inst, // @[:@745.4]
  output        io_deq_bits_tag, // @[:@745.4]
  output [63:0] io_deq_bits_pc, // @[:@745.4]
  input         io_flush // @[:@745.4]
);
  reg [31:0] _T_22_inst; // @[FlushReg.scala 23:24:@747.4]
  reg [31:0] _RAND_0;
  reg  _T_22_tag; // @[FlushReg.scala 23:24:@747.4]
  reg [31:0] _RAND_1;
  reg [63:0] _T_22_pc; // @[FlushReg.scala 23:24:@747.4]
  reg [63:0] _RAND_2;
  reg  _T_25; // @[FlushReg.scala 24:30:@748.4]
  reg [31:0] _RAND_3;
  wire  _T_27; // @[FlushReg.scala 26:33:@749.4]
  wire  _T_28; // @[FlushReg.scala 26:40:@750.4]
  wire  _T_29; // @[FlushReg.scala 26:56:@751.4]
  wire  _GEN_3; // @[FlushReg.scala 28:17:@754.4]
  wire  _T_33; // @[FlushReg.scala 35:28:@764.4]
  assign _T_27 = _T_25 == 1'h0; // @[FlushReg.scala 26:33:@749.4]
  assign _T_28 = _T_27 | io_deq_ready; // @[FlushReg.scala 26:40:@750.4]
  assign _T_29 = _T_28 | io_flush; // @[FlushReg.scala 26:56:@751.4]
  assign _GEN_3 = _T_29 ? io_enq_valid : _T_25; // @[FlushReg.scala 28:17:@754.4]
  assign _T_33 = io_flush == 1'h0; // @[FlushReg.scala 35:28:@764.4]
  assign io_enq_ready = _T_28 | io_flush; // @[FlushReg.scala 32:16:@760.4]
  assign io_deq_valid = _T_25 & _T_33; // @[FlushReg.scala 35:16:@766.4]
  assign io_deq_bits_inst = _T_22_inst; // @[FlushReg.scala 34:15:@763.4]
  assign io_deq_bits_tag = _T_22_tag; // @[FlushReg.scala 34:15:@762.4]
  assign io_deq_bits_pc = _T_22_pc; // @[FlushReg.scala 34:15:@761.4]
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
  _T_22_inst = _RAND_0[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  _T_22_tag = _RAND_1[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {2{`RANDOM}};
  _T_22_pc = _RAND_2[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  _T_25 = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (_T_29) begin
      _T_22_inst <= io_enq_bits_inst;
    end
    if (_T_29) begin
      _T_22_tag <= io_enq_bits_tag;
    end
    if (_T_29) begin
      _T_22_pc <= io_enq_bits_pc;
    end
    if (reset) begin
      _T_25 <= 1'h0;
    end else begin
      if (_T_29) begin
        _T_25 <= io_enq_valid;
      end
    end
  end
endmodule
