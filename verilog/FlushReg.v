module FlushReg(
  input         clock,
  input         reset,
  output        io_enq_ready,
  input         io_enq_valid,
  input  [31:0] io_enq_bits_inst,
  input         io_enq_bits_tag,
  input  [63:0] io_enq_bits_pc,
  input         io_deq_ready,
  output        io_deq_valid,
  output [31:0] io_deq_bits_inst,
  output        io_deq_bits_tag,
  output [63:0] io_deq_bits_pc,
  input         io_flush
);
  reg [31:0] reg_inst; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_0;
  reg  reg_tag; // @[FlushReg.scala 23:16]
  reg [31:0] _RAND_1;
  reg [63:0] reg_pc; // @[FlushReg.scala 23:16]
  reg [63:0] _RAND_2;
  reg  valid; // @[FlushReg.scala 24:22]
  reg [31:0] _RAND_3;
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
  assign io_deq_bits_inst = reg_inst; // @[FlushReg.scala 34:15]
  assign io_deq_bits_tag = reg_tag; // @[FlushReg.scala 34:15]
  assign io_deq_bits_pc = reg_pc; // @[FlushReg.scala 34:15]
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
  reg_inst = _RAND_0[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  reg_tag = _RAND_1[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {2{`RANDOM}};
  reg_pc = _RAND_2[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  valid = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (do_enq) begin
      reg_inst <= io_enq_bits_inst;
    end
    if (do_enq) begin
      reg_tag <= io_enq_bits_tag;
    end
    if (do_enq) begin
      reg_pc <= io_enq_bits_pc;
    end
    if (reset) begin
      valid <= 1'h0;
    end else if (do_enq) begin
      valid <= io_enq_valid;
    end
  end
endmodule
