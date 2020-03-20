module RRArbiter( // @[:@714.2]
  input        clock, // @[:@715.4]
  input        reset, // @[:@716.4]
  input  [1:0] io_ready, // @[:@717.4]
  input        io_next_ready, // @[:@717.4]
  output       io_next_valid, // @[:@717.4]
  output       io_next_bits // @[:@717.4]
);
  wire  valid; // @[issue.scala 35:24:@719.4]
  reg  curr; // @[issue.scala 36:21:@720.4]
  reg [31:0] _RAND_0;
  wire [1:0] _T_16; // @[issue.scala 20:22:@721.4]
  wire  _T_17; // @[issue.scala 21:25:@722.4]
  wire [1:0] _T_19; // @[issue.scala 21:30:@723.4]
  wire  _T_20; // @[issue.scala 21:30:@724.4]
  wire [2:0] _GEN_1; // @[issue.scala 21:22:@725.4]
  wire [2:0] _T_21; // @[issue.scala 21:22:@725.4]
  wire [2:0] _GEN_2; // @[issue.scala 22:20:@726.4]
  wire [2:0] _T_22; // @[issue.scala 22:20:@726.4]
  wire [1:0] ready_ofst; // @[issue.scala 23:8:@727.4]
  wire  _T_23; // @[OneHot.scala 39:40:@728.4]
  wire  ofst; // @[Mux.scala 31:69:@730.4]
  wire [1:0] _T_27; // @[issue.scala 39:19:@731.4]
  wire  next; // @[issue.scala 39:19:@732.4]
  wire  _T_28; // @[issue.scala 43:22:@735.4]
  wire [1:0] _T_30; // @[issue.scala 43:47:@737.6]
  wire  _T_31; // @[issue.scala 43:47:@738.6]
  wire  _GEN_0; // @[issue.scala 43:32:@736.4]
  assign valid = io_ready != 2'h0; // @[issue.scala 35:24:@719.4]
  assign _T_16 = io_ready >> curr; // @[issue.scala 20:22:@721.4]
  assign _T_17 = ~ curr; // @[issue.scala 21:25:@722.4]
  assign _T_19 = _T_17 + 1'h1; // @[issue.scala 21:30:@723.4]
  assign _T_20 = _T_17 + 1'h1; // @[issue.scala 21:30:@724.4]
  assign _GEN_1 = {{1'd0}, io_ready}; // @[issue.scala 21:22:@725.4]
  assign _T_21 = _GEN_1 << _T_20; // @[issue.scala 21:22:@725.4]
  assign _GEN_2 = {{1'd0}, _T_16}; // @[issue.scala 22:20:@726.4]
  assign _T_22 = _T_21 | _GEN_2; // @[issue.scala 22:20:@726.4]
  assign ready_ofst = _T_22[1:0]; // @[issue.scala 23:8:@727.4]
  assign _T_23 = ready_ofst[0]; // @[OneHot.scala 39:40:@728.4]
  assign ofst = _T_23 ? 1'h0 : 1'h1; // @[Mux.scala 31:69:@730.4]
  assign _T_27 = curr + ofst; // @[issue.scala 39:19:@731.4]
  assign next = curr + ofst; // @[issue.scala 39:19:@732.4]
  assign _T_28 = io_next_ready & valid; // @[issue.scala 43:22:@735.4]
  assign _T_30 = next + 1'h1; // @[issue.scala 43:47:@737.6]
  assign _T_31 = next + 1'h1; // @[issue.scala 43:47:@738.6]
  assign _GEN_0 = _T_28 ? _T_31 : curr; // @[issue.scala 43:32:@736.4]
  assign io_next_valid = io_ready != 2'h0; // @[issue.scala 42:17:@734.4]
  assign io_next_bits = curr + ofst; // @[issue.scala 41:16:@733.4]
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
  curr = _RAND_0[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (reset) begin
      curr <= 1'h0;
    end else begin
      if (_T_28) begin
        curr <= _T_31;
      end
    end
  end
endmodule
