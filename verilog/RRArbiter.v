module RRArbiter(
  input        clock,
  input        reset,
  input  [1:0] io_ready,
  input        io_next_ready,
  output       io_next_valid,
  output       io_next_bits
);
  wire  valid; // @[Issue.scala 36:24]
  reg  curr; // @[Issue.scala 37:21]
  reg [31:0] _RAND_0;
  wire [1:0] _T; // @[Issue.scala 21:22]
  wire  _T_1; // @[Issue.scala 22:25]
  wire  _T_3; // @[Issue.scala 22:30]
  wire [2:0] _GEN_1; // @[Issue.scala 22:22]
  wire [2:0] _T_4; // @[Issue.scala 22:22]
  wire [2:0] _GEN_2; // @[Issue.scala 23:20]
  wire [2:0] _T_5; // @[Issue.scala 23:20]
  wire [1:0] ready_ofst; // @[Issue.scala 24:8]
  wire  ofst; // @[Mux.scala 47:69]
  wire  next; // @[Issue.scala 40:19]
  wire  _T_9; // @[Issue.scala 44:22]
  wire  _T_11; // @[Issue.scala 44:47]
  assign valid = |io_ready; // @[Issue.scala 36:24]
  assign _T = io_ready >> curr; // @[Issue.scala 21:22]
  assign _T_1 = ~curr; // @[Issue.scala 22:25]
  assign _T_3 = _T_1 + 1'h1; // @[Issue.scala 22:30]
  assign _GEN_1 = {{1'd0}, io_ready}; // @[Issue.scala 22:22]
  assign _T_4 = _GEN_1 << _T_3; // @[Issue.scala 22:22]
  assign _GEN_2 = {{1'd0}, _T}; // @[Issue.scala 23:20]
  assign _T_5 = _T_4 | _GEN_2; // @[Issue.scala 23:20]
  assign ready_ofst = _T_5[1:0]; // @[Issue.scala 24:8]
  assign ofst = ready_ofst[0] ? 1'h0 : 1'h1; // @[Mux.scala 47:69]
  assign next = curr + ofst; // @[Issue.scala 40:19]
  assign _T_9 = io_next_ready & valid; // @[Issue.scala 44:22]
  assign _T_11 = next + 1'h1; // @[Issue.scala 44:47]
  assign io_next_valid = |io_ready; // @[Issue.scala 43:17]
  assign io_next_bits = curr + ofst; // @[Issue.scala 42:16]
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
  curr = _RAND_0[0:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (reset) begin
      curr <= 1'h0;
    end else if (_T_9) begin
      curr <= _T_11;
    end
  end
endmodule
