module FlushQueue(
  input          clock,
  input          reset,
  output         io_enq_ready,
  input          io_enq_valid,
  input  [168:0] io_enq_bits,
  input          io_deq_ready,
  output         io_deq_valid,
  output [168:0] io_deq_bits,
  input          io_flush
);
  reg [168:0] ram [0:3]; // @[FlushReg.scala 42:16]
  reg [191:0] _RAND_0;
  wire [168:0] ram__T_20_data; // @[FlushReg.scala 42:16]
  wire [1:0] ram__T_20_addr; // @[FlushReg.scala 42:16]
  wire [168:0] ram__T_9_data; // @[FlushReg.scala 42:16]
  wire [1:0] ram__T_9_addr; // @[FlushReg.scala 42:16]
  wire  ram__T_9_mask; // @[FlushReg.scala 42:16]
  wire  ram__T_9_en; // @[FlushReg.scala 42:16]
  reg [1:0] enq_ptr; // @[FlushReg.scala 43:24]
  reg [31:0] _RAND_1;
  reg [1:0] deq_ptr; // @[FlushReg.scala 44:24]
  reg [31:0] _RAND_2;
  reg  maybe_full; // @[FlushReg.scala 45:27]
  reg [31:0] _RAND_3;
  wire  ptr_match; // @[FlushReg.scala 47:36]
  wire  _T_1; // @[FlushReg.scala 48:37]
  wire  empty; // @[FlushReg.scala 48:34]
  wire  full; // @[FlushReg.scala 49:33]
  wire  _T_4; // @[FlushReg.scala 51:42]
  wire  _T_5; // @[FlushReg.scala 51:48]
  wire  do_enq; // @[FlushReg.scala 51:38]
  wire  _T_7; // @[FlushReg.scala 52:41]
  wire  do_deq; // @[FlushReg.scala 52:38]
  wire [1:0] _T_11; // @[FlushReg.scala 56:24]
  wire [1:0] _T_13; // @[FlushReg.scala 59:24]
  wire  _T_14; // @[FlushReg.scala 62:15]
  wire  _T_18; // @[FlushReg.scala 67:29]
  wire  _T_19; // @[FlushReg.scala 67:26]
  assign ram__T_20_addr = deq_ptr;
  assign ram__T_20_data = ram[ram__T_20_addr]; // @[FlushReg.scala 42:16]
  assign ram__T_9_data = io_enq_bits;
  assign ram__T_9_addr = enq_ptr;
  assign ram__T_9_mask = 1'h1;
  assign ram__T_9_en = io_enq_valid & _T_5;
  assign ptr_match = enq_ptr == deq_ptr; // @[FlushReg.scala 47:36]
  assign _T_1 = ~maybe_full; // @[FlushReg.scala 48:37]
  assign empty = ptr_match & _T_1; // @[FlushReg.scala 48:34]
  assign full = ptr_match & maybe_full; // @[FlushReg.scala 49:33]
  assign _T_4 = ~full; // @[FlushReg.scala 51:42]
  assign _T_5 = _T_4 | io_deq_ready; // @[FlushReg.scala 51:48]
  assign do_enq = io_enq_valid & _T_5; // @[FlushReg.scala 51:38]
  assign _T_7 = ~empty; // @[FlushReg.scala 52:41]
  assign do_deq = io_deq_ready & _T_7; // @[FlushReg.scala 52:38]
  assign _T_11 = enq_ptr + 2'h1; // @[FlushReg.scala 56:24]
  assign _T_13 = deq_ptr + 2'h1; // @[FlushReg.scala 59:24]
  assign _T_14 = do_enq != do_deq; // @[FlushReg.scala 62:15]
  assign _T_18 = ~io_flush; // @[FlushReg.scala 67:29]
  assign _T_19 = _T_7 & _T_18; // @[FlushReg.scala 67:26]
  assign io_enq_ready = io_flush ? 1'h0 : _T_5; // @[FlushReg.scala 66:16 FlushReg.scala 76:18]
  assign io_deq_valid = io_flush ? 1'h0 : _T_19; // @[FlushReg.scala 67:16 FlushReg.scala 75:18]
  assign io_deq_bits = ram__T_20_data; // @[FlushReg.scala 68:15]
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
  _RAND_0 = {6{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 4; initvar = initvar+1)
    ram[initvar] = _RAND_0[168:0];
  `endif // RANDOMIZE_MEM_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  enq_ptr = _RAND_1[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  deq_ptr = _RAND_2[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  maybe_full = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if(ram__T_9_en & ram__T_9_mask) begin
      ram[ram__T_9_addr] <= ram__T_9_data; // @[FlushReg.scala 42:16]
    end
    if (reset) begin
      enq_ptr <= 2'h0;
    end else if (io_flush) begin
      enq_ptr <= 2'h0;
    end else if (do_enq) begin
      enq_ptr <= _T_11;
    end
    if (reset) begin
      deq_ptr <= 2'h0;
    end else if (io_flush) begin
      deq_ptr <= 2'h0;
    end else if (do_deq) begin
      deq_ptr <= _T_13;
    end
    if (reset) begin
      maybe_full <= 1'h0;
    end else if (io_flush) begin
      maybe_full <= 1'h0;
    end else if (_T_14) begin
      maybe_full <= do_enq;
    end
  end
endmodule
