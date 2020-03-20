module LogicALU(
  input  [63:0] io_a,
  input  [63:0] io_b,
  input  [3:0]  io_opcode,
  output [63:0] io_res,
  output [3:0]  io_nzcv,
  input         io_is32bit
);
  wire [63:0] _T; // @[Execute.scala 31:34]
  wire [63:0] _T_2; // @[Execute.scala 33:34]
  wire [63:0] _T_4; // @[Execute.scala 35:34]
  wire  _T_6; // @[Mux.scala 80:60]
  wire [63:0] _T_7; // @[Mux.scala 80:57]
  wire  _T_8; // @[Mux.scala 80:60]
  wire [63:0] _T_9; // @[Mux.scala 80:57]
  wire  _T_10; // @[Mux.scala 80:60]
  wire [63:0] _T_11; // @[Mux.scala 80:57]
  wire  _T_12; // @[Mux.scala 80:60]
  wire [63:0] _T_13; // @[Mux.scala 80:57]
  wire  _T_14; // @[Mux.scala 80:60]
  wire [63:0] _T_15; // @[Mux.scala 80:57]
  wire  _T_16; // @[Mux.scala 80:60]
  wire [63:0] res; // @[Mux.scala 80:57]
  wire  _T_19; // @[Execute.scala 40:21]
  wire  _T_20; // @[Execute.scala 40:58]
  wire [1:0] _T_21; // @[Cat.scala 29:58]
  assign _T = io_a & io_b; // @[Execute.scala 31:34]
  assign _T_2 = io_a | io_b; // @[Execute.scala 33:34]
  assign _T_4 = io_a ^ io_b; // @[Execute.scala 35:34]
  assign _T_6 = 4'h0 == io_opcode; // @[Mux.scala 80:60]
  assign _T_7 = _T_6 ? _T : 64'h0; // @[Mux.scala 80:57]
  assign _T_8 = 4'h1 == io_opcode; // @[Mux.scala 80:60]
  assign _T_9 = _T_8 ? _T : _T_7; // @[Mux.scala 80:57]
  assign _T_10 = 4'h2 == io_opcode; // @[Mux.scala 80:60]
  assign _T_11 = _T_10 ? _T_2 : _T_9; // @[Mux.scala 80:57]
  assign _T_12 = 4'h3 == io_opcode; // @[Mux.scala 80:60]
  assign _T_13 = _T_12 ? _T_2 : _T_11; // @[Mux.scala 80:57]
  assign _T_14 = 4'h4 == io_opcode; // @[Mux.scala 80:60]
  assign _T_15 = _T_14 ? _T_4 : _T_13; // @[Mux.scala 80:57]
  assign _T_16 = 4'h5 == io_opcode; // @[Mux.scala 80:60]
  assign res = _T_16 ? _T_4 : _T_15; // @[Mux.scala 80:57]
  assign _T_19 = io_is32bit ? res[31] : res[63]; // @[Execute.scala 40:21]
  assign _T_20 = res == 64'h0; // @[Execute.scala 40:58]
  assign _T_21 = {_T_19,_T_20}; // @[Cat.scala 29:58]
  assign io_res = _T_16 ? _T_4 : _T_15; // @[Execute.scala 39:10]
  assign io_nzcv = {_T_21,2'h0}; // @[Execute.scala 40:11]
endmodule
