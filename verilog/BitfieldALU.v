module BitfieldALU(
  input  [3:0]  io_op,
  input  [5:0]  io_imms,
  input  [63:0] io_src,
  input  [63:0] io_dst,
  input  [63:0] io_wmask,
  input  [63:0] io_tmask,
  input  [63:0] io_rorSrcR,
  output [63:0] io_res
);
  wire  _T; // @[Execute.scala 156:14]
  wire [63:0] _T_1; // @[Execute.scala 157:23]
  wire [63:0] _T_2; // @[Execute.scala 158:22]
  wire [63:0] _T_5; // @[Execute.scala 158:15]
  wire [63:0] _T_6; // @[Execute.scala 160:22]
  wire  _T_9; // @[Execute.scala 162:20]
  wire  _T_17; // @[Execute.scala 168:20]
  wire [63:0] top; // @[Execute.scala 156:27]
  wire [63:0] _T_7; // @[Execute.scala 160:20]
  wire [63:0] _T_10; // @[Execute.scala 163:22]
  wire [63:0] _T_11; // @[Execute.scala 163:20]
  wire [63:0] _T_13; // @[Execute.scala 163:33]
  wire [63:0] _GEN_0; // @[Execute.scala 168:33]
  wire [63:0] _GEN_4; // @[Execute.scala 162:32]
  wire [63:0] bot; // @[Execute.scala 156:27]
  wire [63:0] _T_8; // @[Execute.scala 161:20]
  wire [63:0] _T_15; // @[Execute.scala 166:24]
  wire [63:0] _GEN_2; // @[Execute.scala 168:33]
  wire [63:0] _GEN_6; // @[Execute.scala 162:32]
  wire [63:0] _GEN_7; // @[Execute.scala 162:32]
  wire [63:0] aluVal1; // @[Execute.scala 156:27]
  wire [63:0] aluVal2; // @[Execute.scala 156:27]
  assign _T = io_op == 4'h0; // @[Execute.scala 156:14]
  assign _T_1 = io_rorSrcR & io_wmask; // @[Execute.scala 157:23]
  assign _T_2 = io_src >> io_imms; // @[Execute.scala 158:22]
  assign _T_5 = _T_2[0] ? 64'hffffffffffffffff : 64'h0; // @[Execute.scala 158:15]
  assign _T_6 = ~io_tmask; // @[Execute.scala 160:22]
  assign _T_9 = io_op == 4'h1; // @[Execute.scala 162:20]
  assign _T_17 = io_op == 4'h2; // @[Execute.scala 168:20]
  assign top = _T ? _T_5 : 64'h0; // @[Execute.scala 156:27]
  assign _T_7 = top & _T_6; // @[Execute.scala 160:20]
  assign _T_10 = ~io_wmask; // @[Execute.scala 163:22]
  assign _T_11 = io_dst & _T_10; // @[Execute.scala 163:20]
  assign _T_13 = _T_11 | _T_1; // @[Execute.scala 163:33]
  assign _GEN_0 = _T_17 ? _T_1 : 64'h0; // @[Execute.scala 168:33]
  assign _GEN_4 = _T_9 ? _T_13 : _GEN_0; // @[Execute.scala 162:32]
  assign bot = _T ? _T_1 : _GEN_4; // @[Execute.scala 156:27]
  assign _T_8 = bot & io_tmask; // @[Execute.scala 161:20]
  assign _T_15 = io_dst & _T_6; // @[Execute.scala 166:24]
  assign _GEN_2 = _T_17 ? _T_8 : 64'h0; // @[Execute.scala 168:33]
  assign _GEN_6 = _T_9 ? _T_15 : _GEN_2; // @[Execute.scala 162:32]
  assign _GEN_7 = _T_9 ? _T_8 : _GEN_2; // @[Execute.scala 162:32]
  assign aluVal1 = _T ? _T_7 : _GEN_6; // @[Execute.scala 156:27]
  assign aluVal2 = _T ? _T_8 : _GEN_7; // @[Execute.scala 156:27]
  assign io_res = aluVal1 | aluVal2; // @[Execute.scala 179:10]
endmodule
