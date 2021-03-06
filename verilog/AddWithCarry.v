module AddWithCarry(
  input         io_is32bit,
  input  [63:0] io_a,
  input  [63:0] io_b,
  input         io_carry,
  output [63:0] io_res,
  output [3:0]  io_nzcv
);
  wire [64:0] _T; // @[Execute.scala 82:37]
  wire [64:0] _GEN_7; // @[Execute.scala 82:46]
  wire [64:0] _T_2; // @[Execute.scala 82:46]
  wire [64:0] _T_6; // @[Execute.scala 83:60]
  wire [64:0] _T_8; // @[Execute.scala 83:67]
  wire [32:0] _T_22; // @[Execute.scala 101:33]
  wire [32:0] _GEN_9; // @[Execute.scala 101:48]
  wire [32:0] _T_24; // @[Execute.scala 101:48]
  wire [64:0] unsigned_sum; // @[Execute.scala 100:20]
  wire [63:0] res; // @[Execute.scala 100:20]
  wire  _T_12; // @[Execute.scala 89:24]
  wire [64:0] _GEN_10; // @[Execute.scala 90:24]
  wire  _T_13; // @[Execute.scala 90:24]
  wire [63:0] _T_14; // @[Execute.scala 91:24]
  wire [31:0] _T_27; // @[Execute.scala 103:33]
  wire [31:0] _T_29; // @[Execute.scala 103:54]
  wire [32:0] _T_31; // @[Execute.scala 103:62]
  wire [32:0] _T_33; // @[Execute.scala 103:69]
  wire [64:0] signed_sum; // @[Execute.scala 100:20]
  wire [64:0] _T_15; // @[Execute.scala 91:46]
  wire [64:0] _GEN_12; // @[Execute.scala 91:31]
  wire  _T_16; // @[Execute.scala 91:31]
  wire [32:0] _GEN_13; // @[Execute.scala 106:20]
  wire  _T_40; // @[Execute.scala 106:20]
  wire  c; // @[Execute.scala 100:20]
  wire [31:0] _T_42; // @[Execute.scala 107:20]
  wire [32:0] _T_44; // @[Execute.scala 107:48]
  wire [32:0] _GEN_14; // @[Execute.scala 107:27]
  wire  _T_45; // @[Execute.scala 107:27]
  wire  v; // @[Execute.scala 100:20]
  wire [1:0] _T_17; // @[Execute.scala 98:19]
  wire  n; // @[Execute.scala 100:20]
  wire  _T_37; // @[Execute.scala 105:20]
  wire  z; // @[Execute.scala 100:20]
  wire [1:0] _T_18; // @[Execute.scala 98:19]
  assign _T = io_a + io_b; // @[Execute.scala 82:37]
  assign _GEN_7 = {{64'd0}, io_carry}; // @[Execute.scala 82:46]
  assign _T_2 = _T + _GEN_7; // @[Execute.scala 82:46]
  assign _T_6 = $signed(io_a) + $signed(io_b); // @[Execute.scala 83:60]
  assign _T_8 = _T_6 + _GEN_7; // @[Execute.scala 83:67]
  assign _T_22 = io_a[31:0] + io_b[31:0]; // @[Execute.scala 101:33]
  assign _GEN_9 = {{32'd0}, io_carry}; // @[Execute.scala 101:48]
  assign _T_24 = _T_22 + _GEN_9; // @[Execute.scala 101:48]
  assign unsigned_sum = io_is32bit ? {{32'd0}, _T_24} : _T_2; // @[Execute.scala 100:20]
  assign res = io_is32bit ? {{32'd0}, unsigned_sum[31:0]} : unsigned_sum[63:0]; // @[Execute.scala 100:20]
  assign _T_12 = res == 64'h0; // @[Execute.scala 89:24]
  assign _GEN_10 = {{1'd0}, res}; // @[Execute.scala 90:24]
  assign _T_13 = _GEN_10 != unsigned_sum; // @[Execute.scala 90:24]
  assign _T_14 = io_is32bit ? {{32'd0}, unsigned_sum[31:0]} : unsigned_sum[63:0]; // @[Execute.scala 91:24]
  assign _T_27 = io_a[31:0]; // @[Execute.scala 103:33]
  assign _T_29 = io_b[31:0]; // @[Execute.scala 103:54]
  assign _T_31 = $signed(_T_27) + $signed(_T_29); // @[Execute.scala 103:62]
  assign _T_33 = _T_31 + _GEN_9; // @[Execute.scala 103:69]
  assign signed_sum = io_is32bit ? {{32'd0}, _T_33} : _T_8; // @[Execute.scala 100:20]
  assign _T_15 = io_is32bit ? {{32'd0}, _T_33} : _T_8; // @[Execute.scala 91:46]
  assign _GEN_12 = {{1{_T_14[63]}},_T_14}; // @[Execute.scala 91:31]
  assign _T_16 = $signed(_GEN_12) != $signed(_T_15); // @[Execute.scala 91:31]
  assign _GEN_13 = {{1'd0}, res[31:0]}; // @[Execute.scala 106:20]
  assign _T_40 = _GEN_13 != unsigned_sum[32:0]; // @[Execute.scala 106:20]
  assign c = io_is32bit ? _T_40 : _T_13; // @[Execute.scala 100:20]
  assign _T_42 = res[31:0]; // @[Execute.scala 107:20]
  assign _T_44 = signed_sum[32:0]; // @[Execute.scala 107:48]
  assign _GEN_14 = {{1{_T_42[31]}},_T_42}; // @[Execute.scala 107:27]
  assign _T_45 = $signed(_GEN_14) != $signed(_T_44); // @[Execute.scala 107:27]
  assign v = io_is32bit ? _T_45 : _T_16; // @[Execute.scala 100:20]
  assign _T_17 = {c,v}; // @[Execute.scala 98:19]
  assign n = io_is32bit ? res[31] : res[63]; // @[Execute.scala 100:20]
  assign _T_37 = res[31:0] == 32'h0; // @[Execute.scala 105:20]
  assign z = io_is32bit ? _T_37 : _T_12; // @[Execute.scala 100:20]
  assign _T_18 = {n,z}; // @[Execute.scala 98:19]
  assign io_res = io_is32bit ? {{32'd0}, unsigned_sum[31:0]} : unsigned_sum[63:0]; // @[Execute.scala 97:10]
  assign io_nzcv = {_T_18,_T_17}; // @[Execute.scala 98:11]
endmodule
