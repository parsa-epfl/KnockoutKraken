module ConditionHolds(
  input  [3:0] io_cond,
  input  [3:0] io_nzcv,
  output       io_res
);
  wire  PSTATE_N; // @[Execute.scala 51:25]
  wire  PSTATE_Z; // @[Execute.scala 52:25]
  wire  PSTATE_C; // @[Execute.scala 53:25]
  wire  PSTATE_V; // @[Execute.scala 54:25]
  wire  _T_1; // @[Execute.scala 55:27]
  wire  _T_4; // @[Execute.scala 56:27]
  wire  _T_7; // @[Execute.scala 57:27]
  wire  _T_10; // @[Execute.scala 58:27]
  wire  _T_13; // @[Execute.scala 59:27]
  wire  _T_15; // @[Execute.scala 59:82]
  wire  _T_16; // @[Execute.scala 59:70]
  wire  _T_18; // @[Execute.scala 60:27]
  wire  _T_19; // @[Execute.scala 60:62]
  wire  _T_21; // @[Execute.scala 61:27]
  wire  _T_24; // @[Execute.scala 61:75]
  wire  _T_26; // @[Execute.scala 62:27]
  wire  _GEN_1; // @[Execute.scala 61:41]
  wire  _GEN_2; // @[Execute.scala 60:41]
  wire  _GEN_3; // @[Execute.scala 59:41]
  wire  _GEN_4; // @[Execute.scala 58:41]
  wire  _GEN_5; // @[Execute.scala 57:41]
  wire  _GEN_6; // @[Execute.scala 56:41]
  wire  result; // @[Execute.scala 55:41]
  wire  _T_29; // @[Execute.scala 64:38]
  wire  _T_30; // @[Execute.scala 64:27]
  wire  _T_31; // @[Execute.scala 65:15]
  assign PSTATE_N = io_nzcv[3]; // @[Execute.scala 51:25]
  assign PSTATE_Z = io_nzcv[2]; // @[Execute.scala 52:25]
  assign PSTATE_C = io_nzcv[1]; // @[Execute.scala 53:25]
  assign PSTATE_V = io_nzcv[0]; // @[Execute.scala 54:25]
  assign _T_1 = io_cond[3:1] == 3'h0; // @[Execute.scala 55:27]
  assign _T_4 = io_cond[3:1] == 3'h1; // @[Execute.scala 56:27]
  assign _T_7 = io_cond[3:1] == 3'h2; // @[Execute.scala 57:27]
  assign _T_10 = io_cond[3:1] == 3'h3; // @[Execute.scala 58:27]
  assign _T_13 = io_cond[3:1] == 3'h4; // @[Execute.scala 59:27]
  assign _T_15 = ~PSTATE_Z; // @[Execute.scala 59:82]
  assign _T_16 = PSTATE_C & _T_15; // @[Execute.scala 59:70]
  assign _T_18 = io_cond[3:1] == 3'h5; // @[Execute.scala 60:27]
  assign _T_19 = PSTATE_N == PSTATE_V; // @[Execute.scala 60:62]
  assign _T_21 = io_cond[3:1] == 3'h6; // @[Execute.scala 61:27]
  assign _T_24 = _T_19 & _T_15; // @[Execute.scala 61:75]
  assign _T_26 = io_cond[3:1] == 3'h7; // @[Execute.scala 62:27]
  assign _GEN_1 = _T_21 ? _T_24 : _T_26; // @[Execute.scala 61:41]
  assign _GEN_2 = _T_18 ? _T_19 : _GEN_1; // @[Execute.scala 60:41]
  assign _GEN_3 = _T_13 ? _T_16 : _GEN_2; // @[Execute.scala 59:41]
  assign _GEN_4 = _T_10 ? PSTATE_V : _GEN_3; // @[Execute.scala 58:41]
  assign _GEN_5 = _T_7 ? PSTATE_N : _GEN_4; // @[Execute.scala 57:41]
  assign _GEN_6 = _T_4 ? PSTATE_C : _GEN_5; // @[Execute.scala 56:41]
  assign result = _T_1 ? PSTATE_Z : _GEN_6; // @[Execute.scala 55:41]
  assign _T_29 = io_cond != 4'hf; // @[Execute.scala 64:38]
  assign _T_30 = io_cond[0] & _T_29; // @[Execute.scala 64:27]
  assign _T_31 = ~result; // @[Execute.scala 65:15]
  assign io_res = _T_30 ? _T_31 : result; // @[Execute.scala 65:12 Execute.scala 67:12]
endmodule
