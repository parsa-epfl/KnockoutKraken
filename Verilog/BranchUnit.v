module BranchUnit( // @[:@2672.2]
  input  [25:0] io_dinst_imm, // @[:@2675.4]
  input  [3:0]  io_dinst_cond, // @[:@2675.4]
  input  [2:0]  io_dinst_itype, // @[:@2675.4]
  input  [2:0]  io_dinst_op, // @[:@2675.4]
  input         io_dinst_tag, // @[:@2675.4]
  input  [3:0]  io_nzcv, // @[:@2675.4]
  output        io_binst_valid, // @[:@2675.4]
  output [63:0] io_binst_bits_offset, // @[:@2675.4]
  output        io_binst_bits_tag // @[:@2675.4]
);
  wire [3:0] cond_io_cond; // @[branch.scala 61:20:@2684.4]
  wire [3:0] cond_io_nzcv; // @[branch.scala 61:20:@2684.4]
  wire  cond_io_res; // @[branch.scala 61:20:@2684.4]
  wire [25:0] _T_19; // @[branch.scala 57:32:@2680.4]
  wire [63:0] signExtended; // @[branch.scala 56:26:@2679.4 branch.scala 57:16:@2681.4]
  wire  _T_21; // @[branch.scala 64:24:@2689.4]
  wire  _T_22; // @[branch.scala 64:54:@2690.4]
  wire  _T_23; // @[branch.scala 64:36:@2691.4]
  wire  _T_24; // @[branch.scala 65:22:@2693.6]
  wire  _T_25; // @[branch.scala 67:28:@2698.8]
  wire  _GEN_1; // @[branch.scala 65:36:@2694.6]
  CondUnit cond ( // @[branch.scala 61:20:@2684.4]
    .io_cond(cond_io_cond),
    .io_nzcv(cond_io_nzcv),
    .io_res(cond_io_res)
  );
  assign _T_19 = $signed(io_dinst_imm); // @[branch.scala 57:32:@2680.4]
  assign signExtended = {{38{_T_19[25]}},_T_19}; // @[branch.scala 56:26:@2679.4 branch.scala 57:16:@2681.4]
  assign _T_21 = io_dinst_itype == 3'h2; // @[branch.scala 64:24:@2689.4]
  assign _T_22 = io_dinst_itype == 3'h3; // @[branch.scala 64:54:@2690.4]
  assign _T_23 = _T_21 | _T_22; // @[branch.scala 64:36:@2691.4]
  assign _T_24 = io_dinst_op == 3'h1; // @[branch.scala 65:22:@2693.6]
  assign _T_25 = io_dinst_op == 3'h0; // @[branch.scala 67:28:@2698.8]
  assign _GEN_1 = _T_24 ? cond_io_res : _T_25; // @[branch.scala 65:36:@2694.6]
  assign io_binst_valid = _T_23 ? _GEN_1 : 1'h0; // @[branch.scala 52:18:@2677.4 branch.scala 66:22:@2695.8 branch.scala 68:22:@2700.10]
  assign io_binst_bits_offset = $unsigned(signExtended); // @[branch.scala 58:24:@2683.4]
  assign io_binst_bits_tag = io_dinst_tag; // @[branch.scala 53:21:@2678.4]
  assign cond_io_cond = io_dinst_cond; // @[branch.scala 62:16:@2687.4]
  assign cond_io_nzcv = io_nzcv; // @[branch.scala 63:16:@2688.4]
endmodule