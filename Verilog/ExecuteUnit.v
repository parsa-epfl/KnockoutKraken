module ExecuteUnit( // @[:@2524.2]
  input         io_dinst_rd_valid, // @[:@2527.4]
  input  [4:0]  io_dinst_rd_bits, // @[:@2527.4]
  input         io_dinst_rs2_valid, // @[:@2527.4]
  input  [25:0] io_dinst_imm_bits, // @[:@2527.4]
  input         io_dinst_shift_val_valid, // @[:@2527.4]
  input  [5:0]  io_dinst_shift_val_bits, // @[:@2527.4]
  input  [1:0]  io_dinst_shift_type, // @[:@2527.4]
  input  [2:0]  io_dinst_itype, // @[:@2527.4]
  input  [2:0]  io_dinst_op, // @[:@2527.4]
  input         io_dinst_nzcv_en, // @[:@2527.4]
  input         io_dinst_tag, // @[:@2527.4]
  input  [63:0] io_rVal1, // @[:@2527.4]
  input  [63:0] io_rVal2, // @[:@2527.4]
  output        io_einst_valid, // @[:@2527.4]
  output        io_einst_bits_rd_valid, // @[:@2527.4]
  output [4:0]  io_einst_bits_rd_bits, // @[:@2527.4]
  output        io_einst_bits_nzcv_valid, // @[:@2527.4]
  output [3:0]  io_einst_bits_nzcv_bits, // @[:@2527.4]
  output        io_einst_bits_tag, // @[:@2527.4]
  output [63:0] io_einst_bits_res // @[:@2527.4]
);
  wire [63:0] shiftALU_io_word; // @[execute.scala 113:24:@2530.4]
  wire [5:0] shiftALU_io_amount; // @[execute.scala 113:24:@2530.4]
  wire [1:0] shiftALU_io_opcode; // @[execute.scala 113:24:@2530.4]
  wire [63:0] shiftALU_io_res; // @[execute.scala 113:24:@2530.4]
  wire [63:0] basicALU_io_a; // @[execute.scala 122:24:@2537.4]
  wire [63:0] basicALU_io_b; // @[execute.scala 122:24:@2537.4]
  wire [2:0] basicALU_io_opcode; // @[execute.scala 122:24:@2537.4]
  wire [63:0] basicALU_io_res; // @[execute.scala 122:24:@2537.4]
  wire [3:0] basicALU_io_nzcv; // @[execute.scala 122:24:@2537.4]
  wire [63:0] interVal2; // @[execute.scala 110:22:@2529.4]
  wire  _T_151; // @[Mux.scala 46:19:@2556.4]
  wire  _T_153; // @[Mux.scala 46:19:@2558.4]
  ShiftALU shiftALU ( // @[execute.scala 113:24:@2530.4]
    .io_word(shiftALU_io_word),
    .io_amount(shiftALU_io_amount),
    .io_opcode(shiftALU_io_opcode),
    .io_res(shiftALU_io_res)
  );
  BasicALU basicALU ( // @[execute.scala 122:24:@2537.4]
    .io_a(basicALU_io_a),
    .io_b(basicALU_io_b),
    .io_opcode(basicALU_io_opcode),
    .io_res(basicALU_io_res),
    .io_nzcv(basicALU_io_nzcv)
  );
  assign interVal2 = io_dinst_rs2_valid ? io_rVal2 : {{38'd0}, io_dinst_imm_bits}; // @[execute.scala 110:22:@2529.4]
  assign _T_151 = 3'h4 == io_dinst_itype; // @[Mux.scala 46:19:@2556.4]
  assign _T_153 = 3'h1 == io_dinst_itype; // @[Mux.scala 46:19:@2558.4]
  assign io_einst_valid = _T_153 ? 1'h1 : _T_151; // @[execute.scala 138:18:@2560.4]
  assign io_einst_bits_rd_valid = io_dinst_rd_valid; // @[execute.scala 136:17:@2555.4]
  assign io_einst_bits_rd_bits = io_dinst_rd_bits; // @[execute.scala 136:17:@2554.4]
  assign io_einst_bits_nzcv_valid = io_dinst_nzcv_en; // @[execute.scala 136:17:@2553.4]
  assign io_einst_bits_nzcv_bits = basicALU_io_nzcv; // @[execute.scala 136:17:@2552.4]
  assign io_einst_bits_tag = io_dinst_tag; // @[execute.scala 136:17:@2551.4]
  assign io_einst_bits_res = basicALU_io_res; // @[execute.scala 136:17:@2550.4]
  assign shiftALU_io_word = io_dinst_rs2_valid ? io_rVal2 : {{38'd0}, io_dinst_imm_bits}; // @[execute.scala 114:20:@2533.4]
  assign shiftALU_io_amount = io_dinst_shift_val_bits; // @[execute.scala 115:22:@2534.4]
  assign shiftALU_io_opcode = io_dinst_shift_type; // @[execute.scala 116:22:@2535.4]
  assign basicALU_io_a = io_rVal1; // @[execute.scala 123:17:@2540.4]
  assign basicALU_io_b = io_dinst_shift_val_valid ? shiftALU_io_res : interVal2; // @[execute.scala 124:17:@2541.4]
  assign basicALU_io_opcode = io_dinst_op; // @[execute.scala 125:22:@2542.4]
endmodule
