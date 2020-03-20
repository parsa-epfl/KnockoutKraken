module ExecuteUnit( // @[:@2740.2]
  input         io_dinst_rd_valid, // @[:@2743.4]
  input  [4:0]  io_dinst_rd_bits, // @[:@2743.4]
  input         io_dinst_rs2_valid, // @[:@2743.4]
  input  [25:0] io_dinst_imm_bits, // @[:@2743.4]
  input         io_dinst_shift_val_valid, // @[:@2743.4]
  input  [5:0]  io_dinst_shift_val_bits, // @[:@2743.4]
  input  [1:0]  io_dinst_shift_type, // @[:@2743.4]
  input  [2:0]  io_dinst_itype, // @[:@2743.4]
  input  [2:0]  io_dinst_op, // @[:@2743.4]
  input         io_dinst_nzcv_en, // @[:@2743.4]
  input  [63:0] io_rVal1, // @[:@2743.4]
  input  [63:0] io_rVal2, // @[:@2743.4]
  output        io_einst_valid, // @[:@2743.4]
  output        io_einst_bits_rd_valid, // @[:@2743.4]
  output [4:0]  io_einst_bits_rd_bits, // @[:@2743.4]
  output        io_einst_bits_nzcv_valid, // @[:@2743.4]
  output [3:0]  io_einst_bits_nzcv_bits, // @[:@2743.4]
  output [63:0] io_einst_bits_res // @[:@2743.4]
);
  wire [63:0] shiftALU_io_word; // @[execute.scala 112:24:@2746.4]
  wire [5:0] shiftALU_io_amount; // @[execute.scala 112:24:@2746.4]
  wire [1:0] shiftALU_io_opcode; // @[execute.scala 112:24:@2746.4]
  wire [63:0] shiftALU_io_res; // @[execute.scala 112:24:@2746.4]
  wire [63:0] basicALU_io_a; // @[execute.scala 121:24:@2753.4]
  wire [63:0] basicALU_io_b; // @[execute.scala 121:24:@2753.4]
  wire [2:0] basicALU_io_opcode; // @[execute.scala 121:24:@2753.4]
  wire [63:0] basicALU_io_res; // @[execute.scala 121:24:@2753.4]
  wire [3:0] basicALU_io_nzcv; // @[execute.scala 121:24:@2753.4]
  wire [63:0] interVal2; // @[execute.scala 109:22:@2745.4]
  wire  _T_143; // @[Mux.scala 46:19:@2770.4]
  wire  _T_145; // @[Mux.scala 46:19:@2772.4]
  ShiftALU shiftALU ( // @[execute.scala 112:24:@2746.4]
    .io_word(shiftALU_io_word),
    .io_amount(shiftALU_io_amount),
    .io_opcode(shiftALU_io_opcode),
    .io_res(shiftALU_io_res)
  );
  BasicALU basicALU ( // @[execute.scala 121:24:@2753.4]
    .io_a(basicALU_io_a),
    .io_b(basicALU_io_b),
    .io_opcode(basicALU_io_opcode),
    .io_res(basicALU_io_res),
    .io_nzcv(basicALU_io_nzcv)
  );
  assign interVal2 = io_dinst_rs2_valid ? io_rVal2 : {{38'd0}, io_dinst_imm_bits}; // @[execute.scala 109:22:@2745.4]
  assign _T_143 = 3'h4 == io_dinst_itype; // @[Mux.scala 46:19:@2770.4]
  assign _T_145 = 3'h1 == io_dinst_itype; // @[Mux.scala 46:19:@2772.4]
  assign io_einst_valid = _T_145 ? 1'h1 : _T_143; // @[execute.scala 136:18:@2774.4]
  assign io_einst_bits_rd_valid = io_dinst_rd_valid; // @[execute.scala 134:17:@2769.4]
  assign io_einst_bits_rd_bits = io_dinst_rd_bits; // @[execute.scala 134:17:@2768.4]
  assign io_einst_bits_nzcv_valid = io_dinst_nzcv_en; // @[execute.scala 134:17:@2767.4]
  assign io_einst_bits_nzcv_bits = basicALU_io_nzcv; // @[execute.scala 134:17:@2766.4]
  assign io_einst_bits_res = basicALU_io_res; // @[execute.scala 134:17:@2765.4]
  assign shiftALU_io_word = io_dinst_rs2_valid ? io_rVal2 : {{38'd0}, io_dinst_imm_bits}; // @[execute.scala 113:20:@2749.4]
  assign shiftALU_io_amount = io_dinst_shift_val_bits; // @[execute.scala 114:22:@2750.4]
  assign shiftALU_io_opcode = io_dinst_shift_type; // @[execute.scala 115:22:@2751.4]
  assign basicALU_io_a = io_rVal1; // @[execute.scala 122:17:@2756.4]
  assign basicALU_io_b = io_dinst_shift_val_valid ? shiftALU_io_res : interVal2; // @[execute.scala 123:17:@2757.4]
  assign basicALU_io_opcode = io_dinst_op; // @[execute.scala 124:22:@2758.4]
endmodule
