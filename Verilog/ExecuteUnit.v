module ExecuteUnit( // @[:@2522.2]
  input  [4:0]  io_dinst_rd, // @[:@2525.4]
  input  [25:0] io_dinst_imm, // @[:@2525.4]
  input  [5:0]  io_dinst_shift_val, // @[:@2525.4]
  input  [1:0]  io_dinst_shift_type, // @[:@2525.4]
  input  [2:0]  io_dinst_itype, // @[:@2525.4]
  input  [2:0]  io_dinst_op, // @[:@2525.4]
  input         io_dinst_rd_en, // @[:@2525.4]
  input         io_dinst_rs2_en, // @[:@2525.4]
  input         io_dinst_shift_en, // @[:@2525.4]
  input         io_dinst_nzcv_en, // @[:@2525.4]
  input         io_dinst_tag, // @[:@2525.4]
  input  [63:0] io_rVal1, // @[:@2525.4]
  input  [63:0] io_rVal2, // @[:@2525.4]
  output        io_einst_valid, // @[:@2525.4]
  output [63:0] io_einst_bits_res, // @[:@2525.4]
  output [4:0]  io_einst_bits_rd, // @[:@2525.4]
  output        io_einst_bits_rd_en, // @[:@2525.4]
  output        io_einst_bits_tag, // @[:@2525.4]
  output [3:0]  io_einst_bits_nzcv, // @[:@2525.4]
  output        io_einst_bits_nzcv_en // @[:@2525.4]
);
  wire [63:0] shiftALU_io_word; // @[execute.scala 115:24:@2528.4]
  wire [5:0] shiftALU_io_amount; // @[execute.scala 115:24:@2528.4]
  wire [1:0] shiftALU_io_opcode; // @[execute.scala 115:24:@2528.4]
  wire [63:0] shiftALU_io_res; // @[execute.scala 115:24:@2528.4]
  wire [63:0] basicALU_io_a; // @[execute.scala 124:24:@2535.4]
  wire [63:0] basicALU_io_b; // @[execute.scala 124:24:@2535.4]
  wire [2:0] basicALU_io_opcode; // @[execute.scala 124:24:@2535.4]
  wire [63:0] basicALU_io_res; // @[execute.scala 124:24:@2535.4]
  wire [3:0] basicALU_io_nzcv; // @[execute.scala 124:24:@2535.4]
  wire [63:0] interVal2; // @[execute.scala 112:22:@2527.4]
  wire  _T_23; // @[Mux.scala 46:19:@2554.4]
  wire  _T_25; // @[Mux.scala 46:19:@2556.4]
  ShiftALU shiftALU ( // @[execute.scala 115:24:@2528.4]
    .io_word(shiftALU_io_word),
    .io_amount(shiftALU_io_amount),
    .io_opcode(shiftALU_io_opcode),
    .io_res(shiftALU_io_res)
  );
  BasicALU basicALU ( // @[execute.scala 124:24:@2535.4]
    .io_a(basicALU_io_a),
    .io_b(basicALU_io_b),
    .io_opcode(basicALU_io_opcode),
    .io_res(basicALU_io_res),
    .io_nzcv(basicALU_io_nzcv)
  );
  assign interVal2 = io_dinst_rs2_en ? io_rVal2 : {{38'd0}, io_dinst_imm}; // @[execute.scala 112:22:@2527.4]
  assign _T_23 = 3'h4 == io_dinst_itype; // @[Mux.scala 46:19:@2554.4]
  assign _T_25 = 3'h1 == io_dinst_itype; // @[Mux.scala 46:19:@2556.4]
  assign io_einst_valid = _T_25 ? 1'h1 : _T_23; // @[execute.scala 141:18:@2558.4]
  assign io_einst_bits_res = basicALU_io_res; // @[execute.scala 139:17:@2553.4]
  assign io_einst_bits_rd = io_dinst_rd; // @[execute.scala 139:17:@2552.4]
  assign io_einst_bits_rd_en = io_dinst_rd_en; // @[execute.scala 139:17:@2551.4]
  assign io_einst_bits_tag = io_dinst_tag; // @[execute.scala 139:17:@2550.4]
  assign io_einst_bits_nzcv = basicALU_io_nzcv; // @[execute.scala 139:17:@2549.4]
  assign io_einst_bits_nzcv_en = io_dinst_nzcv_en; // @[execute.scala 139:17:@2548.4]
  assign shiftALU_io_word = io_dinst_rs2_en ? io_rVal2 : {{38'd0}, io_dinst_imm}; // @[execute.scala 116:20:@2531.4]
  assign shiftALU_io_amount = io_dinst_shift_val; // @[execute.scala 117:22:@2532.4]
  assign shiftALU_io_opcode = io_dinst_shift_type; // @[execute.scala 118:22:@2533.4]
  assign basicALU_io_a = io_rVal1; // @[execute.scala 125:17:@2538.4]
  assign basicALU_io_b = io_dinst_shift_en ? shiftALU_io_res : interVal2; // @[execute.scala 126:17:@2539.4]
  assign basicALU_io_opcode = io_dinst_op; // @[execute.scala 127:22:@2540.4]
endmodule
