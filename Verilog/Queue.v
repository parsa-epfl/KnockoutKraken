module Queue( // @[:@1020.2]
  input         clock, // @[:@1021.4]
  input         reset, // @[:@1022.4]
  output        io_enq_ready, // @[:@1023.4]
  input         io_enq_valid, // @[:@1023.4]
  input  [4:0]  io_enq_bits_rd, // @[:@1023.4]
  input  [4:0]  io_enq_bits_rs1, // @[:@1023.4]
  input  [4:0]  io_enq_bits_rs2, // @[:@1023.4]
  input  [25:0] io_enq_bits_imm, // @[:@1023.4]
  input  [5:0]  io_enq_bits_shift_val, // @[:@1023.4]
  input  [1:0]  io_enq_bits_shift_type, // @[:@1023.4]
  input  [3:0]  io_enq_bits_cond, // @[:@1023.4]
  input  [2:0]  io_enq_bits_itype, // @[:@1023.4]
  input  [2:0]  io_enq_bits_op, // @[:@1023.4]
  input         io_enq_bits_rd_en, // @[:@1023.4]
  input         io_enq_bits_rs2_en, // @[:@1023.4]
  input         io_enq_bits_shift_en, // @[:@1023.4]
  input         io_enq_bits_nzcv_en, // @[:@1023.4]
  input         io_enq_bits_tag, // @[:@1023.4]
  input         io_deq_ready, // @[:@1023.4]
  output        io_deq_valid, // @[:@1023.4]
  output [4:0]  io_deq_bits_rd, // @[:@1023.4]
  output [4:0]  io_deq_bits_rs1, // @[:@1023.4]
  output [4:0]  io_deq_bits_rs2, // @[:@1023.4]
  output [25:0] io_deq_bits_imm, // @[:@1023.4]
  output [5:0]  io_deq_bits_shift_val, // @[:@1023.4]
  output [1:0]  io_deq_bits_shift_type, // @[:@1023.4]
  output [3:0]  io_deq_bits_cond, // @[:@1023.4]
  output [2:0]  io_deq_bits_itype, // @[:@1023.4]
  output [2:0]  io_deq_bits_op, // @[:@1023.4]
  output        io_deq_bits_rd_en, // @[:@1023.4]
  output        io_deq_bits_rs2_en, // @[:@1023.4]
  output        io_deq_bits_shift_en, // @[:@1023.4]
  output        io_deq_bits_nzcv_en, // @[:@1023.4]
  output        io_deq_bits_tag // @[:@1023.4]
);
  reg [4:0] _T_35_rd [0:1]; // @[Decoupled.scala 215:24:@1025.4]
  reg [31:0] _RAND_0;
  wire [4:0] _T_35_rd__T_68_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rd__T_68_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire [4:0] _T_35_rd__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rd__T_54_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rd__T_54_mask; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rd__T_54_en; // @[Decoupled.scala 215:24:@1025.4]
  reg [4:0] _T_35_rs1 [0:1]; // @[Decoupled.scala 215:24:@1025.4]
  reg [31:0] _RAND_1;
  wire [4:0] _T_35_rs1__T_68_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rs1__T_68_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire [4:0] _T_35_rs1__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rs1__T_54_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rs1__T_54_mask; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rs1__T_54_en; // @[Decoupled.scala 215:24:@1025.4]
  reg [4:0] _T_35_rs2 [0:1]; // @[Decoupled.scala 215:24:@1025.4]
  reg [31:0] _RAND_2;
  wire [4:0] _T_35_rs2__T_68_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rs2__T_68_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire [4:0] _T_35_rs2__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rs2__T_54_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rs2__T_54_mask; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rs2__T_54_en; // @[Decoupled.scala 215:24:@1025.4]
  reg [25:0] _T_35_imm [0:1]; // @[Decoupled.scala 215:24:@1025.4]
  reg [31:0] _RAND_3;
  wire [25:0] _T_35_imm__T_68_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_imm__T_68_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire [25:0] _T_35_imm__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_imm__T_54_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_imm__T_54_mask; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_imm__T_54_en; // @[Decoupled.scala 215:24:@1025.4]
  reg [5:0] _T_35_shift_val [0:1]; // @[Decoupled.scala 215:24:@1025.4]
  reg [31:0] _RAND_4;
  wire [5:0] _T_35_shift_val__T_68_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_shift_val__T_68_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire [5:0] _T_35_shift_val__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_shift_val__T_54_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_shift_val__T_54_mask; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_shift_val__T_54_en; // @[Decoupled.scala 215:24:@1025.4]
  reg [1:0] _T_35_shift_type [0:1]; // @[Decoupled.scala 215:24:@1025.4]
  reg [31:0] _RAND_5;
  wire [1:0] _T_35_shift_type__T_68_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_shift_type__T_68_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire [1:0] _T_35_shift_type__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_shift_type__T_54_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_shift_type__T_54_mask; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_shift_type__T_54_en; // @[Decoupled.scala 215:24:@1025.4]
  reg [3:0] _T_35_cond [0:1]; // @[Decoupled.scala 215:24:@1025.4]
  reg [31:0] _RAND_6;
  wire [3:0] _T_35_cond__T_68_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_cond__T_68_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire [3:0] _T_35_cond__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_cond__T_54_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_cond__T_54_mask; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_cond__T_54_en; // @[Decoupled.scala 215:24:@1025.4]
  reg [2:0] _T_35_itype [0:1]; // @[Decoupled.scala 215:24:@1025.4]
  reg [31:0] _RAND_7;
  wire [2:0] _T_35_itype__T_68_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_itype__T_68_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire [2:0] _T_35_itype__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_itype__T_54_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_itype__T_54_mask; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_itype__T_54_en; // @[Decoupled.scala 215:24:@1025.4]
  reg [2:0] _T_35_op [0:1]; // @[Decoupled.scala 215:24:@1025.4]
  reg [31:0] _RAND_8;
  wire [2:0] _T_35_op__T_68_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_op__T_68_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire [2:0] _T_35_op__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_op__T_54_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_op__T_54_mask; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_op__T_54_en; // @[Decoupled.scala 215:24:@1025.4]
  reg  _T_35_rd_en [0:1]; // @[Decoupled.scala 215:24:@1025.4]
  reg [31:0] _RAND_9;
  wire  _T_35_rd_en__T_68_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rd_en__T_68_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rd_en__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rd_en__T_54_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rd_en__T_54_mask; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rd_en__T_54_en; // @[Decoupled.scala 215:24:@1025.4]
  reg  _T_35_rs2_en [0:1]; // @[Decoupled.scala 215:24:@1025.4]
  reg [31:0] _RAND_10;
  wire  _T_35_rs2_en__T_68_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rs2_en__T_68_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rs2_en__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rs2_en__T_54_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rs2_en__T_54_mask; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_rs2_en__T_54_en; // @[Decoupled.scala 215:24:@1025.4]
  reg  _T_35_shift_en [0:1]; // @[Decoupled.scala 215:24:@1025.4]
  reg [31:0] _RAND_11;
  wire  _T_35_shift_en__T_68_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_shift_en__T_68_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_shift_en__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_shift_en__T_54_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_shift_en__T_54_mask; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_shift_en__T_54_en; // @[Decoupled.scala 215:24:@1025.4]
  reg  _T_35_nzcv_en [0:1]; // @[Decoupled.scala 215:24:@1025.4]
  reg [31:0] _RAND_12;
  wire  _T_35_nzcv_en__T_68_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_nzcv_en__T_68_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_nzcv_en__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_nzcv_en__T_54_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_nzcv_en__T_54_mask; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_nzcv_en__T_54_en; // @[Decoupled.scala 215:24:@1025.4]
  reg  _T_35_tag [0:1]; // @[Decoupled.scala 215:24:@1025.4]
  reg [31:0] _RAND_13;
  wire  _T_35_tag__T_68_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_tag__T_68_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_tag__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_tag__T_54_addr; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_tag__T_54_mask; // @[Decoupled.scala 215:24:@1025.4]
  wire  _T_35_tag__T_54_en; // @[Decoupled.scala 215:24:@1025.4]
  reg  value; // @[Counter.scala 26:33:@1026.4]
  reg [31:0] _RAND_14;
  reg  value_1; // @[Counter.scala 26:33:@1027.4]
  reg [31:0] _RAND_15;
  reg  _T_42; // @[Decoupled.scala 218:35:@1028.4]
  reg [31:0] _RAND_16;
  wire  _T_43; // @[Decoupled.scala 220:41:@1029.4]
  wire  _T_45; // @[Decoupled.scala 221:36:@1030.4]
  wire  _T_46; // @[Decoupled.scala 221:33:@1031.4]
  wire  _T_47; // @[Decoupled.scala 222:32:@1032.4]
  wire  _T_48; // @[Decoupled.scala 37:37:@1033.4]
  wire  _T_51; // @[Decoupled.scala 37:37:@1036.4]
  wire [1:0] _T_57; // @[Counter.scala 35:22:@1062.6]
  wire  _T_58; // @[Counter.scala 35:22:@1063.6]
  wire  _GEN_28; // @[Decoupled.scala 246:27:@1126.6]
  wire  _GEN_50; // @[Decoupled.scala 243:18:@1104.4]
  wire  _GEN_24; // @[Decoupled.scala 226:17:@1039.4]
  wire [1:0] _T_61; // @[Counter.scala 35:22:@1068.6]
  wire  _T_62; // @[Counter.scala 35:22:@1069.6]
  wire  _GEN_49; // @[Decoupled.scala 243:18:@1104.4]
  wire  _GEN_25; // @[Decoupled.scala 230:17:@1066.4]
  wire  _T_63; // @[Decoupled.scala 233:16:@1072.4]
  wire  _GEN_26; // @[Decoupled.scala 233:28:@1073.4]
  wire  _T_65; // @[Decoupled.scala 237:19:@1076.4]
  wire  _T_67; // @[Decoupled.scala 238:19:@1078.4]
  assign _T_35_rd__T_68_addr = value_1;
  assign _T_35_rd__T_68_data = _T_35_rd[_T_35_rd__T_68_addr]; // @[Decoupled.scala 215:24:@1025.4]
  assign _T_35_rd__T_54_data = io_enq_bits_rd;
  assign _T_35_rd__T_54_addr = value;
  assign _T_35_rd__T_54_mask = 1'h1;
  assign _T_35_rd__T_54_en = _T_46 ? _GEN_28 : _T_48;
  assign _T_35_rs1__T_68_addr = value_1;
  assign _T_35_rs1__T_68_data = _T_35_rs1[_T_35_rs1__T_68_addr]; // @[Decoupled.scala 215:24:@1025.4]
  assign _T_35_rs1__T_54_data = io_enq_bits_rs1;
  assign _T_35_rs1__T_54_addr = value;
  assign _T_35_rs1__T_54_mask = 1'h1;
  assign _T_35_rs1__T_54_en = _T_46 ? _GEN_28 : _T_48;
  assign _T_35_rs2__T_68_addr = value_1;
  assign _T_35_rs2__T_68_data = _T_35_rs2[_T_35_rs2__T_68_addr]; // @[Decoupled.scala 215:24:@1025.4]
  assign _T_35_rs2__T_54_data = io_enq_bits_rs2;
  assign _T_35_rs2__T_54_addr = value;
  assign _T_35_rs2__T_54_mask = 1'h1;
  assign _T_35_rs2__T_54_en = _T_46 ? _GEN_28 : _T_48;
  assign _T_35_imm__T_68_addr = value_1;
  assign _T_35_imm__T_68_data = _T_35_imm[_T_35_imm__T_68_addr]; // @[Decoupled.scala 215:24:@1025.4]
  assign _T_35_imm__T_54_data = io_enq_bits_imm;
  assign _T_35_imm__T_54_addr = value;
  assign _T_35_imm__T_54_mask = 1'h1;
  assign _T_35_imm__T_54_en = _T_46 ? _GEN_28 : _T_48;
  assign _T_35_shift_val__T_68_addr = value_1;
  assign _T_35_shift_val__T_68_data = _T_35_shift_val[_T_35_shift_val__T_68_addr]; // @[Decoupled.scala 215:24:@1025.4]
  assign _T_35_shift_val__T_54_data = io_enq_bits_shift_val;
  assign _T_35_shift_val__T_54_addr = value;
  assign _T_35_shift_val__T_54_mask = 1'h1;
  assign _T_35_shift_val__T_54_en = _T_46 ? _GEN_28 : _T_48;
  assign _T_35_shift_type__T_68_addr = value_1;
  assign _T_35_shift_type__T_68_data = _T_35_shift_type[_T_35_shift_type__T_68_addr]; // @[Decoupled.scala 215:24:@1025.4]
  assign _T_35_shift_type__T_54_data = io_enq_bits_shift_type;
  assign _T_35_shift_type__T_54_addr = value;
  assign _T_35_shift_type__T_54_mask = 1'h1;
  assign _T_35_shift_type__T_54_en = _T_46 ? _GEN_28 : _T_48;
  assign _T_35_cond__T_68_addr = value_1;
  assign _T_35_cond__T_68_data = _T_35_cond[_T_35_cond__T_68_addr]; // @[Decoupled.scala 215:24:@1025.4]
  assign _T_35_cond__T_54_data = io_enq_bits_cond;
  assign _T_35_cond__T_54_addr = value;
  assign _T_35_cond__T_54_mask = 1'h1;
  assign _T_35_cond__T_54_en = _T_46 ? _GEN_28 : _T_48;
  assign _T_35_itype__T_68_addr = value_1;
  assign _T_35_itype__T_68_data = _T_35_itype[_T_35_itype__T_68_addr]; // @[Decoupled.scala 215:24:@1025.4]
  assign _T_35_itype__T_54_data = io_enq_bits_itype;
  assign _T_35_itype__T_54_addr = value;
  assign _T_35_itype__T_54_mask = 1'h1;
  assign _T_35_itype__T_54_en = _T_46 ? _GEN_28 : _T_48;
  assign _T_35_op__T_68_addr = value_1;
  assign _T_35_op__T_68_data = _T_35_op[_T_35_op__T_68_addr]; // @[Decoupled.scala 215:24:@1025.4]
  assign _T_35_op__T_54_data = io_enq_bits_op;
  assign _T_35_op__T_54_addr = value;
  assign _T_35_op__T_54_mask = 1'h1;
  assign _T_35_op__T_54_en = _T_46 ? _GEN_28 : _T_48;
  assign _T_35_rd_en__T_68_addr = value_1;
  assign _T_35_rd_en__T_68_data = _T_35_rd_en[_T_35_rd_en__T_68_addr]; // @[Decoupled.scala 215:24:@1025.4]
  assign _T_35_rd_en__T_54_data = io_enq_bits_rd_en;
  assign _T_35_rd_en__T_54_addr = value;
  assign _T_35_rd_en__T_54_mask = 1'h1;
  assign _T_35_rd_en__T_54_en = _T_46 ? _GEN_28 : _T_48;
  assign _T_35_rs2_en__T_68_addr = value_1;
  assign _T_35_rs2_en__T_68_data = _T_35_rs2_en[_T_35_rs2_en__T_68_addr]; // @[Decoupled.scala 215:24:@1025.4]
  assign _T_35_rs2_en__T_54_data = io_enq_bits_rs2_en;
  assign _T_35_rs2_en__T_54_addr = value;
  assign _T_35_rs2_en__T_54_mask = 1'h1;
  assign _T_35_rs2_en__T_54_en = _T_46 ? _GEN_28 : _T_48;
  assign _T_35_shift_en__T_68_addr = value_1;
  assign _T_35_shift_en__T_68_data = _T_35_shift_en[_T_35_shift_en__T_68_addr]; // @[Decoupled.scala 215:24:@1025.4]
  assign _T_35_shift_en__T_54_data = io_enq_bits_shift_en;
  assign _T_35_shift_en__T_54_addr = value;
  assign _T_35_shift_en__T_54_mask = 1'h1;
  assign _T_35_shift_en__T_54_en = _T_46 ? _GEN_28 : _T_48;
  assign _T_35_nzcv_en__T_68_addr = value_1;
  assign _T_35_nzcv_en__T_68_data = _T_35_nzcv_en[_T_35_nzcv_en__T_68_addr]; // @[Decoupled.scala 215:24:@1025.4]
  assign _T_35_nzcv_en__T_54_data = io_enq_bits_nzcv_en;
  assign _T_35_nzcv_en__T_54_addr = value;
  assign _T_35_nzcv_en__T_54_mask = 1'h1;
  assign _T_35_nzcv_en__T_54_en = _T_46 ? _GEN_28 : _T_48;
  assign _T_35_tag__T_68_addr = value_1;
  assign _T_35_tag__T_68_data = _T_35_tag[_T_35_tag__T_68_addr]; // @[Decoupled.scala 215:24:@1025.4]
  assign _T_35_tag__T_54_data = io_enq_bits_tag;
  assign _T_35_tag__T_54_addr = value;
  assign _T_35_tag__T_54_mask = 1'h1;
  assign _T_35_tag__T_54_en = _T_46 ? _GEN_28 : _T_48;
  assign _T_43 = value == value_1; // @[Decoupled.scala 220:41:@1029.4]
  assign _T_45 = _T_42 == 1'h0; // @[Decoupled.scala 221:36:@1030.4]
  assign _T_46 = _T_43 & _T_45; // @[Decoupled.scala 221:33:@1031.4]
  assign _T_47 = _T_43 & _T_42; // @[Decoupled.scala 222:32:@1032.4]
  assign _T_48 = io_enq_ready & io_enq_valid; // @[Decoupled.scala 37:37:@1033.4]
  assign _T_51 = io_deq_ready & io_deq_valid; // @[Decoupled.scala 37:37:@1036.4]
  assign _T_57 = value + 1'h1; // @[Counter.scala 35:22:@1062.6]
  assign _T_58 = value + 1'h1; // @[Counter.scala 35:22:@1063.6]
  assign _GEN_28 = io_deq_ready ? 1'h0 : _T_48; // @[Decoupled.scala 246:27:@1126.6]
  assign _GEN_50 = _T_46 ? _GEN_28 : _T_48; // @[Decoupled.scala 243:18:@1104.4]
  assign _GEN_24 = _GEN_50 ? _T_58 : value; // @[Decoupled.scala 226:17:@1039.4]
  assign _T_61 = value_1 + 1'h1; // @[Counter.scala 35:22:@1068.6]
  assign _T_62 = value_1 + 1'h1; // @[Counter.scala 35:22:@1069.6]
  assign _GEN_49 = _T_46 ? 1'h0 : _T_51; // @[Decoupled.scala 243:18:@1104.4]
  assign _GEN_25 = _GEN_49 ? _T_62 : value_1; // @[Decoupled.scala 230:17:@1066.4]
  assign _T_63 = _GEN_50 != _GEN_49; // @[Decoupled.scala 233:16:@1072.4]
  assign _GEN_26 = _T_63 ? _GEN_50 : _T_42; // @[Decoupled.scala 233:28:@1073.4]
  assign _T_65 = _T_46 == 1'h0; // @[Decoupled.scala 237:19:@1076.4]
  assign _T_67 = _T_47 == 1'h0; // @[Decoupled.scala 238:19:@1078.4]
  assign io_enq_ready = io_deq_ready ? 1'h1 : _T_67; // @[Decoupled.scala 238:16:@1079.4 Decoupled.scala 251:40:@1131.6]
  assign io_deq_valid = io_enq_valid ? 1'h1 : _T_65; // @[Decoupled.scala 237:16:@1077.4 Decoupled.scala 242:40:@1102.6]
  assign io_deq_bits_rd = _T_46 ? io_enq_bits_rd : _T_35_rd__T_68_data; // @[Decoupled.scala 239:15:@1100.4 Decoupled.scala 244:19:@1124.6]
  assign io_deq_bits_rs1 = _T_46 ? io_enq_bits_rs1 : _T_35_rs1__T_68_data; // @[Decoupled.scala 239:15:@1099.4 Decoupled.scala 244:19:@1123.6]
  assign io_deq_bits_rs2 = _T_46 ? io_enq_bits_rs2 : _T_35_rs2__T_68_data; // @[Decoupled.scala 239:15:@1098.4 Decoupled.scala 244:19:@1122.6]
  assign io_deq_bits_imm = _T_46 ? io_enq_bits_imm : _T_35_imm__T_68_data; // @[Decoupled.scala 239:15:@1097.4 Decoupled.scala 244:19:@1121.6]
  assign io_deq_bits_shift_val = _T_46 ? io_enq_bits_shift_val : _T_35_shift_val__T_68_data; // @[Decoupled.scala 239:15:@1096.4 Decoupled.scala 244:19:@1120.6]
  assign io_deq_bits_shift_type = _T_46 ? io_enq_bits_shift_type : _T_35_shift_type__T_68_data; // @[Decoupled.scala 239:15:@1095.4 Decoupled.scala 244:19:@1119.6]
  assign io_deq_bits_cond = _T_46 ? io_enq_bits_cond : _T_35_cond__T_68_data; // @[Decoupled.scala 239:15:@1094.4 Decoupled.scala 244:19:@1118.6]
  assign io_deq_bits_itype = _T_46 ? io_enq_bits_itype : _T_35_itype__T_68_data; // @[Decoupled.scala 239:15:@1093.4 Decoupled.scala 244:19:@1117.6]
  assign io_deq_bits_op = _T_46 ? io_enq_bits_op : _T_35_op__T_68_data; // @[Decoupled.scala 239:15:@1092.4 Decoupled.scala 244:19:@1116.6]
  assign io_deq_bits_rd_en = _T_46 ? io_enq_bits_rd_en : _T_35_rd_en__T_68_data; // @[Decoupled.scala 239:15:@1091.4 Decoupled.scala 244:19:@1115.6]
  assign io_deq_bits_rs2_en = _T_46 ? io_enq_bits_rs2_en : _T_35_rs2_en__T_68_data; // @[Decoupled.scala 239:15:@1089.4 Decoupled.scala 244:19:@1113.6]
  assign io_deq_bits_shift_en = _T_46 ? io_enq_bits_shift_en : _T_35_shift_en__T_68_data; // @[Decoupled.scala 239:15:@1087.4 Decoupled.scala 244:19:@1111.6]
  assign io_deq_bits_nzcv_en = _T_46 ? io_enq_bits_nzcv_en : _T_35_nzcv_en__T_68_data; // @[Decoupled.scala 239:15:@1085.4 Decoupled.scala 244:19:@1109.6]
  assign io_deq_bits_tag = _T_46 ? io_enq_bits_tag : _T_35_tag__T_68_data; // @[Decoupled.scala 239:15:@1083.4 Decoupled.scala 244:19:@1107.6]
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
`ifdef RANDOMIZE
  integer initvar;
  initial begin
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      #0.002 begin end
    `endif
  _RAND_0 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_35_rd[initvar] = _RAND_0[4:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_1 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_35_rs1[initvar] = _RAND_1[4:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_2 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_35_rs2[initvar] = _RAND_2[4:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_3 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_35_imm[initvar] = _RAND_3[25:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_4 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_35_shift_val[initvar] = _RAND_4[5:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_5 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_35_shift_type[initvar] = _RAND_5[1:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_6 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_35_cond[initvar] = _RAND_6[3:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_7 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_35_itype[initvar] = _RAND_7[2:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_8 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_35_op[initvar] = _RAND_8[2:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_9 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_35_rd_en[initvar] = _RAND_9[0:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_10 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_35_rs2_en[initvar] = _RAND_10[0:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_11 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_35_shift_en[initvar] = _RAND_11[0:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_12 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_35_nzcv_en[initvar] = _RAND_12[0:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_13 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_35_tag[initvar] = _RAND_13[0:0];
  `endif // RANDOMIZE_MEM_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_14 = {1{`RANDOM}};
  value = _RAND_14[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_15 = {1{`RANDOM}};
  value_1 = _RAND_15[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_16 = {1{`RANDOM}};
  _T_42 = _RAND_16[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if(_T_35_rd__T_54_en & _T_35_rd__T_54_mask) begin
      _T_35_rd[_T_35_rd__T_54_addr] <= _T_35_rd__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
    end
    if(_T_35_rs1__T_54_en & _T_35_rs1__T_54_mask) begin
      _T_35_rs1[_T_35_rs1__T_54_addr] <= _T_35_rs1__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
    end
    if(_T_35_rs2__T_54_en & _T_35_rs2__T_54_mask) begin
      _T_35_rs2[_T_35_rs2__T_54_addr] <= _T_35_rs2__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
    end
    if(_T_35_imm__T_54_en & _T_35_imm__T_54_mask) begin
      _T_35_imm[_T_35_imm__T_54_addr] <= _T_35_imm__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
    end
    if(_T_35_shift_val__T_54_en & _T_35_shift_val__T_54_mask) begin
      _T_35_shift_val[_T_35_shift_val__T_54_addr] <= _T_35_shift_val__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
    end
    if(_T_35_shift_type__T_54_en & _T_35_shift_type__T_54_mask) begin
      _T_35_shift_type[_T_35_shift_type__T_54_addr] <= _T_35_shift_type__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
    end
    if(_T_35_cond__T_54_en & _T_35_cond__T_54_mask) begin
      _T_35_cond[_T_35_cond__T_54_addr] <= _T_35_cond__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
    end
    if(_T_35_itype__T_54_en & _T_35_itype__T_54_mask) begin
      _T_35_itype[_T_35_itype__T_54_addr] <= _T_35_itype__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
    end
    if(_T_35_op__T_54_en & _T_35_op__T_54_mask) begin
      _T_35_op[_T_35_op__T_54_addr] <= _T_35_op__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
    end
    if(_T_35_rd_en__T_54_en & _T_35_rd_en__T_54_mask) begin
      _T_35_rd_en[_T_35_rd_en__T_54_addr] <= _T_35_rd_en__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
    end
    if(_T_35_rs2_en__T_54_en & _T_35_rs2_en__T_54_mask) begin
      _T_35_rs2_en[_T_35_rs2_en__T_54_addr] <= _T_35_rs2_en__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
    end
    if(_T_35_shift_en__T_54_en & _T_35_shift_en__T_54_mask) begin
      _T_35_shift_en[_T_35_shift_en__T_54_addr] <= _T_35_shift_en__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
    end
    if(_T_35_nzcv_en__T_54_en & _T_35_nzcv_en__T_54_mask) begin
      _T_35_nzcv_en[_T_35_nzcv_en__T_54_addr] <= _T_35_nzcv_en__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
    end
    if(_T_35_tag__T_54_en & _T_35_tag__T_54_mask) begin
      _T_35_tag[_T_35_tag__T_54_addr] <= _T_35_tag__T_54_data; // @[Decoupled.scala 215:24:@1025.4]
    end
    if (reset) begin
      value <= 1'h0;
    end else begin
      if (_GEN_50) begin
        value <= _T_58;
      end
    end
    if (reset) begin
      value_1 <= 1'h0;
    end else begin
      if (_GEN_49) begin
        value_1 <= _T_62;
      end
    end
    if (reset) begin
      _T_42 <= 1'h0;
    end else begin
      if (_T_63) begin
        if (_T_46) begin
          if (io_deq_ready) begin
            _T_42 <= 1'h0;
          end else begin
            _T_42 <= _T_48;
          end
        end else begin
          _T_42 <= _T_48;
        end
      end
    end
  end
endmodule
