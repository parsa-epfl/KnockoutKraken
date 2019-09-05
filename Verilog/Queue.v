module Queue( // @[:@1022.2]
  input         clock, // @[:@1023.4]
  input         reset, // @[:@1024.4]
  output        io_enq_ready, // @[:@1025.4]
  input         io_enq_valid, // @[:@1025.4]
  input         io_enq_bits_rd_valid, // @[:@1025.4]
  input  [4:0]  io_enq_bits_rd_bits, // @[:@1025.4]
  input  [4:0]  io_enq_bits_rs1_bits, // @[:@1025.4]
  input         io_enq_bits_rs2_valid, // @[:@1025.4]
  input  [4:0]  io_enq_bits_rs2_bits, // @[:@1025.4]
  input  [25:0] io_enq_bits_imm_bits, // @[:@1025.4]
  input         io_enq_bits_shift_val_valid, // @[:@1025.4]
  input  [5:0]  io_enq_bits_shift_val_bits, // @[:@1025.4]
  input  [1:0]  io_enq_bits_shift_type, // @[:@1025.4]
  input  [3:0]  io_enq_bits_cond_bits, // @[:@1025.4]
  input  [2:0]  io_enq_bits_itype, // @[:@1025.4]
  input  [2:0]  io_enq_bits_op, // @[:@1025.4]
  input         io_enq_bits_nzcv_en, // @[:@1025.4]
  input         io_enq_bits_tag, // @[:@1025.4]
  input  [63:0] io_enq_bits_pc, // @[:@1025.4]
  input         io_deq_ready, // @[:@1025.4]
  output        io_deq_valid, // @[:@1025.4]
  output        io_deq_bits_rd_valid, // @[:@1025.4]
  output [4:0]  io_deq_bits_rd_bits, // @[:@1025.4]
  output [4:0]  io_deq_bits_rs1_bits, // @[:@1025.4]
  output        io_deq_bits_rs2_valid, // @[:@1025.4]
  output [4:0]  io_deq_bits_rs2_bits, // @[:@1025.4]
  output [25:0] io_deq_bits_imm_bits, // @[:@1025.4]
  output        io_deq_bits_shift_val_valid, // @[:@1025.4]
  output [5:0]  io_deq_bits_shift_val_bits, // @[:@1025.4]
  output [1:0]  io_deq_bits_shift_type, // @[:@1025.4]
  output [3:0]  io_deq_bits_cond_bits, // @[:@1025.4]
  output [2:0]  io_deq_bits_itype, // @[:@1025.4]
  output [2:0]  io_deq_bits_op, // @[:@1025.4]
  output        io_deq_bits_nzcv_en, // @[:@1025.4]
  output        io_deq_bits_tag, // @[:@1025.4]
  output [63:0] io_deq_bits_pc // @[:@1025.4]
);
  reg  _T_275_rd_valid [0:1]; // @[Decoupled.scala 215:24:@1027.4]
  reg [31:0] _RAND_0;
  wire  _T_275_rd_valid__T_328_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rd_valid__T_328_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rd_valid__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rd_valid__T_294_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rd_valid__T_294_mask; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rd_valid__T_294_en; // @[Decoupled.scala 215:24:@1027.4]
  reg [4:0] _T_275_rd_bits [0:1]; // @[Decoupled.scala 215:24:@1027.4]
  reg [31:0] _RAND_1;
  wire [4:0] _T_275_rd_bits__T_328_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rd_bits__T_328_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire [4:0] _T_275_rd_bits__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rd_bits__T_294_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rd_bits__T_294_mask; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rd_bits__T_294_en; // @[Decoupled.scala 215:24:@1027.4]
  reg [4:0] _T_275_rs1_bits [0:1]; // @[Decoupled.scala 215:24:@1027.4]
  reg [31:0] _RAND_2;
  wire [4:0] _T_275_rs1_bits__T_328_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rs1_bits__T_328_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire [4:0] _T_275_rs1_bits__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rs1_bits__T_294_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rs1_bits__T_294_mask; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rs1_bits__T_294_en; // @[Decoupled.scala 215:24:@1027.4]
  reg  _T_275_rs2_valid [0:1]; // @[Decoupled.scala 215:24:@1027.4]
  reg [31:0] _RAND_3;
  wire  _T_275_rs2_valid__T_328_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rs2_valid__T_328_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rs2_valid__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rs2_valid__T_294_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rs2_valid__T_294_mask; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rs2_valid__T_294_en; // @[Decoupled.scala 215:24:@1027.4]
  reg [4:0] _T_275_rs2_bits [0:1]; // @[Decoupled.scala 215:24:@1027.4]
  reg [31:0] _RAND_4;
  wire [4:0] _T_275_rs2_bits__T_328_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rs2_bits__T_328_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire [4:0] _T_275_rs2_bits__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rs2_bits__T_294_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rs2_bits__T_294_mask; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_rs2_bits__T_294_en; // @[Decoupled.scala 215:24:@1027.4]
  reg [25:0] _T_275_imm_bits [0:1]; // @[Decoupled.scala 215:24:@1027.4]
  reg [31:0] _RAND_5;
  wire [25:0] _T_275_imm_bits__T_328_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_imm_bits__T_328_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire [25:0] _T_275_imm_bits__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_imm_bits__T_294_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_imm_bits__T_294_mask; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_imm_bits__T_294_en; // @[Decoupled.scala 215:24:@1027.4]
  reg  _T_275_shift_val_valid [0:1]; // @[Decoupled.scala 215:24:@1027.4]
  reg [31:0] _RAND_6;
  wire  _T_275_shift_val_valid__T_328_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_shift_val_valid__T_328_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_shift_val_valid__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_shift_val_valid__T_294_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_shift_val_valid__T_294_mask; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_shift_val_valid__T_294_en; // @[Decoupled.scala 215:24:@1027.4]
  reg [5:0] _T_275_shift_val_bits [0:1]; // @[Decoupled.scala 215:24:@1027.4]
  reg [31:0] _RAND_7;
  wire [5:0] _T_275_shift_val_bits__T_328_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_shift_val_bits__T_328_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire [5:0] _T_275_shift_val_bits__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_shift_val_bits__T_294_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_shift_val_bits__T_294_mask; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_shift_val_bits__T_294_en; // @[Decoupled.scala 215:24:@1027.4]
  reg [1:0] _T_275_shift_type [0:1]; // @[Decoupled.scala 215:24:@1027.4]
  reg [31:0] _RAND_8;
  wire [1:0] _T_275_shift_type__T_328_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_shift_type__T_328_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire [1:0] _T_275_shift_type__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_shift_type__T_294_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_shift_type__T_294_mask; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_shift_type__T_294_en; // @[Decoupled.scala 215:24:@1027.4]
  reg [3:0] _T_275_cond_bits [0:1]; // @[Decoupled.scala 215:24:@1027.4]
  reg [31:0] _RAND_9;
  wire [3:0] _T_275_cond_bits__T_328_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_cond_bits__T_328_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire [3:0] _T_275_cond_bits__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_cond_bits__T_294_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_cond_bits__T_294_mask; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_cond_bits__T_294_en; // @[Decoupled.scala 215:24:@1027.4]
  reg [2:0] _T_275_itype [0:1]; // @[Decoupled.scala 215:24:@1027.4]
  reg [31:0] _RAND_10;
  wire [2:0] _T_275_itype__T_328_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_itype__T_328_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire [2:0] _T_275_itype__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_itype__T_294_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_itype__T_294_mask; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_itype__T_294_en; // @[Decoupled.scala 215:24:@1027.4]
  reg [2:0] _T_275_op [0:1]; // @[Decoupled.scala 215:24:@1027.4]
  reg [31:0] _RAND_11;
  wire [2:0] _T_275_op__T_328_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_op__T_328_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire [2:0] _T_275_op__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_op__T_294_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_op__T_294_mask; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_op__T_294_en; // @[Decoupled.scala 215:24:@1027.4]
  reg  _T_275_nzcv_en [0:1]; // @[Decoupled.scala 215:24:@1027.4]
  reg [31:0] _RAND_12;
  wire  _T_275_nzcv_en__T_328_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_nzcv_en__T_328_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_nzcv_en__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_nzcv_en__T_294_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_nzcv_en__T_294_mask; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_nzcv_en__T_294_en; // @[Decoupled.scala 215:24:@1027.4]
  reg  _T_275_tag [0:1]; // @[Decoupled.scala 215:24:@1027.4]
  reg [31:0] _RAND_13;
  wire  _T_275_tag__T_328_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_tag__T_328_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_tag__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_tag__T_294_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_tag__T_294_mask; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_tag__T_294_en; // @[Decoupled.scala 215:24:@1027.4]
  reg [63:0] _T_275_pc [0:1]; // @[Decoupled.scala 215:24:@1027.4]
  reg [63:0] _RAND_14;
  wire [63:0] _T_275_pc__T_328_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_pc__T_328_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire [63:0] _T_275_pc__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_pc__T_294_addr; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_pc__T_294_mask; // @[Decoupled.scala 215:24:@1027.4]
  wire  _T_275_pc__T_294_en; // @[Decoupled.scala 215:24:@1027.4]
  reg  value; // @[Counter.scala 26:33:@1028.4]
  reg [31:0] _RAND_15;
  reg  value_1; // @[Counter.scala 26:33:@1029.4]
  reg [31:0] _RAND_16;
  reg  _T_282; // @[Decoupled.scala 218:35:@1030.4]
  reg [31:0] _RAND_17;
  wire  _T_283; // @[Decoupled.scala 220:41:@1031.4]
  wire  _T_285; // @[Decoupled.scala 221:36:@1032.4]
  wire  _T_286; // @[Decoupled.scala 221:33:@1033.4]
  wire  _T_287; // @[Decoupled.scala 222:32:@1034.4]
  wire  _T_288; // @[Decoupled.scala 37:37:@1035.4]
  wire  _T_291; // @[Decoupled.scala 37:37:@1038.4]
  wire [1:0] _T_317; // @[Counter.scala 35:22:@1064.6]
  wire  _T_318; // @[Counter.scala 35:22:@1065.6]
  wire  _GEN_28; // @[Decoupled.scala 246:27:@1128.6]
  wire  _GEN_50; // @[Decoupled.scala 243:18:@1106.4]
  wire  _GEN_24; // @[Decoupled.scala 226:17:@1041.4]
  wire [1:0] _T_321; // @[Counter.scala 35:22:@1070.6]
  wire  _T_322; // @[Counter.scala 35:22:@1071.6]
  wire  _GEN_49; // @[Decoupled.scala 243:18:@1106.4]
  wire  _GEN_25; // @[Decoupled.scala 230:17:@1068.4]
  wire  _T_323; // @[Decoupled.scala 233:16:@1074.4]
  wire  _GEN_26; // @[Decoupled.scala 233:28:@1075.4]
  wire  _T_325; // @[Decoupled.scala 237:19:@1078.4]
  wire  _T_327; // @[Decoupled.scala 238:19:@1080.4]
  assign _T_275_rd_valid__T_328_addr = value_1;
  assign _T_275_rd_valid__T_328_data = _T_275_rd_valid[_T_275_rd_valid__T_328_addr]; // @[Decoupled.scala 215:24:@1027.4]
  assign _T_275_rd_valid__T_294_data = io_enq_bits_rd_valid;
  assign _T_275_rd_valid__T_294_addr = value;
  assign _T_275_rd_valid__T_294_mask = 1'h1;
  assign _T_275_rd_valid__T_294_en = _T_286 ? _GEN_28 : _T_288;
  assign _T_275_rd_bits__T_328_addr = value_1;
  assign _T_275_rd_bits__T_328_data = _T_275_rd_bits[_T_275_rd_bits__T_328_addr]; // @[Decoupled.scala 215:24:@1027.4]
  assign _T_275_rd_bits__T_294_data = io_enq_bits_rd_bits;
  assign _T_275_rd_bits__T_294_addr = value;
  assign _T_275_rd_bits__T_294_mask = 1'h1;
  assign _T_275_rd_bits__T_294_en = _T_286 ? _GEN_28 : _T_288;
  assign _T_275_rs1_bits__T_328_addr = value_1;
  assign _T_275_rs1_bits__T_328_data = _T_275_rs1_bits[_T_275_rs1_bits__T_328_addr]; // @[Decoupled.scala 215:24:@1027.4]
  assign _T_275_rs1_bits__T_294_data = io_enq_bits_rs1_bits;
  assign _T_275_rs1_bits__T_294_addr = value;
  assign _T_275_rs1_bits__T_294_mask = 1'h1;
  assign _T_275_rs1_bits__T_294_en = _T_286 ? _GEN_28 : _T_288;
  assign _T_275_rs2_valid__T_328_addr = value_1;
  assign _T_275_rs2_valid__T_328_data = _T_275_rs2_valid[_T_275_rs2_valid__T_328_addr]; // @[Decoupled.scala 215:24:@1027.4]
  assign _T_275_rs2_valid__T_294_data = io_enq_bits_rs2_valid;
  assign _T_275_rs2_valid__T_294_addr = value;
  assign _T_275_rs2_valid__T_294_mask = 1'h1;
  assign _T_275_rs2_valid__T_294_en = _T_286 ? _GEN_28 : _T_288;
  assign _T_275_rs2_bits__T_328_addr = value_1;
  assign _T_275_rs2_bits__T_328_data = _T_275_rs2_bits[_T_275_rs2_bits__T_328_addr]; // @[Decoupled.scala 215:24:@1027.4]
  assign _T_275_rs2_bits__T_294_data = io_enq_bits_rs2_bits;
  assign _T_275_rs2_bits__T_294_addr = value;
  assign _T_275_rs2_bits__T_294_mask = 1'h1;
  assign _T_275_rs2_bits__T_294_en = _T_286 ? _GEN_28 : _T_288;
  assign _T_275_imm_bits__T_328_addr = value_1;
  assign _T_275_imm_bits__T_328_data = _T_275_imm_bits[_T_275_imm_bits__T_328_addr]; // @[Decoupled.scala 215:24:@1027.4]
  assign _T_275_imm_bits__T_294_data = io_enq_bits_imm_bits;
  assign _T_275_imm_bits__T_294_addr = value;
  assign _T_275_imm_bits__T_294_mask = 1'h1;
  assign _T_275_imm_bits__T_294_en = _T_286 ? _GEN_28 : _T_288;
  assign _T_275_shift_val_valid__T_328_addr = value_1;
  assign _T_275_shift_val_valid__T_328_data = _T_275_shift_val_valid[_T_275_shift_val_valid__T_328_addr]; // @[Decoupled.scala 215:24:@1027.4]
  assign _T_275_shift_val_valid__T_294_data = io_enq_bits_shift_val_valid;
  assign _T_275_shift_val_valid__T_294_addr = value;
  assign _T_275_shift_val_valid__T_294_mask = 1'h1;
  assign _T_275_shift_val_valid__T_294_en = _T_286 ? _GEN_28 : _T_288;
  assign _T_275_shift_val_bits__T_328_addr = value_1;
  assign _T_275_shift_val_bits__T_328_data = _T_275_shift_val_bits[_T_275_shift_val_bits__T_328_addr]; // @[Decoupled.scala 215:24:@1027.4]
  assign _T_275_shift_val_bits__T_294_data = io_enq_bits_shift_val_bits;
  assign _T_275_shift_val_bits__T_294_addr = value;
  assign _T_275_shift_val_bits__T_294_mask = 1'h1;
  assign _T_275_shift_val_bits__T_294_en = _T_286 ? _GEN_28 : _T_288;
  assign _T_275_shift_type__T_328_addr = value_1;
  assign _T_275_shift_type__T_328_data = _T_275_shift_type[_T_275_shift_type__T_328_addr]; // @[Decoupled.scala 215:24:@1027.4]
  assign _T_275_shift_type__T_294_data = io_enq_bits_shift_type;
  assign _T_275_shift_type__T_294_addr = value;
  assign _T_275_shift_type__T_294_mask = 1'h1;
  assign _T_275_shift_type__T_294_en = _T_286 ? _GEN_28 : _T_288;
  assign _T_275_cond_bits__T_328_addr = value_1;
  assign _T_275_cond_bits__T_328_data = _T_275_cond_bits[_T_275_cond_bits__T_328_addr]; // @[Decoupled.scala 215:24:@1027.4]
  assign _T_275_cond_bits__T_294_data = io_enq_bits_cond_bits;
  assign _T_275_cond_bits__T_294_addr = value;
  assign _T_275_cond_bits__T_294_mask = 1'h1;
  assign _T_275_cond_bits__T_294_en = _T_286 ? _GEN_28 : _T_288;
  assign _T_275_itype__T_328_addr = value_1;
  assign _T_275_itype__T_328_data = _T_275_itype[_T_275_itype__T_328_addr]; // @[Decoupled.scala 215:24:@1027.4]
  assign _T_275_itype__T_294_data = io_enq_bits_itype;
  assign _T_275_itype__T_294_addr = value;
  assign _T_275_itype__T_294_mask = 1'h1;
  assign _T_275_itype__T_294_en = _T_286 ? _GEN_28 : _T_288;
  assign _T_275_op__T_328_addr = value_1;
  assign _T_275_op__T_328_data = _T_275_op[_T_275_op__T_328_addr]; // @[Decoupled.scala 215:24:@1027.4]
  assign _T_275_op__T_294_data = io_enq_bits_op;
  assign _T_275_op__T_294_addr = value;
  assign _T_275_op__T_294_mask = 1'h1;
  assign _T_275_op__T_294_en = _T_286 ? _GEN_28 : _T_288;
  assign _T_275_nzcv_en__T_328_addr = value_1;
  assign _T_275_nzcv_en__T_328_data = _T_275_nzcv_en[_T_275_nzcv_en__T_328_addr]; // @[Decoupled.scala 215:24:@1027.4]
  assign _T_275_nzcv_en__T_294_data = io_enq_bits_nzcv_en;
  assign _T_275_nzcv_en__T_294_addr = value;
  assign _T_275_nzcv_en__T_294_mask = 1'h1;
  assign _T_275_nzcv_en__T_294_en = _T_286 ? _GEN_28 : _T_288;
  assign _T_275_tag__T_328_addr = value_1;
  assign _T_275_tag__T_328_data = _T_275_tag[_T_275_tag__T_328_addr]; // @[Decoupled.scala 215:24:@1027.4]
  assign _T_275_tag__T_294_data = io_enq_bits_tag;
  assign _T_275_tag__T_294_addr = value;
  assign _T_275_tag__T_294_mask = 1'h1;
  assign _T_275_tag__T_294_en = _T_286 ? _GEN_28 : _T_288;
  assign _T_275_pc__T_328_addr = value_1;
  assign _T_275_pc__T_328_data = _T_275_pc[_T_275_pc__T_328_addr]; // @[Decoupled.scala 215:24:@1027.4]
  assign _T_275_pc__T_294_data = io_enq_bits_pc;
  assign _T_275_pc__T_294_addr = value;
  assign _T_275_pc__T_294_mask = 1'h1;
  assign _T_275_pc__T_294_en = _T_286 ? _GEN_28 : _T_288;
  assign _T_283 = value == value_1; // @[Decoupled.scala 220:41:@1031.4]
  assign _T_285 = _T_282 == 1'h0; // @[Decoupled.scala 221:36:@1032.4]
  assign _T_286 = _T_283 & _T_285; // @[Decoupled.scala 221:33:@1033.4]
  assign _T_287 = _T_283 & _T_282; // @[Decoupled.scala 222:32:@1034.4]
  assign _T_288 = io_enq_ready & io_enq_valid; // @[Decoupled.scala 37:37:@1035.4]
  assign _T_291 = io_deq_ready & io_deq_valid; // @[Decoupled.scala 37:37:@1038.4]
  assign _T_317 = value + 1'h1; // @[Counter.scala 35:22:@1064.6]
  assign _T_318 = value + 1'h1; // @[Counter.scala 35:22:@1065.6]
  assign _GEN_28 = io_deq_ready ? 1'h0 : _T_288; // @[Decoupled.scala 246:27:@1128.6]
  assign _GEN_50 = _T_286 ? _GEN_28 : _T_288; // @[Decoupled.scala 243:18:@1106.4]
  assign _GEN_24 = _GEN_50 ? _T_318 : value; // @[Decoupled.scala 226:17:@1041.4]
  assign _T_321 = value_1 + 1'h1; // @[Counter.scala 35:22:@1070.6]
  assign _T_322 = value_1 + 1'h1; // @[Counter.scala 35:22:@1071.6]
  assign _GEN_49 = _T_286 ? 1'h0 : _T_291; // @[Decoupled.scala 243:18:@1106.4]
  assign _GEN_25 = _GEN_49 ? _T_322 : value_1; // @[Decoupled.scala 230:17:@1068.4]
  assign _T_323 = _GEN_50 != _GEN_49; // @[Decoupled.scala 233:16:@1074.4]
  assign _GEN_26 = _T_323 ? _GEN_50 : _T_282; // @[Decoupled.scala 233:28:@1075.4]
  assign _T_325 = _T_286 == 1'h0; // @[Decoupled.scala 237:19:@1078.4]
  assign _T_327 = _T_287 == 1'h0; // @[Decoupled.scala 238:19:@1080.4]
  assign io_enq_ready = io_deq_ready ? 1'h1 : _T_327; // @[Decoupled.scala 238:16:@1081.4 Decoupled.scala 251:40:@1133.6]
  assign io_deq_valid = io_enq_valid ? 1'h1 : _T_325; // @[Decoupled.scala 237:16:@1079.4 Decoupled.scala 242:40:@1104.6]
  assign io_deq_bits_rd_valid = _T_286 ? io_enq_bits_rd_valid : _T_275_rd_valid__T_328_data; // @[Decoupled.scala 239:15:@1102.4 Decoupled.scala 244:19:@1126.6]
  assign io_deq_bits_rd_bits = _T_286 ? io_enq_bits_rd_bits : _T_275_rd_bits__T_328_data; // @[Decoupled.scala 239:15:@1101.4 Decoupled.scala 244:19:@1125.6]
  assign io_deq_bits_rs1_bits = _T_286 ? io_enq_bits_rs1_bits : _T_275_rs1_bits__T_328_data; // @[Decoupled.scala 239:15:@1099.4 Decoupled.scala 244:19:@1123.6]
  assign io_deq_bits_rs2_valid = _T_286 ? io_enq_bits_rs2_valid : _T_275_rs2_valid__T_328_data; // @[Decoupled.scala 239:15:@1098.4 Decoupled.scala 244:19:@1122.6]
  assign io_deq_bits_rs2_bits = _T_286 ? io_enq_bits_rs2_bits : _T_275_rs2_bits__T_328_data; // @[Decoupled.scala 239:15:@1097.4 Decoupled.scala 244:19:@1121.6]
  assign io_deq_bits_imm_bits = _T_286 ? io_enq_bits_imm_bits : _T_275_imm_bits__T_328_data; // @[Decoupled.scala 239:15:@1095.4 Decoupled.scala 244:19:@1119.6]
  assign io_deq_bits_shift_val_valid = _T_286 ? io_enq_bits_shift_val_valid : _T_275_shift_val_valid__T_328_data; // @[Decoupled.scala 239:15:@1094.4 Decoupled.scala 244:19:@1118.6]
  assign io_deq_bits_shift_val_bits = _T_286 ? io_enq_bits_shift_val_bits : _T_275_shift_val_bits__T_328_data; // @[Decoupled.scala 239:15:@1093.4 Decoupled.scala 244:19:@1117.6]
  assign io_deq_bits_shift_type = _T_286 ? io_enq_bits_shift_type : _T_275_shift_type__T_328_data; // @[Decoupled.scala 239:15:@1092.4 Decoupled.scala 244:19:@1116.6]
  assign io_deq_bits_cond_bits = _T_286 ? io_enq_bits_cond_bits : _T_275_cond_bits__T_328_data; // @[Decoupled.scala 239:15:@1090.4 Decoupled.scala 244:19:@1114.6]
  assign io_deq_bits_itype = _T_286 ? io_enq_bits_itype : _T_275_itype__T_328_data; // @[Decoupled.scala 239:15:@1089.4 Decoupled.scala 244:19:@1113.6]
  assign io_deq_bits_op = _T_286 ? io_enq_bits_op : _T_275_op__T_328_data; // @[Decoupled.scala 239:15:@1088.4 Decoupled.scala 244:19:@1112.6]
  assign io_deq_bits_nzcv_en = _T_286 ? io_enq_bits_nzcv_en : _T_275_nzcv_en__T_328_data; // @[Decoupled.scala 239:15:@1087.4 Decoupled.scala 244:19:@1111.6]
  assign io_deq_bits_tag = _T_286 ? io_enq_bits_tag : _T_275_tag__T_328_data; // @[Decoupled.scala 239:15:@1086.4 Decoupled.scala 244:19:@1110.6]
  assign io_deq_bits_pc = _T_286 ? io_enq_bits_pc : _T_275_pc__T_328_data; // @[Decoupled.scala 239:15:@1083.4 Decoupled.scala 244:19:@1107.6]
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
    _T_275_rd_valid[initvar] = _RAND_0[0:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_1 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_275_rd_bits[initvar] = _RAND_1[4:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_2 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_275_rs1_bits[initvar] = _RAND_2[4:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_3 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_275_rs2_valid[initvar] = _RAND_3[0:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_4 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_275_rs2_bits[initvar] = _RAND_4[4:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_5 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_275_imm_bits[initvar] = _RAND_5[25:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_6 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_275_shift_val_valid[initvar] = _RAND_6[0:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_7 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_275_shift_val_bits[initvar] = _RAND_7[5:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_8 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_275_shift_type[initvar] = _RAND_8[1:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_9 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_275_cond_bits[initvar] = _RAND_9[3:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_10 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_275_itype[initvar] = _RAND_10[2:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_11 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_275_op[initvar] = _RAND_11[2:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_12 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_275_nzcv_en[initvar] = _RAND_12[0:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_13 = {1{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_275_tag[initvar] = _RAND_13[0:0];
  `endif // RANDOMIZE_MEM_INIT
  _RAND_14 = {2{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 2; initvar = initvar+1)
    _T_275_pc[initvar] = _RAND_14[63:0];
  `endif // RANDOMIZE_MEM_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_15 = {1{`RANDOM}};
  value = _RAND_15[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_16 = {1{`RANDOM}};
  value_1 = _RAND_16[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_17 = {1{`RANDOM}};
  _T_282 = _RAND_17[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if(_T_275_rd_valid__T_294_en & _T_275_rd_valid__T_294_mask) begin
      _T_275_rd_valid[_T_275_rd_valid__T_294_addr] <= _T_275_rd_valid__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
    end
    if(_T_275_rd_bits__T_294_en & _T_275_rd_bits__T_294_mask) begin
      _T_275_rd_bits[_T_275_rd_bits__T_294_addr] <= _T_275_rd_bits__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
    end
    if(_T_275_rs1_bits__T_294_en & _T_275_rs1_bits__T_294_mask) begin
      _T_275_rs1_bits[_T_275_rs1_bits__T_294_addr] <= _T_275_rs1_bits__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
    end
    if(_T_275_rs2_valid__T_294_en & _T_275_rs2_valid__T_294_mask) begin
      _T_275_rs2_valid[_T_275_rs2_valid__T_294_addr] <= _T_275_rs2_valid__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
    end
    if(_T_275_rs2_bits__T_294_en & _T_275_rs2_bits__T_294_mask) begin
      _T_275_rs2_bits[_T_275_rs2_bits__T_294_addr] <= _T_275_rs2_bits__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
    end
    if(_T_275_imm_bits__T_294_en & _T_275_imm_bits__T_294_mask) begin
      _T_275_imm_bits[_T_275_imm_bits__T_294_addr] <= _T_275_imm_bits__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
    end
    if(_T_275_shift_val_valid__T_294_en & _T_275_shift_val_valid__T_294_mask) begin
      _T_275_shift_val_valid[_T_275_shift_val_valid__T_294_addr] <= _T_275_shift_val_valid__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
    end
    if(_T_275_shift_val_bits__T_294_en & _T_275_shift_val_bits__T_294_mask) begin
      _T_275_shift_val_bits[_T_275_shift_val_bits__T_294_addr] <= _T_275_shift_val_bits__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
    end
    if(_T_275_shift_type__T_294_en & _T_275_shift_type__T_294_mask) begin
      _T_275_shift_type[_T_275_shift_type__T_294_addr] <= _T_275_shift_type__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
    end
    if(_T_275_cond_bits__T_294_en & _T_275_cond_bits__T_294_mask) begin
      _T_275_cond_bits[_T_275_cond_bits__T_294_addr] <= _T_275_cond_bits__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
    end
    if(_T_275_itype__T_294_en & _T_275_itype__T_294_mask) begin
      _T_275_itype[_T_275_itype__T_294_addr] <= _T_275_itype__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
    end
    if(_T_275_op__T_294_en & _T_275_op__T_294_mask) begin
      _T_275_op[_T_275_op__T_294_addr] <= _T_275_op__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
    end
    if(_T_275_nzcv_en__T_294_en & _T_275_nzcv_en__T_294_mask) begin
      _T_275_nzcv_en[_T_275_nzcv_en__T_294_addr] <= _T_275_nzcv_en__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
    end
    if(_T_275_tag__T_294_en & _T_275_tag__T_294_mask) begin
      _T_275_tag[_T_275_tag__T_294_addr] <= _T_275_tag__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
    end
    if(_T_275_pc__T_294_en & _T_275_pc__T_294_mask) begin
      _T_275_pc[_T_275_pc__T_294_addr] <= _T_275_pc__T_294_data; // @[Decoupled.scala 215:24:@1027.4]
    end
    if (reset) begin
      value <= 1'h0;
    end else begin
      if (_GEN_50) begin
        value <= _T_318;
      end
    end
    if (reset) begin
      value_1 <= 1'h0;
    end else begin
      if (_GEN_49) begin
        value_1 <= _T_322;
      end
    end
    if (reset) begin
      _T_282 <= 1'h0;
    end else begin
      if (_T_323) begin
        if (_T_286) begin
          if (io_deq_ready) begin
            _T_282 <= 1'h0;
          end else begin
            _T_282 <= _T_288;
          end
        end else begin
          _T_282 <= _T_288;
        end
      end
    end
  end
endmodule
