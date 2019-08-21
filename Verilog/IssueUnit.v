module IssueUnit( // @[:@1290.2]
  input         clock, // @[:@1291.4]
  input         reset, // @[:@1292.4]
  input         io_flush, // @[:@1293.4]
  input         io_flushTag, // @[:@1293.4]
  output        io_enq_ready, // @[:@1293.4]
  input         io_enq_valid, // @[:@1293.4]
  input  [4:0]  io_enq_bits_rd, // @[:@1293.4]
  input  [4:0]  io_enq_bits_rs1, // @[:@1293.4]
  input  [4:0]  io_enq_bits_rs2, // @[:@1293.4]
  input  [25:0] io_enq_bits_imm, // @[:@1293.4]
  input  [5:0]  io_enq_bits_shift_val, // @[:@1293.4]
  input  [1:0]  io_enq_bits_shift_type, // @[:@1293.4]
  input  [3:0]  io_enq_bits_cond, // @[:@1293.4]
  input  [2:0]  io_enq_bits_itype, // @[:@1293.4]
  input  [2:0]  io_enq_bits_op, // @[:@1293.4]
  input         io_enq_bits_rd_en, // @[:@1293.4]
  input         io_enq_bits_rs2_en, // @[:@1293.4]
  input         io_enq_bits_shift_en, // @[:@1293.4]
  input         io_enq_bits_nzcv_en, // @[:@1293.4]
  input         io_enq_bits_tag, // @[:@1293.4]
  output        io_deq_valid, // @[:@1293.4]
  output [4:0]  io_deq_bits_rd, // @[:@1293.4]
  output [4:0]  io_deq_bits_rs1, // @[:@1293.4]
  output [4:0]  io_deq_bits_rs2, // @[:@1293.4]
  output [25:0] io_deq_bits_imm, // @[:@1293.4]
  output [5:0]  io_deq_bits_shift_val, // @[:@1293.4]
  output [1:0]  io_deq_bits_shift_type, // @[:@1293.4]
  output [3:0]  io_deq_bits_cond, // @[:@1293.4]
  output [2:0]  io_deq_bits_itype, // @[:@1293.4]
  output [2:0]  io_deq_bits_op, // @[:@1293.4]
  output        io_deq_bits_rd_en, // @[:@1293.4]
  output        io_deq_bits_rs2_en, // @[:@1293.4]
  output        io_deq_bits_shift_en, // @[:@1293.4]
  output        io_deq_bits_nzcv_en, // @[:@1293.4]
  output        io_deq_bits_tag, // @[:@1293.4]
  input         io_exeReg_valid, // @[:@1293.4]
  input  [4:0]  io_exeReg_bits_rd, // @[:@1293.4]
  input         io_exeReg_bits_rd_en, // @[:@1293.4]
  input         io_exeReg_bits_tag // @[:@1293.4]
);
  wire  Queue_clock; // @[issue.scala 101:106:@1393.4]
  wire  Queue_reset; // @[issue.scala 101:106:@1393.4]
  wire  Queue_io_enq_ready; // @[issue.scala 101:106:@1393.4]
  wire  Queue_io_enq_valid; // @[issue.scala 101:106:@1393.4]
  wire [4:0] Queue_io_enq_bits_rd; // @[issue.scala 101:106:@1393.4]
  wire [4:0] Queue_io_enq_bits_rs1; // @[issue.scala 101:106:@1393.4]
  wire [4:0] Queue_io_enq_bits_rs2; // @[issue.scala 101:106:@1393.4]
  wire [25:0] Queue_io_enq_bits_imm; // @[issue.scala 101:106:@1393.4]
  wire [5:0] Queue_io_enq_bits_shift_val; // @[issue.scala 101:106:@1393.4]
  wire [1:0] Queue_io_enq_bits_shift_type; // @[issue.scala 101:106:@1393.4]
  wire [3:0] Queue_io_enq_bits_cond; // @[issue.scala 101:106:@1393.4]
  wire [2:0] Queue_io_enq_bits_itype; // @[issue.scala 101:106:@1393.4]
  wire [2:0] Queue_io_enq_bits_op; // @[issue.scala 101:106:@1393.4]
  wire  Queue_io_enq_bits_rd_en; // @[issue.scala 101:106:@1393.4]
  wire  Queue_io_enq_bits_rs2_en; // @[issue.scala 101:106:@1393.4]
  wire  Queue_io_enq_bits_shift_en; // @[issue.scala 101:106:@1393.4]
  wire  Queue_io_enq_bits_nzcv_en; // @[issue.scala 101:106:@1393.4]
  wire  Queue_io_enq_bits_tag; // @[issue.scala 101:106:@1393.4]
  wire  Queue_io_deq_ready; // @[issue.scala 101:106:@1393.4]
  wire  Queue_io_deq_valid; // @[issue.scala 101:106:@1393.4]
  wire [4:0] Queue_io_deq_bits_rd; // @[issue.scala 101:106:@1393.4]
  wire [4:0] Queue_io_deq_bits_rs1; // @[issue.scala 101:106:@1393.4]
  wire [4:0] Queue_io_deq_bits_rs2; // @[issue.scala 101:106:@1393.4]
  wire [25:0] Queue_io_deq_bits_imm; // @[issue.scala 101:106:@1393.4]
  wire [5:0] Queue_io_deq_bits_shift_val; // @[issue.scala 101:106:@1393.4]
  wire [1:0] Queue_io_deq_bits_shift_type; // @[issue.scala 101:106:@1393.4]
  wire [3:0] Queue_io_deq_bits_cond; // @[issue.scala 101:106:@1393.4]
  wire [2:0] Queue_io_deq_bits_itype; // @[issue.scala 101:106:@1393.4]
  wire [2:0] Queue_io_deq_bits_op; // @[issue.scala 101:106:@1393.4]
  wire  Queue_io_deq_bits_rd_en; // @[issue.scala 101:106:@1393.4]
  wire  Queue_io_deq_bits_rs2_en; // @[issue.scala 101:106:@1393.4]
  wire  Queue_io_deq_bits_shift_en; // @[issue.scala 101:106:@1393.4]
  wire  Queue_io_deq_bits_nzcv_en; // @[issue.scala 101:106:@1393.4]
  wire  Queue_io_deq_bits_tag; // @[issue.scala 101:106:@1393.4]
  wire  Queue_1_clock; // @[issue.scala 101:106:@1400.4]
  wire  Queue_1_reset; // @[issue.scala 101:106:@1400.4]
  wire  Queue_1_io_enq_ready; // @[issue.scala 101:106:@1400.4]
  wire  Queue_1_io_enq_valid; // @[issue.scala 101:106:@1400.4]
  wire [4:0] Queue_1_io_enq_bits_rd; // @[issue.scala 101:106:@1400.4]
  wire [4:0] Queue_1_io_enq_bits_rs1; // @[issue.scala 101:106:@1400.4]
  wire [4:0] Queue_1_io_enq_bits_rs2; // @[issue.scala 101:106:@1400.4]
  wire [25:0] Queue_1_io_enq_bits_imm; // @[issue.scala 101:106:@1400.4]
  wire [5:0] Queue_1_io_enq_bits_shift_val; // @[issue.scala 101:106:@1400.4]
  wire [1:0] Queue_1_io_enq_bits_shift_type; // @[issue.scala 101:106:@1400.4]
  wire [3:0] Queue_1_io_enq_bits_cond; // @[issue.scala 101:106:@1400.4]
  wire [2:0] Queue_1_io_enq_bits_itype; // @[issue.scala 101:106:@1400.4]
  wire [2:0] Queue_1_io_enq_bits_op; // @[issue.scala 101:106:@1400.4]
  wire  Queue_1_io_enq_bits_rd_en; // @[issue.scala 101:106:@1400.4]
  wire  Queue_1_io_enq_bits_rs2_en; // @[issue.scala 101:106:@1400.4]
  wire  Queue_1_io_enq_bits_shift_en; // @[issue.scala 101:106:@1400.4]
  wire  Queue_1_io_enq_bits_nzcv_en; // @[issue.scala 101:106:@1400.4]
  wire  Queue_1_io_enq_bits_tag; // @[issue.scala 101:106:@1400.4]
  wire  Queue_1_io_deq_ready; // @[issue.scala 101:106:@1400.4]
  wire  Queue_1_io_deq_valid; // @[issue.scala 101:106:@1400.4]
  wire [4:0] Queue_1_io_deq_bits_rd; // @[issue.scala 101:106:@1400.4]
  wire [4:0] Queue_1_io_deq_bits_rs1; // @[issue.scala 101:106:@1400.4]
  wire [4:0] Queue_1_io_deq_bits_rs2; // @[issue.scala 101:106:@1400.4]
  wire [25:0] Queue_1_io_deq_bits_imm; // @[issue.scala 101:106:@1400.4]
  wire [5:0] Queue_1_io_deq_bits_shift_val; // @[issue.scala 101:106:@1400.4]
  wire [1:0] Queue_1_io_deq_bits_shift_type; // @[issue.scala 101:106:@1400.4]
  wire [3:0] Queue_1_io_deq_bits_cond; // @[issue.scala 101:106:@1400.4]
  wire [2:0] Queue_1_io_deq_bits_itype; // @[issue.scala 101:106:@1400.4]
  wire [2:0] Queue_1_io_deq_bits_op; // @[issue.scala 101:106:@1400.4]
  wire  Queue_1_io_deq_bits_rd_en; // @[issue.scala 101:106:@1400.4]
  wire  Queue_1_io_deq_bits_rs2_en; // @[issue.scala 101:106:@1400.4]
  wire  Queue_1_io_deq_bits_shift_en; // @[issue.scala 101:106:@1400.4]
  wire  Queue_1_io_deq_bits_nzcv_en; // @[issue.scala 101:106:@1400.4]
  wire  Queue_1_io_deq_bits_tag; // @[issue.scala 101:106:@1400.4]
  wire  arbiter_clock; // @[issue.scala 108:29:@1494.4]
  wire  arbiter_reset; // @[issue.scala 108:29:@1494.4]
  wire [1:0] arbiter_io_ready; // @[issue.scala 108:29:@1494.4]
  wire  arbiter_io_next_valid; // @[issue.scala 108:29:@1494.4]
  wire  arbiter_io_next_bits; // @[issue.scala 108:29:@1494.4]
  reg [4:0] reg_pipe_0_rd; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_0;
  reg [4:0] reg_pipe_0_rs1; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_1;
  reg [4:0] reg_pipe_0_rs2; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_2;
  reg [25:0] reg_pipe_0_imm; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_3;
  reg [5:0] reg_pipe_0_shift_val; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_4;
  reg [1:0] reg_pipe_0_shift_type; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_5;
  reg [3:0] reg_pipe_0_cond; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_6;
  reg [2:0] reg_pipe_0_itype; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_7;
  reg [2:0] reg_pipe_0_op; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_8;
  reg  reg_pipe_0_rd_en; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_9;
  reg  reg_pipe_0_rs2_en; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_10;
  reg  reg_pipe_0_shift_en; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_11;
  reg  reg_pipe_0_nzcv_en; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_12;
  reg  reg_pipe_0_tag; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_13;
  reg [4:0] reg_pipe_1_rd; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_14;
  reg [4:0] reg_pipe_1_rs1; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_15;
  reg [4:0] reg_pipe_1_rs2; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_16;
  reg [25:0] reg_pipe_1_imm; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_17;
  reg [5:0] reg_pipe_1_shift_val; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_18;
  reg [1:0] reg_pipe_1_shift_type; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_19;
  reg [3:0] reg_pipe_1_cond; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_20;
  reg [2:0] reg_pipe_1_itype; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_21;
  reg [2:0] reg_pipe_1_op; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_22;
  reg  reg_pipe_1_rd_en; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_23;
  reg  reg_pipe_1_rs2_en; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_24;
  reg  reg_pipe_1_shift_en; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_25;
  reg  reg_pipe_1_nzcv_en; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_26;
  reg  reg_pipe_1_tag; // @[issue.scala 86:27:@1378.4]
  reg [31:0] _RAND_27;
  reg  reg_pipe_v_0; // @[issue.scala 87:27:@1382.4]
  reg [31:0] _RAND_28;
  reg  reg_pipe_v_1; // @[issue.scala 87:27:@1382.4]
  reg [31:0] _RAND_29;
  wire  _T_135; // @[issue.scala 101:68:@1390.4]
  wire  _T_136; // @[issue.scala 101:84:@1391.4]
  wire  _T_141; // @[issue.scala 101:84:@1398.4]
  wire  fifo_vec_0_enq_ready; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1448.4]
  wire  _T_405; // @[issue.scala 157:30:@1638.4]
  wire  sig_next_idx; // @[:@1500.4 :@1501.4 issue.scala 149:20:@1615.4]
  wire  sig_pipe_r_0; // @[issue.scala 129:24:@1550.4]
  wire  _GEN_176; // @[issue.scala 158:38:@1640.6]
  wire  fifo_vec_0_deq_valid; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1425.4]
  wire [4:0] fifo_vec_0_deq_bits_rd; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1424.4]
  wire [4:0] fifo_vec_0_deq_bits_rs1; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1423.4]
  wire [4:0] fifo_vec_0_deq_bits_rs2; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1422.4]
  wire [25:0] fifo_vec_0_deq_bits_imm; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1421.4]
  wire [5:0] fifo_vec_0_deq_bits_shift_val; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1420.4]
  wire [1:0] fifo_vec_0_deq_bits_shift_type; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1419.4]
  wire [3:0] fifo_vec_0_deq_bits_cond; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1418.4]
  wire [2:0] fifo_vec_0_deq_bits_itype; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1417.4]
  wire [2:0] fifo_vec_0_deq_bits_op; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1416.4]
  wire  fifo_vec_0_deq_bits_rd_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1415.4]
  wire  fifo_vec_0_deq_bits_rs2_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1413.4]
  wire  fifo_vec_0_deq_bits_shift_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1411.4]
  wire  fifo_vec_0_deq_bits_nzcv_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1409.4]
  wire  fifo_vec_0_deq_bits_tag; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1407.4]
  wire  fifo_vec_1_enq_ready; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1493.4]
  wire  sig_pipe_r_1; // @[issue.scala 129:24:@1577.4]
  wire  _GEN_177; // @[issue.scala 158:38:@1640.6]
  wire  fifo_vec_1_deq_valid; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1470.4]
  wire [4:0] fifo_vec_1_deq_bits_rd; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1469.4]
  wire [4:0] fifo_vec_1_deq_bits_rs1; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1468.4]
  wire [4:0] fifo_vec_1_deq_bits_rs2; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1467.4]
  wire [25:0] fifo_vec_1_deq_bits_imm; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1466.4]
  wire [5:0] fifo_vec_1_deq_bits_shift_val; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1465.4]
  wire [1:0] fifo_vec_1_deq_bits_shift_type; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1464.4]
  wire [3:0] fifo_vec_1_deq_bits_cond; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1463.4]
  wire [2:0] fifo_vec_1_deq_bits_itype; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1462.4]
  wire [2:0] fifo_vec_1_deq_bits_op; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1461.4]
  wire  fifo_vec_1_deq_bits_rd_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1460.4]
  wire  fifo_vec_1_deq_bits_rs2_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1458.4]
  wire  fifo_vec_1_deq_bits_shift_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1456.4]
  wire  fifo_vec_1_deq_bits_nzcv_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1454.4]
  wire  fifo_vec_1_deq_bits_tag; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1452.4]
  wire [4:0] _T_379_rd; // @[issue.scala 132:27:@1553.4]
  wire [4:0] _T_379_rs1; // @[issue.scala 132:27:@1553.4]
  wire [4:0] _T_379_rs2; // @[issue.scala 132:27:@1553.4]
  wire [25:0] _T_379_imm; // @[issue.scala 132:27:@1553.4]
  wire [5:0] _T_379_shift_val; // @[issue.scala 132:27:@1553.4]
  wire [1:0] _T_379_shift_type; // @[issue.scala 132:27:@1553.4]
  wire [3:0] _T_379_cond; // @[issue.scala 132:27:@1553.4]
  wire [2:0] _T_379_itype; // @[issue.scala 132:27:@1553.4]
  wire [2:0] _T_379_op; // @[issue.scala 132:27:@1553.4]
  wire  _T_379_rd_en; // @[issue.scala 132:27:@1553.4]
  wire  _T_379_rs2_en; // @[issue.scala 132:27:@1553.4]
  wire  _T_379_shift_en; // @[issue.scala 132:27:@1553.4]
  wire  _T_379_nzcv_en; // @[issue.scala 132:27:@1553.4]
  wire  _T_379_tag; // @[issue.scala 132:27:@1553.4]
  wire  _T_380; // @[issue.scala 133:27:@1574.4]
  wire [4:0] _T_382_rd; // @[issue.scala 132:27:@1580.4]
  wire [4:0] _T_382_rs1; // @[issue.scala 132:27:@1580.4]
  wire [4:0] _T_382_rs2; // @[issue.scala 132:27:@1580.4]
  wire [25:0] _T_382_imm; // @[issue.scala 132:27:@1580.4]
  wire [5:0] _T_382_shift_val; // @[issue.scala 132:27:@1580.4]
  wire [1:0] _T_382_shift_type; // @[issue.scala 132:27:@1580.4]
  wire [3:0] _T_382_cond; // @[issue.scala 132:27:@1580.4]
  wire [2:0] _T_382_itype; // @[issue.scala 132:27:@1580.4]
  wire [2:0] _T_382_op; // @[issue.scala 132:27:@1580.4]
  wire  _T_382_rd_en; // @[issue.scala 132:27:@1580.4]
  wire  _T_382_rs2_en; // @[issue.scala 132:27:@1580.4]
  wire  _T_382_shift_en; // @[issue.scala 132:27:@1580.4]
  wire  _T_382_nzcv_en; // @[issue.scala 132:27:@1580.4]
  wire  _T_382_tag; // @[issue.scala 132:27:@1580.4]
  wire  _T_383; // @[issue.scala 133:27:@1601.4]
  wire  rfile_wb_pending; // @[issue.scala 140:42:@1604.4]
  wire [4:0] _GEN_113; // @[issue.scala 142:40:@1605.4]
  wire [4:0] _GEN_114; // @[issue.scala 142:40:@1605.4]
  wire  _T_387; // @[issue.scala 142:40:@1605.4]
  wire  _T_391; // @[issue.scala 143:42:@1606.4]
  wire  _T_392; // @[issue.scala 142:63:@1607.4]
  wire  exe_stall; // @[issue.scala 141:33:@1608.4]
  wire  _T_399; // @[issue.scala 144:71:@1610.4]
  wire  _GEN_133; // @[issue.scala 144:68:@1611.4]
  wire  _T_400; // @[issue.scala 144:68:@1611.4]
  wire  _GEN_134; // @[issue.scala 144:34:@1612.4]
  wire  _GEN_135; // @[issue.scala 144:34:@1612.4]
  wire  _GEN_397; // @[issue.scala 167:29:@1706.6]
  wire  sig_pipe_i_1; // @[issue.scala 164:18:@1663.4]
  wire  _GEN_396; // @[issue.scala 167:29:@1706.6]
  wire  sig_pipe_i_0; // @[issue.scala 164:18:@1663.4]
  wire  _GEN_248; // @[issue.scala 159:30:@1641.6]
  wire  _GEN_178; // @[issue.scala 159:30:@1641.6]
  wire  _GEN_179; // @[issue.scala 159:30:@1641.6]
  wire [4:0] _GEN_249; // @[issue.scala 159:30:@1641.6]
  wire [4:0] _GEN_250; // @[issue.scala 159:30:@1641.6]
  wire [4:0] _GEN_251; // @[issue.scala 159:30:@1641.6]
  wire [25:0] _GEN_252; // @[issue.scala 159:30:@1641.6]
  wire [5:0] _GEN_253; // @[issue.scala 159:30:@1641.6]
  wire [1:0] _GEN_254; // @[issue.scala 159:30:@1641.6]
  wire [3:0] _GEN_255; // @[issue.scala 159:30:@1641.6]
  wire [2:0] _GEN_256; // @[issue.scala 159:30:@1641.6]
  wire [2:0] _GEN_257; // @[issue.scala 159:30:@1641.6]
  wire  _GEN_258; // @[issue.scala 159:30:@1641.6]
  wire  _GEN_260; // @[issue.scala 159:30:@1641.6]
  wire  _GEN_262; // @[issue.scala 159:30:@1641.6]
  wire  _GEN_264; // @[issue.scala 159:30:@1641.6]
  wire  _GEN_266; // @[issue.scala 159:30:@1641.6]
  wire  _GEN_274; // @[issue.scala 160:28:@1644.6]
  wire  _GEN_275; // @[issue.scala 160:28:@1644.6]
  wire  _GEN_278; // @[issue.scala 160:28:@1646.6]
  wire  _GEN_279; // @[issue.scala 160:28:@1646.6]
  wire  _GEN_282; // @[issue.scala 160:28:@1648.6]
  wire  _GEN_283; // @[issue.scala 160:28:@1648.6]
  wire  _GEN_286; // @[issue.scala 160:28:@1650.6]
  wire  _GEN_287; // @[issue.scala 160:28:@1650.6]
  wire  _GEN_290; // @[issue.scala 160:28:@1652.6]
  wire  _GEN_291; // @[issue.scala 160:28:@1652.6]
  wire [2:0] _GEN_292; // @[issue.scala 160:28:@1653.6]
  wire [2:0] _GEN_293; // @[issue.scala 160:28:@1653.6]
  wire [2:0] _GEN_294; // @[issue.scala 160:28:@1654.6]
  wire [2:0] _GEN_295; // @[issue.scala 160:28:@1654.6]
  wire [3:0] _GEN_296; // @[issue.scala 160:28:@1655.6]
  wire [3:0] _GEN_297; // @[issue.scala 160:28:@1655.6]
  wire [1:0] _GEN_298; // @[issue.scala 160:28:@1656.6]
  wire [1:0] _GEN_299; // @[issue.scala 160:28:@1656.6]
  wire [5:0] _GEN_300; // @[issue.scala 160:28:@1657.6]
  wire [5:0] _GEN_301; // @[issue.scala 160:28:@1657.6]
  wire [25:0] _GEN_302; // @[issue.scala 160:28:@1658.6]
  wire [25:0] _GEN_303; // @[issue.scala 160:28:@1658.6]
  wire [4:0] _GEN_304; // @[issue.scala 160:28:@1659.6]
  wire [4:0] _GEN_305; // @[issue.scala 160:28:@1659.6]
  wire [4:0] _GEN_306; // @[issue.scala 160:28:@1660.6]
  wire [4:0] _GEN_307; // @[issue.scala 160:28:@1660.6]
  wire [4:0] _GEN_308; // @[issue.scala 160:28:@1661.6]
  wire [4:0] _GEN_309; // @[issue.scala 160:28:@1661.6]
  wire  _GEN_312; // @[issue.scala 157:47:@1639.4]
  wire  _GEN_313; // @[issue.scala 157:47:@1639.4]
  wire  _GEN_318; // @[issue.scala 157:47:@1639.4]
  wire  _GEN_319; // @[issue.scala 157:47:@1639.4]
  wire  _GEN_322; // @[issue.scala 157:47:@1639.4]
  wire  _GEN_323; // @[issue.scala 157:47:@1639.4]
  wire  _GEN_326; // @[issue.scala 157:47:@1639.4]
  wire  _GEN_327; // @[issue.scala 157:47:@1639.4]
  wire  _GEN_330; // @[issue.scala 157:47:@1639.4]
  wire  _GEN_331; // @[issue.scala 157:47:@1639.4]
  wire  _GEN_334; // @[issue.scala 157:47:@1639.4]
  wire  _GEN_335; // @[issue.scala 157:47:@1639.4]
  wire [2:0] _GEN_336; // @[issue.scala 157:47:@1639.4]
  wire [2:0] _GEN_337; // @[issue.scala 157:47:@1639.4]
  wire [2:0] _GEN_338; // @[issue.scala 157:47:@1639.4]
  wire [2:0] _GEN_339; // @[issue.scala 157:47:@1639.4]
  wire [3:0] _GEN_340; // @[issue.scala 157:47:@1639.4]
  wire [3:0] _GEN_341; // @[issue.scala 157:47:@1639.4]
  wire [1:0] _GEN_342; // @[issue.scala 157:47:@1639.4]
  wire [1:0] _GEN_343; // @[issue.scala 157:47:@1639.4]
  wire [5:0] _GEN_344; // @[issue.scala 157:47:@1639.4]
  wire [5:0] _GEN_345; // @[issue.scala 157:47:@1639.4]
  wire [25:0] _GEN_346; // @[issue.scala 157:47:@1639.4]
  wire [25:0] _GEN_347; // @[issue.scala 157:47:@1639.4]
  wire [4:0] _GEN_348; // @[issue.scala 157:47:@1639.4]
  wire [4:0] _GEN_349; // @[issue.scala 157:47:@1639.4]
  wire [4:0] _GEN_350; // @[issue.scala 157:47:@1639.4]
  wire [4:0] _GEN_351; // @[issue.scala 157:47:@1639.4]
  wire [4:0] _GEN_352; // @[issue.scala 157:47:@1639.4]
  wire [4:0] _GEN_353; // @[issue.scala 157:47:@1639.4]
  wire  _GEN_358; // @[issue.scala 165:27:@1687.6]
  wire  _GEN_359; // @[issue.scala 165:27:@1687.6]
  wire  _GEN_362; // @[issue.scala 165:27:@1689.6]
  wire  _GEN_363; // @[issue.scala 165:27:@1689.6]
  wire  _GEN_366; // @[issue.scala 165:27:@1691.6]
  wire  _GEN_367; // @[issue.scala 165:27:@1691.6]
  wire  _GEN_370; // @[issue.scala 165:27:@1693.6]
  wire  _GEN_371; // @[issue.scala 165:27:@1693.6]
  wire  _GEN_374; // @[issue.scala 165:27:@1695.6]
  wire  _GEN_375; // @[issue.scala 165:27:@1695.6]
  wire [2:0] _GEN_376; // @[issue.scala 165:27:@1696.6]
  wire [2:0] _GEN_377; // @[issue.scala 165:27:@1696.6]
  wire [2:0] _GEN_378; // @[issue.scala 165:27:@1697.6]
  wire [2:0] _GEN_379; // @[issue.scala 165:27:@1697.6]
  wire [3:0] _GEN_380; // @[issue.scala 165:27:@1698.6]
  wire [3:0] _GEN_381; // @[issue.scala 165:27:@1698.6]
  wire [1:0] _GEN_382; // @[issue.scala 165:27:@1699.6]
  wire [1:0] _GEN_383; // @[issue.scala 165:27:@1699.6]
  wire [5:0] _GEN_384; // @[issue.scala 165:27:@1700.6]
  wire [5:0] _GEN_385; // @[issue.scala 165:27:@1700.6]
  wire [25:0] _GEN_386; // @[issue.scala 165:27:@1701.6]
  wire [25:0] _GEN_387; // @[issue.scala 165:27:@1701.6]
  wire [4:0] _GEN_388; // @[issue.scala 165:27:@1702.6]
  wire [4:0] _GEN_389; // @[issue.scala 165:27:@1702.6]
  wire [4:0] _GEN_390; // @[issue.scala 165:27:@1703.6]
  wire [4:0] _GEN_391; // @[issue.scala 165:27:@1703.6]
  wire [4:0] _GEN_392; // @[issue.scala 165:27:@1704.6]
  wire [4:0] _GEN_393; // @[issue.scala 165:27:@1704.6]
  wire  _GEN_394; // @[issue.scala 166:29:@1705.6]
  wire  _GEN_395; // @[issue.scala 166:29:@1705.6]
  wire  _GEN_402; // @[issue.scala 164:18:@1663.4]
  wire  _GEN_403; // @[issue.scala 164:18:@1663.4]
  wire  _GEN_406; // @[issue.scala 164:18:@1663.4]
  wire  _GEN_407; // @[issue.scala 164:18:@1663.4]
  wire  _GEN_410; // @[issue.scala 164:18:@1663.4]
  wire  _GEN_411; // @[issue.scala 164:18:@1663.4]
  wire  _GEN_414; // @[issue.scala 164:18:@1663.4]
  wire  _GEN_415; // @[issue.scala 164:18:@1663.4]
  wire  _GEN_418; // @[issue.scala 164:18:@1663.4]
  wire  _GEN_419; // @[issue.scala 164:18:@1663.4]
  wire [2:0] _GEN_420; // @[issue.scala 164:18:@1663.4]
  wire [2:0] _GEN_421; // @[issue.scala 164:18:@1663.4]
  wire [2:0] _GEN_422; // @[issue.scala 164:18:@1663.4]
  wire [2:0] _GEN_423; // @[issue.scala 164:18:@1663.4]
  wire [3:0] _GEN_424; // @[issue.scala 164:18:@1663.4]
  wire [3:0] _GEN_425; // @[issue.scala 164:18:@1663.4]
  wire [1:0] _GEN_426; // @[issue.scala 164:18:@1663.4]
  wire [1:0] _GEN_427; // @[issue.scala 164:18:@1663.4]
  wire [5:0] _GEN_428; // @[issue.scala 164:18:@1663.4]
  wire [5:0] _GEN_429; // @[issue.scala 164:18:@1663.4]
  wire [25:0] _GEN_430; // @[issue.scala 164:18:@1663.4]
  wire [25:0] _GEN_431; // @[issue.scala 164:18:@1663.4]
  wire [4:0] _GEN_432; // @[issue.scala 164:18:@1663.4]
  wire [4:0] _GEN_433; // @[issue.scala 164:18:@1663.4]
  wire [4:0] _GEN_434; // @[issue.scala 164:18:@1663.4]
  wire [4:0] _GEN_435; // @[issue.scala 164:18:@1663.4]
  wire [4:0] _GEN_436; // @[issue.scala 164:18:@1663.4]
  wire [4:0] _GEN_437; // @[issue.scala 164:18:@1663.4]
  wire  _GEN_438; // @[issue.scala 164:18:@1663.4]
  wire  _GEN_439; // @[issue.scala 164:18:@1663.4]
  Queue Queue ( // @[issue.scala 101:106:@1393.4]
    .clock(Queue_clock),
    .reset(Queue_reset),
    .io_enq_ready(Queue_io_enq_ready),
    .io_enq_valid(Queue_io_enq_valid),
    .io_enq_bits_rd(Queue_io_enq_bits_rd),
    .io_enq_bits_rs1(Queue_io_enq_bits_rs1),
    .io_enq_bits_rs2(Queue_io_enq_bits_rs2),
    .io_enq_bits_imm(Queue_io_enq_bits_imm),
    .io_enq_bits_shift_val(Queue_io_enq_bits_shift_val),
    .io_enq_bits_shift_type(Queue_io_enq_bits_shift_type),
    .io_enq_bits_cond(Queue_io_enq_bits_cond),
    .io_enq_bits_itype(Queue_io_enq_bits_itype),
    .io_enq_bits_op(Queue_io_enq_bits_op),
    .io_enq_bits_rd_en(Queue_io_enq_bits_rd_en),
    .io_enq_bits_rs2_en(Queue_io_enq_bits_rs2_en),
    .io_enq_bits_shift_en(Queue_io_enq_bits_shift_en),
    .io_enq_bits_nzcv_en(Queue_io_enq_bits_nzcv_en),
    .io_enq_bits_tag(Queue_io_enq_bits_tag),
    .io_deq_ready(Queue_io_deq_ready),
    .io_deq_valid(Queue_io_deq_valid),
    .io_deq_bits_rd(Queue_io_deq_bits_rd),
    .io_deq_bits_rs1(Queue_io_deq_bits_rs1),
    .io_deq_bits_rs2(Queue_io_deq_bits_rs2),
    .io_deq_bits_imm(Queue_io_deq_bits_imm),
    .io_deq_bits_shift_val(Queue_io_deq_bits_shift_val),
    .io_deq_bits_shift_type(Queue_io_deq_bits_shift_type),
    .io_deq_bits_cond(Queue_io_deq_bits_cond),
    .io_deq_bits_itype(Queue_io_deq_bits_itype),
    .io_deq_bits_op(Queue_io_deq_bits_op),
    .io_deq_bits_rd_en(Queue_io_deq_bits_rd_en),
    .io_deq_bits_rs2_en(Queue_io_deq_bits_rs2_en),
    .io_deq_bits_shift_en(Queue_io_deq_bits_shift_en),
    .io_deq_bits_nzcv_en(Queue_io_deq_bits_nzcv_en),
    .io_deq_bits_tag(Queue_io_deq_bits_tag)
  );
  Queue Queue_1 ( // @[issue.scala 101:106:@1400.4]
    .clock(Queue_1_clock),
    .reset(Queue_1_reset),
    .io_enq_ready(Queue_1_io_enq_ready),
    .io_enq_valid(Queue_1_io_enq_valid),
    .io_enq_bits_rd(Queue_1_io_enq_bits_rd),
    .io_enq_bits_rs1(Queue_1_io_enq_bits_rs1),
    .io_enq_bits_rs2(Queue_1_io_enq_bits_rs2),
    .io_enq_bits_imm(Queue_1_io_enq_bits_imm),
    .io_enq_bits_shift_val(Queue_1_io_enq_bits_shift_val),
    .io_enq_bits_shift_type(Queue_1_io_enq_bits_shift_type),
    .io_enq_bits_cond(Queue_1_io_enq_bits_cond),
    .io_enq_bits_itype(Queue_1_io_enq_bits_itype),
    .io_enq_bits_op(Queue_1_io_enq_bits_op),
    .io_enq_bits_rd_en(Queue_1_io_enq_bits_rd_en),
    .io_enq_bits_rs2_en(Queue_1_io_enq_bits_rs2_en),
    .io_enq_bits_shift_en(Queue_1_io_enq_bits_shift_en),
    .io_enq_bits_nzcv_en(Queue_1_io_enq_bits_nzcv_en),
    .io_enq_bits_tag(Queue_1_io_enq_bits_tag),
    .io_deq_ready(Queue_1_io_deq_ready),
    .io_deq_valid(Queue_1_io_deq_valid),
    .io_deq_bits_rd(Queue_1_io_deq_bits_rd),
    .io_deq_bits_rs1(Queue_1_io_deq_bits_rs1),
    .io_deq_bits_rs2(Queue_1_io_deq_bits_rs2),
    .io_deq_bits_imm(Queue_1_io_deq_bits_imm),
    .io_deq_bits_shift_val(Queue_1_io_deq_bits_shift_val),
    .io_deq_bits_shift_type(Queue_1_io_deq_bits_shift_type),
    .io_deq_bits_cond(Queue_1_io_deq_bits_cond),
    .io_deq_bits_itype(Queue_1_io_deq_bits_itype),
    .io_deq_bits_op(Queue_1_io_deq_bits_op),
    .io_deq_bits_rd_en(Queue_1_io_deq_bits_rd_en),
    .io_deq_bits_rs2_en(Queue_1_io_deq_bits_rs2_en),
    .io_deq_bits_shift_en(Queue_1_io_deq_bits_shift_en),
    .io_deq_bits_nzcv_en(Queue_1_io_deq_bits_nzcv_en),
    .io_deq_bits_tag(Queue_1_io_deq_bits_tag)
  );
  RRArbiter arbiter ( // @[issue.scala 108:29:@1494.4]
    .clock(arbiter_clock),
    .reset(arbiter_reset),
    .io_ready(arbiter_io_ready),
    .io_next_valid(arbiter_io_next_valid),
    .io_next_bits(arbiter_io_next_bits)
  );
  assign _T_135 = 1'h0 == io_flushTag; // @[issue.scala 101:68:@1390.4]
  assign _T_136 = _T_135 & io_flush; // @[issue.scala 101:84:@1391.4]
  assign _T_141 = io_flushTag & io_flush; // @[issue.scala 101:84:@1398.4]
  assign fifo_vec_0_enq_ready = Queue_io_enq_ready; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1448.4]
  assign _T_405 = arbiter_io_next_valid; // @[issue.scala 157:30:@1638.4]
  assign sig_next_idx = arbiter_io_next_bits; // @[:@1500.4 :@1501.4 issue.scala 149:20:@1615.4]
  assign sig_pipe_r_0 = ~ reg_pipe_v_0; // @[issue.scala 129:24:@1550.4]
  assign _GEN_176 = 1'h0 == sig_next_idx ? 1'h1 : sig_pipe_r_0; // @[issue.scala 158:38:@1640.6]
  assign fifo_vec_0_deq_valid = Queue_io_deq_valid; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1425.4]
  assign fifo_vec_0_deq_bits_rd = Queue_io_deq_bits_rd; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1424.4]
  assign fifo_vec_0_deq_bits_rs1 = Queue_io_deq_bits_rs1; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1423.4]
  assign fifo_vec_0_deq_bits_rs2 = Queue_io_deq_bits_rs2; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1422.4]
  assign fifo_vec_0_deq_bits_imm = Queue_io_deq_bits_imm; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1421.4]
  assign fifo_vec_0_deq_bits_shift_val = Queue_io_deq_bits_shift_val; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1420.4]
  assign fifo_vec_0_deq_bits_shift_type = Queue_io_deq_bits_shift_type; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1419.4]
  assign fifo_vec_0_deq_bits_cond = Queue_io_deq_bits_cond; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1418.4]
  assign fifo_vec_0_deq_bits_itype = Queue_io_deq_bits_itype; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1417.4]
  assign fifo_vec_0_deq_bits_op = Queue_io_deq_bits_op; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1416.4]
  assign fifo_vec_0_deq_bits_rd_en = Queue_io_deq_bits_rd_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1415.4]
  assign fifo_vec_0_deq_bits_rs2_en = Queue_io_deq_bits_rs2_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1413.4]
  assign fifo_vec_0_deq_bits_shift_en = Queue_io_deq_bits_shift_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1411.4]
  assign fifo_vec_0_deq_bits_nzcv_en = Queue_io_deq_bits_nzcv_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1409.4]
  assign fifo_vec_0_deq_bits_tag = Queue_io_deq_bits_tag; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1407.4]
  assign fifo_vec_1_enq_ready = Queue_1_io_enq_ready; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1493.4]
  assign sig_pipe_r_1 = ~ reg_pipe_v_1; // @[issue.scala 129:24:@1577.4]
  assign _GEN_177 = sig_next_idx ? 1'h1 : sig_pipe_r_1; // @[issue.scala 158:38:@1640.6]
  assign fifo_vec_1_deq_valid = Queue_1_io_deq_valid; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1470.4]
  assign fifo_vec_1_deq_bits_rd = Queue_1_io_deq_bits_rd; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1469.4]
  assign fifo_vec_1_deq_bits_rs1 = Queue_1_io_deq_bits_rs1; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1468.4]
  assign fifo_vec_1_deq_bits_rs2 = Queue_1_io_deq_bits_rs2; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1467.4]
  assign fifo_vec_1_deq_bits_imm = Queue_1_io_deq_bits_imm; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1466.4]
  assign fifo_vec_1_deq_bits_shift_val = Queue_1_io_deq_bits_shift_val; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1465.4]
  assign fifo_vec_1_deq_bits_shift_type = Queue_1_io_deq_bits_shift_type; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1464.4]
  assign fifo_vec_1_deq_bits_cond = Queue_1_io_deq_bits_cond; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1463.4]
  assign fifo_vec_1_deq_bits_itype = Queue_1_io_deq_bits_itype; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1462.4]
  assign fifo_vec_1_deq_bits_op = Queue_1_io_deq_bits_op; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1461.4]
  assign fifo_vec_1_deq_bits_rd_en = Queue_1_io_deq_bits_rd_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1460.4]
  assign fifo_vec_1_deq_bits_rs2_en = Queue_1_io_deq_bits_rs2_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1458.4]
  assign fifo_vec_1_deq_bits_shift_en = Queue_1_io_deq_bits_shift_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1456.4]
  assign fifo_vec_1_deq_bits_nzcv_en = Queue_1_io_deq_bits_nzcv_en; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1454.4]
  assign fifo_vec_1_deq_bits_tag = Queue_1_io_deq_bits_tag; // @[issue.scala 100:25:@1403.4 issue.scala 100:25:@1452.4]
  assign _T_379_rd = sig_pipe_r_0 ? fifo_vec_0_deq_bits_rd : reg_pipe_0_rd; // @[issue.scala 132:27:@1553.4]
  assign _T_379_rs1 = sig_pipe_r_0 ? fifo_vec_0_deq_bits_rs1 : reg_pipe_0_rs1; // @[issue.scala 132:27:@1553.4]
  assign _T_379_rs2 = sig_pipe_r_0 ? fifo_vec_0_deq_bits_rs2 : reg_pipe_0_rs2; // @[issue.scala 132:27:@1553.4]
  assign _T_379_imm = sig_pipe_r_0 ? fifo_vec_0_deq_bits_imm : reg_pipe_0_imm; // @[issue.scala 132:27:@1553.4]
  assign _T_379_shift_val = sig_pipe_r_0 ? fifo_vec_0_deq_bits_shift_val : reg_pipe_0_shift_val; // @[issue.scala 132:27:@1553.4]
  assign _T_379_shift_type = sig_pipe_r_0 ? fifo_vec_0_deq_bits_shift_type : reg_pipe_0_shift_type; // @[issue.scala 132:27:@1553.4]
  assign _T_379_cond = sig_pipe_r_0 ? fifo_vec_0_deq_bits_cond : reg_pipe_0_cond; // @[issue.scala 132:27:@1553.4]
  assign _T_379_itype = sig_pipe_r_0 ? fifo_vec_0_deq_bits_itype : reg_pipe_0_itype; // @[issue.scala 132:27:@1553.4]
  assign _T_379_op = sig_pipe_r_0 ? fifo_vec_0_deq_bits_op : reg_pipe_0_op; // @[issue.scala 132:27:@1553.4]
  assign _T_379_rd_en = sig_pipe_r_0 ? fifo_vec_0_deq_bits_rd_en : reg_pipe_0_rd_en; // @[issue.scala 132:27:@1553.4]
  assign _T_379_rs2_en = sig_pipe_r_0 ? fifo_vec_0_deq_bits_rs2_en : reg_pipe_0_rs2_en; // @[issue.scala 132:27:@1553.4]
  assign _T_379_shift_en = sig_pipe_r_0 ? fifo_vec_0_deq_bits_shift_en : reg_pipe_0_shift_en; // @[issue.scala 132:27:@1553.4]
  assign _T_379_nzcv_en = sig_pipe_r_0 ? fifo_vec_0_deq_bits_nzcv_en : reg_pipe_0_nzcv_en; // @[issue.scala 132:27:@1553.4]
  assign _T_379_tag = sig_pipe_r_0 ? fifo_vec_0_deq_bits_tag : reg_pipe_0_tag; // @[issue.scala 132:27:@1553.4]
  assign _T_380 = sig_pipe_r_0 ? fifo_vec_0_deq_valid : reg_pipe_v_0; // @[issue.scala 133:27:@1574.4]
  assign _T_382_rd = sig_pipe_r_1 ? fifo_vec_1_deq_bits_rd : reg_pipe_1_rd; // @[issue.scala 132:27:@1580.4]
  assign _T_382_rs1 = sig_pipe_r_1 ? fifo_vec_1_deq_bits_rs1 : reg_pipe_1_rs1; // @[issue.scala 132:27:@1580.4]
  assign _T_382_rs2 = sig_pipe_r_1 ? fifo_vec_1_deq_bits_rs2 : reg_pipe_1_rs2; // @[issue.scala 132:27:@1580.4]
  assign _T_382_imm = sig_pipe_r_1 ? fifo_vec_1_deq_bits_imm : reg_pipe_1_imm; // @[issue.scala 132:27:@1580.4]
  assign _T_382_shift_val = sig_pipe_r_1 ? fifo_vec_1_deq_bits_shift_val : reg_pipe_1_shift_val; // @[issue.scala 132:27:@1580.4]
  assign _T_382_shift_type = sig_pipe_r_1 ? fifo_vec_1_deq_bits_shift_type : reg_pipe_1_shift_type; // @[issue.scala 132:27:@1580.4]
  assign _T_382_cond = sig_pipe_r_1 ? fifo_vec_1_deq_bits_cond : reg_pipe_1_cond; // @[issue.scala 132:27:@1580.4]
  assign _T_382_itype = sig_pipe_r_1 ? fifo_vec_1_deq_bits_itype : reg_pipe_1_itype; // @[issue.scala 132:27:@1580.4]
  assign _T_382_op = sig_pipe_r_1 ? fifo_vec_1_deq_bits_op : reg_pipe_1_op; // @[issue.scala 132:27:@1580.4]
  assign _T_382_rd_en = sig_pipe_r_1 ? fifo_vec_1_deq_bits_rd_en : reg_pipe_1_rd_en; // @[issue.scala 132:27:@1580.4]
  assign _T_382_rs2_en = sig_pipe_r_1 ? fifo_vec_1_deq_bits_rs2_en : reg_pipe_1_rs2_en; // @[issue.scala 132:27:@1580.4]
  assign _T_382_shift_en = sig_pipe_r_1 ? fifo_vec_1_deq_bits_shift_en : reg_pipe_1_shift_en; // @[issue.scala 132:27:@1580.4]
  assign _T_382_nzcv_en = sig_pipe_r_1 ? fifo_vec_1_deq_bits_nzcv_en : reg_pipe_1_nzcv_en; // @[issue.scala 132:27:@1580.4]
  assign _T_382_tag = sig_pipe_r_1 ? fifo_vec_1_deq_bits_tag : reg_pipe_1_tag; // @[issue.scala 132:27:@1580.4]
  assign _T_383 = sig_pipe_r_1 ? fifo_vec_1_deq_valid : reg_pipe_v_1; // @[issue.scala 133:27:@1601.4]
  assign rfile_wb_pending = io_exeReg_valid & io_exeReg_bits_rd_en; // @[issue.scala 140:42:@1604.4]
  assign _GEN_113 = io_exeReg_bits_tag ? reg_pipe_1_rs1 : reg_pipe_0_rs1; // @[issue.scala 142:40:@1605.4]
  assign _GEN_114 = io_exeReg_bits_tag ? reg_pipe_1_rs2 : reg_pipe_0_rs2; // @[issue.scala 142:40:@1605.4]
  assign _T_387 = _GEN_113 == io_exeReg_bits_rd; // @[issue.scala 142:40:@1605.4]
  assign _T_391 = _GEN_114 == io_exeReg_bits_rd; // @[issue.scala 143:42:@1606.4]
  assign _T_392 = _T_387 | _T_391; // @[issue.scala 142:63:@1607.4]
  assign exe_stall = rfile_wb_pending & _T_392; // @[issue.scala 141:33:@1608.4]
  assign _T_399 = exe_stall == 1'h0; // @[issue.scala 144:71:@1610.4]
  assign _GEN_133 = io_exeReg_bits_tag ? reg_pipe_v_1 : reg_pipe_v_0; // @[issue.scala 144:68:@1611.4]
  assign _T_400 = _GEN_133 & _T_399; // @[issue.scala 144:68:@1611.4]
  assign _GEN_134 = 1'h0 == io_exeReg_bits_tag ? _T_400 : reg_pipe_v_0; // @[issue.scala 144:34:@1612.4]
  assign _GEN_135 = io_exeReg_bits_tag ? _T_400 : reg_pipe_v_1; // @[issue.scala 144:34:@1612.4]
  assign _GEN_397 = io_flushTag ? 1'h0 : _GEN_135; // @[issue.scala 167:29:@1706.6]
  assign sig_pipe_i_1 = io_flush ? _GEN_397 : _GEN_135; // @[issue.scala 164:18:@1663.4]
  assign _GEN_396 = 1'h0 == io_flushTag ? 1'h0 : _GEN_134; // @[issue.scala 167:29:@1706.6]
  assign sig_pipe_i_0 = io_flush ? _GEN_396 : _GEN_134; // @[issue.scala 164:18:@1663.4]
  assign _GEN_248 = sig_next_idx ? fifo_vec_1_deq_valid : fifo_vec_0_deq_valid; // @[issue.scala 159:30:@1641.6]
  assign _GEN_178 = 1'h0 == sig_next_idx ? _GEN_248 : _T_380; // @[issue.scala 159:30:@1641.6]
  assign _GEN_179 = sig_next_idx ? _GEN_248 : _T_383; // @[issue.scala 159:30:@1641.6]
  assign _GEN_249 = sig_next_idx ? fifo_vec_1_deq_bits_rd : fifo_vec_0_deq_bits_rd; // @[issue.scala 159:30:@1641.6]
  assign _GEN_250 = sig_next_idx ? fifo_vec_1_deq_bits_rs1 : fifo_vec_0_deq_bits_rs1; // @[issue.scala 159:30:@1641.6]
  assign _GEN_251 = sig_next_idx ? fifo_vec_1_deq_bits_rs2 : fifo_vec_0_deq_bits_rs2; // @[issue.scala 159:30:@1641.6]
  assign _GEN_252 = sig_next_idx ? fifo_vec_1_deq_bits_imm : fifo_vec_0_deq_bits_imm; // @[issue.scala 159:30:@1641.6]
  assign _GEN_253 = sig_next_idx ? fifo_vec_1_deq_bits_shift_val : fifo_vec_0_deq_bits_shift_val; // @[issue.scala 159:30:@1641.6]
  assign _GEN_254 = sig_next_idx ? fifo_vec_1_deq_bits_shift_type : fifo_vec_0_deq_bits_shift_type; // @[issue.scala 159:30:@1641.6]
  assign _GEN_255 = sig_next_idx ? fifo_vec_1_deq_bits_cond : fifo_vec_0_deq_bits_cond; // @[issue.scala 159:30:@1641.6]
  assign _GEN_256 = sig_next_idx ? fifo_vec_1_deq_bits_itype : fifo_vec_0_deq_bits_itype; // @[issue.scala 159:30:@1641.6]
  assign _GEN_257 = sig_next_idx ? fifo_vec_1_deq_bits_op : fifo_vec_0_deq_bits_op; // @[issue.scala 159:30:@1641.6]
  assign _GEN_258 = sig_next_idx ? fifo_vec_1_deq_bits_rd_en : fifo_vec_0_deq_bits_rd_en; // @[issue.scala 159:30:@1641.6]
  assign _GEN_260 = sig_next_idx ? fifo_vec_1_deq_bits_rs2_en : fifo_vec_0_deq_bits_rs2_en; // @[issue.scala 159:30:@1641.6]
  assign _GEN_262 = sig_next_idx ? fifo_vec_1_deq_bits_shift_en : fifo_vec_0_deq_bits_shift_en; // @[issue.scala 159:30:@1641.6]
  assign _GEN_264 = sig_next_idx ? fifo_vec_1_deq_bits_nzcv_en : fifo_vec_0_deq_bits_nzcv_en; // @[issue.scala 159:30:@1641.6]
  assign _GEN_266 = sig_next_idx ? fifo_vec_1_deq_bits_tag : fifo_vec_0_deq_bits_tag; // @[issue.scala 159:30:@1641.6]
  assign _GEN_274 = 1'h0 == sig_next_idx ? _GEN_266 : _T_379_tag; // @[issue.scala 160:28:@1644.6]
  assign _GEN_275 = sig_next_idx ? _GEN_266 : _T_382_tag; // @[issue.scala 160:28:@1644.6]
  assign _GEN_278 = 1'h0 == sig_next_idx ? _GEN_264 : _T_379_nzcv_en; // @[issue.scala 160:28:@1646.6]
  assign _GEN_279 = sig_next_idx ? _GEN_264 : _T_382_nzcv_en; // @[issue.scala 160:28:@1646.6]
  assign _GEN_282 = 1'h0 == sig_next_idx ? _GEN_262 : _T_379_shift_en; // @[issue.scala 160:28:@1648.6]
  assign _GEN_283 = sig_next_idx ? _GEN_262 : _T_382_shift_en; // @[issue.scala 160:28:@1648.6]
  assign _GEN_286 = 1'h0 == sig_next_idx ? _GEN_260 : _T_379_rs2_en; // @[issue.scala 160:28:@1650.6]
  assign _GEN_287 = sig_next_idx ? _GEN_260 : _T_382_rs2_en; // @[issue.scala 160:28:@1650.6]
  assign _GEN_290 = 1'h0 == sig_next_idx ? _GEN_258 : _T_379_rd_en; // @[issue.scala 160:28:@1652.6]
  assign _GEN_291 = sig_next_idx ? _GEN_258 : _T_382_rd_en; // @[issue.scala 160:28:@1652.6]
  assign _GEN_292 = 1'h0 == sig_next_idx ? _GEN_257 : _T_379_op; // @[issue.scala 160:28:@1653.6]
  assign _GEN_293 = sig_next_idx ? _GEN_257 : _T_382_op; // @[issue.scala 160:28:@1653.6]
  assign _GEN_294 = 1'h0 == sig_next_idx ? _GEN_256 : _T_379_itype; // @[issue.scala 160:28:@1654.6]
  assign _GEN_295 = sig_next_idx ? _GEN_256 : _T_382_itype; // @[issue.scala 160:28:@1654.6]
  assign _GEN_296 = 1'h0 == sig_next_idx ? _GEN_255 : _T_379_cond; // @[issue.scala 160:28:@1655.6]
  assign _GEN_297 = sig_next_idx ? _GEN_255 : _T_382_cond; // @[issue.scala 160:28:@1655.6]
  assign _GEN_298 = 1'h0 == sig_next_idx ? _GEN_254 : _T_379_shift_type; // @[issue.scala 160:28:@1656.6]
  assign _GEN_299 = sig_next_idx ? _GEN_254 : _T_382_shift_type; // @[issue.scala 160:28:@1656.6]
  assign _GEN_300 = 1'h0 == sig_next_idx ? _GEN_253 : _T_379_shift_val; // @[issue.scala 160:28:@1657.6]
  assign _GEN_301 = sig_next_idx ? _GEN_253 : _T_382_shift_val; // @[issue.scala 160:28:@1657.6]
  assign _GEN_302 = 1'h0 == sig_next_idx ? _GEN_252 : _T_379_imm; // @[issue.scala 160:28:@1658.6]
  assign _GEN_303 = sig_next_idx ? _GEN_252 : _T_382_imm; // @[issue.scala 160:28:@1658.6]
  assign _GEN_304 = 1'h0 == sig_next_idx ? _GEN_251 : _T_379_rs2; // @[issue.scala 160:28:@1659.6]
  assign _GEN_305 = sig_next_idx ? _GEN_251 : _T_382_rs2; // @[issue.scala 160:28:@1659.6]
  assign _GEN_306 = 1'h0 == sig_next_idx ? _GEN_250 : _T_379_rs1; // @[issue.scala 160:28:@1660.6]
  assign _GEN_307 = sig_next_idx ? _GEN_250 : _T_382_rs1; // @[issue.scala 160:28:@1660.6]
  assign _GEN_308 = 1'h0 == sig_next_idx ? _GEN_249 : _T_379_rd; // @[issue.scala 160:28:@1661.6]
  assign _GEN_309 = sig_next_idx ? _GEN_249 : _T_382_rd; // @[issue.scala 160:28:@1661.6]
  assign _GEN_312 = _T_405 ? _GEN_178 : _T_380; // @[issue.scala 157:47:@1639.4]
  assign _GEN_313 = _T_405 ? _GEN_179 : _T_383; // @[issue.scala 157:47:@1639.4]
  assign _GEN_318 = _T_405 ? _GEN_274 : _T_379_tag; // @[issue.scala 157:47:@1639.4]
  assign _GEN_319 = _T_405 ? _GEN_275 : _T_382_tag; // @[issue.scala 157:47:@1639.4]
  assign _GEN_322 = _T_405 ? _GEN_278 : _T_379_nzcv_en; // @[issue.scala 157:47:@1639.4]
  assign _GEN_323 = _T_405 ? _GEN_279 : _T_382_nzcv_en; // @[issue.scala 157:47:@1639.4]
  assign _GEN_326 = _T_405 ? _GEN_282 : _T_379_shift_en; // @[issue.scala 157:47:@1639.4]
  assign _GEN_327 = _T_405 ? _GEN_283 : _T_382_shift_en; // @[issue.scala 157:47:@1639.4]
  assign _GEN_330 = _T_405 ? _GEN_286 : _T_379_rs2_en; // @[issue.scala 157:47:@1639.4]
  assign _GEN_331 = _T_405 ? _GEN_287 : _T_382_rs2_en; // @[issue.scala 157:47:@1639.4]
  assign _GEN_334 = _T_405 ? _GEN_290 : _T_379_rd_en; // @[issue.scala 157:47:@1639.4]
  assign _GEN_335 = _T_405 ? _GEN_291 : _T_382_rd_en; // @[issue.scala 157:47:@1639.4]
  assign _GEN_336 = _T_405 ? _GEN_292 : _T_379_op; // @[issue.scala 157:47:@1639.4]
  assign _GEN_337 = _T_405 ? _GEN_293 : _T_382_op; // @[issue.scala 157:47:@1639.4]
  assign _GEN_338 = _T_405 ? _GEN_294 : _T_379_itype; // @[issue.scala 157:47:@1639.4]
  assign _GEN_339 = _T_405 ? _GEN_295 : _T_382_itype; // @[issue.scala 157:47:@1639.4]
  assign _GEN_340 = _T_405 ? _GEN_296 : _T_379_cond; // @[issue.scala 157:47:@1639.4]
  assign _GEN_341 = _T_405 ? _GEN_297 : _T_382_cond; // @[issue.scala 157:47:@1639.4]
  assign _GEN_342 = _T_405 ? _GEN_298 : _T_379_shift_type; // @[issue.scala 157:47:@1639.4]
  assign _GEN_343 = _T_405 ? _GEN_299 : _T_382_shift_type; // @[issue.scala 157:47:@1639.4]
  assign _GEN_344 = _T_405 ? _GEN_300 : _T_379_shift_val; // @[issue.scala 157:47:@1639.4]
  assign _GEN_345 = _T_405 ? _GEN_301 : _T_382_shift_val; // @[issue.scala 157:47:@1639.4]
  assign _GEN_346 = _T_405 ? _GEN_302 : _T_379_imm; // @[issue.scala 157:47:@1639.4]
  assign _GEN_347 = _T_405 ? _GEN_303 : _T_382_imm; // @[issue.scala 157:47:@1639.4]
  assign _GEN_348 = _T_405 ? _GEN_304 : _T_379_rs2; // @[issue.scala 157:47:@1639.4]
  assign _GEN_349 = _T_405 ? _GEN_305 : _T_382_rs2; // @[issue.scala 157:47:@1639.4]
  assign _GEN_350 = _T_405 ? _GEN_306 : _T_379_rs1; // @[issue.scala 157:47:@1639.4]
  assign _GEN_351 = _T_405 ? _GEN_307 : _T_382_rs1; // @[issue.scala 157:47:@1639.4]
  assign _GEN_352 = _T_405 ? _GEN_308 : _T_379_rd; // @[issue.scala 157:47:@1639.4]
  assign _GEN_353 = _T_405 ? _GEN_309 : _T_382_rd; // @[issue.scala 157:47:@1639.4]
  assign _GEN_358 = 1'h0 == io_flushTag ? 1'h0 : _GEN_318; // @[issue.scala 165:27:@1687.6]
  assign _GEN_359 = io_flushTag ? 1'h0 : _GEN_319; // @[issue.scala 165:27:@1687.6]
  assign _GEN_362 = 1'h0 == io_flushTag ? 1'h0 : _GEN_322; // @[issue.scala 165:27:@1689.6]
  assign _GEN_363 = io_flushTag ? 1'h0 : _GEN_323; // @[issue.scala 165:27:@1689.6]
  assign _GEN_366 = 1'h0 == io_flushTag ? 1'h0 : _GEN_326; // @[issue.scala 165:27:@1691.6]
  assign _GEN_367 = io_flushTag ? 1'h0 : _GEN_327; // @[issue.scala 165:27:@1691.6]
  assign _GEN_370 = 1'h0 == io_flushTag ? 1'h0 : _GEN_330; // @[issue.scala 165:27:@1693.6]
  assign _GEN_371 = io_flushTag ? 1'h0 : _GEN_331; // @[issue.scala 165:27:@1693.6]
  assign _GEN_374 = 1'h0 == io_flushTag ? 1'h0 : _GEN_334; // @[issue.scala 165:27:@1695.6]
  assign _GEN_375 = io_flushTag ? 1'h0 : _GEN_335; // @[issue.scala 165:27:@1695.6]
  assign _GEN_376 = 1'h0 == io_flushTag ? 3'h0 : _GEN_336; // @[issue.scala 165:27:@1696.6]
  assign _GEN_377 = io_flushTag ? 3'h0 : _GEN_337; // @[issue.scala 165:27:@1696.6]
  assign _GEN_378 = 1'h0 == io_flushTag ? 3'h0 : _GEN_338; // @[issue.scala 165:27:@1697.6]
  assign _GEN_379 = io_flushTag ? 3'h0 : _GEN_339; // @[issue.scala 165:27:@1697.6]
  assign _GEN_380 = 1'h0 == io_flushTag ? 4'h0 : _GEN_340; // @[issue.scala 165:27:@1698.6]
  assign _GEN_381 = io_flushTag ? 4'h0 : _GEN_341; // @[issue.scala 165:27:@1698.6]
  assign _GEN_382 = 1'h0 == io_flushTag ? 2'h0 : _GEN_342; // @[issue.scala 165:27:@1699.6]
  assign _GEN_383 = io_flushTag ? 2'h0 : _GEN_343; // @[issue.scala 165:27:@1699.6]
  assign _GEN_384 = 1'h0 == io_flushTag ? 6'h0 : _GEN_344; // @[issue.scala 165:27:@1700.6]
  assign _GEN_385 = io_flushTag ? 6'h0 : _GEN_345; // @[issue.scala 165:27:@1700.6]
  assign _GEN_386 = 1'h0 == io_flushTag ? 26'h0 : _GEN_346; // @[issue.scala 165:27:@1701.6]
  assign _GEN_387 = io_flushTag ? 26'h0 : _GEN_347; // @[issue.scala 165:27:@1701.6]
  assign _GEN_388 = 1'h0 == io_flushTag ? 5'h0 : _GEN_348; // @[issue.scala 165:27:@1702.6]
  assign _GEN_389 = io_flushTag ? 5'h0 : _GEN_349; // @[issue.scala 165:27:@1702.6]
  assign _GEN_390 = 1'h0 == io_flushTag ? 5'h0 : _GEN_350; // @[issue.scala 165:27:@1703.6]
  assign _GEN_391 = io_flushTag ? 5'h0 : _GEN_351; // @[issue.scala 165:27:@1703.6]
  assign _GEN_392 = 1'h0 == io_flushTag ? 5'h0 : _GEN_352; // @[issue.scala 165:27:@1704.6]
  assign _GEN_393 = io_flushTag ? 5'h0 : _GEN_353; // @[issue.scala 165:27:@1704.6]
  assign _GEN_394 = 1'h0 == io_flushTag ? 1'h0 : _GEN_312; // @[issue.scala 166:29:@1705.6]
  assign _GEN_395 = io_flushTag ? 1'h0 : _GEN_313; // @[issue.scala 166:29:@1705.6]
  assign _GEN_402 = io_flush ? _GEN_358 : _GEN_318; // @[issue.scala 164:18:@1663.4]
  assign _GEN_403 = io_flush ? _GEN_359 : _GEN_319; // @[issue.scala 164:18:@1663.4]
  assign _GEN_406 = io_flush ? _GEN_362 : _GEN_322; // @[issue.scala 164:18:@1663.4]
  assign _GEN_407 = io_flush ? _GEN_363 : _GEN_323; // @[issue.scala 164:18:@1663.4]
  assign _GEN_410 = io_flush ? _GEN_366 : _GEN_326; // @[issue.scala 164:18:@1663.4]
  assign _GEN_411 = io_flush ? _GEN_367 : _GEN_327; // @[issue.scala 164:18:@1663.4]
  assign _GEN_414 = io_flush ? _GEN_370 : _GEN_330; // @[issue.scala 164:18:@1663.4]
  assign _GEN_415 = io_flush ? _GEN_371 : _GEN_331; // @[issue.scala 164:18:@1663.4]
  assign _GEN_418 = io_flush ? _GEN_374 : _GEN_334; // @[issue.scala 164:18:@1663.4]
  assign _GEN_419 = io_flush ? _GEN_375 : _GEN_335; // @[issue.scala 164:18:@1663.4]
  assign _GEN_420 = io_flush ? _GEN_376 : _GEN_336; // @[issue.scala 164:18:@1663.4]
  assign _GEN_421 = io_flush ? _GEN_377 : _GEN_337; // @[issue.scala 164:18:@1663.4]
  assign _GEN_422 = io_flush ? _GEN_378 : _GEN_338; // @[issue.scala 164:18:@1663.4]
  assign _GEN_423 = io_flush ? _GEN_379 : _GEN_339; // @[issue.scala 164:18:@1663.4]
  assign _GEN_424 = io_flush ? _GEN_380 : _GEN_340; // @[issue.scala 164:18:@1663.4]
  assign _GEN_425 = io_flush ? _GEN_381 : _GEN_341; // @[issue.scala 164:18:@1663.4]
  assign _GEN_426 = io_flush ? _GEN_382 : _GEN_342; // @[issue.scala 164:18:@1663.4]
  assign _GEN_427 = io_flush ? _GEN_383 : _GEN_343; // @[issue.scala 164:18:@1663.4]
  assign _GEN_428 = io_flush ? _GEN_384 : _GEN_344; // @[issue.scala 164:18:@1663.4]
  assign _GEN_429 = io_flush ? _GEN_385 : _GEN_345; // @[issue.scala 164:18:@1663.4]
  assign _GEN_430 = io_flush ? _GEN_386 : _GEN_346; // @[issue.scala 164:18:@1663.4]
  assign _GEN_431 = io_flush ? _GEN_387 : _GEN_347; // @[issue.scala 164:18:@1663.4]
  assign _GEN_432 = io_flush ? _GEN_388 : _GEN_348; // @[issue.scala 164:18:@1663.4]
  assign _GEN_433 = io_flush ? _GEN_389 : _GEN_349; // @[issue.scala 164:18:@1663.4]
  assign _GEN_434 = io_flush ? _GEN_390 : _GEN_350; // @[issue.scala 164:18:@1663.4]
  assign _GEN_435 = io_flush ? _GEN_391 : _GEN_351; // @[issue.scala 164:18:@1663.4]
  assign _GEN_436 = io_flush ? _GEN_392 : _GEN_352; // @[issue.scala 164:18:@1663.4]
  assign _GEN_437 = io_flush ? _GEN_393 : _GEN_353; // @[issue.scala 164:18:@1663.4]
  assign _GEN_438 = io_flush ? _GEN_394 : _GEN_312; // @[issue.scala 164:18:@1663.4]
  assign _GEN_439 = io_flush ? _GEN_395 : _GEN_313; // @[issue.scala 164:18:@1663.4]
  assign io_enq_ready = io_enq_bits_tag ? fifo_vec_1_enq_ready : fifo_vec_0_enq_ready; // @[issue.scala 123:16:@1548.4]
  assign io_deq_valid = arbiter_io_next_valid; // @[issue.scala 152:16:@1636.4]
  assign io_deq_bits_rd = sig_next_idx ? reg_pipe_1_rd : reg_pipe_0_rd; // @[issue.scala 151:16:@1635.4]
  assign io_deq_bits_rs1 = sig_next_idx ? reg_pipe_1_rs1 : reg_pipe_0_rs1; // @[issue.scala 151:16:@1634.4]
  assign io_deq_bits_rs2 = sig_next_idx ? reg_pipe_1_rs2 : reg_pipe_0_rs2; // @[issue.scala 151:16:@1633.4]
  assign io_deq_bits_imm = sig_next_idx ? reg_pipe_1_imm : reg_pipe_0_imm; // @[issue.scala 151:16:@1632.4]
  assign io_deq_bits_shift_val = sig_next_idx ? reg_pipe_1_shift_val : reg_pipe_0_shift_val; // @[issue.scala 151:16:@1631.4]
  assign io_deq_bits_shift_type = sig_next_idx ? reg_pipe_1_shift_type : reg_pipe_0_shift_type; // @[issue.scala 151:16:@1630.4]
  assign io_deq_bits_cond = sig_next_idx ? reg_pipe_1_cond : reg_pipe_0_cond; // @[issue.scala 151:16:@1629.4]
  assign io_deq_bits_itype = sig_next_idx ? reg_pipe_1_itype : reg_pipe_0_itype; // @[issue.scala 151:16:@1628.4]
  assign io_deq_bits_op = sig_next_idx ? reg_pipe_1_op : reg_pipe_0_op; // @[issue.scala 151:16:@1627.4]
  assign io_deq_bits_rd_en = sig_next_idx ? reg_pipe_1_rd_en : reg_pipe_0_rd_en; // @[issue.scala 151:16:@1626.4]
  assign io_deq_bits_rs2_en = sig_next_idx ? reg_pipe_1_rs2_en : reg_pipe_0_rs2_en; // @[issue.scala 151:16:@1624.4]
  assign io_deq_bits_shift_en = sig_next_idx ? reg_pipe_1_shift_en : reg_pipe_0_shift_en; // @[issue.scala 151:16:@1622.4]
  assign io_deq_bits_nzcv_en = sig_next_idx ? reg_pipe_1_nzcv_en : reg_pipe_0_nzcv_en; // @[issue.scala 151:16:@1620.4]
  assign io_deq_bits_tag = sig_next_idx ? reg_pipe_1_tag : reg_pipe_0_tag; // @[issue.scala 151:16:@1618.4]
  assign Queue_clock = clock; // @[:@1394.4]
  assign Queue_reset = reset | _T_136; // @[:@1395.4]
  assign Queue_io_enq_valid = 1'h0 == io_enq_bits_tag ? io_enq_valid : 1'h0; // @[issue.scala 100:25:@1447.4]
  assign Queue_io_enq_bits_rd = io_enq_bits_rd; // @[issue.scala 100:25:@1446.4]
  assign Queue_io_enq_bits_rs1 = io_enq_bits_rs1; // @[issue.scala 100:25:@1445.4]
  assign Queue_io_enq_bits_rs2 = io_enq_bits_rs2; // @[issue.scala 100:25:@1444.4]
  assign Queue_io_enq_bits_imm = io_enq_bits_imm; // @[issue.scala 100:25:@1443.4]
  assign Queue_io_enq_bits_shift_val = io_enq_bits_shift_val; // @[issue.scala 100:25:@1442.4]
  assign Queue_io_enq_bits_shift_type = io_enq_bits_shift_type; // @[issue.scala 100:25:@1441.4]
  assign Queue_io_enq_bits_cond = io_enq_bits_cond; // @[issue.scala 100:25:@1440.4]
  assign Queue_io_enq_bits_itype = io_enq_bits_itype; // @[issue.scala 100:25:@1439.4]
  assign Queue_io_enq_bits_op = io_enq_bits_op; // @[issue.scala 100:25:@1438.4]
  assign Queue_io_enq_bits_rd_en = io_enq_bits_rd_en; // @[issue.scala 100:25:@1437.4]
  assign Queue_io_enq_bits_rs2_en = io_enq_bits_rs2_en; // @[issue.scala 100:25:@1435.4]
  assign Queue_io_enq_bits_shift_en = io_enq_bits_shift_en; // @[issue.scala 100:25:@1433.4]
  assign Queue_io_enq_bits_nzcv_en = io_enq_bits_nzcv_en; // @[issue.scala 100:25:@1431.4]
  assign Queue_io_enq_bits_tag = io_enq_bits_tag; // @[issue.scala 100:25:@1429.4]
  assign Queue_io_deq_ready = _T_405 ? _GEN_176 : sig_pipe_r_0; // @[issue.scala 100:25:@1426.4]
  assign Queue_1_clock = clock; // @[:@1401.4]
  assign Queue_1_reset = reset | _T_141; // @[:@1402.4]
  assign Queue_1_io_enq_valid = io_enq_bits_tag ? io_enq_valid : 1'h0; // @[issue.scala 100:25:@1492.4]
  assign Queue_1_io_enq_bits_rd = io_enq_bits_rd; // @[issue.scala 100:25:@1491.4]
  assign Queue_1_io_enq_bits_rs1 = io_enq_bits_rs1; // @[issue.scala 100:25:@1490.4]
  assign Queue_1_io_enq_bits_rs2 = io_enq_bits_rs2; // @[issue.scala 100:25:@1489.4]
  assign Queue_1_io_enq_bits_imm = io_enq_bits_imm; // @[issue.scala 100:25:@1488.4]
  assign Queue_1_io_enq_bits_shift_val = io_enq_bits_shift_val; // @[issue.scala 100:25:@1487.4]
  assign Queue_1_io_enq_bits_shift_type = io_enq_bits_shift_type; // @[issue.scala 100:25:@1486.4]
  assign Queue_1_io_enq_bits_cond = io_enq_bits_cond; // @[issue.scala 100:25:@1485.4]
  assign Queue_1_io_enq_bits_itype = io_enq_bits_itype; // @[issue.scala 100:25:@1484.4]
  assign Queue_1_io_enq_bits_op = io_enq_bits_op; // @[issue.scala 100:25:@1483.4]
  assign Queue_1_io_enq_bits_rd_en = io_enq_bits_rd_en; // @[issue.scala 100:25:@1482.4]
  assign Queue_1_io_enq_bits_rs2_en = io_enq_bits_rs2_en; // @[issue.scala 100:25:@1480.4]
  assign Queue_1_io_enq_bits_shift_en = io_enq_bits_shift_en; // @[issue.scala 100:25:@1478.4]
  assign Queue_1_io_enq_bits_nzcv_en = io_enq_bits_nzcv_en; // @[issue.scala 100:25:@1476.4]
  assign Queue_1_io_enq_bits_tag = io_enq_bits_tag; // @[issue.scala 100:25:@1474.4]
  assign Queue_1_io_deq_ready = _T_405 ? _GEN_177 : sig_pipe_r_1; // @[issue.scala 100:25:@1471.4]
  assign arbiter_clock = clock; // @[:@1495.4]
  assign arbiter_reset = reset; // @[:@1496.4]
  assign arbiter_io_ready = {sig_pipe_i_1,sig_pipe_i_0}; // @[issue.scala 148:20:@1614.4]
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
  `ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  reg_pipe_0_rd = _RAND_0[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  reg_pipe_0_rs1 = _RAND_1[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  reg_pipe_0_rs2 = _RAND_2[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  reg_pipe_0_imm = _RAND_3[25:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  reg_pipe_0_shift_val = _RAND_4[5:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  reg_pipe_0_shift_type = _RAND_5[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  reg_pipe_0_cond = _RAND_6[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  reg_pipe_0_itype = _RAND_7[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  reg_pipe_0_op = _RAND_8[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  reg_pipe_0_rd_en = _RAND_9[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  reg_pipe_0_rs2_en = _RAND_10[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {1{`RANDOM}};
  reg_pipe_0_shift_en = _RAND_11[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_12 = {1{`RANDOM}};
  reg_pipe_0_nzcv_en = _RAND_12[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_13 = {1{`RANDOM}};
  reg_pipe_0_tag = _RAND_13[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_14 = {1{`RANDOM}};
  reg_pipe_1_rd = _RAND_14[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_15 = {1{`RANDOM}};
  reg_pipe_1_rs1 = _RAND_15[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_16 = {1{`RANDOM}};
  reg_pipe_1_rs2 = _RAND_16[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_17 = {1{`RANDOM}};
  reg_pipe_1_imm = _RAND_17[25:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_18 = {1{`RANDOM}};
  reg_pipe_1_shift_val = _RAND_18[5:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_19 = {1{`RANDOM}};
  reg_pipe_1_shift_type = _RAND_19[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_20 = {1{`RANDOM}};
  reg_pipe_1_cond = _RAND_20[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_21 = {1{`RANDOM}};
  reg_pipe_1_itype = _RAND_21[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_22 = {1{`RANDOM}};
  reg_pipe_1_op = _RAND_22[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_23 = {1{`RANDOM}};
  reg_pipe_1_rd_en = _RAND_23[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_24 = {1{`RANDOM}};
  reg_pipe_1_rs2_en = _RAND_24[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_25 = {1{`RANDOM}};
  reg_pipe_1_shift_en = _RAND_25[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_26 = {1{`RANDOM}};
  reg_pipe_1_nzcv_en = _RAND_26[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_27 = {1{`RANDOM}};
  reg_pipe_1_tag = _RAND_27[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_28 = {1{`RANDOM}};
  reg_pipe_v_0 = _RAND_28[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_29 = {1{`RANDOM}};
  reg_pipe_v_1 = _RAND_29[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (reset) begin
      reg_pipe_0_rd <= 5'h0;
    end else begin
      if (io_flush) begin
        if (1'h0 == io_flushTag) begin
          reg_pipe_0_rd <= 5'h0;
        end else begin
          if (_T_405) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_rd <= fifo_vec_1_deq_bits_rd;
              end else begin
                reg_pipe_0_rd <= fifo_vec_0_deq_bits_rd;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_rd <= fifo_vec_0_deq_bits_rd;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rd <= fifo_vec_0_deq_bits_rd;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_rd <= fifo_vec_1_deq_bits_rd;
            end else begin
              reg_pipe_0_rd <= fifo_vec_0_deq_bits_rd;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rd <= fifo_vec_0_deq_bits_rd;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_rd <= fifo_vec_0_deq_bits_rd;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_rs1 <= 5'h0;
    end else begin
      if (io_flush) begin
        if (1'h0 == io_flushTag) begin
          reg_pipe_0_rs1 <= 5'h0;
        end else begin
          if (_T_405) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_rs1 <= fifo_vec_1_deq_bits_rs1;
              end else begin
                reg_pipe_0_rs1 <= fifo_vec_0_deq_bits_rs1;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_rs1 <= fifo_vec_0_deq_bits_rs1;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rs1 <= fifo_vec_0_deq_bits_rs1;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_rs1 <= fifo_vec_1_deq_bits_rs1;
            end else begin
              reg_pipe_0_rs1 <= fifo_vec_0_deq_bits_rs1;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rs1 <= fifo_vec_0_deq_bits_rs1;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_rs1 <= fifo_vec_0_deq_bits_rs1;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_rs2 <= 5'h0;
    end else begin
      if (io_flush) begin
        if (1'h0 == io_flushTag) begin
          reg_pipe_0_rs2 <= 5'h0;
        end else begin
          if (_T_405) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_rs2 <= fifo_vec_1_deq_bits_rs2;
              end else begin
                reg_pipe_0_rs2 <= fifo_vec_0_deq_bits_rs2;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_rs2 <= fifo_vec_0_deq_bits_rs2;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rs2 <= fifo_vec_0_deq_bits_rs2;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_rs2 <= fifo_vec_1_deq_bits_rs2;
            end else begin
              reg_pipe_0_rs2 <= fifo_vec_0_deq_bits_rs2;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rs2 <= fifo_vec_0_deq_bits_rs2;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_rs2 <= fifo_vec_0_deq_bits_rs2;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_imm <= 26'h0;
    end else begin
      if (io_flush) begin
        if (1'h0 == io_flushTag) begin
          reg_pipe_0_imm <= 26'h0;
        end else begin
          if (_T_405) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_imm <= fifo_vec_1_deq_bits_imm;
              end else begin
                reg_pipe_0_imm <= fifo_vec_0_deq_bits_imm;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_imm <= fifo_vec_0_deq_bits_imm;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_imm <= fifo_vec_0_deq_bits_imm;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_imm <= fifo_vec_1_deq_bits_imm;
            end else begin
              reg_pipe_0_imm <= fifo_vec_0_deq_bits_imm;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_imm <= fifo_vec_0_deq_bits_imm;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_imm <= fifo_vec_0_deq_bits_imm;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_shift_val <= 6'h0;
    end else begin
      if (io_flush) begin
        if (1'h0 == io_flushTag) begin
          reg_pipe_0_shift_val <= 6'h0;
        end else begin
          if (_T_405) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_shift_val <= fifo_vec_1_deq_bits_shift_val;
              end else begin
                reg_pipe_0_shift_val <= fifo_vec_0_deq_bits_shift_val;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_shift_val <= fifo_vec_0_deq_bits_shift_val;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_shift_val <= fifo_vec_0_deq_bits_shift_val;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_shift_val <= fifo_vec_1_deq_bits_shift_val;
            end else begin
              reg_pipe_0_shift_val <= fifo_vec_0_deq_bits_shift_val;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_shift_val <= fifo_vec_0_deq_bits_shift_val;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_shift_val <= fifo_vec_0_deq_bits_shift_val;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_shift_type <= 2'h0;
    end else begin
      if (io_flush) begin
        if (1'h0 == io_flushTag) begin
          reg_pipe_0_shift_type <= 2'h0;
        end else begin
          if (_T_405) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_shift_type <= fifo_vec_1_deq_bits_shift_type;
              end else begin
                reg_pipe_0_shift_type <= fifo_vec_0_deq_bits_shift_type;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_shift_type <= fifo_vec_0_deq_bits_shift_type;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_shift_type <= fifo_vec_0_deq_bits_shift_type;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_shift_type <= fifo_vec_1_deq_bits_shift_type;
            end else begin
              reg_pipe_0_shift_type <= fifo_vec_0_deq_bits_shift_type;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_shift_type <= fifo_vec_0_deq_bits_shift_type;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_shift_type <= fifo_vec_0_deq_bits_shift_type;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_cond <= 4'h0;
    end else begin
      if (io_flush) begin
        if (1'h0 == io_flushTag) begin
          reg_pipe_0_cond <= 4'h0;
        end else begin
          if (_T_405) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_cond <= fifo_vec_1_deq_bits_cond;
              end else begin
                reg_pipe_0_cond <= fifo_vec_0_deq_bits_cond;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_cond <= fifo_vec_0_deq_bits_cond;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_cond <= fifo_vec_0_deq_bits_cond;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_cond <= fifo_vec_1_deq_bits_cond;
            end else begin
              reg_pipe_0_cond <= fifo_vec_0_deq_bits_cond;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_cond <= fifo_vec_0_deq_bits_cond;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_cond <= fifo_vec_0_deq_bits_cond;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_itype <= 3'h0;
    end else begin
      if (io_flush) begin
        if (1'h0 == io_flushTag) begin
          reg_pipe_0_itype <= 3'h0;
        end else begin
          if (_T_405) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_itype <= fifo_vec_1_deq_bits_itype;
              end else begin
                reg_pipe_0_itype <= fifo_vec_0_deq_bits_itype;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_itype <= fifo_vec_0_deq_bits_itype;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_itype <= fifo_vec_0_deq_bits_itype;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_itype <= fifo_vec_1_deq_bits_itype;
            end else begin
              reg_pipe_0_itype <= fifo_vec_0_deq_bits_itype;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_itype <= fifo_vec_0_deq_bits_itype;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_itype <= fifo_vec_0_deq_bits_itype;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_op <= 3'h0;
    end else begin
      if (io_flush) begin
        if (1'h0 == io_flushTag) begin
          reg_pipe_0_op <= 3'h0;
        end else begin
          if (_T_405) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_op <= fifo_vec_1_deq_bits_op;
              end else begin
                reg_pipe_0_op <= fifo_vec_0_deq_bits_op;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_op <= fifo_vec_0_deq_bits_op;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_op <= fifo_vec_0_deq_bits_op;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_op <= fifo_vec_1_deq_bits_op;
            end else begin
              reg_pipe_0_op <= fifo_vec_0_deq_bits_op;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_op <= fifo_vec_0_deq_bits_op;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_op <= fifo_vec_0_deq_bits_op;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_rd_en <= 1'h0;
    end else begin
      if (io_flush) begin
        if (1'h0 == io_flushTag) begin
          reg_pipe_0_rd_en <= 1'h0;
        end else begin
          if (_T_405) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_rd_en <= fifo_vec_1_deq_bits_rd_en;
              end else begin
                reg_pipe_0_rd_en <= fifo_vec_0_deq_bits_rd_en;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_rd_en <= fifo_vec_0_deq_bits_rd_en;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rd_en <= fifo_vec_0_deq_bits_rd_en;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_rd_en <= fifo_vec_1_deq_bits_rd_en;
            end else begin
              reg_pipe_0_rd_en <= fifo_vec_0_deq_bits_rd_en;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rd_en <= fifo_vec_0_deq_bits_rd_en;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_rd_en <= fifo_vec_0_deq_bits_rd_en;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_rs2_en <= 1'h0;
    end else begin
      if (io_flush) begin
        if (1'h0 == io_flushTag) begin
          reg_pipe_0_rs2_en <= 1'h0;
        end else begin
          if (_T_405) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_rs2_en <= fifo_vec_1_deq_bits_rs2_en;
              end else begin
                reg_pipe_0_rs2_en <= fifo_vec_0_deq_bits_rs2_en;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_rs2_en <= fifo_vec_0_deq_bits_rs2_en;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rs2_en <= fifo_vec_0_deq_bits_rs2_en;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_rs2_en <= fifo_vec_1_deq_bits_rs2_en;
            end else begin
              reg_pipe_0_rs2_en <= fifo_vec_0_deq_bits_rs2_en;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rs2_en <= fifo_vec_0_deq_bits_rs2_en;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_rs2_en <= fifo_vec_0_deq_bits_rs2_en;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_shift_en <= 1'h0;
    end else begin
      if (io_flush) begin
        if (1'h0 == io_flushTag) begin
          reg_pipe_0_shift_en <= 1'h0;
        end else begin
          if (_T_405) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_shift_en <= fifo_vec_1_deq_bits_shift_en;
              end else begin
                reg_pipe_0_shift_en <= fifo_vec_0_deq_bits_shift_en;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_shift_en <= fifo_vec_0_deq_bits_shift_en;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_shift_en <= fifo_vec_0_deq_bits_shift_en;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_shift_en <= fifo_vec_1_deq_bits_shift_en;
            end else begin
              reg_pipe_0_shift_en <= fifo_vec_0_deq_bits_shift_en;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_shift_en <= fifo_vec_0_deq_bits_shift_en;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_shift_en <= fifo_vec_0_deq_bits_shift_en;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_nzcv_en <= 1'h0;
    end else begin
      if (io_flush) begin
        if (1'h0 == io_flushTag) begin
          reg_pipe_0_nzcv_en <= 1'h0;
        end else begin
          if (_T_405) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_nzcv_en <= fifo_vec_1_deq_bits_nzcv_en;
              end else begin
                reg_pipe_0_nzcv_en <= fifo_vec_0_deq_bits_nzcv_en;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_nzcv_en <= fifo_vec_0_deq_bits_nzcv_en;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_nzcv_en <= fifo_vec_0_deq_bits_nzcv_en;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_nzcv_en <= fifo_vec_1_deq_bits_nzcv_en;
            end else begin
              reg_pipe_0_nzcv_en <= fifo_vec_0_deq_bits_nzcv_en;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_nzcv_en <= fifo_vec_0_deq_bits_nzcv_en;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_nzcv_en <= fifo_vec_0_deq_bits_nzcv_en;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_tag <= 1'h0;
    end else begin
      if (io_flush) begin
        if (1'h0 == io_flushTag) begin
          reg_pipe_0_tag <= 1'h0;
        end else begin
          if (_T_405) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_tag <= fifo_vec_1_deq_bits_tag;
              end else begin
                reg_pipe_0_tag <= fifo_vec_0_deq_bits_tag;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_tag <= fifo_vec_0_deq_bits_tag;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_tag <= fifo_vec_0_deq_bits_tag;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_tag <= fifo_vec_1_deq_bits_tag;
            end else begin
              reg_pipe_0_tag <= fifo_vec_0_deq_bits_tag;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_tag <= fifo_vec_0_deq_bits_tag;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_tag <= fifo_vec_0_deq_bits_tag;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_rd <= 5'h0;
    end else begin
      if (io_flush) begin
        if (io_flushTag) begin
          reg_pipe_1_rd <= 5'h0;
        end else begin
          if (_T_405) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_rd <= fifo_vec_1_deq_bits_rd;
              end else begin
                reg_pipe_1_rd <= fifo_vec_0_deq_bits_rd;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_rd <= fifo_vec_1_deq_bits_rd;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rd <= fifo_vec_1_deq_bits_rd;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_rd <= fifo_vec_1_deq_bits_rd;
            end else begin
              reg_pipe_1_rd <= fifo_vec_0_deq_bits_rd;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rd <= fifo_vec_1_deq_bits_rd;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_rd <= fifo_vec_1_deq_bits_rd;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_rs1 <= 5'h0;
    end else begin
      if (io_flush) begin
        if (io_flushTag) begin
          reg_pipe_1_rs1 <= 5'h0;
        end else begin
          if (_T_405) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_rs1 <= fifo_vec_1_deq_bits_rs1;
              end else begin
                reg_pipe_1_rs1 <= fifo_vec_0_deq_bits_rs1;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_rs1 <= fifo_vec_1_deq_bits_rs1;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rs1 <= fifo_vec_1_deq_bits_rs1;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_rs1 <= fifo_vec_1_deq_bits_rs1;
            end else begin
              reg_pipe_1_rs1 <= fifo_vec_0_deq_bits_rs1;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rs1 <= fifo_vec_1_deq_bits_rs1;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_rs1 <= fifo_vec_1_deq_bits_rs1;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_rs2 <= 5'h0;
    end else begin
      if (io_flush) begin
        if (io_flushTag) begin
          reg_pipe_1_rs2 <= 5'h0;
        end else begin
          if (_T_405) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_rs2 <= fifo_vec_1_deq_bits_rs2;
              end else begin
                reg_pipe_1_rs2 <= fifo_vec_0_deq_bits_rs2;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_rs2 <= fifo_vec_1_deq_bits_rs2;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rs2 <= fifo_vec_1_deq_bits_rs2;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_rs2 <= fifo_vec_1_deq_bits_rs2;
            end else begin
              reg_pipe_1_rs2 <= fifo_vec_0_deq_bits_rs2;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rs2 <= fifo_vec_1_deq_bits_rs2;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_rs2 <= fifo_vec_1_deq_bits_rs2;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_imm <= 26'h0;
    end else begin
      if (io_flush) begin
        if (io_flushTag) begin
          reg_pipe_1_imm <= 26'h0;
        end else begin
          if (_T_405) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_imm <= fifo_vec_1_deq_bits_imm;
              end else begin
                reg_pipe_1_imm <= fifo_vec_0_deq_bits_imm;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_imm <= fifo_vec_1_deq_bits_imm;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_imm <= fifo_vec_1_deq_bits_imm;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_imm <= fifo_vec_1_deq_bits_imm;
            end else begin
              reg_pipe_1_imm <= fifo_vec_0_deq_bits_imm;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_imm <= fifo_vec_1_deq_bits_imm;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_imm <= fifo_vec_1_deq_bits_imm;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_shift_val <= 6'h0;
    end else begin
      if (io_flush) begin
        if (io_flushTag) begin
          reg_pipe_1_shift_val <= 6'h0;
        end else begin
          if (_T_405) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_shift_val <= fifo_vec_1_deq_bits_shift_val;
              end else begin
                reg_pipe_1_shift_val <= fifo_vec_0_deq_bits_shift_val;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_shift_val <= fifo_vec_1_deq_bits_shift_val;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_shift_val <= fifo_vec_1_deq_bits_shift_val;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_shift_val <= fifo_vec_1_deq_bits_shift_val;
            end else begin
              reg_pipe_1_shift_val <= fifo_vec_0_deq_bits_shift_val;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_shift_val <= fifo_vec_1_deq_bits_shift_val;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_shift_val <= fifo_vec_1_deq_bits_shift_val;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_shift_type <= 2'h0;
    end else begin
      if (io_flush) begin
        if (io_flushTag) begin
          reg_pipe_1_shift_type <= 2'h0;
        end else begin
          if (_T_405) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_shift_type <= fifo_vec_1_deq_bits_shift_type;
              end else begin
                reg_pipe_1_shift_type <= fifo_vec_0_deq_bits_shift_type;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_shift_type <= fifo_vec_1_deq_bits_shift_type;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_shift_type <= fifo_vec_1_deq_bits_shift_type;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_shift_type <= fifo_vec_1_deq_bits_shift_type;
            end else begin
              reg_pipe_1_shift_type <= fifo_vec_0_deq_bits_shift_type;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_shift_type <= fifo_vec_1_deq_bits_shift_type;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_shift_type <= fifo_vec_1_deq_bits_shift_type;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_cond <= 4'h0;
    end else begin
      if (io_flush) begin
        if (io_flushTag) begin
          reg_pipe_1_cond <= 4'h0;
        end else begin
          if (_T_405) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_cond <= fifo_vec_1_deq_bits_cond;
              end else begin
                reg_pipe_1_cond <= fifo_vec_0_deq_bits_cond;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_cond <= fifo_vec_1_deq_bits_cond;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_cond <= fifo_vec_1_deq_bits_cond;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_cond <= fifo_vec_1_deq_bits_cond;
            end else begin
              reg_pipe_1_cond <= fifo_vec_0_deq_bits_cond;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_cond <= fifo_vec_1_deq_bits_cond;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_cond <= fifo_vec_1_deq_bits_cond;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_itype <= 3'h0;
    end else begin
      if (io_flush) begin
        if (io_flushTag) begin
          reg_pipe_1_itype <= 3'h0;
        end else begin
          if (_T_405) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_itype <= fifo_vec_1_deq_bits_itype;
              end else begin
                reg_pipe_1_itype <= fifo_vec_0_deq_bits_itype;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_itype <= fifo_vec_1_deq_bits_itype;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_itype <= fifo_vec_1_deq_bits_itype;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_itype <= fifo_vec_1_deq_bits_itype;
            end else begin
              reg_pipe_1_itype <= fifo_vec_0_deq_bits_itype;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_itype <= fifo_vec_1_deq_bits_itype;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_itype <= fifo_vec_1_deq_bits_itype;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_op <= 3'h0;
    end else begin
      if (io_flush) begin
        if (io_flushTag) begin
          reg_pipe_1_op <= 3'h0;
        end else begin
          if (_T_405) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_op <= fifo_vec_1_deq_bits_op;
              end else begin
                reg_pipe_1_op <= fifo_vec_0_deq_bits_op;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_op <= fifo_vec_1_deq_bits_op;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_op <= fifo_vec_1_deq_bits_op;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_op <= fifo_vec_1_deq_bits_op;
            end else begin
              reg_pipe_1_op <= fifo_vec_0_deq_bits_op;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_op <= fifo_vec_1_deq_bits_op;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_op <= fifo_vec_1_deq_bits_op;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_rd_en <= 1'h0;
    end else begin
      if (io_flush) begin
        if (io_flushTag) begin
          reg_pipe_1_rd_en <= 1'h0;
        end else begin
          if (_T_405) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_rd_en <= fifo_vec_1_deq_bits_rd_en;
              end else begin
                reg_pipe_1_rd_en <= fifo_vec_0_deq_bits_rd_en;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_rd_en <= fifo_vec_1_deq_bits_rd_en;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rd_en <= fifo_vec_1_deq_bits_rd_en;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_rd_en <= fifo_vec_1_deq_bits_rd_en;
            end else begin
              reg_pipe_1_rd_en <= fifo_vec_0_deq_bits_rd_en;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rd_en <= fifo_vec_1_deq_bits_rd_en;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_rd_en <= fifo_vec_1_deq_bits_rd_en;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_rs2_en <= 1'h0;
    end else begin
      if (io_flush) begin
        if (io_flushTag) begin
          reg_pipe_1_rs2_en <= 1'h0;
        end else begin
          if (_T_405) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_rs2_en <= fifo_vec_1_deq_bits_rs2_en;
              end else begin
                reg_pipe_1_rs2_en <= fifo_vec_0_deq_bits_rs2_en;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_rs2_en <= fifo_vec_1_deq_bits_rs2_en;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rs2_en <= fifo_vec_1_deq_bits_rs2_en;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_rs2_en <= fifo_vec_1_deq_bits_rs2_en;
            end else begin
              reg_pipe_1_rs2_en <= fifo_vec_0_deq_bits_rs2_en;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rs2_en <= fifo_vec_1_deq_bits_rs2_en;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_rs2_en <= fifo_vec_1_deq_bits_rs2_en;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_shift_en <= 1'h0;
    end else begin
      if (io_flush) begin
        if (io_flushTag) begin
          reg_pipe_1_shift_en <= 1'h0;
        end else begin
          if (_T_405) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_shift_en <= fifo_vec_1_deq_bits_shift_en;
              end else begin
                reg_pipe_1_shift_en <= fifo_vec_0_deq_bits_shift_en;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_shift_en <= fifo_vec_1_deq_bits_shift_en;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_shift_en <= fifo_vec_1_deq_bits_shift_en;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_shift_en <= fifo_vec_1_deq_bits_shift_en;
            end else begin
              reg_pipe_1_shift_en <= fifo_vec_0_deq_bits_shift_en;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_shift_en <= fifo_vec_1_deq_bits_shift_en;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_shift_en <= fifo_vec_1_deq_bits_shift_en;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_nzcv_en <= 1'h0;
    end else begin
      if (io_flush) begin
        if (io_flushTag) begin
          reg_pipe_1_nzcv_en <= 1'h0;
        end else begin
          if (_T_405) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_nzcv_en <= fifo_vec_1_deq_bits_nzcv_en;
              end else begin
                reg_pipe_1_nzcv_en <= fifo_vec_0_deq_bits_nzcv_en;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_nzcv_en <= fifo_vec_1_deq_bits_nzcv_en;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_nzcv_en <= fifo_vec_1_deq_bits_nzcv_en;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_nzcv_en <= fifo_vec_1_deq_bits_nzcv_en;
            end else begin
              reg_pipe_1_nzcv_en <= fifo_vec_0_deq_bits_nzcv_en;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_nzcv_en <= fifo_vec_1_deq_bits_nzcv_en;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_nzcv_en <= fifo_vec_1_deq_bits_nzcv_en;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_tag <= 1'h0;
    end else begin
      if (io_flush) begin
        if (io_flushTag) begin
          reg_pipe_1_tag <= 1'h0;
        end else begin
          if (_T_405) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_tag <= fifo_vec_1_deq_bits_tag;
              end else begin
                reg_pipe_1_tag <= fifo_vec_0_deq_bits_tag;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_tag <= fifo_vec_1_deq_bits_tag;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_tag <= fifo_vec_1_deq_bits_tag;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_tag <= fifo_vec_1_deq_bits_tag;
            end else begin
              reg_pipe_1_tag <= fifo_vec_0_deq_bits_tag;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_tag <= fifo_vec_1_deq_bits_tag;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_tag <= fifo_vec_1_deq_bits_tag;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_v_0 <= 1'h0;
    end else begin
      if (io_flush) begin
        if (1'h0 == io_flushTag) begin
          reg_pipe_v_0 <= 1'h0;
        end else begin
          if (_T_405) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_v_0 <= fifo_vec_1_deq_valid;
              end else begin
                reg_pipe_v_0 <= fifo_vec_0_deq_valid;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_v_0 <= fifo_vec_0_deq_valid;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_v_0 <= fifo_vec_0_deq_valid;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_v_0 <= fifo_vec_1_deq_valid;
            end else begin
              reg_pipe_v_0 <= fifo_vec_0_deq_valid;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_v_0 <= fifo_vec_0_deq_valid;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_v_0 <= fifo_vec_0_deq_valid;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_v_1 <= 1'h0;
    end else begin
      if (io_flush) begin
        if (io_flushTag) begin
          reg_pipe_v_1 <= 1'h0;
        end else begin
          if (_T_405) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_v_1 <= fifo_vec_1_deq_valid;
              end else begin
                reg_pipe_v_1 <= fifo_vec_0_deq_valid;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_v_1 <= fifo_vec_1_deq_valid;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_v_1 <= fifo_vec_1_deq_valid;
            end
          end
        end
      end else begin
        if (_T_405) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_v_1 <= fifo_vec_1_deq_valid;
            end else begin
              reg_pipe_v_1 <= fifo_vec_0_deq_valid;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_v_1 <= fifo_vec_1_deq_valid;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_v_1 <= fifo_vec_1_deq_valid;
          end
        end
      end
    end
  end
endmodule
