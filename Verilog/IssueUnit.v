module IssueUnit( // @[:@1508.2]
  input         clock, // @[:@1509.4]
  input         reset, // @[:@1510.4]
  input         io_flush_tag, // @[:@1511.4]
  input         io_flush_valid, // @[:@1511.4]
  output        io_enq_ready, // @[:@1511.4]
  input         io_enq_valid, // @[:@1511.4]
  input         io_enq_bits_rd_valid, // @[:@1511.4]
  input  [4:0]  io_enq_bits_rd_bits, // @[:@1511.4]
  input  [4:0]  io_enq_bits_rs1_bits, // @[:@1511.4]
  input         io_enq_bits_rs2_valid, // @[:@1511.4]
  input  [4:0]  io_enq_bits_rs2_bits, // @[:@1511.4]
  input  [25:0] io_enq_bits_imm_bits, // @[:@1511.4]
  input         io_enq_bits_shift_val_valid, // @[:@1511.4]
  input  [5:0]  io_enq_bits_shift_val_bits, // @[:@1511.4]
  input  [1:0]  io_enq_bits_shift_type, // @[:@1511.4]
  input  [3:0]  io_enq_bits_cond_bits, // @[:@1511.4]
  input  [2:0]  io_enq_bits_itype, // @[:@1511.4]
  input  [2:0]  io_enq_bits_op, // @[:@1511.4]
  input         io_enq_bits_nzcv_en, // @[:@1511.4]
  input         io_enq_bits_tag, // @[:@1511.4]
  input  [63:0] io_enq_bits_pc, // @[:@1511.4]
  output        io_deq_valid, // @[:@1511.4]
  output        io_deq_bits_rd_valid, // @[:@1511.4]
  output [4:0]  io_deq_bits_rd_bits, // @[:@1511.4]
  output [4:0]  io_deq_bits_rs1_bits, // @[:@1511.4]
  output        io_deq_bits_rs2_valid, // @[:@1511.4]
  output [4:0]  io_deq_bits_rs2_bits, // @[:@1511.4]
  output [25:0] io_deq_bits_imm_bits, // @[:@1511.4]
  output        io_deq_bits_shift_val_valid, // @[:@1511.4]
  output [5:0]  io_deq_bits_shift_val_bits, // @[:@1511.4]
  output [1:0]  io_deq_bits_shift_type, // @[:@1511.4]
  output [3:0]  io_deq_bits_cond_bits, // @[:@1511.4]
  output [2:0]  io_deq_bits_itype, // @[:@1511.4]
  output [2:0]  io_deq_bits_op, // @[:@1511.4]
  output        io_deq_bits_nzcv_en, // @[:@1511.4]
  output        io_deq_bits_tag, // @[:@1511.4]
  output [63:0] io_deq_bits_pc, // @[:@1511.4]
  input         io_commitReg_valid, // @[:@1511.4]
  input         io_commitReg_bits_exe_bits_rd_valid, // @[:@1511.4]
  input  [4:0]  io_commitReg_bits_exe_bits_rd_bits, // @[:@1511.4]
  input         io_commitReg_bits_tag // @[:@1511.4]
);
  wire  Queue_clock; // @[issue.scala 94:113:@1611.4]
  wire  Queue_reset; // @[issue.scala 94:113:@1611.4]
  wire  Queue_io_enq_ready; // @[issue.scala 94:113:@1611.4]
  wire  Queue_io_enq_valid; // @[issue.scala 94:113:@1611.4]
  wire  Queue_io_enq_bits_rd_valid; // @[issue.scala 94:113:@1611.4]
  wire [4:0] Queue_io_enq_bits_rd_bits; // @[issue.scala 94:113:@1611.4]
  wire [4:0] Queue_io_enq_bits_rs1_bits; // @[issue.scala 94:113:@1611.4]
  wire  Queue_io_enq_bits_rs2_valid; // @[issue.scala 94:113:@1611.4]
  wire [4:0] Queue_io_enq_bits_rs2_bits; // @[issue.scala 94:113:@1611.4]
  wire [25:0] Queue_io_enq_bits_imm_bits; // @[issue.scala 94:113:@1611.4]
  wire  Queue_io_enq_bits_shift_val_valid; // @[issue.scala 94:113:@1611.4]
  wire [5:0] Queue_io_enq_bits_shift_val_bits; // @[issue.scala 94:113:@1611.4]
  wire [1:0] Queue_io_enq_bits_shift_type; // @[issue.scala 94:113:@1611.4]
  wire [3:0] Queue_io_enq_bits_cond_bits; // @[issue.scala 94:113:@1611.4]
  wire [2:0] Queue_io_enq_bits_itype; // @[issue.scala 94:113:@1611.4]
  wire [2:0] Queue_io_enq_bits_op; // @[issue.scala 94:113:@1611.4]
  wire  Queue_io_enq_bits_nzcv_en; // @[issue.scala 94:113:@1611.4]
  wire  Queue_io_enq_bits_tag; // @[issue.scala 94:113:@1611.4]
  wire [63:0] Queue_io_enq_bits_pc; // @[issue.scala 94:113:@1611.4]
  wire  Queue_io_deq_ready; // @[issue.scala 94:113:@1611.4]
  wire  Queue_io_deq_valid; // @[issue.scala 94:113:@1611.4]
  wire  Queue_io_deq_bits_rd_valid; // @[issue.scala 94:113:@1611.4]
  wire [4:0] Queue_io_deq_bits_rd_bits; // @[issue.scala 94:113:@1611.4]
  wire [4:0] Queue_io_deq_bits_rs1_bits; // @[issue.scala 94:113:@1611.4]
  wire  Queue_io_deq_bits_rs2_valid; // @[issue.scala 94:113:@1611.4]
  wire [4:0] Queue_io_deq_bits_rs2_bits; // @[issue.scala 94:113:@1611.4]
  wire [25:0] Queue_io_deq_bits_imm_bits; // @[issue.scala 94:113:@1611.4]
  wire  Queue_io_deq_bits_shift_val_valid; // @[issue.scala 94:113:@1611.4]
  wire [5:0] Queue_io_deq_bits_shift_val_bits; // @[issue.scala 94:113:@1611.4]
  wire [1:0] Queue_io_deq_bits_shift_type; // @[issue.scala 94:113:@1611.4]
  wire [3:0] Queue_io_deq_bits_cond_bits; // @[issue.scala 94:113:@1611.4]
  wire [2:0] Queue_io_deq_bits_itype; // @[issue.scala 94:113:@1611.4]
  wire [2:0] Queue_io_deq_bits_op; // @[issue.scala 94:113:@1611.4]
  wire  Queue_io_deq_bits_nzcv_en; // @[issue.scala 94:113:@1611.4]
  wire  Queue_io_deq_bits_tag; // @[issue.scala 94:113:@1611.4]
  wire [63:0] Queue_io_deq_bits_pc; // @[issue.scala 94:113:@1611.4]
  wire  Queue_1_clock; // @[issue.scala 94:113:@1618.4]
  wire  Queue_1_reset; // @[issue.scala 94:113:@1618.4]
  wire  Queue_1_io_enq_ready; // @[issue.scala 94:113:@1618.4]
  wire  Queue_1_io_enq_valid; // @[issue.scala 94:113:@1618.4]
  wire  Queue_1_io_enq_bits_rd_valid; // @[issue.scala 94:113:@1618.4]
  wire [4:0] Queue_1_io_enq_bits_rd_bits; // @[issue.scala 94:113:@1618.4]
  wire [4:0] Queue_1_io_enq_bits_rs1_bits; // @[issue.scala 94:113:@1618.4]
  wire  Queue_1_io_enq_bits_rs2_valid; // @[issue.scala 94:113:@1618.4]
  wire [4:0] Queue_1_io_enq_bits_rs2_bits; // @[issue.scala 94:113:@1618.4]
  wire [25:0] Queue_1_io_enq_bits_imm_bits; // @[issue.scala 94:113:@1618.4]
  wire  Queue_1_io_enq_bits_shift_val_valid; // @[issue.scala 94:113:@1618.4]
  wire [5:0] Queue_1_io_enq_bits_shift_val_bits; // @[issue.scala 94:113:@1618.4]
  wire [1:0] Queue_1_io_enq_bits_shift_type; // @[issue.scala 94:113:@1618.4]
  wire [3:0] Queue_1_io_enq_bits_cond_bits; // @[issue.scala 94:113:@1618.4]
  wire [2:0] Queue_1_io_enq_bits_itype; // @[issue.scala 94:113:@1618.4]
  wire [2:0] Queue_1_io_enq_bits_op; // @[issue.scala 94:113:@1618.4]
  wire  Queue_1_io_enq_bits_nzcv_en; // @[issue.scala 94:113:@1618.4]
  wire  Queue_1_io_enq_bits_tag; // @[issue.scala 94:113:@1618.4]
  wire [63:0] Queue_1_io_enq_bits_pc; // @[issue.scala 94:113:@1618.4]
  wire  Queue_1_io_deq_ready; // @[issue.scala 94:113:@1618.4]
  wire  Queue_1_io_deq_valid; // @[issue.scala 94:113:@1618.4]
  wire  Queue_1_io_deq_bits_rd_valid; // @[issue.scala 94:113:@1618.4]
  wire [4:0] Queue_1_io_deq_bits_rd_bits; // @[issue.scala 94:113:@1618.4]
  wire [4:0] Queue_1_io_deq_bits_rs1_bits; // @[issue.scala 94:113:@1618.4]
  wire  Queue_1_io_deq_bits_rs2_valid; // @[issue.scala 94:113:@1618.4]
  wire [4:0] Queue_1_io_deq_bits_rs2_bits; // @[issue.scala 94:113:@1618.4]
  wire [25:0] Queue_1_io_deq_bits_imm_bits; // @[issue.scala 94:113:@1618.4]
  wire  Queue_1_io_deq_bits_shift_val_valid; // @[issue.scala 94:113:@1618.4]
  wire [5:0] Queue_1_io_deq_bits_shift_val_bits; // @[issue.scala 94:113:@1618.4]
  wire [1:0] Queue_1_io_deq_bits_shift_type; // @[issue.scala 94:113:@1618.4]
  wire [3:0] Queue_1_io_deq_bits_cond_bits; // @[issue.scala 94:113:@1618.4]
  wire [2:0] Queue_1_io_deq_bits_itype; // @[issue.scala 94:113:@1618.4]
  wire [2:0] Queue_1_io_deq_bits_op; // @[issue.scala 94:113:@1618.4]
  wire  Queue_1_io_deq_bits_nzcv_en; // @[issue.scala 94:113:@1618.4]
  wire  Queue_1_io_deq_bits_tag; // @[issue.scala 94:113:@1618.4]
  wire [63:0] Queue_1_io_deq_bits_pc; // @[issue.scala 94:113:@1618.4]
  wire  arbiter_clock; // @[issue.scala 101:29:@1712.4]
  wire  arbiter_reset; // @[issue.scala 101:29:@1712.4]
  wire [1:0] arbiter_io_ready; // @[issue.scala 101:29:@1712.4]
  wire  arbiter_io_next_ready; // @[issue.scala 101:29:@1712.4]
  wire  arbiter_io_next_valid; // @[issue.scala 101:29:@1712.4]
  wire  arbiter_io_next_bits; // @[issue.scala 101:29:@1712.4]
  reg  reg_pipe_0_rd_valid; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_0;
  reg [4:0] reg_pipe_0_rd_bits; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_1;
  reg [4:0] reg_pipe_0_rs1_bits; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_2;
  reg  reg_pipe_0_rs2_valid; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_3;
  reg [4:0] reg_pipe_0_rs2_bits; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_4;
  reg [25:0] reg_pipe_0_imm_bits; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_5;
  reg  reg_pipe_0_shift_val_valid; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_6;
  reg [5:0] reg_pipe_0_shift_val_bits; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_7;
  reg [1:0] reg_pipe_0_shift_type; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_8;
  reg [3:0] reg_pipe_0_cond_bits; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_9;
  reg [2:0] reg_pipe_0_itype; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_10;
  reg [2:0] reg_pipe_0_op; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_11;
  reg  reg_pipe_0_nzcv_en; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_12;
  reg  reg_pipe_0_tag; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_13;
  reg [63:0] reg_pipe_0_pc; // @[issue.scala 79:27:@1596.4]
  reg [63:0] _RAND_14;
  reg  reg_pipe_1_rd_valid; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_15;
  reg [4:0] reg_pipe_1_rd_bits; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_16;
  reg [4:0] reg_pipe_1_rs1_bits; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_17;
  reg  reg_pipe_1_rs2_valid; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_18;
  reg [4:0] reg_pipe_1_rs2_bits; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_19;
  reg [25:0] reg_pipe_1_imm_bits; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_20;
  reg  reg_pipe_1_shift_val_valid; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_21;
  reg [5:0] reg_pipe_1_shift_val_bits; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_22;
  reg [1:0] reg_pipe_1_shift_type; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_23;
  reg [3:0] reg_pipe_1_cond_bits; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_24;
  reg [2:0] reg_pipe_1_itype; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_25;
  reg [2:0] reg_pipe_1_op; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_26;
  reg  reg_pipe_1_nzcv_en; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_27;
  reg  reg_pipe_1_tag; // @[issue.scala 79:27:@1596.4]
  reg [31:0] _RAND_28;
  reg [63:0] reg_pipe_1_pc; // @[issue.scala 79:27:@1596.4]
  reg [63:0] _RAND_29;
  reg  reg_pipe_v_0; // @[issue.scala 80:27:@1600.4]
  reg [31:0] _RAND_30;
  reg  reg_pipe_v_1; // @[issue.scala 80:27:@1600.4]
  reg [31:0] _RAND_31;
  wire  _T_1167; // @[issue.scala 94:68:@1608.4]
  wire  _T_1168; // @[issue.scala 94:85:@1609.4]
  wire  _T_1193; // @[issue.scala 94:85:@1616.4]
  wire  fifo_vec_0_enq_ready; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1666.4]
  wire  _T_3437; // @[issue.scala 150:30:@1856.4]
  wire  sig_next_idx; // @[:@1718.4 :@1719.4 issue.scala 142:20:@1833.4]
  wire  sig_pipe_r_0; // @[issue.scala 122:24:@1768.4]
  wire  _GEN_176; // @[issue.scala 151:38:@1858.6]
  wire  fifo_vec_0_deq_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1643.4]
  wire  fifo_vec_0_deq_bits_rd_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1642.4]
  wire [4:0] fifo_vec_0_deq_bits_rd_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1641.4]
  wire [4:0] fifo_vec_0_deq_bits_rs1_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1639.4]
  wire  fifo_vec_0_deq_bits_rs2_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1638.4]
  wire [4:0] fifo_vec_0_deq_bits_rs2_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1637.4]
  wire [25:0] fifo_vec_0_deq_bits_imm_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1635.4]
  wire  fifo_vec_0_deq_bits_shift_val_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1634.4]
  wire [5:0] fifo_vec_0_deq_bits_shift_val_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1633.4]
  wire [1:0] fifo_vec_0_deq_bits_shift_type; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1632.4]
  wire [3:0] fifo_vec_0_deq_bits_cond_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1630.4]
  wire [2:0] fifo_vec_0_deq_bits_itype; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1629.4]
  wire [2:0] fifo_vec_0_deq_bits_op; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1628.4]
  wire  fifo_vec_0_deq_bits_nzcv_en; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1627.4]
  wire  fifo_vec_0_deq_bits_tag; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1626.4]
  wire [63:0] fifo_vec_0_deq_bits_pc; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1623.4]
  wire  fifo_vec_1_enq_ready; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1711.4]
  wire  sig_pipe_r_1; // @[issue.scala 122:24:@1795.4]
  wire  _GEN_177; // @[issue.scala 151:38:@1858.6]
  wire  fifo_vec_1_deq_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1688.4]
  wire  fifo_vec_1_deq_bits_rd_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1687.4]
  wire [4:0] fifo_vec_1_deq_bits_rd_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1686.4]
  wire [4:0] fifo_vec_1_deq_bits_rs1_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1684.4]
  wire  fifo_vec_1_deq_bits_rs2_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1683.4]
  wire [4:0] fifo_vec_1_deq_bits_rs2_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1682.4]
  wire [25:0] fifo_vec_1_deq_bits_imm_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1680.4]
  wire  fifo_vec_1_deq_bits_shift_val_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1679.4]
  wire [5:0] fifo_vec_1_deq_bits_shift_val_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1678.4]
  wire [1:0] fifo_vec_1_deq_bits_shift_type; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1677.4]
  wire [3:0] fifo_vec_1_deq_bits_cond_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1675.4]
  wire [2:0] fifo_vec_1_deq_bits_itype; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1674.4]
  wire [2:0] fifo_vec_1_deq_bits_op; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1673.4]
  wire  fifo_vec_1_deq_bits_nzcv_en; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1672.4]
  wire  fifo_vec_1_deq_bits_tag; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1671.4]
  wire [63:0] fifo_vec_1_deq_bits_pc; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1668.4]
  wire  _T_3131_rd_valid; // @[issue.scala 125:27:@1771.4]
  wire [4:0] _T_3131_rd_bits; // @[issue.scala 125:27:@1771.4]
  wire [4:0] _T_3131_rs1_bits; // @[issue.scala 125:27:@1771.4]
  wire  _T_3131_rs2_valid; // @[issue.scala 125:27:@1771.4]
  wire [4:0] _T_3131_rs2_bits; // @[issue.scala 125:27:@1771.4]
  wire [25:0] _T_3131_imm_bits; // @[issue.scala 125:27:@1771.4]
  wire  _T_3131_shift_val_valid; // @[issue.scala 125:27:@1771.4]
  wire [5:0] _T_3131_shift_val_bits; // @[issue.scala 125:27:@1771.4]
  wire [1:0] _T_3131_shift_type; // @[issue.scala 125:27:@1771.4]
  wire [3:0] _T_3131_cond_bits; // @[issue.scala 125:27:@1771.4]
  wire [2:0] _T_3131_itype; // @[issue.scala 125:27:@1771.4]
  wire [2:0] _T_3131_op; // @[issue.scala 125:27:@1771.4]
  wire  _T_3131_nzcv_en; // @[issue.scala 125:27:@1771.4]
  wire  _T_3131_tag; // @[issue.scala 125:27:@1771.4]
  wire [63:0] _T_3131_pc; // @[issue.scala 125:27:@1771.4]
  wire  _T_3152; // @[issue.scala 126:27:@1792.4]
  wire  _T_3154_rd_valid; // @[issue.scala 125:27:@1798.4]
  wire [4:0] _T_3154_rd_bits; // @[issue.scala 125:27:@1798.4]
  wire [4:0] _T_3154_rs1_bits; // @[issue.scala 125:27:@1798.4]
  wire  _T_3154_rs2_valid; // @[issue.scala 125:27:@1798.4]
  wire [4:0] _T_3154_rs2_bits; // @[issue.scala 125:27:@1798.4]
  wire [25:0] _T_3154_imm_bits; // @[issue.scala 125:27:@1798.4]
  wire  _T_3154_shift_val_valid; // @[issue.scala 125:27:@1798.4]
  wire [5:0] _T_3154_shift_val_bits; // @[issue.scala 125:27:@1798.4]
  wire [1:0] _T_3154_shift_type; // @[issue.scala 125:27:@1798.4]
  wire [3:0] _T_3154_cond_bits; // @[issue.scala 125:27:@1798.4]
  wire [2:0] _T_3154_itype; // @[issue.scala 125:27:@1798.4]
  wire [2:0] _T_3154_op; // @[issue.scala 125:27:@1798.4]
  wire  _T_3154_nzcv_en; // @[issue.scala 125:27:@1798.4]
  wire  _T_3154_tag; // @[issue.scala 125:27:@1798.4]
  wire [63:0] _T_3154_pc; // @[issue.scala 125:27:@1798.4]
  wire  _T_3175; // @[issue.scala 126:27:@1819.4]
  wire  rfile_wb_pending; // @[issue.scala 133:45:@1822.4]
  wire [4:0] _GEN_115; // @[issue.scala 135:48:@1823.4]
  wire [4:0] _GEN_117; // @[issue.scala 135:48:@1823.4]
  wire  _T_3259; // @[issue.scala 135:48:@1823.4]
  wire  _T_3343; // @[issue.scala 136:50:@1824.4]
  wire  _T_3344; // @[issue.scala 135:88:@1825.4]
  wire  exe_stall; // @[issue.scala 134:33:@1826.4]
  wire  _T_3351; // @[issue.scala 137:77:@1828.4]
  wire  _GEN_133; // @[issue.scala 137:74:@1829.4]
  wire  _T_3352; // @[issue.scala 137:74:@1829.4]
  wire  _GEN_134; // @[issue.scala 137:37:@1830.4]
  wire  _GEN_135; // @[issue.scala 137:37:@1830.4]
  wire  _GEN_397; // @[issue.scala 160:30:@1924.6]
  wire  sig_pipe_i_1; // @[issue.scala 157:24:@1881.4]
  wire  _GEN_396; // @[issue.scala 160:30:@1924.6]
  wire  sig_pipe_i_0; // @[issue.scala 157:24:@1881.4]
  wire  _GEN_248; // @[issue.scala 152:30:@1859.6]
  wire  _GEN_178; // @[issue.scala 152:30:@1859.6]
  wire  _GEN_179; // @[issue.scala 152:30:@1859.6]
  wire  _GEN_249; // @[issue.scala 152:30:@1859.6]
  wire [4:0] _GEN_250; // @[issue.scala 152:30:@1859.6]
  wire [4:0] _GEN_252; // @[issue.scala 152:30:@1859.6]
  wire  _GEN_253; // @[issue.scala 152:30:@1859.6]
  wire [4:0] _GEN_254; // @[issue.scala 152:30:@1859.6]
  wire [25:0] _GEN_256; // @[issue.scala 152:30:@1859.6]
  wire  _GEN_257; // @[issue.scala 152:30:@1859.6]
  wire [5:0] _GEN_258; // @[issue.scala 152:30:@1859.6]
  wire [1:0] _GEN_259; // @[issue.scala 152:30:@1859.6]
  wire [3:0] _GEN_261; // @[issue.scala 152:30:@1859.6]
  wire [2:0] _GEN_262; // @[issue.scala 152:30:@1859.6]
  wire [2:0] _GEN_263; // @[issue.scala 152:30:@1859.6]
  wire  _GEN_264; // @[issue.scala 152:30:@1859.6]
  wire  _GEN_265; // @[issue.scala 152:30:@1859.6]
  wire [63:0] _GEN_268; // @[issue.scala 152:30:@1859.6]
  wire [63:0] _GEN_270; // @[issue.scala 153:28:@1860.6]
  wire [63:0] _GEN_271; // @[issue.scala 153:28:@1860.6]
  wire  _GEN_276; // @[issue.scala 153:28:@1863.6]
  wire  _GEN_277; // @[issue.scala 153:28:@1863.6]
  wire  _GEN_278; // @[issue.scala 153:28:@1864.6]
  wire  _GEN_279; // @[issue.scala 153:28:@1864.6]
  wire [2:0] _GEN_280; // @[issue.scala 153:28:@1865.6]
  wire [2:0] _GEN_281; // @[issue.scala 153:28:@1865.6]
  wire [2:0] _GEN_282; // @[issue.scala 153:28:@1866.6]
  wire [2:0] _GEN_283; // @[issue.scala 153:28:@1866.6]
  wire [3:0] _GEN_284; // @[issue.scala 153:28:@1867.6]
  wire [3:0] _GEN_285; // @[issue.scala 153:28:@1867.6]
  wire [1:0] _GEN_288; // @[issue.scala 153:28:@1869.6]
  wire [1:0] _GEN_289; // @[issue.scala 153:28:@1869.6]
  wire [5:0] _GEN_290; // @[issue.scala 153:28:@1870.6]
  wire [5:0] _GEN_291; // @[issue.scala 153:28:@1870.6]
  wire  _GEN_292; // @[issue.scala 153:28:@1871.6]
  wire  _GEN_293; // @[issue.scala 153:28:@1871.6]
  wire [25:0] _GEN_294; // @[issue.scala 153:28:@1872.6]
  wire [25:0] _GEN_295; // @[issue.scala 153:28:@1872.6]
  wire [4:0] _GEN_298; // @[issue.scala 153:28:@1874.6]
  wire [4:0] _GEN_299; // @[issue.scala 153:28:@1874.6]
  wire  _GEN_300; // @[issue.scala 153:28:@1875.6]
  wire  _GEN_301; // @[issue.scala 153:28:@1875.6]
  wire [4:0] _GEN_302; // @[issue.scala 153:28:@1876.6]
  wire [4:0] _GEN_303; // @[issue.scala 153:28:@1876.6]
  wire [4:0] _GEN_306; // @[issue.scala 153:28:@1878.6]
  wire [4:0] _GEN_307; // @[issue.scala 153:28:@1878.6]
  wire  _GEN_308; // @[issue.scala 153:28:@1879.6]
  wire  _GEN_309; // @[issue.scala 153:28:@1879.6]
  wire  _GEN_312; // @[issue.scala 150:47:@1857.4]
  wire  _GEN_313; // @[issue.scala 150:47:@1857.4]
  wire [63:0] _GEN_314; // @[issue.scala 150:47:@1857.4]
  wire [63:0] _GEN_315; // @[issue.scala 150:47:@1857.4]
  wire  _GEN_320; // @[issue.scala 150:47:@1857.4]
  wire  _GEN_321; // @[issue.scala 150:47:@1857.4]
  wire  _GEN_322; // @[issue.scala 150:47:@1857.4]
  wire  _GEN_323; // @[issue.scala 150:47:@1857.4]
  wire [2:0] _GEN_324; // @[issue.scala 150:47:@1857.4]
  wire [2:0] _GEN_325; // @[issue.scala 150:47:@1857.4]
  wire [2:0] _GEN_326; // @[issue.scala 150:47:@1857.4]
  wire [2:0] _GEN_327; // @[issue.scala 150:47:@1857.4]
  wire [3:0] _GEN_328; // @[issue.scala 150:47:@1857.4]
  wire [3:0] _GEN_329; // @[issue.scala 150:47:@1857.4]
  wire [1:0] _GEN_332; // @[issue.scala 150:47:@1857.4]
  wire [1:0] _GEN_333; // @[issue.scala 150:47:@1857.4]
  wire [5:0] _GEN_334; // @[issue.scala 150:47:@1857.4]
  wire [5:0] _GEN_335; // @[issue.scala 150:47:@1857.4]
  wire  _GEN_336; // @[issue.scala 150:47:@1857.4]
  wire  _GEN_337; // @[issue.scala 150:47:@1857.4]
  wire [25:0] _GEN_338; // @[issue.scala 150:47:@1857.4]
  wire [25:0] _GEN_339; // @[issue.scala 150:47:@1857.4]
  wire [4:0] _GEN_342; // @[issue.scala 150:47:@1857.4]
  wire [4:0] _GEN_343; // @[issue.scala 150:47:@1857.4]
  wire  _GEN_344; // @[issue.scala 150:47:@1857.4]
  wire  _GEN_345; // @[issue.scala 150:47:@1857.4]
  wire [4:0] _GEN_346; // @[issue.scala 150:47:@1857.4]
  wire [4:0] _GEN_347; // @[issue.scala 150:47:@1857.4]
  wire [4:0] _GEN_350; // @[issue.scala 150:47:@1857.4]
  wire [4:0] _GEN_351; // @[issue.scala 150:47:@1857.4]
  wire  _GEN_352; // @[issue.scala 150:47:@1857.4]
  wire  _GEN_353; // @[issue.scala 150:47:@1857.4]
  wire [63:0] _GEN_354; // @[issue.scala 158:28:@1903.6]
  wire [63:0] _GEN_355; // @[issue.scala 158:28:@1903.6]
  wire  _GEN_360; // @[issue.scala 158:28:@1906.6]
  wire  _GEN_361; // @[issue.scala 158:28:@1906.6]
  wire  _GEN_362; // @[issue.scala 158:28:@1907.6]
  wire  _GEN_363; // @[issue.scala 158:28:@1907.6]
  wire [2:0] _GEN_364; // @[issue.scala 158:28:@1908.6]
  wire [2:0] _GEN_365; // @[issue.scala 158:28:@1908.6]
  wire [2:0] _GEN_366; // @[issue.scala 158:28:@1909.6]
  wire [2:0] _GEN_367; // @[issue.scala 158:28:@1909.6]
  wire [3:0] _GEN_368; // @[issue.scala 158:28:@1910.6]
  wire [3:0] _GEN_369; // @[issue.scala 158:28:@1910.6]
  wire [1:0] _GEN_372; // @[issue.scala 158:28:@1912.6]
  wire [1:0] _GEN_373; // @[issue.scala 158:28:@1912.6]
  wire [5:0] _GEN_374; // @[issue.scala 158:28:@1913.6]
  wire [5:0] _GEN_375; // @[issue.scala 158:28:@1913.6]
  wire  _GEN_376; // @[issue.scala 158:28:@1914.6]
  wire  _GEN_377; // @[issue.scala 158:28:@1914.6]
  wire [25:0] _GEN_378; // @[issue.scala 158:28:@1915.6]
  wire [25:0] _GEN_379; // @[issue.scala 158:28:@1915.6]
  wire [4:0] _GEN_382; // @[issue.scala 158:28:@1917.6]
  wire [4:0] _GEN_383; // @[issue.scala 158:28:@1917.6]
  wire  _GEN_384; // @[issue.scala 158:28:@1918.6]
  wire  _GEN_385; // @[issue.scala 158:28:@1918.6]
  wire [4:0] _GEN_386; // @[issue.scala 158:28:@1919.6]
  wire [4:0] _GEN_387; // @[issue.scala 158:28:@1919.6]
  wire [4:0] _GEN_390; // @[issue.scala 158:28:@1921.6]
  wire [4:0] _GEN_391; // @[issue.scala 158:28:@1921.6]
  wire  _GEN_392; // @[issue.scala 158:28:@1922.6]
  wire  _GEN_393; // @[issue.scala 158:28:@1922.6]
  wire  _GEN_394; // @[issue.scala 159:30:@1923.6]
  wire  _GEN_395; // @[issue.scala 159:30:@1923.6]
  wire [63:0] _GEN_398; // @[issue.scala 157:24:@1881.4]
  wire [63:0] _GEN_399; // @[issue.scala 157:24:@1881.4]
  wire  _GEN_404; // @[issue.scala 157:24:@1881.4]
  wire  _GEN_405; // @[issue.scala 157:24:@1881.4]
  wire  _GEN_406; // @[issue.scala 157:24:@1881.4]
  wire  _GEN_407; // @[issue.scala 157:24:@1881.4]
  wire [2:0] _GEN_408; // @[issue.scala 157:24:@1881.4]
  wire [2:0] _GEN_409; // @[issue.scala 157:24:@1881.4]
  wire [2:0] _GEN_410; // @[issue.scala 157:24:@1881.4]
  wire [2:0] _GEN_411; // @[issue.scala 157:24:@1881.4]
  wire [3:0] _GEN_412; // @[issue.scala 157:24:@1881.4]
  wire [3:0] _GEN_413; // @[issue.scala 157:24:@1881.4]
  wire [1:0] _GEN_416; // @[issue.scala 157:24:@1881.4]
  wire [1:0] _GEN_417; // @[issue.scala 157:24:@1881.4]
  wire [5:0] _GEN_418; // @[issue.scala 157:24:@1881.4]
  wire [5:0] _GEN_419; // @[issue.scala 157:24:@1881.4]
  wire  _GEN_420; // @[issue.scala 157:24:@1881.4]
  wire  _GEN_421; // @[issue.scala 157:24:@1881.4]
  wire [25:0] _GEN_422; // @[issue.scala 157:24:@1881.4]
  wire [25:0] _GEN_423; // @[issue.scala 157:24:@1881.4]
  wire [4:0] _GEN_426; // @[issue.scala 157:24:@1881.4]
  wire [4:0] _GEN_427; // @[issue.scala 157:24:@1881.4]
  wire  _GEN_428; // @[issue.scala 157:24:@1881.4]
  wire  _GEN_429; // @[issue.scala 157:24:@1881.4]
  wire [4:0] _GEN_430; // @[issue.scala 157:24:@1881.4]
  wire [4:0] _GEN_431; // @[issue.scala 157:24:@1881.4]
  wire [4:0] _GEN_434; // @[issue.scala 157:24:@1881.4]
  wire [4:0] _GEN_435; // @[issue.scala 157:24:@1881.4]
  wire  _GEN_436; // @[issue.scala 157:24:@1881.4]
  wire  _GEN_437; // @[issue.scala 157:24:@1881.4]
  wire  _GEN_438; // @[issue.scala 157:24:@1881.4]
  wire  _GEN_439; // @[issue.scala 157:24:@1881.4]
  Queue Queue ( // @[issue.scala 94:113:@1611.4]
    .clock(Queue_clock),
    .reset(Queue_reset),
    .io_enq_ready(Queue_io_enq_ready),
    .io_enq_valid(Queue_io_enq_valid),
    .io_enq_bits_rd_valid(Queue_io_enq_bits_rd_valid),
    .io_enq_bits_rd_bits(Queue_io_enq_bits_rd_bits),
    .io_enq_bits_rs1_bits(Queue_io_enq_bits_rs1_bits),
    .io_enq_bits_rs2_valid(Queue_io_enq_bits_rs2_valid),
    .io_enq_bits_rs2_bits(Queue_io_enq_bits_rs2_bits),
    .io_enq_bits_imm_bits(Queue_io_enq_bits_imm_bits),
    .io_enq_bits_shift_val_valid(Queue_io_enq_bits_shift_val_valid),
    .io_enq_bits_shift_val_bits(Queue_io_enq_bits_shift_val_bits),
    .io_enq_bits_shift_type(Queue_io_enq_bits_shift_type),
    .io_enq_bits_cond_bits(Queue_io_enq_bits_cond_bits),
    .io_enq_bits_itype(Queue_io_enq_bits_itype),
    .io_enq_bits_op(Queue_io_enq_bits_op),
    .io_enq_bits_nzcv_en(Queue_io_enq_bits_nzcv_en),
    .io_enq_bits_tag(Queue_io_enq_bits_tag),
    .io_enq_bits_pc(Queue_io_enq_bits_pc),
    .io_deq_ready(Queue_io_deq_ready),
    .io_deq_valid(Queue_io_deq_valid),
    .io_deq_bits_rd_valid(Queue_io_deq_bits_rd_valid),
    .io_deq_bits_rd_bits(Queue_io_deq_bits_rd_bits),
    .io_deq_bits_rs1_bits(Queue_io_deq_bits_rs1_bits),
    .io_deq_bits_rs2_valid(Queue_io_deq_bits_rs2_valid),
    .io_deq_bits_rs2_bits(Queue_io_deq_bits_rs2_bits),
    .io_deq_bits_imm_bits(Queue_io_deq_bits_imm_bits),
    .io_deq_bits_shift_val_valid(Queue_io_deq_bits_shift_val_valid),
    .io_deq_bits_shift_val_bits(Queue_io_deq_bits_shift_val_bits),
    .io_deq_bits_shift_type(Queue_io_deq_bits_shift_type),
    .io_deq_bits_cond_bits(Queue_io_deq_bits_cond_bits),
    .io_deq_bits_itype(Queue_io_deq_bits_itype),
    .io_deq_bits_op(Queue_io_deq_bits_op),
    .io_deq_bits_nzcv_en(Queue_io_deq_bits_nzcv_en),
    .io_deq_bits_tag(Queue_io_deq_bits_tag),
    .io_deq_bits_pc(Queue_io_deq_bits_pc)
  );
  Queue Queue_1 ( // @[issue.scala 94:113:@1618.4]
    .clock(Queue_1_clock),
    .reset(Queue_1_reset),
    .io_enq_ready(Queue_1_io_enq_ready),
    .io_enq_valid(Queue_1_io_enq_valid),
    .io_enq_bits_rd_valid(Queue_1_io_enq_bits_rd_valid),
    .io_enq_bits_rd_bits(Queue_1_io_enq_bits_rd_bits),
    .io_enq_bits_rs1_bits(Queue_1_io_enq_bits_rs1_bits),
    .io_enq_bits_rs2_valid(Queue_1_io_enq_bits_rs2_valid),
    .io_enq_bits_rs2_bits(Queue_1_io_enq_bits_rs2_bits),
    .io_enq_bits_imm_bits(Queue_1_io_enq_bits_imm_bits),
    .io_enq_bits_shift_val_valid(Queue_1_io_enq_bits_shift_val_valid),
    .io_enq_bits_shift_val_bits(Queue_1_io_enq_bits_shift_val_bits),
    .io_enq_bits_shift_type(Queue_1_io_enq_bits_shift_type),
    .io_enq_bits_cond_bits(Queue_1_io_enq_bits_cond_bits),
    .io_enq_bits_itype(Queue_1_io_enq_bits_itype),
    .io_enq_bits_op(Queue_1_io_enq_bits_op),
    .io_enq_bits_nzcv_en(Queue_1_io_enq_bits_nzcv_en),
    .io_enq_bits_tag(Queue_1_io_enq_bits_tag),
    .io_enq_bits_pc(Queue_1_io_enq_bits_pc),
    .io_deq_ready(Queue_1_io_deq_ready),
    .io_deq_valid(Queue_1_io_deq_valid),
    .io_deq_bits_rd_valid(Queue_1_io_deq_bits_rd_valid),
    .io_deq_bits_rd_bits(Queue_1_io_deq_bits_rd_bits),
    .io_deq_bits_rs1_bits(Queue_1_io_deq_bits_rs1_bits),
    .io_deq_bits_rs2_valid(Queue_1_io_deq_bits_rs2_valid),
    .io_deq_bits_rs2_bits(Queue_1_io_deq_bits_rs2_bits),
    .io_deq_bits_imm_bits(Queue_1_io_deq_bits_imm_bits),
    .io_deq_bits_shift_val_valid(Queue_1_io_deq_bits_shift_val_valid),
    .io_deq_bits_shift_val_bits(Queue_1_io_deq_bits_shift_val_bits),
    .io_deq_bits_shift_type(Queue_1_io_deq_bits_shift_type),
    .io_deq_bits_cond_bits(Queue_1_io_deq_bits_cond_bits),
    .io_deq_bits_itype(Queue_1_io_deq_bits_itype),
    .io_deq_bits_op(Queue_1_io_deq_bits_op),
    .io_deq_bits_nzcv_en(Queue_1_io_deq_bits_nzcv_en),
    .io_deq_bits_tag(Queue_1_io_deq_bits_tag),
    .io_deq_bits_pc(Queue_1_io_deq_bits_pc)
  );
  RRArbiter arbiter ( // @[issue.scala 101:29:@1712.4]
    .clock(arbiter_clock),
    .reset(arbiter_reset),
    .io_ready(arbiter_io_ready),
    .io_next_ready(arbiter_io_next_ready),
    .io_next_valid(arbiter_io_next_valid),
    .io_next_bits(arbiter_io_next_bits)
  );
  assign _T_1167 = 1'h0 == io_flush_tag; // @[issue.scala 94:68:@1608.4]
  assign _T_1168 = _T_1167 & io_flush_valid; // @[issue.scala 94:85:@1609.4]
  assign _T_1193 = io_flush_tag & io_flush_valid; // @[issue.scala 94:85:@1616.4]
  assign fifo_vec_0_enq_ready = Queue_io_enq_ready; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1666.4]
  assign _T_3437 = arbiter_io_next_valid; // @[issue.scala 150:30:@1856.4]
  assign sig_next_idx = arbiter_io_next_bits; // @[:@1718.4 :@1719.4 issue.scala 142:20:@1833.4]
  assign sig_pipe_r_0 = ~ reg_pipe_v_0; // @[issue.scala 122:24:@1768.4]
  assign _GEN_176 = 1'h0 == sig_next_idx ? 1'h1 : sig_pipe_r_0; // @[issue.scala 151:38:@1858.6]
  assign fifo_vec_0_deq_valid = Queue_io_deq_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1643.4]
  assign fifo_vec_0_deq_bits_rd_valid = Queue_io_deq_bits_rd_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1642.4]
  assign fifo_vec_0_deq_bits_rd_bits = Queue_io_deq_bits_rd_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1641.4]
  assign fifo_vec_0_deq_bits_rs1_bits = Queue_io_deq_bits_rs1_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1639.4]
  assign fifo_vec_0_deq_bits_rs2_valid = Queue_io_deq_bits_rs2_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1638.4]
  assign fifo_vec_0_deq_bits_rs2_bits = Queue_io_deq_bits_rs2_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1637.4]
  assign fifo_vec_0_deq_bits_imm_bits = Queue_io_deq_bits_imm_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1635.4]
  assign fifo_vec_0_deq_bits_shift_val_valid = Queue_io_deq_bits_shift_val_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1634.4]
  assign fifo_vec_0_deq_bits_shift_val_bits = Queue_io_deq_bits_shift_val_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1633.4]
  assign fifo_vec_0_deq_bits_shift_type = Queue_io_deq_bits_shift_type; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1632.4]
  assign fifo_vec_0_deq_bits_cond_bits = Queue_io_deq_bits_cond_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1630.4]
  assign fifo_vec_0_deq_bits_itype = Queue_io_deq_bits_itype; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1629.4]
  assign fifo_vec_0_deq_bits_op = Queue_io_deq_bits_op; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1628.4]
  assign fifo_vec_0_deq_bits_nzcv_en = Queue_io_deq_bits_nzcv_en; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1627.4]
  assign fifo_vec_0_deq_bits_tag = Queue_io_deq_bits_tag; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1626.4]
  assign fifo_vec_0_deq_bits_pc = Queue_io_deq_bits_pc; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1623.4]
  assign fifo_vec_1_enq_ready = Queue_1_io_enq_ready; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1711.4]
  assign sig_pipe_r_1 = ~ reg_pipe_v_1; // @[issue.scala 122:24:@1795.4]
  assign _GEN_177 = sig_next_idx ? 1'h1 : sig_pipe_r_1; // @[issue.scala 151:38:@1858.6]
  assign fifo_vec_1_deq_valid = Queue_1_io_deq_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1688.4]
  assign fifo_vec_1_deq_bits_rd_valid = Queue_1_io_deq_bits_rd_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1687.4]
  assign fifo_vec_1_deq_bits_rd_bits = Queue_1_io_deq_bits_rd_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1686.4]
  assign fifo_vec_1_deq_bits_rs1_bits = Queue_1_io_deq_bits_rs1_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1684.4]
  assign fifo_vec_1_deq_bits_rs2_valid = Queue_1_io_deq_bits_rs2_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1683.4]
  assign fifo_vec_1_deq_bits_rs2_bits = Queue_1_io_deq_bits_rs2_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1682.4]
  assign fifo_vec_1_deq_bits_imm_bits = Queue_1_io_deq_bits_imm_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1680.4]
  assign fifo_vec_1_deq_bits_shift_val_valid = Queue_1_io_deq_bits_shift_val_valid; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1679.4]
  assign fifo_vec_1_deq_bits_shift_val_bits = Queue_1_io_deq_bits_shift_val_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1678.4]
  assign fifo_vec_1_deq_bits_shift_type = Queue_1_io_deq_bits_shift_type; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1677.4]
  assign fifo_vec_1_deq_bits_cond_bits = Queue_1_io_deq_bits_cond_bits; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1675.4]
  assign fifo_vec_1_deq_bits_itype = Queue_1_io_deq_bits_itype; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1674.4]
  assign fifo_vec_1_deq_bits_op = Queue_1_io_deq_bits_op; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1673.4]
  assign fifo_vec_1_deq_bits_nzcv_en = Queue_1_io_deq_bits_nzcv_en; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1672.4]
  assign fifo_vec_1_deq_bits_tag = Queue_1_io_deq_bits_tag; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1671.4]
  assign fifo_vec_1_deq_bits_pc = Queue_1_io_deq_bits_pc; // @[issue.scala 93:25:@1621.4 issue.scala 93:25:@1668.4]
  assign _T_3131_rd_valid = sig_pipe_r_0 ? fifo_vec_0_deq_bits_rd_valid : reg_pipe_0_rd_valid; // @[issue.scala 125:27:@1771.4]
  assign _T_3131_rd_bits = sig_pipe_r_0 ? fifo_vec_0_deq_bits_rd_bits : reg_pipe_0_rd_bits; // @[issue.scala 125:27:@1771.4]
  assign _T_3131_rs1_bits = sig_pipe_r_0 ? fifo_vec_0_deq_bits_rs1_bits : reg_pipe_0_rs1_bits; // @[issue.scala 125:27:@1771.4]
  assign _T_3131_rs2_valid = sig_pipe_r_0 ? fifo_vec_0_deq_bits_rs2_valid : reg_pipe_0_rs2_valid; // @[issue.scala 125:27:@1771.4]
  assign _T_3131_rs2_bits = sig_pipe_r_0 ? fifo_vec_0_deq_bits_rs2_bits : reg_pipe_0_rs2_bits; // @[issue.scala 125:27:@1771.4]
  assign _T_3131_imm_bits = sig_pipe_r_0 ? fifo_vec_0_deq_bits_imm_bits : reg_pipe_0_imm_bits; // @[issue.scala 125:27:@1771.4]
  assign _T_3131_shift_val_valid = sig_pipe_r_0 ? fifo_vec_0_deq_bits_shift_val_valid : reg_pipe_0_shift_val_valid; // @[issue.scala 125:27:@1771.4]
  assign _T_3131_shift_val_bits = sig_pipe_r_0 ? fifo_vec_0_deq_bits_shift_val_bits : reg_pipe_0_shift_val_bits; // @[issue.scala 125:27:@1771.4]
  assign _T_3131_shift_type = sig_pipe_r_0 ? fifo_vec_0_deq_bits_shift_type : reg_pipe_0_shift_type; // @[issue.scala 125:27:@1771.4]
  assign _T_3131_cond_bits = sig_pipe_r_0 ? fifo_vec_0_deq_bits_cond_bits : reg_pipe_0_cond_bits; // @[issue.scala 125:27:@1771.4]
  assign _T_3131_itype = sig_pipe_r_0 ? fifo_vec_0_deq_bits_itype : reg_pipe_0_itype; // @[issue.scala 125:27:@1771.4]
  assign _T_3131_op = sig_pipe_r_0 ? fifo_vec_0_deq_bits_op : reg_pipe_0_op; // @[issue.scala 125:27:@1771.4]
  assign _T_3131_nzcv_en = sig_pipe_r_0 ? fifo_vec_0_deq_bits_nzcv_en : reg_pipe_0_nzcv_en; // @[issue.scala 125:27:@1771.4]
  assign _T_3131_tag = sig_pipe_r_0 ? fifo_vec_0_deq_bits_tag : reg_pipe_0_tag; // @[issue.scala 125:27:@1771.4]
  assign _T_3131_pc = sig_pipe_r_0 ? fifo_vec_0_deq_bits_pc : reg_pipe_0_pc; // @[issue.scala 125:27:@1771.4]
  assign _T_3152 = sig_pipe_r_0 ? fifo_vec_0_deq_valid : reg_pipe_v_0; // @[issue.scala 126:27:@1792.4]
  assign _T_3154_rd_valid = sig_pipe_r_1 ? fifo_vec_1_deq_bits_rd_valid : reg_pipe_1_rd_valid; // @[issue.scala 125:27:@1798.4]
  assign _T_3154_rd_bits = sig_pipe_r_1 ? fifo_vec_1_deq_bits_rd_bits : reg_pipe_1_rd_bits; // @[issue.scala 125:27:@1798.4]
  assign _T_3154_rs1_bits = sig_pipe_r_1 ? fifo_vec_1_deq_bits_rs1_bits : reg_pipe_1_rs1_bits; // @[issue.scala 125:27:@1798.4]
  assign _T_3154_rs2_valid = sig_pipe_r_1 ? fifo_vec_1_deq_bits_rs2_valid : reg_pipe_1_rs2_valid; // @[issue.scala 125:27:@1798.4]
  assign _T_3154_rs2_bits = sig_pipe_r_1 ? fifo_vec_1_deq_bits_rs2_bits : reg_pipe_1_rs2_bits; // @[issue.scala 125:27:@1798.4]
  assign _T_3154_imm_bits = sig_pipe_r_1 ? fifo_vec_1_deq_bits_imm_bits : reg_pipe_1_imm_bits; // @[issue.scala 125:27:@1798.4]
  assign _T_3154_shift_val_valid = sig_pipe_r_1 ? fifo_vec_1_deq_bits_shift_val_valid : reg_pipe_1_shift_val_valid; // @[issue.scala 125:27:@1798.4]
  assign _T_3154_shift_val_bits = sig_pipe_r_1 ? fifo_vec_1_deq_bits_shift_val_bits : reg_pipe_1_shift_val_bits; // @[issue.scala 125:27:@1798.4]
  assign _T_3154_shift_type = sig_pipe_r_1 ? fifo_vec_1_deq_bits_shift_type : reg_pipe_1_shift_type; // @[issue.scala 125:27:@1798.4]
  assign _T_3154_cond_bits = sig_pipe_r_1 ? fifo_vec_1_deq_bits_cond_bits : reg_pipe_1_cond_bits; // @[issue.scala 125:27:@1798.4]
  assign _T_3154_itype = sig_pipe_r_1 ? fifo_vec_1_deq_bits_itype : reg_pipe_1_itype; // @[issue.scala 125:27:@1798.4]
  assign _T_3154_op = sig_pipe_r_1 ? fifo_vec_1_deq_bits_op : reg_pipe_1_op; // @[issue.scala 125:27:@1798.4]
  assign _T_3154_nzcv_en = sig_pipe_r_1 ? fifo_vec_1_deq_bits_nzcv_en : reg_pipe_1_nzcv_en; // @[issue.scala 125:27:@1798.4]
  assign _T_3154_tag = sig_pipe_r_1 ? fifo_vec_1_deq_bits_tag : reg_pipe_1_tag; // @[issue.scala 125:27:@1798.4]
  assign _T_3154_pc = sig_pipe_r_1 ? fifo_vec_1_deq_bits_pc : reg_pipe_1_pc; // @[issue.scala 125:27:@1798.4]
  assign _T_3175 = sig_pipe_r_1 ? fifo_vec_1_deq_valid : reg_pipe_v_1; // @[issue.scala 126:27:@1819.4]
  assign rfile_wb_pending = io_commitReg_valid & io_commitReg_bits_exe_bits_rd_valid; // @[issue.scala 133:45:@1822.4]
  assign _GEN_115 = io_commitReg_bits_tag ? reg_pipe_1_rs1_bits : reg_pipe_0_rs1_bits; // @[issue.scala 135:48:@1823.4]
  assign _GEN_117 = io_commitReg_bits_tag ? reg_pipe_1_rs2_bits : reg_pipe_0_rs2_bits; // @[issue.scala 135:48:@1823.4]
  assign _T_3259 = _GEN_115 == io_commitReg_bits_exe_bits_rd_bits; // @[issue.scala 135:48:@1823.4]
  assign _T_3343 = _GEN_117 == io_commitReg_bits_exe_bits_rd_bits; // @[issue.scala 136:50:@1824.4]
  assign _T_3344 = _T_3259 | _T_3343; // @[issue.scala 135:88:@1825.4]
  assign exe_stall = rfile_wb_pending & _T_3344; // @[issue.scala 134:33:@1826.4]
  assign _T_3351 = exe_stall == 1'h0; // @[issue.scala 137:77:@1828.4]
  assign _GEN_133 = io_commitReg_bits_tag ? reg_pipe_v_1 : reg_pipe_v_0; // @[issue.scala 137:74:@1829.4]
  assign _T_3352 = _GEN_133 & _T_3351; // @[issue.scala 137:74:@1829.4]
  assign _GEN_134 = 1'h0 == io_commitReg_bits_tag ? _T_3352 : reg_pipe_v_0; // @[issue.scala 137:37:@1830.4]
  assign _GEN_135 = io_commitReg_bits_tag ? _T_3352 : reg_pipe_v_1; // @[issue.scala 137:37:@1830.4]
  assign _GEN_397 = io_flush_tag ? 1'h0 : _GEN_135; // @[issue.scala 160:30:@1924.6]
  assign sig_pipe_i_1 = io_flush_valid ? _GEN_397 : _GEN_135; // @[issue.scala 157:24:@1881.4]
  assign _GEN_396 = 1'h0 == io_flush_tag ? 1'h0 : _GEN_134; // @[issue.scala 160:30:@1924.6]
  assign sig_pipe_i_0 = io_flush_valid ? _GEN_396 : _GEN_134; // @[issue.scala 157:24:@1881.4]
  assign _GEN_248 = sig_next_idx ? fifo_vec_1_deq_valid : fifo_vec_0_deq_valid; // @[issue.scala 152:30:@1859.6]
  assign _GEN_178 = 1'h0 == sig_next_idx ? _GEN_248 : _T_3152; // @[issue.scala 152:30:@1859.6]
  assign _GEN_179 = sig_next_idx ? _GEN_248 : _T_3175; // @[issue.scala 152:30:@1859.6]
  assign _GEN_249 = sig_next_idx ? fifo_vec_1_deq_bits_rd_valid : fifo_vec_0_deq_bits_rd_valid; // @[issue.scala 152:30:@1859.6]
  assign _GEN_250 = sig_next_idx ? fifo_vec_1_deq_bits_rd_bits : fifo_vec_0_deq_bits_rd_bits; // @[issue.scala 152:30:@1859.6]
  assign _GEN_252 = sig_next_idx ? fifo_vec_1_deq_bits_rs1_bits : fifo_vec_0_deq_bits_rs1_bits; // @[issue.scala 152:30:@1859.6]
  assign _GEN_253 = sig_next_idx ? fifo_vec_1_deq_bits_rs2_valid : fifo_vec_0_deq_bits_rs2_valid; // @[issue.scala 152:30:@1859.6]
  assign _GEN_254 = sig_next_idx ? fifo_vec_1_deq_bits_rs2_bits : fifo_vec_0_deq_bits_rs2_bits; // @[issue.scala 152:30:@1859.6]
  assign _GEN_256 = sig_next_idx ? fifo_vec_1_deq_bits_imm_bits : fifo_vec_0_deq_bits_imm_bits; // @[issue.scala 152:30:@1859.6]
  assign _GEN_257 = sig_next_idx ? fifo_vec_1_deq_bits_shift_val_valid : fifo_vec_0_deq_bits_shift_val_valid; // @[issue.scala 152:30:@1859.6]
  assign _GEN_258 = sig_next_idx ? fifo_vec_1_deq_bits_shift_val_bits : fifo_vec_0_deq_bits_shift_val_bits; // @[issue.scala 152:30:@1859.6]
  assign _GEN_259 = sig_next_idx ? fifo_vec_1_deq_bits_shift_type : fifo_vec_0_deq_bits_shift_type; // @[issue.scala 152:30:@1859.6]
  assign _GEN_261 = sig_next_idx ? fifo_vec_1_deq_bits_cond_bits : fifo_vec_0_deq_bits_cond_bits; // @[issue.scala 152:30:@1859.6]
  assign _GEN_262 = sig_next_idx ? fifo_vec_1_deq_bits_itype : fifo_vec_0_deq_bits_itype; // @[issue.scala 152:30:@1859.6]
  assign _GEN_263 = sig_next_idx ? fifo_vec_1_deq_bits_op : fifo_vec_0_deq_bits_op; // @[issue.scala 152:30:@1859.6]
  assign _GEN_264 = sig_next_idx ? fifo_vec_1_deq_bits_nzcv_en : fifo_vec_0_deq_bits_nzcv_en; // @[issue.scala 152:30:@1859.6]
  assign _GEN_265 = sig_next_idx ? fifo_vec_1_deq_bits_tag : fifo_vec_0_deq_bits_tag; // @[issue.scala 152:30:@1859.6]
  assign _GEN_268 = sig_next_idx ? fifo_vec_1_deq_bits_pc : fifo_vec_0_deq_bits_pc; // @[issue.scala 152:30:@1859.6]
  assign _GEN_270 = 1'h0 == sig_next_idx ? _GEN_268 : _T_3131_pc; // @[issue.scala 153:28:@1860.6]
  assign _GEN_271 = sig_next_idx ? _GEN_268 : _T_3154_pc; // @[issue.scala 153:28:@1860.6]
  assign _GEN_276 = 1'h0 == sig_next_idx ? _GEN_265 : _T_3131_tag; // @[issue.scala 153:28:@1863.6]
  assign _GEN_277 = sig_next_idx ? _GEN_265 : _T_3154_tag; // @[issue.scala 153:28:@1863.6]
  assign _GEN_278 = 1'h0 == sig_next_idx ? _GEN_264 : _T_3131_nzcv_en; // @[issue.scala 153:28:@1864.6]
  assign _GEN_279 = sig_next_idx ? _GEN_264 : _T_3154_nzcv_en; // @[issue.scala 153:28:@1864.6]
  assign _GEN_280 = 1'h0 == sig_next_idx ? _GEN_263 : _T_3131_op; // @[issue.scala 153:28:@1865.6]
  assign _GEN_281 = sig_next_idx ? _GEN_263 : _T_3154_op; // @[issue.scala 153:28:@1865.6]
  assign _GEN_282 = 1'h0 == sig_next_idx ? _GEN_262 : _T_3131_itype; // @[issue.scala 153:28:@1866.6]
  assign _GEN_283 = sig_next_idx ? _GEN_262 : _T_3154_itype; // @[issue.scala 153:28:@1866.6]
  assign _GEN_284 = 1'h0 == sig_next_idx ? _GEN_261 : _T_3131_cond_bits; // @[issue.scala 153:28:@1867.6]
  assign _GEN_285 = sig_next_idx ? _GEN_261 : _T_3154_cond_bits; // @[issue.scala 153:28:@1867.6]
  assign _GEN_288 = 1'h0 == sig_next_idx ? _GEN_259 : _T_3131_shift_type; // @[issue.scala 153:28:@1869.6]
  assign _GEN_289 = sig_next_idx ? _GEN_259 : _T_3154_shift_type; // @[issue.scala 153:28:@1869.6]
  assign _GEN_290 = 1'h0 == sig_next_idx ? _GEN_258 : _T_3131_shift_val_bits; // @[issue.scala 153:28:@1870.6]
  assign _GEN_291 = sig_next_idx ? _GEN_258 : _T_3154_shift_val_bits; // @[issue.scala 153:28:@1870.6]
  assign _GEN_292 = 1'h0 == sig_next_idx ? _GEN_257 : _T_3131_shift_val_valid; // @[issue.scala 153:28:@1871.6]
  assign _GEN_293 = sig_next_idx ? _GEN_257 : _T_3154_shift_val_valid; // @[issue.scala 153:28:@1871.6]
  assign _GEN_294 = 1'h0 == sig_next_idx ? _GEN_256 : _T_3131_imm_bits; // @[issue.scala 153:28:@1872.6]
  assign _GEN_295 = sig_next_idx ? _GEN_256 : _T_3154_imm_bits; // @[issue.scala 153:28:@1872.6]
  assign _GEN_298 = 1'h0 == sig_next_idx ? _GEN_254 : _T_3131_rs2_bits; // @[issue.scala 153:28:@1874.6]
  assign _GEN_299 = sig_next_idx ? _GEN_254 : _T_3154_rs2_bits; // @[issue.scala 153:28:@1874.6]
  assign _GEN_300 = 1'h0 == sig_next_idx ? _GEN_253 : _T_3131_rs2_valid; // @[issue.scala 153:28:@1875.6]
  assign _GEN_301 = sig_next_idx ? _GEN_253 : _T_3154_rs2_valid; // @[issue.scala 153:28:@1875.6]
  assign _GEN_302 = 1'h0 == sig_next_idx ? _GEN_252 : _T_3131_rs1_bits; // @[issue.scala 153:28:@1876.6]
  assign _GEN_303 = sig_next_idx ? _GEN_252 : _T_3154_rs1_bits; // @[issue.scala 153:28:@1876.6]
  assign _GEN_306 = 1'h0 == sig_next_idx ? _GEN_250 : _T_3131_rd_bits; // @[issue.scala 153:28:@1878.6]
  assign _GEN_307 = sig_next_idx ? _GEN_250 : _T_3154_rd_bits; // @[issue.scala 153:28:@1878.6]
  assign _GEN_308 = 1'h0 == sig_next_idx ? _GEN_249 : _T_3131_rd_valid; // @[issue.scala 153:28:@1879.6]
  assign _GEN_309 = sig_next_idx ? _GEN_249 : _T_3154_rd_valid; // @[issue.scala 153:28:@1879.6]
  assign _GEN_312 = _T_3437 ? _GEN_178 : _T_3152; // @[issue.scala 150:47:@1857.4]
  assign _GEN_313 = _T_3437 ? _GEN_179 : _T_3175; // @[issue.scala 150:47:@1857.4]
  assign _GEN_314 = _T_3437 ? _GEN_270 : _T_3131_pc; // @[issue.scala 150:47:@1857.4]
  assign _GEN_315 = _T_3437 ? _GEN_271 : _T_3154_pc; // @[issue.scala 150:47:@1857.4]
  assign _GEN_320 = _T_3437 ? _GEN_276 : _T_3131_tag; // @[issue.scala 150:47:@1857.4]
  assign _GEN_321 = _T_3437 ? _GEN_277 : _T_3154_tag; // @[issue.scala 150:47:@1857.4]
  assign _GEN_322 = _T_3437 ? _GEN_278 : _T_3131_nzcv_en; // @[issue.scala 150:47:@1857.4]
  assign _GEN_323 = _T_3437 ? _GEN_279 : _T_3154_nzcv_en; // @[issue.scala 150:47:@1857.4]
  assign _GEN_324 = _T_3437 ? _GEN_280 : _T_3131_op; // @[issue.scala 150:47:@1857.4]
  assign _GEN_325 = _T_3437 ? _GEN_281 : _T_3154_op; // @[issue.scala 150:47:@1857.4]
  assign _GEN_326 = _T_3437 ? _GEN_282 : _T_3131_itype; // @[issue.scala 150:47:@1857.4]
  assign _GEN_327 = _T_3437 ? _GEN_283 : _T_3154_itype; // @[issue.scala 150:47:@1857.4]
  assign _GEN_328 = _T_3437 ? _GEN_284 : _T_3131_cond_bits; // @[issue.scala 150:47:@1857.4]
  assign _GEN_329 = _T_3437 ? _GEN_285 : _T_3154_cond_bits; // @[issue.scala 150:47:@1857.4]
  assign _GEN_332 = _T_3437 ? _GEN_288 : _T_3131_shift_type; // @[issue.scala 150:47:@1857.4]
  assign _GEN_333 = _T_3437 ? _GEN_289 : _T_3154_shift_type; // @[issue.scala 150:47:@1857.4]
  assign _GEN_334 = _T_3437 ? _GEN_290 : _T_3131_shift_val_bits; // @[issue.scala 150:47:@1857.4]
  assign _GEN_335 = _T_3437 ? _GEN_291 : _T_3154_shift_val_bits; // @[issue.scala 150:47:@1857.4]
  assign _GEN_336 = _T_3437 ? _GEN_292 : _T_3131_shift_val_valid; // @[issue.scala 150:47:@1857.4]
  assign _GEN_337 = _T_3437 ? _GEN_293 : _T_3154_shift_val_valid; // @[issue.scala 150:47:@1857.4]
  assign _GEN_338 = _T_3437 ? _GEN_294 : _T_3131_imm_bits; // @[issue.scala 150:47:@1857.4]
  assign _GEN_339 = _T_3437 ? _GEN_295 : _T_3154_imm_bits; // @[issue.scala 150:47:@1857.4]
  assign _GEN_342 = _T_3437 ? _GEN_298 : _T_3131_rs2_bits; // @[issue.scala 150:47:@1857.4]
  assign _GEN_343 = _T_3437 ? _GEN_299 : _T_3154_rs2_bits; // @[issue.scala 150:47:@1857.4]
  assign _GEN_344 = _T_3437 ? _GEN_300 : _T_3131_rs2_valid; // @[issue.scala 150:47:@1857.4]
  assign _GEN_345 = _T_3437 ? _GEN_301 : _T_3154_rs2_valid; // @[issue.scala 150:47:@1857.4]
  assign _GEN_346 = _T_3437 ? _GEN_302 : _T_3131_rs1_bits; // @[issue.scala 150:47:@1857.4]
  assign _GEN_347 = _T_3437 ? _GEN_303 : _T_3154_rs1_bits; // @[issue.scala 150:47:@1857.4]
  assign _GEN_350 = _T_3437 ? _GEN_306 : _T_3131_rd_bits; // @[issue.scala 150:47:@1857.4]
  assign _GEN_351 = _T_3437 ? _GEN_307 : _T_3154_rd_bits; // @[issue.scala 150:47:@1857.4]
  assign _GEN_352 = _T_3437 ? _GEN_308 : _T_3131_rd_valid; // @[issue.scala 150:47:@1857.4]
  assign _GEN_353 = _T_3437 ? _GEN_309 : _T_3154_rd_valid; // @[issue.scala 150:47:@1857.4]
  assign _GEN_354 = 1'h0 == io_flush_tag ? 64'h0 : _GEN_314; // @[issue.scala 158:28:@1903.6]
  assign _GEN_355 = io_flush_tag ? 64'h0 : _GEN_315; // @[issue.scala 158:28:@1903.6]
  assign _GEN_360 = 1'h0 == io_flush_tag ? 1'h0 : _GEN_320; // @[issue.scala 158:28:@1906.6]
  assign _GEN_361 = io_flush_tag ? 1'h0 : _GEN_321; // @[issue.scala 158:28:@1906.6]
  assign _GEN_362 = 1'h0 == io_flush_tag ? 1'h0 : _GEN_322; // @[issue.scala 158:28:@1907.6]
  assign _GEN_363 = io_flush_tag ? 1'h0 : _GEN_323; // @[issue.scala 158:28:@1907.6]
  assign _GEN_364 = 1'h0 == io_flush_tag ? 3'h0 : _GEN_324; // @[issue.scala 158:28:@1908.6]
  assign _GEN_365 = io_flush_tag ? 3'h0 : _GEN_325; // @[issue.scala 158:28:@1908.6]
  assign _GEN_366 = 1'h0 == io_flush_tag ? 3'h0 : _GEN_326; // @[issue.scala 158:28:@1909.6]
  assign _GEN_367 = io_flush_tag ? 3'h0 : _GEN_327; // @[issue.scala 158:28:@1909.6]
  assign _GEN_368 = 1'h0 == io_flush_tag ? 4'h0 : _GEN_328; // @[issue.scala 158:28:@1910.6]
  assign _GEN_369 = io_flush_tag ? 4'h0 : _GEN_329; // @[issue.scala 158:28:@1910.6]
  assign _GEN_372 = 1'h0 == io_flush_tag ? 2'h0 : _GEN_332; // @[issue.scala 158:28:@1912.6]
  assign _GEN_373 = io_flush_tag ? 2'h0 : _GEN_333; // @[issue.scala 158:28:@1912.6]
  assign _GEN_374 = 1'h0 == io_flush_tag ? 6'h0 : _GEN_334; // @[issue.scala 158:28:@1913.6]
  assign _GEN_375 = io_flush_tag ? 6'h0 : _GEN_335; // @[issue.scala 158:28:@1913.6]
  assign _GEN_376 = 1'h0 == io_flush_tag ? 1'h0 : _GEN_336; // @[issue.scala 158:28:@1914.6]
  assign _GEN_377 = io_flush_tag ? 1'h0 : _GEN_337; // @[issue.scala 158:28:@1914.6]
  assign _GEN_378 = 1'h0 == io_flush_tag ? 26'h0 : _GEN_338; // @[issue.scala 158:28:@1915.6]
  assign _GEN_379 = io_flush_tag ? 26'h0 : _GEN_339; // @[issue.scala 158:28:@1915.6]
  assign _GEN_382 = 1'h0 == io_flush_tag ? 5'h0 : _GEN_342; // @[issue.scala 158:28:@1917.6]
  assign _GEN_383 = io_flush_tag ? 5'h0 : _GEN_343; // @[issue.scala 158:28:@1917.6]
  assign _GEN_384 = 1'h0 == io_flush_tag ? 1'h0 : _GEN_344; // @[issue.scala 158:28:@1918.6]
  assign _GEN_385 = io_flush_tag ? 1'h0 : _GEN_345; // @[issue.scala 158:28:@1918.6]
  assign _GEN_386 = 1'h0 == io_flush_tag ? 5'h0 : _GEN_346; // @[issue.scala 158:28:@1919.6]
  assign _GEN_387 = io_flush_tag ? 5'h0 : _GEN_347; // @[issue.scala 158:28:@1919.6]
  assign _GEN_390 = 1'h0 == io_flush_tag ? 5'h0 : _GEN_350; // @[issue.scala 158:28:@1921.6]
  assign _GEN_391 = io_flush_tag ? 5'h0 : _GEN_351; // @[issue.scala 158:28:@1921.6]
  assign _GEN_392 = 1'h0 == io_flush_tag ? 1'h0 : _GEN_352; // @[issue.scala 158:28:@1922.6]
  assign _GEN_393 = io_flush_tag ? 1'h0 : _GEN_353; // @[issue.scala 158:28:@1922.6]
  assign _GEN_394 = 1'h0 == io_flush_tag ? 1'h0 : _GEN_312; // @[issue.scala 159:30:@1923.6]
  assign _GEN_395 = io_flush_tag ? 1'h0 : _GEN_313; // @[issue.scala 159:30:@1923.6]
  assign _GEN_398 = io_flush_valid ? _GEN_354 : _GEN_314; // @[issue.scala 157:24:@1881.4]
  assign _GEN_399 = io_flush_valid ? _GEN_355 : _GEN_315; // @[issue.scala 157:24:@1881.4]
  assign _GEN_404 = io_flush_valid ? _GEN_360 : _GEN_320; // @[issue.scala 157:24:@1881.4]
  assign _GEN_405 = io_flush_valid ? _GEN_361 : _GEN_321; // @[issue.scala 157:24:@1881.4]
  assign _GEN_406 = io_flush_valid ? _GEN_362 : _GEN_322; // @[issue.scala 157:24:@1881.4]
  assign _GEN_407 = io_flush_valid ? _GEN_363 : _GEN_323; // @[issue.scala 157:24:@1881.4]
  assign _GEN_408 = io_flush_valid ? _GEN_364 : _GEN_324; // @[issue.scala 157:24:@1881.4]
  assign _GEN_409 = io_flush_valid ? _GEN_365 : _GEN_325; // @[issue.scala 157:24:@1881.4]
  assign _GEN_410 = io_flush_valid ? _GEN_366 : _GEN_326; // @[issue.scala 157:24:@1881.4]
  assign _GEN_411 = io_flush_valid ? _GEN_367 : _GEN_327; // @[issue.scala 157:24:@1881.4]
  assign _GEN_412 = io_flush_valid ? _GEN_368 : _GEN_328; // @[issue.scala 157:24:@1881.4]
  assign _GEN_413 = io_flush_valid ? _GEN_369 : _GEN_329; // @[issue.scala 157:24:@1881.4]
  assign _GEN_416 = io_flush_valid ? _GEN_372 : _GEN_332; // @[issue.scala 157:24:@1881.4]
  assign _GEN_417 = io_flush_valid ? _GEN_373 : _GEN_333; // @[issue.scala 157:24:@1881.4]
  assign _GEN_418 = io_flush_valid ? _GEN_374 : _GEN_334; // @[issue.scala 157:24:@1881.4]
  assign _GEN_419 = io_flush_valid ? _GEN_375 : _GEN_335; // @[issue.scala 157:24:@1881.4]
  assign _GEN_420 = io_flush_valid ? _GEN_376 : _GEN_336; // @[issue.scala 157:24:@1881.4]
  assign _GEN_421 = io_flush_valid ? _GEN_377 : _GEN_337; // @[issue.scala 157:24:@1881.4]
  assign _GEN_422 = io_flush_valid ? _GEN_378 : _GEN_338; // @[issue.scala 157:24:@1881.4]
  assign _GEN_423 = io_flush_valid ? _GEN_379 : _GEN_339; // @[issue.scala 157:24:@1881.4]
  assign _GEN_426 = io_flush_valid ? _GEN_382 : _GEN_342; // @[issue.scala 157:24:@1881.4]
  assign _GEN_427 = io_flush_valid ? _GEN_383 : _GEN_343; // @[issue.scala 157:24:@1881.4]
  assign _GEN_428 = io_flush_valid ? _GEN_384 : _GEN_344; // @[issue.scala 157:24:@1881.4]
  assign _GEN_429 = io_flush_valid ? _GEN_385 : _GEN_345; // @[issue.scala 157:24:@1881.4]
  assign _GEN_430 = io_flush_valid ? _GEN_386 : _GEN_346; // @[issue.scala 157:24:@1881.4]
  assign _GEN_431 = io_flush_valid ? _GEN_387 : _GEN_347; // @[issue.scala 157:24:@1881.4]
  assign _GEN_434 = io_flush_valid ? _GEN_390 : _GEN_350; // @[issue.scala 157:24:@1881.4]
  assign _GEN_435 = io_flush_valid ? _GEN_391 : _GEN_351; // @[issue.scala 157:24:@1881.4]
  assign _GEN_436 = io_flush_valid ? _GEN_392 : _GEN_352; // @[issue.scala 157:24:@1881.4]
  assign _GEN_437 = io_flush_valid ? _GEN_393 : _GEN_353; // @[issue.scala 157:24:@1881.4]
  assign _GEN_438 = io_flush_valid ? _GEN_394 : _GEN_312; // @[issue.scala 157:24:@1881.4]
  assign _GEN_439 = io_flush_valid ? _GEN_395 : _GEN_313; // @[issue.scala 157:24:@1881.4]
  assign io_enq_ready = io_enq_bits_tag ? fifo_vec_1_enq_ready : fifo_vec_0_enq_ready; // @[issue.scala 116:16:@1766.4]
  assign io_deq_valid = arbiter_io_next_valid; // @[issue.scala 145:16:@1854.4]
  assign io_deq_bits_rd_valid = sig_next_idx ? reg_pipe_1_rd_valid : reg_pipe_0_rd_valid; // @[issue.scala 144:16:@1853.4]
  assign io_deq_bits_rd_bits = sig_next_idx ? reg_pipe_1_rd_bits : reg_pipe_0_rd_bits; // @[issue.scala 144:16:@1852.4]
  assign io_deq_bits_rs1_bits = sig_next_idx ? reg_pipe_1_rs1_bits : reg_pipe_0_rs1_bits; // @[issue.scala 144:16:@1850.4]
  assign io_deq_bits_rs2_valid = sig_next_idx ? reg_pipe_1_rs2_valid : reg_pipe_0_rs2_valid; // @[issue.scala 144:16:@1849.4]
  assign io_deq_bits_rs2_bits = sig_next_idx ? reg_pipe_1_rs2_bits : reg_pipe_0_rs2_bits; // @[issue.scala 144:16:@1848.4]
  assign io_deq_bits_imm_bits = sig_next_idx ? reg_pipe_1_imm_bits : reg_pipe_0_imm_bits; // @[issue.scala 144:16:@1846.4]
  assign io_deq_bits_shift_val_valid = sig_next_idx ? reg_pipe_1_shift_val_valid : reg_pipe_0_shift_val_valid; // @[issue.scala 144:16:@1845.4]
  assign io_deq_bits_shift_val_bits = sig_next_idx ? reg_pipe_1_shift_val_bits : reg_pipe_0_shift_val_bits; // @[issue.scala 144:16:@1844.4]
  assign io_deq_bits_shift_type = sig_next_idx ? reg_pipe_1_shift_type : reg_pipe_0_shift_type; // @[issue.scala 144:16:@1843.4]
  assign io_deq_bits_cond_bits = sig_next_idx ? reg_pipe_1_cond_bits : reg_pipe_0_cond_bits; // @[issue.scala 144:16:@1841.4]
  assign io_deq_bits_itype = sig_next_idx ? reg_pipe_1_itype : reg_pipe_0_itype; // @[issue.scala 144:16:@1840.4]
  assign io_deq_bits_op = sig_next_idx ? reg_pipe_1_op : reg_pipe_0_op; // @[issue.scala 144:16:@1839.4]
  assign io_deq_bits_nzcv_en = sig_next_idx ? reg_pipe_1_nzcv_en : reg_pipe_0_nzcv_en; // @[issue.scala 144:16:@1838.4]
  assign io_deq_bits_tag = sig_next_idx ? reg_pipe_1_tag : reg_pipe_0_tag; // @[issue.scala 144:16:@1837.4]
  assign io_deq_bits_pc = sig_next_idx ? reg_pipe_1_pc : reg_pipe_0_pc; // @[issue.scala 144:16:@1834.4]
  assign Queue_clock = clock; // @[:@1612.4]
  assign Queue_reset = reset | _T_1168; // @[:@1613.4]
  assign Queue_io_enq_valid = 1'h0 == io_enq_bits_tag ? io_enq_valid : 1'h0; // @[issue.scala 93:25:@1665.4]
  assign Queue_io_enq_bits_rd_valid = io_enq_bits_rd_valid; // @[issue.scala 93:25:@1664.4]
  assign Queue_io_enq_bits_rd_bits = io_enq_bits_rd_bits; // @[issue.scala 93:25:@1663.4]
  assign Queue_io_enq_bits_rs1_bits = io_enq_bits_rs1_bits; // @[issue.scala 93:25:@1661.4]
  assign Queue_io_enq_bits_rs2_valid = io_enq_bits_rs2_valid; // @[issue.scala 93:25:@1660.4]
  assign Queue_io_enq_bits_rs2_bits = io_enq_bits_rs2_bits; // @[issue.scala 93:25:@1659.4]
  assign Queue_io_enq_bits_imm_bits = io_enq_bits_imm_bits; // @[issue.scala 93:25:@1657.4]
  assign Queue_io_enq_bits_shift_val_valid = io_enq_bits_shift_val_valid; // @[issue.scala 93:25:@1656.4]
  assign Queue_io_enq_bits_shift_val_bits = io_enq_bits_shift_val_bits; // @[issue.scala 93:25:@1655.4]
  assign Queue_io_enq_bits_shift_type = io_enq_bits_shift_type; // @[issue.scala 93:25:@1654.4]
  assign Queue_io_enq_bits_cond_bits = io_enq_bits_cond_bits; // @[issue.scala 93:25:@1652.4]
  assign Queue_io_enq_bits_itype = io_enq_bits_itype; // @[issue.scala 93:25:@1651.4]
  assign Queue_io_enq_bits_op = io_enq_bits_op; // @[issue.scala 93:25:@1650.4]
  assign Queue_io_enq_bits_nzcv_en = io_enq_bits_nzcv_en; // @[issue.scala 93:25:@1649.4]
  assign Queue_io_enq_bits_tag = io_enq_bits_tag; // @[issue.scala 93:25:@1648.4]
  assign Queue_io_enq_bits_pc = io_enq_bits_pc; // @[issue.scala 93:25:@1645.4]
  assign Queue_io_deq_ready = _T_3437 ? _GEN_176 : sig_pipe_r_0; // @[issue.scala 93:25:@1644.4]
  assign Queue_1_clock = clock; // @[:@1619.4]
  assign Queue_1_reset = reset | _T_1193; // @[:@1620.4]
  assign Queue_1_io_enq_valid = io_enq_bits_tag ? io_enq_valid : 1'h0; // @[issue.scala 93:25:@1710.4]
  assign Queue_1_io_enq_bits_rd_valid = io_enq_bits_rd_valid; // @[issue.scala 93:25:@1709.4]
  assign Queue_1_io_enq_bits_rd_bits = io_enq_bits_rd_bits; // @[issue.scala 93:25:@1708.4]
  assign Queue_1_io_enq_bits_rs1_bits = io_enq_bits_rs1_bits; // @[issue.scala 93:25:@1706.4]
  assign Queue_1_io_enq_bits_rs2_valid = io_enq_bits_rs2_valid; // @[issue.scala 93:25:@1705.4]
  assign Queue_1_io_enq_bits_rs2_bits = io_enq_bits_rs2_bits; // @[issue.scala 93:25:@1704.4]
  assign Queue_1_io_enq_bits_imm_bits = io_enq_bits_imm_bits; // @[issue.scala 93:25:@1702.4]
  assign Queue_1_io_enq_bits_shift_val_valid = io_enq_bits_shift_val_valid; // @[issue.scala 93:25:@1701.4]
  assign Queue_1_io_enq_bits_shift_val_bits = io_enq_bits_shift_val_bits; // @[issue.scala 93:25:@1700.4]
  assign Queue_1_io_enq_bits_shift_type = io_enq_bits_shift_type; // @[issue.scala 93:25:@1699.4]
  assign Queue_1_io_enq_bits_cond_bits = io_enq_bits_cond_bits; // @[issue.scala 93:25:@1697.4]
  assign Queue_1_io_enq_bits_itype = io_enq_bits_itype; // @[issue.scala 93:25:@1696.4]
  assign Queue_1_io_enq_bits_op = io_enq_bits_op; // @[issue.scala 93:25:@1695.4]
  assign Queue_1_io_enq_bits_nzcv_en = io_enq_bits_nzcv_en; // @[issue.scala 93:25:@1694.4]
  assign Queue_1_io_enq_bits_tag = io_enq_bits_tag; // @[issue.scala 93:25:@1693.4]
  assign Queue_1_io_enq_bits_pc = io_enq_bits_pc; // @[issue.scala 93:25:@1690.4]
  assign Queue_1_io_deq_ready = _T_3437 ? _GEN_177 : sig_pipe_r_1; // @[issue.scala 93:25:@1689.4]
  assign arbiter_clock = clock; // @[:@1713.4]
  assign arbiter_reset = reset; // @[:@1714.4]
  assign arbiter_io_ready = {sig_pipe_i_1,sig_pipe_i_0}; // @[issue.scala 141:20:@1832.4]
  assign arbiter_io_next_ready = 1'h1; // @[issue.scala 146:25:@1855.4]
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
  reg_pipe_0_rd_valid = _RAND_0[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  reg_pipe_0_rd_bits = _RAND_1[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  reg_pipe_0_rs1_bits = _RAND_2[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  reg_pipe_0_rs2_valid = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  reg_pipe_0_rs2_bits = _RAND_4[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  reg_pipe_0_imm_bits = _RAND_5[25:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  reg_pipe_0_shift_val_valid = _RAND_6[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  reg_pipe_0_shift_val_bits = _RAND_7[5:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  reg_pipe_0_shift_type = _RAND_8[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  reg_pipe_0_cond_bits = _RAND_9[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  reg_pipe_0_itype = _RAND_10[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {1{`RANDOM}};
  reg_pipe_0_op = _RAND_11[2:0];
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
  _RAND_14 = {2{`RANDOM}};
  reg_pipe_0_pc = _RAND_14[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_15 = {1{`RANDOM}};
  reg_pipe_1_rd_valid = _RAND_15[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_16 = {1{`RANDOM}};
  reg_pipe_1_rd_bits = _RAND_16[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_17 = {1{`RANDOM}};
  reg_pipe_1_rs1_bits = _RAND_17[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_18 = {1{`RANDOM}};
  reg_pipe_1_rs2_valid = _RAND_18[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_19 = {1{`RANDOM}};
  reg_pipe_1_rs2_bits = _RAND_19[4:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_20 = {1{`RANDOM}};
  reg_pipe_1_imm_bits = _RAND_20[25:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_21 = {1{`RANDOM}};
  reg_pipe_1_shift_val_valid = _RAND_21[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_22 = {1{`RANDOM}};
  reg_pipe_1_shift_val_bits = _RAND_22[5:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_23 = {1{`RANDOM}};
  reg_pipe_1_shift_type = _RAND_23[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_24 = {1{`RANDOM}};
  reg_pipe_1_cond_bits = _RAND_24[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_25 = {1{`RANDOM}};
  reg_pipe_1_itype = _RAND_25[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_26 = {1{`RANDOM}};
  reg_pipe_1_op = _RAND_26[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_27 = {1{`RANDOM}};
  reg_pipe_1_nzcv_en = _RAND_27[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_28 = {1{`RANDOM}};
  reg_pipe_1_tag = _RAND_28[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_29 = {2{`RANDOM}};
  reg_pipe_1_pc = _RAND_29[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_30 = {1{`RANDOM}};
  reg_pipe_v_0 = _RAND_30[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_31 = {1{`RANDOM}};
  reg_pipe_v_1 = _RAND_31[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (reset) begin
      reg_pipe_0_rd_valid <= 1'h0;
    end else begin
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_0_rd_valid <= 1'h0;
        end else begin
          if (_T_3437) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_rd_valid <= fifo_vec_1_deq_bits_rd_valid;
              end else begin
                reg_pipe_0_rd_valid <= fifo_vec_0_deq_bits_rd_valid;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_rd_valid <= fifo_vec_0_deq_bits_rd_valid;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rd_valid <= fifo_vec_0_deq_bits_rd_valid;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_rd_valid <= fifo_vec_1_deq_bits_rd_valid;
            end else begin
              reg_pipe_0_rd_valid <= fifo_vec_0_deq_bits_rd_valid;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rd_valid <= fifo_vec_0_deq_bits_rd_valid;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_rd_valid <= fifo_vec_0_deq_bits_rd_valid;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_rd_bits <= 5'h0;
    end else begin
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_0_rd_bits <= 5'h0;
        end else begin
          if (_T_3437) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_rd_bits <= fifo_vec_1_deq_bits_rd_bits;
              end else begin
                reg_pipe_0_rd_bits <= fifo_vec_0_deq_bits_rd_bits;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_rd_bits <= fifo_vec_0_deq_bits_rd_bits;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rd_bits <= fifo_vec_0_deq_bits_rd_bits;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_rd_bits <= fifo_vec_1_deq_bits_rd_bits;
            end else begin
              reg_pipe_0_rd_bits <= fifo_vec_0_deq_bits_rd_bits;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rd_bits <= fifo_vec_0_deq_bits_rd_bits;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_rd_bits <= fifo_vec_0_deq_bits_rd_bits;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_rs1_bits <= 5'h0;
    end else begin
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_0_rs1_bits <= 5'h0;
        end else begin
          if (_T_3437) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_rs1_bits <= fifo_vec_1_deq_bits_rs1_bits;
              end else begin
                reg_pipe_0_rs1_bits <= fifo_vec_0_deq_bits_rs1_bits;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_rs1_bits <= fifo_vec_0_deq_bits_rs1_bits;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rs1_bits <= fifo_vec_0_deq_bits_rs1_bits;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_rs1_bits <= fifo_vec_1_deq_bits_rs1_bits;
            end else begin
              reg_pipe_0_rs1_bits <= fifo_vec_0_deq_bits_rs1_bits;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rs1_bits <= fifo_vec_0_deq_bits_rs1_bits;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_rs1_bits <= fifo_vec_0_deq_bits_rs1_bits;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_rs2_valid <= 1'h0;
    end else begin
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_0_rs2_valid <= 1'h0;
        end else begin
          if (_T_3437) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_rs2_valid <= fifo_vec_1_deq_bits_rs2_valid;
              end else begin
                reg_pipe_0_rs2_valid <= fifo_vec_0_deq_bits_rs2_valid;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_rs2_valid <= fifo_vec_0_deq_bits_rs2_valid;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rs2_valid <= fifo_vec_0_deq_bits_rs2_valid;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_rs2_valid <= fifo_vec_1_deq_bits_rs2_valid;
            end else begin
              reg_pipe_0_rs2_valid <= fifo_vec_0_deq_bits_rs2_valid;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rs2_valid <= fifo_vec_0_deq_bits_rs2_valid;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_rs2_valid <= fifo_vec_0_deq_bits_rs2_valid;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_rs2_bits <= 5'h0;
    end else begin
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_0_rs2_bits <= 5'h0;
        end else begin
          if (_T_3437) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_rs2_bits <= fifo_vec_1_deq_bits_rs2_bits;
              end else begin
                reg_pipe_0_rs2_bits <= fifo_vec_0_deq_bits_rs2_bits;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_rs2_bits <= fifo_vec_0_deq_bits_rs2_bits;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rs2_bits <= fifo_vec_0_deq_bits_rs2_bits;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_rs2_bits <= fifo_vec_1_deq_bits_rs2_bits;
            end else begin
              reg_pipe_0_rs2_bits <= fifo_vec_0_deq_bits_rs2_bits;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_rs2_bits <= fifo_vec_0_deq_bits_rs2_bits;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_rs2_bits <= fifo_vec_0_deq_bits_rs2_bits;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_imm_bits <= 26'h0;
    end else begin
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_0_imm_bits <= 26'h0;
        end else begin
          if (_T_3437) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_imm_bits <= fifo_vec_1_deq_bits_imm_bits;
              end else begin
                reg_pipe_0_imm_bits <= fifo_vec_0_deq_bits_imm_bits;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_imm_bits <= fifo_vec_0_deq_bits_imm_bits;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_imm_bits <= fifo_vec_0_deq_bits_imm_bits;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_imm_bits <= fifo_vec_1_deq_bits_imm_bits;
            end else begin
              reg_pipe_0_imm_bits <= fifo_vec_0_deq_bits_imm_bits;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_imm_bits <= fifo_vec_0_deq_bits_imm_bits;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_imm_bits <= fifo_vec_0_deq_bits_imm_bits;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_shift_val_valid <= 1'h0;
    end else begin
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_0_shift_val_valid <= 1'h0;
        end else begin
          if (_T_3437) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_shift_val_valid <= fifo_vec_1_deq_bits_shift_val_valid;
              end else begin
                reg_pipe_0_shift_val_valid <= fifo_vec_0_deq_bits_shift_val_valid;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_shift_val_valid <= fifo_vec_0_deq_bits_shift_val_valid;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_shift_val_valid <= fifo_vec_0_deq_bits_shift_val_valid;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_shift_val_valid <= fifo_vec_1_deq_bits_shift_val_valid;
            end else begin
              reg_pipe_0_shift_val_valid <= fifo_vec_0_deq_bits_shift_val_valid;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_shift_val_valid <= fifo_vec_0_deq_bits_shift_val_valid;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_shift_val_valid <= fifo_vec_0_deq_bits_shift_val_valid;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_shift_val_bits <= 6'h0;
    end else begin
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_0_shift_val_bits <= 6'h0;
        end else begin
          if (_T_3437) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_shift_val_bits <= fifo_vec_1_deq_bits_shift_val_bits;
              end else begin
                reg_pipe_0_shift_val_bits <= fifo_vec_0_deq_bits_shift_val_bits;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_shift_val_bits <= fifo_vec_0_deq_bits_shift_val_bits;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_shift_val_bits <= fifo_vec_0_deq_bits_shift_val_bits;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_shift_val_bits <= fifo_vec_1_deq_bits_shift_val_bits;
            end else begin
              reg_pipe_0_shift_val_bits <= fifo_vec_0_deq_bits_shift_val_bits;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_shift_val_bits <= fifo_vec_0_deq_bits_shift_val_bits;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_shift_val_bits <= fifo_vec_0_deq_bits_shift_val_bits;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_shift_type <= 2'h0;
    end else begin
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_0_shift_type <= 2'h0;
        end else begin
          if (_T_3437) begin
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
        if (_T_3437) begin
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
      reg_pipe_0_cond_bits <= 4'h0;
    end else begin
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_0_cond_bits <= 4'h0;
        end else begin
          if (_T_3437) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_cond_bits <= fifo_vec_1_deq_bits_cond_bits;
              end else begin
                reg_pipe_0_cond_bits <= fifo_vec_0_deq_bits_cond_bits;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_cond_bits <= fifo_vec_0_deq_bits_cond_bits;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_cond_bits <= fifo_vec_0_deq_bits_cond_bits;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_cond_bits <= fifo_vec_1_deq_bits_cond_bits;
            end else begin
              reg_pipe_0_cond_bits <= fifo_vec_0_deq_bits_cond_bits;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_cond_bits <= fifo_vec_0_deq_bits_cond_bits;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_cond_bits <= fifo_vec_0_deq_bits_cond_bits;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_0_itype <= 3'h0;
    end else begin
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_0_itype <= 3'h0;
        end else begin
          if (_T_3437) begin
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
        if (_T_3437) begin
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
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_0_op <= 3'h0;
        end else begin
          if (_T_3437) begin
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
        if (_T_3437) begin
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
      reg_pipe_0_nzcv_en <= 1'h0;
    end else begin
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_0_nzcv_en <= 1'h0;
        end else begin
          if (_T_3437) begin
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
        if (_T_3437) begin
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
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_0_tag <= 1'h0;
        end else begin
          if (_T_3437) begin
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
        if (_T_3437) begin
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
      reg_pipe_0_pc <= 64'h0;
    end else begin
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_0_pc <= 64'h0;
        end else begin
          if (_T_3437) begin
            if (1'h0 == sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_0_pc <= fifo_vec_1_deq_bits_pc;
              end else begin
                reg_pipe_0_pc <= fifo_vec_0_deq_bits_pc;
              end
            end else begin
              if (sig_pipe_r_0) begin
                reg_pipe_0_pc <= fifo_vec_0_deq_bits_pc;
              end
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_pc <= fifo_vec_0_deq_bits_pc;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (1'h0 == sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_0_pc <= fifo_vec_1_deq_bits_pc;
            end else begin
              reg_pipe_0_pc <= fifo_vec_0_deq_bits_pc;
            end
          end else begin
            if (sig_pipe_r_0) begin
              reg_pipe_0_pc <= fifo_vec_0_deq_bits_pc;
            end
          end
        end else begin
          if (sig_pipe_r_0) begin
            reg_pipe_0_pc <= fifo_vec_0_deq_bits_pc;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_rd_valid <= 1'h0;
    end else begin
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_1_rd_valid <= 1'h0;
        end else begin
          if (_T_3437) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_rd_valid <= fifo_vec_1_deq_bits_rd_valid;
              end else begin
                reg_pipe_1_rd_valid <= fifo_vec_0_deq_bits_rd_valid;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_rd_valid <= fifo_vec_1_deq_bits_rd_valid;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rd_valid <= fifo_vec_1_deq_bits_rd_valid;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_rd_valid <= fifo_vec_1_deq_bits_rd_valid;
            end else begin
              reg_pipe_1_rd_valid <= fifo_vec_0_deq_bits_rd_valid;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rd_valid <= fifo_vec_1_deq_bits_rd_valid;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_rd_valid <= fifo_vec_1_deq_bits_rd_valid;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_rd_bits <= 5'h0;
    end else begin
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_1_rd_bits <= 5'h0;
        end else begin
          if (_T_3437) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_rd_bits <= fifo_vec_1_deq_bits_rd_bits;
              end else begin
                reg_pipe_1_rd_bits <= fifo_vec_0_deq_bits_rd_bits;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_rd_bits <= fifo_vec_1_deq_bits_rd_bits;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rd_bits <= fifo_vec_1_deq_bits_rd_bits;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_rd_bits <= fifo_vec_1_deq_bits_rd_bits;
            end else begin
              reg_pipe_1_rd_bits <= fifo_vec_0_deq_bits_rd_bits;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rd_bits <= fifo_vec_1_deq_bits_rd_bits;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_rd_bits <= fifo_vec_1_deq_bits_rd_bits;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_rs1_bits <= 5'h0;
    end else begin
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_1_rs1_bits <= 5'h0;
        end else begin
          if (_T_3437) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_rs1_bits <= fifo_vec_1_deq_bits_rs1_bits;
              end else begin
                reg_pipe_1_rs1_bits <= fifo_vec_0_deq_bits_rs1_bits;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_rs1_bits <= fifo_vec_1_deq_bits_rs1_bits;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rs1_bits <= fifo_vec_1_deq_bits_rs1_bits;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_rs1_bits <= fifo_vec_1_deq_bits_rs1_bits;
            end else begin
              reg_pipe_1_rs1_bits <= fifo_vec_0_deq_bits_rs1_bits;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rs1_bits <= fifo_vec_1_deq_bits_rs1_bits;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_rs1_bits <= fifo_vec_1_deq_bits_rs1_bits;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_rs2_valid <= 1'h0;
    end else begin
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_1_rs2_valid <= 1'h0;
        end else begin
          if (_T_3437) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_rs2_valid <= fifo_vec_1_deq_bits_rs2_valid;
              end else begin
                reg_pipe_1_rs2_valid <= fifo_vec_0_deq_bits_rs2_valid;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_rs2_valid <= fifo_vec_1_deq_bits_rs2_valid;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rs2_valid <= fifo_vec_1_deq_bits_rs2_valid;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_rs2_valid <= fifo_vec_1_deq_bits_rs2_valid;
            end else begin
              reg_pipe_1_rs2_valid <= fifo_vec_0_deq_bits_rs2_valid;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rs2_valid <= fifo_vec_1_deq_bits_rs2_valid;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_rs2_valid <= fifo_vec_1_deq_bits_rs2_valid;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_rs2_bits <= 5'h0;
    end else begin
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_1_rs2_bits <= 5'h0;
        end else begin
          if (_T_3437) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_rs2_bits <= fifo_vec_1_deq_bits_rs2_bits;
              end else begin
                reg_pipe_1_rs2_bits <= fifo_vec_0_deq_bits_rs2_bits;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_rs2_bits <= fifo_vec_1_deq_bits_rs2_bits;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rs2_bits <= fifo_vec_1_deq_bits_rs2_bits;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_rs2_bits <= fifo_vec_1_deq_bits_rs2_bits;
            end else begin
              reg_pipe_1_rs2_bits <= fifo_vec_0_deq_bits_rs2_bits;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_rs2_bits <= fifo_vec_1_deq_bits_rs2_bits;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_rs2_bits <= fifo_vec_1_deq_bits_rs2_bits;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_imm_bits <= 26'h0;
    end else begin
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_1_imm_bits <= 26'h0;
        end else begin
          if (_T_3437) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_imm_bits <= fifo_vec_1_deq_bits_imm_bits;
              end else begin
                reg_pipe_1_imm_bits <= fifo_vec_0_deq_bits_imm_bits;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_imm_bits <= fifo_vec_1_deq_bits_imm_bits;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_imm_bits <= fifo_vec_1_deq_bits_imm_bits;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_imm_bits <= fifo_vec_1_deq_bits_imm_bits;
            end else begin
              reg_pipe_1_imm_bits <= fifo_vec_0_deq_bits_imm_bits;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_imm_bits <= fifo_vec_1_deq_bits_imm_bits;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_imm_bits <= fifo_vec_1_deq_bits_imm_bits;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_shift_val_valid <= 1'h0;
    end else begin
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_1_shift_val_valid <= 1'h0;
        end else begin
          if (_T_3437) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_shift_val_valid <= fifo_vec_1_deq_bits_shift_val_valid;
              end else begin
                reg_pipe_1_shift_val_valid <= fifo_vec_0_deq_bits_shift_val_valid;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_shift_val_valid <= fifo_vec_1_deq_bits_shift_val_valid;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_shift_val_valid <= fifo_vec_1_deq_bits_shift_val_valid;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_shift_val_valid <= fifo_vec_1_deq_bits_shift_val_valid;
            end else begin
              reg_pipe_1_shift_val_valid <= fifo_vec_0_deq_bits_shift_val_valid;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_shift_val_valid <= fifo_vec_1_deq_bits_shift_val_valid;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_shift_val_valid <= fifo_vec_1_deq_bits_shift_val_valid;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_shift_val_bits <= 6'h0;
    end else begin
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_1_shift_val_bits <= 6'h0;
        end else begin
          if (_T_3437) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_shift_val_bits <= fifo_vec_1_deq_bits_shift_val_bits;
              end else begin
                reg_pipe_1_shift_val_bits <= fifo_vec_0_deq_bits_shift_val_bits;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_shift_val_bits <= fifo_vec_1_deq_bits_shift_val_bits;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_shift_val_bits <= fifo_vec_1_deq_bits_shift_val_bits;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_shift_val_bits <= fifo_vec_1_deq_bits_shift_val_bits;
            end else begin
              reg_pipe_1_shift_val_bits <= fifo_vec_0_deq_bits_shift_val_bits;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_shift_val_bits <= fifo_vec_1_deq_bits_shift_val_bits;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_shift_val_bits <= fifo_vec_1_deq_bits_shift_val_bits;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_shift_type <= 2'h0;
    end else begin
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_1_shift_type <= 2'h0;
        end else begin
          if (_T_3437) begin
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
        if (_T_3437) begin
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
      reg_pipe_1_cond_bits <= 4'h0;
    end else begin
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_1_cond_bits <= 4'h0;
        end else begin
          if (_T_3437) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_cond_bits <= fifo_vec_1_deq_bits_cond_bits;
              end else begin
                reg_pipe_1_cond_bits <= fifo_vec_0_deq_bits_cond_bits;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_cond_bits <= fifo_vec_1_deq_bits_cond_bits;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_cond_bits <= fifo_vec_1_deq_bits_cond_bits;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_cond_bits <= fifo_vec_1_deq_bits_cond_bits;
            end else begin
              reg_pipe_1_cond_bits <= fifo_vec_0_deq_bits_cond_bits;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_cond_bits <= fifo_vec_1_deq_bits_cond_bits;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_cond_bits <= fifo_vec_1_deq_bits_cond_bits;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_1_itype <= 3'h0;
    end else begin
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_1_itype <= 3'h0;
        end else begin
          if (_T_3437) begin
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
        if (_T_3437) begin
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
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_1_op <= 3'h0;
        end else begin
          if (_T_3437) begin
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
        if (_T_3437) begin
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
      reg_pipe_1_nzcv_en <= 1'h0;
    end else begin
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_1_nzcv_en <= 1'h0;
        end else begin
          if (_T_3437) begin
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
        if (_T_3437) begin
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
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_1_tag <= 1'h0;
        end else begin
          if (_T_3437) begin
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
        if (_T_3437) begin
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
      reg_pipe_1_pc <= 64'h0;
    end else begin
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_1_pc <= 64'h0;
        end else begin
          if (_T_3437) begin
            if (sig_next_idx) begin
              if (sig_next_idx) begin
                reg_pipe_1_pc <= fifo_vec_1_deq_bits_pc;
              end else begin
                reg_pipe_1_pc <= fifo_vec_0_deq_bits_pc;
              end
            end else begin
              if (sig_pipe_r_1) begin
                reg_pipe_1_pc <= fifo_vec_1_deq_bits_pc;
              end
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_pc <= fifo_vec_1_deq_bits_pc;
            end
          end
        end
      end else begin
        if (_T_3437) begin
          if (sig_next_idx) begin
            if (sig_next_idx) begin
              reg_pipe_1_pc <= fifo_vec_1_deq_bits_pc;
            end else begin
              reg_pipe_1_pc <= fifo_vec_0_deq_bits_pc;
            end
          end else begin
            if (sig_pipe_r_1) begin
              reg_pipe_1_pc <= fifo_vec_1_deq_bits_pc;
            end
          end
        end else begin
          if (sig_pipe_r_1) begin
            reg_pipe_1_pc <= fifo_vec_1_deq_bits_pc;
          end
        end
      end
    end
    if (reset) begin
      reg_pipe_v_0 <= 1'h0;
    end else begin
      if (io_flush_valid) begin
        if (1'h0 == io_flush_tag) begin
          reg_pipe_v_0 <= 1'h0;
        end else begin
          if (_T_3437) begin
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
        if (_T_3437) begin
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
      if (io_flush_valid) begin
        if (io_flush_tag) begin
          reg_pipe_v_1 <= 1'h0;
        end else begin
          if (_T_3437) begin
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
        if (_T_3437) begin
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
