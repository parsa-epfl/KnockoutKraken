module IssueUnit(
  input         clock,
  input         reset,
  output        io_enq_ready,
  input         io_enq_valid,
  input         io_enq_bits_rd_valid,
  input  [4:0]  io_enq_bits_rd_bits,
  input  [4:0]  io_enq_bits_rs1,
  input  [4:0]  io_enq_bits_rs2,
  input  [25:0] io_enq_bits_imm,
  input         io_enq_bits_shift_val_valid,
  input  [5:0]  io_enq_bits_shift_val_bits,
  input  [1:0]  io_enq_bits_shift_type,
  input         io_enq_bits_cond_valid,
  input  [3:0]  io_enq_bits_cond_bits,
  input         io_enq_bits_is32bit,
  input  [4:0]  io_enq_bits_itype,
  input  [3:0]  io_enq_bits_op,
  input         io_enq_bits_nzcv_valid,
  input  [3:0]  io_enq_bits_nzcv_bits,
  input         io_enq_bits_tag,
  input         io_enq_bits_inst32_valid,
  input  [31:0] io_enq_bits_inst32_bits,
  input  [63:0] io_enq_bits_pc,
  input         io_deq_ready,
  output        io_deq_valid,
  output        io_deq_bits_rd_valid,
  output [4:0]  io_deq_bits_rd_bits,
  output [4:0]  io_deq_bits_rs1,
  output [4:0]  io_deq_bits_rs2,
  output [25:0] io_deq_bits_imm,
  output        io_deq_bits_shift_val_valid,
  output [5:0]  io_deq_bits_shift_val_bits,
  output [1:0]  io_deq_bits_shift_type,
  output [3:0]  io_deq_bits_cond_bits,
  output        io_deq_bits_is32bit,
  output [4:0]  io_deq_bits_itype,
  output [3:0]  io_deq_bits_op,
  output        io_deq_bits_nzcv_valid,
  output [3:0]  io_deq_bits_nzcv_bits,
  output        io_deq_bits_tag,
  output        io_deq_bits_inst32_valid,
  input         io_commitReg_valid,
  input         io_commitReg_bits_tag,
  input         io_flush_tag,
  input         io_flush_valid
);
  wire  FlushQueue_clock; // @[Issue.scala 60:54]
  wire  FlushQueue_reset; // @[Issue.scala 60:54]
  wire  FlushQueue_io_enq_ready; // @[Issue.scala 60:54]
  wire  FlushQueue_io_enq_valid; // @[Issue.scala 60:54]
  wire [168:0] FlushQueue_io_enq_bits; // @[Issue.scala 60:54]
  wire  FlushQueue_io_deq_ready; // @[Issue.scala 60:54]
  wire  FlushQueue_io_deq_valid; // @[Issue.scala 60:54]
  wire [168:0] FlushQueue_io_deq_bits; // @[Issue.scala 60:54]
  wire  FlushQueue_io_flush; // @[Issue.scala 60:54]
  wire  FlushQueue_1_clock; // @[Issue.scala 60:54]
  wire  FlushQueue_1_reset; // @[Issue.scala 60:54]
  wire  FlushQueue_1_io_enq_ready; // @[Issue.scala 60:54]
  wire  FlushQueue_1_io_enq_valid; // @[Issue.scala 60:54]
  wire [168:0] FlushQueue_1_io_enq_bits; // @[Issue.scala 60:54]
  wire  FlushQueue_1_io_deq_ready; // @[Issue.scala 60:54]
  wire  FlushQueue_1_io_deq_valid; // @[Issue.scala 60:54]
  wire [168:0] FlushQueue_1_io_deq_bits; // @[Issue.scala 60:54]
  wire  FlushQueue_1_io_flush; // @[Issue.scala 60:54]
  wire  arbiter_clock; // @[Issue.scala 77:23]
  wire  arbiter_reset; // @[Issue.scala 77:23]
  wire [1:0] arbiter_io_ready; // @[Issue.scala 77:23]
  wire  arbiter_io_next_ready; // @[Issue.scala 77:23]
  wire  arbiter_io_next_valid; // @[Issue.scala 77:23]
  wire  arbiter_io_next_bits; // @[Issue.scala 77:23]
  wire [112:0] _T_8; // @[Issue.scala 60:78]
  wire [55:0] _T_17; // @[Issue.scala 60:78]
  wire  fifos_0_enq_ready; // @[Issue.scala 60:22 Issue.scala 60:22]
  wire  _GEN_56; // @[Issue.scala 70:36]
  wire  issue_thread;
  wire  _T_103; // @[Issue.scala 87:49]
  wire  _GEN_57; // @[Issue.scala 87:33]
  wire  fifos_0_deq_valid; // @[Issue.scala 60:22 Issue.scala 60:22]
  wire [168:0] fifos_0_deq_bits; // @[Issue.scala 60:22 Issue.scala 60:22]
  wire  _GEN_48; // @[Issue.scala 90:31]
  wire  fifos_1_enq_ready; // @[Issue.scala 60:22 Issue.scala 60:22]
  wire  fifos_1_deq_valid; // @[Issue.scala 60:22 Issue.scala 60:22]
  wire [168:0] fifos_1_deq_bits; // @[Issue.scala 60:22 Issue.scala 60:22]
  wire  _T_73; // @[Issue.scala 79:43]
  wire  _GEN_16; // @[Issue.scala 79:40]
  wire  _GEN_17; // @[Issue.scala 79:40]
  wire  _GEN_51; // @[Issue.scala 91:33]
  wire  ready_threads_1; // @[Issue.scala 89:24]
  wire  _GEN_50; // @[Issue.scala 91:33]
  wire  ready_threads_0; // @[Issue.scala 89:24]
  wire  _GEN_29;
  wire [168:0] _GEN_30;
  FlushQueue FlushQueue ( // @[Issue.scala 60:54]
    .clock(FlushQueue_clock),
    .reset(FlushQueue_reset),
    .io_enq_ready(FlushQueue_io_enq_ready),
    .io_enq_valid(FlushQueue_io_enq_valid),
    .io_enq_bits(FlushQueue_io_enq_bits),
    .io_deq_ready(FlushQueue_io_deq_ready),
    .io_deq_valid(FlushQueue_io_deq_valid),
    .io_deq_bits(FlushQueue_io_deq_bits),
    .io_flush(FlushQueue_io_flush)
  );
  FlushQueue FlushQueue_1 ( // @[Issue.scala 60:54]
    .clock(FlushQueue_1_clock),
    .reset(FlushQueue_1_reset),
    .io_enq_ready(FlushQueue_1_io_enq_ready),
    .io_enq_valid(FlushQueue_1_io_enq_valid),
    .io_enq_bits(FlushQueue_1_io_enq_bits),
    .io_deq_ready(FlushQueue_1_io_deq_ready),
    .io_deq_valid(FlushQueue_1_io_deq_valid),
    .io_deq_bits(FlushQueue_1_io_deq_bits),
    .io_flush(FlushQueue_1_io_flush)
  );
  RRArbiter arbiter ( // @[Issue.scala 77:23]
    .clock(arbiter_clock),
    .reset(arbiter_reset),
    .io_ready(arbiter_io_ready),
    .io_next_ready(arbiter_io_next_ready),
    .io_next_valid(arbiter_io_next_valid),
    .io_next_bits(arbiter_io_next_bits)
  );
  assign _T_8 = {io_enq_bits_is32bit,io_enq_bits_itype,io_enq_bits_op,io_enq_bits_nzcv_valid,io_enq_bits_nzcv_bits,io_enq_bits_tag,io_enq_bits_inst32_valid,io_enq_bits_inst32_bits,io_enq_bits_pc}; // @[Issue.scala 60:78]
  assign _T_17 = {io_enq_bits_rd_valid,io_enq_bits_rd_bits,io_enq_bits_rs1,io_enq_bits_rs2,io_enq_bits_imm,io_enq_bits_shift_val_valid,io_enq_bits_shift_val_bits,io_enq_bits_shift_type,io_enq_bits_cond_valid,io_enq_bits_cond_bits}; // @[Issue.scala 60:78]
  assign fifos_0_enq_ready = FlushQueue_io_enq_ready; // @[Issue.scala 60:22 Issue.scala 60:22]
  assign _GEN_56 = ~io_enq_bits_tag; // @[Issue.scala 70:36]
  assign issue_thread = arbiter_io_next_bits;
  assign _T_103 = io_deq_ready & arbiter_io_next_valid; // @[Issue.scala 87:49]
  assign _GEN_57 = ~issue_thread; // @[Issue.scala 87:33]
  assign fifos_0_deq_valid = FlushQueue_io_deq_valid; // @[Issue.scala 60:22 Issue.scala 60:22]
  assign fifos_0_deq_bits = FlushQueue_io_deq_bits; // @[Issue.scala 60:22 Issue.scala 60:22]
  assign _GEN_48 = ~io_flush_tag; // @[Issue.scala 90:31]
  assign fifos_1_enq_ready = FlushQueue_1_io_enq_ready; // @[Issue.scala 60:22 Issue.scala 60:22]
  assign fifos_1_deq_valid = FlushQueue_1_io_deq_valid; // @[Issue.scala 60:22 Issue.scala 60:22]
  assign fifos_1_deq_bits = FlushQueue_1_io_deq_bits; // @[Issue.scala 60:22 Issue.scala 60:22]
  assign _T_73 = ~io_commitReg_valid; // @[Issue.scala 79:43]
  assign _GEN_16 = ~io_commitReg_bits_tag ? _T_73 : fifos_0_deq_valid; // @[Issue.scala 79:40]
  assign _GEN_17 = io_commitReg_bits_tag ? _T_73 : fifos_1_deq_valid; // @[Issue.scala 79:40]
  assign _GEN_51 = io_flush_tag ? 1'h0 : _GEN_17; // @[Issue.scala 91:33]
  assign ready_threads_1 = io_flush_valid ? _GEN_51 : _GEN_17; // @[Issue.scala 89:24]
  assign _GEN_50 = ~io_flush_tag ? 1'h0 : _GEN_16; // @[Issue.scala 91:33]
  assign ready_threads_0 = io_flush_valid ? _GEN_50 : _GEN_16; // @[Issue.scala 89:24]
  assign _GEN_29 = issue_thread ? fifos_1_deq_valid : fifos_0_deq_valid;
  assign _GEN_30 = issue_thread ? fifos_1_deq_bits : fifos_0_deq_bits;
  assign io_enq_ready = io_enq_bits_tag ? fifos_1_enq_ready : fifos_0_enq_ready; // @[Issue.scala 69:16]
  assign io_deq_valid = _GEN_29 & arbiter_io_next_valid; // @[Issue.scala 86:16]
  assign io_deq_bits_rd_valid = _GEN_30[168]; // @[Issue.scala 85:16]
  assign io_deq_bits_rd_bits = _GEN_30[167:163]; // @[Issue.scala 85:16]
  assign io_deq_bits_rs1 = _GEN_30[162:158]; // @[Issue.scala 85:16]
  assign io_deq_bits_rs2 = _GEN_30[157:153]; // @[Issue.scala 85:16]
  assign io_deq_bits_imm = _GEN_30[152:127]; // @[Issue.scala 85:16]
  assign io_deq_bits_shift_val_valid = _GEN_30[126]; // @[Issue.scala 85:16]
  assign io_deq_bits_shift_val_bits = _GEN_30[125:120]; // @[Issue.scala 85:16]
  assign io_deq_bits_shift_type = _GEN_30[119:118]; // @[Issue.scala 85:16]
  assign io_deq_bits_cond_bits = _GEN_30[116:113]; // @[Issue.scala 85:16]
  assign io_deq_bits_is32bit = _GEN_30[112]; // @[Issue.scala 85:16]
  assign io_deq_bits_itype = _GEN_30[111:107]; // @[Issue.scala 85:16]
  assign io_deq_bits_op = _GEN_30[106:103]; // @[Issue.scala 85:16]
  assign io_deq_bits_nzcv_valid = _GEN_30[102]; // @[Issue.scala 85:16]
  assign io_deq_bits_nzcv_bits = _GEN_30[101:98]; // @[Issue.scala 85:16]
  assign io_deq_bits_tag = _GEN_30[97]; // @[Issue.scala 85:16]
  assign io_deq_bits_inst32_valid = _GEN_30[96]; // @[Issue.scala 85:16]
  assign FlushQueue_clock = clock;
  assign FlushQueue_reset = reset;
  assign FlushQueue_io_enq_valid = _GEN_56 & io_enq_valid; // @[Issue.scala 60:22]
  assign FlushQueue_io_enq_bits = {_T_17,_T_8}; // @[Issue.scala 60:22]
  assign FlushQueue_io_deq_ready = _GEN_57 & _T_103; // @[Issue.scala 60:22]
  assign FlushQueue_io_flush = io_flush_valid & _GEN_48; // @[Issue.scala 60:22]
  assign FlushQueue_1_clock = clock;
  assign FlushQueue_1_reset = reset;
  assign FlushQueue_1_io_enq_valid = io_enq_bits_tag & io_enq_valid; // @[Issue.scala 60:22]
  assign FlushQueue_1_io_enq_bits = {_T_17,_T_8}; // @[Issue.scala 60:22]
  assign FlushQueue_1_io_deq_ready = issue_thread & _T_103; // @[Issue.scala 60:22]
  assign FlushQueue_1_io_flush = io_flush_valid & io_flush_tag; // @[Issue.scala 60:22]
  assign arbiter_clock = clock;
  assign arbiter_reset = reset;
  assign arbiter_io_ready = {ready_threads_1,ready_threads_0}; // @[Issue.scala 80:20]
  assign arbiter_io_next_ready = io_deq_ready; // @[Issue.scala 82:25]
endmodule
