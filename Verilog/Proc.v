module Proc( // @[:@3002.2]
  input         clock, // @[:@3003.4]
  input         reset, // @[:@3004.4]
  input         io_ppageBRAM_writeEn, // @[:@3005.4]
  input         io_ppageBRAM_en, // @[:@3005.4]
  input  [9:0]  io_ppageBRAM_addr, // @[:@3005.4]
  input  [35:0] io_ppageBRAM_dataIn, // @[:@3005.4]
  output [35:0] io_ppageBRAM_dataOut, // @[:@3005.4]
  input         io_stateBRAM_writeEn, // @[:@3005.4]
  input         io_stateBRAM_en, // @[:@3005.4]
  input  [9:0]  io_stateBRAM_addr, // @[:@3005.4]
  input  [35:0] io_stateBRAM_dataIn, // @[:@3005.4]
  output [35:0] io_stateBRAM_dataOut, // @[:@3005.4]
  input         io_host2tpu_fire, // @[:@3005.4]
  input         io_host2tpu_fireTag, // @[:@3005.4]
  output        io_host2tpu_done, // @[:@3005.4]
  output        io_host2tpu_doneTag // @[:@3005.4]
);
  wire  ppage_clock; // @[proc.scala 66:21:@3007.4]
  wire  ppage_io_portA_writeEn; // @[proc.scala 66:21:@3007.4]
  wire  ppage_io_portA_en; // @[proc.scala 66:21:@3007.4]
  wire [9:0] ppage_io_portA_addr; // @[proc.scala 66:21:@3007.4]
  wire [35:0] ppage_io_portA_dataIn; // @[proc.scala 66:21:@3007.4]
  wire [35:0] ppage_io_portA_dataOut; // @[proc.scala 66:21:@3007.4]
  wire  ppage_io_portB_writeEn; // @[proc.scala 66:21:@3007.4]
  wire [9:0] ppage_io_portB_addr; // @[proc.scala 66:21:@3007.4]
  wire [35:0] ppage_io_portB_dataIn; // @[proc.scala 66:21:@3007.4]
  wire [35:0] ppage_io_portB_dataOut; // @[proc.scala 66:21:@3007.4]
  wire  state_clock; // @[proc.scala 68:21:@3010.4]
  wire  state_io_portA_writeEn; // @[proc.scala 68:21:@3010.4]
  wire  state_io_portA_en; // @[proc.scala 68:21:@3010.4]
  wire [9:0] state_io_portA_addr; // @[proc.scala 68:21:@3010.4]
  wire [35:0] state_io_portA_dataIn; // @[proc.scala 68:21:@3010.4]
  wire [35:0] state_io_portA_dataOut; // @[proc.scala 68:21:@3010.4]
  wire  state_io_portB_writeEn; // @[proc.scala 68:21:@3010.4]
  wire [9:0] state_io_portB_addr; // @[proc.scala 68:21:@3010.4]
  wire [35:0] state_io_portB_dataIn; // @[proc.scala 68:21:@3010.4]
  wire [35:0] state_io_portB_dataOut; // @[proc.scala 68:21:@3010.4]
  wire  tpu_clock; // @[proc.scala 70:19:@3013.4]
  wire  tpu_reset; // @[proc.scala 70:19:@3013.4]
  wire  tpu_io_host2tpu_fire; // @[proc.scala 70:19:@3013.4]
  wire  tpu_io_host2tpu_fireTag; // @[proc.scala 70:19:@3013.4]
  wire  tpu_io_host2tpu_done; // @[proc.scala 70:19:@3013.4]
  wire  tpu_io_host2tpu_doneTag; // @[proc.scala 70:19:@3013.4]
  wire  tpu_io_tpu2cpu_flush; // @[proc.scala 70:19:@3013.4]
  wire  tpu_io_tpu2cpu_freeze; // @[proc.scala 70:19:@3013.4]
  wire  tpu_io_tpu2cpu_fire; // @[proc.scala 70:19:@3013.4]
  wire  tpu_io_tpu2cpu_done; // @[proc.scala 70:19:@3013.4]
  wire  tpu_io_tpu2cpu_flushTag; // @[proc.scala 70:19:@3013.4]
  wire  tpu_io_tpu2cpu_fireTag; // @[proc.scala 70:19:@3013.4]
  wire  tpu_io_tpu2cpu_freezeTag; // @[proc.scala 70:19:@3013.4]
  wire  tpu_io_tpu2cpu_doneTag; // @[proc.scala 70:19:@3013.4]
  wire  tpu_io_tpu2cpuStateReg_valid; // @[proc.scala 70:19:@3013.4]
  wire [1:0] tpu_io_tpu2cpuStateReg_bits; // @[proc.scala 70:19:@3013.4]
  wire [63:0] tpu_io_tpu2cpuState_PC; // @[proc.scala 70:19:@3013.4]
  wire [31:0] tpu_io_tpu2cpuState_SP; // @[proc.scala 70:19:@3013.4]
  wire [31:0] tpu_io_tpu2cpuState_EL; // @[proc.scala 70:19:@3013.4]
  wire [3:0] tpu_io_tpu2cpuState_NZCV; // @[proc.scala 70:19:@3013.4]
  wire [63:0] tpu_io_cpu2tpuState_PC; // @[proc.scala 70:19:@3013.4]
  wire [31:0] tpu_io_cpu2tpuState_SP; // @[proc.scala 70:19:@3013.4]
  wire [31:0] tpu_io_cpu2tpuState_EL; // @[proc.scala 70:19:@3013.4]
  wire [3:0] tpu_io_cpu2tpuState_NZCV; // @[proc.scala 70:19:@3013.4]
  wire [4:0] tpu_io_rfile_rs1_addr; // @[proc.scala 70:19:@3013.4]
  wire [63:0] tpu_io_rfile_rs1_data; // @[proc.scala 70:19:@3013.4]
  wire [4:0] tpu_io_rfile_waddr; // @[proc.scala 70:19:@3013.4]
  wire [63:0] tpu_io_rfile_wdata; // @[proc.scala 70:19:@3013.4]
  wire  tpu_io_rfile_wen; // @[proc.scala 70:19:@3013.4]
  wire  tpu_io_stateBRAM_writeEn; // @[proc.scala 70:19:@3013.4]
  wire [9:0] tpu_io_stateBRAM_addr; // @[proc.scala 70:19:@3013.4]
  wire [35:0] tpu_io_stateBRAM_dataIn; // @[proc.scala 70:19:@3013.4]
  wire [35:0] tpu_io_stateBRAM_dataOut; // @[proc.scala 70:19:@3013.4]
  wire  RFile_clock; // @[proc.scala 75:58:@3021.4]
  wire [4:0] RFile_io_rs1_addr; // @[proc.scala 75:58:@3021.4]
  wire [63:0] RFile_io_rs1_data; // @[proc.scala 75:58:@3021.4]
  wire [4:0] RFile_io_rs2_addr; // @[proc.scala 75:58:@3021.4]
  wire [63:0] RFile_io_rs2_data; // @[proc.scala 75:58:@3021.4]
  wire [4:0] RFile_io_waddr; // @[proc.scala 75:58:@3021.4]
  wire [63:0] RFile_io_wdata; // @[proc.scala 75:58:@3021.4]
  wire  RFile_io_wen; // @[proc.scala 75:58:@3021.4]
  wire  RFile_1_clock; // @[proc.scala 75:58:@3024.4]
  wire [4:0] RFile_1_io_rs1_addr; // @[proc.scala 75:58:@3024.4]
  wire [63:0] RFile_1_io_rs1_data; // @[proc.scala 75:58:@3024.4]
  wire [4:0] RFile_1_io_rs2_addr; // @[proc.scala 75:58:@3024.4]
  wire [63:0] RFile_1_io_rs2_data; // @[proc.scala 75:58:@3024.4]
  wire [4:0] RFile_1_io_waddr; // @[proc.scala 75:58:@3024.4]
  wire [63:0] RFile_1_io_wdata; // @[proc.scala 75:58:@3024.4]
  wire  RFile_1_io_wen; // @[proc.scala 75:58:@3024.4]
  wire  fetch_clock; // @[proc.scala 81:21:@3053.4]
  wire  fetch_reset; // @[proc.scala 81:21:@3053.4]
  wire  fetch_io_en; // @[proc.scala 81:21:@3053.4]
  wire [63:0] fetch_io_PC; // @[proc.scala 81:21:@3053.4]
  wire  fetch_io_tagIn; // @[proc.scala 81:21:@3053.4]
  wire  fetch_io_incr; // @[proc.scala 81:21:@3053.4]
  wire  fetch_io_flush; // @[proc.scala 81:21:@3053.4]
  wire [9:0] fetch_io_ppageBRAM_addr; // @[proc.scala 81:21:@3053.4]
  wire [35:0] fetch_io_ppageBRAM_dataOut; // @[proc.scala 81:21:@3053.4]
  wire  fetch_io_deq_ready; // @[proc.scala 81:21:@3053.4]
  wire  fetch_io_deq_valid; // @[proc.scala 81:21:@3053.4]
  wire [31:0] fetch_io_deq_bits_inst; // @[proc.scala 81:21:@3053.4]
  wire  fetch_io_deq_bits_tag; // @[proc.scala 81:21:@3053.4]
  wire [31:0] decoder_io_finst_inst; // @[proc.scala 84:23:@3056.4]
  wire  decoder_io_finst_tag; // @[proc.scala 84:23:@3056.4]
  wire [4:0] decoder_io_dinst_rd; // @[proc.scala 84:23:@3056.4]
  wire [4:0] decoder_io_dinst_rs1; // @[proc.scala 84:23:@3056.4]
  wire [4:0] decoder_io_dinst_rs2; // @[proc.scala 84:23:@3056.4]
  wire [25:0] decoder_io_dinst_imm; // @[proc.scala 84:23:@3056.4]
  wire [5:0] decoder_io_dinst_shift_val; // @[proc.scala 84:23:@3056.4]
  wire [1:0] decoder_io_dinst_shift_type; // @[proc.scala 84:23:@3056.4]
  wire [3:0] decoder_io_dinst_cond; // @[proc.scala 84:23:@3056.4]
  wire [2:0] decoder_io_dinst_itype; // @[proc.scala 84:23:@3056.4]
  wire [2:0] decoder_io_dinst_op; // @[proc.scala 84:23:@3056.4]
  wire  decoder_io_dinst_rd_en; // @[proc.scala 84:23:@3056.4]
  wire  decoder_io_dinst_rs2_en; // @[proc.scala 84:23:@3056.4]
  wire  decoder_io_dinst_shift_en; // @[proc.scala 84:23:@3056.4]
  wire  decoder_io_dinst_nzcv_en; // @[proc.scala 84:23:@3056.4]
  wire  decoder_io_dinst_tag; // @[proc.scala 84:23:@3056.4]
  wire  decReg_clock; // @[proc.scala 85:22:@3059.4]
  wire  decReg_reset; // @[proc.scala 85:22:@3059.4]
  wire  decReg_io_enq_ready; // @[proc.scala 85:22:@3059.4]
  wire  decReg_io_enq_valid; // @[proc.scala 85:22:@3059.4]
  wire [4:0] decReg_io_enq_bits_rd; // @[proc.scala 85:22:@3059.4]
  wire [4:0] decReg_io_enq_bits_rs1; // @[proc.scala 85:22:@3059.4]
  wire [4:0] decReg_io_enq_bits_rs2; // @[proc.scala 85:22:@3059.4]
  wire [25:0] decReg_io_enq_bits_imm; // @[proc.scala 85:22:@3059.4]
  wire [5:0] decReg_io_enq_bits_shift_val; // @[proc.scala 85:22:@3059.4]
  wire [1:0] decReg_io_enq_bits_shift_type; // @[proc.scala 85:22:@3059.4]
  wire [3:0] decReg_io_enq_bits_cond; // @[proc.scala 85:22:@3059.4]
  wire [2:0] decReg_io_enq_bits_itype; // @[proc.scala 85:22:@3059.4]
  wire [2:0] decReg_io_enq_bits_op; // @[proc.scala 85:22:@3059.4]
  wire  decReg_io_enq_bits_rd_en; // @[proc.scala 85:22:@3059.4]
  wire  decReg_io_enq_bits_rs2_en; // @[proc.scala 85:22:@3059.4]
  wire  decReg_io_enq_bits_shift_en; // @[proc.scala 85:22:@3059.4]
  wire  decReg_io_enq_bits_nzcv_en; // @[proc.scala 85:22:@3059.4]
  wire  decReg_io_enq_bits_tag; // @[proc.scala 85:22:@3059.4]
  wire  decReg_io_deq_ready; // @[proc.scala 85:22:@3059.4]
  wire  decReg_io_deq_valid; // @[proc.scala 85:22:@3059.4]
  wire [4:0] decReg_io_deq_bits_rd; // @[proc.scala 85:22:@3059.4]
  wire [4:0] decReg_io_deq_bits_rs1; // @[proc.scala 85:22:@3059.4]
  wire [4:0] decReg_io_deq_bits_rs2; // @[proc.scala 85:22:@3059.4]
  wire [25:0] decReg_io_deq_bits_imm; // @[proc.scala 85:22:@3059.4]
  wire [5:0] decReg_io_deq_bits_shift_val; // @[proc.scala 85:22:@3059.4]
  wire [1:0] decReg_io_deq_bits_shift_type; // @[proc.scala 85:22:@3059.4]
  wire [3:0] decReg_io_deq_bits_cond; // @[proc.scala 85:22:@3059.4]
  wire [2:0] decReg_io_deq_bits_itype; // @[proc.scala 85:22:@3059.4]
  wire [2:0] decReg_io_deq_bits_op; // @[proc.scala 85:22:@3059.4]
  wire  decReg_io_deq_bits_rd_en; // @[proc.scala 85:22:@3059.4]
  wire  decReg_io_deq_bits_rs2_en; // @[proc.scala 85:22:@3059.4]
  wire  decReg_io_deq_bits_shift_en; // @[proc.scala 85:22:@3059.4]
  wire  decReg_io_deq_bits_nzcv_en; // @[proc.scala 85:22:@3059.4]
  wire  decReg_io_deq_bits_tag; // @[proc.scala 85:22:@3059.4]
  wire  decReg_io_flush; // @[proc.scala 85:22:@3059.4]
  wire  issuer_clock; // @[proc.scala 87:22:@3062.4]
  wire  issuer_reset; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_flush; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_flushTag; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_enq_ready; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_enq_valid; // @[proc.scala 87:22:@3062.4]
  wire [4:0] issuer_io_enq_bits_rd; // @[proc.scala 87:22:@3062.4]
  wire [4:0] issuer_io_enq_bits_rs1; // @[proc.scala 87:22:@3062.4]
  wire [4:0] issuer_io_enq_bits_rs2; // @[proc.scala 87:22:@3062.4]
  wire [25:0] issuer_io_enq_bits_imm; // @[proc.scala 87:22:@3062.4]
  wire [5:0] issuer_io_enq_bits_shift_val; // @[proc.scala 87:22:@3062.4]
  wire [1:0] issuer_io_enq_bits_shift_type; // @[proc.scala 87:22:@3062.4]
  wire [3:0] issuer_io_enq_bits_cond; // @[proc.scala 87:22:@3062.4]
  wire [2:0] issuer_io_enq_bits_itype; // @[proc.scala 87:22:@3062.4]
  wire [2:0] issuer_io_enq_bits_op; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_enq_bits_rd_en; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_enq_bits_rs2_en; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_enq_bits_shift_en; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_enq_bits_nzcv_en; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_enq_bits_tag; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_deq_valid; // @[proc.scala 87:22:@3062.4]
  wire [4:0] issuer_io_deq_bits_rd; // @[proc.scala 87:22:@3062.4]
  wire [4:0] issuer_io_deq_bits_rs1; // @[proc.scala 87:22:@3062.4]
  wire [4:0] issuer_io_deq_bits_rs2; // @[proc.scala 87:22:@3062.4]
  wire [25:0] issuer_io_deq_bits_imm; // @[proc.scala 87:22:@3062.4]
  wire [5:0] issuer_io_deq_bits_shift_val; // @[proc.scala 87:22:@3062.4]
  wire [1:0] issuer_io_deq_bits_shift_type; // @[proc.scala 87:22:@3062.4]
  wire [3:0] issuer_io_deq_bits_cond; // @[proc.scala 87:22:@3062.4]
  wire [2:0] issuer_io_deq_bits_itype; // @[proc.scala 87:22:@3062.4]
  wire [2:0] issuer_io_deq_bits_op; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_deq_bits_rd_en; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_deq_bits_rs2_en; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_deq_bits_shift_en; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_deq_bits_nzcv_en; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_deq_bits_tag; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_exeReg_valid; // @[proc.scala 87:22:@3062.4]
  wire [4:0] issuer_io_exeReg_bits_rd; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_exeReg_bits_rd_en; // @[proc.scala 87:22:@3062.4]
  wire  issuer_io_exeReg_bits_tag; // @[proc.scala 87:22:@3062.4]
  wire [4:0] executer_io_dinst_rd; // @[proc.scala 90:24:@3065.4]
  wire [25:0] executer_io_dinst_imm; // @[proc.scala 90:24:@3065.4]
  wire [5:0] executer_io_dinst_shift_val; // @[proc.scala 90:24:@3065.4]
  wire [1:0] executer_io_dinst_shift_type; // @[proc.scala 90:24:@3065.4]
  wire [2:0] executer_io_dinst_itype; // @[proc.scala 90:24:@3065.4]
  wire [2:0] executer_io_dinst_op; // @[proc.scala 90:24:@3065.4]
  wire  executer_io_dinst_rd_en; // @[proc.scala 90:24:@3065.4]
  wire  executer_io_dinst_rs2_en; // @[proc.scala 90:24:@3065.4]
  wire  executer_io_dinst_shift_en; // @[proc.scala 90:24:@3065.4]
  wire  executer_io_dinst_nzcv_en; // @[proc.scala 90:24:@3065.4]
  wire  executer_io_dinst_tag; // @[proc.scala 90:24:@3065.4]
  wire [63:0] executer_io_rVal1; // @[proc.scala 90:24:@3065.4]
  wire [63:0] executer_io_rVal2; // @[proc.scala 90:24:@3065.4]
  wire  executer_io_einst_valid; // @[proc.scala 90:24:@3065.4]
  wire [63:0] executer_io_einst_bits_res; // @[proc.scala 90:24:@3065.4]
  wire [4:0] executer_io_einst_bits_rd; // @[proc.scala 90:24:@3065.4]
  wire  executer_io_einst_bits_rd_en; // @[proc.scala 90:24:@3065.4]
  wire  executer_io_einst_bits_tag; // @[proc.scala 90:24:@3065.4]
  wire [3:0] executer_io_einst_bits_nzcv; // @[proc.scala 90:24:@3065.4]
  wire  executer_io_einst_bits_nzcv_en; // @[proc.scala 90:24:@3065.4]
  wire  exeReg_clock; // @[proc.scala 91:22:@3068.4]
  wire  exeReg_reset; // @[proc.scala 91:22:@3068.4]
  wire  exeReg_io_enq_valid; // @[proc.scala 91:22:@3068.4]
  wire [63:0] exeReg_io_enq_bits_res; // @[proc.scala 91:22:@3068.4]
  wire [4:0] exeReg_io_enq_bits_rd; // @[proc.scala 91:22:@3068.4]
  wire  exeReg_io_enq_bits_rd_en; // @[proc.scala 91:22:@3068.4]
  wire  exeReg_io_enq_bits_tag; // @[proc.scala 91:22:@3068.4]
  wire [3:0] exeReg_io_enq_bits_nzcv; // @[proc.scala 91:22:@3068.4]
  wire  exeReg_io_enq_bits_nzcv_en; // @[proc.scala 91:22:@3068.4]
  wire  exeReg_io_deq_valid; // @[proc.scala 91:22:@3068.4]
  wire [63:0] exeReg_io_deq_bits_res; // @[proc.scala 91:22:@3068.4]
  wire [4:0] exeReg_io_deq_bits_rd; // @[proc.scala 91:22:@3068.4]
  wire  exeReg_io_deq_bits_rd_en; // @[proc.scala 91:22:@3068.4]
  wire  exeReg_io_deq_bits_tag; // @[proc.scala 91:22:@3068.4]
  wire [3:0] exeReg_io_deq_bits_nzcv; // @[proc.scala 91:22:@3068.4]
  wire  exeReg_io_deq_bits_nzcv_en; // @[proc.scala 91:22:@3068.4]
  wire  exeReg_io_flush; // @[proc.scala 91:22:@3068.4]
  wire [25:0] brancher_io_dinst_imm; // @[proc.scala 93:24:@3071.4]
  wire [3:0] brancher_io_dinst_cond; // @[proc.scala 93:24:@3071.4]
  wire [2:0] brancher_io_dinst_itype; // @[proc.scala 93:24:@3071.4]
  wire [2:0] brancher_io_dinst_op; // @[proc.scala 93:24:@3071.4]
  wire  brancher_io_dinst_tag; // @[proc.scala 93:24:@3071.4]
  wire [3:0] brancher_io_nzcv; // @[proc.scala 93:24:@3071.4]
  wire  brancher_io_binst_valid; // @[proc.scala 93:24:@3071.4]
  wire [63:0] brancher_io_binst_bits_offset; // @[proc.scala 93:24:@3071.4]
  wire  brancher_io_binst_bits_tag; // @[proc.scala 93:24:@3071.4]
  wire  brReg_clock; // @[proc.scala 94:21:@3074.4]
  wire  brReg_reset; // @[proc.scala 94:21:@3074.4]
  wire  brReg_io_enq_valid; // @[proc.scala 94:21:@3074.4]
  wire [63:0] brReg_io_enq_bits_offset; // @[proc.scala 94:21:@3074.4]
  wire  brReg_io_enq_bits_tag; // @[proc.scala 94:21:@3074.4]
  wire  brReg_io_deq_valid; // @[proc.scala 94:21:@3074.4]
  wire [63:0] brReg_io_deq_bits_offset; // @[proc.scala 94:21:@3074.4]
  wire  brReg_io_deq_bits_tag; // @[proc.scala 94:21:@3074.4]
  wire  brReg_io_flush; // @[proc.scala 94:21:@3074.4]
  wire  ldstU_clock; // @[proc.scala 96:21:@3077.4]
  wire  ldstU_reset; // @[proc.scala 96:21:@3077.4]
  wire  ldstU_io_dinst_valid; // @[proc.scala 96:21:@3077.4]
  wire [25:0] ldstU_io_dinst_bits_imm; // @[proc.scala 96:21:@3077.4]
  wire [2:0] ldstU_io_dinst_bits_itype; // @[proc.scala 96:21:@3077.4]
  wire [63:0] ldstU_io_pc; // @[proc.scala 96:21:@3077.4]
  reg [63:0] vec_pregs_0_PC; // @[proc.scala 76:26:@3051.4]
  reg [63:0] _RAND_0;
  reg [31:0] vec_pregs_0_SP; // @[proc.scala 76:26:@3051.4]
  reg [31:0] _RAND_1;
  reg [31:0] vec_pregs_0_EL; // @[proc.scala 76:26:@3051.4]
  reg [31:0] _RAND_2;
  reg [3:0] vec_pregs_0_NZCV; // @[proc.scala 76:26:@3051.4]
  reg [31:0] _RAND_3;
  reg [63:0] vec_pregs_1_PC; // @[proc.scala 76:26:@3051.4]
  reg [63:0] _RAND_4;
  reg [31:0] vec_pregs_1_SP; // @[proc.scala 76:26:@3051.4]
  reg [31:0] _RAND_5;
  reg [31:0] vec_pregs_1_EL; // @[proc.scala 76:26:@3051.4]
  reg [31:0] _RAND_6;
  reg [3:0] vec_pregs_1_NZCV; // @[proc.scala 76:26:@3051.4]
  reg [31:0] _RAND_7;
  reg [64:0] fake_PC; // @[proc.scala 100:24:@3083.4]
  reg [95:0] _RAND_8;
  reg  fetch_en_0; // @[proc.scala 101:25:@3087.4]
  reg [31:0] _RAND_9;
  reg  fetch_en_1; // @[proc.scala 101:25:@3087.4]
  reg [31:0] _RAND_10;
  wire [65:0] _T_186; // @[proc.scala 129:24:@3128.6]
  wire [64:0] _T_187; // @[proc.scala 129:24:@3129.6]
  wire [64:0] _GEN_4; // @[proc.scala 128:23:@3127.4]
  wire [4:0] _vec_rfile_tpu_io_tpu2cpu_freezeTag_rs1_addr; // @[proc.scala 281:41:@3407.6 proc.scala 281:41:@3407.6]
  wire [4:0] _GEN_136; // @[proc.scala 281:41:@3407.6]
  wire [63:0] vec_rfile_0_rs1_data; // @[proc.scala 75:26:@3027.4 proc.scala 75:26:@3033.4]
  wire [4:0] _GEN_134; // @[proc.scala 281:41:@3405.6]
  wire [63:0] vec_rfile_0_rs2_data; // @[proc.scala 75:26:@3027.4 proc.scala 75:26:@3031.4]
  wire [4:0] _vec_rfile_tpu_io_tpu2cpu_freezeTag_waddr; // @[proc.scala 281:41:@3403.6 proc.scala 281:41:@3403.6]
  wire [4:0] _GEN_27; // @[proc.scala 208:39:@3293.6]
  wire [4:0] _GEN_29; // @[proc.scala 205:31:@3288.4]
  wire [4:0] _GEN_118; // @[proc.scala 281:41:@3403.6]
  wire [63:0] _vec_rfile_tpu_io_tpu2cpu_freezeTag_wdata; // @[proc.scala 281:41:@3402.6 proc.scala 281:41:@3402.6]
  wire [63:0] _GEN_28; // @[proc.scala 208:39:@3293.6]
  wire [63:0] _GEN_30; // @[proc.scala 205:31:@3288.4]
  wire [63:0] _GEN_116; // @[proc.scala 281:41:@3402.6]
  wire  _vec_rfile_tpu_io_tpu2cpu_freezeTag_wen; // @[proc.scala 281:41:@3401.6 proc.scala 281:41:@3401.6]
  wire  _vec_rfile_exeReg_io_deq_bits_tag_wen; // @[proc.scala 218:43:@3317.6 proc.scala 218:43:@3317.6]
  wire  _GEN_31; // @[proc.scala 218:43:@3317.6]
  wire  _GEN_37; // @[proc.scala 217:29:@3316.4]
  wire  _GEN_114; // @[proc.scala 281:41:@3401.6]
  wire [4:0] _GEN_137; // @[proc.scala 281:41:@3407.6]
  wire [63:0] vec_rfile_1_rs1_data; // @[proc.scala 75:26:@3027.4 proc.scala 75:26:@3040.4]
  wire [4:0] _GEN_135; // @[proc.scala 281:41:@3405.6]
  wire [63:0] vec_rfile_1_rs2_data; // @[proc.scala 75:26:@3027.4 proc.scala 75:26:@3038.4]
  wire [4:0] _GEN_119; // @[proc.scala 281:41:@3403.6]
  wire [63:0] _GEN_117; // @[proc.scala 281:41:@3402.6]
  wire  _GEN_32; // @[proc.scala 218:43:@3317.6]
  wire  _GEN_38; // @[proc.scala 217:29:@3316.4]
  wire  _GEN_115; // @[proc.scala 281:41:@3401.6]
  wire  _T_266; // @[proc.scala 224:29:@3323.4]
  wire [3:0] _vec_pregs_exeReg_io_deq_bits_tag_NZCV; // @[proc.scala 225:44:@3325.6 proc.scala 225:44:@3325.6]
  wire [3:0] _GEN_39; // @[proc.scala 225:44:@3325.6]
  wire [3:0] _GEN_40; // @[proc.scala 225:44:@3325.6]
  wire [3:0] _GEN_41; // @[proc.scala 224:60:@3324.4]
  wire [3:0] _GEN_42; // @[proc.scala 224:60:@3324.4]
  wire [63:0] _GEN_47; // @[proc.scala 232:53:@3329.6]
  wire [64:0] _T_274; // @[proc.scala 232:53:@3329.6]
  wire [63:0] _T_275; // @[proc.scala 232:85:@3330.6]
  wire [64:0] _GEN_196; // @[proc.scala 232:58:@3331.6]
  wire [65:0] _T_276; // @[proc.scala 232:58:@3331.6]
  wire [64:0] _T_277; // @[proc.scala 232:58:@3332.6]
  wire [64:0] _T_278; // @[proc.scala 232:58:@3333.6]
  wire [64:0] _T_279; // @[proc.scala 232:99:@3334.6]
  wire [63:0] _vec_pregs_brReg_io_deq_bits_tag_PC; // @[proc.scala 233:41:@3342.6 proc.scala 233:41:@3342.6]
  wire [63:0] _GEN_51; // @[proc.scala 233:41:@3342.6]
  wire [63:0] _GEN_52; // @[proc.scala 233:41:@3342.6]
  wire [63:0] _GEN_57; // @[proc.scala 236:82:@3347.8]
  wire [64:0] _T_299; // @[proc.scala 236:82:@3347.8]
  wire [63:0] _T_300; // @[proc.scala 236:82:@3348.8]
  wire [63:0] _GEN_61; // @[proc.scala 236:42:@3349.8]
  wire [63:0] _GEN_62; // @[proc.scala 236:42:@3349.8]
  wire [63:0] _GEN_76; // @[proc.scala 235:35:@3346.6]
  wire [63:0] _GEN_77; // @[proc.scala 235:35:@3346.6]
  wire [64:0] _GEN_79; // @[proc.scala 231:28:@3328.4]
  wire [63:0] _GEN_80; // @[proc.scala 231:28:@3328.4]
  wire [63:0] _GEN_81; // @[proc.scala 231:28:@3328.4]
  wire [63:0] _GEN_87; // @[proc.scala 245:13:@3360.6]
  wire  _GEN_91; // @[proc.scala 246:38:@3361.6]
  wire  _GEN_92; // @[proc.scala 246:38:@3361.6]
  wire [64:0] _GEN_93; // @[proc.scala 244:29:@3359.4]
  wire  _GEN_94; // @[proc.scala 244:29:@3359.4]
  wire  _GEN_95; // @[proc.scala 244:29:@3359.4]
  wire  _T_317; // @[proc.scala 248:68:@3363.4]
  wire  _T_318; // @[proc.scala 248:46:@3364.4]
  wire  _GEN_96; // @[proc.scala 251:39:@3370.6]
  wire  _GEN_97; // @[proc.scala 251:39:@3370.6]
  wire  _GEN_98; // @[proc.scala 250:82:@3369.4]
  wire  _GEN_99; // @[proc.scala 250:82:@3369.4]
  wire  _T_325; // @[proc.scala 258:45:@3374.6]
  wire  _T_326; // @[proc.scala 260:47:@3377.6]
  wire  _T_327; // @[proc.scala 261:45:@3379.6]
  wire  _T_328; // @[proc.scala 262:47:@3381.6]
  wire [63:0] _GEN_110; // @[proc.scala 275:23:@3394.4]
  wire [31:0] _GEN_111; // @[proc.scala 275:23:@3394.4]
  wire [31:0] _GEN_112; // @[proc.scala 275:23:@3394.4]
  wire [3:0] _GEN_113; // @[proc.scala 275:23:@3394.4]
  wire [63:0] _GEN_128; // @[proc.scala 281:41:@3404.6]
  wire [3:0] _GEN_138; // @[proc.scala 283:41:@3408.6]
  wire [3:0] _GEN_139; // @[proc.scala 283:41:@3408.6]
  wire [31:0] _GEN_140; // @[proc.scala 283:41:@3409.6]
  wire [31:0] _GEN_141; // @[proc.scala 283:41:@3409.6]
  wire [31:0] _GEN_142; // @[proc.scala 283:41:@3410.6]
  wire [31:0] _GEN_143; // @[proc.scala 283:41:@3410.6]
  wire [63:0] _GEN_144; // @[proc.scala 283:41:@3411.6]
  wire [63:0] _GEN_145; // @[proc.scala 283:41:@3411.6]
  wire  _T_360; // @[proc.scala 285:40:@3413.8]
  wire [63:0] _vec_pregs_tpu_io_tpu2cpu_freezeTag_PC_0; // @[proc.scala 286:48:@3415.10 proc.scala 286:48:@3415.10]
  wire [63:0] _GEN_146; // @[proc.scala 286:48:@3415.10]
  wire [63:0] _GEN_147; // @[proc.scala 286:48:@3415.10]
  wire  _T_364; // @[proc.scala 287:46:@3418.10]
  wire [31:0] _vec_pregs_tpu_io_tpu2cpu_freezeTag_SP_0; // @[proc.scala 288:48:@3420.12 proc.scala 288:48:@3420.12]
  wire [31:0] _GEN_148; // @[proc.scala 288:48:@3420.12]
  wire [31:0] _GEN_149; // @[proc.scala 288:48:@3420.12]
  wire [31:0] _vec_pregs_tpu_io_tpu2cpu_freezeTag_EL_0; // @[proc.scala 289:48:@3421.12 proc.scala 289:48:@3421.12]
  wire [31:0] _GEN_150; // @[proc.scala 289:48:@3421.12]
  wire [31:0] _GEN_151; // @[proc.scala 289:48:@3421.12]
  wire [3:0] _vec_pregs_tpu_io_tpu2cpu_freezeTag_NZCV_0; // @[proc.scala 290:50:@3422.12 proc.scala 290:50:@3422.12]
  wire [3:0] _GEN_152; // @[proc.scala 290:50:@3422.12]
  wire [3:0] _GEN_153; // @[proc.scala 290:50:@3422.12]
  wire [31:0] _GEN_154; // @[proc.scala 287:73:@3419.10]
  wire [31:0] _GEN_155; // @[proc.scala 287:73:@3419.10]
  wire [31:0] _GEN_156; // @[proc.scala 287:73:@3419.10]
  wire [31:0] _GEN_157; // @[proc.scala 287:73:@3419.10]
  wire [3:0] _GEN_158; // @[proc.scala 287:73:@3419.10]
  wire [3:0] _GEN_159; // @[proc.scala 287:73:@3419.10]
  wire [63:0] _GEN_160; // @[proc.scala 285:60:@3414.8]
  wire [63:0] _GEN_161; // @[proc.scala 285:60:@3414.8]
  wire [31:0] _GEN_162; // @[proc.scala 285:60:@3414.8]
  wire [31:0] _GEN_163; // @[proc.scala 285:60:@3414.8]
  wire [31:0] _GEN_164; // @[proc.scala 285:60:@3414.8]
  wire [31:0] _GEN_165; // @[proc.scala 285:60:@3414.8]
  wire [3:0] _GEN_166; // @[proc.scala 285:60:@3414.8]
  wire [3:0] _GEN_167; // @[proc.scala 285:60:@3414.8]
  wire [63:0] _GEN_168; // @[proc.scala 284:40:@3412.6]
  wire [63:0] _GEN_169; // @[proc.scala 284:40:@3412.6]
  wire [31:0] _GEN_170; // @[proc.scala 284:40:@3412.6]
  wire [31:0] _GEN_171; // @[proc.scala 284:40:@3412.6]
  wire [31:0] _GEN_172; // @[proc.scala 284:40:@3412.6]
  wire [31:0] _GEN_173; // @[proc.scala 284:40:@3412.6]
  wire [3:0] _GEN_174; // @[proc.scala 284:40:@3412.6]
  wire [3:0] _GEN_175; // @[proc.scala 284:40:@3412.6]
  wire [3:0] _GEN_188; // @[proc.scala 279:31:@3400.4]
  wire [3:0] _GEN_189; // @[proc.scala 279:31:@3400.4]
  wire [31:0] _GEN_190; // @[proc.scala 279:31:@3400.4]
  wire [31:0] _GEN_191; // @[proc.scala 279:31:@3400.4]
  wire [31:0] _GEN_192; // @[proc.scala 279:31:@3400.4]
  wire [31:0] _GEN_193; // @[proc.scala 279:31:@3400.4]
  wire [63:0] _GEN_194; // @[proc.scala 279:31:@3400.4]
  wire [63:0] _GEN_195; // @[proc.scala 279:31:@3400.4]
  BRAM ppage ( // @[proc.scala 66:21:@3007.4]
    .clock(ppage_clock),
    .io_portA_writeEn(ppage_io_portA_writeEn),
    .io_portA_en(ppage_io_portA_en),
    .io_portA_addr(ppage_io_portA_addr),
    .io_portA_dataIn(ppage_io_portA_dataIn),
    .io_portA_dataOut(ppage_io_portA_dataOut),
    .io_portB_writeEn(ppage_io_portB_writeEn),
    .io_portB_addr(ppage_io_portB_addr),
    .io_portB_dataIn(ppage_io_portB_dataIn),
    .io_portB_dataOut(ppage_io_portB_dataOut)
  );
  BRAM state ( // @[proc.scala 68:21:@3010.4]
    .clock(state_clock),
    .io_portA_writeEn(state_io_portA_writeEn),
    .io_portA_en(state_io_portA_en),
    .io_portA_addr(state_io_portA_addr),
    .io_portA_dataIn(state_io_portA_dataIn),
    .io_portA_dataOut(state_io_portA_dataOut),
    .io_portB_writeEn(state_io_portB_writeEn),
    .io_portB_addr(state_io_portB_addr),
    .io_portB_dataIn(state_io_portB_dataIn),
    .io_portB_dataOut(state_io_portB_dataOut)
  );
  TransplantUnit tpu ( // @[proc.scala 70:19:@3013.4]
    .clock(tpu_clock),
    .reset(tpu_reset),
    .io_host2tpu_fire(tpu_io_host2tpu_fire),
    .io_host2tpu_fireTag(tpu_io_host2tpu_fireTag),
    .io_host2tpu_done(tpu_io_host2tpu_done),
    .io_host2tpu_doneTag(tpu_io_host2tpu_doneTag),
    .io_tpu2cpu_flush(tpu_io_tpu2cpu_flush),
    .io_tpu2cpu_freeze(tpu_io_tpu2cpu_freeze),
    .io_tpu2cpu_fire(tpu_io_tpu2cpu_fire),
    .io_tpu2cpu_done(tpu_io_tpu2cpu_done),
    .io_tpu2cpu_flushTag(tpu_io_tpu2cpu_flushTag),
    .io_tpu2cpu_fireTag(tpu_io_tpu2cpu_fireTag),
    .io_tpu2cpu_freezeTag(tpu_io_tpu2cpu_freezeTag),
    .io_tpu2cpu_doneTag(tpu_io_tpu2cpu_doneTag),
    .io_tpu2cpuStateReg_valid(tpu_io_tpu2cpuStateReg_valid),
    .io_tpu2cpuStateReg_bits(tpu_io_tpu2cpuStateReg_bits),
    .io_tpu2cpuState_PC(tpu_io_tpu2cpuState_PC),
    .io_tpu2cpuState_SP(tpu_io_tpu2cpuState_SP),
    .io_tpu2cpuState_EL(tpu_io_tpu2cpuState_EL),
    .io_tpu2cpuState_NZCV(tpu_io_tpu2cpuState_NZCV),
    .io_cpu2tpuState_PC(tpu_io_cpu2tpuState_PC),
    .io_cpu2tpuState_SP(tpu_io_cpu2tpuState_SP),
    .io_cpu2tpuState_EL(tpu_io_cpu2tpuState_EL),
    .io_cpu2tpuState_NZCV(tpu_io_cpu2tpuState_NZCV),
    .io_rfile_rs1_addr(tpu_io_rfile_rs1_addr),
    .io_rfile_rs1_data(tpu_io_rfile_rs1_data),
    .io_rfile_waddr(tpu_io_rfile_waddr),
    .io_rfile_wdata(tpu_io_rfile_wdata),
    .io_rfile_wen(tpu_io_rfile_wen),
    .io_stateBRAM_writeEn(tpu_io_stateBRAM_writeEn),
    .io_stateBRAM_addr(tpu_io_stateBRAM_addr),
    .io_stateBRAM_dataIn(tpu_io_stateBRAM_dataIn),
    .io_stateBRAM_dataOut(tpu_io_stateBRAM_dataOut)
  );
  RFile RFile ( // @[proc.scala 75:58:@3021.4]
    .clock(RFile_clock),
    .io_rs1_addr(RFile_io_rs1_addr),
    .io_rs1_data(RFile_io_rs1_data),
    .io_rs2_addr(RFile_io_rs2_addr),
    .io_rs2_data(RFile_io_rs2_data),
    .io_waddr(RFile_io_waddr),
    .io_wdata(RFile_io_wdata),
    .io_wen(RFile_io_wen)
  );
  RFile RFile_1 ( // @[proc.scala 75:58:@3024.4]
    .clock(RFile_1_clock),
    .io_rs1_addr(RFile_1_io_rs1_addr),
    .io_rs1_data(RFile_1_io_rs1_data),
    .io_rs2_addr(RFile_1_io_rs2_addr),
    .io_rs2_data(RFile_1_io_rs2_data),
    .io_waddr(RFile_1_io_waddr),
    .io_wdata(RFile_1_io_wdata),
    .io_wen(RFile_1_io_wen)
  );
  FetchUnit fetch ( // @[proc.scala 81:21:@3053.4]
    .clock(fetch_clock),
    .reset(fetch_reset),
    .io_en(fetch_io_en),
    .io_PC(fetch_io_PC),
    .io_tagIn(fetch_io_tagIn),
    .io_incr(fetch_io_incr),
    .io_flush(fetch_io_flush),
    .io_ppageBRAM_addr(fetch_io_ppageBRAM_addr),
    .io_ppageBRAM_dataOut(fetch_io_ppageBRAM_dataOut),
    .io_deq_ready(fetch_io_deq_ready),
    .io_deq_valid(fetch_io_deq_valid),
    .io_deq_bits_inst(fetch_io_deq_bits_inst),
    .io_deq_bits_tag(fetch_io_deq_bits_tag)
  );
  DecodeUnit decoder ( // @[proc.scala 84:23:@3056.4]
    .io_finst_inst(decoder_io_finst_inst),
    .io_finst_tag(decoder_io_finst_tag),
    .io_dinst_rd(decoder_io_dinst_rd),
    .io_dinst_rs1(decoder_io_dinst_rs1),
    .io_dinst_rs2(decoder_io_dinst_rs2),
    .io_dinst_imm(decoder_io_dinst_imm),
    .io_dinst_shift_val(decoder_io_dinst_shift_val),
    .io_dinst_shift_type(decoder_io_dinst_shift_type),
    .io_dinst_cond(decoder_io_dinst_cond),
    .io_dinst_itype(decoder_io_dinst_itype),
    .io_dinst_op(decoder_io_dinst_op),
    .io_dinst_rd_en(decoder_io_dinst_rd_en),
    .io_dinst_rs2_en(decoder_io_dinst_rs2_en),
    .io_dinst_shift_en(decoder_io_dinst_shift_en),
    .io_dinst_nzcv_en(decoder_io_dinst_nzcv_en),
    .io_dinst_tag(decoder_io_dinst_tag)
  );
  FReg decReg ( // @[proc.scala 85:22:@3059.4]
    .clock(decReg_clock),
    .reset(decReg_reset),
    .io_enq_ready(decReg_io_enq_ready),
    .io_enq_valid(decReg_io_enq_valid),
    .io_enq_bits_rd(decReg_io_enq_bits_rd),
    .io_enq_bits_rs1(decReg_io_enq_bits_rs1),
    .io_enq_bits_rs2(decReg_io_enq_bits_rs2),
    .io_enq_bits_imm(decReg_io_enq_bits_imm),
    .io_enq_bits_shift_val(decReg_io_enq_bits_shift_val),
    .io_enq_bits_shift_type(decReg_io_enq_bits_shift_type),
    .io_enq_bits_cond(decReg_io_enq_bits_cond),
    .io_enq_bits_itype(decReg_io_enq_bits_itype),
    .io_enq_bits_op(decReg_io_enq_bits_op),
    .io_enq_bits_rd_en(decReg_io_enq_bits_rd_en),
    .io_enq_bits_rs2_en(decReg_io_enq_bits_rs2_en),
    .io_enq_bits_shift_en(decReg_io_enq_bits_shift_en),
    .io_enq_bits_nzcv_en(decReg_io_enq_bits_nzcv_en),
    .io_enq_bits_tag(decReg_io_enq_bits_tag),
    .io_deq_ready(decReg_io_deq_ready),
    .io_deq_valid(decReg_io_deq_valid),
    .io_deq_bits_rd(decReg_io_deq_bits_rd),
    .io_deq_bits_rs1(decReg_io_deq_bits_rs1),
    .io_deq_bits_rs2(decReg_io_deq_bits_rs2),
    .io_deq_bits_imm(decReg_io_deq_bits_imm),
    .io_deq_bits_shift_val(decReg_io_deq_bits_shift_val),
    .io_deq_bits_shift_type(decReg_io_deq_bits_shift_type),
    .io_deq_bits_cond(decReg_io_deq_bits_cond),
    .io_deq_bits_itype(decReg_io_deq_bits_itype),
    .io_deq_bits_op(decReg_io_deq_bits_op),
    .io_deq_bits_rd_en(decReg_io_deq_bits_rd_en),
    .io_deq_bits_rs2_en(decReg_io_deq_bits_rs2_en),
    .io_deq_bits_shift_en(decReg_io_deq_bits_shift_en),
    .io_deq_bits_nzcv_en(decReg_io_deq_bits_nzcv_en),
    .io_deq_bits_tag(decReg_io_deq_bits_tag),
    .io_flush(decReg_io_flush)
  );
  IssueUnit issuer ( // @[proc.scala 87:22:@3062.4]
    .clock(issuer_clock),
    .reset(issuer_reset),
    .io_flush(issuer_io_flush),
    .io_flushTag(issuer_io_flushTag),
    .io_enq_ready(issuer_io_enq_ready),
    .io_enq_valid(issuer_io_enq_valid),
    .io_enq_bits_rd(issuer_io_enq_bits_rd),
    .io_enq_bits_rs1(issuer_io_enq_bits_rs1),
    .io_enq_bits_rs2(issuer_io_enq_bits_rs2),
    .io_enq_bits_imm(issuer_io_enq_bits_imm),
    .io_enq_bits_shift_val(issuer_io_enq_bits_shift_val),
    .io_enq_bits_shift_type(issuer_io_enq_bits_shift_type),
    .io_enq_bits_cond(issuer_io_enq_bits_cond),
    .io_enq_bits_itype(issuer_io_enq_bits_itype),
    .io_enq_bits_op(issuer_io_enq_bits_op),
    .io_enq_bits_rd_en(issuer_io_enq_bits_rd_en),
    .io_enq_bits_rs2_en(issuer_io_enq_bits_rs2_en),
    .io_enq_bits_shift_en(issuer_io_enq_bits_shift_en),
    .io_enq_bits_nzcv_en(issuer_io_enq_bits_nzcv_en),
    .io_enq_bits_tag(issuer_io_enq_bits_tag),
    .io_deq_valid(issuer_io_deq_valid),
    .io_deq_bits_rd(issuer_io_deq_bits_rd),
    .io_deq_bits_rs1(issuer_io_deq_bits_rs1),
    .io_deq_bits_rs2(issuer_io_deq_bits_rs2),
    .io_deq_bits_imm(issuer_io_deq_bits_imm),
    .io_deq_bits_shift_val(issuer_io_deq_bits_shift_val),
    .io_deq_bits_shift_type(issuer_io_deq_bits_shift_type),
    .io_deq_bits_cond(issuer_io_deq_bits_cond),
    .io_deq_bits_itype(issuer_io_deq_bits_itype),
    .io_deq_bits_op(issuer_io_deq_bits_op),
    .io_deq_bits_rd_en(issuer_io_deq_bits_rd_en),
    .io_deq_bits_rs2_en(issuer_io_deq_bits_rs2_en),
    .io_deq_bits_shift_en(issuer_io_deq_bits_shift_en),
    .io_deq_bits_nzcv_en(issuer_io_deq_bits_nzcv_en),
    .io_deq_bits_tag(issuer_io_deq_bits_tag),
    .io_exeReg_valid(issuer_io_exeReg_valid),
    .io_exeReg_bits_rd(issuer_io_exeReg_bits_rd),
    .io_exeReg_bits_rd_en(issuer_io_exeReg_bits_rd_en),
    .io_exeReg_bits_tag(issuer_io_exeReg_bits_tag)
  );
  ExecuteUnit executer ( // @[proc.scala 90:24:@3065.4]
    .io_dinst_rd(executer_io_dinst_rd),
    .io_dinst_imm(executer_io_dinst_imm),
    .io_dinst_shift_val(executer_io_dinst_shift_val),
    .io_dinst_shift_type(executer_io_dinst_shift_type),
    .io_dinst_itype(executer_io_dinst_itype),
    .io_dinst_op(executer_io_dinst_op),
    .io_dinst_rd_en(executer_io_dinst_rd_en),
    .io_dinst_rs2_en(executer_io_dinst_rs2_en),
    .io_dinst_shift_en(executer_io_dinst_shift_en),
    .io_dinst_nzcv_en(executer_io_dinst_nzcv_en),
    .io_dinst_tag(executer_io_dinst_tag),
    .io_rVal1(executer_io_rVal1),
    .io_rVal2(executer_io_rVal2),
    .io_einst_valid(executer_io_einst_valid),
    .io_einst_bits_res(executer_io_einst_bits_res),
    .io_einst_bits_rd(executer_io_einst_bits_rd),
    .io_einst_bits_rd_en(executer_io_einst_bits_rd_en),
    .io_einst_bits_tag(executer_io_einst_bits_tag),
    .io_einst_bits_nzcv(executer_io_einst_bits_nzcv),
    .io_einst_bits_nzcv_en(executer_io_einst_bits_nzcv_en)
  );
  FReg_1 exeReg ( // @[proc.scala 91:22:@3068.4]
    .clock(exeReg_clock),
    .reset(exeReg_reset),
    .io_enq_valid(exeReg_io_enq_valid),
    .io_enq_bits_res(exeReg_io_enq_bits_res),
    .io_enq_bits_rd(exeReg_io_enq_bits_rd),
    .io_enq_bits_rd_en(exeReg_io_enq_bits_rd_en),
    .io_enq_bits_tag(exeReg_io_enq_bits_tag),
    .io_enq_bits_nzcv(exeReg_io_enq_bits_nzcv),
    .io_enq_bits_nzcv_en(exeReg_io_enq_bits_nzcv_en),
    .io_deq_valid(exeReg_io_deq_valid),
    .io_deq_bits_res(exeReg_io_deq_bits_res),
    .io_deq_bits_rd(exeReg_io_deq_bits_rd),
    .io_deq_bits_rd_en(exeReg_io_deq_bits_rd_en),
    .io_deq_bits_tag(exeReg_io_deq_bits_tag),
    .io_deq_bits_nzcv(exeReg_io_deq_bits_nzcv),
    .io_deq_bits_nzcv_en(exeReg_io_deq_bits_nzcv_en),
    .io_flush(exeReg_io_flush)
  );
  BranchUnit brancher ( // @[proc.scala 93:24:@3071.4]
    .io_dinst_imm(brancher_io_dinst_imm),
    .io_dinst_cond(brancher_io_dinst_cond),
    .io_dinst_itype(brancher_io_dinst_itype),
    .io_dinst_op(brancher_io_dinst_op),
    .io_dinst_tag(brancher_io_dinst_tag),
    .io_nzcv(brancher_io_nzcv),
    .io_binst_valid(brancher_io_binst_valid),
    .io_binst_bits_offset(brancher_io_binst_bits_offset),
    .io_binst_bits_tag(brancher_io_binst_bits_tag)
  );
  FReg_2 brReg ( // @[proc.scala 94:21:@3074.4]
    .clock(brReg_clock),
    .reset(brReg_reset),
    .io_enq_valid(brReg_io_enq_valid),
    .io_enq_bits_offset(brReg_io_enq_bits_offset),
    .io_enq_bits_tag(brReg_io_enq_bits_tag),
    .io_deq_valid(brReg_io_deq_valid),
    .io_deq_bits_offset(brReg_io_deq_bits_offset),
    .io_deq_bits_tag(brReg_io_deq_bits_tag),
    .io_flush(brReg_io_flush)
  );
  LoadStoreUnit ldstU ( // @[proc.scala 96:21:@3077.4]
    .clock(ldstU_clock),
    .reset(ldstU_reset),
    .io_dinst_valid(ldstU_io_dinst_valid),
    .io_dinst_bits_imm(ldstU_io_dinst_bits_imm),
    .io_dinst_bits_itype(ldstU_io_dinst_bits_itype),
    .io_pc(ldstU_io_pc)
  );
  assign _T_186 = fake_PC + 65'h4; // @[proc.scala 129:24:@3128.6]
  assign _T_187 = fake_PC + 65'h4; // @[proc.scala 129:24:@3129.6]
  assign _GEN_4 = fetch_io_incr ? _T_187 : fake_PC; // @[proc.scala 128:23:@3127.4]
  assign _vec_rfile_tpu_io_tpu2cpu_freezeTag_rs1_addr = tpu_io_rfile_rs1_addr; // @[proc.scala 281:41:@3407.6 proc.scala 281:41:@3407.6]
  assign _GEN_136 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_rs1_addr : issuer_io_deq_bits_rs1; // @[proc.scala 281:41:@3407.6]
  assign vec_rfile_0_rs1_data = RFile_io_rs1_data; // @[proc.scala 75:26:@3027.4 proc.scala 75:26:@3033.4]
  assign _GEN_134 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? 5'h0 : issuer_io_deq_bits_rs2; // @[proc.scala 281:41:@3405.6]
  assign vec_rfile_0_rs2_data = RFile_io_rs2_data; // @[proc.scala 75:26:@3027.4 proc.scala 75:26:@3031.4]
  assign _vec_rfile_tpu_io_tpu2cpu_freezeTag_waddr = tpu_io_rfile_waddr; // @[proc.scala 281:41:@3403.6 proc.scala 281:41:@3403.6]
  assign _GEN_27 = exeReg_io_deq_bits_rd; // @[proc.scala 208:39:@3293.6]
  assign _GEN_29 = exeReg_io_deq_valid ? exeReg_io_deq_bits_rd : _GEN_27; // @[proc.scala 205:31:@3288.4]
  assign _GEN_118 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_waddr : _GEN_29; // @[proc.scala 281:41:@3403.6]
  assign _vec_rfile_tpu_io_tpu2cpu_freezeTag_wdata = tpu_io_rfile_wdata; // @[proc.scala 281:41:@3402.6 proc.scala 281:41:@3402.6]
  assign _GEN_28 = exeReg_io_deq_bits_res; // @[proc.scala 208:39:@3293.6]
  assign _GEN_30 = exeReg_io_deq_valid ? exeReg_io_deq_bits_res : _GEN_28; // @[proc.scala 205:31:@3288.4]
  assign _GEN_116 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_wdata : _GEN_30; // @[proc.scala 281:41:@3402.6]
  assign _vec_rfile_tpu_io_tpu2cpu_freezeTag_wen = tpu_io_rfile_wen; // @[proc.scala 281:41:@3401.6 proc.scala 281:41:@3401.6]
  assign _vec_rfile_exeReg_io_deq_bits_tag_wen = exeReg_io_deq_bits_rd_en; // @[proc.scala 218:43:@3317.6 proc.scala 218:43:@3317.6]
  assign _GEN_31 = 1'h0 == exeReg_io_deq_bits_tag ? _vec_rfile_exeReg_io_deq_bits_tag_wen : 1'h0; // @[proc.scala 218:43:@3317.6]
  assign _GEN_37 = exeReg_io_deq_valid ? _GEN_31 : 1'h0; // @[proc.scala 217:29:@3316.4]
  assign _GEN_114 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_wen : _GEN_37; // @[proc.scala 281:41:@3401.6]
  assign _GEN_137 = tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_rs1_addr : issuer_io_deq_bits_rs1; // @[proc.scala 281:41:@3407.6]
  assign vec_rfile_1_rs1_data = RFile_1_io_rs1_data; // @[proc.scala 75:26:@3027.4 proc.scala 75:26:@3040.4]
  assign _GEN_135 = tpu_io_tpu2cpu_freezeTag ? 5'h0 : issuer_io_deq_bits_rs2; // @[proc.scala 281:41:@3405.6]
  assign vec_rfile_1_rs2_data = RFile_1_io_rs2_data; // @[proc.scala 75:26:@3027.4 proc.scala 75:26:@3038.4]
  assign _GEN_119 = tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_waddr : _GEN_29; // @[proc.scala 281:41:@3403.6]
  assign _GEN_117 = tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_wdata : _GEN_30; // @[proc.scala 281:41:@3402.6]
  assign _GEN_32 = exeReg_io_deq_bits_tag ? _vec_rfile_exeReg_io_deq_bits_tag_wen : 1'h0; // @[proc.scala 218:43:@3317.6]
  assign _GEN_38 = exeReg_io_deq_valid ? _GEN_32 : 1'h0; // @[proc.scala 217:29:@3316.4]
  assign _GEN_115 = tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_wen : _GEN_38; // @[proc.scala 281:41:@3401.6]
  assign _T_266 = exeReg_io_deq_valid & exeReg_io_deq_bits_nzcv_en; // @[proc.scala 224:29:@3323.4]
  assign _vec_pregs_exeReg_io_deq_bits_tag_NZCV = exeReg_io_deq_bits_nzcv; // @[proc.scala 225:44:@3325.6 proc.scala 225:44:@3325.6]
  assign _GEN_39 = 1'h0 == exeReg_io_deq_bits_tag ? _vec_pregs_exeReg_io_deq_bits_tag_NZCV : vec_pregs_0_NZCV; // @[proc.scala 225:44:@3325.6]
  assign _GEN_40 = exeReg_io_deq_bits_tag ? _vec_pregs_exeReg_io_deq_bits_tag_NZCV : vec_pregs_1_NZCV; // @[proc.scala 225:44:@3325.6]
  assign _GEN_41 = _T_266 ? _GEN_39 : vec_pregs_0_NZCV; // @[proc.scala 224:60:@3324.4]
  assign _GEN_42 = _T_266 ? _GEN_40 : vec_pregs_1_NZCV; // @[proc.scala 224:60:@3324.4]
  assign _GEN_47 = brReg_io_deq_bits_tag ? vec_pregs_1_PC : vec_pregs_0_PC; // @[proc.scala 232:53:@3329.6]
  assign _T_274 = {1'b0,$signed(_GEN_47)}; // @[proc.scala 232:53:@3329.6]
  assign _T_275 = $signed(brReg_io_deq_bits_offset); // @[proc.scala 232:85:@3330.6]
  assign _GEN_196 = {{1{_T_275[63]}},_T_275}; // @[proc.scala 232:58:@3331.6]
  assign _T_276 = $signed(_T_274) + $signed(_GEN_196); // @[proc.scala 232:58:@3331.6]
  assign _T_277 = $signed(_T_274) + $signed(_GEN_196); // @[proc.scala 232:58:@3332.6]
  assign _T_278 = $signed(_T_277); // @[proc.scala 232:58:@3333.6]
  assign _T_279 = $unsigned(_T_278); // @[proc.scala 232:99:@3334.6]
  assign _vec_pregs_brReg_io_deq_bits_tag_PC = _T_279[63:0]; // @[proc.scala 233:41:@3342.6 proc.scala 233:41:@3342.6]
  assign _GEN_51 = 1'h0 == brReg_io_deq_bits_tag ? _vec_pregs_brReg_io_deq_bits_tag_PC : vec_pregs_0_PC; // @[proc.scala 233:41:@3342.6]
  assign _GEN_52 = brReg_io_deq_bits_tag ? _vec_pregs_brReg_io_deq_bits_tag_PC : vec_pregs_1_PC; // @[proc.scala 233:41:@3342.6]
  assign _GEN_57 = exeReg_io_deq_bits_tag ? vec_pregs_1_PC : vec_pregs_0_PC; // @[proc.scala 236:82:@3347.8]
  assign _T_299 = _GEN_57 + 64'h4; // @[proc.scala 236:82:@3347.8]
  assign _T_300 = _GEN_57 + 64'h4; // @[proc.scala 236:82:@3348.8]
  assign _GEN_61 = 1'h0 == exeReg_io_deq_bits_tag ? _T_300 : vec_pregs_0_PC; // @[proc.scala 236:42:@3349.8]
  assign _GEN_62 = exeReg_io_deq_bits_tag ? _T_300 : vec_pregs_1_PC; // @[proc.scala 236:42:@3349.8]
  assign _GEN_76 = exeReg_io_deq_valid ? _GEN_61 : vec_pregs_0_PC; // @[proc.scala 235:35:@3346.6]
  assign _GEN_77 = exeReg_io_deq_valid ? _GEN_62 : vec_pregs_1_PC; // @[proc.scala 235:35:@3346.6]
  assign _GEN_79 = brReg_io_deq_valid ? _T_279 : _GEN_4; // @[proc.scala 231:28:@3328.4]
  assign _GEN_80 = brReg_io_deq_valid ? _GEN_51 : _GEN_76; // @[proc.scala 231:28:@3328.4]
  assign _GEN_81 = brReg_io_deq_valid ? _GEN_52 : _GEN_77; // @[proc.scala 231:28:@3328.4]
  assign _GEN_87 = tpu_io_tpu2cpu_fireTag ? vec_pregs_1_PC : vec_pregs_0_PC; // @[proc.scala 245:13:@3360.6]
  assign _GEN_91 = 1'h0 == tpu_io_tpu2cpu_fireTag ? 1'h1 : fetch_en_0; // @[proc.scala 246:38:@3361.6]
  assign _GEN_92 = tpu_io_tpu2cpu_fireTag ? 1'h1 : fetch_en_1; // @[proc.scala 246:38:@3361.6]
  assign _GEN_93 = tpu_io_tpu2cpu_fire ? {{1'd0}, _GEN_87} : _GEN_79; // @[proc.scala 244:29:@3359.4]
  assign _GEN_94 = tpu_io_tpu2cpu_fire ? _GEN_91 : fetch_en_0; // @[proc.scala 244:29:@3359.4]
  assign _GEN_95 = tpu_io_tpu2cpu_fire ? _GEN_92 : fetch_en_1; // @[proc.scala 244:29:@3359.4]
  assign _T_317 = issuer_io_deq_bits_itype == 3'h0; // @[proc.scala 248:68:@3363.4]
  assign _T_318 = issuer_io_deq_valid & _T_317; // @[proc.scala 248:46:@3364.4]
  assign _GEN_96 = 1'h0 == tpu_io_tpu2cpu_flushTag ? 1'h0 : _GEN_94; // @[proc.scala 251:39:@3370.6]
  assign _GEN_97 = tpu_io_tpu2cpu_flushTag ? 1'h0 : _GEN_95; // @[proc.scala 251:39:@3370.6]
  assign _GEN_98 = _T_318 ? _GEN_96 : _GEN_94; // @[proc.scala 250:82:@3369.4]
  assign _GEN_99 = _T_318 ? _GEN_97 : _GEN_95; // @[proc.scala 250:82:@3369.4]
  assign _T_325 = fetch_io_deq_bits_tag == tpu_io_tpu2cpu_flushTag; // @[proc.scala 258:45:@3374.6]
  assign _T_326 = decReg_io_deq_bits_tag == tpu_io_tpu2cpu_flushTag; // @[proc.scala 260:47:@3377.6]
  assign _T_327 = brReg_io_deq_bits_tag == tpu_io_tpu2cpu_flushTag; // @[proc.scala 261:45:@3379.6]
  assign _T_328 = exeReg_io_deq_bits_tag == tpu_io_tpu2cpu_flushTag; // @[proc.scala 262:47:@3381.6]
  assign _GEN_110 = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_PC : vec_pregs_0_PC; // @[proc.scala 275:23:@3394.4]
  assign _GEN_111 = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_SP : vec_pregs_0_SP; // @[proc.scala 275:23:@3394.4]
  assign _GEN_112 = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_EL : vec_pregs_0_EL; // @[proc.scala 275:23:@3394.4]
  assign _GEN_113 = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_NZCV : vec_pregs_0_NZCV; // @[proc.scala 275:23:@3394.4]
  assign _GEN_128 = tpu_io_tpu2cpu_freezeTag ? vec_rfile_1_rs1_data : vec_rfile_0_rs1_data; // @[proc.scala 281:41:@3404.6]
  assign _GEN_138 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _GEN_113 : _GEN_41; // @[proc.scala 283:41:@3408.6]
  assign _GEN_139 = tpu_io_tpu2cpu_freezeTag ? _GEN_113 : _GEN_42; // @[proc.scala 283:41:@3408.6]
  assign _GEN_140 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _GEN_112 : vec_pregs_0_EL; // @[proc.scala 283:41:@3409.6]
  assign _GEN_141 = tpu_io_tpu2cpu_freezeTag ? _GEN_112 : vec_pregs_1_EL; // @[proc.scala 283:41:@3409.6]
  assign _GEN_142 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _GEN_111 : vec_pregs_0_SP; // @[proc.scala 283:41:@3410.6]
  assign _GEN_143 = tpu_io_tpu2cpu_freezeTag ? _GEN_111 : vec_pregs_1_SP; // @[proc.scala 283:41:@3410.6]
  assign _GEN_144 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _GEN_110 : _GEN_80; // @[proc.scala 283:41:@3411.6]
  assign _GEN_145 = tpu_io_tpu2cpu_freezeTag ? _GEN_110 : _GEN_81; // @[proc.scala 283:41:@3411.6]
  assign _T_360 = tpu_io_tpu2cpuStateReg_bits == 2'h2; // @[proc.scala 285:40:@3413.8]
  assign _vec_pregs_tpu_io_tpu2cpu_freezeTag_PC_0 = tpu_io_tpu2cpuState_PC; // @[proc.scala 286:48:@3415.10 proc.scala 286:48:@3415.10]
  assign _GEN_146 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_PC_0 : _GEN_144; // @[proc.scala 286:48:@3415.10]
  assign _GEN_147 = tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_PC_0 : _GEN_145; // @[proc.scala 286:48:@3415.10]
  assign _T_364 = tpu_io_tpu2cpuStateReg_bits == 2'h3; // @[proc.scala 287:46:@3418.10]
  assign _vec_pregs_tpu_io_tpu2cpu_freezeTag_SP_0 = tpu_io_tpu2cpuState_SP; // @[proc.scala 288:48:@3420.12 proc.scala 288:48:@3420.12]
  assign _GEN_148 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_SP_0 : _GEN_142; // @[proc.scala 288:48:@3420.12]
  assign _GEN_149 = tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_SP_0 : _GEN_143; // @[proc.scala 288:48:@3420.12]
  assign _vec_pregs_tpu_io_tpu2cpu_freezeTag_EL_0 = tpu_io_tpu2cpuState_EL; // @[proc.scala 289:48:@3421.12 proc.scala 289:48:@3421.12]
  assign _GEN_150 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_EL_0 : _GEN_140; // @[proc.scala 289:48:@3421.12]
  assign _GEN_151 = tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_EL_0 : _GEN_141; // @[proc.scala 289:48:@3421.12]
  assign _vec_pregs_tpu_io_tpu2cpu_freezeTag_NZCV_0 = tpu_io_tpu2cpuState_NZCV; // @[proc.scala 290:50:@3422.12 proc.scala 290:50:@3422.12]
  assign _GEN_152 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_NZCV_0 : _GEN_138; // @[proc.scala 290:50:@3422.12]
  assign _GEN_153 = tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_NZCV_0 : _GEN_139; // @[proc.scala 290:50:@3422.12]
  assign _GEN_154 = _T_364 ? _GEN_148 : _GEN_142; // @[proc.scala 287:73:@3419.10]
  assign _GEN_155 = _T_364 ? _GEN_149 : _GEN_143; // @[proc.scala 287:73:@3419.10]
  assign _GEN_156 = _T_364 ? _GEN_150 : _GEN_140; // @[proc.scala 287:73:@3419.10]
  assign _GEN_157 = _T_364 ? _GEN_151 : _GEN_141; // @[proc.scala 287:73:@3419.10]
  assign _GEN_158 = _T_364 ? _GEN_152 : _GEN_138; // @[proc.scala 287:73:@3419.10]
  assign _GEN_159 = _T_364 ? _GEN_153 : _GEN_139; // @[proc.scala 287:73:@3419.10]
  assign _GEN_160 = _T_360 ? _GEN_146 : _GEN_144; // @[proc.scala 285:60:@3414.8]
  assign _GEN_161 = _T_360 ? _GEN_147 : _GEN_145; // @[proc.scala 285:60:@3414.8]
  assign _GEN_162 = _T_360 ? _GEN_142 : _GEN_154; // @[proc.scala 285:60:@3414.8]
  assign _GEN_163 = _T_360 ? _GEN_143 : _GEN_155; // @[proc.scala 285:60:@3414.8]
  assign _GEN_164 = _T_360 ? _GEN_140 : _GEN_156; // @[proc.scala 285:60:@3414.8]
  assign _GEN_165 = _T_360 ? _GEN_141 : _GEN_157; // @[proc.scala 285:60:@3414.8]
  assign _GEN_166 = _T_360 ? _GEN_138 : _GEN_158; // @[proc.scala 285:60:@3414.8]
  assign _GEN_167 = _T_360 ? _GEN_139 : _GEN_159; // @[proc.scala 285:60:@3414.8]
  assign _GEN_168 = tpu_io_tpu2cpuStateReg_valid ? _GEN_160 : _GEN_144; // @[proc.scala 284:40:@3412.6]
  assign _GEN_169 = tpu_io_tpu2cpuStateReg_valid ? _GEN_161 : _GEN_145; // @[proc.scala 284:40:@3412.6]
  assign _GEN_170 = tpu_io_tpu2cpuStateReg_valid ? _GEN_162 : _GEN_142; // @[proc.scala 284:40:@3412.6]
  assign _GEN_171 = tpu_io_tpu2cpuStateReg_valid ? _GEN_163 : _GEN_143; // @[proc.scala 284:40:@3412.6]
  assign _GEN_172 = tpu_io_tpu2cpuStateReg_valid ? _GEN_164 : _GEN_140; // @[proc.scala 284:40:@3412.6]
  assign _GEN_173 = tpu_io_tpu2cpuStateReg_valid ? _GEN_165 : _GEN_141; // @[proc.scala 284:40:@3412.6]
  assign _GEN_174 = tpu_io_tpu2cpuStateReg_valid ? _GEN_166 : _GEN_138; // @[proc.scala 284:40:@3412.6]
  assign _GEN_175 = tpu_io_tpu2cpuStateReg_valid ? _GEN_167 : _GEN_139; // @[proc.scala 284:40:@3412.6]
  assign _GEN_188 = tpu_io_tpu2cpu_freeze ? _GEN_174 : _GEN_41; // @[proc.scala 279:31:@3400.4]
  assign _GEN_189 = tpu_io_tpu2cpu_freeze ? _GEN_175 : _GEN_42; // @[proc.scala 279:31:@3400.4]
  assign _GEN_190 = tpu_io_tpu2cpu_freeze ? _GEN_172 : vec_pregs_0_EL; // @[proc.scala 279:31:@3400.4]
  assign _GEN_191 = tpu_io_tpu2cpu_freeze ? _GEN_173 : vec_pregs_1_EL; // @[proc.scala 279:31:@3400.4]
  assign _GEN_192 = tpu_io_tpu2cpu_freeze ? _GEN_170 : vec_pregs_0_SP; // @[proc.scala 279:31:@3400.4]
  assign _GEN_193 = tpu_io_tpu2cpu_freeze ? _GEN_171 : vec_pregs_1_SP; // @[proc.scala 279:31:@3400.4]
  assign _GEN_194 = tpu_io_tpu2cpu_freeze ? _GEN_168 : _GEN_80; // @[proc.scala 279:31:@3400.4]
  assign _GEN_195 = tpu_io_tpu2cpu_freeze ? _GEN_169 : _GEN_81; // @[proc.scala 279:31:@3400.4]
  assign io_ppageBRAM_dataOut = ppage_io_portA_dataOut; // @[proc.scala 106:16:@3088.4]
  assign io_stateBRAM_dataOut = state_io_portA_dataOut; // @[proc.scala 107:16:@3093.4]
  assign io_host2tpu_done = tpu_io_host2tpu_done; // @[proc.scala 109:15:@3099.4]
  assign io_host2tpu_doneTag = tpu_io_host2tpu_doneTag; // @[proc.scala 109:15:@3098.4]
  assign ppage_clock = clock; // @[:@3008.4]
  assign ppage_io_portA_writeEn = io_ppageBRAM_writeEn; // @[proc.scala 106:16:@3092.4]
  assign ppage_io_portA_en = io_ppageBRAM_en; // @[proc.scala 106:16:@3091.4]
  assign ppage_io_portA_addr = io_ppageBRAM_addr; // @[proc.scala 106:16:@3090.4]
  assign ppage_io_portA_dataIn = io_ppageBRAM_dataIn; // @[proc.scala 106:16:@3089.4]
  assign ppage_io_portB_writeEn = 1'h0; // @[proc.scala 117:22:@3113.4]
  assign ppage_io_portB_addr = fetch_io_ppageBRAM_addr; // @[proc.scala 117:22:@3111.4]
  assign ppage_io_portB_dataIn = 36'h0; // @[proc.scala 117:22:@3110.4]
  assign state_clock = clock; // @[:@3011.4]
  assign state_io_portA_writeEn = io_stateBRAM_writeEn; // @[proc.scala 107:16:@3097.4]
  assign state_io_portA_en = io_stateBRAM_en; // @[proc.scala 107:16:@3096.4]
  assign state_io_portA_addr = io_stateBRAM_addr; // @[proc.scala 107:16:@3095.4]
  assign state_io_portA_dataIn = io_stateBRAM_dataIn; // @[proc.scala 107:16:@3094.4]
  assign state_io_portB_writeEn = tpu_io_stateBRAM_writeEn; // @[proc.scala 112:20:@3106.4]
  assign state_io_portB_addr = tpu_io_stateBRAM_addr; // @[proc.scala 112:20:@3104.4]
  assign state_io_portB_dataIn = tpu_io_stateBRAM_dataIn; // @[proc.scala 112:20:@3103.4]
  assign tpu_clock = clock; // @[:@3014.4]
  assign tpu_reset = reset; // @[:@3015.4]
  assign tpu_io_host2tpu_fire = io_host2tpu_fire; // @[proc.scala 109:15:@3101.4]
  assign tpu_io_host2tpu_fireTag = io_host2tpu_fireTag; // @[proc.scala 109:15:@3100.4]
  assign tpu_io_tpu2cpu_done = issuer_io_deq_valid & _T_317; // @[proc.scala 113:23:@3107.4 proc.scala 248:23:@3365.4]
  assign tpu_io_tpu2cpu_doneTag = issuer_io_deq_bits_tag; // @[proc.scala 114:26:@3108.4 proc.scala 249:26:@3366.4]
  assign tpu_io_cpu2tpuState_PC = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_PC : vec_pregs_0_PC; // @[proc.scala 275:23:@3397.4]
  assign tpu_io_cpu2tpuState_SP = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_SP : vec_pregs_0_SP; // @[proc.scala 275:23:@3396.4]
  assign tpu_io_cpu2tpuState_EL = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_EL : vec_pregs_0_EL; // @[proc.scala 275:23:@3395.4]
  assign tpu_io_cpu2tpuState_NZCV = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_NZCV : vec_pregs_0_NZCV; // @[proc.scala 275:23:@3394.4]
  assign tpu_io_rfile_rs1_data = tpu_io_tpu2cpu_freeze ? _GEN_128 : vec_rfile_0_rs1_data; // @[proc.scala 276:25:@3398.4 proc.scala 281:41:@3406.6]
  assign tpu_io_stateBRAM_dataOut = state_io_portB_dataOut; // @[proc.scala 112:20:@3102.4]
  assign RFile_clock = clock; // @[:@3022.4]
  assign RFile_io_rs1_addr = tpu_io_tpu2cpu_freeze ? _GEN_136 : issuer_io_deq_bits_rs1; // @[proc.scala 75:26:@3034.4]
  assign RFile_io_rs2_addr = tpu_io_tpu2cpu_freeze ? _GEN_134 : issuer_io_deq_bits_rs2; // @[proc.scala 75:26:@3032.4]
  assign RFile_io_waddr = tpu_io_tpu2cpu_freeze ? _GEN_118 : _GEN_29; // @[proc.scala 75:26:@3030.4]
  assign RFile_io_wdata = tpu_io_tpu2cpu_freeze ? _GEN_116 : _GEN_30; // @[proc.scala 75:26:@3029.4]
  assign RFile_io_wen = tpu_io_tpu2cpu_freeze ? _GEN_114 : _GEN_37; // @[proc.scala 75:26:@3028.4]
  assign RFile_1_clock = clock; // @[:@3025.4]
  assign RFile_1_io_rs1_addr = tpu_io_tpu2cpu_freeze ? _GEN_137 : issuer_io_deq_bits_rs1; // @[proc.scala 75:26:@3041.4]
  assign RFile_1_io_rs2_addr = tpu_io_tpu2cpu_freeze ? _GEN_135 : issuer_io_deq_bits_rs2; // @[proc.scala 75:26:@3039.4]
  assign RFile_1_io_waddr = tpu_io_tpu2cpu_freeze ? _GEN_119 : _GEN_29; // @[proc.scala 75:26:@3037.4]
  assign RFile_1_io_wdata = tpu_io_tpu2cpu_freeze ? _GEN_117 : _GEN_30; // @[proc.scala 75:26:@3036.4]
  assign RFile_1_io_wen = tpu_io_tpu2cpu_freeze ? _GEN_115 : _GEN_38; // @[proc.scala 75:26:@3035.4]
  assign fetch_clock = clock; // @[:@3054.4]
  assign fetch_reset = reset; // @[:@3055.4]
  assign fetch_io_en = fetch_en_1 ? fetch_en_1 : fetch_en_0; // @[proc.scala 126:15:@3125.4]
  assign fetch_io_PC = fake_PC[63:0]; // @[proc.scala 127:15:@3126.4]
  assign fetch_io_tagIn = fetch_en_1; // @[proc.scala 125:18:@3122.4]
  assign fetch_io_flush = tpu_io_tpu2cpu_flush ? _T_325 : 1'h0; // @[proc.scala 258:20:@3375.6 proc.scala 265:20:@3387.6]
  assign fetch_io_ppageBRAM_dataOut = ppage_io_portB_dataOut; // @[proc.scala 117:22:@3109.4]
  assign fetch_io_deq_ready = decReg_io_enq_ready; // @[proc.scala 136:22:@3155.4]
  assign decoder_io_finst_inst = fetch_io_deq_bits_inst; // @[proc.scala 133:20:@3134.4]
  assign decoder_io_finst_tag = fetch_io_deq_bits_tag; // @[proc.scala 133:20:@3133.4]
  assign decReg_clock = clock; // @[:@3060.4]
  assign decReg_reset = reset; // @[:@3061.4]
  assign decReg_io_enq_valid = fetch_io_deq_valid; // @[proc.scala 137:23:@3156.4]
  assign decReg_io_enq_bits_rd = decoder_io_dinst_rd; // @[proc.scala 134:22:@3154.4]
  assign decReg_io_enq_bits_rs1 = decoder_io_dinst_rs1; // @[proc.scala 134:22:@3153.4]
  assign decReg_io_enq_bits_rs2 = decoder_io_dinst_rs2; // @[proc.scala 134:22:@3152.4]
  assign decReg_io_enq_bits_imm = decoder_io_dinst_imm; // @[proc.scala 134:22:@3151.4]
  assign decReg_io_enq_bits_shift_val = decoder_io_dinst_shift_val; // @[proc.scala 134:22:@3150.4]
  assign decReg_io_enq_bits_shift_type = decoder_io_dinst_shift_type; // @[proc.scala 134:22:@3149.4]
  assign decReg_io_enq_bits_cond = decoder_io_dinst_cond; // @[proc.scala 134:22:@3148.4]
  assign decReg_io_enq_bits_itype = decoder_io_dinst_itype; // @[proc.scala 134:22:@3147.4]
  assign decReg_io_enq_bits_op = decoder_io_dinst_op; // @[proc.scala 134:22:@3146.4]
  assign decReg_io_enq_bits_rd_en = decoder_io_dinst_rd_en; // @[proc.scala 134:22:@3145.4]
  assign decReg_io_enq_bits_rs2_en = decoder_io_dinst_rs2_en; // @[proc.scala 134:22:@3143.4]
  assign decReg_io_enq_bits_shift_en = decoder_io_dinst_shift_en; // @[proc.scala 134:22:@3141.4]
  assign decReg_io_enq_bits_nzcv_en = decoder_io_dinst_nzcv_en; // @[proc.scala 134:22:@3139.4]
  assign decReg_io_enq_bits_tag = decoder_io_dinst_tag; // @[proc.scala 134:22:@3137.4]
  assign decReg_io_deq_ready = issuer_io_enq_ready; // @[proc.scala 140:17:@3178.4]
  assign decReg_io_flush = tpu_io_tpu2cpu_flush ? _T_326 : 1'h0; // @[proc.scala 260:21:@3378.6 proc.scala 267:21:@3389.6]
  assign issuer_clock = clock; // @[:@3063.4]
  assign issuer_reset = reset; // @[:@3064.4]
  assign issuer_io_flush = tpu_io_tpu2cpu_flush ? tpu_io_tpu2cpu_flush : 1'h0; // @[proc.scala 259:21:@3376.6 proc.scala 266:21:@3388.6]
  assign issuer_io_flushTag = tpu_io_tpu2cpu_flushTag; // @[proc.scala 256:22:@3372.4]
  assign issuer_io_enq_valid = decReg_io_deq_valid; // @[proc.scala 140:17:@3177.4]
  assign issuer_io_enq_bits_rd = decReg_io_deq_bits_rd; // @[proc.scala 140:17:@3176.4]
  assign issuer_io_enq_bits_rs1 = decReg_io_deq_bits_rs1; // @[proc.scala 140:17:@3175.4]
  assign issuer_io_enq_bits_rs2 = decReg_io_deq_bits_rs2; // @[proc.scala 140:17:@3174.4]
  assign issuer_io_enq_bits_imm = decReg_io_deq_bits_imm; // @[proc.scala 140:17:@3173.4]
  assign issuer_io_enq_bits_shift_val = decReg_io_deq_bits_shift_val; // @[proc.scala 140:17:@3172.4]
  assign issuer_io_enq_bits_shift_type = decReg_io_deq_bits_shift_type; // @[proc.scala 140:17:@3171.4]
  assign issuer_io_enq_bits_cond = decReg_io_deq_bits_cond; // @[proc.scala 140:17:@3170.4]
  assign issuer_io_enq_bits_itype = decReg_io_deq_bits_itype; // @[proc.scala 140:17:@3169.4]
  assign issuer_io_enq_bits_op = decReg_io_deq_bits_op; // @[proc.scala 140:17:@3168.4]
  assign issuer_io_enq_bits_rd_en = decReg_io_deq_bits_rd_en; // @[proc.scala 140:17:@3167.4]
  assign issuer_io_enq_bits_rs2_en = decReg_io_deq_bits_rs2_en; // @[proc.scala 140:17:@3165.4]
  assign issuer_io_enq_bits_shift_en = decReg_io_deq_bits_shift_en; // @[proc.scala 140:17:@3163.4]
  assign issuer_io_enq_bits_nzcv_en = decReg_io_deq_bits_nzcv_en; // @[proc.scala 140:17:@3161.4]
  assign issuer_io_enq_bits_tag = decReg_io_deq_bits_tag; // @[proc.scala 140:17:@3159.4]
  assign issuer_io_exeReg_valid = exeReg_io_deq_valid; // @[proc.scala 148:26:@3187.4]
  assign issuer_io_exeReg_bits_rd = exeReg_io_deq_bits_rd; // @[proc.scala 147:26:@3185.4]
  assign issuer_io_exeReg_bits_rd_en = exeReg_io_deq_bits_rd_en; // @[proc.scala 147:26:@3184.4]
  assign issuer_io_exeReg_bits_tag = exeReg_io_deq_bits_tag; // @[proc.scala 147:26:@3183.4]
  assign executer_io_dinst_rd = issuer_io_deq_bits_rd; // @[proc.scala 161:21:@3211.4]
  assign executer_io_dinst_imm = issuer_io_deq_bits_imm; // @[proc.scala 161:21:@3208.4]
  assign executer_io_dinst_shift_val = issuer_io_deq_bits_shift_val; // @[proc.scala 161:21:@3207.4]
  assign executer_io_dinst_shift_type = issuer_io_deq_bits_shift_type; // @[proc.scala 161:21:@3206.4]
  assign executer_io_dinst_itype = issuer_io_deq_bits_itype; // @[proc.scala 161:21:@3204.4]
  assign executer_io_dinst_op = issuer_io_deq_bits_op; // @[proc.scala 161:21:@3203.4]
  assign executer_io_dinst_rd_en = issuer_io_deq_bits_rd_en; // @[proc.scala 161:21:@3202.4]
  assign executer_io_dinst_rs2_en = issuer_io_deq_bits_rs2_en; // @[proc.scala 161:21:@3200.4]
  assign executer_io_dinst_shift_en = issuer_io_deq_bits_shift_en; // @[proc.scala 161:21:@3198.4]
  assign executer_io_dinst_nzcv_en = issuer_io_deq_bits_nzcv_en; // @[proc.scala 161:21:@3196.4]
  assign executer_io_dinst_tag = issuer_io_deq_bits_tag; // @[proc.scala 161:21:@3194.4]
  assign executer_io_rVal1 = issuer_io_deq_bits_tag ? vec_rfile_1_rs1_data : vec_rfile_0_rs1_data; // @[proc.scala 162:21:@3212.4]
  assign executer_io_rVal2 = issuer_io_deq_bits_tag ? vec_rfile_1_rs2_data : vec_rfile_0_rs2_data; // @[proc.scala 163:21:@3213.4]
  assign exeReg_clock = clock; // @[:@3069.4]
  assign exeReg_reset = reset; // @[:@3070.4]
  assign exeReg_io_enq_valid = executer_io_einst_valid & issuer_io_deq_valid; // @[proc.scala 165:23:@3216.4]
  assign exeReg_io_enq_bits_res = executer_io_einst_bits_res; // @[proc.scala 166:23:@3222.4]
  assign exeReg_io_enq_bits_rd = executer_io_einst_bits_rd; // @[proc.scala 166:23:@3221.4]
  assign exeReg_io_enq_bits_rd_en = executer_io_einst_bits_rd_en; // @[proc.scala 166:23:@3220.4]
  assign exeReg_io_enq_bits_tag = executer_io_einst_bits_tag; // @[proc.scala 166:23:@3219.4]
  assign exeReg_io_enq_bits_nzcv = executer_io_einst_bits_nzcv; // @[proc.scala 166:23:@3218.4]
  assign exeReg_io_enq_bits_nzcv_en = executer_io_einst_bits_nzcv_en; // @[proc.scala 166:23:@3217.4]
  assign exeReg_io_flush = tpu_io_tpu2cpu_flush ? _T_328 : 1'h0; // @[proc.scala 262:21:@3382.6 proc.scala 269:21:@3391.6]
  assign brancher_io_dinst_imm = issuer_io_deq_bits_imm; // @[proc.scala 169:21:@3239.4]
  assign brancher_io_dinst_cond = issuer_io_deq_bits_cond; // @[proc.scala 169:21:@3236.4]
  assign brancher_io_dinst_itype = issuer_io_deq_bits_itype; // @[proc.scala 169:21:@3235.4]
  assign brancher_io_dinst_op = issuer_io_deq_bits_op; // @[proc.scala 169:21:@3234.4]
  assign brancher_io_dinst_tag = issuer_io_deq_bits_tag; // @[proc.scala 169:21:@3225.4]
  assign brancher_io_nzcv = issuer_io_deq_bits_tag ? vec_pregs_1_NZCV : vec_pregs_0_NZCV; // @[proc.scala 170:20:@3243.4]
  assign brReg_clock = clock; // @[:@3075.4]
  assign brReg_reset = reset; // @[:@3076.4]
  assign brReg_io_enq_valid = issuer_io_deq_valid & brancher_io_binst_valid; // @[proc.scala 173:22:@3245.4]
  assign brReg_io_enq_bits_offset = brancher_io_binst_bits_offset; // @[proc.scala 174:28:@3246.4]
  assign brReg_io_enq_bits_tag = brancher_io_binst_bits_tag; // @[proc.scala 175:25:@3247.4]
  assign brReg_io_flush = tpu_io_tpu2cpu_flush ? _T_327 : 1'h0; // @[proc.scala 261:20:@3380.6 proc.scala 268:20:@3390.6]
  assign ldstU_clock = clock; // @[:@3078.4]
  assign ldstU_reset = reset; // @[:@3079.4]
  assign ldstU_io_dinst_valid = issuer_io_deq_valid; // @[proc.scala 179:24:@3268.4]
  assign ldstU_io_dinst_bits_imm = issuer_io_deq_bits_imm; // @[proc.scala 178:23:@3264.4]
  assign ldstU_io_dinst_bits_itype = issuer_io_deq_bits_itype; // @[proc.scala 178:23:@3260.4]
  assign ldstU_io_pc = issuer_io_deq_bits_tag ? vec_pregs_1_PC : vec_pregs_0_PC; // @[proc.scala 182:18:@3271.4]
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
  _RAND_0 = {2{`RANDOM}};
  vec_pregs_0_PC = _RAND_0[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  vec_pregs_0_SP = _RAND_1[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  vec_pregs_0_EL = _RAND_2[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  vec_pregs_0_NZCV = _RAND_3[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {2{`RANDOM}};
  vec_pregs_1_PC = _RAND_4[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  vec_pregs_1_SP = _RAND_5[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  vec_pregs_1_EL = _RAND_6[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  vec_pregs_1_NZCV = _RAND_7[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {3{`RANDOM}};
  fake_PC = _RAND_8[64:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  fetch_en_0 = _RAND_9[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  fetch_en_1 = _RAND_10[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (reset) begin
      vec_pregs_0_PC <= 64'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_360) begin
            if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
              vec_pregs_0_PC <= _vec_pregs_tpu_io_tpu2cpu_freezeTag_PC_0;
            end else begin
              if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
                if (tpu_io_tpu2cpu_freezeTag) begin
                  vec_pregs_0_PC <= vec_pregs_1_PC;
                end
              end else begin
                if (brReg_io_deq_valid) begin
                  if (1'h0 == brReg_io_deq_bits_tag) begin
                    vec_pregs_0_PC <= _vec_pregs_brReg_io_deq_bits_tag_PC;
                  end
                end else begin
                  if (exeReg_io_deq_valid) begin
                    if (1'h0 == exeReg_io_deq_bits_tag) begin
                      vec_pregs_0_PC <= _T_300;
                    end
                  end
                end
              end
            end
          end else begin
            if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
              if (tpu_io_tpu2cpu_freezeTag) begin
                vec_pregs_0_PC <= vec_pregs_1_PC;
              end
            end else begin
              if (brReg_io_deq_valid) begin
                if (1'h0 == brReg_io_deq_bits_tag) begin
                  vec_pregs_0_PC <= _vec_pregs_brReg_io_deq_bits_tag_PC;
                end
              end else begin
                if (exeReg_io_deq_valid) begin
                  if (1'h0 == exeReg_io_deq_bits_tag) begin
                    vec_pregs_0_PC <= _T_300;
                  end
                end
              end
            end
          end
        end else begin
          if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
            if (tpu_io_tpu2cpu_freezeTag) begin
              vec_pregs_0_PC <= vec_pregs_1_PC;
            end
          end else begin
            if (brReg_io_deq_valid) begin
              if (1'h0 == brReg_io_deq_bits_tag) begin
                vec_pregs_0_PC <= _vec_pregs_brReg_io_deq_bits_tag_PC;
              end
            end else begin
              if (exeReg_io_deq_valid) begin
                if (1'h0 == exeReg_io_deq_bits_tag) begin
                  vec_pregs_0_PC <= _T_300;
                end
              end
            end
          end
        end
      end else begin
        if (brReg_io_deq_valid) begin
          if (1'h0 == brReg_io_deq_bits_tag) begin
            vec_pregs_0_PC <= _vec_pregs_brReg_io_deq_bits_tag_PC;
          end
        end else begin
          if (exeReg_io_deq_valid) begin
            if (1'h0 == exeReg_io_deq_bits_tag) begin
              vec_pregs_0_PC <= _T_300;
            end
          end
        end
      end
    end
    if (reset) begin
      vec_pregs_0_SP <= 32'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_360) begin
            if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
              if (tpu_io_tpu2cpu_freezeTag) begin
                vec_pregs_0_SP <= vec_pregs_1_SP;
              end
            end
          end else begin
            if (_T_364) begin
              if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
                vec_pregs_0_SP <= _vec_pregs_tpu_io_tpu2cpu_freezeTag_SP_0;
              end else begin
                if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
                  if (tpu_io_tpu2cpu_freezeTag) begin
                    vec_pregs_0_SP <= vec_pregs_1_SP;
                  end
                end
              end
            end else begin
              if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
                if (tpu_io_tpu2cpu_freezeTag) begin
                  vec_pregs_0_SP <= vec_pregs_1_SP;
                end
              end
            end
          end
        end else begin
          if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
            if (tpu_io_tpu2cpu_freezeTag) begin
              vec_pregs_0_SP <= vec_pregs_1_SP;
            end
          end
        end
      end
    end
    if (reset) begin
      vec_pregs_0_EL <= 32'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_360) begin
            if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
              if (tpu_io_tpu2cpu_freezeTag) begin
                vec_pregs_0_EL <= vec_pregs_1_EL;
              end
            end
          end else begin
            if (_T_364) begin
              if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
                vec_pregs_0_EL <= _vec_pregs_tpu_io_tpu2cpu_freezeTag_EL_0;
              end else begin
                if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
                  if (tpu_io_tpu2cpu_freezeTag) begin
                    vec_pregs_0_EL <= vec_pregs_1_EL;
                  end
                end
              end
            end else begin
              if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
                if (tpu_io_tpu2cpu_freezeTag) begin
                  vec_pregs_0_EL <= vec_pregs_1_EL;
                end
              end
            end
          end
        end else begin
          if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
            if (tpu_io_tpu2cpu_freezeTag) begin
              vec_pregs_0_EL <= vec_pregs_1_EL;
            end
          end
        end
      end
    end
    if (reset) begin
      vec_pregs_0_NZCV <= 4'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_360) begin
            if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
              if (tpu_io_tpu2cpu_freezeTag) begin
                vec_pregs_0_NZCV <= vec_pregs_1_NZCV;
              end
            end else begin
              if (_T_266) begin
                if (1'h0 == exeReg_io_deq_bits_tag) begin
                  vec_pregs_0_NZCV <= _vec_pregs_exeReg_io_deq_bits_tag_NZCV;
                end
              end
            end
          end else begin
            if (_T_364) begin
              if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
                vec_pregs_0_NZCV <= _vec_pregs_tpu_io_tpu2cpu_freezeTag_NZCV_0;
              end else begin
                if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
                  if (tpu_io_tpu2cpu_freezeTag) begin
                    vec_pregs_0_NZCV <= vec_pregs_1_NZCV;
                  end
                end else begin
                  if (_T_266) begin
                    if (1'h0 == exeReg_io_deq_bits_tag) begin
                      vec_pregs_0_NZCV <= _vec_pregs_exeReg_io_deq_bits_tag_NZCV;
                    end
                  end
                end
              end
            end else begin
              if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
                if (tpu_io_tpu2cpu_freezeTag) begin
                  vec_pregs_0_NZCV <= vec_pregs_1_NZCV;
                end
              end else begin
                if (_T_266) begin
                  if (1'h0 == exeReg_io_deq_bits_tag) begin
                    vec_pregs_0_NZCV <= _vec_pregs_exeReg_io_deq_bits_tag_NZCV;
                  end
                end
              end
            end
          end
        end else begin
          if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
            if (tpu_io_tpu2cpu_freezeTag) begin
              vec_pregs_0_NZCV <= vec_pregs_1_NZCV;
            end
          end else begin
            if (_T_266) begin
              if (1'h0 == exeReg_io_deq_bits_tag) begin
                vec_pregs_0_NZCV <= _vec_pregs_exeReg_io_deq_bits_tag_NZCV;
              end
            end
          end
        end
      end else begin
        vec_pregs_0_NZCV <= _GEN_41;
      end
    end
    if (reset) begin
      vec_pregs_1_PC <= 64'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_360) begin
            if (tpu_io_tpu2cpu_freezeTag) begin
              vec_pregs_1_PC <= _vec_pregs_tpu_io_tpu2cpu_freezeTag_PC_0;
            end else begin
              if (tpu_io_tpu2cpu_freezeTag) begin
                if (!(tpu_io_tpu2cpu_freezeTag)) begin
                  vec_pregs_1_PC <= vec_pregs_0_PC;
                end
              end else begin
                if (brReg_io_deq_valid) begin
                  if (brReg_io_deq_bits_tag) begin
                    vec_pregs_1_PC <= _vec_pregs_brReg_io_deq_bits_tag_PC;
                  end
                end else begin
                  if (exeReg_io_deq_valid) begin
                    if (exeReg_io_deq_bits_tag) begin
                      vec_pregs_1_PC <= _T_300;
                    end
                  end
                end
              end
            end
          end else begin
            if (tpu_io_tpu2cpu_freezeTag) begin
              if (!(tpu_io_tpu2cpu_freezeTag)) begin
                vec_pregs_1_PC <= vec_pregs_0_PC;
              end
            end else begin
              if (brReg_io_deq_valid) begin
                if (brReg_io_deq_bits_tag) begin
                  vec_pregs_1_PC <= _vec_pregs_brReg_io_deq_bits_tag_PC;
                end
              end else begin
                if (exeReg_io_deq_valid) begin
                  if (exeReg_io_deq_bits_tag) begin
                    vec_pregs_1_PC <= _T_300;
                  end
                end
              end
            end
          end
        end else begin
          if (tpu_io_tpu2cpu_freezeTag) begin
            if (!(tpu_io_tpu2cpu_freezeTag)) begin
              vec_pregs_1_PC <= vec_pregs_0_PC;
            end
          end else begin
            if (brReg_io_deq_valid) begin
              if (brReg_io_deq_bits_tag) begin
                vec_pregs_1_PC <= _vec_pregs_brReg_io_deq_bits_tag_PC;
              end
            end else begin
              if (exeReg_io_deq_valid) begin
                if (exeReg_io_deq_bits_tag) begin
                  vec_pregs_1_PC <= _T_300;
                end
              end
            end
          end
        end
      end else begin
        if (brReg_io_deq_valid) begin
          if (brReg_io_deq_bits_tag) begin
            vec_pregs_1_PC <= _vec_pregs_brReg_io_deq_bits_tag_PC;
          end
        end else begin
          if (exeReg_io_deq_valid) begin
            if (exeReg_io_deq_bits_tag) begin
              vec_pregs_1_PC <= _T_300;
            end
          end
        end
      end
    end
    if (reset) begin
      vec_pregs_1_SP <= 32'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_360) begin
            if (tpu_io_tpu2cpu_freezeTag) begin
              if (!(tpu_io_tpu2cpu_freezeTag)) begin
                vec_pregs_1_SP <= vec_pregs_0_SP;
              end
            end
          end else begin
            if (_T_364) begin
              if (tpu_io_tpu2cpu_freezeTag) begin
                vec_pregs_1_SP <= _vec_pregs_tpu_io_tpu2cpu_freezeTag_SP_0;
              end else begin
                if (tpu_io_tpu2cpu_freezeTag) begin
                  if (!(tpu_io_tpu2cpu_freezeTag)) begin
                    vec_pregs_1_SP <= vec_pregs_0_SP;
                  end
                end
              end
            end else begin
              if (tpu_io_tpu2cpu_freezeTag) begin
                if (!(tpu_io_tpu2cpu_freezeTag)) begin
                  vec_pregs_1_SP <= vec_pregs_0_SP;
                end
              end
            end
          end
        end else begin
          if (tpu_io_tpu2cpu_freezeTag) begin
            if (!(tpu_io_tpu2cpu_freezeTag)) begin
              vec_pregs_1_SP <= vec_pregs_0_SP;
            end
          end
        end
      end
    end
    if (reset) begin
      vec_pregs_1_EL <= 32'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_360) begin
            if (tpu_io_tpu2cpu_freezeTag) begin
              if (!(tpu_io_tpu2cpu_freezeTag)) begin
                vec_pregs_1_EL <= vec_pregs_0_EL;
              end
            end
          end else begin
            if (_T_364) begin
              if (tpu_io_tpu2cpu_freezeTag) begin
                vec_pregs_1_EL <= _vec_pregs_tpu_io_tpu2cpu_freezeTag_EL_0;
              end else begin
                if (tpu_io_tpu2cpu_freezeTag) begin
                  if (!(tpu_io_tpu2cpu_freezeTag)) begin
                    vec_pregs_1_EL <= vec_pregs_0_EL;
                  end
                end
              end
            end else begin
              if (tpu_io_tpu2cpu_freezeTag) begin
                if (!(tpu_io_tpu2cpu_freezeTag)) begin
                  vec_pregs_1_EL <= vec_pregs_0_EL;
                end
              end
            end
          end
        end else begin
          if (tpu_io_tpu2cpu_freezeTag) begin
            if (!(tpu_io_tpu2cpu_freezeTag)) begin
              vec_pregs_1_EL <= vec_pregs_0_EL;
            end
          end
        end
      end
    end
    if (reset) begin
      vec_pregs_1_NZCV <= 4'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_360) begin
            if (tpu_io_tpu2cpu_freezeTag) begin
              if (!(tpu_io_tpu2cpu_freezeTag)) begin
                vec_pregs_1_NZCV <= vec_pregs_0_NZCV;
              end
            end else begin
              if (_T_266) begin
                if (exeReg_io_deq_bits_tag) begin
                  vec_pregs_1_NZCV <= _vec_pregs_exeReg_io_deq_bits_tag_NZCV;
                end
              end
            end
          end else begin
            if (_T_364) begin
              if (tpu_io_tpu2cpu_freezeTag) begin
                vec_pregs_1_NZCV <= _vec_pregs_tpu_io_tpu2cpu_freezeTag_NZCV_0;
              end else begin
                if (tpu_io_tpu2cpu_freezeTag) begin
                  if (!(tpu_io_tpu2cpu_freezeTag)) begin
                    vec_pregs_1_NZCV <= vec_pregs_0_NZCV;
                  end
                end else begin
                  if (_T_266) begin
                    if (exeReg_io_deq_bits_tag) begin
                      vec_pregs_1_NZCV <= _vec_pregs_exeReg_io_deq_bits_tag_NZCV;
                    end
                  end
                end
              end
            end else begin
              if (tpu_io_tpu2cpu_freezeTag) begin
                if (!(tpu_io_tpu2cpu_freezeTag)) begin
                  vec_pregs_1_NZCV <= vec_pregs_0_NZCV;
                end
              end else begin
                if (_T_266) begin
                  if (exeReg_io_deq_bits_tag) begin
                    vec_pregs_1_NZCV <= _vec_pregs_exeReg_io_deq_bits_tag_NZCV;
                  end
                end
              end
            end
          end
        end else begin
          if (tpu_io_tpu2cpu_freezeTag) begin
            if (!(tpu_io_tpu2cpu_freezeTag)) begin
              vec_pregs_1_NZCV <= vec_pregs_0_NZCV;
            end
          end else begin
            if (_T_266) begin
              if (exeReg_io_deq_bits_tag) begin
                vec_pregs_1_NZCV <= _vec_pregs_exeReg_io_deq_bits_tag_NZCV;
              end
            end
          end
        end
      end else begin
        vec_pregs_1_NZCV <= _GEN_42;
      end
    end
    if (reset) begin
      fake_PC <= 65'h0;
    end else begin
      if (tpu_io_tpu2cpu_fire) begin
        fake_PC <= {{1'd0}, _GEN_87};
      end else begin
        if (brReg_io_deq_valid) begin
          fake_PC <= _T_279;
        end else begin
          if (fetch_io_incr) begin
            fake_PC <= _T_187;
          end
        end
      end
    end
    if (reset) begin
      fetch_en_0 <= 1'h0;
    end else begin
      if (_T_318) begin
        if (1'h0 == tpu_io_tpu2cpu_flushTag) begin
          fetch_en_0 <= 1'h0;
        end else begin
          if (tpu_io_tpu2cpu_fire) begin
            if (1'h0 == tpu_io_tpu2cpu_fireTag) begin
              fetch_en_0 <= 1'h1;
            end
          end
        end
      end else begin
        if (tpu_io_tpu2cpu_fire) begin
          if (1'h0 == tpu_io_tpu2cpu_fireTag) begin
            fetch_en_0 <= 1'h1;
          end
        end
      end
    end
    if (reset) begin
      fetch_en_1 <= 1'h0;
    end else begin
      if (_T_318) begin
        if (tpu_io_tpu2cpu_flushTag) begin
          fetch_en_1 <= 1'h0;
        end else begin
          if (tpu_io_tpu2cpu_fire) begin
            if (tpu_io_tpu2cpu_fireTag) begin
              fetch_en_1 <= 1'h1;
            end
          end
        end
      end else begin
        if (tpu_io_tpu2cpu_fire) begin
          if (tpu_io_tpu2cpu_fireTag) begin
            fetch_en_1 <= 1'h1;
          end
        end
      end
    end
  end
endmodule
