module Proc( // @[:@3179.2]
  input         clock, // @[:@3180.4]
  input         reset, // @[:@3181.4]
  input         io_ppageBRAM_WE, // @[:@3182.4]
  input         io_ppageBRAM_EN, // @[:@3182.4]
  input  [9:0]  io_ppageBRAM_ADDR, // @[:@3182.4]
  input  [35:0] io_ppageBRAM_DI, // @[:@3182.4]
  output [35:0] io_ppageBRAM_DO, // @[:@3182.4]
  input         io_stateBRAM_WE, // @[:@3182.4]
  input         io_stateBRAM_EN, // @[:@3182.4]
  input  [9:0]  io_stateBRAM_ADDR, // @[:@3182.4]
  input  [35:0] io_stateBRAM_DI, // @[:@3182.4]
  output [35:0] io_stateBRAM_DO, // @[:@3182.4]
  input         io_host2tpu_fire_tag, // @[:@3182.4]
  input         io_host2tpu_fire_valid, // @[:@3182.4]
  output        io_host2tpu_done_tag, // @[:@3182.4]
  output        io_host2tpu_done_valid, // @[:@3182.4]
  input         io_host2tpu_getState_tag, // @[:@3182.4]
  input         io_host2tpu_getState_valid // @[:@3182.4]
);
  wire  ppage_clock; // @[proc.scala 83:21:@3184.4]
  wire  ppage_io_portA_writeEn; // @[proc.scala 83:21:@3184.4]
  wire  ppage_io_portA_en; // @[proc.scala 83:21:@3184.4]
  wire [9:0] ppage_io_portA_addr; // @[proc.scala 83:21:@3184.4]
  wire [35:0] ppage_io_portA_dataIn; // @[proc.scala 83:21:@3184.4]
  wire [35:0] ppage_io_portA_dataOut; // @[proc.scala 83:21:@3184.4]
  wire  ppage_io_portB_writeEn; // @[proc.scala 83:21:@3184.4]
  wire [9:0] ppage_io_portB_addr; // @[proc.scala 83:21:@3184.4]
  wire [35:0] ppage_io_portB_dataIn; // @[proc.scala 83:21:@3184.4]
  wire [35:0] ppage_io_portB_dataOut; // @[proc.scala 83:21:@3184.4]
  wire  state_clock; // @[proc.scala 85:21:@3187.4]
  wire  state_io_portA_writeEn; // @[proc.scala 85:21:@3187.4]
  wire  state_io_portA_en; // @[proc.scala 85:21:@3187.4]
  wire [9:0] state_io_portA_addr; // @[proc.scala 85:21:@3187.4]
  wire [35:0] state_io_portA_dataIn; // @[proc.scala 85:21:@3187.4]
  wire [35:0] state_io_portA_dataOut; // @[proc.scala 85:21:@3187.4]
  wire  state_io_portB_writeEn; // @[proc.scala 85:21:@3187.4]
  wire [9:0] state_io_portB_addr; // @[proc.scala 85:21:@3187.4]
  wire [35:0] state_io_portB_dataIn; // @[proc.scala 85:21:@3187.4]
  wire [35:0] state_io_portB_dataOut; // @[proc.scala 85:21:@3187.4]
  wire  tpu_clock; // @[proc.scala 87:19:@3190.4]
  wire  tpu_reset; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_host2tpu_fire_tag; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_host2tpu_fire_valid; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_host2tpu_done_tag; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_host2tpu_done_valid; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_host2tpu_getState_tag; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_host2tpu_getState_valid; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_tpu2cpu_flush_tag; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_tpu2cpu_flush_valid; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_tpu2cpu_fire_tag; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_tpu2cpu_fire_valid; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_tpu2cpu_freeze_tag; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_tpu2cpu_freeze_valid; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_tpu2cpu_done_tag; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_tpu2cpu_done_valid; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_tpu2cpuStateReg_valid; // @[proc.scala 87:19:@3190.4]
  wire [1:0] tpu_io_tpu2cpuStateReg_bits; // @[proc.scala 87:19:@3190.4]
  wire [63:0] tpu_io_tpu2cpuState_PC; // @[proc.scala 87:19:@3190.4]
  wire [31:0] tpu_io_tpu2cpuState_SP; // @[proc.scala 87:19:@3190.4]
  wire [31:0] tpu_io_tpu2cpuState_EL; // @[proc.scala 87:19:@3190.4]
  wire [3:0] tpu_io_tpu2cpuState_NZCV; // @[proc.scala 87:19:@3190.4]
  wire [63:0] tpu_io_cpu2tpuState_PC; // @[proc.scala 87:19:@3190.4]
  wire [31:0] tpu_io_cpu2tpuState_SP; // @[proc.scala 87:19:@3190.4]
  wire [31:0] tpu_io_cpu2tpuState_EL; // @[proc.scala 87:19:@3190.4]
  wire [3:0] tpu_io_cpu2tpuState_NZCV; // @[proc.scala 87:19:@3190.4]
  wire [4:0] tpu_io_rfile_rs1_addr; // @[proc.scala 87:19:@3190.4]
  wire [63:0] tpu_io_rfile_rs1_data; // @[proc.scala 87:19:@3190.4]
  wire [4:0] tpu_io_rfile_waddr; // @[proc.scala 87:19:@3190.4]
  wire [63:0] tpu_io_rfile_wdata; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_rfile_wen; // @[proc.scala 87:19:@3190.4]
  wire  tpu_io_stateBRAM_writeEn; // @[proc.scala 87:19:@3190.4]
  wire [9:0] tpu_io_stateBRAM_addr; // @[proc.scala 87:19:@3190.4]
  wire [35:0] tpu_io_stateBRAM_dataIn; // @[proc.scala 87:19:@3190.4]
  wire [35:0] tpu_io_stateBRAM_dataOut; // @[proc.scala 87:19:@3190.4]
  wire  RFile_clock; // @[proc.scala 91:57:@3193.4]
  wire [4:0] RFile_io_rs1_addr; // @[proc.scala 91:57:@3193.4]
  wire [63:0] RFile_io_rs1_data; // @[proc.scala 91:57:@3193.4]
  wire [4:0] RFile_io_rs2_addr; // @[proc.scala 91:57:@3193.4]
  wire [63:0] RFile_io_rs2_data; // @[proc.scala 91:57:@3193.4]
  wire [4:0] RFile_io_waddr; // @[proc.scala 91:57:@3193.4]
  wire [63:0] RFile_io_wdata; // @[proc.scala 91:57:@3193.4]
  wire  RFile_io_wen; // @[proc.scala 91:57:@3193.4]
  wire  RFile_1_clock; // @[proc.scala 91:57:@3196.4]
  wire [4:0] RFile_1_io_rs1_addr; // @[proc.scala 91:57:@3196.4]
  wire [63:0] RFile_1_io_rs1_data; // @[proc.scala 91:57:@3196.4]
  wire [4:0] RFile_1_io_rs2_addr; // @[proc.scala 91:57:@3196.4]
  wire [63:0] RFile_1_io_rs2_data; // @[proc.scala 91:57:@3196.4]
  wire [4:0] RFile_1_io_waddr; // @[proc.scala 91:57:@3196.4]
  wire [63:0] RFile_1_io_wdata; // @[proc.scala 91:57:@3196.4]
  wire  RFile_1_io_wen; // @[proc.scala 91:57:@3196.4]
  wire [63:0] insnTLB_io_vaddr; // @[proc.scala 95:23:@3234.4]
  wire [63:0] insnTLB_io_paddr; // @[proc.scala 95:23:@3234.4]
  wire  fetch_clock; // @[proc.scala 99:21:@3237.4]
  wire  fetch_reset; // @[proc.scala 99:21:@3237.4]
  wire  fetch_io_flush_tag; // @[proc.scala 99:21:@3237.4]
  wire  fetch_io_flush_valid; // @[proc.scala 99:21:@3237.4]
  wire  fetch_io_fire_tag; // @[proc.scala 99:21:@3237.4]
  wire  fetch_io_fire_valid; // @[proc.scala 99:21:@3237.4]
  wire  fetch_io_commitReg_valid; // @[proc.scala 99:21:@3237.4]
  wire  fetch_io_commitReg_bits_br_valid; // @[proc.scala 99:21:@3237.4]
  wire  fetch_io_commitReg_bits_tag; // @[proc.scala 99:21:@3237.4]
  wire [63:0] fetch_io_nextPC; // @[proc.scala 99:21:@3237.4]
  wire  fetch_io_fetchEn_0; // @[proc.scala 99:21:@3237.4]
  wire  fetch_io_fetchEn_1; // @[proc.scala 99:21:@3237.4]
  wire [63:0] fetch_io_pcVec_0; // @[proc.scala 99:21:@3237.4]
  wire [63:0] fetch_io_pcVec_1; // @[proc.scala 99:21:@3237.4]
  wire [63:0] fetch_io_pc_data; // @[proc.scala 99:21:@3237.4]
  wire  fetch_io_pc_valid; // @[proc.scala 99:21:@3237.4]
  wire [31:0] fetch_io_insn; // @[proc.scala 99:21:@3237.4]
  wire  fetch_io_deq_ready; // @[proc.scala 99:21:@3237.4]
  wire  fetch_io_deq_valid; // @[proc.scala 99:21:@3237.4]
  wire [31:0] fetch_io_deq_bits_inst; // @[proc.scala 99:21:@3237.4]
  wire  fetch_io_deq_bits_tag; // @[proc.scala 99:21:@3237.4]
  wire [63:0] fetch_io_deq_bits_pc; // @[proc.scala 99:21:@3237.4]
  wire [31:0] decoder_io_finst_inst; // @[proc.scala 102:23:@3240.4]
  wire  decoder_io_finst_tag; // @[proc.scala 102:23:@3240.4]
  wire [63:0] decoder_io_finst_pc; // @[proc.scala 102:23:@3240.4]
  wire  decoder_io_dinst_rd_valid; // @[proc.scala 102:23:@3240.4]
  wire [4:0] decoder_io_dinst_rd_bits; // @[proc.scala 102:23:@3240.4]
  wire [4:0] decoder_io_dinst_rs1_bits; // @[proc.scala 102:23:@3240.4]
  wire  decoder_io_dinst_rs2_valid; // @[proc.scala 102:23:@3240.4]
  wire [4:0] decoder_io_dinst_rs2_bits; // @[proc.scala 102:23:@3240.4]
  wire [25:0] decoder_io_dinst_imm_bits; // @[proc.scala 102:23:@3240.4]
  wire  decoder_io_dinst_shift_val_valid; // @[proc.scala 102:23:@3240.4]
  wire [5:0] decoder_io_dinst_shift_val_bits; // @[proc.scala 102:23:@3240.4]
  wire [1:0] decoder_io_dinst_shift_type; // @[proc.scala 102:23:@3240.4]
  wire [3:0] decoder_io_dinst_cond_bits; // @[proc.scala 102:23:@3240.4]
  wire [2:0] decoder_io_dinst_itype; // @[proc.scala 102:23:@3240.4]
  wire [2:0] decoder_io_dinst_op; // @[proc.scala 102:23:@3240.4]
  wire  decoder_io_dinst_nzcv_en; // @[proc.scala 102:23:@3240.4]
  wire  decoder_io_dinst_tag; // @[proc.scala 102:23:@3240.4]
  wire [63:0] decoder_io_dinst_pc; // @[proc.scala 102:23:@3240.4]
  wire  decReg_clock; // @[proc.scala 103:22:@3243.4]
  wire  decReg_reset; // @[proc.scala 103:22:@3243.4]
  wire  decReg_io_enq_ready; // @[proc.scala 103:22:@3243.4]
  wire  decReg_io_enq_valid; // @[proc.scala 103:22:@3243.4]
  wire  decReg_io_enq_bits_rd_valid; // @[proc.scala 103:22:@3243.4]
  wire [4:0] decReg_io_enq_bits_rd_bits; // @[proc.scala 103:22:@3243.4]
  wire [4:0] decReg_io_enq_bits_rs1_bits; // @[proc.scala 103:22:@3243.4]
  wire  decReg_io_enq_bits_rs2_valid; // @[proc.scala 103:22:@3243.4]
  wire [4:0] decReg_io_enq_bits_rs2_bits; // @[proc.scala 103:22:@3243.4]
  wire [25:0] decReg_io_enq_bits_imm_bits; // @[proc.scala 103:22:@3243.4]
  wire  decReg_io_enq_bits_shift_val_valid; // @[proc.scala 103:22:@3243.4]
  wire [5:0] decReg_io_enq_bits_shift_val_bits; // @[proc.scala 103:22:@3243.4]
  wire [1:0] decReg_io_enq_bits_shift_type; // @[proc.scala 103:22:@3243.4]
  wire [3:0] decReg_io_enq_bits_cond_bits; // @[proc.scala 103:22:@3243.4]
  wire [2:0] decReg_io_enq_bits_itype; // @[proc.scala 103:22:@3243.4]
  wire [2:0] decReg_io_enq_bits_op; // @[proc.scala 103:22:@3243.4]
  wire  decReg_io_enq_bits_nzcv_en; // @[proc.scala 103:22:@3243.4]
  wire  decReg_io_enq_bits_tag; // @[proc.scala 103:22:@3243.4]
  wire [63:0] decReg_io_enq_bits_pc; // @[proc.scala 103:22:@3243.4]
  wire  decReg_io_deq_ready; // @[proc.scala 103:22:@3243.4]
  wire  decReg_io_deq_valid; // @[proc.scala 103:22:@3243.4]
  wire  decReg_io_deq_bits_rd_valid; // @[proc.scala 103:22:@3243.4]
  wire [4:0] decReg_io_deq_bits_rd_bits; // @[proc.scala 103:22:@3243.4]
  wire [4:0] decReg_io_deq_bits_rs1_bits; // @[proc.scala 103:22:@3243.4]
  wire  decReg_io_deq_bits_rs2_valid; // @[proc.scala 103:22:@3243.4]
  wire [4:0] decReg_io_deq_bits_rs2_bits; // @[proc.scala 103:22:@3243.4]
  wire [25:0] decReg_io_deq_bits_imm_bits; // @[proc.scala 103:22:@3243.4]
  wire  decReg_io_deq_bits_shift_val_valid; // @[proc.scala 103:22:@3243.4]
  wire [5:0] decReg_io_deq_bits_shift_val_bits; // @[proc.scala 103:22:@3243.4]
  wire [1:0] decReg_io_deq_bits_shift_type; // @[proc.scala 103:22:@3243.4]
  wire [3:0] decReg_io_deq_bits_cond_bits; // @[proc.scala 103:22:@3243.4]
  wire [2:0] decReg_io_deq_bits_itype; // @[proc.scala 103:22:@3243.4]
  wire [2:0] decReg_io_deq_bits_op; // @[proc.scala 103:22:@3243.4]
  wire  decReg_io_deq_bits_nzcv_en; // @[proc.scala 103:22:@3243.4]
  wire  decReg_io_deq_bits_tag; // @[proc.scala 103:22:@3243.4]
  wire [63:0] decReg_io_deq_bits_pc; // @[proc.scala 103:22:@3243.4]
  wire  decReg_io_flush; // @[proc.scala 103:22:@3243.4]
  wire  issuer_clock; // @[proc.scala 105:22:@3246.4]
  wire  issuer_reset; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_flush_tag; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_flush_valid; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_enq_ready; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_enq_valid; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_enq_bits_rd_valid; // @[proc.scala 105:22:@3246.4]
  wire [4:0] issuer_io_enq_bits_rd_bits; // @[proc.scala 105:22:@3246.4]
  wire [4:0] issuer_io_enq_bits_rs1_bits; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_enq_bits_rs2_valid; // @[proc.scala 105:22:@3246.4]
  wire [4:0] issuer_io_enq_bits_rs2_bits; // @[proc.scala 105:22:@3246.4]
  wire [25:0] issuer_io_enq_bits_imm_bits; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_enq_bits_shift_val_valid; // @[proc.scala 105:22:@3246.4]
  wire [5:0] issuer_io_enq_bits_shift_val_bits; // @[proc.scala 105:22:@3246.4]
  wire [1:0] issuer_io_enq_bits_shift_type; // @[proc.scala 105:22:@3246.4]
  wire [3:0] issuer_io_enq_bits_cond_bits; // @[proc.scala 105:22:@3246.4]
  wire [2:0] issuer_io_enq_bits_itype; // @[proc.scala 105:22:@3246.4]
  wire [2:0] issuer_io_enq_bits_op; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_enq_bits_nzcv_en; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_enq_bits_tag; // @[proc.scala 105:22:@3246.4]
  wire [63:0] issuer_io_enq_bits_pc; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_deq_valid; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_deq_bits_rd_valid; // @[proc.scala 105:22:@3246.4]
  wire [4:0] issuer_io_deq_bits_rd_bits; // @[proc.scala 105:22:@3246.4]
  wire [4:0] issuer_io_deq_bits_rs1_bits; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_deq_bits_rs2_valid; // @[proc.scala 105:22:@3246.4]
  wire [4:0] issuer_io_deq_bits_rs2_bits; // @[proc.scala 105:22:@3246.4]
  wire [25:0] issuer_io_deq_bits_imm_bits; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_deq_bits_shift_val_valid; // @[proc.scala 105:22:@3246.4]
  wire [5:0] issuer_io_deq_bits_shift_val_bits; // @[proc.scala 105:22:@3246.4]
  wire [1:0] issuer_io_deq_bits_shift_type; // @[proc.scala 105:22:@3246.4]
  wire [3:0] issuer_io_deq_bits_cond_bits; // @[proc.scala 105:22:@3246.4]
  wire [2:0] issuer_io_deq_bits_itype; // @[proc.scala 105:22:@3246.4]
  wire [2:0] issuer_io_deq_bits_op; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_deq_bits_nzcv_en; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_deq_bits_tag; // @[proc.scala 105:22:@3246.4]
  wire [63:0] issuer_io_deq_bits_pc; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_commitReg_valid; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_commitReg_bits_exe_bits_rd_valid; // @[proc.scala 105:22:@3246.4]
  wire [4:0] issuer_io_commitReg_bits_exe_bits_rd_bits; // @[proc.scala 105:22:@3246.4]
  wire  issuer_io_commitReg_bits_tag; // @[proc.scala 105:22:@3246.4]
  wire  executer_io_dinst_rd_valid; // @[proc.scala 113:24:@3249.4]
  wire [4:0] executer_io_dinst_rd_bits; // @[proc.scala 113:24:@3249.4]
  wire  executer_io_dinst_rs2_valid; // @[proc.scala 113:24:@3249.4]
  wire [25:0] executer_io_dinst_imm_bits; // @[proc.scala 113:24:@3249.4]
  wire  executer_io_dinst_shift_val_valid; // @[proc.scala 113:24:@3249.4]
  wire [5:0] executer_io_dinst_shift_val_bits; // @[proc.scala 113:24:@3249.4]
  wire [1:0] executer_io_dinst_shift_type; // @[proc.scala 113:24:@3249.4]
  wire [2:0] executer_io_dinst_itype; // @[proc.scala 113:24:@3249.4]
  wire [2:0] executer_io_dinst_op; // @[proc.scala 113:24:@3249.4]
  wire  executer_io_dinst_nzcv_en; // @[proc.scala 113:24:@3249.4]
  wire [63:0] executer_io_rVal1; // @[proc.scala 113:24:@3249.4]
  wire [63:0] executer_io_rVal2; // @[proc.scala 113:24:@3249.4]
  wire  executer_io_einst_valid; // @[proc.scala 113:24:@3249.4]
  wire  executer_io_einst_bits_rd_valid; // @[proc.scala 113:24:@3249.4]
  wire [4:0] executer_io_einst_bits_rd_bits; // @[proc.scala 113:24:@3249.4]
  wire  executer_io_einst_bits_nzcv_valid; // @[proc.scala 113:24:@3249.4]
  wire [3:0] executer_io_einst_bits_nzcv_bits; // @[proc.scala 113:24:@3249.4]
  wire [63:0] executer_io_einst_bits_res; // @[proc.scala 113:24:@3249.4]
  wire [25:0] brancher_io_dinst_imm_bits; // @[proc.scala 114:24:@3252.4]
  wire [3:0] brancher_io_dinst_cond_bits; // @[proc.scala 114:24:@3252.4]
  wire [2:0] brancher_io_dinst_itype; // @[proc.scala 114:24:@3252.4]
  wire [2:0] brancher_io_dinst_op; // @[proc.scala 114:24:@3252.4]
  wire [3:0] brancher_io_nzcv; // @[proc.scala 114:24:@3252.4]
  wire  brancher_io_binst_valid; // @[proc.scala 114:24:@3252.4]
  wire [63:0] brancher_io_binst_bits_offset; // @[proc.scala 114:24:@3252.4]
  wire  ldstU_clock; // @[proc.scala 115:21:@3255.4]
  wire  ldstU_reset; // @[proc.scala 115:21:@3255.4]
  wire  ldstU_io_dinst_valid; // @[proc.scala 115:21:@3255.4]
  wire [25:0] ldstU_io_dinst_bits_imm_bits; // @[proc.scala 115:21:@3255.4]
  wire [2:0] ldstU_io_dinst_bits_itype; // @[proc.scala 115:21:@3255.4]
  wire [63:0] ldstU_io_dinst_bits_pc; // @[proc.scala 115:21:@3255.4]
  wire  commitReg_clock; // @[proc.scala 116:25:@3258.4]
  wire  commitReg_reset; // @[proc.scala 116:25:@3258.4]
  wire  commitReg_io_enq_valid; // @[proc.scala 116:25:@3258.4]
  wire  commitReg_io_enq_bits_exe_valid; // @[proc.scala 116:25:@3258.4]
  wire  commitReg_io_enq_bits_exe_bits_rd_valid; // @[proc.scala 116:25:@3258.4]
  wire [4:0] commitReg_io_enq_bits_exe_bits_rd_bits; // @[proc.scala 116:25:@3258.4]
  wire  commitReg_io_enq_bits_exe_bits_nzcv_valid; // @[proc.scala 116:25:@3258.4]
  wire [3:0] commitReg_io_enq_bits_exe_bits_nzcv_bits; // @[proc.scala 116:25:@3258.4]
  wire [63:0] commitReg_io_enq_bits_exe_bits_res; // @[proc.scala 116:25:@3258.4]
  wire  commitReg_io_enq_bits_br_valid; // @[proc.scala 116:25:@3258.4]
  wire [63:0] commitReg_io_enq_bits_br_bits_offset; // @[proc.scala 116:25:@3258.4]
  wire  commitReg_io_enq_bits_undef; // @[proc.scala 116:25:@3258.4]
  wire  commitReg_io_enq_bits_tag; // @[proc.scala 116:25:@3258.4]
  wire  commitReg_io_deq_valid; // @[proc.scala 116:25:@3258.4]
  wire  commitReg_io_deq_bits_exe_valid; // @[proc.scala 116:25:@3258.4]
  wire  commitReg_io_deq_bits_exe_bits_rd_valid; // @[proc.scala 116:25:@3258.4]
  wire [4:0] commitReg_io_deq_bits_exe_bits_rd_bits; // @[proc.scala 116:25:@3258.4]
  wire  commitReg_io_deq_bits_exe_bits_nzcv_valid; // @[proc.scala 116:25:@3258.4]
  wire [3:0] commitReg_io_deq_bits_exe_bits_nzcv_bits; // @[proc.scala 116:25:@3258.4]
  wire [63:0] commitReg_io_deq_bits_exe_bits_res; // @[proc.scala 116:25:@3258.4]
  wire  commitReg_io_deq_bits_br_valid; // @[proc.scala 116:25:@3258.4]
  wire [63:0] commitReg_io_deq_bits_br_bits_offset; // @[proc.scala 116:25:@3258.4]
  wire  commitReg_io_deq_bits_undef; // @[proc.scala 116:25:@3258.4]
  wire  commitReg_io_deq_bits_tag; // @[proc.scala 116:25:@3258.4]
  reg [63:0] pregsVec_0_PC; // @[proc.scala 92:25:@3233.4]
  reg [63:0] _RAND_0;
  reg [31:0] pregsVec_0_SP; // @[proc.scala 92:25:@3233.4]
  reg [31:0] _RAND_1;
  reg [31:0] pregsVec_0_EL; // @[proc.scala 92:25:@3233.4]
  reg [31:0] _RAND_2;
  reg [3:0] pregsVec_0_NZCV; // @[proc.scala 92:25:@3233.4]
  reg [31:0] _RAND_3;
  reg [63:0] pregsVec_1_PC; // @[proc.scala 92:25:@3233.4]
  reg [63:0] _RAND_4;
  reg [31:0] pregsVec_1_SP; // @[proc.scala 92:25:@3233.4]
  reg [31:0] _RAND_5;
  reg [31:0] pregsVec_1_EL; // @[proc.scala 92:25:@3233.4]
  reg [31:0] _RAND_6;
  reg [3:0] pregsVec_1_NZCV; // @[proc.scala 92:25:@3233.4]
  reg [31:0] _RAND_7;
  reg  fetchEn_0; // @[proc.scala 119:24:@3264.4]
  reg [31:0] _RAND_8;
  reg  fetchEn_1; // @[proc.scala 119:24:@3264.4]
  reg [31:0] _RAND_9;
  wire [63:0] _GEN_4; // @[proc.scala 129:39:@3267.6]
  wire [64:0] _T_246; // @[proc.scala 129:39:@3267.6]
  wire [63:0] _T_247; // @[proc.scala 129:67:@3268.6]
  wire [64:0] _GEN_167; // @[proc.scala 129:44:@3269.6]
  wire [65:0] _T_248; // @[proc.scala 129:44:@3269.6]
  wire [64:0] _T_249; // @[proc.scala 129:44:@3270.6]
  wire [64:0] _T_250; // @[proc.scala 129:44:@3271.6]
  wire [64:0] _T_251; // @[proc.scala 129:81:@3272.6]
  wire [64:0] _T_256; // @[proc.scala 131:38:@3276.6]
  wire [63:0] _T_257; // @[proc.scala 131:38:@3277.6]
  wire [64:0] _GEN_8; // @[proc.scala 128:24:@3266.4]
  wire [4:0] _rfileVec_tpu_io_tpu2cpu_freeze_tag_rs1_addr; // @[proc.scala 297:41:@3609.6 proc.scala 297:41:@3609.6]
  wire [4:0] _GEN_107; // @[proc.scala 297:41:@3609.6]
  wire [63:0] rfileVec_0_rs1_data; // @[proc.scala 91:25:@3199.4 proc.scala 91:25:@3205.4]
  wire [4:0] _GEN_105; // @[proc.scala 297:41:@3607.6]
  wire [63:0] rfileVec_0_rs2_data; // @[proc.scala 91:25:@3199.4 proc.scala 91:25:@3203.4]
  wire [4:0] _rfileVec_tpu_io_tpu2cpu_freeze_tag_waddr; // @[proc.scala 297:41:@3605.6 proc.scala 297:41:@3605.6]
  wire [4:0] _GEN_31; // @[proc.scala 236:33:@3508.6]
  wire [4:0] _GEN_33; // @[proc.scala 233:28:@3503.4]
  wire [4:0] _GEN_89; // @[proc.scala 297:41:@3605.6]
  wire [63:0] _rfileVec_tpu_io_tpu2cpu_freeze_tag_wdata; // @[proc.scala 297:41:@3604.6 proc.scala 297:41:@3604.6]
  wire [63:0] _GEN_32; // @[proc.scala 236:33:@3508.6]
  wire [63:0] _GEN_34; // @[proc.scala 233:28:@3503.4]
  wire [63:0] _GEN_87; // @[proc.scala 297:41:@3604.6]
  wire  _rfileVec_tpu_io_tpu2cpu_freeze_tag_wen; // @[proc.scala 297:41:@3603.6 proc.scala 297:41:@3603.6]
  wire  _T_319; // @[proc.scala 248:25:@3532.6]
  wire  _GEN_35; // @[proc.scala 247:29:@3535.6]
  wire  _GEN_47; // @[proc.scala 246:22:@3531.4]
  wire  _GEN_85; // @[proc.scala 297:41:@3603.6]
  wire [4:0] _GEN_108; // @[proc.scala 297:41:@3609.6]
  wire [63:0] rfileVec_1_rs1_data; // @[proc.scala 91:25:@3199.4 proc.scala 91:25:@3212.4]
  wire [4:0] _GEN_106; // @[proc.scala 297:41:@3607.6]
  wire [63:0] rfileVec_1_rs2_data; // @[proc.scala 91:25:@3199.4 proc.scala 91:25:@3210.4]
  wire [4:0] _GEN_90; // @[proc.scala 297:41:@3605.6]
  wire [63:0] _GEN_88; // @[proc.scala 297:41:@3604.6]
  wire  _GEN_36; // @[proc.scala 247:29:@3535.6]
  wire  _GEN_48; // @[proc.scala 246:22:@3531.4]
  wire  _GEN_86; // @[proc.scala 297:41:@3603.6]
  wire  _T_300; // @[proc.scala 222:53:@3498.4]
  wire  _T_322; // @[proc.scala 251:27:@3536.6]
  wire [3:0] _pregsVec_commitReg_io_deq_bits_tag_NZCV; // @[proc.scala 252:32:@3538.8 proc.scala 252:32:@3538.8]
  wire [3:0] _GEN_37; // @[proc.scala 252:32:@3538.8]
  wire [3:0] _GEN_38; // @[proc.scala 252:32:@3538.8]
  wire [3:0] _GEN_39; // @[proc.scala 251:58:@3537.6]
  wire [3:0] _GEN_40; // @[proc.scala 251:58:@3537.6]
  wire [63:0] _pregsVec_commitReg_io_deq_bits_tag_PC; // @[proc.scala 256:30:@3547.8 proc.scala 256:30:@3547.8]
  wire [63:0] _GEN_41; // @[proc.scala 256:30:@3547.8]
  wire [63:0] _GEN_42; // @[proc.scala 256:30:@3547.8]
  wire [63:0] _GEN_43; // @[proc.scala 258:30:@3552.8]
  wire [63:0] _GEN_44; // @[proc.scala 258:30:@3552.8]
  wire [63:0] _GEN_45; // @[proc.scala 255:26:@3540.6]
  wire [63:0] _GEN_46; // @[proc.scala 255:26:@3540.6]
  wire [3:0] _GEN_49; // @[proc.scala 246:22:@3531.4]
  wire [3:0] _GEN_50; // @[proc.scala 246:22:@3531.4]
  wire [63:0] _GEN_51; // @[proc.scala 246:22:@3531.4]
  wire [63:0] _GEN_52; // @[proc.scala 246:22:@3531.4]
  wire  _GEN_57; // @[proc.scala 264:73:@3559.6]
  wire  _GEN_58; // @[proc.scala 264:73:@3559.6]
  wire  _GEN_59; // @[proc.scala 264:38:@3558.4]
  wire  _GEN_60; // @[proc.scala 264:38:@3558.4]
  wire  _T_360; // @[proc.scala 266:29:@3565.4]
  wire  _GEN_65; // @[proc.scala 266:97:@3567.6]
  wire  _GEN_66; // @[proc.scala 266:97:@3567.6]
  wire  _GEN_67; // @[proc.scala 266:61:@3566.4]
  wire  _GEN_68; // @[proc.scala 266:61:@3566.4]
  wire  _T_367; // @[proc.scala 277:47:@3578.6]
  wire  _T_368; // @[proc.scala 278:26:@3582.6]
  wire  _T_369; // @[proc.scala 279:51:@3584.8]
  wire  _T_371; // @[proc.scala 283:47:@3589.8]
  wire  _GEN_69; // @[proc.scala 278:46:@3583.6]
  wire  _GEN_70; // @[proc.scala 278:46:@3583.6]
  wire  _GEN_72; // @[proc.scala 278:46:@3583.6]
  wire [63:0] _GEN_81; // @[proc.scala 291:23:@3596.4]
  wire [31:0] _GEN_82; // @[proc.scala 291:23:@3596.4]
  wire [31:0] _GEN_83; // @[proc.scala 291:23:@3596.4]
  wire [3:0] _GEN_84; // @[proc.scala 291:23:@3596.4]
  wire [63:0] _GEN_99; // @[proc.scala 297:41:@3606.6]
  wire [3:0] _GEN_109; // @[proc.scala 299:41:@3610.6]
  wire [3:0] _GEN_110; // @[proc.scala 299:41:@3610.6]
  wire [31:0] _GEN_111; // @[proc.scala 299:41:@3611.6]
  wire [31:0] _GEN_112; // @[proc.scala 299:41:@3611.6]
  wire [31:0] _GEN_113; // @[proc.scala 299:41:@3612.6]
  wire [31:0] _GEN_114; // @[proc.scala 299:41:@3612.6]
  wire [63:0] _GEN_115; // @[proc.scala 299:41:@3613.6]
  wire [63:0] _GEN_116; // @[proc.scala 299:41:@3613.6]
  wire  _T_398; // @[proc.scala 301:40:@3615.8]
  wire [63:0] _pregsVec_tpu_io_tpu2cpu_freeze_tag_PC_0; // @[proc.scala 302:48:@3617.10 proc.scala 302:48:@3617.10]
  wire [63:0] _GEN_117; // @[proc.scala 302:48:@3617.10]
  wire [63:0] _GEN_118; // @[proc.scala 302:48:@3617.10]
  wire  _T_402; // @[proc.scala 303:46:@3620.10]
  wire [31:0] _pregsVec_tpu_io_tpu2cpu_freeze_tag_SP_0; // @[proc.scala 304:48:@3622.12 proc.scala 304:48:@3622.12]
  wire [31:0] _GEN_119; // @[proc.scala 304:48:@3622.12]
  wire [31:0] _GEN_120; // @[proc.scala 304:48:@3622.12]
  wire [31:0] _pregsVec_tpu_io_tpu2cpu_freeze_tag_EL_0; // @[proc.scala 305:48:@3623.12 proc.scala 305:48:@3623.12]
  wire [31:0] _GEN_121; // @[proc.scala 305:48:@3623.12]
  wire [31:0] _GEN_122; // @[proc.scala 305:48:@3623.12]
  wire [3:0] _pregsVec_tpu_io_tpu2cpu_freeze_tag_NZCV_0; // @[proc.scala 306:50:@3624.12 proc.scala 306:50:@3624.12]
  wire [3:0] _GEN_123; // @[proc.scala 306:50:@3624.12]
  wire [3:0] _GEN_124; // @[proc.scala 306:50:@3624.12]
  wire [31:0] _GEN_125; // @[proc.scala 303:73:@3621.10]
  wire [31:0] _GEN_126; // @[proc.scala 303:73:@3621.10]
  wire [31:0] _GEN_127; // @[proc.scala 303:73:@3621.10]
  wire [31:0] _GEN_128; // @[proc.scala 303:73:@3621.10]
  wire [3:0] _GEN_129; // @[proc.scala 303:73:@3621.10]
  wire [3:0] _GEN_130; // @[proc.scala 303:73:@3621.10]
  wire [63:0] _GEN_131; // @[proc.scala 301:60:@3616.8]
  wire [63:0] _GEN_132; // @[proc.scala 301:60:@3616.8]
  wire [31:0] _GEN_133; // @[proc.scala 301:60:@3616.8]
  wire [31:0] _GEN_134; // @[proc.scala 301:60:@3616.8]
  wire [31:0] _GEN_135; // @[proc.scala 301:60:@3616.8]
  wire [31:0] _GEN_136; // @[proc.scala 301:60:@3616.8]
  wire [3:0] _GEN_137; // @[proc.scala 301:60:@3616.8]
  wire [3:0] _GEN_138; // @[proc.scala 301:60:@3616.8]
  wire [63:0] _GEN_139; // @[proc.scala 300:40:@3614.6]
  wire [63:0] _GEN_140; // @[proc.scala 300:40:@3614.6]
  wire [31:0] _GEN_141; // @[proc.scala 300:40:@3614.6]
  wire [31:0] _GEN_142; // @[proc.scala 300:40:@3614.6]
  wire [31:0] _GEN_143; // @[proc.scala 300:40:@3614.6]
  wire [31:0] _GEN_144; // @[proc.scala 300:40:@3614.6]
  wire [3:0] _GEN_145; // @[proc.scala 300:40:@3614.6]
  wire [3:0] _GEN_146; // @[proc.scala 300:40:@3614.6]
  wire [3:0] _GEN_159; // @[proc.scala 295:37:@3602.4]
  wire [3:0] _GEN_160; // @[proc.scala 295:37:@3602.4]
  wire [31:0] _GEN_161; // @[proc.scala 295:37:@3602.4]
  wire [31:0] _GEN_162; // @[proc.scala 295:37:@3602.4]
  wire [31:0] _GEN_163; // @[proc.scala 295:37:@3602.4]
  wire [31:0] _GEN_164; // @[proc.scala 295:37:@3602.4]
  wire [63:0] _GEN_165; // @[proc.scala 295:37:@3602.4]
  wire [63:0] _GEN_166; // @[proc.scala 295:37:@3602.4]
  BRAM ppage ( // @[proc.scala 83:21:@3184.4]
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
  BRAM state ( // @[proc.scala 85:21:@3187.4]
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
  TransplantUnit tpu ( // @[proc.scala 87:19:@3190.4]
    .clock(tpu_clock),
    .reset(tpu_reset),
    .io_host2tpu_fire_tag(tpu_io_host2tpu_fire_tag),
    .io_host2tpu_fire_valid(tpu_io_host2tpu_fire_valid),
    .io_host2tpu_done_tag(tpu_io_host2tpu_done_tag),
    .io_host2tpu_done_valid(tpu_io_host2tpu_done_valid),
    .io_host2tpu_getState_tag(tpu_io_host2tpu_getState_tag),
    .io_host2tpu_getState_valid(tpu_io_host2tpu_getState_valid),
    .io_tpu2cpu_flush_tag(tpu_io_tpu2cpu_flush_tag),
    .io_tpu2cpu_flush_valid(tpu_io_tpu2cpu_flush_valid),
    .io_tpu2cpu_fire_tag(tpu_io_tpu2cpu_fire_tag),
    .io_tpu2cpu_fire_valid(tpu_io_tpu2cpu_fire_valid),
    .io_tpu2cpu_freeze_tag(tpu_io_tpu2cpu_freeze_tag),
    .io_tpu2cpu_freeze_valid(tpu_io_tpu2cpu_freeze_valid),
    .io_tpu2cpu_done_tag(tpu_io_tpu2cpu_done_tag),
    .io_tpu2cpu_done_valid(tpu_io_tpu2cpu_done_valid),
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
  RFile RFile ( // @[proc.scala 91:57:@3193.4]
    .clock(RFile_clock),
    .io_rs1_addr(RFile_io_rs1_addr),
    .io_rs1_data(RFile_io_rs1_data),
    .io_rs2_addr(RFile_io_rs2_addr),
    .io_rs2_data(RFile_io_rs2_data),
    .io_waddr(RFile_io_waddr),
    .io_wdata(RFile_io_wdata),
    .io_wen(RFile_io_wen)
  );
  RFile RFile_1 ( // @[proc.scala 91:57:@3196.4]
    .clock(RFile_1_clock),
    .io_rs1_addr(RFile_1_io_rs1_addr),
    .io_rs1_data(RFile_1_io_rs1_data),
    .io_rs2_addr(RFile_1_io_rs2_addr),
    .io_rs2_data(RFile_1_io_rs2_data),
    .io_waddr(RFile_1_io_waddr),
    .io_wdata(RFile_1_io_wdata),
    .io_wen(RFile_1_io_wen)
  );
  TLBUnit insnTLB ( // @[proc.scala 95:23:@3234.4]
    .io_vaddr(insnTLB_io_vaddr),
    .io_paddr(insnTLB_io_paddr)
  );
  FetchUnit fetch ( // @[proc.scala 99:21:@3237.4]
    .clock(fetch_clock),
    .reset(fetch_reset),
    .io_flush_tag(fetch_io_flush_tag),
    .io_flush_valid(fetch_io_flush_valid),
    .io_fire_tag(fetch_io_fire_tag),
    .io_fire_valid(fetch_io_fire_valid),
    .io_commitReg_valid(fetch_io_commitReg_valid),
    .io_commitReg_bits_br_valid(fetch_io_commitReg_bits_br_valid),
    .io_commitReg_bits_tag(fetch_io_commitReg_bits_tag),
    .io_nextPC(fetch_io_nextPC),
    .io_fetchEn_0(fetch_io_fetchEn_0),
    .io_fetchEn_1(fetch_io_fetchEn_1),
    .io_pcVec_0(fetch_io_pcVec_0),
    .io_pcVec_1(fetch_io_pcVec_1),
    .io_pc_data(fetch_io_pc_data),
    .io_pc_valid(fetch_io_pc_valid),
    .io_insn(fetch_io_insn),
    .io_deq_ready(fetch_io_deq_ready),
    .io_deq_valid(fetch_io_deq_valid),
    .io_deq_bits_inst(fetch_io_deq_bits_inst),
    .io_deq_bits_tag(fetch_io_deq_bits_tag),
    .io_deq_bits_pc(fetch_io_deq_bits_pc)
  );
  DecodeUnit decoder ( // @[proc.scala 102:23:@3240.4]
    .io_finst_inst(decoder_io_finst_inst),
    .io_finst_tag(decoder_io_finst_tag),
    .io_finst_pc(decoder_io_finst_pc),
    .io_dinst_rd_valid(decoder_io_dinst_rd_valid),
    .io_dinst_rd_bits(decoder_io_dinst_rd_bits),
    .io_dinst_rs1_bits(decoder_io_dinst_rs1_bits),
    .io_dinst_rs2_valid(decoder_io_dinst_rs2_valid),
    .io_dinst_rs2_bits(decoder_io_dinst_rs2_bits),
    .io_dinst_imm_bits(decoder_io_dinst_imm_bits),
    .io_dinst_shift_val_valid(decoder_io_dinst_shift_val_valid),
    .io_dinst_shift_val_bits(decoder_io_dinst_shift_val_bits),
    .io_dinst_shift_type(decoder_io_dinst_shift_type),
    .io_dinst_cond_bits(decoder_io_dinst_cond_bits),
    .io_dinst_itype(decoder_io_dinst_itype),
    .io_dinst_op(decoder_io_dinst_op),
    .io_dinst_nzcv_en(decoder_io_dinst_nzcv_en),
    .io_dinst_tag(decoder_io_dinst_tag),
    .io_dinst_pc(decoder_io_dinst_pc)
  );
  FlushReg_1 decReg ( // @[proc.scala 103:22:@3243.4]
    .clock(decReg_clock),
    .reset(decReg_reset),
    .io_enq_ready(decReg_io_enq_ready),
    .io_enq_valid(decReg_io_enq_valid),
    .io_enq_bits_rd_valid(decReg_io_enq_bits_rd_valid),
    .io_enq_bits_rd_bits(decReg_io_enq_bits_rd_bits),
    .io_enq_bits_rs1_bits(decReg_io_enq_bits_rs1_bits),
    .io_enq_bits_rs2_valid(decReg_io_enq_bits_rs2_valid),
    .io_enq_bits_rs2_bits(decReg_io_enq_bits_rs2_bits),
    .io_enq_bits_imm_bits(decReg_io_enq_bits_imm_bits),
    .io_enq_bits_shift_val_valid(decReg_io_enq_bits_shift_val_valid),
    .io_enq_bits_shift_val_bits(decReg_io_enq_bits_shift_val_bits),
    .io_enq_bits_shift_type(decReg_io_enq_bits_shift_type),
    .io_enq_bits_cond_bits(decReg_io_enq_bits_cond_bits),
    .io_enq_bits_itype(decReg_io_enq_bits_itype),
    .io_enq_bits_op(decReg_io_enq_bits_op),
    .io_enq_bits_nzcv_en(decReg_io_enq_bits_nzcv_en),
    .io_enq_bits_tag(decReg_io_enq_bits_tag),
    .io_enq_bits_pc(decReg_io_enq_bits_pc),
    .io_deq_ready(decReg_io_deq_ready),
    .io_deq_valid(decReg_io_deq_valid),
    .io_deq_bits_rd_valid(decReg_io_deq_bits_rd_valid),
    .io_deq_bits_rd_bits(decReg_io_deq_bits_rd_bits),
    .io_deq_bits_rs1_bits(decReg_io_deq_bits_rs1_bits),
    .io_deq_bits_rs2_valid(decReg_io_deq_bits_rs2_valid),
    .io_deq_bits_rs2_bits(decReg_io_deq_bits_rs2_bits),
    .io_deq_bits_imm_bits(decReg_io_deq_bits_imm_bits),
    .io_deq_bits_shift_val_valid(decReg_io_deq_bits_shift_val_valid),
    .io_deq_bits_shift_val_bits(decReg_io_deq_bits_shift_val_bits),
    .io_deq_bits_shift_type(decReg_io_deq_bits_shift_type),
    .io_deq_bits_cond_bits(decReg_io_deq_bits_cond_bits),
    .io_deq_bits_itype(decReg_io_deq_bits_itype),
    .io_deq_bits_op(decReg_io_deq_bits_op),
    .io_deq_bits_nzcv_en(decReg_io_deq_bits_nzcv_en),
    .io_deq_bits_tag(decReg_io_deq_bits_tag),
    .io_deq_bits_pc(decReg_io_deq_bits_pc),
    .io_flush(decReg_io_flush)
  );
  IssueUnit issuer ( // @[proc.scala 105:22:@3246.4]
    .clock(issuer_clock),
    .reset(issuer_reset),
    .io_flush_tag(issuer_io_flush_tag),
    .io_flush_valid(issuer_io_flush_valid),
    .io_enq_ready(issuer_io_enq_ready),
    .io_enq_valid(issuer_io_enq_valid),
    .io_enq_bits_rd_valid(issuer_io_enq_bits_rd_valid),
    .io_enq_bits_rd_bits(issuer_io_enq_bits_rd_bits),
    .io_enq_bits_rs1_bits(issuer_io_enq_bits_rs1_bits),
    .io_enq_bits_rs2_valid(issuer_io_enq_bits_rs2_valid),
    .io_enq_bits_rs2_bits(issuer_io_enq_bits_rs2_bits),
    .io_enq_bits_imm_bits(issuer_io_enq_bits_imm_bits),
    .io_enq_bits_shift_val_valid(issuer_io_enq_bits_shift_val_valid),
    .io_enq_bits_shift_val_bits(issuer_io_enq_bits_shift_val_bits),
    .io_enq_bits_shift_type(issuer_io_enq_bits_shift_type),
    .io_enq_bits_cond_bits(issuer_io_enq_bits_cond_bits),
    .io_enq_bits_itype(issuer_io_enq_bits_itype),
    .io_enq_bits_op(issuer_io_enq_bits_op),
    .io_enq_bits_nzcv_en(issuer_io_enq_bits_nzcv_en),
    .io_enq_bits_tag(issuer_io_enq_bits_tag),
    .io_enq_bits_pc(issuer_io_enq_bits_pc),
    .io_deq_valid(issuer_io_deq_valid),
    .io_deq_bits_rd_valid(issuer_io_deq_bits_rd_valid),
    .io_deq_bits_rd_bits(issuer_io_deq_bits_rd_bits),
    .io_deq_bits_rs1_bits(issuer_io_deq_bits_rs1_bits),
    .io_deq_bits_rs2_valid(issuer_io_deq_bits_rs2_valid),
    .io_deq_bits_rs2_bits(issuer_io_deq_bits_rs2_bits),
    .io_deq_bits_imm_bits(issuer_io_deq_bits_imm_bits),
    .io_deq_bits_shift_val_valid(issuer_io_deq_bits_shift_val_valid),
    .io_deq_bits_shift_val_bits(issuer_io_deq_bits_shift_val_bits),
    .io_deq_bits_shift_type(issuer_io_deq_bits_shift_type),
    .io_deq_bits_cond_bits(issuer_io_deq_bits_cond_bits),
    .io_deq_bits_itype(issuer_io_deq_bits_itype),
    .io_deq_bits_op(issuer_io_deq_bits_op),
    .io_deq_bits_nzcv_en(issuer_io_deq_bits_nzcv_en),
    .io_deq_bits_tag(issuer_io_deq_bits_tag),
    .io_deq_bits_pc(issuer_io_deq_bits_pc),
    .io_commitReg_valid(issuer_io_commitReg_valid),
    .io_commitReg_bits_exe_bits_rd_valid(issuer_io_commitReg_bits_exe_bits_rd_valid),
    .io_commitReg_bits_exe_bits_rd_bits(issuer_io_commitReg_bits_exe_bits_rd_bits),
    .io_commitReg_bits_tag(issuer_io_commitReg_bits_tag)
  );
  ExecuteUnit executer ( // @[proc.scala 113:24:@3249.4]
    .io_dinst_rd_valid(executer_io_dinst_rd_valid),
    .io_dinst_rd_bits(executer_io_dinst_rd_bits),
    .io_dinst_rs2_valid(executer_io_dinst_rs2_valid),
    .io_dinst_imm_bits(executer_io_dinst_imm_bits),
    .io_dinst_shift_val_valid(executer_io_dinst_shift_val_valid),
    .io_dinst_shift_val_bits(executer_io_dinst_shift_val_bits),
    .io_dinst_shift_type(executer_io_dinst_shift_type),
    .io_dinst_itype(executer_io_dinst_itype),
    .io_dinst_op(executer_io_dinst_op),
    .io_dinst_nzcv_en(executer_io_dinst_nzcv_en),
    .io_rVal1(executer_io_rVal1),
    .io_rVal2(executer_io_rVal2),
    .io_einst_valid(executer_io_einst_valid),
    .io_einst_bits_rd_valid(executer_io_einst_bits_rd_valid),
    .io_einst_bits_rd_bits(executer_io_einst_bits_rd_bits),
    .io_einst_bits_nzcv_valid(executer_io_einst_bits_nzcv_valid),
    .io_einst_bits_nzcv_bits(executer_io_einst_bits_nzcv_bits),
    .io_einst_bits_res(executer_io_einst_bits_res)
  );
  BranchUnit brancher ( // @[proc.scala 114:24:@3252.4]
    .io_dinst_imm_bits(brancher_io_dinst_imm_bits),
    .io_dinst_cond_bits(brancher_io_dinst_cond_bits),
    .io_dinst_itype(brancher_io_dinst_itype),
    .io_dinst_op(brancher_io_dinst_op),
    .io_nzcv(brancher_io_nzcv),
    .io_binst_valid(brancher_io_binst_valid),
    .io_binst_bits_offset(brancher_io_binst_bits_offset)
  );
  LoadStoreUnit ldstU ( // @[proc.scala 115:21:@3255.4]
    .clock(ldstU_clock),
    .reset(ldstU_reset),
    .io_dinst_valid(ldstU_io_dinst_valid),
    .io_dinst_bits_imm_bits(ldstU_io_dinst_bits_imm_bits),
    .io_dinst_bits_itype(ldstU_io_dinst_bits_itype),
    .io_dinst_bits_pc(ldstU_io_dinst_bits_pc)
  );
  FlushReg_2 commitReg ( // @[proc.scala 116:25:@3258.4]
    .clock(commitReg_clock),
    .reset(commitReg_reset),
    .io_enq_valid(commitReg_io_enq_valid),
    .io_enq_bits_exe_valid(commitReg_io_enq_bits_exe_valid),
    .io_enq_bits_exe_bits_rd_valid(commitReg_io_enq_bits_exe_bits_rd_valid),
    .io_enq_bits_exe_bits_rd_bits(commitReg_io_enq_bits_exe_bits_rd_bits),
    .io_enq_bits_exe_bits_nzcv_valid(commitReg_io_enq_bits_exe_bits_nzcv_valid),
    .io_enq_bits_exe_bits_nzcv_bits(commitReg_io_enq_bits_exe_bits_nzcv_bits),
    .io_enq_bits_exe_bits_res(commitReg_io_enq_bits_exe_bits_res),
    .io_enq_bits_br_valid(commitReg_io_enq_bits_br_valid),
    .io_enq_bits_br_bits_offset(commitReg_io_enq_bits_br_bits_offset),
    .io_enq_bits_undef(commitReg_io_enq_bits_undef),
    .io_enq_bits_tag(commitReg_io_enq_bits_tag),
    .io_deq_valid(commitReg_io_deq_valid),
    .io_deq_bits_exe_valid(commitReg_io_deq_bits_exe_valid),
    .io_deq_bits_exe_bits_rd_valid(commitReg_io_deq_bits_exe_bits_rd_valid),
    .io_deq_bits_exe_bits_rd_bits(commitReg_io_deq_bits_exe_bits_rd_bits),
    .io_deq_bits_exe_bits_nzcv_valid(commitReg_io_deq_bits_exe_bits_nzcv_valid),
    .io_deq_bits_exe_bits_nzcv_bits(commitReg_io_deq_bits_exe_bits_nzcv_bits),
    .io_deq_bits_exe_bits_res(commitReg_io_deq_bits_exe_bits_res),
    .io_deq_bits_br_valid(commitReg_io_deq_bits_br_valid),
    .io_deq_bits_br_bits_offset(commitReg_io_deq_bits_br_bits_offset),
    .io_deq_bits_undef(commitReg_io_deq_bits_undef),
    .io_deq_bits_tag(commitReg_io_deq_bits_tag)
  );
  assign _GEN_4 = commitReg_io_deq_bits_tag ? pregsVec_1_PC : pregsVec_0_PC; // @[proc.scala 129:39:@3267.6]
  assign _T_246 = {1'b0,$signed(_GEN_4)}; // @[proc.scala 129:39:@3267.6]
  assign _T_247 = $signed(commitReg_io_deq_bits_br_bits_offset); // @[proc.scala 129:67:@3268.6]
  assign _GEN_167 = {{1{_T_247[63]}},_T_247}; // @[proc.scala 129:44:@3269.6]
  assign _T_248 = $signed(_T_246) + $signed(_GEN_167); // @[proc.scala 129:44:@3269.6]
  assign _T_249 = $signed(_T_246) + $signed(_GEN_167); // @[proc.scala 129:44:@3270.6]
  assign _T_250 = $signed(_T_249); // @[proc.scala 129:44:@3271.6]
  assign _T_251 = $unsigned(_T_250); // @[proc.scala 129:81:@3272.6]
  assign _T_256 = _GEN_4 + 64'h4; // @[proc.scala 131:38:@3276.6]
  assign _T_257 = _GEN_4 + 64'h4; // @[proc.scala 131:38:@3277.6]
  assign _GEN_8 = commitReg_io_deq_bits_br_valid ? _T_251 : {{1'd0}, _T_257}; // @[proc.scala 128:24:@3266.4]
  assign _rfileVec_tpu_io_tpu2cpu_freeze_tag_rs1_addr = tpu_io_rfile_rs1_addr; // @[proc.scala 297:41:@3609.6 proc.scala 297:41:@3609.6]
  assign _GEN_107 = 1'h0 == tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_rs1_addr : issuer_io_deq_bits_rs1_bits; // @[proc.scala 297:41:@3609.6]
  assign rfileVec_0_rs1_data = RFile_io_rs1_data; // @[proc.scala 91:25:@3199.4 proc.scala 91:25:@3205.4]
  assign _GEN_105 = 1'h0 == tpu_io_tpu2cpu_freeze_tag ? 5'h0 : issuer_io_deq_bits_rs2_bits; // @[proc.scala 297:41:@3607.6]
  assign rfileVec_0_rs2_data = RFile_io_rs2_data; // @[proc.scala 91:25:@3199.4 proc.scala 91:25:@3203.4]
  assign _rfileVec_tpu_io_tpu2cpu_freeze_tag_waddr = tpu_io_rfile_waddr; // @[proc.scala 297:41:@3605.6 proc.scala 297:41:@3605.6]
  assign _GEN_31 = commitReg_io_deq_bits_exe_bits_rd_bits; // @[proc.scala 236:33:@3508.6]
  assign _GEN_33 = commitReg_io_deq_bits_exe_valid ? commitReg_io_deq_bits_exe_bits_rd_bits : _GEN_31; // @[proc.scala 233:28:@3503.4]
  assign _GEN_89 = 1'h0 == tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_waddr : _GEN_33; // @[proc.scala 297:41:@3605.6]
  assign _rfileVec_tpu_io_tpu2cpu_freeze_tag_wdata = tpu_io_rfile_wdata; // @[proc.scala 297:41:@3604.6 proc.scala 297:41:@3604.6]
  assign _GEN_32 = commitReg_io_deq_bits_exe_bits_res; // @[proc.scala 236:33:@3508.6]
  assign _GEN_34 = commitReg_io_deq_bits_exe_valid ? commitReg_io_deq_bits_exe_bits_res : _GEN_32; // @[proc.scala 233:28:@3503.4]
  assign _GEN_87 = 1'h0 == tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_wdata : _GEN_34; // @[proc.scala 297:41:@3604.6]
  assign _rfileVec_tpu_io_tpu2cpu_freeze_tag_wen = tpu_io_rfile_wen; // @[proc.scala 297:41:@3603.6 proc.scala 297:41:@3603.6]
  assign _T_319 = commitReg_io_deq_bits_exe_valid & commitReg_io_deq_bits_exe_bits_rd_valid; // @[proc.scala 248:25:@3532.6]
  assign _GEN_35 = 1'h0 == commitReg_io_deq_bits_tag ? _T_319 : 1'h0; // @[proc.scala 247:29:@3535.6]
  assign _GEN_47 = commitReg_io_deq_valid ? _GEN_35 : 1'h0; // @[proc.scala 246:22:@3531.4]
  assign _GEN_85 = 1'h0 == tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_wen : _GEN_47; // @[proc.scala 297:41:@3603.6]
  assign _GEN_108 = tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_rs1_addr : issuer_io_deq_bits_rs1_bits; // @[proc.scala 297:41:@3609.6]
  assign rfileVec_1_rs1_data = RFile_1_io_rs1_data; // @[proc.scala 91:25:@3199.4 proc.scala 91:25:@3212.4]
  assign _GEN_106 = tpu_io_tpu2cpu_freeze_tag ? 5'h0 : issuer_io_deq_bits_rs2_bits; // @[proc.scala 297:41:@3607.6]
  assign rfileVec_1_rs2_data = RFile_1_io_rs2_data; // @[proc.scala 91:25:@3199.4 proc.scala 91:25:@3210.4]
  assign _GEN_90 = tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_waddr : _GEN_33; // @[proc.scala 297:41:@3605.6]
  assign _GEN_88 = tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_wdata : _GEN_34; // @[proc.scala 297:41:@3604.6]
  assign _GEN_36 = commitReg_io_deq_bits_tag ? _T_319 : 1'h0; // @[proc.scala 247:29:@3535.6]
  assign _GEN_48 = commitReg_io_deq_valid ? _GEN_36 : 1'h0; // @[proc.scala 246:22:@3531.4]
  assign _GEN_86 = tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_wen : _GEN_48; // @[proc.scala 297:41:@3603.6]
  assign _T_300 = issuer_io_deq_bits_itype == 3'h0; // @[proc.scala 222:53:@3498.4]
  assign _T_322 = commitReg_io_deq_bits_exe_valid & commitReg_io_deq_bits_exe_bits_nzcv_valid; // @[proc.scala 251:27:@3536.6]
  assign _pregsVec_commitReg_io_deq_bits_tag_NZCV = commitReg_io_deq_bits_exe_bits_nzcv_bits; // @[proc.scala 252:32:@3538.8 proc.scala 252:32:@3538.8]
  assign _GEN_37 = 1'h0 == commitReg_io_deq_bits_tag ? _pregsVec_commitReg_io_deq_bits_tag_NZCV : pregsVec_0_NZCV; // @[proc.scala 252:32:@3538.8]
  assign _GEN_38 = commitReg_io_deq_bits_tag ? _pregsVec_commitReg_io_deq_bits_tag_NZCV : pregsVec_1_NZCV; // @[proc.scala 252:32:@3538.8]
  assign _GEN_39 = _T_322 ? _GEN_37 : pregsVec_0_NZCV; // @[proc.scala 251:58:@3537.6]
  assign _GEN_40 = _T_322 ? _GEN_38 : pregsVec_1_NZCV; // @[proc.scala 251:58:@3537.6]
  assign _pregsVec_commitReg_io_deq_bits_tag_PC = _T_251[63:0]; // @[proc.scala 256:30:@3547.8 proc.scala 256:30:@3547.8]
  assign _GEN_41 = 1'h0 == commitReg_io_deq_bits_tag ? _pregsVec_commitReg_io_deq_bits_tag_PC : pregsVec_0_PC; // @[proc.scala 256:30:@3547.8]
  assign _GEN_42 = commitReg_io_deq_bits_tag ? _pregsVec_commitReg_io_deq_bits_tag_PC : pregsVec_1_PC; // @[proc.scala 256:30:@3547.8]
  assign _GEN_43 = 1'h0 == commitReg_io_deq_bits_tag ? _T_257 : pregsVec_0_PC; // @[proc.scala 258:30:@3552.8]
  assign _GEN_44 = commitReg_io_deq_bits_tag ? _T_257 : pregsVec_1_PC; // @[proc.scala 258:30:@3552.8]
  assign _GEN_45 = commitReg_io_deq_bits_br_valid ? _GEN_41 : _GEN_43; // @[proc.scala 255:26:@3540.6]
  assign _GEN_46 = commitReg_io_deq_bits_br_valid ? _GEN_42 : _GEN_44; // @[proc.scala 255:26:@3540.6]
  assign _GEN_49 = commitReg_io_deq_valid ? _GEN_39 : pregsVec_0_NZCV; // @[proc.scala 246:22:@3531.4]
  assign _GEN_50 = commitReg_io_deq_valid ? _GEN_40 : pregsVec_1_NZCV; // @[proc.scala 246:22:@3531.4]
  assign _GEN_51 = commitReg_io_deq_valid ? _GEN_45 : pregsVec_0_PC; // @[proc.scala 246:22:@3531.4]
  assign _GEN_52 = commitReg_io_deq_valid ? _GEN_46 : pregsVec_1_PC; // @[proc.scala 246:22:@3531.4]
  assign _GEN_57 = 1'h0 == tpu_io_tpu2cpu_fire_tag ? 1'h1 : fetchEn_0; // @[proc.scala 264:73:@3559.6]
  assign _GEN_58 = tpu_io_tpu2cpu_fire_tag ? 1'h1 : fetchEn_1; // @[proc.scala 264:73:@3559.6]
  assign _GEN_59 = tpu_io_tpu2cpu_fire_valid ? _GEN_57 : fetchEn_0; // @[proc.scala 264:38:@3558.4]
  assign _GEN_60 = tpu_io_tpu2cpu_fire_valid ? _GEN_58 : fetchEn_1; // @[proc.scala 264:38:@3558.4]
  assign _T_360 = issuer_io_deq_valid & _T_300; // @[proc.scala 266:29:@3565.4]
  assign _GEN_65 = 1'h0 == tpu_io_tpu2cpu_flush_tag ? 1'h0 : _GEN_59; // @[proc.scala 266:97:@3567.6]
  assign _GEN_66 = tpu_io_tpu2cpu_flush_tag ? 1'h0 : _GEN_60; // @[proc.scala 266:97:@3567.6]
  assign _GEN_67 = _T_360 ? _GEN_65 : _GEN_59; // @[proc.scala 266:61:@3566.4]
  assign _GEN_68 = _T_360 ? _GEN_66 : _GEN_60; // @[proc.scala 266:61:@3566.4]
  assign _T_367 = decReg_io_deq_bits_tag == tpu_io_tpu2cpu_flush_tag; // @[proc.scala 277:47:@3578.6]
  assign _T_368 = commitReg_io_deq_valid & commitReg_io_deq_bits_br_valid; // @[proc.scala 278:26:@3582.6]
  assign _T_369 = fetch_io_deq_bits_tag == commitReg_io_deq_bits_tag; // @[proc.scala 279:51:@3584.8]
  assign _T_371 = decReg_io_deq_bits_tag == commitReg_io_deq_bits_tag; // @[proc.scala 283:47:@3589.8]
  assign _GEN_69 = _T_368 ? _T_369 : tpu_io_tpu2cpu_flush_valid; // @[proc.scala 278:46:@3583.6]
  assign _GEN_70 = _T_368 ? commitReg_io_deq_bits_tag : tpu_io_tpu2cpu_flush_tag; // @[proc.scala 278:46:@3583.6]
  assign _GEN_72 = _T_368 ? _T_371 : 1'h0; // @[proc.scala 278:46:@3583.6]
  assign _GEN_81 = tpu_io_tpu2cpu_freeze_tag ? pregsVec_1_PC : pregsVec_0_PC; // @[proc.scala 291:23:@3596.4]
  assign _GEN_82 = tpu_io_tpu2cpu_freeze_tag ? pregsVec_1_SP : pregsVec_0_SP; // @[proc.scala 291:23:@3596.4]
  assign _GEN_83 = tpu_io_tpu2cpu_freeze_tag ? pregsVec_1_EL : pregsVec_0_EL; // @[proc.scala 291:23:@3596.4]
  assign _GEN_84 = tpu_io_tpu2cpu_freeze_tag ? pregsVec_1_NZCV : pregsVec_0_NZCV; // @[proc.scala 291:23:@3596.4]
  assign _GEN_99 = tpu_io_tpu2cpu_freeze_tag ? rfileVec_1_rs1_data : rfileVec_0_rs1_data; // @[proc.scala 297:41:@3606.6]
  assign _GEN_109 = 1'h0 == tpu_io_tpu2cpu_freeze_tag ? _GEN_84 : _GEN_49; // @[proc.scala 299:41:@3610.6]
  assign _GEN_110 = tpu_io_tpu2cpu_freeze_tag ? _GEN_84 : _GEN_50; // @[proc.scala 299:41:@3610.6]
  assign _GEN_111 = 1'h0 == tpu_io_tpu2cpu_freeze_tag ? _GEN_83 : pregsVec_0_EL; // @[proc.scala 299:41:@3611.6]
  assign _GEN_112 = tpu_io_tpu2cpu_freeze_tag ? _GEN_83 : pregsVec_1_EL; // @[proc.scala 299:41:@3611.6]
  assign _GEN_113 = 1'h0 == tpu_io_tpu2cpu_freeze_tag ? _GEN_82 : pregsVec_0_SP; // @[proc.scala 299:41:@3612.6]
  assign _GEN_114 = tpu_io_tpu2cpu_freeze_tag ? _GEN_82 : pregsVec_1_SP; // @[proc.scala 299:41:@3612.6]
  assign _GEN_115 = 1'h0 == tpu_io_tpu2cpu_freeze_tag ? _GEN_81 : _GEN_51; // @[proc.scala 299:41:@3613.6]
  assign _GEN_116 = tpu_io_tpu2cpu_freeze_tag ? _GEN_81 : _GEN_52; // @[proc.scala 299:41:@3613.6]
  assign _T_398 = tpu_io_tpu2cpuStateReg_bits == 2'h2; // @[proc.scala 301:40:@3615.8]
  assign _pregsVec_tpu_io_tpu2cpu_freeze_tag_PC_0 = tpu_io_tpu2cpuState_PC; // @[proc.scala 302:48:@3617.10 proc.scala 302:48:@3617.10]
  assign _GEN_117 = 1'h0 == tpu_io_tpu2cpu_freeze_tag ? _pregsVec_tpu_io_tpu2cpu_freeze_tag_PC_0 : _GEN_115; // @[proc.scala 302:48:@3617.10]
  assign _GEN_118 = tpu_io_tpu2cpu_freeze_tag ? _pregsVec_tpu_io_tpu2cpu_freeze_tag_PC_0 : _GEN_116; // @[proc.scala 302:48:@3617.10]
  assign _T_402 = tpu_io_tpu2cpuStateReg_bits == 2'h3; // @[proc.scala 303:46:@3620.10]
  assign _pregsVec_tpu_io_tpu2cpu_freeze_tag_SP_0 = tpu_io_tpu2cpuState_SP; // @[proc.scala 304:48:@3622.12 proc.scala 304:48:@3622.12]
  assign _GEN_119 = 1'h0 == tpu_io_tpu2cpu_freeze_tag ? _pregsVec_tpu_io_tpu2cpu_freeze_tag_SP_0 : _GEN_113; // @[proc.scala 304:48:@3622.12]
  assign _GEN_120 = tpu_io_tpu2cpu_freeze_tag ? _pregsVec_tpu_io_tpu2cpu_freeze_tag_SP_0 : _GEN_114; // @[proc.scala 304:48:@3622.12]
  assign _pregsVec_tpu_io_tpu2cpu_freeze_tag_EL_0 = tpu_io_tpu2cpuState_EL; // @[proc.scala 305:48:@3623.12 proc.scala 305:48:@3623.12]
  assign _GEN_121 = 1'h0 == tpu_io_tpu2cpu_freeze_tag ? _pregsVec_tpu_io_tpu2cpu_freeze_tag_EL_0 : _GEN_111; // @[proc.scala 305:48:@3623.12]
  assign _GEN_122 = tpu_io_tpu2cpu_freeze_tag ? _pregsVec_tpu_io_tpu2cpu_freeze_tag_EL_0 : _GEN_112; // @[proc.scala 305:48:@3623.12]
  assign _pregsVec_tpu_io_tpu2cpu_freeze_tag_NZCV_0 = tpu_io_tpu2cpuState_NZCV; // @[proc.scala 306:50:@3624.12 proc.scala 306:50:@3624.12]
  assign _GEN_123 = 1'h0 == tpu_io_tpu2cpu_freeze_tag ? _pregsVec_tpu_io_tpu2cpu_freeze_tag_NZCV_0 : _GEN_109; // @[proc.scala 306:50:@3624.12]
  assign _GEN_124 = tpu_io_tpu2cpu_freeze_tag ? _pregsVec_tpu_io_tpu2cpu_freeze_tag_NZCV_0 : _GEN_110; // @[proc.scala 306:50:@3624.12]
  assign _GEN_125 = _T_402 ? _GEN_119 : _GEN_113; // @[proc.scala 303:73:@3621.10]
  assign _GEN_126 = _T_402 ? _GEN_120 : _GEN_114; // @[proc.scala 303:73:@3621.10]
  assign _GEN_127 = _T_402 ? _GEN_121 : _GEN_111; // @[proc.scala 303:73:@3621.10]
  assign _GEN_128 = _T_402 ? _GEN_122 : _GEN_112; // @[proc.scala 303:73:@3621.10]
  assign _GEN_129 = _T_402 ? _GEN_123 : _GEN_109; // @[proc.scala 303:73:@3621.10]
  assign _GEN_130 = _T_402 ? _GEN_124 : _GEN_110; // @[proc.scala 303:73:@3621.10]
  assign _GEN_131 = _T_398 ? _GEN_117 : _GEN_115; // @[proc.scala 301:60:@3616.8]
  assign _GEN_132 = _T_398 ? _GEN_118 : _GEN_116; // @[proc.scala 301:60:@3616.8]
  assign _GEN_133 = _T_398 ? _GEN_113 : _GEN_125; // @[proc.scala 301:60:@3616.8]
  assign _GEN_134 = _T_398 ? _GEN_114 : _GEN_126; // @[proc.scala 301:60:@3616.8]
  assign _GEN_135 = _T_398 ? _GEN_111 : _GEN_127; // @[proc.scala 301:60:@3616.8]
  assign _GEN_136 = _T_398 ? _GEN_112 : _GEN_128; // @[proc.scala 301:60:@3616.8]
  assign _GEN_137 = _T_398 ? _GEN_109 : _GEN_129; // @[proc.scala 301:60:@3616.8]
  assign _GEN_138 = _T_398 ? _GEN_110 : _GEN_130; // @[proc.scala 301:60:@3616.8]
  assign _GEN_139 = tpu_io_tpu2cpuStateReg_valid ? _GEN_131 : _GEN_115; // @[proc.scala 300:40:@3614.6]
  assign _GEN_140 = tpu_io_tpu2cpuStateReg_valid ? _GEN_132 : _GEN_116; // @[proc.scala 300:40:@3614.6]
  assign _GEN_141 = tpu_io_tpu2cpuStateReg_valid ? _GEN_133 : _GEN_113; // @[proc.scala 300:40:@3614.6]
  assign _GEN_142 = tpu_io_tpu2cpuStateReg_valid ? _GEN_134 : _GEN_114; // @[proc.scala 300:40:@3614.6]
  assign _GEN_143 = tpu_io_tpu2cpuStateReg_valid ? _GEN_135 : _GEN_111; // @[proc.scala 300:40:@3614.6]
  assign _GEN_144 = tpu_io_tpu2cpuStateReg_valid ? _GEN_136 : _GEN_112; // @[proc.scala 300:40:@3614.6]
  assign _GEN_145 = tpu_io_tpu2cpuStateReg_valid ? _GEN_137 : _GEN_109; // @[proc.scala 300:40:@3614.6]
  assign _GEN_146 = tpu_io_tpu2cpuStateReg_valid ? _GEN_138 : _GEN_110; // @[proc.scala 300:40:@3614.6]
  assign _GEN_159 = tpu_io_tpu2cpu_freeze_valid ? _GEN_145 : _GEN_49; // @[proc.scala 295:37:@3602.4]
  assign _GEN_160 = tpu_io_tpu2cpu_freeze_valid ? _GEN_146 : _GEN_50; // @[proc.scala 295:37:@3602.4]
  assign _GEN_161 = tpu_io_tpu2cpu_freeze_valid ? _GEN_143 : pregsVec_0_EL; // @[proc.scala 295:37:@3602.4]
  assign _GEN_162 = tpu_io_tpu2cpu_freeze_valid ? _GEN_144 : pregsVec_1_EL; // @[proc.scala 295:37:@3602.4]
  assign _GEN_163 = tpu_io_tpu2cpu_freeze_valid ? _GEN_141 : pregsVec_0_SP; // @[proc.scala 295:37:@3602.4]
  assign _GEN_164 = tpu_io_tpu2cpu_freeze_valid ? _GEN_142 : pregsVec_1_SP; // @[proc.scala 295:37:@3602.4]
  assign _GEN_165 = tpu_io_tpu2cpu_freeze_valid ? _GEN_139 : _GEN_51; // @[proc.scala 295:37:@3602.4]
  assign _GEN_166 = tpu_io_tpu2cpu_freeze_valid ? _GEN_140 : _GEN_52; // @[proc.scala 295:37:@3602.4]
  assign io_ppageBRAM_DO = ppage_io_portA_dataOut; // @[bram.scala 151:8:@3283.4]
  assign io_stateBRAM_DO = state_io_portA_dataOut; // @[bram.scala 151:8:@3288.4]
  assign io_host2tpu_done_tag = tpu_io_host2tpu_done_tag; // @[proc.scala 140:15:@3300.4]
  assign io_host2tpu_done_valid = tpu_io_host2tpu_done_valid; // @[proc.scala 140:15:@3299.4]
  assign ppage_clock = clock; // @[:@3185.4]
  assign ppage_io_portA_writeEn = io_ppageBRAM_WE; // @[bram.scala 148:8:@3280.4]
  assign ppage_io_portA_en = io_ppageBRAM_EN; // @[bram.scala 149:8:@3281.4]
  assign ppage_io_portA_addr = io_ppageBRAM_ADDR; // @[bram.scala 152:10:@3284.4]
  assign ppage_io_portA_dataIn = io_ppageBRAM_DI; // @[bram.scala 150:8:@3282.4]
  assign ppage_io_portB_writeEn = 1'h0; // @[proc.scala 156:35:@3318.4]
  assign ppage_io_portB_addr = insnTLB_io_paddr[9:0]; // @[proc.scala 157:28:@3319.4]
  assign ppage_io_portB_dataIn = 36'h0; // @[proc.scala 155:34:@3317.4]
  assign state_clock = clock; // @[:@3188.4]
  assign state_io_portA_writeEn = io_stateBRAM_WE; // @[bram.scala 148:8:@3285.4]
  assign state_io_portA_en = io_stateBRAM_EN; // @[bram.scala 149:8:@3286.4]
  assign state_io_portA_addr = io_stateBRAM_ADDR; // @[bram.scala 152:10:@3289.4]
  assign state_io_portA_dataIn = io_stateBRAM_DI; // @[bram.scala 150:8:@3287.4]
  assign state_io_portB_writeEn = tpu_io_stateBRAM_writeEn; // @[proc.scala 143:20:@3307.4]
  assign state_io_portB_addr = tpu_io_stateBRAM_addr; // @[proc.scala 143:20:@3305.4]
  assign state_io_portB_dataIn = tpu_io_stateBRAM_dataIn; // @[proc.scala 143:20:@3304.4]
  assign tpu_clock = clock; // @[:@3191.4]
  assign tpu_reset = reset; // @[:@3192.4]
  assign tpu_io_host2tpu_fire_tag = io_host2tpu_fire_tag; // @[proc.scala 140:15:@3302.4]
  assign tpu_io_host2tpu_fire_valid = io_host2tpu_fire_valid; // @[proc.scala 140:15:@3301.4]
  assign tpu_io_host2tpu_getState_tag = io_host2tpu_getState_tag; // @[proc.scala 140:15:@3291.4]
  assign tpu_io_host2tpu_getState_valid = io_host2tpu_getState_valid; // @[proc.scala 140:15:@3290.4]
  assign tpu_io_tpu2cpu_done_tag = commitReg_io_deq_bits_tag; // @[proc.scala 145:27:@3309.4 proc.scala 270:27:@3571.4]
  assign tpu_io_tpu2cpu_done_valid = commitReg_io_deq_valid & commitReg_io_deq_bits_undef; // @[proc.scala 144:29:@3308.4 proc.scala 269:29:@3570.4]
  assign tpu_io_cpu2tpuState_PC = tpu_io_tpu2cpu_freeze_tag ? pregsVec_1_PC : pregsVec_0_PC; // @[proc.scala 291:23:@3599.4]
  assign tpu_io_cpu2tpuState_SP = tpu_io_tpu2cpu_freeze_tag ? pregsVec_1_SP : pregsVec_0_SP; // @[proc.scala 291:23:@3598.4]
  assign tpu_io_cpu2tpuState_EL = tpu_io_tpu2cpu_freeze_tag ? pregsVec_1_EL : pregsVec_0_EL; // @[proc.scala 291:23:@3597.4]
  assign tpu_io_cpu2tpuState_NZCV = tpu_io_tpu2cpu_freeze_tag ? pregsVec_1_NZCV : pregsVec_0_NZCV; // @[proc.scala 291:23:@3596.4]
  assign tpu_io_rfile_rs1_data = tpu_io_tpu2cpu_freeze_valid ? _GEN_99 : rfileVec_0_rs1_data; // @[proc.scala 292:25:@3600.4 proc.scala 297:41:@3608.6]
  assign tpu_io_stateBRAM_dataOut = state_io_portB_dataOut; // @[proc.scala 143:20:@3303.4]
  assign RFile_clock = clock; // @[:@3194.4]
  assign RFile_io_rs1_addr = tpu_io_tpu2cpu_freeze_valid ? _GEN_107 : issuer_io_deq_bits_rs1_bits; // @[proc.scala 91:25:@3206.4]
  assign RFile_io_rs2_addr = tpu_io_tpu2cpu_freeze_valid ? _GEN_105 : issuer_io_deq_bits_rs2_bits; // @[proc.scala 91:25:@3204.4]
  assign RFile_io_waddr = tpu_io_tpu2cpu_freeze_valid ? _GEN_89 : _GEN_33; // @[proc.scala 91:25:@3202.4]
  assign RFile_io_wdata = tpu_io_tpu2cpu_freeze_valid ? _GEN_87 : _GEN_34; // @[proc.scala 91:25:@3201.4]
  assign RFile_io_wen = tpu_io_tpu2cpu_freeze_valid ? _GEN_85 : _GEN_47; // @[proc.scala 91:25:@3200.4]
  assign RFile_1_clock = clock; // @[:@3197.4]
  assign RFile_1_io_rs1_addr = tpu_io_tpu2cpu_freeze_valid ? _GEN_108 : issuer_io_deq_bits_rs1_bits; // @[proc.scala 91:25:@3213.4]
  assign RFile_1_io_rs2_addr = tpu_io_tpu2cpu_freeze_valid ? _GEN_106 : issuer_io_deq_bits_rs2_bits; // @[proc.scala 91:25:@3211.4]
  assign RFile_1_io_waddr = tpu_io_tpu2cpu_freeze_valid ? _GEN_90 : _GEN_33; // @[proc.scala 91:25:@3209.4]
  assign RFile_1_io_wdata = tpu_io_tpu2cpu_freeze_valid ? _GEN_88 : _GEN_34; // @[proc.scala 91:25:@3208.4]
  assign RFile_1_io_wen = tpu_io_tpu2cpu_freeze_valid ? _GEN_86 : _GEN_48; // @[proc.scala 91:25:@3207.4]
  assign insnTLB_io_vaddr = fetch_io_pc_data; // @[proc.scala 165:20:@3342.4]
  assign fetch_clock = clock; // @[:@3238.4]
  assign fetch_reset = reset; // @[:@3239.4]
  assign fetch_io_flush_tag = tpu_io_tpu2cpu_flush_valid ? tpu_io_tpu2cpu_flush_tag : _GEN_70; // @[proc.scala 274:18:@3575.4 proc.scala 280:24:@3586.8]
  assign fetch_io_flush_valid = tpu_io_tpu2cpu_flush_valid ? tpu_io_tpu2cpu_flush_valid : _GEN_69; // @[proc.scala 274:18:@3574.4 proc.scala 279:26:@3585.8]
  assign fetch_io_fire_tag = tpu_io_tpu2cpu_fire_tag; // @[proc.scala 159:17:@3321.4]
  assign fetch_io_fire_valid = tpu_io_tpu2cpu_fire_valid; // @[proc.scala 159:17:@3320.4]
  assign fetch_io_commitReg_valid = commitReg_io_deq_valid; // @[proc.scala 164:28:@3341.4]
  assign fetch_io_commitReg_bits_br_valid = commitReg_io_deq_bits_br_valid; // @[proc.scala 163:27:@3334.4]
  assign fetch_io_commitReg_bits_tag = commitReg_io_deq_bits_tag; // @[proc.scala 163:27:@3327.4]
  assign fetch_io_nextPC = _GEN_8[63:0]; // @[proc.scala 162:19:@3326.4]
  assign fetch_io_fetchEn_0 = fetchEn_0; // @[proc.scala 160:20:@3322.4]
  assign fetch_io_fetchEn_1 = fetchEn_1; // @[proc.scala 160:20:@3323.4]
  assign fetch_io_pcVec_0 = pregsVec_0_PC; // @[proc.scala 161:75:@3324.4]
  assign fetch_io_pcVec_1 = pregsVec_1_PC; // @[proc.scala 161:75:@3325.4]
  assign fetch_io_insn = ppage_io_portB_dataOut[31:0]; // @[proc.scala 167:17:@3345.4]
  assign fetch_io_deq_ready = decReg_io_enq_ready; // @[proc.scala 173:22:@3369.4]
  assign decoder_io_finst_inst = fetch_io_deq_bits_inst; // @[proc.scala 170:20:@3348.4]
  assign decoder_io_finst_tag = fetch_io_deq_bits_tag; // @[proc.scala 170:20:@3347.4]
  assign decoder_io_finst_pc = fetch_io_deq_bits_pc; // @[proc.scala 170:20:@3346.4]
  assign decReg_clock = clock; // @[:@3244.4]
  assign decReg_reset = reset; // @[:@3245.4]
  assign decReg_io_enq_valid = fetch_io_deq_valid; // @[proc.scala 174:23:@3370.4]
  assign decReg_io_enq_bits_rd_valid = decoder_io_dinst_rd_valid; // @[proc.scala 171:22:@3368.4]
  assign decReg_io_enq_bits_rd_bits = decoder_io_dinst_rd_bits; // @[proc.scala 171:22:@3367.4]
  assign decReg_io_enq_bits_rs1_bits = decoder_io_dinst_rs1_bits; // @[proc.scala 171:22:@3365.4]
  assign decReg_io_enq_bits_rs2_valid = decoder_io_dinst_rs2_valid; // @[proc.scala 171:22:@3364.4]
  assign decReg_io_enq_bits_rs2_bits = decoder_io_dinst_rs2_bits; // @[proc.scala 171:22:@3363.4]
  assign decReg_io_enq_bits_imm_bits = decoder_io_dinst_imm_bits; // @[proc.scala 171:22:@3361.4]
  assign decReg_io_enq_bits_shift_val_valid = decoder_io_dinst_shift_val_valid; // @[proc.scala 171:22:@3360.4]
  assign decReg_io_enq_bits_shift_val_bits = decoder_io_dinst_shift_val_bits; // @[proc.scala 171:22:@3359.4]
  assign decReg_io_enq_bits_shift_type = decoder_io_dinst_shift_type; // @[proc.scala 171:22:@3358.4]
  assign decReg_io_enq_bits_cond_bits = decoder_io_dinst_cond_bits; // @[proc.scala 171:22:@3356.4]
  assign decReg_io_enq_bits_itype = decoder_io_dinst_itype; // @[proc.scala 171:22:@3355.4]
  assign decReg_io_enq_bits_op = decoder_io_dinst_op; // @[proc.scala 171:22:@3354.4]
  assign decReg_io_enq_bits_nzcv_en = decoder_io_dinst_nzcv_en; // @[proc.scala 171:22:@3353.4]
  assign decReg_io_enq_bits_tag = decoder_io_dinst_tag; // @[proc.scala 171:22:@3352.4]
  assign decReg_io_enq_bits_pc = decoder_io_dinst_pc; // @[proc.scala 171:22:@3349.4]
  assign decReg_io_deq_ready = issuer_io_enq_ready; // @[proc.scala 177:17:@3392.4]
  assign decReg_io_flush = tpu_io_tpu2cpu_flush_valid ? _T_367 : _GEN_72; // @[proc.scala 277:21:@3579.6 proc.scala 283:21:@3590.8 proc.scala 286:21:@3594.8]
  assign issuer_clock = clock; // @[:@3247.4]
  assign issuer_reset = reset; // @[:@3248.4]
  assign issuer_io_flush_tag = tpu_io_tpu2cpu_flush_valid ? tpu_io_tpu2cpu_flush_tag : _GEN_70; // @[proc.scala 273:19:@3573.4 proc.scala 282:25:@3588.8]
  assign issuer_io_flush_valid = tpu_io_tpu2cpu_flush_valid ? tpu_io_tpu2cpu_flush_valid : _T_368; // @[proc.scala 273:19:@3572.4 proc.scala 281:27:@3587.8 proc.scala 285:27:@3593.8]
  assign issuer_io_enq_valid = decReg_io_deq_valid; // @[proc.scala 177:17:@3391.4]
  assign issuer_io_enq_bits_rd_valid = decReg_io_deq_bits_rd_valid; // @[proc.scala 177:17:@3390.4]
  assign issuer_io_enq_bits_rd_bits = decReg_io_deq_bits_rd_bits; // @[proc.scala 177:17:@3389.4]
  assign issuer_io_enq_bits_rs1_bits = decReg_io_deq_bits_rs1_bits; // @[proc.scala 177:17:@3387.4]
  assign issuer_io_enq_bits_rs2_valid = decReg_io_deq_bits_rs2_valid; // @[proc.scala 177:17:@3386.4]
  assign issuer_io_enq_bits_rs2_bits = decReg_io_deq_bits_rs2_bits; // @[proc.scala 177:17:@3385.4]
  assign issuer_io_enq_bits_imm_bits = decReg_io_deq_bits_imm_bits; // @[proc.scala 177:17:@3383.4]
  assign issuer_io_enq_bits_shift_val_valid = decReg_io_deq_bits_shift_val_valid; // @[proc.scala 177:17:@3382.4]
  assign issuer_io_enq_bits_shift_val_bits = decReg_io_deq_bits_shift_val_bits; // @[proc.scala 177:17:@3381.4]
  assign issuer_io_enq_bits_shift_type = decReg_io_deq_bits_shift_type; // @[proc.scala 177:17:@3380.4]
  assign issuer_io_enq_bits_cond_bits = decReg_io_deq_bits_cond_bits; // @[proc.scala 177:17:@3378.4]
  assign issuer_io_enq_bits_itype = decReg_io_deq_bits_itype; // @[proc.scala 177:17:@3377.4]
  assign issuer_io_enq_bits_op = decReg_io_deq_bits_op; // @[proc.scala 177:17:@3376.4]
  assign issuer_io_enq_bits_nzcv_en = decReg_io_deq_bits_nzcv_en; // @[proc.scala 177:17:@3375.4]
  assign issuer_io_enq_bits_tag = decReg_io_deq_bits_tag; // @[proc.scala 177:17:@3374.4]
  assign issuer_io_enq_bits_pc = decReg_io_deq_bits_pc; // @[proc.scala 177:17:@3371.4]
  assign issuer_io_commitReg_valid = commitReg_io_deq_valid; // @[proc.scala 184:29:@3408.4]
  assign issuer_io_commitReg_bits_exe_bits_rd_valid = commitReg_io_deq_bits_exe_bits_rd_valid; // @[proc.scala 183:28:@3406.4]
  assign issuer_io_commitReg_bits_exe_bits_rd_bits = commitReg_io_deq_bits_exe_bits_rd_bits; // @[proc.scala 183:28:@3405.4]
  assign issuer_io_commitReg_bits_tag = commitReg_io_deq_bits_tag; // @[proc.scala 183:28:@3394.4]
  assign executer_io_dinst_rd_valid = issuer_io_deq_bits_rd_valid; // @[proc.scala 197:21:@3432.4]
  assign executer_io_dinst_rd_bits = issuer_io_deq_bits_rd_bits; // @[proc.scala 197:21:@3431.4]
  assign executer_io_dinst_rs2_valid = issuer_io_deq_bits_rs2_valid; // @[proc.scala 197:21:@3428.4]
  assign executer_io_dinst_imm_bits = issuer_io_deq_bits_imm_bits; // @[proc.scala 197:21:@3425.4]
  assign executer_io_dinst_shift_val_valid = issuer_io_deq_bits_shift_val_valid; // @[proc.scala 197:21:@3424.4]
  assign executer_io_dinst_shift_val_bits = issuer_io_deq_bits_shift_val_bits; // @[proc.scala 197:21:@3423.4]
  assign executer_io_dinst_shift_type = issuer_io_deq_bits_shift_type; // @[proc.scala 197:21:@3422.4]
  assign executer_io_dinst_itype = issuer_io_deq_bits_itype; // @[proc.scala 197:21:@3419.4]
  assign executer_io_dinst_op = issuer_io_deq_bits_op; // @[proc.scala 197:21:@3418.4]
  assign executer_io_dinst_nzcv_en = issuer_io_deq_bits_nzcv_en; // @[proc.scala 197:21:@3417.4]
  assign executer_io_rVal1 = issuer_io_deq_bits_tag ? rfileVec_1_rs1_data : rfileVec_0_rs1_data; // @[proc.scala 198:21:@3433.4]
  assign executer_io_rVal2 = issuer_io_deq_bits_tag ? rfileVec_1_rs2_data : rfileVec_0_rs2_data; // @[proc.scala 199:21:@3434.4]
  assign brancher_io_dinst_imm_bits = issuer_io_deq_bits_imm_bits; // @[proc.scala 202:21:@3447.4]
  assign brancher_io_dinst_cond_bits = issuer_io_deq_bits_cond_bits; // @[proc.scala 202:21:@3442.4]
  assign brancher_io_dinst_itype = issuer_io_deq_bits_itype; // @[proc.scala 202:21:@3441.4]
  assign brancher_io_dinst_op = issuer_io_deq_bits_op; // @[proc.scala 202:21:@3440.4]
  assign brancher_io_nzcv = issuer_io_deq_bits_tag ? pregsVec_1_NZCV : pregsVec_0_NZCV; // @[proc.scala 203:20:@3455.4]
  assign ldstU_clock = clock; // @[:@3256.4]
  assign ldstU_reset = reset; // @[:@3257.4]
  assign ldstU_io_dinst_valid = issuer_io_deq_valid; // @[proc.scala 207:24:@3476.4]
  assign ldstU_io_dinst_bits_imm_bits = issuer_io_deq_bits_imm_bits; // @[proc.scala 206:23:@3468.4]
  assign ldstU_io_dinst_bits_itype = issuer_io_deq_bits_itype; // @[proc.scala 206:23:@3462.4]
  assign ldstU_io_dinst_bits_pc = issuer_io_deq_bits_pc; // @[proc.scala 206:23:@3456.4]
  assign commitReg_clock = clock; // @[:@3259.4]
  assign commitReg_reset = reset; // @[:@3260.4]
  assign commitReg_io_enq_valid = issuer_io_deq_valid; // @[proc.scala 224:26:@3501.4]
  assign commitReg_io_enq_bits_exe_valid = executer_io_einst_valid; // @[proc.scala 219:29:@3491.4]
  assign commitReg_io_enq_bits_exe_bits_rd_valid = executer_io_einst_bits_rd_valid; // @[proc.scala 219:29:@3490.4]
  assign commitReg_io_enq_bits_exe_bits_rd_bits = executer_io_einst_bits_rd_bits; // @[proc.scala 219:29:@3489.4]
  assign commitReg_io_enq_bits_exe_bits_nzcv_valid = executer_io_einst_bits_nzcv_valid; // @[proc.scala 219:29:@3488.4]
  assign commitReg_io_enq_bits_exe_bits_nzcv_bits = executer_io_einst_bits_nzcv_bits; // @[proc.scala 219:29:@3487.4]
  assign commitReg_io_enq_bits_exe_bits_res = executer_io_einst_bits_res; // @[proc.scala 219:29:@3486.4]
  assign commitReg_io_enq_bits_br_valid = brancher_io_binst_valid; // @[proc.scala 220:28:@3493.4]
  assign commitReg_io_enq_bits_br_bits_offset = brancher_io_binst_bits_offset; // @[proc.scala 220:28:@3492.4]
  assign commitReg_io_enq_bits_undef = issuer_io_deq_bits_itype == 3'h0; // @[proc.scala 222:31:@3499.4]
  assign commitReg_io_enq_bits_tag = issuer_io_deq_bits_tag; // @[proc.scala 223:29:@3500.4]
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
  pregsVec_0_PC = _RAND_0[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  pregsVec_0_SP = _RAND_1[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  pregsVec_0_EL = _RAND_2[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  pregsVec_0_NZCV = _RAND_3[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {2{`RANDOM}};
  pregsVec_1_PC = _RAND_4[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  pregsVec_1_SP = _RAND_5[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  pregsVec_1_EL = _RAND_6[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  pregsVec_1_NZCV = _RAND_7[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  fetchEn_0 = _RAND_8[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  fetchEn_1 = _RAND_9[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (reset) begin
      pregsVec_0_PC <= 64'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze_valid) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_398) begin
            if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
              pregsVec_0_PC <= _pregsVec_tpu_io_tpu2cpu_freeze_tag_PC_0;
            end else begin
              if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
                if (tpu_io_tpu2cpu_freeze_tag) begin
                  pregsVec_0_PC <= pregsVec_1_PC;
                end
              end else begin
                if (commitReg_io_deq_valid) begin
                  if (commitReg_io_deq_bits_br_valid) begin
                    if (1'h0 == commitReg_io_deq_bits_tag) begin
                      pregsVec_0_PC <= _pregsVec_commitReg_io_deq_bits_tag_PC;
                    end
                  end else begin
                    if (1'h0 == commitReg_io_deq_bits_tag) begin
                      pregsVec_0_PC <= _T_257;
                    end
                  end
                end
              end
            end
          end else begin
            if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
              if (tpu_io_tpu2cpu_freeze_tag) begin
                pregsVec_0_PC <= pregsVec_1_PC;
              end
            end else begin
              if (commitReg_io_deq_valid) begin
                if (commitReg_io_deq_bits_br_valid) begin
                  if (1'h0 == commitReg_io_deq_bits_tag) begin
                    pregsVec_0_PC <= _pregsVec_commitReg_io_deq_bits_tag_PC;
                  end
                end else begin
                  if (1'h0 == commitReg_io_deq_bits_tag) begin
                    pregsVec_0_PC <= _T_257;
                  end
                end
              end
            end
          end
        end else begin
          if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
            if (tpu_io_tpu2cpu_freeze_tag) begin
              pregsVec_0_PC <= pregsVec_1_PC;
            end
          end else begin
            if (commitReg_io_deq_valid) begin
              if (commitReg_io_deq_bits_br_valid) begin
                if (1'h0 == commitReg_io_deq_bits_tag) begin
                  pregsVec_0_PC <= _pregsVec_commitReg_io_deq_bits_tag_PC;
                end
              end else begin
                if (1'h0 == commitReg_io_deq_bits_tag) begin
                  pregsVec_0_PC <= _T_257;
                end
              end
            end
          end
        end
      end else begin
        if (commitReg_io_deq_valid) begin
          if (commitReg_io_deq_bits_br_valid) begin
            if (1'h0 == commitReg_io_deq_bits_tag) begin
              pregsVec_0_PC <= _pregsVec_commitReg_io_deq_bits_tag_PC;
            end
          end else begin
            if (1'h0 == commitReg_io_deq_bits_tag) begin
              pregsVec_0_PC <= _T_257;
            end
          end
        end
      end
    end
    if (reset) begin
      pregsVec_0_SP <= 32'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze_valid) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_398) begin
            if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
              if (tpu_io_tpu2cpu_freeze_tag) begin
                pregsVec_0_SP <= pregsVec_1_SP;
              end
            end
          end else begin
            if (_T_402) begin
              if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
                pregsVec_0_SP <= _pregsVec_tpu_io_tpu2cpu_freeze_tag_SP_0;
              end else begin
                if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
                  if (tpu_io_tpu2cpu_freeze_tag) begin
                    pregsVec_0_SP <= pregsVec_1_SP;
                  end
                end
              end
            end else begin
              if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
                if (tpu_io_tpu2cpu_freeze_tag) begin
                  pregsVec_0_SP <= pregsVec_1_SP;
                end
              end
            end
          end
        end else begin
          if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
            if (tpu_io_tpu2cpu_freeze_tag) begin
              pregsVec_0_SP <= pregsVec_1_SP;
            end
          end
        end
      end
    end
    if (reset) begin
      pregsVec_0_EL <= 32'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze_valid) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_398) begin
            if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
              if (tpu_io_tpu2cpu_freeze_tag) begin
                pregsVec_0_EL <= pregsVec_1_EL;
              end
            end
          end else begin
            if (_T_402) begin
              if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
                pregsVec_0_EL <= _pregsVec_tpu_io_tpu2cpu_freeze_tag_EL_0;
              end else begin
                if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
                  if (tpu_io_tpu2cpu_freeze_tag) begin
                    pregsVec_0_EL <= pregsVec_1_EL;
                  end
                end
              end
            end else begin
              if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
                if (tpu_io_tpu2cpu_freeze_tag) begin
                  pregsVec_0_EL <= pregsVec_1_EL;
                end
              end
            end
          end
        end else begin
          if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
            if (tpu_io_tpu2cpu_freeze_tag) begin
              pregsVec_0_EL <= pregsVec_1_EL;
            end
          end
        end
      end
    end
    if (reset) begin
      pregsVec_0_NZCV <= 4'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze_valid) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_398) begin
            if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
              if (tpu_io_tpu2cpu_freeze_tag) begin
                pregsVec_0_NZCV <= pregsVec_1_NZCV;
              end
            end else begin
              if (commitReg_io_deq_valid) begin
                if (_T_322) begin
                  if (1'h0 == commitReg_io_deq_bits_tag) begin
                    pregsVec_0_NZCV <= _pregsVec_commitReg_io_deq_bits_tag_NZCV;
                  end
                end
              end
            end
          end else begin
            if (_T_402) begin
              if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
                pregsVec_0_NZCV <= _pregsVec_tpu_io_tpu2cpu_freeze_tag_NZCV_0;
              end else begin
                if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
                  if (tpu_io_tpu2cpu_freeze_tag) begin
                    pregsVec_0_NZCV <= pregsVec_1_NZCV;
                  end
                end else begin
                  if (commitReg_io_deq_valid) begin
                    if (_T_322) begin
                      if (1'h0 == commitReg_io_deq_bits_tag) begin
                        pregsVec_0_NZCV <= _pregsVec_commitReg_io_deq_bits_tag_NZCV;
                      end
                    end
                  end
                end
              end
            end else begin
              if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
                if (tpu_io_tpu2cpu_freeze_tag) begin
                  pregsVec_0_NZCV <= pregsVec_1_NZCV;
                end
              end else begin
                if (commitReg_io_deq_valid) begin
                  if (_T_322) begin
                    if (1'h0 == commitReg_io_deq_bits_tag) begin
                      pregsVec_0_NZCV <= _pregsVec_commitReg_io_deq_bits_tag_NZCV;
                    end
                  end
                end
              end
            end
          end
        end else begin
          if (1'h0 == tpu_io_tpu2cpu_freeze_tag) begin
            if (tpu_io_tpu2cpu_freeze_tag) begin
              pregsVec_0_NZCV <= pregsVec_1_NZCV;
            end
          end else begin
            if (commitReg_io_deq_valid) begin
              if (_T_322) begin
                if (1'h0 == commitReg_io_deq_bits_tag) begin
                  pregsVec_0_NZCV <= _pregsVec_commitReg_io_deq_bits_tag_NZCV;
                end
              end
            end
          end
        end
      end else begin
        pregsVec_0_NZCV <= _GEN_49;
      end
    end
    if (reset) begin
      pregsVec_1_PC <= 64'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze_valid) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_398) begin
            if (tpu_io_tpu2cpu_freeze_tag) begin
              pregsVec_1_PC <= _pregsVec_tpu_io_tpu2cpu_freeze_tag_PC_0;
            end else begin
              if (tpu_io_tpu2cpu_freeze_tag) begin
                if (!(tpu_io_tpu2cpu_freeze_tag)) begin
                  pregsVec_1_PC <= pregsVec_0_PC;
                end
              end else begin
                if (commitReg_io_deq_valid) begin
                  if (commitReg_io_deq_bits_br_valid) begin
                    if (commitReg_io_deq_bits_tag) begin
                      pregsVec_1_PC <= _pregsVec_commitReg_io_deq_bits_tag_PC;
                    end
                  end else begin
                    if (commitReg_io_deq_bits_tag) begin
                      pregsVec_1_PC <= _T_257;
                    end
                  end
                end
              end
            end
          end else begin
            if (tpu_io_tpu2cpu_freeze_tag) begin
              if (!(tpu_io_tpu2cpu_freeze_tag)) begin
                pregsVec_1_PC <= pregsVec_0_PC;
              end
            end else begin
              if (commitReg_io_deq_valid) begin
                if (commitReg_io_deq_bits_br_valid) begin
                  if (commitReg_io_deq_bits_tag) begin
                    pregsVec_1_PC <= _pregsVec_commitReg_io_deq_bits_tag_PC;
                  end
                end else begin
                  if (commitReg_io_deq_bits_tag) begin
                    pregsVec_1_PC <= _T_257;
                  end
                end
              end
            end
          end
        end else begin
          if (tpu_io_tpu2cpu_freeze_tag) begin
            if (!(tpu_io_tpu2cpu_freeze_tag)) begin
              pregsVec_1_PC <= pregsVec_0_PC;
            end
          end else begin
            if (commitReg_io_deq_valid) begin
              if (commitReg_io_deq_bits_br_valid) begin
                if (commitReg_io_deq_bits_tag) begin
                  pregsVec_1_PC <= _pregsVec_commitReg_io_deq_bits_tag_PC;
                end
              end else begin
                if (commitReg_io_deq_bits_tag) begin
                  pregsVec_1_PC <= _T_257;
                end
              end
            end
          end
        end
      end else begin
        if (commitReg_io_deq_valid) begin
          if (commitReg_io_deq_bits_br_valid) begin
            if (commitReg_io_deq_bits_tag) begin
              pregsVec_1_PC <= _pregsVec_commitReg_io_deq_bits_tag_PC;
            end
          end else begin
            if (commitReg_io_deq_bits_tag) begin
              pregsVec_1_PC <= _T_257;
            end
          end
        end
      end
    end
    if (reset) begin
      pregsVec_1_SP <= 32'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze_valid) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_398) begin
            if (tpu_io_tpu2cpu_freeze_tag) begin
              if (!(tpu_io_tpu2cpu_freeze_tag)) begin
                pregsVec_1_SP <= pregsVec_0_SP;
              end
            end
          end else begin
            if (_T_402) begin
              if (tpu_io_tpu2cpu_freeze_tag) begin
                pregsVec_1_SP <= _pregsVec_tpu_io_tpu2cpu_freeze_tag_SP_0;
              end else begin
                if (tpu_io_tpu2cpu_freeze_tag) begin
                  if (!(tpu_io_tpu2cpu_freeze_tag)) begin
                    pregsVec_1_SP <= pregsVec_0_SP;
                  end
                end
              end
            end else begin
              if (tpu_io_tpu2cpu_freeze_tag) begin
                if (!(tpu_io_tpu2cpu_freeze_tag)) begin
                  pregsVec_1_SP <= pregsVec_0_SP;
                end
              end
            end
          end
        end else begin
          if (tpu_io_tpu2cpu_freeze_tag) begin
            if (!(tpu_io_tpu2cpu_freeze_tag)) begin
              pregsVec_1_SP <= pregsVec_0_SP;
            end
          end
        end
      end
    end
    if (reset) begin
      pregsVec_1_EL <= 32'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze_valid) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_398) begin
            if (tpu_io_tpu2cpu_freeze_tag) begin
              if (!(tpu_io_tpu2cpu_freeze_tag)) begin
                pregsVec_1_EL <= pregsVec_0_EL;
              end
            end
          end else begin
            if (_T_402) begin
              if (tpu_io_tpu2cpu_freeze_tag) begin
                pregsVec_1_EL <= _pregsVec_tpu_io_tpu2cpu_freeze_tag_EL_0;
              end else begin
                if (tpu_io_tpu2cpu_freeze_tag) begin
                  if (!(tpu_io_tpu2cpu_freeze_tag)) begin
                    pregsVec_1_EL <= pregsVec_0_EL;
                  end
                end
              end
            end else begin
              if (tpu_io_tpu2cpu_freeze_tag) begin
                if (!(tpu_io_tpu2cpu_freeze_tag)) begin
                  pregsVec_1_EL <= pregsVec_0_EL;
                end
              end
            end
          end
        end else begin
          if (tpu_io_tpu2cpu_freeze_tag) begin
            if (!(tpu_io_tpu2cpu_freeze_tag)) begin
              pregsVec_1_EL <= pregsVec_0_EL;
            end
          end
        end
      end
    end
    if (reset) begin
      pregsVec_1_NZCV <= 4'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze_valid) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_398) begin
            if (tpu_io_tpu2cpu_freeze_tag) begin
              if (!(tpu_io_tpu2cpu_freeze_tag)) begin
                pregsVec_1_NZCV <= pregsVec_0_NZCV;
              end
            end else begin
              if (commitReg_io_deq_valid) begin
                if (_T_322) begin
                  if (commitReg_io_deq_bits_tag) begin
                    pregsVec_1_NZCV <= _pregsVec_commitReg_io_deq_bits_tag_NZCV;
                  end
                end
              end
            end
          end else begin
            if (_T_402) begin
              if (tpu_io_tpu2cpu_freeze_tag) begin
                pregsVec_1_NZCV <= _pregsVec_tpu_io_tpu2cpu_freeze_tag_NZCV_0;
              end else begin
                if (tpu_io_tpu2cpu_freeze_tag) begin
                  if (!(tpu_io_tpu2cpu_freeze_tag)) begin
                    pregsVec_1_NZCV <= pregsVec_0_NZCV;
                  end
                end else begin
                  if (commitReg_io_deq_valid) begin
                    if (_T_322) begin
                      if (commitReg_io_deq_bits_tag) begin
                        pregsVec_1_NZCV <= _pregsVec_commitReg_io_deq_bits_tag_NZCV;
                      end
                    end
                  end
                end
              end
            end else begin
              if (tpu_io_tpu2cpu_freeze_tag) begin
                if (!(tpu_io_tpu2cpu_freeze_tag)) begin
                  pregsVec_1_NZCV <= pregsVec_0_NZCV;
                end
              end else begin
                if (commitReg_io_deq_valid) begin
                  if (_T_322) begin
                    if (commitReg_io_deq_bits_tag) begin
                      pregsVec_1_NZCV <= _pregsVec_commitReg_io_deq_bits_tag_NZCV;
                    end
                  end
                end
              end
            end
          end
        end else begin
          if (tpu_io_tpu2cpu_freeze_tag) begin
            if (!(tpu_io_tpu2cpu_freeze_tag)) begin
              pregsVec_1_NZCV <= pregsVec_0_NZCV;
            end
          end else begin
            if (commitReg_io_deq_valid) begin
              if (_T_322) begin
                if (commitReg_io_deq_bits_tag) begin
                  pregsVec_1_NZCV <= _pregsVec_commitReg_io_deq_bits_tag_NZCV;
                end
              end
            end
          end
        end
      end else begin
        pregsVec_1_NZCV <= _GEN_50;
      end
    end
    if (reset) begin
      fetchEn_0 <= 1'h0;
    end else begin
      if (_T_360) begin
        if (1'h0 == tpu_io_tpu2cpu_flush_tag) begin
          fetchEn_0 <= 1'h0;
        end else begin
          if (tpu_io_tpu2cpu_fire_valid) begin
            if (1'h0 == tpu_io_tpu2cpu_fire_tag) begin
              fetchEn_0 <= 1'h1;
            end
          end
        end
      end else begin
        if (tpu_io_tpu2cpu_fire_valid) begin
          if (1'h0 == tpu_io_tpu2cpu_fire_tag) begin
            fetchEn_0 <= 1'h1;
          end
        end
      end
    end
    if (reset) begin
      fetchEn_1 <= 1'h0;
    end else begin
      if (_T_360) begin
        if (tpu_io_tpu2cpu_flush_tag) begin
          fetchEn_1 <= 1'h0;
        end else begin
          if (tpu_io_tpu2cpu_fire_valid) begin
            if (tpu_io_tpu2cpu_fire_tag) begin
              fetchEn_1 <= 1'h1;
            end
          end
        end
      end else begin
        if (tpu_io_tpu2cpu_fire_valid) begin
          if (tpu_io_tpu2cpu_fire_tag) begin
            fetchEn_1 <= 1'h1;
          end
        end
      end
    end
  end
endmodule
