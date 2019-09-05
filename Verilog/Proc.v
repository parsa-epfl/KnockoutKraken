module Proc( // @[:@3004.2]
  input         clock, // @[:@3005.4]
  input         reset, // @[:@3006.4]
  input         io_ppageBRAM_WE, // @[:@3007.4]
  input         io_ppageBRAM_EN, // @[:@3007.4]
  input  [9:0]  io_ppageBRAM_ADDR, // @[:@3007.4]
  input  [35:0] io_ppageBRAM_DI, // @[:@3007.4]
  output [35:0] io_ppageBRAM_DO, // @[:@3007.4]
  input         io_stateBRAM_WE, // @[:@3007.4]
  input         io_stateBRAM_EN, // @[:@3007.4]
  input  [9:0]  io_stateBRAM_ADDR, // @[:@3007.4]
  input  [35:0] io_stateBRAM_DI, // @[:@3007.4]
  output [35:0] io_stateBRAM_DO, // @[:@3007.4]
  input         io_host2tpu_fire, // @[:@3007.4]
  input         io_host2tpu_fireTag, // @[:@3007.4]
  output        io_host2tpu_done, // @[:@3007.4]
  output        io_host2tpu_doneTag // @[:@3007.4]
);
  wire  ppage_clock; // @[proc.scala 71:21:@3009.4]
  wire  ppage_io_portA_writeEn; // @[proc.scala 71:21:@3009.4]
  wire  ppage_io_portA_en; // @[proc.scala 71:21:@3009.4]
  wire [9:0] ppage_io_portA_addr; // @[proc.scala 71:21:@3009.4]
  wire [35:0] ppage_io_portA_dataIn; // @[proc.scala 71:21:@3009.4]
  wire [35:0] ppage_io_portA_dataOut; // @[proc.scala 71:21:@3009.4]
  wire  ppage_io_portB_writeEn; // @[proc.scala 71:21:@3009.4]
  wire [9:0] ppage_io_portB_addr; // @[proc.scala 71:21:@3009.4]
  wire [35:0] ppage_io_portB_dataIn; // @[proc.scala 71:21:@3009.4]
  wire [35:0] ppage_io_portB_dataOut; // @[proc.scala 71:21:@3009.4]
  wire  state_clock; // @[proc.scala 73:21:@3012.4]
  wire  state_io_portA_writeEn; // @[proc.scala 73:21:@3012.4]
  wire  state_io_portA_en; // @[proc.scala 73:21:@3012.4]
  wire [9:0] state_io_portA_addr; // @[proc.scala 73:21:@3012.4]
  wire [35:0] state_io_portA_dataIn; // @[proc.scala 73:21:@3012.4]
  wire [35:0] state_io_portA_dataOut; // @[proc.scala 73:21:@3012.4]
  wire  state_io_portB_writeEn; // @[proc.scala 73:21:@3012.4]
  wire [9:0] state_io_portB_addr; // @[proc.scala 73:21:@3012.4]
  wire [35:0] state_io_portB_dataIn; // @[proc.scala 73:21:@3012.4]
  wire [35:0] state_io_portB_dataOut; // @[proc.scala 73:21:@3012.4]
  wire  tpu_clock; // @[proc.scala 75:19:@3015.4]
  wire  tpu_reset; // @[proc.scala 75:19:@3015.4]
  wire  tpu_io_host2tpu_fire; // @[proc.scala 75:19:@3015.4]
  wire  tpu_io_host2tpu_fireTag; // @[proc.scala 75:19:@3015.4]
  wire  tpu_io_host2tpu_done; // @[proc.scala 75:19:@3015.4]
  wire  tpu_io_host2tpu_doneTag; // @[proc.scala 75:19:@3015.4]
  wire  tpu_io_tpu2cpu_flush; // @[proc.scala 75:19:@3015.4]
  wire  tpu_io_tpu2cpu_freeze; // @[proc.scala 75:19:@3015.4]
  wire  tpu_io_tpu2cpu_fire; // @[proc.scala 75:19:@3015.4]
  wire  tpu_io_tpu2cpu_done; // @[proc.scala 75:19:@3015.4]
  wire  tpu_io_tpu2cpu_flushTag; // @[proc.scala 75:19:@3015.4]
  wire  tpu_io_tpu2cpu_fireTag; // @[proc.scala 75:19:@3015.4]
  wire  tpu_io_tpu2cpu_freezeTag; // @[proc.scala 75:19:@3015.4]
  wire  tpu_io_tpu2cpu_doneTag; // @[proc.scala 75:19:@3015.4]
  wire  tpu_io_tpu2cpuStateReg_valid; // @[proc.scala 75:19:@3015.4]
  wire [1:0] tpu_io_tpu2cpuStateReg_bits; // @[proc.scala 75:19:@3015.4]
  wire [63:0] tpu_io_tpu2cpuState_PC; // @[proc.scala 75:19:@3015.4]
  wire [31:0] tpu_io_tpu2cpuState_SP; // @[proc.scala 75:19:@3015.4]
  wire [31:0] tpu_io_tpu2cpuState_EL; // @[proc.scala 75:19:@3015.4]
  wire [3:0] tpu_io_tpu2cpuState_NZCV; // @[proc.scala 75:19:@3015.4]
  wire [63:0] tpu_io_cpu2tpuState_PC; // @[proc.scala 75:19:@3015.4]
  wire [31:0] tpu_io_cpu2tpuState_SP; // @[proc.scala 75:19:@3015.4]
  wire [31:0] tpu_io_cpu2tpuState_EL; // @[proc.scala 75:19:@3015.4]
  wire [3:0] tpu_io_cpu2tpuState_NZCV; // @[proc.scala 75:19:@3015.4]
  wire [4:0] tpu_io_rfile_rs1_addr; // @[proc.scala 75:19:@3015.4]
  wire [63:0] tpu_io_rfile_rs1_data; // @[proc.scala 75:19:@3015.4]
  wire [4:0] tpu_io_rfile_waddr; // @[proc.scala 75:19:@3015.4]
  wire [63:0] tpu_io_rfile_wdata; // @[proc.scala 75:19:@3015.4]
  wire  tpu_io_rfile_wen; // @[proc.scala 75:19:@3015.4]
  wire  tpu_io_stateBRAM_writeEn; // @[proc.scala 75:19:@3015.4]
  wire [9:0] tpu_io_stateBRAM_addr; // @[proc.scala 75:19:@3015.4]
  wire [35:0] tpu_io_stateBRAM_dataIn; // @[proc.scala 75:19:@3015.4]
  wire [35:0] tpu_io_stateBRAM_dataOut; // @[proc.scala 75:19:@3015.4]
  wire  RFile_clock; // @[proc.scala 80:58:@3028.4]
  wire [4:0] RFile_io_rs1_addr; // @[proc.scala 80:58:@3028.4]
  wire [63:0] RFile_io_rs1_data; // @[proc.scala 80:58:@3028.4]
  wire [4:0] RFile_io_rs2_addr; // @[proc.scala 80:58:@3028.4]
  wire [63:0] RFile_io_rs2_data; // @[proc.scala 80:58:@3028.4]
  wire [4:0] RFile_io_waddr; // @[proc.scala 80:58:@3028.4]
  wire [63:0] RFile_io_wdata; // @[proc.scala 80:58:@3028.4]
  wire  RFile_io_wen; // @[proc.scala 80:58:@3028.4]
  wire  RFile_1_clock; // @[proc.scala 80:58:@3031.4]
  wire [4:0] RFile_1_io_rs1_addr; // @[proc.scala 80:58:@3031.4]
  wire [63:0] RFile_1_io_rs1_data; // @[proc.scala 80:58:@3031.4]
  wire [4:0] RFile_1_io_rs2_addr; // @[proc.scala 80:58:@3031.4]
  wire [63:0] RFile_1_io_rs2_data; // @[proc.scala 80:58:@3031.4]
  wire [4:0] RFile_1_io_waddr; // @[proc.scala 80:58:@3031.4]
  wire [63:0] RFile_1_io_wdata; // @[proc.scala 80:58:@3031.4]
  wire  RFile_1_io_wen; // @[proc.scala 80:58:@3031.4]
  wire  fetch_clock; // @[proc.scala 86:21:@3060.4]
  wire  fetch_reset; // @[proc.scala 86:21:@3060.4]
  wire  fetch_io_en; // @[proc.scala 86:21:@3060.4]
  wire [63:0] fetch_io_vecPC_0; // @[proc.scala 86:21:@3060.4]
  wire [63:0] fetch_io_vecPC_1; // @[proc.scala 86:21:@3060.4]
  wire  fetch_io_tagIn; // @[proc.scala 86:21:@3060.4]
  wire  fetch_io_flush; // @[proc.scala 86:21:@3060.4]
  wire  fetch_io_fire_valid; // @[proc.scala 86:21:@3060.4]
  wire  fetch_io_fire_bits; // @[proc.scala 86:21:@3060.4]
  wire  fetch_io_branch_valid; // @[proc.scala 86:21:@3060.4]
  wire [63:0] fetch_io_branch_bits_offset; // @[proc.scala 86:21:@3060.4]
  wire  fetch_io_branch_bits_tag; // @[proc.scala 86:21:@3060.4]
  wire [9:0] fetch_io_ppageBRAM_addr; // @[proc.scala 86:21:@3060.4]
  wire [35:0] fetch_io_ppageBRAM_dataOut; // @[proc.scala 86:21:@3060.4]
  wire  fetch_io_deq_ready; // @[proc.scala 86:21:@3060.4]
  wire  fetch_io_deq_valid; // @[proc.scala 86:21:@3060.4]
  wire [31:0] fetch_io_deq_bits_inst; // @[proc.scala 86:21:@3060.4]
  wire  fetch_io_deq_bits_tag; // @[proc.scala 86:21:@3060.4]
  wire [63:0] fetch_io_deq_bits_pc; // @[proc.scala 86:21:@3060.4]
  wire [31:0] decoder_io_finst_inst; // @[proc.scala 89:23:@3063.4]
  wire  decoder_io_finst_tag; // @[proc.scala 89:23:@3063.4]
  wire [63:0] decoder_io_finst_pc; // @[proc.scala 89:23:@3063.4]
  wire  decoder_io_dinst_rd_valid; // @[proc.scala 89:23:@3063.4]
  wire [4:0] decoder_io_dinst_rd_bits; // @[proc.scala 89:23:@3063.4]
  wire [4:0] decoder_io_dinst_rs1_bits; // @[proc.scala 89:23:@3063.4]
  wire  decoder_io_dinst_rs2_valid; // @[proc.scala 89:23:@3063.4]
  wire [4:0] decoder_io_dinst_rs2_bits; // @[proc.scala 89:23:@3063.4]
  wire [25:0] decoder_io_dinst_imm_bits; // @[proc.scala 89:23:@3063.4]
  wire  decoder_io_dinst_shift_val_valid; // @[proc.scala 89:23:@3063.4]
  wire [5:0] decoder_io_dinst_shift_val_bits; // @[proc.scala 89:23:@3063.4]
  wire [1:0] decoder_io_dinst_shift_type; // @[proc.scala 89:23:@3063.4]
  wire [3:0] decoder_io_dinst_cond_bits; // @[proc.scala 89:23:@3063.4]
  wire [2:0] decoder_io_dinst_itype; // @[proc.scala 89:23:@3063.4]
  wire [2:0] decoder_io_dinst_op; // @[proc.scala 89:23:@3063.4]
  wire  decoder_io_dinst_nzcv_en; // @[proc.scala 89:23:@3063.4]
  wire  decoder_io_dinst_tag; // @[proc.scala 89:23:@3063.4]
  wire [63:0] decoder_io_dinst_pc; // @[proc.scala 89:23:@3063.4]
  wire  decReg_clock; // @[proc.scala 90:22:@3066.4]
  wire  decReg_reset; // @[proc.scala 90:22:@3066.4]
  wire  decReg_io_enq_ready; // @[proc.scala 90:22:@3066.4]
  wire  decReg_io_enq_valid; // @[proc.scala 90:22:@3066.4]
  wire  decReg_io_enq_bits_rd_valid; // @[proc.scala 90:22:@3066.4]
  wire [4:0] decReg_io_enq_bits_rd_bits; // @[proc.scala 90:22:@3066.4]
  wire [4:0] decReg_io_enq_bits_rs1_bits; // @[proc.scala 90:22:@3066.4]
  wire  decReg_io_enq_bits_rs2_valid; // @[proc.scala 90:22:@3066.4]
  wire [4:0] decReg_io_enq_bits_rs2_bits; // @[proc.scala 90:22:@3066.4]
  wire [25:0] decReg_io_enq_bits_imm_bits; // @[proc.scala 90:22:@3066.4]
  wire  decReg_io_enq_bits_shift_val_valid; // @[proc.scala 90:22:@3066.4]
  wire [5:0] decReg_io_enq_bits_shift_val_bits; // @[proc.scala 90:22:@3066.4]
  wire [1:0] decReg_io_enq_bits_shift_type; // @[proc.scala 90:22:@3066.4]
  wire [3:0] decReg_io_enq_bits_cond_bits; // @[proc.scala 90:22:@3066.4]
  wire [2:0] decReg_io_enq_bits_itype; // @[proc.scala 90:22:@3066.4]
  wire [2:0] decReg_io_enq_bits_op; // @[proc.scala 90:22:@3066.4]
  wire  decReg_io_enq_bits_nzcv_en; // @[proc.scala 90:22:@3066.4]
  wire  decReg_io_enq_bits_tag; // @[proc.scala 90:22:@3066.4]
  wire [63:0] decReg_io_enq_bits_pc; // @[proc.scala 90:22:@3066.4]
  wire  decReg_io_deq_ready; // @[proc.scala 90:22:@3066.4]
  wire  decReg_io_deq_valid; // @[proc.scala 90:22:@3066.4]
  wire  decReg_io_deq_bits_rd_valid; // @[proc.scala 90:22:@3066.4]
  wire [4:0] decReg_io_deq_bits_rd_bits; // @[proc.scala 90:22:@3066.4]
  wire [4:0] decReg_io_deq_bits_rs1_bits; // @[proc.scala 90:22:@3066.4]
  wire  decReg_io_deq_bits_rs2_valid; // @[proc.scala 90:22:@3066.4]
  wire [4:0] decReg_io_deq_bits_rs2_bits; // @[proc.scala 90:22:@3066.4]
  wire [25:0] decReg_io_deq_bits_imm_bits; // @[proc.scala 90:22:@3066.4]
  wire  decReg_io_deq_bits_shift_val_valid; // @[proc.scala 90:22:@3066.4]
  wire [5:0] decReg_io_deq_bits_shift_val_bits; // @[proc.scala 90:22:@3066.4]
  wire [1:0] decReg_io_deq_bits_shift_type; // @[proc.scala 90:22:@3066.4]
  wire [3:0] decReg_io_deq_bits_cond_bits; // @[proc.scala 90:22:@3066.4]
  wire [2:0] decReg_io_deq_bits_itype; // @[proc.scala 90:22:@3066.4]
  wire [2:0] decReg_io_deq_bits_op; // @[proc.scala 90:22:@3066.4]
  wire  decReg_io_deq_bits_nzcv_en; // @[proc.scala 90:22:@3066.4]
  wire  decReg_io_deq_bits_tag; // @[proc.scala 90:22:@3066.4]
  wire [63:0] decReg_io_deq_bits_pc; // @[proc.scala 90:22:@3066.4]
  wire  decReg_io_flush; // @[proc.scala 90:22:@3066.4]
  wire  issuer_clock; // @[proc.scala 92:22:@3069.4]
  wire  issuer_reset; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_flush; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_flushTag; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_enq_ready; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_enq_valid; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_enq_bits_rd_valid; // @[proc.scala 92:22:@3069.4]
  wire [4:0] issuer_io_enq_bits_rd_bits; // @[proc.scala 92:22:@3069.4]
  wire [4:0] issuer_io_enq_bits_rs1_bits; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_enq_bits_rs2_valid; // @[proc.scala 92:22:@3069.4]
  wire [4:0] issuer_io_enq_bits_rs2_bits; // @[proc.scala 92:22:@3069.4]
  wire [25:0] issuer_io_enq_bits_imm_bits; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_enq_bits_shift_val_valid; // @[proc.scala 92:22:@3069.4]
  wire [5:0] issuer_io_enq_bits_shift_val_bits; // @[proc.scala 92:22:@3069.4]
  wire [1:0] issuer_io_enq_bits_shift_type; // @[proc.scala 92:22:@3069.4]
  wire [3:0] issuer_io_enq_bits_cond_bits; // @[proc.scala 92:22:@3069.4]
  wire [2:0] issuer_io_enq_bits_itype; // @[proc.scala 92:22:@3069.4]
  wire [2:0] issuer_io_enq_bits_op; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_enq_bits_nzcv_en; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_enq_bits_tag; // @[proc.scala 92:22:@3069.4]
  wire [63:0] issuer_io_enq_bits_pc; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_deq_valid; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_deq_bits_rd_valid; // @[proc.scala 92:22:@3069.4]
  wire [4:0] issuer_io_deq_bits_rd_bits; // @[proc.scala 92:22:@3069.4]
  wire [4:0] issuer_io_deq_bits_rs1_bits; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_deq_bits_rs2_valid; // @[proc.scala 92:22:@3069.4]
  wire [4:0] issuer_io_deq_bits_rs2_bits; // @[proc.scala 92:22:@3069.4]
  wire [25:0] issuer_io_deq_bits_imm_bits; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_deq_bits_shift_val_valid; // @[proc.scala 92:22:@3069.4]
  wire [5:0] issuer_io_deq_bits_shift_val_bits; // @[proc.scala 92:22:@3069.4]
  wire [1:0] issuer_io_deq_bits_shift_type; // @[proc.scala 92:22:@3069.4]
  wire [3:0] issuer_io_deq_bits_cond_bits; // @[proc.scala 92:22:@3069.4]
  wire [2:0] issuer_io_deq_bits_itype; // @[proc.scala 92:22:@3069.4]
  wire [2:0] issuer_io_deq_bits_op; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_deq_bits_nzcv_en; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_deq_bits_tag; // @[proc.scala 92:22:@3069.4]
  wire [63:0] issuer_io_deq_bits_pc; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_exeReg_valid; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_exeReg_bits_rd_valid; // @[proc.scala 92:22:@3069.4]
  wire [4:0] issuer_io_exeReg_bits_rd_bits; // @[proc.scala 92:22:@3069.4]
  wire  issuer_io_exeReg_bits_tag; // @[proc.scala 92:22:@3069.4]
  wire  executer_io_dinst_rd_valid; // @[proc.scala 96:24:@3072.4]
  wire [4:0] executer_io_dinst_rd_bits; // @[proc.scala 96:24:@3072.4]
  wire  executer_io_dinst_rs2_valid; // @[proc.scala 96:24:@3072.4]
  wire [25:0] executer_io_dinst_imm_bits; // @[proc.scala 96:24:@3072.4]
  wire  executer_io_dinst_shift_val_valid; // @[proc.scala 96:24:@3072.4]
  wire [5:0] executer_io_dinst_shift_val_bits; // @[proc.scala 96:24:@3072.4]
  wire [1:0] executer_io_dinst_shift_type; // @[proc.scala 96:24:@3072.4]
  wire [2:0] executer_io_dinst_itype; // @[proc.scala 96:24:@3072.4]
  wire [2:0] executer_io_dinst_op; // @[proc.scala 96:24:@3072.4]
  wire  executer_io_dinst_nzcv_en; // @[proc.scala 96:24:@3072.4]
  wire  executer_io_dinst_tag; // @[proc.scala 96:24:@3072.4]
  wire [63:0] executer_io_rVal1; // @[proc.scala 96:24:@3072.4]
  wire [63:0] executer_io_rVal2; // @[proc.scala 96:24:@3072.4]
  wire  executer_io_einst_valid; // @[proc.scala 96:24:@3072.4]
  wire  executer_io_einst_bits_rd_valid; // @[proc.scala 96:24:@3072.4]
  wire [4:0] executer_io_einst_bits_rd_bits; // @[proc.scala 96:24:@3072.4]
  wire  executer_io_einst_bits_nzcv_valid; // @[proc.scala 96:24:@3072.4]
  wire [3:0] executer_io_einst_bits_nzcv_bits; // @[proc.scala 96:24:@3072.4]
  wire  executer_io_einst_bits_tag; // @[proc.scala 96:24:@3072.4]
  wire [63:0] executer_io_einst_bits_res; // @[proc.scala 96:24:@3072.4]
  wire  exeReg_clock; // @[proc.scala 97:22:@3075.4]
  wire  exeReg_reset; // @[proc.scala 97:22:@3075.4]
  wire  exeReg_io_enq_valid; // @[proc.scala 97:22:@3075.4]
  wire  exeReg_io_enq_bits_rd_valid; // @[proc.scala 97:22:@3075.4]
  wire [4:0] exeReg_io_enq_bits_rd_bits; // @[proc.scala 97:22:@3075.4]
  wire  exeReg_io_enq_bits_nzcv_valid; // @[proc.scala 97:22:@3075.4]
  wire [3:0] exeReg_io_enq_bits_nzcv_bits; // @[proc.scala 97:22:@3075.4]
  wire  exeReg_io_enq_bits_tag; // @[proc.scala 97:22:@3075.4]
  wire [63:0] exeReg_io_enq_bits_res; // @[proc.scala 97:22:@3075.4]
  wire  exeReg_io_deq_valid; // @[proc.scala 97:22:@3075.4]
  wire  exeReg_io_deq_bits_rd_valid; // @[proc.scala 97:22:@3075.4]
  wire [4:0] exeReg_io_deq_bits_rd_bits; // @[proc.scala 97:22:@3075.4]
  wire  exeReg_io_deq_bits_nzcv_valid; // @[proc.scala 97:22:@3075.4]
  wire [3:0] exeReg_io_deq_bits_nzcv_bits; // @[proc.scala 97:22:@3075.4]
  wire  exeReg_io_deq_bits_tag; // @[proc.scala 97:22:@3075.4]
  wire [63:0] exeReg_io_deq_bits_res; // @[proc.scala 97:22:@3075.4]
  wire  exeReg_io_flush; // @[proc.scala 97:22:@3075.4]
  wire [25:0] brancher_io_dinst_imm_bits; // @[proc.scala 99:24:@3078.4]
  wire [3:0] brancher_io_dinst_cond_bits; // @[proc.scala 99:24:@3078.4]
  wire [2:0] brancher_io_dinst_itype; // @[proc.scala 99:24:@3078.4]
  wire [2:0] brancher_io_dinst_op; // @[proc.scala 99:24:@3078.4]
  wire  brancher_io_dinst_tag; // @[proc.scala 99:24:@3078.4]
  wire [3:0] brancher_io_nzcv; // @[proc.scala 99:24:@3078.4]
  wire  brancher_io_binst_valid; // @[proc.scala 99:24:@3078.4]
  wire [63:0] brancher_io_binst_bits_offset; // @[proc.scala 99:24:@3078.4]
  wire  brancher_io_binst_bits_tag; // @[proc.scala 99:24:@3078.4]
  wire  brReg_clock; // @[proc.scala 100:21:@3081.4]
  wire  brReg_reset; // @[proc.scala 100:21:@3081.4]
  wire  brReg_io_enq_valid; // @[proc.scala 100:21:@3081.4]
  wire [63:0] brReg_io_enq_bits_offset; // @[proc.scala 100:21:@3081.4]
  wire  brReg_io_enq_bits_tag; // @[proc.scala 100:21:@3081.4]
  wire  brReg_io_deq_valid; // @[proc.scala 100:21:@3081.4]
  wire [63:0] brReg_io_deq_bits_offset; // @[proc.scala 100:21:@3081.4]
  wire  brReg_io_deq_bits_tag; // @[proc.scala 100:21:@3081.4]
  wire  brReg_io_flush; // @[proc.scala 100:21:@3081.4]
  wire  ldstU_clock; // @[proc.scala 102:21:@3084.4]
  wire  ldstU_reset; // @[proc.scala 102:21:@3084.4]
  wire  ldstU_io_dinst_valid; // @[proc.scala 102:21:@3084.4]
  wire [25:0] ldstU_io_dinst_bits_imm_bits; // @[proc.scala 102:21:@3084.4]
  wire [2:0] ldstU_io_dinst_bits_itype; // @[proc.scala 102:21:@3084.4]
  wire [63:0] ldstU_io_dinst_bits_pc; // @[proc.scala 102:21:@3084.4]
  reg [63:0] vec_pregs_0_PC; // @[proc.scala 81:26:@3058.4]
  reg [63:0] _RAND_0;
  reg [31:0] vec_pregs_0_SP; // @[proc.scala 81:26:@3058.4]
  reg [31:0] _RAND_1;
  reg [31:0] vec_pregs_0_EL; // @[proc.scala 81:26:@3058.4]
  reg [31:0] _RAND_2;
  reg [3:0] vec_pregs_0_NZCV; // @[proc.scala 81:26:@3058.4]
  reg [31:0] _RAND_3;
  reg [63:0] vec_pregs_1_PC; // @[proc.scala 81:26:@3058.4]
  reg [63:0] _RAND_4;
  reg [31:0] vec_pregs_1_SP; // @[proc.scala 81:26:@3058.4]
  reg [31:0] _RAND_5;
  reg [31:0] vec_pregs_1_EL; // @[proc.scala 81:26:@3058.4]
  reg [31:0] _RAND_6;
  reg [3:0] vec_pregs_1_NZCV; // @[proc.scala 81:26:@3058.4]
  reg [31:0] _RAND_7;
  reg  fetch_en_0; // @[proc.scala 108:25:@3094.4]
  reg [31:0] _RAND_8;
  reg  fetch_en_1; // @[proc.scala 108:25:@3094.4]
  reg [31:0] _RAND_9;
  wire [4:0] _vec_rfile_tpu_io_tpu2cpu_freezeTag_rs1_addr; // @[proc.scala 289:41:@3412.6 proc.scala 289:41:@3412.6]
  wire [4:0] _GEN_125; // @[proc.scala 289:41:@3412.6]
  wire [63:0] vec_rfile_0_rs1_data; // @[proc.scala 80:26:@3034.4 proc.scala 80:26:@3040.4]
  wire [4:0] _GEN_123; // @[proc.scala 289:41:@3410.6]
  wire [63:0] vec_rfile_0_rs2_data; // @[proc.scala 80:26:@3034.4 proc.scala 80:26:@3038.4]
  wire [4:0] _vec_rfile_tpu_io_tpu2cpu_freezeTag_waddr; // @[proc.scala 289:41:@3408.6 proc.scala 289:41:@3408.6]
  wire [4:0] _GEN_26; // @[proc.scala 215:39:@3300.6]
  wire [4:0] _GEN_28; // @[proc.scala 212:31:@3295.4]
  wire [4:0] _GEN_107; // @[proc.scala 289:41:@3408.6]
  wire [63:0] _vec_rfile_tpu_io_tpu2cpu_freezeTag_wdata; // @[proc.scala 289:41:@3407.6 proc.scala 289:41:@3407.6]
  wire [63:0] _GEN_27; // @[proc.scala 215:39:@3300.6]
  wire [63:0] _GEN_29; // @[proc.scala 212:31:@3295.4]
  wire [63:0] _GEN_105; // @[proc.scala 289:41:@3407.6]
  wire  _vec_rfile_tpu_io_tpu2cpu_freezeTag_wen; // @[proc.scala 289:41:@3406.6 proc.scala 289:41:@3406.6]
  wire  _vec_rfile_exeReg_io_deq_bits_tag_wen; // @[proc.scala 225:43:@3324.6 proc.scala 225:43:@3324.6]
  wire  _GEN_30; // @[proc.scala 225:43:@3324.6]
  wire  _GEN_36; // @[proc.scala 224:29:@3323.4]
  wire  _GEN_103; // @[proc.scala 289:41:@3406.6]
  wire [4:0] _GEN_126; // @[proc.scala 289:41:@3412.6]
  wire [63:0] vec_rfile_1_rs1_data; // @[proc.scala 80:26:@3034.4 proc.scala 80:26:@3047.4]
  wire [4:0] _GEN_124; // @[proc.scala 289:41:@3410.6]
  wire [63:0] vec_rfile_1_rs2_data; // @[proc.scala 80:26:@3034.4 proc.scala 80:26:@3045.4]
  wire [4:0] _GEN_108; // @[proc.scala 289:41:@3408.6]
  wire [63:0] _GEN_106; // @[proc.scala 289:41:@3407.6]
  wire  _GEN_31; // @[proc.scala 225:43:@3324.6]
  wire  _GEN_37; // @[proc.scala 224:29:@3323.4]
  wire  _GEN_104; // @[proc.scala 289:41:@3406.6]
  wire  _T_302; // @[proc.scala 231:29:@3330.4]
  wire [3:0] _vec_pregs_exeReg_io_deq_bits_tag_NZCV; // @[proc.scala 232:44:@3332.6 proc.scala 232:44:@3332.6]
  wire [3:0] _GEN_38; // @[proc.scala 232:44:@3332.6]
  wire [3:0] _GEN_39; // @[proc.scala 232:44:@3332.6]
  wire [3:0] _GEN_40; // @[proc.scala 231:63:@3331.4]
  wire [3:0] _GEN_41; // @[proc.scala 231:63:@3331.4]
  wire [63:0] _GEN_46; // @[proc.scala 239:81:@3336.6]
  wire [64:0] _T_313; // @[proc.scala 239:81:@3336.6]
  wire [63:0] _T_314; // @[proc.scala 239:113:@3337.6]
  wire [64:0] _GEN_185; // @[proc.scala 239:86:@3338.6]
  wire [65:0] _T_315; // @[proc.scala 239:86:@3338.6]
  wire [64:0] _T_316; // @[proc.scala 239:86:@3339.6]
  wire [64:0] _T_317; // @[proc.scala 239:86:@3340.6]
  wire [64:0] _T_318; // @[proc.scala 239:127:@3341.6]
  wire [63:0] _vec_pregs_brReg_io_deq_bits_tag_PC; // @[proc.scala 239:41:@3342.6 proc.scala 239:41:@3342.6]
  wire [63:0] _GEN_50; // @[proc.scala 239:41:@3342.6]
  wire [63:0] _GEN_51; // @[proc.scala 239:41:@3342.6]
  wire [63:0] _GEN_56; // @[proc.scala 242:82:@3347.8]
  wire [64:0] _T_326; // @[proc.scala 242:82:@3347.8]
  wire [63:0] _T_327; // @[proc.scala 242:82:@3348.8]
  wire [63:0] _GEN_60; // @[proc.scala 242:42:@3349.8]
  wire [63:0] _GEN_61; // @[proc.scala 242:42:@3349.8]
  wire [63:0] _GEN_75; // @[proc.scala 241:35:@3346.6]
  wire [63:0] _GEN_76; // @[proc.scala 241:35:@3346.6]
  wire [63:0] _GEN_78; // @[proc.scala 238:28:@3335.4]
  wire [63:0] _GEN_79; // @[proc.scala 238:28:@3335.4]
  wire  _GEN_81; // @[proc.scala 251:38:@3360.6]
  wire  _GEN_82; // @[proc.scala 251:38:@3360.6]
  wire  _GEN_83; // @[proc.scala 250:29:@3359.4]
  wire  _GEN_84; // @[proc.scala 250:29:@3359.4]
  reg  undefINSN_bits; // @[proc.scala 254:28:@3362.4]
  reg [31:0] _RAND_10;
  wire  _T_343; // @[proc.scala 255:72:@3365.4]
  wire  _T_344; // @[proc.scala 255:50:@3366.4]
  reg  undefINSN_valid; // @[proc.scala 255:29:@3367.4]
  reg [31:0] _RAND_11;
  wire  _GEN_85; // @[proc.scala 259:39:@3375.6]
  wire  _GEN_86; // @[proc.scala 259:39:@3375.6]
  wire  _GEN_87; // @[proc.scala 258:82:@3374.4]
  wire  _GEN_88; // @[proc.scala 258:82:@3374.4]
  wire  _T_353; // @[proc.scala 266:45:@3379.6]
  wire  _T_354; // @[proc.scala 268:47:@3382.6]
  wire  _T_355; // @[proc.scala 269:45:@3384.6]
  wire  _T_356; // @[proc.scala 270:47:@3386.6]
  wire [63:0] _GEN_99; // @[proc.scala 283:23:@3399.4]
  wire [31:0] _GEN_100; // @[proc.scala 283:23:@3399.4]
  wire [31:0] _GEN_101; // @[proc.scala 283:23:@3399.4]
  wire [3:0] _GEN_102; // @[proc.scala 283:23:@3399.4]
  wire [63:0] _GEN_117; // @[proc.scala 289:41:@3409.6]
  wire [3:0] _GEN_127; // @[proc.scala 291:41:@3413.6]
  wire [3:0] _GEN_128; // @[proc.scala 291:41:@3413.6]
  wire [31:0] _GEN_129; // @[proc.scala 291:41:@3414.6]
  wire [31:0] _GEN_130; // @[proc.scala 291:41:@3414.6]
  wire [31:0] _GEN_131; // @[proc.scala 291:41:@3415.6]
  wire [31:0] _GEN_132; // @[proc.scala 291:41:@3415.6]
  wire [63:0] _GEN_133; // @[proc.scala 291:41:@3416.6]
  wire [63:0] _GEN_134; // @[proc.scala 291:41:@3416.6]
  wire  _T_388; // @[proc.scala 293:40:@3418.8]
  wire [63:0] _vec_pregs_tpu_io_tpu2cpu_freezeTag_PC_0; // @[proc.scala 294:48:@3420.10 proc.scala 294:48:@3420.10]
  wire [63:0] _GEN_135; // @[proc.scala 294:48:@3420.10]
  wire [63:0] _GEN_136; // @[proc.scala 294:48:@3420.10]
  wire  _T_392; // @[proc.scala 295:46:@3423.10]
  wire [31:0] _vec_pregs_tpu_io_tpu2cpu_freezeTag_SP_0; // @[proc.scala 296:48:@3425.12 proc.scala 296:48:@3425.12]
  wire [31:0] _GEN_137; // @[proc.scala 296:48:@3425.12]
  wire [31:0] _GEN_138; // @[proc.scala 296:48:@3425.12]
  wire [31:0] _vec_pregs_tpu_io_tpu2cpu_freezeTag_EL_0; // @[proc.scala 297:48:@3426.12 proc.scala 297:48:@3426.12]
  wire [31:0] _GEN_139; // @[proc.scala 297:48:@3426.12]
  wire [31:0] _GEN_140; // @[proc.scala 297:48:@3426.12]
  wire [3:0] _vec_pregs_tpu_io_tpu2cpu_freezeTag_NZCV_0; // @[proc.scala 298:50:@3427.12 proc.scala 298:50:@3427.12]
  wire [3:0] _GEN_141; // @[proc.scala 298:50:@3427.12]
  wire [3:0] _GEN_142; // @[proc.scala 298:50:@3427.12]
  wire [31:0] _GEN_143; // @[proc.scala 295:73:@3424.10]
  wire [31:0] _GEN_144; // @[proc.scala 295:73:@3424.10]
  wire [31:0] _GEN_145; // @[proc.scala 295:73:@3424.10]
  wire [31:0] _GEN_146; // @[proc.scala 295:73:@3424.10]
  wire [3:0] _GEN_147; // @[proc.scala 295:73:@3424.10]
  wire [3:0] _GEN_148; // @[proc.scala 295:73:@3424.10]
  wire [63:0] _GEN_149; // @[proc.scala 293:60:@3419.8]
  wire [63:0] _GEN_150; // @[proc.scala 293:60:@3419.8]
  wire [31:0] _GEN_151; // @[proc.scala 293:60:@3419.8]
  wire [31:0] _GEN_152; // @[proc.scala 293:60:@3419.8]
  wire [31:0] _GEN_153; // @[proc.scala 293:60:@3419.8]
  wire [31:0] _GEN_154; // @[proc.scala 293:60:@3419.8]
  wire [3:0] _GEN_155; // @[proc.scala 293:60:@3419.8]
  wire [3:0] _GEN_156; // @[proc.scala 293:60:@3419.8]
  wire [63:0] _GEN_157; // @[proc.scala 292:40:@3417.6]
  wire [63:0] _GEN_158; // @[proc.scala 292:40:@3417.6]
  wire [31:0] _GEN_159; // @[proc.scala 292:40:@3417.6]
  wire [31:0] _GEN_160; // @[proc.scala 292:40:@3417.6]
  wire [31:0] _GEN_161; // @[proc.scala 292:40:@3417.6]
  wire [31:0] _GEN_162; // @[proc.scala 292:40:@3417.6]
  wire [3:0] _GEN_163; // @[proc.scala 292:40:@3417.6]
  wire [3:0] _GEN_164; // @[proc.scala 292:40:@3417.6]
  wire [3:0] _GEN_177; // @[proc.scala 287:31:@3405.4]
  wire [3:0] _GEN_178; // @[proc.scala 287:31:@3405.4]
  wire [31:0] _GEN_179; // @[proc.scala 287:31:@3405.4]
  wire [31:0] _GEN_180; // @[proc.scala 287:31:@3405.4]
  wire [31:0] _GEN_181; // @[proc.scala 287:31:@3405.4]
  wire [31:0] _GEN_182; // @[proc.scala 287:31:@3405.4]
  wire [63:0] _GEN_183; // @[proc.scala 287:31:@3405.4]
  wire [63:0] _GEN_184; // @[proc.scala 287:31:@3405.4]
  BRAM ppage ( // @[proc.scala 71:21:@3009.4]
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
  BRAM state ( // @[proc.scala 73:21:@3012.4]
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
  TransplantUnit tpu ( // @[proc.scala 75:19:@3015.4]
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
  RFile RFile ( // @[proc.scala 80:58:@3028.4]
    .clock(RFile_clock),
    .io_rs1_addr(RFile_io_rs1_addr),
    .io_rs1_data(RFile_io_rs1_data),
    .io_rs2_addr(RFile_io_rs2_addr),
    .io_rs2_data(RFile_io_rs2_data),
    .io_waddr(RFile_io_waddr),
    .io_wdata(RFile_io_wdata),
    .io_wen(RFile_io_wen)
  );
  RFile RFile_1 ( // @[proc.scala 80:58:@3031.4]
    .clock(RFile_1_clock),
    .io_rs1_addr(RFile_1_io_rs1_addr),
    .io_rs1_data(RFile_1_io_rs1_data),
    .io_rs2_addr(RFile_1_io_rs2_addr),
    .io_rs2_data(RFile_1_io_rs2_data),
    .io_waddr(RFile_1_io_waddr),
    .io_wdata(RFile_1_io_wdata),
    .io_wen(RFile_1_io_wen)
  );
  FetchUnit fetch ( // @[proc.scala 86:21:@3060.4]
    .clock(fetch_clock),
    .reset(fetch_reset),
    .io_en(fetch_io_en),
    .io_vecPC_0(fetch_io_vecPC_0),
    .io_vecPC_1(fetch_io_vecPC_1),
    .io_tagIn(fetch_io_tagIn),
    .io_flush(fetch_io_flush),
    .io_fire_valid(fetch_io_fire_valid),
    .io_fire_bits(fetch_io_fire_bits),
    .io_branch_valid(fetch_io_branch_valid),
    .io_branch_bits_offset(fetch_io_branch_bits_offset),
    .io_branch_bits_tag(fetch_io_branch_bits_tag),
    .io_ppageBRAM_addr(fetch_io_ppageBRAM_addr),
    .io_ppageBRAM_dataOut(fetch_io_ppageBRAM_dataOut),
    .io_deq_ready(fetch_io_deq_ready),
    .io_deq_valid(fetch_io_deq_valid),
    .io_deq_bits_inst(fetch_io_deq_bits_inst),
    .io_deq_bits_tag(fetch_io_deq_bits_tag),
    .io_deq_bits_pc(fetch_io_deq_bits_pc)
  );
  DecodeUnit decoder ( // @[proc.scala 89:23:@3063.4]
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
  FReg decReg ( // @[proc.scala 90:22:@3066.4]
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
  IssueUnit issuer ( // @[proc.scala 92:22:@3069.4]
    .clock(issuer_clock),
    .reset(issuer_reset),
    .io_flush(issuer_io_flush),
    .io_flushTag(issuer_io_flushTag),
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
    .io_exeReg_valid(issuer_io_exeReg_valid),
    .io_exeReg_bits_rd_valid(issuer_io_exeReg_bits_rd_valid),
    .io_exeReg_bits_rd_bits(issuer_io_exeReg_bits_rd_bits),
    .io_exeReg_bits_tag(issuer_io_exeReg_bits_tag)
  );
  ExecuteUnit executer ( // @[proc.scala 96:24:@3072.4]
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
    .io_dinst_tag(executer_io_dinst_tag),
    .io_rVal1(executer_io_rVal1),
    .io_rVal2(executer_io_rVal2),
    .io_einst_valid(executer_io_einst_valid),
    .io_einst_bits_rd_valid(executer_io_einst_bits_rd_valid),
    .io_einst_bits_rd_bits(executer_io_einst_bits_rd_bits),
    .io_einst_bits_nzcv_valid(executer_io_einst_bits_nzcv_valid),
    .io_einst_bits_nzcv_bits(executer_io_einst_bits_nzcv_bits),
    .io_einst_bits_tag(executer_io_einst_bits_tag),
    .io_einst_bits_res(executer_io_einst_bits_res)
  );
  FReg_1 exeReg ( // @[proc.scala 97:22:@3075.4]
    .clock(exeReg_clock),
    .reset(exeReg_reset),
    .io_enq_valid(exeReg_io_enq_valid),
    .io_enq_bits_rd_valid(exeReg_io_enq_bits_rd_valid),
    .io_enq_bits_rd_bits(exeReg_io_enq_bits_rd_bits),
    .io_enq_bits_nzcv_valid(exeReg_io_enq_bits_nzcv_valid),
    .io_enq_bits_nzcv_bits(exeReg_io_enq_bits_nzcv_bits),
    .io_enq_bits_tag(exeReg_io_enq_bits_tag),
    .io_enq_bits_res(exeReg_io_enq_bits_res),
    .io_deq_valid(exeReg_io_deq_valid),
    .io_deq_bits_rd_valid(exeReg_io_deq_bits_rd_valid),
    .io_deq_bits_rd_bits(exeReg_io_deq_bits_rd_bits),
    .io_deq_bits_nzcv_valid(exeReg_io_deq_bits_nzcv_valid),
    .io_deq_bits_nzcv_bits(exeReg_io_deq_bits_nzcv_bits),
    .io_deq_bits_tag(exeReg_io_deq_bits_tag),
    .io_deq_bits_res(exeReg_io_deq_bits_res),
    .io_flush(exeReg_io_flush)
  );
  BranchUnit brancher ( // @[proc.scala 99:24:@3078.4]
    .io_dinst_imm_bits(brancher_io_dinst_imm_bits),
    .io_dinst_cond_bits(brancher_io_dinst_cond_bits),
    .io_dinst_itype(brancher_io_dinst_itype),
    .io_dinst_op(brancher_io_dinst_op),
    .io_dinst_tag(brancher_io_dinst_tag),
    .io_nzcv(brancher_io_nzcv),
    .io_binst_valid(brancher_io_binst_valid),
    .io_binst_bits_offset(brancher_io_binst_bits_offset),
    .io_binst_bits_tag(brancher_io_binst_bits_tag)
  );
  FReg_2 brReg ( // @[proc.scala 100:21:@3081.4]
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
  LoadStoreUnit ldstU ( // @[proc.scala 102:21:@3084.4]
    .clock(ldstU_clock),
    .reset(ldstU_reset),
    .io_dinst_valid(ldstU_io_dinst_valid),
    .io_dinst_bits_imm_bits(ldstU_io_dinst_bits_imm_bits),
    .io_dinst_bits_itype(ldstU_io_dinst_bits_itype),
    .io_dinst_bits_pc(ldstU_io_dinst_bits_pc)
  );
  assign _vec_rfile_tpu_io_tpu2cpu_freezeTag_rs1_addr = tpu_io_rfile_rs1_addr; // @[proc.scala 289:41:@3412.6 proc.scala 289:41:@3412.6]
  assign _GEN_125 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_rs1_addr : issuer_io_deq_bits_rs1_bits; // @[proc.scala 289:41:@3412.6]
  assign vec_rfile_0_rs1_data = RFile_io_rs1_data; // @[proc.scala 80:26:@3034.4 proc.scala 80:26:@3040.4]
  assign _GEN_123 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? 5'h0 : issuer_io_deq_bits_rs2_bits; // @[proc.scala 289:41:@3410.6]
  assign vec_rfile_0_rs2_data = RFile_io_rs2_data; // @[proc.scala 80:26:@3034.4 proc.scala 80:26:@3038.4]
  assign _vec_rfile_tpu_io_tpu2cpu_freezeTag_waddr = tpu_io_rfile_waddr; // @[proc.scala 289:41:@3408.6 proc.scala 289:41:@3408.6]
  assign _GEN_26 = exeReg_io_deq_bits_rd_bits; // @[proc.scala 215:39:@3300.6]
  assign _GEN_28 = exeReg_io_deq_valid ? exeReg_io_deq_bits_rd_bits : _GEN_26; // @[proc.scala 212:31:@3295.4]
  assign _GEN_107 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_waddr : _GEN_28; // @[proc.scala 289:41:@3408.6]
  assign _vec_rfile_tpu_io_tpu2cpu_freezeTag_wdata = tpu_io_rfile_wdata; // @[proc.scala 289:41:@3407.6 proc.scala 289:41:@3407.6]
  assign _GEN_27 = exeReg_io_deq_bits_res; // @[proc.scala 215:39:@3300.6]
  assign _GEN_29 = exeReg_io_deq_valid ? exeReg_io_deq_bits_res : _GEN_27; // @[proc.scala 212:31:@3295.4]
  assign _GEN_105 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_wdata : _GEN_29; // @[proc.scala 289:41:@3407.6]
  assign _vec_rfile_tpu_io_tpu2cpu_freezeTag_wen = tpu_io_rfile_wen; // @[proc.scala 289:41:@3406.6 proc.scala 289:41:@3406.6]
  assign _vec_rfile_exeReg_io_deq_bits_tag_wen = exeReg_io_deq_bits_rd_valid; // @[proc.scala 225:43:@3324.6 proc.scala 225:43:@3324.6]
  assign _GEN_30 = 1'h0 == exeReg_io_deq_bits_tag ? _vec_rfile_exeReg_io_deq_bits_tag_wen : 1'h0; // @[proc.scala 225:43:@3324.6]
  assign _GEN_36 = exeReg_io_deq_valid ? _GEN_30 : 1'h0; // @[proc.scala 224:29:@3323.4]
  assign _GEN_103 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_wen : _GEN_36; // @[proc.scala 289:41:@3406.6]
  assign _GEN_126 = tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_rs1_addr : issuer_io_deq_bits_rs1_bits; // @[proc.scala 289:41:@3412.6]
  assign vec_rfile_1_rs1_data = RFile_1_io_rs1_data; // @[proc.scala 80:26:@3034.4 proc.scala 80:26:@3047.4]
  assign _GEN_124 = tpu_io_tpu2cpu_freezeTag ? 5'h0 : issuer_io_deq_bits_rs2_bits; // @[proc.scala 289:41:@3410.6]
  assign vec_rfile_1_rs2_data = RFile_1_io_rs2_data; // @[proc.scala 80:26:@3034.4 proc.scala 80:26:@3045.4]
  assign _GEN_108 = tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_waddr : _GEN_28; // @[proc.scala 289:41:@3408.6]
  assign _GEN_106 = tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_wdata : _GEN_29; // @[proc.scala 289:41:@3407.6]
  assign _GEN_31 = exeReg_io_deq_bits_tag ? _vec_rfile_exeReg_io_deq_bits_tag_wen : 1'h0; // @[proc.scala 225:43:@3324.6]
  assign _GEN_37 = exeReg_io_deq_valid ? _GEN_31 : 1'h0; // @[proc.scala 224:29:@3323.4]
  assign _GEN_104 = tpu_io_tpu2cpu_freezeTag ? _vec_rfile_tpu_io_tpu2cpu_freezeTag_wen : _GEN_37; // @[proc.scala 289:41:@3406.6]
  assign _T_302 = exeReg_io_deq_valid & exeReg_io_deq_bits_nzcv_valid; // @[proc.scala 231:29:@3330.4]
  assign _vec_pregs_exeReg_io_deq_bits_tag_NZCV = exeReg_io_deq_bits_nzcv_bits; // @[proc.scala 232:44:@3332.6 proc.scala 232:44:@3332.6]
  assign _GEN_38 = 1'h0 == exeReg_io_deq_bits_tag ? _vec_pregs_exeReg_io_deq_bits_tag_NZCV : vec_pregs_0_NZCV; // @[proc.scala 232:44:@3332.6]
  assign _GEN_39 = exeReg_io_deq_bits_tag ? _vec_pregs_exeReg_io_deq_bits_tag_NZCV : vec_pregs_1_NZCV; // @[proc.scala 232:44:@3332.6]
  assign _GEN_40 = _T_302 ? _GEN_38 : vec_pregs_0_NZCV; // @[proc.scala 231:63:@3331.4]
  assign _GEN_41 = _T_302 ? _GEN_39 : vec_pregs_1_NZCV; // @[proc.scala 231:63:@3331.4]
  assign _GEN_46 = brReg_io_deq_bits_tag ? vec_pregs_1_PC : vec_pregs_0_PC; // @[proc.scala 239:81:@3336.6]
  assign _T_313 = {1'b0,$signed(_GEN_46)}; // @[proc.scala 239:81:@3336.6]
  assign _T_314 = $signed(brReg_io_deq_bits_offset); // @[proc.scala 239:113:@3337.6]
  assign _GEN_185 = {{1{_T_314[63]}},_T_314}; // @[proc.scala 239:86:@3338.6]
  assign _T_315 = $signed(_T_313) + $signed(_GEN_185); // @[proc.scala 239:86:@3338.6]
  assign _T_316 = $signed(_T_313) + $signed(_GEN_185); // @[proc.scala 239:86:@3339.6]
  assign _T_317 = $signed(_T_316); // @[proc.scala 239:86:@3340.6]
  assign _T_318 = $unsigned(_T_317); // @[proc.scala 239:127:@3341.6]
  assign _vec_pregs_brReg_io_deq_bits_tag_PC = _T_318[63:0]; // @[proc.scala 239:41:@3342.6 proc.scala 239:41:@3342.6]
  assign _GEN_50 = 1'h0 == brReg_io_deq_bits_tag ? _vec_pregs_brReg_io_deq_bits_tag_PC : vec_pregs_0_PC; // @[proc.scala 239:41:@3342.6]
  assign _GEN_51 = brReg_io_deq_bits_tag ? _vec_pregs_brReg_io_deq_bits_tag_PC : vec_pregs_1_PC; // @[proc.scala 239:41:@3342.6]
  assign _GEN_56 = exeReg_io_deq_bits_tag ? vec_pregs_1_PC : vec_pregs_0_PC; // @[proc.scala 242:82:@3347.8]
  assign _T_326 = _GEN_56 + 64'h4; // @[proc.scala 242:82:@3347.8]
  assign _T_327 = _GEN_56 + 64'h4; // @[proc.scala 242:82:@3348.8]
  assign _GEN_60 = 1'h0 == exeReg_io_deq_bits_tag ? _T_327 : vec_pregs_0_PC; // @[proc.scala 242:42:@3349.8]
  assign _GEN_61 = exeReg_io_deq_bits_tag ? _T_327 : vec_pregs_1_PC; // @[proc.scala 242:42:@3349.8]
  assign _GEN_75 = exeReg_io_deq_valid ? _GEN_60 : vec_pregs_0_PC; // @[proc.scala 241:35:@3346.6]
  assign _GEN_76 = exeReg_io_deq_valid ? _GEN_61 : vec_pregs_1_PC; // @[proc.scala 241:35:@3346.6]
  assign _GEN_78 = brReg_io_deq_valid ? _GEN_50 : _GEN_75; // @[proc.scala 238:28:@3335.4]
  assign _GEN_79 = brReg_io_deq_valid ? _GEN_51 : _GEN_76; // @[proc.scala 238:28:@3335.4]
  assign _GEN_81 = 1'h0 == tpu_io_tpu2cpu_fireTag ? 1'h1 : fetch_en_0; // @[proc.scala 251:38:@3360.6]
  assign _GEN_82 = tpu_io_tpu2cpu_fireTag ? 1'h1 : fetch_en_1; // @[proc.scala 251:38:@3360.6]
  assign _GEN_83 = tpu_io_tpu2cpu_fire ? _GEN_81 : fetch_en_0; // @[proc.scala 250:29:@3359.4]
  assign _GEN_84 = tpu_io_tpu2cpu_fire ? _GEN_82 : fetch_en_1; // @[proc.scala 250:29:@3359.4]
  assign _T_343 = issuer_io_deq_bits_itype == 3'h0; // @[proc.scala 255:72:@3365.4]
  assign _T_344 = issuer_io_deq_valid & _T_343; // @[proc.scala 255:50:@3366.4]
  assign _GEN_85 = 1'h0 == tpu_io_tpu2cpu_flushTag ? 1'h0 : _GEN_83; // @[proc.scala 259:39:@3375.6]
  assign _GEN_86 = tpu_io_tpu2cpu_flushTag ? 1'h0 : _GEN_84; // @[proc.scala 259:39:@3375.6]
  assign _GEN_87 = _T_344 ? _GEN_85 : _GEN_83; // @[proc.scala 258:82:@3374.4]
  assign _GEN_88 = _T_344 ? _GEN_86 : _GEN_84; // @[proc.scala 258:82:@3374.4]
  assign _T_353 = fetch_io_deq_bits_tag == tpu_io_tpu2cpu_flushTag; // @[proc.scala 266:45:@3379.6]
  assign _T_354 = decReg_io_deq_bits_tag == tpu_io_tpu2cpu_flushTag; // @[proc.scala 268:47:@3382.6]
  assign _T_355 = brReg_io_deq_bits_tag == tpu_io_tpu2cpu_flushTag; // @[proc.scala 269:45:@3384.6]
  assign _T_356 = exeReg_io_deq_bits_tag == tpu_io_tpu2cpu_flushTag; // @[proc.scala 270:47:@3386.6]
  assign _GEN_99 = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_PC : vec_pregs_0_PC; // @[proc.scala 283:23:@3399.4]
  assign _GEN_100 = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_SP : vec_pregs_0_SP; // @[proc.scala 283:23:@3399.4]
  assign _GEN_101 = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_EL : vec_pregs_0_EL; // @[proc.scala 283:23:@3399.4]
  assign _GEN_102 = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_NZCV : vec_pregs_0_NZCV; // @[proc.scala 283:23:@3399.4]
  assign _GEN_117 = tpu_io_tpu2cpu_freezeTag ? vec_rfile_1_rs1_data : vec_rfile_0_rs1_data; // @[proc.scala 289:41:@3409.6]
  assign _GEN_127 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _GEN_102 : _GEN_40; // @[proc.scala 291:41:@3413.6]
  assign _GEN_128 = tpu_io_tpu2cpu_freezeTag ? _GEN_102 : _GEN_41; // @[proc.scala 291:41:@3413.6]
  assign _GEN_129 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _GEN_101 : vec_pregs_0_EL; // @[proc.scala 291:41:@3414.6]
  assign _GEN_130 = tpu_io_tpu2cpu_freezeTag ? _GEN_101 : vec_pregs_1_EL; // @[proc.scala 291:41:@3414.6]
  assign _GEN_131 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _GEN_100 : vec_pregs_0_SP; // @[proc.scala 291:41:@3415.6]
  assign _GEN_132 = tpu_io_tpu2cpu_freezeTag ? _GEN_100 : vec_pregs_1_SP; // @[proc.scala 291:41:@3415.6]
  assign _GEN_133 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _GEN_99 : _GEN_78; // @[proc.scala 291:41:@3416.6]
  assign _GEN_134 = tpu_io_tpu2cpu_freezeTag ? _GEN_99 : _GEN_79; // @[proc.scala 291:41:@3416.6]
  assign _T_388 = tpu_io_tpu2cpuStateReg_bits == 2'h2; // @[proc.scala 293:40:@3418.8]
  assign _vec_pregs_tpu_io_tpu2cpu_freezeTag_PC_0 = tpu_io_tpu2cpuState_PC; // @[proc.scala 294:48:@3420.10 proc.scala 294:48:@3420.10]
  assign _GEN_135 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_PC_0 : _GEN_133; // @[proc.scala 294:48:@3420.10]
  assign _GEN_136 = tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_PC_0 : _GEN_134; // @[proc.scala 294:48:@3420.10]
  assign _T_392 = tpu_io_tpu2cpuStateReg_bits == 2'h3; // @[proc.scala 295:46:@3423.10]
  assign _vec_pregs_tpu_io_tpu2cpu_freezeTag_SP_0 = tpu_io_tpu2cpuState_SP; // @[proc.scala 296:48:@3425.12 proc.scala 296:48:@3425.12]
  assign _GEN_137 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_SP_0 : _GEN_131; // @[proc.scala 296:48:@3425.12]
  assign _GEN_138 = tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_SP_0 : _GEN_132; // @[proc.scala 296:48:@3425.12]
  assign _vec_pregs_tpu_io_tpu2cpu_freezeTag_EL_0 = tpu_io_tpu2cpuState_EL; // @[proc.scala 297:48:@3426.12 proc.scala 297:48:@3426.12]
  assign _GEN_139 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_EL_0 : _GEN_129; // @[proc.scala 297:48:@3426.12]
  assign _GEN_140 = tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_EL_0 : _GEN_130; // @[proc.scala 297:48:@3426.12]
  assign _vec_pregs_tpu_io_tpu2cpu_freezeTag_NZCV_0 = tpu_io_tpu2cpuState_NZCV; // @[proc.scala 298:50:@3427.12 proc.scala 298:50:@3427.12]
  assign _GEN_141 = 1'h0 == tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_NZCV_0 : _GEN_127; // @[proc.scala 298:50:@3427.12]
  assign _GEN_142 = tpu_io_tpu2cpu_freezeTag ? _vec_pregs_tpu_io_tpu2cpu_freezeTag_NZCV_0 : _GEN_128; // @[proc.scala 298:50:@3427.12]
  assign _GEN_143 = _T_392 ? _GEN_137 : _GEN_131; // @[proc.scala 295:73:@3424.10]
  assign _GEN_144 = _T_392 ? _GEN_138 : _GEN_132; // @[proc.scala 295:73:@3424.10]
  assign _GEN_145 = _T_392 ? _GEN_139 : _GEN_129; // @[proc.scala 295:73:@3424.10]
  assign _GEN_146 = _T_392 ? _GEN_140 : _GEN_130; // @[proc.scala 295:73:@3424.10]
  assign _GEN_147 = _T_392 ? _GEN_141 : _GEN_127; // @[proc.scala 295:73:@3424.10]
  assign _GEN_148 = _T_392 ? _GEN_142 : _GEN_128; // @[proc.scala 295:73:@3424.10]
  assign _GEN_149 = _T_388 ? _GEN_135 : _GEN_133; // @[proc.scala 293:60:@3419.8]
  assign _GEN_150 = _T_388 ? _GEN_136 : _GEN_134; // @[proc.scala 293:60:@3419.8]
  assign _GEN_151 = _T_388 ? _GEN_131 : _GEN_143; // @[proc.scala 293:60:@3419.8]
  assign _GEN_152 = _T_388 ? _GEN_132 : _GEN_144; // @[proc.scala 293:60:@3419.8]
  assign _GEN_153 = _T_388 ? _GEN_129 : _GEN_145; // @[proc.scala 293:60:@3419.8]
  assign _GEN_154 = _T_388 ? _GEN_130 : _GEN_146; // @[proc.scala 293:60:@3419.8]
  assign _GEN_155 = _T_388 ? _GEN_127 : _GEN_147; // @[proc.scala 293:60:@3419.8]
  assign _GEN_156 = _T_388 ? _GEN_128 : _GEN_148; // @[proc.scala 293:60:@3419.8]
  assign _GEN_157 = tpu_io_tpu2cpuStateReg_valid ? _GEN_149 : _GEN_133; // @[proc.scala 292:40:@3417.6]
  assign _GEN_158 = tpu_io_tpu2cpuStateReg_valid ? _GEN_150 : _GEN_134; // @[proc.scala 292:40:@3417.6]
  assign _GEN_159 = tpu_io_tpu2cpuStateReg_valid ? _GEN_151 : _GEN_131; // @[proc.scala 292:40:@3417.6]
  assign _GEN_160 = tpu_io_tpu2cpuStateReg_valid ? _GEN_152 : _GEN_132; // @[proc.scala 292:40:@3417.6]
  assign _GEN_161 = tpu_io_tpu2cpuStateReg_valid ? _GEN_153 : _GEN_129; // @[proc.scala 292:40:@3417.6]
  assign _GEN_162 = tpu_io_tpu2cpuStateReg_valid ? _GEN_154 : _GEN_130; // @[proc.scala 292:40:@3417.6]
  assign _GEN_163 = tpu_io_tpu2cpuStateReg_valid ? _GEN_155 : _GEN_127; // @[proc.scala 292:40:@3417.6]
  assign _GEN_164 = tpu_io_tpu2cpuStateReg_valid ? _GEN_156 : _GEN_128; // @[proc.scala 292:40:@3417.6]
  assign _GEN_177 = tpu_io_tpu2cpu_freeze ? _GEN_163 : _GEN_40; // @[proc.scala 287:31:@3405.4]
  assign _GEN_178 = tpu_io_tpu2cpu_freeze ? _GEN_164 : _GEN_41; // @[proc.scala 287:31:@3405.4]
  assign _GEN_179 = tpu_io_tpu2cpu_freeze ? _GEN_161 : vec_pregs_0_EL; // @[proc.scala 287:31:@3405.4]
  assign _GEN_180 = tpu_io_tpu2cpu_freeze ? _GEN_162 : vec_pregs_1_EL; // @[proc.scala 287:31:@3405.4]
  assign _GEN_181 = tpu_io_tpu2cpu_freeze ? _GEN_159 : vec_pregs_0_SP; // @[proc.scala 287:31:@3405.4]
  assign _GEN_182 = tpu_io_tpu2cpu_freeze ? _GEN_160 : vec_pregs_1_SP; // @[proc.scala 287:31:@3405.4]
  assign _GEN_183 = tpu_io_tpu2cpu_freeze ? _GEN_157 : _GEN_78; // @[proc.scala 287:31:@3405.4]
  assign _GEN_184 = tpu_io_tpu2cpu_freeze ? _GEN_158 : _GEN_79; // @[proc.scala 287:31:@3405.4]
  assign io_ppageBRAM_DO = ppage_io_portA_dataOut; // @[bram.scala 151:8:@3098.4]
  assign io_stateBRAM_DO = state_io_portA_dataOut; // @[bram.scala 151:8:@3103.4]
  assign io_host2tpu_done = tpu_io_host2tpu_done; // @[proc.scala 116:15:@3106.4]
  assign io_host2tpu_doneTag = tpu_io_host2tpu_doneTag; // @[proc.scala 116:15:@3105.4]
  assign ppage_clock = clock; // @[:@3010.4]
  assign ppage_io_portA_writeEn = io_ppageBRAM_WE; // @[bram.scala 148:8:@3095.4]
  assign ppage_io_portA_en = io_ppageBRAM_EN; // @[bram.scala 149:8:@3096.4]
  assign ppage_io_portA_addr = io_ppageBRAM_ADDR; // @[bram.scala 152:10:@3099.4]
  assign ppage_io_portA_dataIn = io_ppageBRAM_DI; // @[bram.scala 150:8:@3097.4]
  assign ppage_io_portB_writeEn = 1'h0; // @[proc.scala 131:22:@3128.4]
  assign ppage_io_portB_addr = fetch_io_ppageBRAM_addr; // @[proc.scala 131:22:@3126.4]
  assign ppage_io_portB_dataIn = 36'h0; // @[proc.scala 131:22:@3125.4]
  assign state_clock = clock; // @[:@3013.4]
  assign state_io_portA_writeEn = io_stateBRAM_WE; // @[bram.scala 148:8:@3100.4]
  assign state_io_portA_en = io_stateBRAM_EN; // @[bram.scala 149:8:@3101.4]
  assign state_io_portA_addr = io_stateBRAM_ADDR; // @[bram.scala 152:10:@3104.4]
  assign state_io_portA_dataIn = io_stateBRAM_DI; // @[bram.scala 150:8:@3102.4]
  assign state_io_portB_writeEn = tpu_io_stateBRAM_writeEn; // @[proc.scala 119:20:@3113.4]
  assign state_io_portB_addr = tpu_io_stateBRAM_addr; // @[proc.scala 119:20:@3111.4]
  assign state_io_portB_dataIn = tpu_io_stateBRAM_dataIn; // @[proc.scala 119:20:@3110.4]
  assign tpu_clock = clock; // @[:@3016.4]
  assign tpu_reset = reset; // @[:@3017.4]
  assign tpu_io_host2tpu_fire = io_host2tpu_fire; // @[proc.scala 116:15:@3108.4]
  assign tpu_io_host2tpu_fireTag = io_host2tpu_fireTag; // @[proc.scala 116:15:@3107.4]
  assign tpu_io_tpu2cpu_done = undefINSN_valid; // @[proc.scala 120:23:@3114.4 proc.scala 256:23:@3370.4]
  assign tpu_io_tpu2cpu_doneTag = undefINSN_bits; // @[proc.scala 121:26:@3115.4 proc.scala 257:26:@3371.4]
  assign tpu_io_cpu2tpuState_PC = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_PC : vec_pregs_0_PC; // @[proc.scala 283:23:@3402.4]
  assign tpu_io_cpu2tpuState_SP = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_SP : vec_pregs_0_SP; // @[proc.scala 283:23:@3401.4]
  assign tpu_io_cpu2tpuState_EL = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_EL : vec_pregs_0_EL; // @[proc.scala 283:23:@3400.4]
  assign tpu_io_cpu2tpuState_NZCV = tpu_io_tpu2cpu_freezeTag ? vec_pregs_1_NZCV : vec_pregs_0_NZCV; // @[proc.scala 283:23:@3399.4]
  assign tpu_io_rfile_rs1_data = tpu_io_tpu2cpu_freeze ? _GEN_117 : vec_rfile_0_rs1_data; // @[proc.scala 284:25:@3403.4 proc.scala 289:41:@3411.6]
  assign tpu_io_stateBRAM_dataOut = state_io_portB_dataOut; // @[proc.scala 119:20:@3109.4]
  assign RFile_clock = clock; // @[:@3029.4]
  assign RFile_io_rs1_addr = tpu_io_tpu2cpu_freeze ? _GEN_125 : issuer_io_deq_bits_rs1_bits; // @[proc.scala 80:26:@3041.4]
  assign RFile_io_rs2_addr = tpu_io_tpu2cpu_freeze ? _GEN_123 : issuer_io_deq_bits_rs2_bits; // @[proc.scala 80:26:@3039.4]
  assign RFile_io_waddr = tpu_io_tpu2cpu_freeze ? _GEN_107 : _GEN_28; // @[proc.scala 80:26:@3037.4]
  assign RFile_io_wdata = tpu_io_tpu2cpu_freeze ? _GEN_105 : _GEN_29; // @[proc.scala 80:26:@3036.4]
  assign RFile_io_wen = tpu_io_tpu2cpu_freeze ? _GEN_103 : _GEN_36; // @[proc.scala 80:26:@3035.4]
  assign RFile_1_clock = clock; // @[:@3032.4]
  assign RFile_1_io_rs1_addr = tpu_io_tpu2cpu_freeze ? _GEN_126 : issuer_io_deq_bits_rs1_bits; // @[proc.scala 80:26:@3048.4]
  assign RFile_1_io_rs2_addr = tpu_io_tpu2cpu_freeze ? _GEN_124 : issuer_io_deq_bits_rs2_bits; // @[proc.scala 80:26:@3046.4]
  assign RFile_1_io_waddr = tpu_io_tpu2cpu_freeze ? _GEN_108 : _GEN_28; // @[proc.scala 80:26:@3044.4]
  assign RFile_1_io_wdata = tpu_io_tpu2cpu_freeze ? _GEN_106 : _GEN_29; // @[proc.scala 80:26:@3043.4]
  assign RFile_1_io_wen = tpu_io_tpu2cpu_freeze ? _GEN_104 : _GEN_37; // @[proc.scala 80:26:@3042.4]
  assign fetch_clock = clock; // @[:@3061.4]
  assign fetch_reset = reset; // @[:@3062.4]
  assign fetch_io_en = fetch_en_1 ? fetch_en_1 : fetch_en_0; // @[proc.scala 135:15:@3134.4]
  assign fetch_io_vecPC_0 = vec_pregs_0_PC; // @[proc.scala 132:64:@3129.4]
  assign fetch_io_vecPC_1 = vec_pregs_1_PC; // @[proc.scala 132:64:@3130.4]
  assign fetch_io_tagIn = fetch_en_1; // @[proc.scala 134:18:@3131.4]
  assign fetch_io_flush = tpu_io_tpu2cpu_flush ? _T_353 : 1'h0; // @[proc.scala 266:20:@3380.6 proc.scala 273:20:@3392.6]
  assign fetch_io_fire_valid = tpu_io_tpu2cpu_fire; // @[proc.scala 138:23:@3138.4]
  assign fetch_io_fire_bits = tpu_io_tpu2cpu_fireTag; // @[proc.scala 139:22:@3139.4]
  assign fetch_io_branch_valid = brReg_io_deq_valid; // @[proc.scala 136:25:@3135.4]
  assign fetch_io_branch_bits_offset = brReg_io_deq_bits_offset; // @[proc.scala 137:24:@3137.4]
  assign fetch_io_branch_bits_tag = brReg_io_deq_bits_tag; // @[proc.scala 137:24:@3136.4]
  assign fetch_io_ppageBRAM_dataOut = ppage_io_portB_dataOut; // @[proc.scala 131:22:@3124.4]
  assign fetch_io_deq_ready = decReg_io_enq_ready; // @[proc.scala 145:22:@3163.4]
  assign decoder_io_finst_inst = fetch_io_deq_bits_inst; // @[proc.scala 142:20:@3142.4]
  assign decoder_io_finst_tag = fetch_io_deq_bits_tag; // @[proc.scala 142:20:@3141.4]
  assign decoder_io_finst_pc = fetch_io_deq_bits_pc; // @[proc.scala 142:20:@3140.4]
  assign decReg_clock = clock; // @[:@3067.4]
  assign decReg_reset = reset; // @[:@3068.4]
  assign decReg_io_enq_valid = fetch_io_deq_valid; // @[proc.scala 146:23:@3164.4]
  assign decReg_io_enq_bits_rd_valid = decoder_io_dinst_rd_valid; // @[proc.scala 143:22:@3162.4]
  assign decReg_io_enq_bits_rd_bits = decoder_io_dinst_rd_bits; // @[proc.scala 143:22:@3161.4]
  assign decReg_io_enq_bits_rs1_bits = decoder_io_dinst_rs1_bits; // @[proc.scala 143:22:@3159.4]
  assign decReg_io_enq_bits_rs2_valid = decoder_io_dinst_rs2_valid; // @[proc.scala 143:22:@3158.4]
  assign decReg_io_enq_bits_rs2_bits = decoder_io_dinst_rs2_bits; // @[proc.scala 143:22:@3157.4]
  assign decReg_io_enq_bits_imm_bits = decoder_io_dinst_imm_bits; // @[proc.scala 143:22:@3155.4]
  assign decReg_io_enq_bits_shift_val_valid = decoder_io_dinst_shift_val_valid; // @[proc.scala 143:22:@3154.4]
  assign decReg_io_enq_bits_shift_val_bits = decoder_io_dinst_shift_val_bits; // @[proc.scala 143:22:@3153.4]
  assign decReg_io_enq_bits_shift_type = decoder_io_dinst_shift_type; // @[proc.scala 143:22:@3152.4]
  assign decReg_io_enq_bits_cond_bits = decoder_io_dinst_cond_bits; // @[proc.scala 143:22:@3150.4]
  assign decReg_io_enq_bits_itype = decoder_io_dinst_itype; // @[proc.scala 143:22:@3149.4]
  assign decReg_io_enq_bits_op = decoder_io_dinst_op; // @[proc.scala 143:22:@3148.4]
  assign decReg_io_enq_bits_nzcv_en = decoder_io_dinst_nzcv_en; // @[proc.scala 143:22:@3147.4]
  assign decReg_io_enq_bits_tag = decoder_io_dinst_tag; // @[proc.scala 143:22:@3146.4]
  assign decReg_io_enq_bits_pc = decoder_io_dinst_pc; // @[proc.scala 143:22:@3143.4]
  assign decReg_io_deq_ready = issuer_io_enq_ready; // @[proc.scala 149:17:@3186.4]
  assign decReg_io_flush = tpu_io_tpu2cpu_flush ? _T_354 : 1'h0; // @[proc.scala 268:21:@3383.6 proc.scala 275:21:@3394.6]
  assign issuer_clock = clock; // @[:@3070.4]
  assign issuer_reset = reset; // @[:@3071.4]
  assign issuer_io_flush = tpu_io_tpu2cpu_flush ? tpu_io_tpu2cpu_flush : 1'h0; // @[proc.scala 267:21:@3381.6 proc.scala 274:21:@3393.6]
  assign issuer_io_flushTag = tpu_io_tpu2cpu_flushTag; // @[proc.scala 264:22:@3377.4]
  assign issuer_io_enq_valid = decReg_io_deq_valid; // @[proc.scala 149:17:@3185.4]
  assign issuer_io_enq_bits_rd_valid = decReg_io_deq_bits_rd_valid; // @[proc.scala 149:17:@3184.4]
  assign issuer_io_enq_bits_rd_bits = decReg_io_deq_bits_rd_bits; // @[proc.scala 149:17:@3183.4]
  assign issuer_io_enq_bits_rs1_bits = decReg_io_deq_bits_rs1_bits; // @[proc.scala 149:17:@3181.4]
  assign issuer_io_enq_bits_rs2_valid = decReg_io_deq_bits_rs2_valid; // @[proc.scala 149:17:@3180.4]
  assign issuer_io_enq_bits_rs2_bits = decReg_io_deq_bits_rs2_bits; // @[proc.scala 149:17:@3179.4]
  assign issuer_io_enq_bits_imm_bits = decReg_io_deq_bits_imm_bits; // @[proc.scala 149:17:@3177.4]
  assign issuer_io_enq_bits_shift_val_valid = decReg_io_deq_bits_shift_val_valid; // @[proc.scala 149:17:@3176.4]
  assign issuer_io_enq_bits_shift_val_bits = decReg_io_deq_bits_shift_val_bits; // @[proc.scala 149:17:@3175.4]
  assign issuer_io_enq_bits_shift_type = decReg_io_deq_bits_shift_type; // @[proc.scala 149:17:@3174.4]
  assign issuer_io_enq_bits_cond_bits = decReg_io_deq_bits_cond_bits; // @[proc.scala 149:17:@3172.4]
  assign issuer_io_enq_bits_itype = decReg_io_deq_bits_itype; // @[proc.scala 149:17:@3171.4]
  assign issuer_io_enq_bits_op = decReg_io_deq_bits_op; // @[proc.scala 149:17:@3170.4]
  assign issuer_io_enq_bits_nzcv_en = decReg_io_deq_bits_nzcv_en; // @[proc.scala 149:17:@3169.4]
  assign issuer_io_enq_bits_tag = decReg_io_deq_bits_tag; // @[proc.scala 149:17:@3168.4]
  assign issuer_io_enq_bits_pc = decReg_io_deq_bits_pc; // @[proc.scala 149:17:@3165.4]
  assign issuer_io_exeReg_valid = exeReg_io_deq_valid; // @[proc.scala 157:26:@3195.4]
  assign issuer_io_exeReg_bits_rd_valid = exeReg_io_deq_bits_rd_valid; // @[proc.scala 156:26:@3194.4]
  assign issuer_io_exeReg_bits_rd_bits = exeReg_io_deq_bits_rd_bits; // @[proc.scala 156:26:@3193.4]
  assign issuer_io_exeReg_bits_tag = exeReg_io_deq_bits_tag; // @[proc.scala 156:26:@3190.4]
  assign executer_io_dinst_rd_valid = issuer_io_deq_bits_rd_valid; // @[proc.scala 170:21:@3219.4]
  assign executer_io_dinst_rd_bits = issuer_io_deq_bits_rd_bits; // @[proc.scala 170:21:@3218.4]
  assign executer_io_dinst_rs2_valid = issuer_io_deq_bits_rs2_valid; // @[proc.scala 170:21:@3215.4]
  assign executer_io_dinst_imm_bits = issuer_io_deq_bits_imm_bits; // @[proc.scala 170:21:@3212.4]
  assign executer_io_dinst_shift_val_valid = issuer_io_deq_bits_shift_val_valid; // @[proc.scala 170:21:@3211.4]
  assign executer_io_dinst_shift_val_bits = issuer_io_deq_bits_shift_val_bits; // @[proc.scala 170:21:@3210.4]
  assign executer_io_dinst_shift_type = issuer_io_deq_bits_shift_type; // @[proc.scala 170:21:@3209.4]
  assign executer_io_dinst_itype = issuer_io_deq_bits_itype; // @[proc.scala 170:21:@3206.4]
  assign executer_io_dinst_op = issuer_io_deq_bits_op; // @[proc.scala 170:21:@3205.4]
  assign executer_io_dinst_nzcv_en = issuer_io_deq_bits_nzcv_en; // @[proc.scala 170:21:@3204.4]
  assign executer_io_dinst_tag = issuer_io_deq_bits_tag; // @[proc.scala 170:21:@3203.4]
  assign executer_io_rVal1 = issuer_io_deq_bits_tag ? vec_rfile_1_rs1_data : vec_rfile_0_rs1_data; // @[proc.scala 171:21:@3220.4]
  assign executer_io_rVal2 = issuer_io_deq_bits_tag ? vec_rfile_1_rs2_data : vec_rfile_0_rs2_data; // @[proc.scala 172:21:@3221.4]
  assign exeReg_clock = clock; // @[:@3076.4]
  assign exeReg_reset = reset; // @[:@3077.4]
  assign exeReg_io_enq_valid = executer_io_einst_valid & issuer_io_deq_valid; // @[proc.scala 174:23:@3224.4]
  assign exeReg_io_enq_bits_rd_valid = executer_io_einst_bits_rd_valid; // @[proc.scala 175:23:@3230.4]
  assign exeReg_io_enq_bits_rd_bits = executer_io_einst_bits_rd_bits; // @[proc.scala 175:23:@3229.4]
  assign exeReg_io_enq_bits_nzcv_valid = executer_io_einst_bits_nzcv_valid; // @[proc.scala 175:23:@3228.4]
  assign exeReg_io_enq_bits_nzcv_bits = executer_io_einst_bits_nzcv_bits; // @[proc.scala 175:23:@3227.4]
  assign exeReg_io_enq_bits_tag = executer_io_einst_bits_tag; // @[proc.scala 175:23:@3226.4]
  assign exeReg_io_enq_bits_res = executer_io_einst_bits_res; // @[proc.scala 175:23:@3225.4]
  assign exeReg_io_flush = tpu_io_tpu2cpu_flush ? _T_356 : 1'h0; // @[proc.scala 270:21:@3387.6 proc.scala 277:21:@3396.6]
  assign brancher_io_dinst_imm_bits = issuer_io_deq_bits_imm_bits; // @[proc.scala 178:21:@3243.4]
  assign brancher_io_dinst_cond_bits = issuer_io_deq_bits_cond_bits; // @[proc.scala 178:21:@3238.4]
  assign brancher_io_dinst_itype = issuer_io_deq_bits_itype; // @[proc.scala 178:21:@3237.4]
  assign brancher_io_dinst_op = issuer_io_deq_bits_op; // @[proc.scala 178:21:@3236.4]
  assign brancher_io_dinst_tag = issuer_io_deq_bits_tag; // @[proc.scala 178:21:@3234.4]
  assign brancher_io_nzcv = issuer_io_deq_bits_tag ? vec_pregs_1_NZCV : vec_pregs_0_NZCV; // @[proc.scala 179:20:@3251.4]
  assign brReg_clock = clock; // @[:@3082.4]
  assign brReg_reset = reset; // @[:@3083.4]
  assign brReg_io_enq_valid = issuer_io_deq_valid & brancher_io_binst_valid; // @[proc.scala 182:22:@3253.4]
  assign brReg_io_enq_bits_offset = brancher_io_binst_bits_offset; // @[proc.scala 183:28:@3254.4]
  assign brReg_io_enq_bits_tag = brancher_io_binst_bits_tag; // @[proc.scala 184:25:@3255.4]
  assign brReg_io_flush = tpu_io_tpu2cpu_flush ? _T_355 : 1'h0; // @[proc.scala 269:20:@3385.6 proc.scala 276:20:@3395.6]
  assign ldstU_clock = clock; // @[:@3085.4]
  assign ldstU_reset = reset; // @[:@3086.4]
  assign ldstU_io_dinst_valid = issuer_io_deq_valid; // @[proc.scala 188:24:@3276.4]
  assign ldstU_io_dinst_bits_imm_bits = issuer_io_deq_bits_imm_bits; // @[proc.scala 187:23:@3268.4]
  assign ldstU_io_dinst_bits_itype = issuer_io_deq_bits_itype; // @[proc.scala 187:23:@3262.4]
  assign ldstU_io_dinst_bits_pc = issuer_io_deq_bits_pc; // @[proc.scala 187:23:@3256.4]
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
  _RAND_8 = {1{`RANDOM}};
  fetch_en_0 = _RAND_8[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  fetch_en_1 = _RAND_9[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  undefINSN_bits = _RAND_10[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {1{`RANDOM}};
  undefINSN_valid = _RAND_11[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (reset) begin
      vec_pregs_0_PC <= 64'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_388) begin
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
                      vec_pregs_0_PC <= _T_327;
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
                    vec_pregs_0_PC <= _T_327;
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
                  vec_pregs_0_PC <= _T_327;
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
              vec_pregs_0_PC <= _T_327;
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
          if (_T_388) begin
            if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
              if (tpu_io_tpu2cpu_freezeTag) begin
                vec_pregs_0_SP <= vec_pregs_1_SP;
              end
            end
          end else begin
            if (_T_392) begin
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
          if (_T_388) begin
            if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
              if (tpu_io_tpu2cpu_freezeTag) begin
                vec_pregs_0_EL <= vec_pregs_1_EL;
              end
            end
          end else begin
            if (_T_392) begin
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
          if (_T_388) begin
            if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
              if (tpu_io_tpu2cpu_freezeTag) begin
                vec_pregs_0_NZCV <= vec_pregs_1_NZCV;
              end
            end else begin
              if (_T_302) begin
                if (1'h0 == exeReg_io_deq_bits_tag) begin
                  vec_pregs_0_NZCV <= _vec_pregs_exeReg_io_deq_bits_tag_NZCV;
                end
              end
            end
          end else begin
            if (_T_392) begin
              if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
                vec_pregs_0_NZCV <= _vec_pregs_tpu_io_tpu2cpu_freezeTag_NZCV_0;
              end else begin
                if (1'h0 == tpu_io_tpu2cpu_freezeTag) begin
                  if (tpu_io_tpu2cpu_freezeTag) begin
                    vec_pregs_0_NZCV <= vec_pregs_1_NZCV;
                  end
                end else begin
                  if (_T_302) begin
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
                if (_T_302) begin
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
            if (_T_302) begin
              if (1'h0 == exeReg_io_deq_bits_tag) begin
                vec_pregs_0_NZCV <= _vec_pregs_exeReg_io_deq_bits_tag_NZCV;
              end
            end
          end
        end
      end else begin
        vec_pregs_0_NZCV <= _GEN_40;
      end
    end
    if (reset) begin
      vec_pregs_1_PC <= 64'h0;
    end else begin
      if (tpu_io_tpu2cpu_freeze) begin
        if (tpu_io_tpu2cpuStateReg_valid) begin
          if (_T_388) begin
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
                      vec_pregs_1_PC <= _T_327;
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
                    vec_pregs_1_PC <= _T_327;
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
                  vec_pregs_1_PC <= _T_327;
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
              vec_pregs_1_PC <= _T_327;
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
          if (_T_388) begin
            if (tpu_io_tpu2cpu_freezeTag) begin
              if (!(tpu_io_tpu2cpu_freezeTag)) begin
                vec_pregs_1_SP <= vec_pregs_0_SP;
              end
            end
          end else begin
            if (_T_392) begin
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
          if (_T_388) begin
            if (tpu_io_tpu2cpu_freezeTag) begin
              if (!(tpu_io_tpu2cpu_freezeTag)) begin
                vec_pregs_1_EL <= vec_pregs_0_EL;
              end
            end
          end else begin
            if (_T_392) begin
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
          if (_T_388) begin
            if (tpu_io_tpu2cpu_freezeTag) begin
              if (!(tpu_io_tpu2cpu_freezeTag)) begin
                vec_pregs_1_NZCV <= vec_pregs_0_NZCV;
              end
            end else begin
              if (_T_302) begin
                if (exeReg_io_deq_bits_tag) begin
                  vec_pregs_1_NZCV <= _vec_pregs_exeReg_io_deq_bits_tag_NZCV;
                end
              end
            end
          end else begin
            if (_T_392) begin
              if (tpu_io_tpu2cpu_freezeTag) begin
                vec_pregs_1_NZCV <= _vec_pregs_tpu_io_tpu2cpu_freezeTag_NZCV_0;
              end else begin
                if (tpu_io_tpu2cpu_freezeTag) begin
                  if (!(tpu_io_tpu2cpu_freezeTag)) begin
                    vec_pregs_1_NZCV <= vec_pregs_0_NZCV;
                  end
                end else begin
                  if (_T_302) begin
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
                if (_T_302) begin
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
            if (_T_302) begin
              if (exeReg_io_deq_bits_tag) begin
                vec_pregs_1_NZCV <= _vec_pregs_exeReg_io_deq_bits_tag_NZCV;
              end
            end
          end
        end
      end else begin
        vec_pregs_1_NZCV <= _GEN_41;
      end
    end
    if (reset) begin
      fetch_en_0 <= 1'h0;
    end else begin
      if (_T_344) begin
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
      if (_T_344) begin
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
    undefINSN_bits <= issuer_io_deq_bits_tag;
    undefINSN_valid <= issuer_io_deq_valid & _T_343;
  end
endmodule
