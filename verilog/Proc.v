module Proc(
  input         clock,
  input         reset,
  input         io_memoryBRAM_EN,
  input  [7:0]  io_memoryBRAM_WE,
  input  [14:0] io_memoryBRAM_ADDR,
  input  [63:0] io_memoryBRAM_DI,
  output [63:0] io_memoryBRAM_DO,
  input         io_stateBRAM_EN,
  input  [7:0]  io_stateBRAM_WE,
  input  [8:0]  io_stateBRAM_ADDR,
  input  [63:0] io_stateBRAM_DI,
  output [63:0] io_stateBRAM_DO,
  input         io_host2tpu_fire_tag,
  input         io_host2tpu_fire_valid,
  output        io_host2tpu_done_tag,
  output        io_host2tpu_done_valid,
  output [63:0] io_host2tpu_missTLB_bits_vaddr,
  output [5:0]  io_host2tpu_missTLB_bits_tlbIdx,
  output        io_host2tpu_missTLB_valid,
  input         io_host2tpu_fillTLB_valid,
  input         io_host2tpu_fillTLB_bits_tlbEntry_wrEn,
  input  [51:0] io_host2tpu_fillTLB_bits_tlbEntry_tag,
  input  [63:0] io_host2tpu_fillTLB_bits_vaddr,
  input  [5:0]  io_host2tpu_fillTLB_bits_tlbIdx,
  input         io_host2tpu_getState_tag,
  input         io_host2tpu_getState_valid
);
  wire  memory_clock; // @[Proc.scala 67:22]
  wire  memory_reset; // @[Proc.scala 67:22]
  wire  memory_portA_EN; // @[Proc.scala 67:22]
  wire [7:0] memory_portA_WE; // @[Proc.scala 67:22]
  wire [14:0] memory_portA_ADDR; // @[Proc.scala 67:22]
  wire [63:0] memory_portA_DI; // @[Proc.scala 67:22]
  wire [63:0] memory_portA_DO; // @[Proc.scala 67:22]
  wire  memory_portB_EN; // @[Proc.scala 67:22]
  wire [7:0] memory_portB_WE; // @[Proc.scala 67:22]
  wire [14:0] memory_portB_ADDR; // @[Proc.scala 67:22]
  wire [63:0] memory_portB_DI; // @[Proc.scala 67:22]
  wire [63:0] memory_portB_DO; // @[Proc.scala 67:22]
  wire  state_clock; // @[Proc.scala 69:21]
  wire  state_reset; // @[Proc.scala 69:21]
  wire  state_portA_EN; // @[Proc.scala 69:21]
  wire [7:0] state_portA_WE; // @[Proc.scala 69:21]
  wire [8:0] state_portA_ADDR; // @[Proc.scala 69:21]
  wire [63:0] state_portA_DI; // @[Proc.scala 69:21]
  wire [63:0] state_portA_DO; // @[Proc.scala 69:21]
  wire [7:0] state_portB_WE; // @[Proc.scala 69:21]
  wire [8:0] state_portB_ADDR; // @[Proc.scala 69:21]
  wire [63:0] state_portB_DI; // @[Proc.scala 69:21]
  wire [63:0] state_portB_DO; // @[Proc.scala 69:21]
  wire  tpu_clock; // @[Proc.scala 71:19]
  wire  tpu_reset; // @[Proc.scala 71:19]
  wire  tpu_io_host2tpu_fire_tag; // @[Proc.scala 71:19]
  wire  tpu_io_host2tpu_fire_valid; // @[Proc.scala 71:19]
  wire  tpu_io_host2tpu_done_tag; // @[Proc.scala 71:19]
  wire  tpu_io_host2tpu_done_valid; // @[Proc.scala 71:19]
  wire [63:0] tpu_io_host2tpu_missTLB_bits_vaddr; // @[Proc.scala 71:19]
  wire [5:0] tpu_io_host2tpu_missTLB_bits_tlbIdx; // @[Proc.scala 71:19]
  wire  tpu_io_host2tpu_missTLB_valid; // @[Proc.scala 71:19]
  wire  tpu_io_host2tpu_fillTLB_valid; // @[Proc.scala 71:19]
  wire  tpu_io_host2tpu_fillTLB_bits_tlbEntry_wrEn; // @[Proc.scala 71:19]
  wire [51:0] tpu_io_host2tpu_fillTLB_bits_tlbEntry_tag; // @[Proc.scala 71:19]
  wire [63:0] tpu_io_host2tpu_fillTLB_bits_vaddr; // @[Proc.scala 71:19]
  wire [5:0] tpu_io_host2tpu_fillTLB_bits_tlbIdx; // @[Proc.scala 71:19]
  wire  tpu_io_host2tpu_getState_tag; // @[Proc.scala 71:19]
  wire  tpu_io_host2tpu_getState_valid; // @[Proc.scala 71:19]
  wire  tpu_io_tpu2cpu_flush_tag; // @[Proc.scala 71:19]
  wire  tpu_io_tpu2cpu_flush_valid; // @[Proc.scala 71:19]
  wire  tpu_io_tpu2cpu_fire_tag; // @[Proc.scala 71:19]
  wire  tpu_io_tpu2cpu_fire_valid; // @[Proc.scala 71:19]
  wire  tpu_io_tpu2cpu_freeze_tag; // @[Proc.scala 71:19]
  wire  tpu_io_tpu2cpu_freeze_valid; // @[Proc.scala 71:19]
  wire  tpu_io_tpu2cpu_done_tag; // @[Proc.scala 71:19]
  wire  tpu_io_tpu2cpu_done_valid; // @[Proc.scala 71:19]
  wire [63:0] tpu_io_tpu2cpu_missTLB_bits_vaddr; // @[Proc.scala 71:19]
  wire [5:0] tpu_io_tpu2cpu_missTLB_bits_tlbIdx; // @[Proc.scala 71:19]
  wire  tpu_io_tpu2cpu_missTLB_valid; // @[Proc.scala 71:19]
  wire  tpu_io_tpu2cpu_fillTLB_valid; // @[Proc.scala 71:19]
  wire  tpu_io_tpu2cpu_fillTLB_bits_tlbEntry_wrEn; // @[Proc.scala 71:19]
  wire [51:0] tpu_io_tpu2cpu_fillTLB_bits_tlbEntry_tag; // @[Proc.scala 71:19]
  wire [63:0] tpu_io_tpu2cpu_fillTLB_bits_vaddr; // @[Proc.scala 71:19]
  wire [5:0] tpu_io_tpu2cpu_fillTLB_bits_tlbIdx; // @[Proc.scala 71:19]
  wire  tpu_io_tpu2cpuStateReg_valid; // @[Proc.scala 71:19]
  wire [2:0] tpu_io_tpu2cpuStateReg_bits; // @[Proc.scala 71:19]
  wire [63:0] tpu_io_tpu2cpuState_PC; // @[Proc.scala 71:19]
  wire [63:0] tpu_io_tpu2cpuState_SP; // @[Proc.scala 71:19]
  wire [3:0] tpu_io_tpu2cpuState_NZCV; // @[Proc.scala 71:19]
  wire [63:0] tpu_io_cpu2tpuState_PC; // @[Proc.scala 71:19]
  wire [63:0] tpu_io_cpu2tpuState_SP; // @[Proc.scala 71:19]
  wire [3:0] tpu_io_cpu2tpuState_NZCV; // @[Proc.scala 71:19]
  wire [4:0] tpu_io_rfile_rs1_addr; // @[Proc.scala 71:19]
  wire [63:0] tpu_io_rfile_rs1_data; // @[Proc.scala 71:19]
  wire [4:0] tpu_io_rfile_w1_addr; // @[Proc.scala 71:19]
  wire [63:0] tpu_io_rfile_w1_data; // @[Proc.scala 71:19]
  wire  tpu_io_rfile_w1_en; // @[Proc.scala 71:19]
  wire [7:0] tpu_io_stateBRAM_WE; // @[Proc.scala 71:19]
  wire [8:0] tpu_io_stateBRAM_ADDR; // @[Proc.scala 71:19]
  wire [63:0] tpu_io_stateBRAM_DI; // @[Proc.scala 71:19]
  wire [63:0] tpu_io_stateBRAM_DO; // @[Proc.scala 71:19]
  wire  RFile_clock; // @[Proc.scala 75:57]
  wire [4:0] RFile_io_rs1_addr; // @[Proc.scala 75:57]
  wire [63:0] RFile_io_rs1_data; // @[Proc.scala 75:57]
  wire [4:0] RFile_io_rs2_addr; // @[Proc.scala 75:57]
  wire [63:0] RFile_io_rs2_data; // @[Proc.scala 75:57]
  wire [4:0] RFile_io_w1_addr; // @[Proc.scala 75:57]
  wire [63:0] RFile_io_w1_data; // @[Proc.scala 75:57]
  wire  RFile_io_w1_en; // @[Proc.scala 75:57]
  wire [4:0] RFile_io_rw_addr; // @[Proc.scala 75:57]
  wire [63:0] RFile_io_rw_di; // @[Proc.scala 75:57]
  wire  RFile_io_rw_wen; // @[Proc.scala 75:57]
  wire [63:0] RFile_io_rw_do; // @[Proc.scala 75:57]
  wire  RFile_1_clock; // @[Proc.scala 75:57]
  wire [4:0] RFile_1_io_rs1_addr; // @[Proc.scala 75:57]
  wire [63:0] RFile_1_io_rs1_data; // @[Proc.scala 75:57]
  wire [4:0] RFile_1_io_rs2_addr; // @[Proc.scala 75:57]
  wire [63:0] RFile_1_io_rs2_data; // @[Proc.scala 75:57]
  wire [4:0] RFile_1_io_w1_addr; // @[Proc.scala 75:57]
  wire [63:0] RFile_1_io_w1_data; // @[Proc.scala 75:57]
  wire  RFile_1_io_w1_en; // @[Proc.scala 75:57]
  wire [4:0] RFile_1_io_rw_addr; // @[Proc.scala 75:57]
  wire [63:0] RFile_1_io_rw_di; // @[Proc.scala 75:57]
  wire  RFile_1_io_rw_wen; // @[Proc.scala 75:57]
  wire [63:0] RFile_1_io_rw_do; // @[Proc.scala 75:57]
  wire  insnTLB_clock; // @[Proc.scala 79:23]
  wire  insnTLB_reset; // @[Proc.scala 79:23]
  wire  insnTLB_io_fillTLB_valid; // @[Proc.scala 79:23]
  wire  insnTLB_io_fillTLB_bits_tlbEntry_wrEn; // @[Proc.scala 79:23]
  wire [51:0] insnTLB_io_fillTLB_bits_tlbEntry_tag; // @[Proc.scala 79:23]
  wire [5:0] insnTLB_io_fillTLB_bits_tlbIdx; // @[Proc.scala 79:23]
  wire  insnTLB_io_iPort_vaddr_valid; // @[Proc.scala 79:23]
  wire [63:0] insnTLB_io_iPort_vaddr_bits; // @[Proc.scala 79:23]
  wire [63:0] insnTLB_io_iPort_paddr; // @[Proc.scala 79:23]
  wire  insnTLB_io_iPort_miss_valid; // @[Proc.scala 79:23]
  wire [63:0] insnTLB_io_iPort_miss_bits_vaddr; // @[Proc.scala 79:23]
  wire [5:0] insnTLB_io_iPort_miss_bits_tlbIdx; // @[Proc.scala 79:23]
  wire  insnTLB_io_dPort_vaddr_valid; // @[Proc.scala 79:23]
  wire [63:0] insnTLB_io_dPort_vaddr_bits; // @[Proc.scala 79:23]
  wire [63:0] insnTLB_io_dPort_paddr; // @[Proc.scala 79:23]
  wire  insnTLB_io_dPort_miss_valid; // @[Proc.scala 79:23]
  wire [63:0] insnTLB_io_dPort_miss_bits_vaddr; // @[Proc.scala 79:23]
  wire [5:0] insnTLB_io_dPort_miss_bits_tlbIdx; // @[Proc.scala 79:23]
  wire  fetch_clock; // @[Proc.scala 83:21]
  wire  fetch_reset; // @[Proc.scala 83:21]
  wire  fetch_io_flush_tag; // @[Proc.scala 83:21]
  wire  fetch_io_flush_valid; // @[Proc.scala 83:21]
  wire  fetch_io_fire_tag; // @[Proc.scala 83:21]
  wire  fetch_io_fire_valid; // @[Proc.scala 83:21]
  wire  fetch_io_commitReg_valid; // @[Proc.scala 83:21]
  wire  fetch_io_commitReg_bits_br_valid; // @[Proc.scala 83:21]
  wire  fetch_io_commitReg_bits_tag; // @[Proc.scala 83:21]
  wire [63:0] fetch_io_nextPC; // @[Proc.scala 83:21]
  wire  fetch_io_fetchEn_0; // @[Proc.scala 83:21]
  wire  fetch_io_fetchEn_1; // @[Proc.scala 83:21]
  wire [63:0] fetch_io_pcVec_0; // @[Proc.scala 83:21]
  wire [63:0] fetch_io_pcVec_1; // @[Proc.scala 83:21]
  wire  fetch_io_pc_tag; // @[Proc.scala 83:21]
  wire [63:0] fetch_io_pc_bits; // @[Proc.scala 83:21]
  wire  fetch_io_pc_valid; // @[Proc.scala 83:21]
  wire  fetch_io_hit; // @[Proc.scala 83:21]
  wire [31:0] fetch_io_insn; // @[Proc.scala 83:21]
  wire  fetch_io_deq_ready; // @[Proc.scala 83:21]
  wire  fetch_io_deq_valid; // @[Proc.scala 83:21]
  wire [31:0] fetch_io_deq_bits_inst; // @[Proc.scala 83:21]
  wire  fetch_io_deq_bits_tag; // @[Proc.scala 83:21]
  wire [63:0] fetch_io_deq_bits_pc; // @[Proc.scala 83:21]
  wire [31:0] decoder_io_finst_inst; // @[Proc.scala 86:23]
  wire  decoder_io_finst_tag; // @[Proc.scala 86:23]
  wire [63:0] decoder_io_finst_pc; // @[Proc.scala 86:23]
  wire  decoder_io_dinst_rd_valid; // @[Proc.scala 86:23]
  wire [4:0] decoder_io_dinst_rd_bits; // @[Proc.scala 86:23]
  wire [4:0] decoder_io_dinst_rs1; // @[Proc.scala 86:23]
  wire [4:0] decoder_io_dinst_rs2; // @[Proc.scala 86:23]
  wire [25:0] decoder_io_dinst_imm; // @[Proc.scala 86:23]
  wire  decoder_io_dinst_shift_val_valid; // @[Proc.scala 86:23]
  wire [5:0] decoder_io_dinst_shift_val_bits; // @[Proc.scala 86:23]
  wire [1:0] decoder_io_dinst_shift_type; // @[Proc.scala 86:23]
  wire  decoder_io_dinst_cond_valid; // @[Proc.scala 86:23]
  wire [3:0] decoder_io_dinst_cond_bits; // @[Proc.scala 86:23]
  wire  decoder_io_dinst_is32bit; // @[Proc.scala 86:23]
  wire [4:0] decoder_io_dinst_itype; // @[Proc.scala 86:23]
  wire [3:0] decoder_io_dinst_op; // @[Proc.scala 86:23]
  wire  decoder_io_dinst_nzcv_valid; // @[Proc.scala 86:23]
  wire [3:0] decoder_io_dinst_nzcv_bits; // @[Proc.scala 86:23]
  wire  decoder_io_dinst_tag; // @[Proc.scala 86:23]
  wire  decoder_io_dinst_inst32_valid; // @[Proc.scala 86:23]
  wire [31:0] decoder_io_dinst_inst32_bits; // @[Proc.scala 86:23]
  wire [63:0] decoder_io_dinst_pc; // @[Proc.scala 86:23]
  wire  decReg_clock; // @[Proc.scala 87:22]
  wire  decReg_reset; // @[Proc.scala 87:22]
  wire  decReg_io_enq_ready; // @[Proc.scala 87:22]
  wire  decReg_io_enq_valid; // @[Proc.scala 87:22]
  wire  decReg_io_enq_bits_rd_valid; // @[Proc.scala 87:22]
  wire [4:0] decReg_io_enq_bits_rd_bits; // @[Proc.scala 87:22]
  wire [4:0] decReg_io_enq_bits_rs1; // @[Proc.scala 87:22]
  wire [4:0] decReg_io_enq_bits_rs2; // @[Proc.scala 87:22]
  wire [25:0] decReg_io_enq_bits_imm; // @[Proc.scala 87:22]
  wire  decReg_io_enq_bits_shift_val_valid; // @[Proc.scala 87:22]
  wire [5:0] decReg_io_enq_bits_shift_val_bits; // @[Proc.scala 87:22]
  wire [1:0] decReg_io_enq_bits_shift_type; // @[Proc.scala 87:22]
  wire  decReg_io_enq_bits_cond_valid; // @[Proc.scala 87:22]
  wire [3:0] decReg_io_enq_bits_cond_bits; // @[Proc.scala 87:22]
  wire  decReg_io_enq_bits_is32bit; // @[Proc.scala 87:22]
  wire [4:0] decReg_io_enq_bits_itype; // @[Proc.scala 87:22]
  wire [3:0] decReg_io_enq_bits_op; // @[Proc.scala 87:22]
  wire  decReg_io_enq_bits_nzcv_valid; // @[Proc.scala 87:22]
  wire [3:0] decReg_io_enq_bits_nzcv_bits; // @[Proc.scala 87:22]
  wire  decReg_io_enq_bits_tag; // @[Proc.scala 87:22]
  wire  decReg_io_enq_bits_inst32_valid; // @[Proc.scala 87:22]
  wire [31:0] decReg_io_enq_bits_inst32_bits; // @[Proc.scala 87:22]
  wire [63:0] decReg_io_enq_bits_pc; // @[Proc.scala 87:22]
  wire  decReg_io_deq_ready; // @[Proc.scala 87:22]
  wire  decReg_io_deq_valid; // @[Proc.scala 87:22]
  wire  decReg_io_deq_bits_rd_valid; // @[Proc.scala 87:22]
  wire [4:0] decReg_io_deq_bits_rd_bits; // @[Proc.scala 87:22]
  wire [4:0] decReg_io_deq_bits_rs1; // @[Proc.scala 87:22]
  wire [4:0] decReg_io_deq_bits_rs2; // @[Proc.scala 87:22]
  wire [25:0] decReg_io_deq_bits_imm; // @[Proc.scala 87:22]
  wire  decReg_io_deq_bits_shift_val_valid; // @[Proc.scala 87:22]
  wire [5:0] decReg_io_deq_bits_shift_val_bits; // @[Proc.scala 87:22]
  wire [1:0] decReg_io_deq_bits_shift_type; // @[Proc.scala 87:22]
  wire  decReg_io_deq_bits_cond_valid; // @[Proc.scala 87:22]
  wire [3:0] decReg_io_deq_bits_cond_bits; // @[Proc.scala 87:22]
  wire  decReg_io_deq_bits_is32bit; // @[Proc.scala 87:22]
  wire [4:0] decReg_io_deq_bits_itype; // @[Proc.scala 87:22]
  wire [3:0] decReg_io_deq_bits_op; // @[Proc.scala 87:22]
  wire  decReg_io_deq_bits_nzcv_valid; // @[Proc.scala 87:22]
  wire [3:0] decReg_io_deq_bits_nzcv_bits; // @[Proc.scala 87:22]
  wire  decReg_io_deq_bits_tag; // @[Proc.scala 87:22]
  wire  decReg_io_deq_bits_inst32_valid; // @[Proc.scala 87:22]
  wire [31:0] decReg_io_deq_bits_inst32_bits; // @[Proc.scala 87:22]
  wire [63:0] decReg_io_deq_bits_pc; // @[Proc.scala 87:22]
  wire  decReg_io_flush; // @[Proc.scala 87:22]
  wire  issuer_clock; // @[Proc.scala 89:22]
  wire  issuer_reset; // @[Proc.scala 89:22]
  wire  issuer_io_enq_ready; // @[Proc.scala 89:22]
  wire  issuer_io_enq_valid; // @[Proc.scala 89:22]
  wire  issuer_io_enq_bits_rd_valid; // @[Proc.scala 89:22]
  wire [4:0] issuer_io_enq_bits_rd_bits; // @[Proc.scala 89:22]
  wire [4:0] issuer_io_enq_bits_rs1; // @[Proc.scala 89:22]
  wire [4:0] issuer_io_enq_bits_rs2; // @[Proc.scala 89:22]
  wire [25:0] issuer_io_enq_bits_imm; // @[Proc.scala 89:22]
  wire  issuer_io_enq_bits_shift_val_valid; // @[Proc.scala 89:22]
  wire [5:0] issuer_io_enq_bits_shift_val_bits; // @[Proc.scala 89:22]
  wire [1:0] issuer_io_enq_bits_shift_type; // @[Proc.scala 89:22]
  wire  issuer_io_enq_bits_cond_valid; // @[Proc.scala 89:22]
  wire [3:0] issuer_io_enq_bits_cond_bits; // @[Proc.scala 89:22]
  wire  issuer_io_enq_bits_is32bit; // @[Proc.scala 89:22]
  wire [4:0] issuer_io_enq_bits_itype; // @[Proc.scala 89:22]
  wire [3:0] issuer_io_enq_bits_op; // @[Proc.scala 89:22]
  wire  issuer_io_enq_bits_nzcv_valid; // @[Proc.scala 89:22]
  wire [3:0] issuer_io_enq_bits_nzcv_bits; // @[Proc.scala 89:22]
  wire  issuer_io_enq_bits_tag; // @[Proc.scala 89:22]
  wire  issuer_io_enq_bits_inst32_valid; // @[Proc.scala 89:22]
  wire [31:0] issuer_io_enq_bits_inst32_bits; // @[Proc.scala 89:22]
  wire [63:0] issuer_io_enq_bits_pc; // @[Proc.scala 89:22]
  wire  issuer_io_deq_ready; // @[Proc.scala 89:22]
  wire  issuer_io_deq_valid; // @[Proc.scala 89:22]
  wire  issuer_io_deq_bits_rd_valid; // @[Proc.scala 89:22]
  wire [4:0] issuer_io_deq_bits_rd_bits; // @[Proc.scala 89:22]
  wire [4:0] issuer_io_deq_bits_rs1; // @[Proc.scala 89:22]
  wire [4:0] issuer_io_deq_bits_rs2; // @[Proc.scala 89:22]
  wire [25:0] issuer_io_deq_bits_imm; // @[Proc.scala 89:22]
  wire  issuer_io_deq_bits_shift_val_valid; // @[Proc.scala 89:22]
  wire [5:0] issuer_io_deq_bits_shift_val_bits; // @[Proc.scala 89:22]
  wire [1:0] issuer_io_deq_bits_shift_type; // @[Proc.scala 89:22]
  wire [3:0] issuer_io_deq_bits_cond_bits; // @[Proc.scala 89:22]
  wire  issuer_io_deq_bits_is32bit; // @[Proc.scala 89:22]
  wire [4:0] issuer_io_deq_bits_itype; // @[Proc.scala 89:22]
  wire [3:0] issuer_io_deq_bits_op; // @[Proc.scala 89:22]
  wire  issuer_io_deq_bits_nzcv_valid; // @[Proc.scala 89:22]
  wire [3:0] issuer_io_deq_bits_nzcv_bits; // @[Proc.scala 89:22]
  wire  issuer_io_deq_bits_tag; // @[Proc.scala 89:22]
  wire  issuer_io_deq_bits_inst32_valid; // @[Proc.scala 89:22]
  wire  issuer_io_commitReg_valid; // @[Proc.scala 89:22]
  wire  issuer_io_commitReg_bits_tag; // @[Proc.scala 89:22]
  wire  issuer_io_flush_tag; // @[Proc.scala 89:22]
  wire  issuer_io_flush_valid; // @[Proc.scala 89:22]
  wire  executer_clock; // @[Proc.scala 97:24]
  wire  executer_reset; // @[Proc.scala 97:24]
  wire  executer_io_dinst_rd_valid; // @[Proc.scala 97:24]
  wire [4:0] executer_io_dinst_rd_bits; // @[Proc.scala 97:24]
  wire [4:0] executer_io_dinst_rs1; // @[Proc.scala 97:24]
  wire [4:0] executer_io_dinst_rs2; // @[Proc.scala 97:24]
  wire [25:0] executer_io_dinst_imm; // @[Proc.scala 97:24]
  wire  executer_io_dinst_shift_val_valid; // @[Proc.scala 97:24]
  wire [5:0] executer_io_dinst_shift_val_bits; // @[Proc.scala 97:24]
  wire [1:0] executer_io_dinst_shift_type; // @[Proc.scala 97:24]
  wire [3:0] executer_io_dinst_cond_bits; // @[Proc.scala 97:24]
  wire  executer_io_dinst_is32bit; // @[Proc.scala 97:24]
  wire [4:0] executer_io_dinst_itype; // @[Proc.scala 97:24]
  wire [3:0] executer_io_dinst_op; // @[Proc.scala 97:24]
  wire  executer_io_dinst_nzcv_valid; // @[Proc.scala 97:24]
  wire [3:0] executer_io_dinst_nzcv_bits; // @[Proc.scala 97:24]
  wire [63:0] executer_io_rVal1; // @[Proc.scala 97:24]
  wire [63:0] executer_io_rVal2; // @[Proc.scala 97:24]
  wire [63:0] executer_io_rVal3; // @[Proc.scala 97:24]
  wire [3:0] executer_io_nzcv; // @[Proc.scala 97:24]
  wire  executer_io_condRes; // @[Proc.scala 97:24]
  wire  executer_io_einst_valid; // @[Proc.scala 97:24]
  wire  executer_io_einst_bits_rd_valid; // @[Proc.scala 97:24]
  wire [4:0] executer_io_einst_bits_rd_bits; // @[Proc.scala 97:24]
  wire  executer_io_einst_bits_nzcv_valid; // @[Proc.scala 97:24]
  wire [3:0] executer_io_einst_bits_nzcv_bits; // @[Proc.scala 97:24]
  wire [63:0] executer_io_einst_bits_res; // @[Proc.scala 97:24]
  wire [4:0] brancher_io_dinst_rd_bits; // @[Proc.scala 98:24]
  wire [25:0] brancher_io_dinst_imm; // @[Proc.scala 98:24]
  wire  brancher_io_dinst_is32bit; // @[Proc.scala 98:24]
  wire [4:0] brancher_io_dinst_itype; // @[Proc.scala 98:24]
  wire [3:0] brancher_io_dinst_op; // @[Proc.scala 98:24]
  wire [63:0] brancher_io_rVal1; // @[Proc.scala 98:24]
  wire [63:0] brancher_io_rVal2; // @[Proc.scala 98:24]
  wire  brancher_io_cond; // @[Proc.scala 98:24]
  wire [63:0] brancher_io_pc; // @[Proc.scala 98:24]
  wire  brancher_io_binst_valid; // @[Proc.scala 98:24]
  wire [63:0] brancher_io_binst_bits_pc; // @[Proc.scala 98:24]
  wire  brancher_io_binst_bits_unalignedExcp; // @[Proc.scala 98:24]
  wire  brancher_io_pcrel_valid; // @[Proc.scala 98:24]
  wire [4:0] brancher_io_pcrel_bits_rd; // @[Proc.scala 98:24]
  wire [63:0] brancher_io_pcrel_bits_res; // @[Proc.scala 98:24]
  wire [4:0] ldstU_io_dinst_rd_bits; // @[Proc.scala 99:21]
  wire [4:0] ldstU_io_dinst_rs1; // @[Proc.scala 99:21]
  wire [4:0] ldstU_io_dinst_rs2; // @[Proc.scala 99:21]
  wire [25:0] ldstU_io_dinst_imm; // @[Proc.scala 99:21]
  wire  ldstU_io_dinst_shift_val_valid; // @[Proc.scala 99:21]
  wire [5:0] ldstU_io_dinst_shift_val_bits; // @[Proc.scala 99:21]
  wire [4:0] ldstU_io_dinst_itype; // @[Proc.scala 99:21]
  wire [3:0] ldstU_io_dinst_op; // @[Proc.scala 99:21]
  wire [63:0] ldstU_io_rVal1; // @[Proc.scala 99:21]
  wire [63:0] ldstU_io_rVal2; // @[Proc.scala 99:21]
  wire  ldstU_io_minst_valid; // @[Proc.scala 99:21]
  wire [1:0] ldstU_io_minst_bits_size; // @[Proc.scala 99:21]
  wire  ldstU_io_minst_bits_isPair; // @[Proc.scala 99:21]
  wire  ldstU_io_minst_bits_isLoad; // @[Proc.scala 99:21]
  wire [63:0] ldstU_io_minst_bits_memReq_0_addr; // @[Proc.scala 99:21]
  wire [4:0] ldstU_io_minst_bits_memReq_0_reg; // @[Proc.scala 99:21]
  wire [63:0] ldstU_io_minst_bits_memReq_1_addr; // @[Proc.scala 99:21]
  wire [4:0] ldstU_io_minst_bits_memReq_1_reg; // @[Proc.scala 99:21]
  wire [63:0] ldstU_io_minst_bits_rd_res; // @[Proc.scala 99:21]
  wire  ldstU_io_minst_bits_rd_valid; // @[Proc.scala 99:21]
  wire [4:0] ldstU_io_minst_bits_rd_bits; // @[Proc.scala 99:21]
  wire  ldstU_io_minst_bits_unalignedExcpSP; // @[Proc.scala 99:21]
  wire  commitReg_clock; // @[Proc.scala 100:25]
  wire  commitReg_reset; // @[Proc.scala 100:25]
  wire  commitReg_io_enq_ready; // @[Proc.scala 100:25]
  wire  commitReg_io_enq_valid; // @[Proc.scala 100:25]
  wire  commitReg_io_enq_bits_exe_valid; // @[Proc.scala 100:25]
  wire  commitReg_io_enq_bits_exe_bits_rd_valid; // @[Proc.scala 100:25]
  wire [4:0] commitReg_io_enq_bits_exe_bits_rd_bits; // @[Proc.scala 100:25]
  wire  commitReg_io_enq_bits_exe_bits_nzcv_valid; // @[Proc.scala 100:25]
  wire [3:0] commitReg_io_enq_bits_exe_bits_nzcv_bits; // @[Proc.scala 100:25]
  wire [63:0] commitReg_io_enq_bits_exe_bits_res; // @[Proc.scala 100:25]
  wire  commitReg_io_enq_bits_br_valid; // @[Proc.scala 100:25]
  wire [63:0] commitReg_io_enq_bits_br_bits_pc; // @[Proc.scala 100:25]
  wire  commitReg_io_enq_bits_br_bits_unalignedExcp; // @[Proc.scala 100:25]
  wire  commitReg_io_enq_bits_pcrel_valid; // @[Proc.scala 100:25]
  wire [4:0] commitReg_io_enq_bits_pcrel_bits_rd; // @[Proc.scala 100:25]
  wire [63:0] commitReg_io_enq_bits_pcrel_bits_res; // @[Proc.scala 100:25]
  wire  commitReg_io_enq_bits_mem_valid; // @[Proc.scala 100:25]
  wire [1:0] commitReg_io_enq_bits_mem_bits_size; // @[Proc.scala 100:25]
  wire  commitReg_io_enq_bits_mem_bits_isPair; // @[Proc.scala 100:25]
  wire  commitReg_io_enq_bits_mem_bits_isLoad; // @[Proc.scala 100:25]
  wire [63:0] commitReg_io_enq_bits_mem_bits_memReq_0_addr; // @[Proc.scala 100:25]
  wire [4:0] commitReg_io_enq_bits_mem_bits_memReq_0_reg; // @[Proc.scala 100:25]
  wire [63:0] commitReg_io_enq_bits_mem_bits_memReq_1_addr; // @[Proc.scala 100:25]
  wire [4:0] commitReg_io_enq_bits_mem_bits_memReq_1_reg; // @[Proc.scala 100:25]
  wire [63:0] commitReg_io_enq_bits_mem_bits_rd_res; // @[Proc.scala 100:25]
  wire  commitReg_io_enq_bits_mem_bits_rd_valid; // @[Proc.scala 100:25]
  wire [4:0] commitReg_io_enq_bits_mem_bits_rd_bits; // @[Proc.scala 100:25]
  wire  commitReg_io_enq_bits_mem_bits_unalignedExcpSP; // @[Proc.scala 100:25]
  wire  commitReg_io_enq_bits_undef; // @[Proc.scala 100:25]
  wire  commitReg_io_enq_bits_tag; // @[Proc.scala 100:25]
  wire  commitReg_io_deq_ready; // @[Proc.scala 100:25]
  wire  commitReg_io_deq_valid; // @[Proc.scala 100:25]
  wire  commitReg_io_deq_bits_exe_valid; // @[Proc.scala 100:25]
  wire  commitReg_io_deq_bits_exe_bits_rd_valid; // @[Proc.scala 100:25]
  wire [4:0] commitReg_io_deq_bits_exe_bits_rd_bits; // @[Proc.scala 100:25]
  wire  commitReg_io_deq_bits_exe_bits_nzcv_valid; // @[Proc.scala 100:25]
  wire [3:0] commitReg_io_deq_bits_exe_bits_nzcv_bits; // @[Proc.scala 100:25]
  wire [63:0] commitReg_io_deq_bits_exe_bits_res; // @[Proc.scala 100:25]
  wire  commitReg_io_deq_bits_br_valid; // @[Proc.scala 100:25]
  wire [63:0] commitReg_io_deq_bits_br_bits_pc; // @[Proc.scala 100:25]
  wire  commitReg_io_deq_bits_br_bits_unalignedExcp; // @[Proc.scala 100:25]
  wire  commitReg_io_deq_bits_pcrel_valid; // @[Proc.scala 100:25]
  wire [4:0] commitReg_io_deq_bits_pcrel_bits_rd; // @[Proc.scala 100:25]
  wire [63:0] commitReg_io_deq_bits_pcrel_bits_res; // @[Proc.scala 100:25]
  wire  commitReg_io_deq_bits_mem_valid; // @[Proc.scala 100:25]
  wire [1:0] commitReg_io_deq_bits_mem_bits_size; // @[Proc.scala 100:25]
  wire  commitReg_io_deq_bits_mem_bits_isPair; // @[Proc.scala 100:25]
  wire  commitReg_io_deq_bits_mem_bits_isLoad; // @[Proc.scala 100:25]
  wire [63:0] commitReg_io_deq_bits_mem_bits_memReq_0_addr; // @[Proc.scala 100:25]
  wire [4:0] commitReg_io_deq_bits_mem_bits_memReq_0_reg; // @[Proc.scala 100:25]
  wire [63:0] commitReg_io_deq_bits_mem_bits_memReq_1_addr; // @[Proc.scala 100:25]
  wire [4:0] commitReg_io_deq_bits_mem_bits_memReq_1_reg; // @[Proc.scala 100:25]
  wire [63:0] commitReg_io_deq_bits_mem_bits_rd_res; // @[Proc.scala 100:25]
  wire  commitReg_io_deq_bits_mem_bits_rd_valid; // @[Proc.scala 100:25]
  wire [4:0] commitReg_io_deq_bits_mem_bits_rd_bits; // @[Proc.scala 100:25]
  wire  commitReg_io_deq_bits_mem_bits_unalignedExcpSP; // @[Proc.scala 100:25]
  wire  commitReg_io_deq_bits_undef; // @[Proc.scala 100:25]
  wire  commitReg_io_deq_bits_tag; // @[Proc.scala 100:25]
  wire  commitReg_io_flush; // @[Proc.scala 100:25]
  wire  memArbiterInst_clock; // @[Proc.scala 118:30]
  wire  memArbiterInst_reset; // @[Proc.scala 118:30]
  wire  memArbiterInst_io_vaddr_valid; // @[Proc.scala 118:30]
  wire [63:0] memArbiterInst_io_vaddr_bits; // @[Proc.scala 118:30]
  wire  memArbiterInst_io_selHost; // @[Proc.scala 118:30]
  wire  memArbiterInst_io_selMem; // @[Proc.scala 118:30]
  wire  memArbiterInst_io_fillTLB_valid; // @[Proc.scala 118:30]
  wire [63:0] memArbiterInst_io_fillTLB_bits_vaddr; // @[Proc.scala 118:30]
  wire [14:0] memArbiterInst_io_memPort_ADDR; // @[Proc.scala 118:30]
  wire  memArbiterInst_io_tlbPort_vaddr_valid; // @[Proc.scala 118:30]
  wire [63:0] memArbiterInst_io_tlbPort_vaddr_bits; // @[Proc.scala 118:30]
  wire [63:0] memArbiterInst_io_tlbPort_paddr; // @[Proc.scala 118:30]
  wire  memArbiterInst_io_tlbPort_miss_valid; // @[Proc.scala 118:30]
  wire [63:0] memArbiterInst_io_tlbPort_miss_bits_vaddr; // @[Proc.scala 118:30]
  wire [5:0] memArbiterInst_io_tlbPort_miss_bits_tlbIdx; // @[Proc.scala 118:30]
  wire  memArbiterInst_io_reqMiss_valid; // @[Proc.scala 118:30]
  wire [63:0] memArbiterInst_io_reqMiss_bits_vaddr; // @[Proc.scala 118:30]
  wire [5:0] memArbiterInst_io_reqMiss_bits_tlbIdx; // @[Proc.scala 118:30]
  wire  memArbiterData_clock; // @[Proc.scala 119:30]
  wire  memArbiterData_reset; // @[Proc.scala 119:30]
  wire  memArbiterData_io_selHost; // @[Proc.scala 119:30]
  wire  memArbiterData_io_selMem; // @[Proc.scala 119:30]
  wire  memArbiterData_io_commitEnq_ready; // @[Proc.scala 119:30]
  wire  memArbiterData_io_commitEnq_valid; // @[Proc.scala 119:30]
  wire  memArbiterData_io_commitEnq_bits_mem_valid; // @[Proc.scala 119:30]
  wire [63:0] memArbiterData_io_commitEnq_bits_mem_bits_memReq_0_addr; // @[Proc.scala 119:30]
  wire [1:0] memArbiterData_io_commitDeq_bits_mem_bits_size; // @[Proc.scala 119:30]
  wire  memArbiterData_io_commitDeq_bits_mem_bits_isPair; // @[Proc.scala 119:30]
  wire  memArbiterData_io_commitDeq_bits_mem_bits_isLoad; // @[Proc.scala 119:30]
  wire [63:0] memArbiterData_io_commitDeq_bits_mem_bits_memReq_0_addr; // @[Proc.scala 119:30]
  wire [4:0] memArbiterData_io_commitDeq_bits_mem_bits_memReq_0_reg; // @[Proc.scala 119:30]
  wire [63:0] memArbiterData_io_commitDeq_bits_mem_bits_memReq_1_addr; // @[Proc.scala 119:30]
  wire [4:0] memArbiterData_io_commitDeq_bits_mem_bits_memReq_1_reg; // @[Proc.scala 119:30]
  wire [63:0] memArbiterData_io_commitDeq_bits_mem_bits_rd_res; // @[Proc.scala 119:30]
  wire  memArbiterData_io_commitDeq_bits_mem_bits_rd_valid; // @[Proc.scala 119:30]
  wire [4:0] memArbiterData_io_commitDeq_bits_mem_bits_rd_bits; // @[Proc.scala 119:30]
  wire  memArbiterData_io_fillTLB_valid; // @[Proc.scala 119:30]
  wire [63:0] memArbiterData_io_fillTLB_bits_vaddr; // @[Proc.scala 119:30]
  wire [7:0] memArbiterData_io_memPort_WE; // @[Proc.scala 119:30]
  wire [14:0] memArbiterData_io_memPort_ADDR; // @[Proc.scala 119:30]
  wire [63:0] memArbiterData_io_memPort_DI; // @[Proc.scala 119:30]
  wire [63:0] memArbiterData_io_memPort_DO; // @[Proc.scala 119:30]
  wire  memArbiterData_io_tlbPort_vaddr_valid; // @[Proc.scala 119:30]
  wire [63:0] memArbiterData_io_tlbPort_vaddr_bits; // @[Proc.scala 119:30]
  wire [63:0] memArbiterData_io_tlbPort_paddr; // @[Proc.scala 119:30]
  wire  memArbiterData_io_tlbPort_miss_valid; // @[Proc.scala 119:30]
  wire [63:0] memArbiterData_io_tlbPort_miss_bits_vaddr; // @[Proc.scala 119:30]
  wire [5:0] memArbiterData_io_tlbPort_miss_bits_tlbIdx; // @[Proc.scala 119:30]
  wire [4:0] memArbiterData_io_rfile_rs1_addr; // @[Proc.scala 119:30]
  wire [63:0] memArbiterData_io_rfile_rs1_data; // @[Proc.scala 119:30]
  wire [4:0] memArbiterData_io_rfile_w1_addr; // @[Proc.scala 119:30]
  wire [63:0] memArbiterData_io_rfile_w1_data; // @[Proc.scala 119:30]
  wire  memArbiterData_io_rfile_w1_en; // @[Proc.scala 119:30]
  wire [4:0] memArbiterData_io_rfile_rw_addr; // @[Proc.scala 119:30]
  wire [63:0] memArbiterData_io_rfile_rw_di; // @[Proc.scala 119:30]
  wire  memArbiterData_io_rfile_rw_wen; // @[Proc.scala 119:30]
  wire  memArbiterData_io_rfileWr; // @[Proc.scala 119:30]
  wire  memArbiterData_io_rfileRd; // @[Proc.scala 119:30]
  wire  memArbiterData_io_busy; // @[Proc.scala 119:30]
  wire [63:0] memArbiterData_io_reqMiss_bits_vaddr; // @[Proc.scala 119:30]
  wire [5:0] memArbiterData_io_reqMiss_bits_tlbIdx; // @[Proc.scala 119:30]
  wire  memArbiterData_io_reqMiss_valid; // @[Proc.scala 119:30]
  wire  memArbiterData_io_unalignedExcp; // @[Proc.scala 119:30]
  reg [63:0] pregsVec_0_PC; // @[Proc.scala 76:25]
  reg [63:0] _RAND_0;
  reg [63:0] pregsVec_0_SP; // @[Proc.scala 76:25]
  reg [63:0] _RAND_1;
  reg [3:0] pregsVec_0_NZCV; // @[Proc.scala 76:25]
  reg [31:0] _RAND_2;
  reg [63:0] pregsVec_1_PC; // @[Proc.scala 76:25]
  reg [63:0] _RAND_3;
  reg [63:0] pregsVec_1_SP; // @[Proc.scala 76:25]
  reg [63:0] _RAND_4;
  reg [3:0] pregsVec_1_NZCV; // @[Proc.scala 76:25]
  reg [31:0] _RAND_5;
  reg  fetchEn_0; // @[Proc.scala 104:24]
  reg [31:0] _RAND_6;
  reg  fetchEn_1; // @[Proc.scala 104:24]
  reg [31:0] _RAND_7;
  wire  _GEN_0; // @[Proc.scala 150:46]
  wire [5:0] _GEN_2; // @[Proc.scala 150:46]
  wire [63:0] _GEN_3; // @[Proc.scala 150:46]
  wire  _GEN_8; // @[Proc.scala 163:41]
  wire [7:0] _GEN_9; // @[Proc.scala 163:41]
  wire [63:0] _GEN_10; // @[Proc.scala 163:41]
  wire [14:0] _GEN_11; // @[Proc.scala 163:41]
  reg  sel32bit; // @[Proc.scala 167:25]
  reg [31:0] _RAND_8;
  wire [4:0] _T_14; // @[Proc.scala 193:26]
  wire [4:0] _T_15; // @[Proc.scala 194:26]
  wire [4:0] _rfileVec_tpu_io_tpu2cpu_freeze_tag_rs1_addr; // @[Proc.scala 357:41 Proc.scala 357:41]
  wire [4:0] _GEN_220; // @[Proc.scala 357:41]
  wire [63:0] rfileVec_0_rs1_data; // @[Proc.scala 75:25 Proc.scala 75:25]
  wire [4:0] _GEN_218; // @[Proc.scala 357:41]
  wire [63:0] rfileVec_0_rs2_data; // @[Proc.scala 75:25 Proc.scala 75:25]
  wire [4:0] _rfileVec_tpu_io_tpu2cpu_freeze_tag_w1_addr; // @[Proc.scala 357:41 Proc.scala 357:41]
  wire  commitExec_valid;
  wire [4:0] commitExec_bits_rd_bits;
  wire  commitPcRel_valid;
  wire [4:0] commitPcRel_bits_rd;
  wire [4:0] _GEN_89; // @[Proc.scala 264:42]
  wire [4:0] _GEN_91; // @[Proc.scala 261:35]
  wire [4:0] _GEN_93; // @[Proc.scala 258:28]
  wire [4:0] _GEN_216; // @[Proc.scala 357:41]
  wire [63:0] _rfileVec_tpu_io_tpu2cpu_freeze_tag_w1_data; // @[Proc.scala 357:41 Proc.scala 357:41]
  wire [63:0] commitExec_bits_res;
  wire [63:0] commitPcRel_bits_res;
  wire [63:0] _GEN_90; // @[Proc.scala 264:42]
  wire [63:0] _GEN_92; // @[Proc.scala 261:35]
  wire [63:0] _GEN_94; // @[Proc.scala 258:28]
  wire [63:0] _GEN_214; // @[Proc.scala 357:41]
  wire  _rfileVec_tpu_io_tpu2cpu_freeze_tag_w1_en; // @[Proc.scala 357:41 Proc.scala 357:41]
  wire  commitMem_valid;
  wire  unalignedExcpData; // @[Proc.scala 247:52]
  wire  commitBr_valid;
  wire  commitBr_bits_unalignedExcp;
  wire  unalignedExcpBranch; // @[Proc.scala 249:53]
  wire  _T_28; // @[Proc.scala 251:73]
  wire  commitMem_bits_unalignedExcpSP;
  wire  unalignedExcpSP; // @[Proc.scala 248:50]
  wire  _T_29; // @[Proc.scala 251:96]
  wire  exception; // @[Proc.scala 251:51]
  wire  _T_59; // @[Proc.scala 302:37]
  wire  _T_60; // @[Proc.scala 302:34]
  wire  commitTag;
  wire  _rfileVec_T_62_w1_en; // @[Proc.scala 303:31 Proc.scala 303:31]
  wire  commited; // @[Decoupled.scala 40:37]
  wire  _T_41; // @[Proc.scala 285:17]
  wire  commitUndef; // @[Proc.scala 245:53]
  wire  _T_42; // @[Proc.scala 285:34]
  wire  _T_43; // @[Proc.scala 285:31]
  wire  commitExec_bits_rd_valid;
  wire  _T_46; // @[Proc.scala 286:52]
  wire  _T_47; // @[Proc.scala 286:81]
  wire  _GEN_277; // @[Proc.scala 286:31]
  wire  _GEN_107; // @[Proc.scala 286:31]
  wire  _GEN_124; // @[Proc.scala 285:48]
  wire  _GEN_133; // @[Proc.scala 303:31]
  wire  _GEN_137; // @[Proc.scala 302:49]
  wire  _GEN_212; // @[Proc.scala 357:41]
  wire [4:0] _T_33; // @[Proc.scala 275:33]
  wire [4:0] _GEN_210; // @[Proc.scala 357:41]
  wire [63:0] _GEN_208; // @[Proc.scala 357:41]
  wire  _rfileVec_T_64_rw_wen; // @[Proc.scala 304:32 Proc.scala 304:32]
  wire  _GEN_135; // @[Proc.scala 304:32]
  wire  _GEN_139; // @[Proc.scala 302:49]
  wire  _GEN_206; // @[Proc.scala 357:41]
  wire [63:0] rfileVec_0_rw_do; // @[Proc.scala 75:25 Proc.scala 75:25]
  wire [4:0] _GEN_221; // @[Proc.scala 357:41]
  wire [63:0] rfileVec_1_rs1_data; // @[Proc.scala 75:25 Proc.scala 75:25]
  wire [4:0] _GEN_219; // @[Proc.scala 357:41]
  wire [63:0] rfileVec_1_rs2_data; // @[Proc.scala 75:25 Proc.scala 75:25]
  wire [4:0] _GEN_217; // @[Proc.scala 357:41]
  wire [63:0] _GEN_215; // @[Proc.scala 357:41]
  wire  _GEN_108; // @[Proc.scala 286:31]
  wire  _GEN_125; // @[Proc.scala 285:48]
  wire  _GEN_134; // @[Proc.scala 303:31]
  wire  _GEN_138; // @[Proc.scala 302:49]
  wire  _GEN_213; // @[Proc.scala 357:41]
  wire [4:0] _GEN_211; // @[Proc.scala 357:41]
  wire [63:0] _GEN_209; // @[Proc.scala 357:41]
  wire  _GEN_136; // @[Proc.scala 304:32]
  wire  _GEN_140; // @[Proc.scala 302:49]
  wire  _GEN_207; // @[Proc.scala 357:41]
  wire [63:0] rfileVec_1_rw_do; // @[Proc.scala 75:25 Proc.scala 75:25]
  wire [63:0] _GEN_98; // @[Proc.scala 283:10]
  wire  commitExec_bits_nzcv_valid;
  wire  _T_50; // @[Proc.scala 289:27]
  wire [3:0] commitExec_bits_nzcv_bits;
  wire [3:0] _GEN_111; // @[Proc.scala 290:32]
  wire [3:0] _GEN_112; // @[Proc.scala 290:32]
  wire [3:0] _GEN_113; // @[Proc.scala 289:58]
  wire [3:0] _GEN_114; // @[Proc.scala 289:58]
  wire [63:0] _T_56; // @[Proc.scala 296:40]
  wire [63:0] commitBr_bits_pc;
  wire [63:0] _GEN_121; // @[Proc.scala 293:26]
  wire [63:0] nextPC; // @[Proc.scala 285:48]
  wire [3:0] _GEN_128; // @[Proc.scala 285:48]
  wire [3:0] _GEN_129; // @[Proc.scala 285:48]
  wire  _GEN_141; // @[Proc.scala 314:41]
  wire [7:0] _GEN_142; // @[Proc.scala 314:41]
  wire [63:0] _GEN_143; // @[Proc.scala 314:41]
  wire [63:0] _GEN_144; // @[Proc.scala 314:41]
  wire [14:0] _GEN_145; // @[Proc.scala 314:41]
  wire  _GEN_152; // @[Proc.scala 320:65]
  wire  _GEN_153; // @[Proc.scala 320:65]
  wire  _GEN_154; // @[Proc.scala 320:38]
  wire  _GEN_155; // @[Proc.scala 320:38]
  wire  _GEN_156; // @[Proc.scala 321:38]
  wire  _GEN_157; // @[Proc.scala 322:75]
  wire  _GEN_158; // @[Proc.scala 322:75]
  wire  _GEN_159; // @[Proc.scala 322:38]
  wire  _GEN_160; // @[Proc.scala 322:38]
  wire  _GEN_279; // @[Proc.scala 323:73]
  wire  _GEN_161; // @[Proc.scala 323:73]
  wire  _GEN_162; // @[Proc.scala 323:73]
  wire  _T_66; // @[Proc.scala 338:47]
  wire  _T_67; // @[Proc.scala 339:53]
  wire  _T_68; // @[Proc.scala 340:23]
  wire  _T_69; // @[Proc.scala 341:51]
  wire  _T_70; // @[Proc.scala 345:47]
  wire  _GEN_169; // @[Proc.scala 340:43]
  wire  _GEN_170; // @[Proc.scala 340:43]
  wire  _GEN_171; // @[Proc.scala 340:43]
  wire  _GEN_172; // @[Proc.scala 340:43]
  wire [63:0] _GEN_181; // @[Proc.scala 350:23]
  wire [63:0] _GEN_182; // @[Proc.scala 350:23]
  wire [3:0] _GEN_183; // @[Proc.scala 350:23]
  wire [3:0] _GEN_222; // @[Proc.scala 359:41]
  wire [3:0] _GEN_223; // @[Proc.scala 359:41]
  wire  _T_71; // @[Proc.scala 361:40]
  wire [63:0] _pregsVec_tpu_io_tpu2cpu_freeze_tag_PC_0; // @[Proc.scala 362:48 Proc.scala 362:48]
  wire  _T_72; // @[Proc.scala 363:46]
  wire [63:0] _pregsVec_tpu_io_tpu2cpu_freeze_tag_SP_0; // @[Proc.scala 364:48 Proc.scala 364:48]
  wire  _T_73; // @[Proc.scala 365:46]
  wire [3:0] _pregsVec_tpu_io_tpu2cpu_freeze_tag_NZCV_0; // @[Proc.scala 366:50 Proc.scala 366:50]
  BRAM memory ( // @[Proc.scala 67:22]
    .clock(memory_clock),
    .reset(memory_reset),
    .portA_EN(memory_portA_EN),
    .portA_WE(memory_portA_WE),
    .portA_ADDR(memory_portA_ADDR),
    .portA_DI(memory_portA_DI),
    .portA_DO(memory_portA_DO),
    .portB_EN(memory_portB_EN),
    .portB_WE(memory_portB_WE),
    .portB_ADDR(memory_portB_ADDR),
    .portB_DI(memory_portB_DI),
    .portB_DO(memory_portB_DO)
  );
  BRAM_1 state ( // @[Proc.scala 69:21]
    .clock(state_clock),
    .reset(state_reset),
    .portA_EN(state_portA_EN),
    .portA_WE(state_portA_WE),
    .portA_ADDR(state_portA_ADDR),
    .portA_DI(state_portA_DI),
    .portA_DO(state_portA_DO),
    .portB_WE(state_portB_WE),
    .portB_ADDR(state_portB_ADDR),
    .portB_DI(state_portB_DI),
    .portB_DO(state_portB_DO)
  );
  TransplantUnit tpu ( // @[Proc.scala 71:19]
    .clock(tpu_clock),
    .reset(tpu_reset),
    .io_host2tpu_fire_tag(tpu_io_host2tpu_fire_tag),
    .io_host2tpu_fire_valid(tpu_io_host2tpu_fire_valid),
    .io_host2tpu_done_tag(tpu_io_host2tpu_done_tag),
    .io_host2tpu_done_valid(tpu_io_host2tpu_done_valid),
    .io_host2tpu_missTLB_bits_vaddr(tpu_io_host2tpu_missTLB_bits_vaddr),
    .io_host2tpu_missTLB_bits_tlbIdx(tpu_io_host2tpu_missTLB_bits_tlbIdx),
    .io_host2tpu_missTLB_valid(tpu_io_host2tpu_missTLB_valid),
    .io_host2tpu_fillTLB_valid(tpu_io_host2tpu_fillTLB_valid),
    .io_host2tpu_fillTLB_bits_tlbEntry_wrEn(tpu_io_host2tpu_fillTLB_bits_tlbEntry_wrEn),
    .io_host2tpu_fillTLB_bits_tlbEntry_tag(tpu_io_host2tpu_fillTLB_bits_tlbEntry_tag),
    .io_host2tpu_fillTLB_bits_vaddr(tpu_io_host2tpu_fillTLB_bits_vaddr),
    .io_host2tpu_fillTLB_bits_tlbIdx(tpu_io_host2tpu_fillTLB_bits_tlbIdx),
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
    .io_tpu2cpu_missTLB_bits_vaddr(tpu_io_tpu2cpu_missTLB_bits_vaddr),
    .io_tpu2cpu_missTLB_bits_tlbIdx(tpu_io_tpu2cpu_missTLB_bits_tlbIdx),
    .io_tpu2cpu_missTLB_valid(tpu_io_tpu2cpu_missTLB_valid),
    .io_tpu2cpu_fillTLB_valid(tpu_io_tpu2cpu_fillTLB_valid),
    .io_tpu2cpu_fillTLB_bits_tlbEntry_wrEn(tpu_io_tpu2cpu_fillTLB_bits_tlbEntry_wrEn),
    .io_tpu2cpu_fillTLB_bits_tlbEntry_tag(tpu_io_tpu2cpu_fillTLB_bits_tlbEntry_tag),
    .io_tpu2cpu_fillTLB_bits_vaddr(tpu_io_tpu2cpu_fillTLB_bits_vaddr),
    .io_tpu2cpu_fillTLB_bits_tlbIdx(tpu_io_tpu2cpu_fillTLB_bits_tlbIdx),
    .io_tpu2cpuStateReg_valid(tpu_io_tpu2cpuStateReg_valid),
    .io_tpu2cpuStateReg_bits(tpu_io_tpu2cpuStateReg_bits),
    .io_tpu2cpuState_PC(tpu_io_tpu2cpuState_PC),
    .io_tpu2cpuState_SP(tpu_io_tpu2cpuState_SP),
    .io_tpu2cpuState_NZCV(tpu_io_tpu2cpuState_NZCV),
    .io_cpu2tpuState_PC(tpu_io_cpu2tpuState_PC),
    .io_cpu2tpuState_SP(tpu_io_cpu2tpuState_SP),
    .io_cpu2tpuState_NZCV(tpu_io_cpu2tpuState_NZCV),
    .io_rfile_rs1_addr(tpu_io_rfile_rs1_addr),
    .io_rfile_rs1_data(tpu_io_rfile_rs1_data),
    .io_rfile_w1_addr(tpu_io_rfile_w1_addr),
    .io_rfile_w1_data(tpu_io_rfile_w1_data),
    .io_rfile_w1_en(tpu_io_rfile_w1_en),
    .io_stateBRAM_WE(tpu_io_stateBRAM_WE),
    .io_stateBRAM_ADDR(tpu_io_stateBRAM_ADDR),
    .io_stateBRAM_DI(tpu_io_stateBRAM_DI),
    .io_stateBRAM_DO(tpu_io_stateBRAM_DO)
  );
  RFile RFile ( // @[Proc.scala 75:57]
    .clock(RFile_clock),
    .io_rs1_addr(RFile_io_rs1_addr),
    .io_rs1_data(RFile_io_rs1_data),
    .io_rs2_addr(RFile_io_rs2_addr),
    .io_rs2_data(RFile_io_rs2_data),
    .io_w1_addr(RFile_io_w1_addr),
    .io_w1_data(RFile_io_w1_data),
    .io_w1_en(RFile_io_w1_en),
    .io_rw_addr(RFile_io_rw_addr),
    .io_rw_di(RFile_io_rw_di),
    .io_rw_wen(RFile_io_rw_wen),
    .io_rw_do(RFile_io_rw_do)
  );
  RFile RFile_1 ( // @[Proc.scala 75:57]
    .clock(RFile_1_clock),
    .io_rs1_addr(RFile_1_io_rs1_addr),
    .io_rs1_data(RFile_1_io_rs1_data),
    .io_rs2_addr(RFile_1_io_rs2_addr),
    .io_rs2_data(RFile_1_io_rs2_data),
    .io_w1_addr(RFile_1_io_w1_addr),
    .io_w1_data(RFile_1_io_w1_data),
    .io_w1_en(RFile_1_io_w1_en),
    .io_rw_addr(RFile_1_io_rw_addr),
    .io_rw_di(RFile_1_io_rw_di),
    .io_rw_wen(RFile_1_io_rw_wen),
    .io_rw_do(RFile_1_io_rw_do)
  );
  TLBUnit insnTLB ( // @[Proc.scala 79:23]
    .clock(insnTLB_clock),
    .reset(insnTLB_reset),
    .io_fillTLB_valid(insnTLB_io_fillTLB_valid),
    .io_fillTLB_bits_tlbEntry_wrEn(insnTLB_io_fillTLB_bits_tlbEntry_wrEn),
    .io_fillTLB_bits_tlbEntry_tag(insnTLB_io_fillTLB_bits_tlbEntry_tag),
    .io_fillTLB_bits_tlbIdx(insnTLB_io_fillTLB_bits_tlbIdx),
    .io_iPort_vaddr_valid(insnTLB_io_iPort_vaddr_valid),
    .io_iPort_vaddr_bits(insnTLB_io_iPort_vaddr_bits),
    .io_iPort_paddr(insnTLB_io_iPort_paddr),
    .io_iPort_miss_valid(insnTLB_io_iPort_miss_valid),
    .io_iPort_miss_bits_vaddr(insnTLB_io_iPort_miss_bits_vaddr),
    .io_iPort_miss_bits_tlbIdx(insnTLB_io_iPort_miss_bits_tlbIdx),
    .io_dPort_vaddr_valid(insnTLB_io_dPort_vaddr_valid),
    .io_dPort_vaddr_bits(insnTLB_io_dPort_vaddr_bits),
    .io_dPort_paddr(insnTLB_io_dPort_paddr),
    .io_dPort_miss_valid(insnTLB_io_dPort_miss_valid),
    .io_dPort_miss_bits_vaddr(insnTLB_io_dPort_miss_bits_vaddr),
    .io_dPort_miss_bits_tlbIdx(insnTLB_io_dPort_miss_bits_tlbIdx)
  );
  FetchUnit fetch ( // @[Proc.scala 83:21]
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
    .io_pc_tag(fetch_io_pc_tag),
    .io_pc_bits(fetch_io_pc_bits),
    .io_pc_valid(fetch_io_pc_valid),
    .io_hit(fetch_io_hit),
    .io_insn(fetch_io_insn),
    .io_deq_ready(fetch_io_deq_ready),
    .io_deq_valid(fetch_io_deq_valid),
    .io_deq_bits_inst(fetch_io_deq_bits_inst),
    .io_deq_bits_tag(fetch_io_deq_bits_tag),
    .io_deq_bits_pc(fetch_io_deq_bits_pc)
  );
  DecodeUnit decoder ( // @[Proc.scala 86:23]
    .io_finst_inst(decoder_io_finst_inst),
    .io_finst_tag(decoder_io_finst_tag),
    .io_finst_pc(decoder_io_finst_pc),
    .io_dinst_rd_valid(decoder_io_dinst_rd_valid),
    .io_dinst_rd_bits(decoder_io_dinst_rd_bits),
    .io_dinst_rs1(decoder_io_dinst_rs1),
    .io_dinst_rs2(decoder_io_dinst_rs2),
    .io_dinst_imm(decoder_io_dinst_imm),
    .io_dinst_shift_val_valid(decoder_io_dinst_shift_val_valid),
    .io_dinst_shift_val_bits(decoder_io_dinst_shift_val_bits),
    .io_dinst_shift_type(decoder_io_dinst_shift_type),
    .io_dinst_cond_valid(decoder_io_dinst_cond_valid),
    .io_dinst_cond_bits(decoder_io_dinst_cond_bits),
    .io_dinst_is32bit(decoder_io_dinst_is32bit),
    .io_dinst_itype(decoder_io_dinst_itype),
    .io_dinst_op(decoder_io_dinst_op),
    .io_dinst_nzcv_valid(decoder_io_dinst_nzcv_valid),
    .io_dinst_nzcv_bits(decoder_io_dinst_nzcv_bits),
    .io_dinst_tag(decoder_io_dinst_tag),
    .io_dinst_inst32_valid(decoder_io_dinst_inst32_valid),
    .io_dinst_inst32_bits(decoder_io_dinst_inst32_bits),
    .io_dinst_pc(decoder_io_dinst_pc)
  );
  FlushReg_1 decReg ( // @[Proc.scala 87:22]
    .clock(decReg_clock),
    .reset(decReg_reset),
    .io_enq_ready(decReg_io_enq_ready),
    .io_enq_valid(decReg_io_enq_valid),
    .io_enq_bits_rd_valid(decReg_io_enq_bits_rd_valid),
    .io_enq_bits_rd_bits(decReg_io_enq_bits_rd_bits),
    .io_enq_bits_rs1(decReg_io_enq_bits_rs1),
    .io_enq_bits_rs2(decReg_io_enq_bits_rs2),
    .io_enq_bits_imm(decReg_io_enq_bits_imm),
    .io_enq_bits_shift_val_valid(decReg_io_enq_bits_shift_val_valid),
    .io_enq_bits_shift_val_bits(decReg_io_enq_bits_shift_val_bits),
    .io_enq_bits_shift_type(decReg_io_enq_bits_shift_type),
    .io_enq_bits_cond_valid(decReg_io_enq_bits_cond_valid),
    .io_enq_bits_cond_bits(decReg_io_enq_bits_cond_bits),
    .io_enq_bits_is32bit(decReg_io_enq_bits_is32bit),
    .io_enq_bits_itype(decReg_io_enq_bits_itype),
    .io_enq_bits_op(decReg_io_enq_bits_op),
    .io_enq_bits_nzcv_valid(decReg_io_enq_bits_nzcv_valid),
    .io_enq_bits_nzcv_bits(decReg_io_enq_bits_nzcv_bits),
    .io_enq_bits_tag(decReg_io_enq_bits_tag),
    .io_enq_bits_inst32_valid(decReg_io_enq_bits_inst32_valid),
    .io_enq_bits_inst32_bits(decReg_io_enq_bits_inst32_bits),
    .io_enq_bits_pc(decReg_io_enq_bits_pc),
    .io_deq_ready(decReg_io_deq_ready),
    .io_deq_valid(decReg_io_deq_valid),
    .io_deq_bits_rd_valid(decReg_io_deq_bits_rd_valid),
    .io_deq_bits_rd_bits(decReg_io_deq_bits_rd_bits),
    .io_deq_bits_rs1(decReg_io_deq_bits_rs1),
    .io_deq_bits_rs2(decReg_io_deq_bits_rs2),
    .io_deq_bits_imm(decReg_io_deq_bits_imm),
    .io_deq_bits_shift_val_valid(decReg_io_deq_bits_shift_val_valid),
    .io_deq_bits_shift_val_bits(decReg_io_deq_bits_shift_val_bits),
    .io_deq_bits_shift_type(decReg_io_deq_bits_shift_type),
    .io_deq_bits_cond_valid(decReg_io_deq_bits_cond_valid),
    .io_deq_bits_cond_bits(decReg_io_deq_bits_cond_bits),
    .io_deq_bits_is32bit(decReg_io_deq_bits_is32bit),
    .io_deq_bits_itype(decReg_io_deq_bits_itype),
    .io_deq_bits_op(decReg_io_deq_bits_op),
    .io_deq_bits_nzcv_valid(decReg_io_deq_bits_nzcv_valid),
    .io_deq_bits_nzcv_bits(decReg_io_deq_bits_nzcv_bits),
    .io_deq_bits_tag(decReg_io_deq_bits_tag),
    .io_deq_bits_inst32_valid(decReg_io_deq_bits_inst32_valid),
    .io_deq_bits_inst32_bits(decReg_io_deq_bits_inst32_bits),
    .io_deq_bits_pc(decReg_io_deq_bits_pc),
    .io_flush(decReg_io_flush)
  );
  IssueUnit issuer ( // @[Proc.scala 89:22]
    .clock(issuer_clock),
    .reset(issuer_reset),
    .io_enq_ready(issuer_io_enq_ready),
    .io_enq_valid(issuer_io_enq_valid),
    .io_enq_bits_rd_valid(issuer_io_enq_bits_rd_valid),
    .io_enq_bits_rd_bits(issuer_io_enq_bits_rd_bits),
    .io_enq_bits_rs1(issuer_io_enq_bits_rs1),
    .io_enq_bits_rs2(issuer_io_enq_bits_rs2),
    .io_enq_bits_imm(issuer_io_enq_bits_imm),
    .io_enq_bits_shift_val_valid(issuer_io_enq_bits_shift_val_valid),
    .io_enq_bits_shift_val_bits(issuer_io_enq_bits_shift_val_bits),
    .io_enq_bits_shift_type(issuer_io_enq_bits_shift_type),
    .io_enq_bits_cond_valid(issuer_io_enq_bits_cond_valid),
    .io_enq_bits_cond_bits(issuer_io_enq_bits_cond_bits),
    .io_enq_bits_is32bit(issuer_io_enq_bits_is32bit),
    .io_enq_bits_itype(issuer_io_enq_bits_itype),
    .io_enq_bits_op(issuer_io_enq_bits_op),
    .io_enq_bits_nzcv_valid(issuer_io_enq_bits_nzcv_valid),
    .io_enq_bits_nzcv_bits(issuer_io_enq_bits_nzcv_bits),
    .io_enq_bits_tag(issuer_io_enq_bits_tag),
    .io_enq_bits_inst32_valid(issuer_io_enq_bits_inst32_valid),
    .io_enq_bits_inst32_bits(issuer_io_enq_bits_inst32_bits),
    .io_enq_bits_pc(issuer_io_enq_bits_pc),
    .io_deq_ready(issuer_io_deq_ready),
    .io_deq_valid(issuer_io_deq_valid),
    .io_deq_bits_rd_valid(issuer_io_deq_bits_rd_valid),
    .io_deq_bits_rd_bits(issuer_io_deq_bits_rd_bits),
    .io_deq_bits_rs1(issuer_io_deq_bits_rs1),
    .io_deq_bits_rs2(issuer_io_deq_bits_rs2),
    .io_deq_bits_imm(issuer_io_deq_bits_imm),
    .io_deq_bits_shift_val_valid(issuer_io_deq_bits_shift_val_valid),
    .io_deq_bits_shift_val_bits(issuer_io_deq_bits_shift_val_bits),
    .io_deq_bits_shift_type(issuer_io_deq_bits_shift_type),
    .io_deq_bits_cond_bits(issuer_io_deq_bits_cond_bits),
    .io_deq_bits_is32bit(issuer_io_deq_bits_is32bit),
    .io_deq_bits_itype(issuer_io_deq_bits_itype),
    .io_deq_bits_op(issuer_io_deq_bits_op),
    .io_deq_bits_nzcv_valid(issuer_io_deq_bits_nzcv_valid),
    .io_deq_bits_nzcv_bits(issuer_io_deq_bits_nzcv_bits),
    .io_deq_bits_tag(issuer_io_deq_bits_tag),
    .io_deq_bits_inst32_valid(issuer_io_deq_bits_inst32_valid),
    .io_commitReg_valid(issuer_io_commitReg_valid),
    .io_commitReg_bits_tag(issuer_io_commitReg_bits_tag),
    .io_flush_tag(issuer_io_flush_tag),
    .io_flush_valid(issuer_io_flush_valid)
  );
  ExecuteUnit executer ( // @[Proc.scala 97:24]
    .clock(executer_clock),
    .reset(executer_reset),
    .io_dinst_rd_valid(executer_io_dinst_rd_valid),
    .io_dinst_rd_bits(executer_io_dinst_rd_bits),
    .io_dinst_rs1(executer_io_dinst_rs1),
    .io_dinst_rs2(executer_io_dinst_rs2),
    .io_dinst_imm(executer_io_dinst_imm),
    .io_dinst_shift_val_valid(executer_io_dinst_shift_val_valid),
    .io_dinst_shift_val_bits(executer_io_dinst_shift_val_bits),
    .io_dinst_shift_type(executer_io_dinst_shift_type),
    .io_dinst_cond_bits(executer_io_dinst_cond_bits),
    .io_dinst_is32bit(executer_io_dinst_is32bit),
    .io_dinst_itype(executer_io_dinst_itype),
    .io_dinst_op(executer_io_dinst_op),
    .io_dinst_nzcv_valid(executer_io_dinst_nzcv_valid),
    .io_dinst_nzcv_bits(executer_io_dinst_nzcv_bits),
    .io_rVal1(executer_io_rVal1),
    .io_rVal2(executer_io_rVal2),
    .io_rVal3(executer_io_rVal3),
    .io_nzcv(executer_io_nzcv),
    .io_condRes(executer_io_condRes),
    .io_einst_valid(executer_io_einst_valid),
    .io_einst_bits_rd_valid(executer_io_einst_bits_rd_valid),
    .io_einst_bits_rd_bits(executer_io_einst_bits_rd_bits),
    .io_einst_bits_nzcv_valid(executer_io_einst_bits_nzcv_valid),
    .io_einst_bits_nzcv_bits(executer_io_einst_bits_nzcv_bits),
    .io_einst_bits_res(executer_io_einst_bits_res)
  );
  BranchUnit brancher ( // @[Proc.scala 98:24]
    .io_dinst_rd_bits(brancher_io_dinst_rd_bits),
    .io_dinst_imm(brancher_io_dinst_imm),
    .io_dinst_is32bit(brancher_io_dinst_is32bit),
    .io_dinst_itype(brancher_io_dinst_itype),
    .io_dinst_op(brancher_io_dinst_op),
    .io_rVal1(brancher_io_rVal1),
    .io_rVal2(brancher_io_rVal2),
    .io_cond(brancher_io_cond),
    .io_pc(brancher_io_pc),
    .io_binst_valid(brancher_io_binst_valid),
    .io_binst_bits_pc(brancher_io_binst_bits_pc),
    .io_binst_bits_unalignedExcp(brancher_io_binst_bits_unalignedExcp),
    .io_pcrel_valid(brancher_io_pcrel_valid),
    .io_pcrel_bits_rd(brancher_io_pcrel_bits_rd),
    .io_pcrel_bits_res(brancher_io_pcrel_bits_res)
  );
  LDSTUnit ldstU ( // @[Proc.scala 99:21]
    .io_dinst_rd_bits(ldstU_io_dinst_rd_bits),
    .io_dinst_rs1(ldstU_io_dinst_rs1),
    .io_dinst_rs2(ldstU_io_dinst_rs2),
    .io_dinst_imm(ldstU_io_dinst_imm),
    .io_dinst_shift_val_valid(ldstU_io_dinst_shift_val_valid),
    .io_dinst_shift_val_bits(ldstU_io_dinst_shift_val_bits),
    .io_dinst_itype(ldstU_io_dinst_itype),
    .io_dinst_op(ldstU_io_dinst_op),
    .io_rVal1(ldstU_io_rVal1),
    .io_rVal2(ldstU_io_rVal2),
    .io_minst_valid(ldstU_io_minst_valid),
    .io_minst_bits_size(ldstU_io_minst_bits_size),
    .io_minst_bits_isPair(ldstU_io_minst_bits_isPair),
    .io_minst_bits_isLoad(ldstU_io_minst_bits_isLoad),
    .io_minst_bits_memReq_0_addr(ldstU_io_minst_bits_memReq_0_addr),
    .io_minst_bits_memReq_0_reg(ldstU_io_minst_bits_memReq_0_reg),
    .io_minst_bits_memReq_1_addr(ldstU_io_minst_bits_memReq_1_addr),
    .io_minst_bits_memReq_1_reg(ldstU_io_minst_bits_memReq_1_reg),
    .io_minst_bits_rd_res(ldstU_io_minst_bits_rd_res),
    .io_minst_bits_rd_valid(ldstU_io_minst_bits_rd_valid),
    .io_minst_bits_rd_bits(ldstU_io_minst_bits_rd_bits),
    .io_minst_bits_unalignedExcpSP(ldstU_io_minst_bits_unalignedExcpSP)
  );
  FlushReg_2 commitReg ( // @[Proc.scala 100:25]
    .clock(commitReg_clock),
    .reset(commitReg_reset),
    .io_enq_ready(commitReg_io_enq_ready),
    .io_enq_valid(commitReg_io_enq_valid),
    .io_enq_bits_exe_valid(commitReg_io_enq_bits_exe_valid),
    .io_enq_bits_exe_bits_rd_valid(commitReg_io_enq_bits_exe_bits_rd_valid),
    .io_enq_bits_exe_bits_rd_bits(commitReg_io_enq_bits_exe_bits_rd_bits),
    .io_enq_bits_exe_bits_nzcv_valid(commitReg_io_enq_bits_exe_bits_nzcv_valid),
    .io_enq_bits_exe_bits_nzcv_bits(commitReg_io_enq_bits_exe_bits_nzcv_bits),
    .io_enq_bits_exe_bits_res(commitReg_io_enq_bits_exe_bits_res),
    .io_enq_bits_br_valid(commitReg_io_enq_bits_br_valid),
    .io_enq_bits_br_bits_pc(commitReg_io_enq_bits_br_bits_pc),
    .io_enq_bits_br_bits_unalignedExcp(commitReg_io_enq_bits_br_bits_unalignedExcp),
    .io_enq_bits_pcrel_valid(commitReg_io_enq_bits_pcrel_valid),
    .io_enq_bits_pcrel_bits_rd(commitReg_io_enq_bits_pcrel_bits_rd),
    .io_enq_bits_pcrel_bits_res(commitReg_io_enq_bits_pcrel_bits_res),
    .io_enq_bits_mem_valid(commitReg_io_enq_bits_mem_valid),
    .io_enq_bits_mem_bits_size(commitReg_io_enq_bits_mem_bits_size),
    .io_enq_bits_mem_bits_isPair(commitReg_io_enq_bits_mem_bits_isPair),
    .io_enq_bits_mem_bits_isLoad(commitReg_io_enq_bits_mem_bits_isLoad),
    .io_enq_bits_mem_bits_memReq_0_addr(commitReg_io_enq_bits_mem_bits_memReq_0_addr),
    .io_enq_bits_mem_bits_memReq_0_reg(commitReg_io_enq_bits_mem_bits_memReq_0_reg),
    .io_enq_bits_mem_bits_memReq_1_addr(commitReg_io_enq_bits_mem_bits_memReq_1_addr),
    .io_enq_bits_mem_bits_memReq_1_reg(commitReg_io_enq_bits_mem_bits_memReq_1_reg),
    .io_enq_bits_mem_bits_rd_res(commitReg_io_enq_bits_mem_bits_rd_res),
    .io_enq_bits_mem_bits_rd_valid(commitReg_io_enq_bits_mem_bits_rd_valid),
    .io_enq_bits_mem_bits_rd_bits(commitReg_io_enq_bits_mem_bits_rd_bits),
    .io_enq_bits_mem_bits_unalignedExcpSP(commitReg_io_enq_bits_mem_bits_unalignedExcpSP),
    .io_enq_bits_undef(commitReg_io_enq_bits_undef),
    .io_enq_bits_tag(commitReg_io_enq_bits_tag),
    .io_deq_ready(commitReg_io_deq_ready),
    .io_deq_valid(commitReg_io_deq_valid),
    .io_deq_bits_exe_valid(commitReg_io_deq_bits_exe_valid),
    .io_deq_bits_exe_bits_rd_valid(commitReg_io_deq_bits_exe_bits_rd_valid),
    .io_deq_bits_exe_bits_rd_bits(commitReg_io_deq_bits_exe_bits_rd_bits),
    .io_deq_bits_exe_bits_nzcv_valid(commitReg_io_deq_bits_exe_bits_nzcv_valid),
    .io_deq_bits_exe_bits_nzcv_bits(commitReg_io_deq_bits_exe_bits_nzcv_bits),
    .io_deq_bits_exe_bits_res(commitReg_io_deq_bits_exe_bits_res),
    .io_deq_bits_br_valid(commitReg_io_deq_bits_br_valid),
    .io_deq_bits_br_bits_pc(commitReg_io_deq_bits_br_bits_pc),
    .io_deq_bits_br_bits_unalignedExcp(commitReg_io_deq_bits_br_bits_unalignedExcp),
    .io_deq_bits_pcrel_valid(commitReg_io_deq_bits_pcrel_valid),
    .io_deq_bits_pcrel_bits_rd(commitReg_io_deq_bits_pcrel_bits_rd),
    .io_deq_bits_pcrel_bits_res(commitReg_io_deq_bits_pcrel_bits_res),
    .io_deq_bits_mem_valid(commitReg_io_deq_bits_mem_valid),
    .io_deq_bits_mem_bits_size(commitReg_io_deq_bits_mem_bits_size),
    .io_deq_bits_mem_bits_isPair(commitReg_io_deq_bits_mem_bits_isPair),
    .io_deq_bits_mem_bits_isLoad(commitReg_io_deq_bits_mem_bits_isLoad),
    .io_deq_bits_mem_bits_memReq_0_addr(commitReg_io_deq_bits_mem_bits_memReq_0_addr),
    .io_deq_bits_mem_bits_memReq_0_reg(commitReg_io_deq_bits_mem_bits_memReq_0_reg),
    .io_deq_bits_mem_bits_memReq_1_addr(commitReg_io_deq_bits_mem_bits_memReq_1_addr),
    .io_deq_bits_mem_bits_memReq_1_reg(commitReg_io_deq_bits_mem_bits_memReq_1_reg),
    .io_deq_bits_mem_bits_rd_res(commitReg_io_deq_bits_mem_bits_rd_res),
    .io_deq_bits_mem_bits_rd_valid(commitReg_io_deq_bits_mem_bits_rd_valid),
    .io_deq_bits_mem_bits_rd_bits(commitReg_io_deq_bits_mem_bits_rd_bits),
    .io_deq_bits_mem_bits_unalignedExcpSP(commitReg_io_deq_bits_mem_bits_unalignedExcpSP),
    .io_deq_bits_undef(commitReg_io_deq_bits_undef),
    .io_deq_bits_tag(commitReg_io_deq_bits_tag),
    .io_flush(commitReg_io_flush)
  );
  MemArbiterInst memArbiterInst ( // @[Proc.scala 118:30]
    .clock(memArbiterInst_clock),
    .reset(memArbiterInst_reset),
    .io_vaddr_valid(memArbiterInst_io_vaddr_valid),
    .io_vaddr_bits(memArbiterInst_io_vaddr_bits),
    .io_selHost(memArbiterInst_io_selHost),
    .io_selMem(memArbiterInst_io_selMem),
    .io_fillTLB_valid(memArbiterInst_io_fillTLB_valid),
    .io_fillTLB_bits_vaddr(memArbiterInst_io_fillTLB_bits_vaddr),
    .io_memPort_ADDR(memArbiterInst_io_memPort_ADDR),
    .io_tlbPort_vaddr_valid(memArbiterInst_io_tlbPort_vaddr_valid),
    .io_tlbPort_vaddr_bits(memArbiterInst_io_tlbPort_vaddr_bits),
    .io_tlbPort_paddr(memArbiterInst_io_tlbPort_paddr),
    .io_tlbPort_miss_valid(memArbiterInst_io_tlbPort_miss_valid),
    .io_tlbPort_miss_bits_vaddr(memArbiterInst_io_tlbPort_miss_bits_vaddr),
    .io_tlbPort_miss_bits_tlbIdx(memArbiterInst_io_tlbPort_miss_bits_tlbIdx),
    .io_reqMiss_valid(memArbiterInst_io_reqMiss_valid),
    .io_reqMiss_bits_vaddr(memArbiterInst_io_reqMiss_bits_vaddr),
    .io_reqMiss_bits_tlbIdx(memArbiterInst_io_reqMiss_bits_tlbIdx)
  );
  MemArbiterData memArbiterData ( // @[Proc.scala 119:30]
    .clock(memArbiterData_clock),
    .reset(memArbiterData_reset),
    .io_selHost(memArbiterData_io_selHost),
    .io_selMem(memArbiterData_io_selMem),
    .io_commitEnq_ready(memArbiterData_io_commitEnq_ready),
    .io_commitEnq_valid(memArbiterData_io_commitEnq_valid),
    .io_commitEnq_bits_mem_valid(memArbiterData_io_commitEnq_bits_mem_valid),
    .io_commitEnq_bits_mem_bits_memReq_0_addr(memArbiterData_io_commitEnq_bits_mem_bits_memReq_0_addr),
    .io_commitDeq_bits_mem_bits_size(memArbiterData_io_commitDeq_bits_mem_bits_size),
    .io_commitDeq_bits_mem_bits_isPair(memArbiterData_io_commitDeq_bits_mem_bits_isPair),
    .io_commitDeq_bits_mem_bits_isLoad(memArbiterData_io_commitDeq_bits_mem_bits_isLoad),
    .io_commitDeq_bits_mem_bits_memReq_0_addr(memArbiterData_io_commitDeq_bits_mem_bits_memReq_0_addr),
    .io_commitDeq_bits_mem_bits_memReq_0_reg(memArbiterData_io_commitDeq_bits_mem_bits_memReq_0_reg),
    .io_commitDeq_bits_mem_bits_memReq_1_addr(memArbiterData_io_commitDeq_bits_mem_bits_memReq_1_addr),
    .io_commitDeq_bits_mem_bits_memReq_1_reg(memArbiterData_io_commitDeq_bits_mem_bits_memReq_1_reg),
    .io_commitDeq_bits_mem_bits_rd_res(memArbiterData_io_commitDeq_bits_mem_bits_rd_res),
    .io_commitDeq_bits_mem_bits_rd_valid(memArbiterData_io_commitDeq_bits_mem_bits_rd_valid),
    .io_commitDeq_bits_mem_bits_rd_bits(memArbiterData_io_commitDeq_bits_mem_bits_rd_bits),
    .io_fillTLB_valid(memArbiterData_io_fillTLB_valid),
    .io_fillTLB_bits_vaddr(memArbiterData_io_fillTLB_bits_vaddr),
    .io_memPort_WE(memArbiterData_io_memPort_WE),
    .io_memPort_ADDR(memArbiterData_io_memPort_ADDR),
    .io_memPort_DI(memArbiterData_io_memPort_DI),
    .io_memPort_DO(memArbiterData_io_memPort_DO),
    .io_tlbPort_vaddr_valid(memArbiterData_io_tlbPort_vaddr_valid),
    .io_tlbPort_vaddr_bits(memArbiterData_io_tlbPort_vaddr_bits),
    .io_tlbPort_paddr(memArbiterData_io_tlbPort_paddr),
    .io_tlbPort_miss_valid(memArbiterData_io_tlbPort_miss_valid),
    .io_tlbPort_miss_bits_vaddr(memArbiterData_io_tlbPort_miss_bits_vaddr),
    .io_tlbPort_miss_bits_tlbIdx(memArbiterData_io_tlbPort_miss_bits_tlbIdx),
    .io_rfile_rs1_addr(memArbiterData_io_rfile_rs1_addr),
    .io_rfile_rs1_data(memArbiterData_io_rfile_rs1_data),
    .io_rfile_w1_addr(memArbiterData_io_rfile_w1_addr),
    .io_rfile_w1_data(memArbiterData_io_rfile_w1_data),
    .io_rfile_w1_en(memArbiterData_io_rfile_w1_en),
    .io_rfile_rw_addr(memArbiterData_io_rfile_rw_addr),
    .io_rfile_rw_di(memArbiterData_io_rfile_rw_di),
    .io_rfile_rw_wen(memArbiterData_io_rfile_rw_wen),
    .io_rfileWr(memArbiterData_io_rfileWr),
    .io_rfileRd(memArbiterData_io_rfileRd),
    .io_busy(memArbiterData_io_busy),
    .io_reqMiss_bits_vaddr(memArbiterData_io_reqMiss_bits_vaddr),
    .io_reqMiss_bits_tlbIdx(memArbiterData_io_reqMiss_bits_tlbIdx),
    .io_reqMiss_valid(memArbiterData_io_reqMiss_valid),
    .io_unalignedExcp(memArbiterData_io_unalignedExcp)
  );
  assign _GEN_0 = memArbiterData_io_reqMiss_valid; // @[Proc.scala 150:46]
  assign _GEN_2 = memArbiterData_io_reqMiss_bits_tlbIdx; // @[Proc.scala 150:46]
  assign _GEN_3 = memArbiterData_io_reqMiss_bits_vaddr; // @[Proc.scala 150:46]
  assign _GEN_8 = memArbiterInst_io_selHost ? io_memoryBRAM_EN : 1'h1; // @[Proc.scala 163:41]
  assign _GEN_9 = memArbiterInst_io_selHost ? io_memoryBRAM_WE : 8'h0; // @[Proc.scala 163:41]
  assign _GEN_10 = memArbiterInst_io_selHost ? io_memoryBRAM_DI : 64'h0; // @[Proc.scala 163:41]
  assign _GEN_11 = memArbiterInst_io_selHost ? io_memoryBRAM_ADDR : memArbiterInst_io_memPort_ADDR; // @[Proc.scala 163:41]
  assign _T_14 = memArbiterData_io_rfileRd ? memArbiterData_io_rfile_rs1_addr : issuer_io_deq_bits_rs1; // @[Proc.scala 193:26]
  assign _T_15 = memArbiterData_io_rfileRd ? 5'h0 : issuer_io_deq_bits_rs2; // @[Proc.scala 194:26]
  assign _rfileVec_tpu_io_tpu2cpu_freeze_tag_rs1_addr = tpu_io_rfile_rs1_addr; // @[Proc.scala 357:41 Proc.scala 357:41]
  assign _GEN_220 = ~tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_rs1_addr : _T_14; // @[Proc.scala 357:41]
  assign rfileVec_0_rs1_data = RFile_io_rs1_data; // @[Proc.scala 75:25 Proc.scala 75:25]
  assign _GEN_218 = ~tpu_io_tpu2cpu_freeze_tag ? 5'h0 : _T_15; // @[Proc.scala 357:41]
  assign rfileVec_0_rs2_data = RFile_io_rs2_data; // @[Proc.scala 75:25 Proc.scala 75:25]
  assign _rfileVec_tpu_io_tpu2cpu_freeze_tag_w1_addr = tpu_io_rfile_w1_addr; // @[Proc.scala 357:41 Proc.scala 357:41]
  assign commitExec_valid = commitReg_io_deq_bits_exe_valid;
  assign commitExec_bits_rd_bits = commitReg_io_deq_bits_exe_bits_rd_bits;
  assign commitPcRel_valid = commitReg_io_deq_bits_pcrel_valid;
  assign commitPcRel_bits_rd = commitReg_io_deq_bits_pcrel_bits_rd;
  assign _GEN_89 = memArbiterData_io_rfile_w1_addr; // @[Proc.scala 264:42]
  assign _GEN_91 = commitPcRel_valid ? commitPcRel_bits_rd : _GEN_89; // @[Proc.scala 261:35]
  assign _GEN_93 = commitExec_valid ? commitExec_bits_rd_bits : _GEN_91; // @[Proc.scala 258:28]
  assign _GEN_216 = ~tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_w1_addr : _GEN_93; // @[Proc.scala 357:41]
  assign _rfileVec_tpu_io_tpu2cpu_freeze_tag_w1_data = tpu_io_rfile_w1_data; // @[Proc.scala 357:41 Proc.scala 357:41]
  assign commitExec_bits_res = commitReg_io_deq_bits_exe_bits_res;
  assign commitPcRel_bits_res = commitReg_io_deq_bits_pcrel_bits_res;
  assign _GEN_90 = memArbiterData_io_rfile_w1_data; // @[Proc.scala 264:42]
  assign _GEN_92 = commitPcRel_valid ? commitPcRel_bits_res : _GEN_90; // @[Proc.scala 261:35]
  assign _GEN_94 = commitExec_valid ? commitExec_bits_res : _GEN_92; // @[Proc.scala 258:28]
  assign _GEN_214 = ~tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_w1_data : _GEN_94; // @[Proc.scala 357:41]
  assign _rfileVec_tpu_io_tpu2cpu_freeze_tag_w1_en = tpu_io_rfile_w1_en; // @[Proc.scala 357:41 Proc.scala 357:41]
  assign commitMem_valid = commitReg_io_deq_bits_mem_valid;
  assign unalignedExcpData = commitMem_valid & memArbiterData_io_unalignedExcp; // @[Proc.scala 247:52]
  assign commitBr_valid = commitReg_io_deq_bits_br_valid;
  assign commitBr_bits_unalignedExcp = commitReg_io_deq_bits_br_bits_unalignedExcp;
  assign unalignedExcpBranch = commitBr_valid & commitBr_bits_unalignedExcp; // @[Proc.scala 249:53]
  assign _T_28 = unalignedExcpData | unalignedExcpBranch; // @[Proc.scala 251:73]
  assign commitMem_bits_unalignedExcpSP = commitReg_io_deq_bits_mem_bits_unalignedExcpSP;
  assign unalignedExcpSP = commitMem_valid & commitMem_bits_unalignedExcpSP; // @[Proc.scala 248:50]
  assign _T_29 = _T_28 | unalignedExcpSP; // @[Proc.scala 251:96]
  assign exception = commitReg_io_deq_valid & _T_29; // @[Proc.scala 251:51]
  assign _T_59 = ~exception; // @[Proc.scala 302:37]
  assign _T_60 = memArbiterData_io_rfileWr & _T_59; // @[Proc.scala 302:34]
  assign commitTag = commitReg_io_deq_bits_tag;
  assign _rfileVec_T_62_w1_en = memArbiterData_io_rfile_w1_en; // @[Proc.scala 303:31 Proc.scala 303:31]
  assign commited = commitReg_io_deq_ready & commitReg_io_deq_valid; // @[Decoupled.scala 40:37]
  assign _T_41 = commited & _T_59; // @[Proc.scala 285:17]
  assign commitUndef = commitReg_io_deq_valid & commitReg_io_deq_bits_undef; // @[Proc.scala 245:53]
  assign _T_42 = ~commitUndef; // @[Proc.scala 285:34]
  assign _T_43 = _T_41 & _T_42; // @[Proc.scala 285:31]
  assign commitExec_bits_rd_valid = commitReg_io_deq_bits_exe_bits_rd_valid;
  assign _T_46 = commitExec_valid & commitExec_bits_rd_valid; // @[Proc.scala 286:52]
  assign _T_47 = _T_46 | commitPcRel_valid; // @[Proc.scala 286:81]
  assign _GEN_277 = ~commitTag; // @[Proc.scala 286:31]
  assign _GEN_107 = _GEN_277 & _T_47; // @[Proc.scala 286:31]
  assign _GEN_124 = _T_43 & _GEN_107; // @[Proc.scala 285:48]
  assign _GEN_133 = ~commitTag ? _rfileVec_T_62_w1_en : _GEN_124; // @[Proc.scala 303:31]
  assign _GEN_137 = _T_60 ? _GEN_133 : _GEN_124; // @[Proc.scala 302:49]
  assign _GEN_212 = ~tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_w1_en : _GEN_137; // @[Proc.scala 357:41]
  assign _T_33 = memArbiterData_io_rfileWr ? memArbiterData_io_rfile_rw_addr : issuer_io_deq_bits_imm[4:0]; // @[Proc.scala 275:33]
  assign _GEN_210 = ~tpu_io_tpu2cpu_freeze_tag ? 5'h0 : _T_33; // @[Proc.scala 357:41]
  assign _GEN_208 = ~tpu_io_tpu2cpu_freeze_tag ? 64'h0 : memArbiterData_io_rfile_rw_di; // @[Proc.scala 357:41]
  assign _rfileVec_T_64_rw_wen = memArbiterData_io_rfile_rw_wen; // @[Proc.scala 304:32 Proc.scala 304:32]
  assign _GEN_135 = _GEN_277 & _rfileVec_T_64_rw_wen; // @[Proc.scala 304:32]
  assign _GEN_139 = _T_60 & _GEN_135; // @[Proc.scala 302:49]
  assign _GEN_206 = ~tpu_io_tpu2cpu_freeze_tag ? 1'h0 : _GEN_139; // @[Proc.scala 357:41]
  assign rfileVec_0_rw_do = RFile_io_rw_do; // @[Proc.scala 75:25 Proc.scala 75:25]
  assign _GEN_221 = tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_rs1_addr : _T_14; // @[Proc.scala 357:41]
  assign rfileVec_1_rs1_data = RFile_1_io_rs1_data; // @[Proc.scala 75:25 Proc.scala 75:25]
  assign _GEN_219 = tpu_io_tpu2cpu_freeze_tag ? 5'h0 : _T_15; // @[Proc.scala 357:41]
  assign rfileVec_1_rs2_data = RFile_1_io_rs2_data; // @[Proc.scala 75:25 Proc.scala 75:25]
  assign _GEN_217 = tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_w1_addr : _GEN_93; // @[Proc.scala 357:41]
  assign _GEN_215 = tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_w1_data : _GEN_94; // @[Proc.scala 357:41]
  assign _GEN_108 = commitTag & _T_47; // @[Proc.scala 286:31]
  assign _GEN_125 = _T_43 & _GEN_108; // @[Proc.scala 285:48]
  assign _GEN_134 = commitTag ? _rfileVec_T_62_w1_en : _GEN_125; // @[Proc.scala 303:31]
  assign _GEN_138 = _T_60 ? _GEN_134 : _GEN_125; // @[Proc.scala 302:49]
  assign _GEN_213 = tpu_io_tpu2cpu_freeze_tag ? _rfileVec_tpu_io_tpu2cpu_freeze_tag_w1_en : _GEN_138; // @[Proc.scala 357:41]
  assign _GEN_211 = tpu_io_tpu2cpu_freeze_tag ? 5'h0 : _T_33; // @[Proc.scala 357:41]
  assign _GEN_209 = tpu_io_tpu2cpu_freeze_tag ? 64'h0 : memArbiterData_io_rfile_rw_di; // @[Proc.scala 357:41]
  assign _GEN_136 = commitTag & _rfileVec_T_64_rw_wen; // @[Proc.scala 304:32]
  assign _GEN_140 = _T_60 & _GEN_136; // @[Proc.scala 302:49]
  assign _GEN_207 = tpu_io_tpu2cpu_freeze_tag ? 1'h0 : _GEN_140; // @[Proc.scala 357:41]
  assign rfileVec_1_rw_do = RFile_1_io_rw_do; // @[Proc.scala 75:25 Proc.scala 75:25]
  assign _GEN_98 = commitTag ? pregsVec_1_PC : pregsVec_0_PC; // @[Proc.scala 283:10]
  assign commitExec_bits_nzcv_valid = commitReg_io_deq_bits_exe_bits_nzcv_valid;
  assign _T_50 = commitExec_valid & commitExec_bits_nzcv_valid; // @[Proc.scala 289:27]
  assign commitExec_bits_nzcv_bits = commitReg_io_deq_bits_exe_bits_nzcv_bits;
  assign _GEN_111 = ~commitTag ? commitExec_bits_nzcv_bits : pregsVec_0_NZCV; // @[Proc.scala 290:32]
  assign _GEN_112 = commitTag ? commitExec_bits_nzcv_bits : pregsVec_1_NZCV; // @[Proc.scala 290:32]
  assign _GEN_113 = _T_50 ? _GEN_111 : pregsVec_0_NZCV; // @[Proc.scala 289:58]
  assign _GEN_114 = _T_50 ? _GEN_112 : pregsVec_1_NZCV; // @[Proc.scala 289:58]
  assign _T_56 = _GEN_98 + 64'h4; // @[Proc.scala 296:40]
  assign commitBr_bits_pc = commitReg_io_deq_bits_br_bits_pc;
  assign _GEN_121 = commitBr_valid ? commitBr_bits_pc : _T_56; // @[Proc.scala 293:26]
  assign nextPC = _T_43 ? _GEN_121 : _GEN_98; // @[Proc.scala 285:48]
  assign _GEN_128 = _T_43 ? _GEN_113 : pregsVec_0_NZCV; // @[Proc.scala 285:48]
  assign _GEN_129 = _T_43 ? _GEN_114 : pregsVec_1_NZCV; // @[Proc.scala 285:48]
  assign _GEN_141 = memArbiterData_io_selHost ? io_memoryBRAM_EN : 1'h1; // @[Proc.scala 314:41]
  assign _GEN_142 = memArbiterData_io_selHost ? io_memoryBRAM_WE : memArbiterData_io_memPort_WE; // @[Proc.scala 314:41]
  assign _GEN_143 = memArbiterData_io_selHost ? io_memoryBRAM_DI : memArbiterData_io_memPort_DI; // @[Proc.scala 314:41]
  assign _GEN_144 = memory_portA_DO; // @[Proc.scala 314:41]
  assign _GEN_145 = memArbiterData_io_selHost ? io_memoryBRAM_ADDR : memArbiterData_io_memPort_ADDR; // @[Proc.scala 314:41]
  assign _GEN_152 = ~fetch_io_pc_tag ? 1'h0 : fetchEn_0; // @[Proc.scala 320:65]
  assign _GEN_153 = fetch_io_pc_tag ? 1'h0 : fetchEn_1; // @[Proc.scala 320:65]
  assign _GEN_154 = insnTLB_io_iPort_miss_valid ? _GEN_152 : fetchEn_0; // @[Proc.scala 320:38]
  assign _GEN_155 = insnTLB_io_iPort_miss_valid ? _GEN_153 : fetchEn_1; // @[Proc.scala 320:38]
  assign _GEN_156 = tpu_io_tpu2cpu_fillTLB_valid | _GEN_154; // @[Proc.scala 321:38]
  assign _GEN_157 = ~tpu_io_tpu2cpu_freeze_tag ? 1'h0 : _GEN_156; // @[Proc.scala 322:75]
  assign _GEN_158 = tpu_io_tpu2cpu_freeze_tag ? 1'h0 : _GEN_155; // @[Proc.scala 322:75]
  assign _GEN_159 = tpu_io_tpu2cpu_freeze_valid ? _GEN_157 : _GEN_156; // @[Proc.scala 322:38]
  assign _GEN_160 = tpu_io_tpu2cpu_freeze_valid ? _GEN_158 : _GEN_155; // @[Proc.scala 322:38]
  assign _GEN_279 = ~tpu_io_tpu2cpu_fire_tag; // @[Proc.scala 323:73]
  assign _GEN_161 = _GEN_279 | _GEN_159; // @[Proc.scala 323:73]
  assign _GEN_162 = tpu_io_tpu2cpu_fire_tag | _GEN_160; // @[Proc.scala 323:73]
  assign _T_66 = decReg_io_deq_bits_tag == tpu_io_tpu2cpu_flush_tag; // @[Proc.scala 338:47]
  assign _T_67 = commitReg_io_deq_bits_tag == tpu_io_tpu2cpu_flush_tag; // @[Proc.scala 339:53]
  assign _T_68 = commited & commitBr_valid; // @[Proc.scala 340:23]
  assign _T_69 = fetch_io_deq_bits_tag == commitTag; // @[Proc.scala 341:51]
  assign _T_70 = decReg_io_deq_bits_tag == commitTag; // @[Proc.scala 345:47]
  assign _GEN_169 = _T_68 ? _T_69 : tpu_io_tpu2cpu_flush_valid; // @[Proc.scala 340:43]
  assign _GEN_170 = _T_68 ? commitTag : tpu_io_tpu2cpu_flush_tag; // @[Proc.scala 340:43]
  assign _GEN_171 = _T_68 | tpu_io_tpu2cpu_flush_valid; // @[Proc.scala 340:43]
  assign _GEN_172 = _T_68 & _T_70; // @[Proc.scala 340:43]
  assign _GEN_181 = tpu_io_tpu2cpu_freeze_tag ? pregsVec_1_PC : pregsVec_0_PC; // @[Proc.scala 350:23]
  assign _GEN_182 = tpu_io_tpu2cpu_freeze_tag ? pregsVec_1_SP : pregsVec_0_SP; // @[Proc.scala 350:23]
  assign _GEN_183 = tpu_io_tpu2cpu_freeze_tag ? pregsVec_1_NZCV : pregsVec_0_NZCV; // @[Proc.scala 350:23]
  assign _GEN_222 = ~tpu_io_tpu2cpu_freeze_tag ? _GEN_183 : _GEN_128; // @[Proc.scala 359:41]
  assign _GEN_223 = tpu_io_tpu2cpu_freeze_tag ? _GEN_183 : _GEN_129; // @[Proc.scala 359:41]
  assign _T_71 = tpu_io_tpu2cpuStateReg_bits == 3'h2; // @[Proc.scala 361:40]
  assign _pregsVec_tpu_io_tpu2cpu_freeze_tag_PC_0 = tpu_io_tpu2cpuState_PC; // @[Proc.scala 362:48 Proc.scala 362:48]
  assign _T_72 = tpu_io_tpu2cpuStateReg_bits == 3'h3; // @[Proc.scala 363:46]
  assign _pregsVec_tpu_io_tpu2cpu_freeze_tag_SP_0 = tpu_io_tpu2cpuState_SP; // @[Proc.scala 364:48 Proc.scala 364:48]
  assign _T_73 = tpu_io_tpu2cpuStateReg_bits == 3'h4; // @[Proc.scala 365:46]
  assign _pregsVec_tpu_io_tpu2cpu_freeze_tag_NZCV_0 = tpu_io_tpu2cpuState_NZCV; // @[Proc.scala 366:50 Proc.scala 366:50]
  assign io_memoryBRAM_DO = memArbiterData_io_selMem ? memory_portA_DO : _GEN_144; // @[BramModules.scala 204:8 Proc.scala 308:20 BramModules.scala 204:8]
  assign io_stateBRAM_DO = state_portA_DO; // @[BramModules.scala 204:8]
  assign io_host2tpu_done_tag = tpu_io_host2tpu_done_tag; // @[Proc.scala 124:15]
  assign io_host2tpu_done_valid = tpu_io_host2tpu_done_valid; // @[Proc.scala 124:15]
  assign io_host2tpu_missTLB_bits_vaddr = tpu_io_host2tpu_missTLB_bits_vaddr; // @[Proc.scala 124:15]
  assign io_host2tpu_missTLB_bits_tlbIdx = tpu_io_host2tpu_missTLB_bits_tlbIdx; // @[Proc.scala 124:15]
  assign io_host2tpu_missTLB_valid = tpu_io_host2tpu_missTLB_valid; // @[Proc.scala 124:15]
  assign memory_clock = clock;
  assign memory_reset = reset;
  assign memory_portA_EN = memArbiterData_io_selMem | _GEN_141; // @[BramModules.scala 201:8 BramModules.scala 201:8 BramModules.scala 201:8]
  assign memory_portA_WE = memArbiterData_io_selMem ? memArbiterData_io_memPort_WE : _GEN_142; // @[BramModules.scala 202:8 BramModules.scala 202:8 BramModules.scala 202:8]
  assign memory_portA_ADDR = memArbiterData_io_selMem ? memArbiterData_io_memPort_ADDR : _GEN_145; // @[BramModules.scala 209:34 BramModules.scala 209:34 BramModules.scala 209:34]
  assign memory_portA_DI = memArbiterData_io_selMem ? memArbiterData_io_memPort_DI : _GEN_143; // @[BramModules.scala 203:8 BramModules.scala 203:8 BramModules.scala 203:8]
  assign memory_portB_EN = memArbiterInst_io_selMem | _GEN_8; // @[BramModules.scala 201:8 BramModules.scala 201:8 BramModules.scala 201:8]
  assign memory_portB_WE = memArbiterInst_io_selMem ? 8'h0 : _GEN_9; // @[BramModules.scala 202:8 BramModules.scala 202:8 BramModules.scala 202:8]
  assign memory_portB_ADDR = memArbiterInst_io_selMem ? memArbiterInst_io_memPort_ADDR : _GEN_11; // @[BramModules.scala 209:34 BramModules.scala 209:34 BramModules.scala 209:34]
  assign memory_portB_DI = memArbiterInst_io_selMem ? 64'h0 : _GEN_10; // @[BramModules.scala 203:8 BramModules.scala 203:8 BramModules.scala 203:8]
  assign state_clock = clock;
  assign state_reset = reset;
  assign state_portA_EN = io_stateBRAM_EN; // @[BramModules.scala 201:8]
  assign state_portA_WE = io_stateBRAM_WE; // @[BramModules.scala 202:8]
  assign state_portA_ADDR = io_stateBRAM_ADDR; // @[BramModules.scala 209:34]
  assign state_portA_DI = io_stateBRAM_DI; // @[BramModules.scala 203:8]
  assign state_portB_WE = tpu_io_stateBRAM_WE; // @[BramModules.scala 202:8]
  assign state_portB_ADDR = tpu_io_stateBRAM_ADDR; // @[BramModules.scala 209:34]
  assign state_portB_DI = tpu_io_stateBRAM_DI; // @[BramModules.scala 203:8]
  assign tpu_clock = clock;
  assign tpu_reset = reset;
  assign tpu_io_host2tpu_fire_tag = io_host2tpu_fire_tag; // @[Proc.scala 124:15]
  assign tpu_io_host2tpu_fire_valid = io_host2tpu_fire_valid; // @[Proc.scala 124:15]
  assign tpu_io_host2tpu_fillTLB_valid = io_host2tpu_fillTLB_valid; // @[Proc.scala 124:15]
  assign tpu_io_host2tpu_fillTLB_bits_tlbEntry_wrEn = io_host2tpu_fillTLB_bits_tlbEntry_wrEn; // @[Proc.scala 124:15]
  assign tpu_io_host2tpu_fillTLB_bits_tlbEntry_tag = io_host2tpu_fillTLB_bits_tlbEntry_tag; // @[Proc.scala 124:15]
  assign tpu_io_host2tpu_fillTLB_bits_vaddr = io_host2tpu_fillTLB_bits_vaddr; // @[Proc.scala 124:15]
  assign tpu_io_host2tpu_fillTLB_bits_tlbIdx = io_host2tpu_fillTLB_bits_tlbIdx; // @[Proc.scala 124:15]
  assign tpu_io_host2tpu_getState_tag = io_host2tpu_getState_tag; // @[Proc.scala 124:15]
  assign tpu_io_host2tpu_getState_valid = io_host2tpu_getState_valid; // @[Proc.scala 124:15]
  assign tpu_io_tpu2cpu_done_tag = commitReg_io_deq_bits_tag; // @[Proc.scala 129:27 Proc.scala 328:27]
  assign tpu_io_tpu2cpu_done_valid = commitUndef | exception; // @[Proc.scala 128:29 Proc.scala 327:29]
  assign tpu_io_tpu2cpu_missTLB_bits_vaddr = memArbiterInst_io_reqMiss_valid ? memArbiterInst_io_reqMiss_bits_vaddr : _GEN_3; // @[Proc.scala 149:37 Proc.scala 153:37]
  assign tpu_io_tpu2cpu_missTLB_bits_tlbIdx = memArbiterInst_io_reqMiss_valid ? memArbiterInst_io_reqMiss_bits_tlbIdx : _GEN_2; // @[Proc.scala 149:37 Proc.scala 153:37]
  assign tpu_io_tpu2cpu_missTLB_valid = memArbiterInst_io_reqMiss_valid | _GEN_0; // @[Proc.scala 147:34 Proc.scala 151:34 Proc.scala 155:34]
  assign tpu_io_cpu2tpuState_PC = tpu_io_tpu2cpu_freeze_tag ? pregsVec_1_PC : pregsVec_0_PC; // @[Proc.scala 350:23]
  assign tpu_io_cpu2tpuState_SP = tpu_io_tpu2cpu_freeze_tag ? pregsVec_1_SP : pregsVec_0_SP; // @[Proc.scala 350:23]
  assign tpu_io_cpu2tpuState_NZCV = tpu_io_tpu2cpu_freeze_tag ? pregsVec_1_NZCV : pregsVec_0_NZCV; // @[Proc.scala 350:23]
  assign tpu_io_rfile_rs1_data = tpu_io_tpu2cpu_freeze_tag ? rfileVec_1_rs1_data : rfileVec_0_rs1_data; // @[Proc.scala 357:41]
  assign tpu_io_stateBRAM_DO = state_portB_DO; // @[BramModules.scala 204:8]
  assign RFile_clock = clock;
  assign RFile_io_rs1_addr = tpu_io_tpu2cpu_freeze_valid ? _GEN_220 : _T_14; // @[Proc.scala 75:25]
  assign RFile_io_rs2_addr = tpu_io_tpu2cpu_freeze_valid ? _GEN_218 : _T_15; // @[Proc.scala 75:25]
  assign RFile_io_w1_addr = tpu_io_tpu2cpu_freeze_valid ? _GEN_216 : _GEN_93; // @[Proc.scala 75:25]
  assign RFile_io_w1_data = tpu_io_tpu2cpu_freeze_valid ? _GEN_214 : _GEN_94; // @[Proc.scala 75:25]
  assign RFile_io_w1_en = tpu_io_tpu2cpu_freeze_valid ? _GEN_212 : _GEN_137; // @[Proc.scala 75:25]
  assign RFile_io_rw_addr = tpu_io_tpu2cpu_freeze_valid ? _GEN_210 : _T_33; // @[Proc.scala 75:25]
  assign RFile_io_rw_di = tpu_io_tpu2cpu_freeze_valid ? _GEN_208 : memArbiterData_io_rfile_rw_di; // @[Proc.scala 75:25]
  assign RFile_io_rw_wen = tpu_io_tpu2cpu_freeze_valid ? _GEN_206 : _GEN_139; // @[Proc.scala 75:25]
  assign RFile_1_clock = clock;
  assign RFile_1_io_rs1_addr = tpu_io_tpu2cpu_freeze_valid ? _GEN_221 : _T_14; // @[Proc.scala 75:25]
  assign RFile_1_io_rs2_addr = tpu_io_tpu2cpu_freeze_valid ? _GEN_219 : _T_15; // @[Proc.scala 75:25]
  assign RFile_1_io_w1_addr = tpu_io_tpu2cpu_freeze_valid ? _GEN_217 : _GEN_93; // @[Proc.scala 75:25]
  assign RFile_1_io_w1_data = tpu_io_tpu2cpu_freeze_valid ? _GEN_215 : _GEN_94; // @[Proc.scala 75:25]
  assign RFile_1_io_w1_en = tpu_io_tpu2cpu_freeze_valid ? _GEN_213 : _GEN_138; // @[Proc.scala 75:25]
  assign RFile_1_io_rw_addr = tpu_io_tpu2cpu_freeze_valid ? _GEN_211 : _T_33; // @[Proc.scala 75:25]
  assign RFile_1_io_rw_di = tpu_io_tpu2cpu_freeze_valid ? _GEN_209 : memArbiterData_io_rfile_rw_di; // @[Proc.scala 75:25]
  assign RFile_1_io_rw_wen = tpu_io_tpu2cpu_freeze_valid ? _GEN_207 : _GEN_140; // @[Proc.scala 75:25]
  assign insnTLB_clock = clock;
  assign insnTLB_reset = reset;
  assign insnTLB_io_fillTLB_valid = tpu_io_tpu2cpu_fillTLB_valid; // @[Proc.scala 130:22]
  assign insnTLB_io_fillTLB_bits_tlbEntry_wrEn = tpu_io_tpu2cpu_fillTLB_bits_tlbEntry_wrEn; // @[Proc.scala 130:22]
  assign insnTLB_io_fillTLB_bits_tlbEntry_tag = tpu_io_tpu2cpu_fillTLB_bits_tlbEntry_tag; // @[Proc.scala 130:22]
  assign insnTLB_io_fillTLB_bits_tlbIdx = tpu_io_tpu2cpu_fillTLB_bits_tlbIdx; // @[Proc.scala 130:22]
  assign insnTLB_io_iPort_vaddr_valid = memArbiterInst_io_tlbPort_vaddr_valid; // @[Proc.scala 142:20]
  assign insnTLB_io_iPort_vaddr_bits = memArbiterInst_io_tlbPort_vaddr_bits; // @[Proc.scala 142:20]
  assign insnTLB_io_dPort_vaddr_valid = memArbiterData_io_tlbPort_vaddr_valid; // @[Proc.scala 309:20]
  assign insnTLB_io_dPort_vaddr_bits = memArbiterData_io_tlbPort_vaddr_bits; // @[Proc.scala 309:20]
  assign fetch_clock = clock;
  assign fetch_reset = reset;
  assign fetch_io_flush_tag = tpu_io_tpu2cpu_flush_valid ? tpu_io_tpu2cpu_flush_tag : _GEN_170; // @[Proc.scala 332:18 Proc.scala 337:20 Proc.scala 342:24]
  assign fetch_io_flush_valid = tpu_io_tpu2cpu_flush_valid ? tpu_io_tpu2cpu_flush_valid : _GEN_169; // @[Proc.scala 332:18 Proc.scala 337:20 Proc.scala 341:26]
  assign fetch_io_fire_tag = tpu_io_tpu2cpu_fire_tag; // @[Proc.scala 133:17]
  assign fetch_io_fire_valid = tpu_io_tpu2cpu_fire_valid; // @[Proc.scala 133:17]
  assign fetch_io_commitReg_valid = commitReg_io_deq_valid; // @[Proc.scala 138:28]
  assign fetch_io_commitReg_bits_br_valid = commitReg_io_deq_bits_br_valid; // @[Proc.scala 137:27]
  assign fetch_io_commitReg_bits_tag = commitReg_io_deq_bits_tag; // @[Proc.scala 137:27]
  assign fetch_io_nextPC = _T_43 ? _GEN_121 : _GEN_98; // @[Proc.scala 136:19]
  assign fetch_io_fetchEn_0 = fetchEn_0; // @[Proc.scala 134:20]
  assign fetch_io_fetchEn_1 = fetchEn_1; // @[Proc.scala 134:20]
  assign fetch_io_pcVec_0 = pregsVec_0_PC; // @[Proc.scala 135:75]
  assign fetch_io_pcVec_1 = pregsVec_1_PC; // @[Proc.scala 135:75]
  assign fetch_io_hit = ~memArbiterInst_io_tlbPort_miss_valid; // @[Proc.scala 169:16]
  assign fetch_io_insn = sel32bit ? memory_portB_DO[63:32] : memory_portB_DO[31:0]; // @[Proc.scala 170:17]
  assign fetch_io_deq_ready = decReg_io_enq_ready; // @[Proc.scala 177:22]
  assign decoder_io_finst_inst = fetch_io_deq_bits_inst; // @[Proc.scala 174:20]
  assign decoder_io_finst_tag = fetch_io_deq_bits_tag; // @[Proc.scala 174:20]
  assign decoder_io_finst_pc = fetch_io_deq_bits_pc; // @[Proc.scala 174:20]
  assign decReg_clock = clock;
  assign decReg_reset = reset;
  assign decReg_io_enq_valid = fetch_io_deq_valid; // @[Proc.scala 178:23]
  assign decReg_io_enq_bits_rd_valid = decoder_io_dinst_rd_valid; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_rd_bits = decoder_io_dinst_rd_bits; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_rs1 = decoder_io_dinst_rs1; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_rs2 = decoder_io_dinst_rs2; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_imm = decoder_io_dinst_imm; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_shift_val_valid = decoder_io_dinst_shift_val_valid; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_shift_val_bits = decoder_io_dinst_shift_val_bits; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_shift_type = decoder_io_dinst_shift_type; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_cond_valid = decoder_io_dinst_cond_valid; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_cond_bits = decoder_io_dinst_cond_bits; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_is32bit = decoder_io_dinst_is32bit; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_itype = decoder_io_dinst_itype; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_op = decoder_io_dinst_op; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_nzcv_valid = decoder_io_dinst_nzcv_valid; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_nzcv_bits = decoder_io_dinst_nzcv_bits; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_tag = decoder_io_dinst_tag; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_inst32_valid = decoder_io_dinst_inst32_valid; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_inst32_bits = decoder_io_dinst_inst32_bits; // @[Proc.scala 175:22]
  assign decReg_io_enq_bits_pc = decoder_io_dinst_pc; // @[Proc.scala 175:22]
  assign decReg_io_deq_ready = issuer_io_enq_ready; // @[Proc.scala 181:17]
  assign decReg_io_flush = tpu_io_tpu2cpu_flush_valid ? _T_66 : _GEN_172; // @[Proc.scala 333:19 Proc.scala 338:21 Proc.scala 345:21]
  assign issuer_clock = clock;
  assign issuer_reset = reset;
  assign issuer_io_enq_valid = decReg_io_deq_valid; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_rd_valid = decReg_io_deq_bits_rd_valid; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_rd_bits = decReg_io_deq_bits_rd_bits; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_rs1 = decReg_io_deq_bits_rs1; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_rs2 = decReg_io_deq_bits_rs2; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_imm = decReg_io_deq_bits_imm; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_shift_val_valid = decReg_io_deq_bits_shift_val_valid; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_shift_val_bits = decReg_io_deq_bits_shift_val_bits; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_shift_type = decReg_io_deq_bits_shift_type; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_cond_valid = decReg_io_deq_bits_cond_valid; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_cond_bits = decReg_io_deq_bits_cond_bits; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_is32bit = decReg_io_deq_bits_is32bit; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_itype = decReg_io_deq_bits_itype; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_op = decReg_io_deq_bits_op; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_nzcv_valid = decReg_io_deq_bits_nzcv_valid; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_nzcv_bits = decReg_io_deq_bits_nzcv_bits; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_tag = decReg_io_deq_bits_tag; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_inst32_valid = decReg_io_deq_bits_inst32_valid; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_inst32_bits = decReg_io_deq_bits_inst32_bits; // @[Proc.scala 181:17]
  assign issuer_io_enq_bits_pc = decReg_io_deq_bits_pc; // @[Proc.scala 181:17]
  assign issuer_io_deq_ready = commitReg_io_enq_ready; // @[Proc.scala 186:23]
  assign issuer_io_commitReg_valid = commitReg_io_deq_valid; // @[Proc.scala 188:29]
  assign issuer_io_commitReg_bits_tag = commitReg_io_deq_bits_tag; // @[Proc.scala 187:28]
  assign issuer_io_flush_tag = tpu_io_tpu2cpu_flush_valid ? tpu_io_tpu2cpu_flush_tag : _GEN_170; // @[Proc.scala 331:19 Proc.scala 336:21 Proc.scala 344:25]
  assign issuer_io_flush_valid = tpu_io_tpu2cpu_flush_valid ? tpu_io_tpu2cpu_flush_valid : _GEN_171; // @[Proc.scala 331:19 Proc.scala 336:21 Proc.scala 343:27]
  assign executer_clock = clock;
  assign executer_reset = reset;
  assign executer_io_dinst_rd_valid = issuer_io_deq_bits_rd_valid; // @[Proc.scala 201:21]
  assign executer_io_dinst_rd_bits = issuer_io_deq_bits_rd_bits; // @[Proc.scala 201:21]
  assign executer_io_dinst_rs1 = issuer_io_deq_bits_rs1; // @[Proc.scala 201:21]
  assign executer_io_dinst_rs2 = issuer_io_deq_bits_rs2; // @[Proc.scala 201:21]
  assign executer_io_dinst_imm = issuer_io_deq_bits_imm; // @[Proc.scala 201:21]
  assign executer_io_dinst_shift_val_valid = issuer_io_deq_bits_shift_val_valid; // @[Proc.scala 201:21]
  assign executer_io_dinst_shift_val_bits = issuer_io_deq_bits_shift_val_bits; // @[Proc.scala 201:21]
  assign executer_io_dinst_shift_type = issuer_io_deq_bits_shift_type; // @[Proc.scala 201:21]
  assign executer_io_dinst_cond_bits = issuer_io_deq_bits_cond_bits; // @[Proc.scala 201:21]
  assign executer_io_dinst_is32bit = issuer_io_deq_bits_is32bit; // @[Proc.scala 201:21]
  assign executer_io_dinst_itype = issuer_io_deq_bits_itype; // @[Proc.scala 201:21]
  assign executer_io_dinst_op = issuer_io_deq_bits_op; // @[Proc.scala 201:21]
  assign executer_io_dinst_nzcv_valid = issuer_io_deq_bits_nzcv_valid; // @[Proc.scala 201:21]
  assign executer_io_dinst_nzcv_bits = issuer_io_deq_bits_nzcv_bits; // @[Proc.scala 201:21]
  assign executer_io_rVal1 = issuer_io_deq_bits_tag ? rfileVec_1_rs1_data : rfileVec_0_rs1_data; // @[Proc.scala 202:21]
  assign executer_io_rVal2 = issuer_io_deq_bits_tag ? rfileVec_1_rs2_data : rfileVec_0_rs2_data; // @[Proc.scala 203:21]
  assign executer_io_rVal3 = issuer_io_deq_bits_tag ? rfileVec_1_rw_do : rfileVec_0_rw_do; // @[Proc.scala 204:21]
  assign executer_io_nzcv = issuer_io_deq_bits_tag ? pregsVec_1_NZCV : pregsVec_0_NZCV; // @[Proc.scala 205:20]
  assign brancher_io_dinst_rd_bits = issuer_io_deq_bits_rd_bits; // @[Proc.scala 208:21]
  assign brancher_io_dinst_imm = issuer_io_deq_bits_imm; // @[Proc.scala 208:21]
  assign brancher_io_dinst_is32bit = issuer_io_deq_bits_is32bit; // @[Proc.scala 208:21]
  assign brancher_io_dinst_itype = issuer_io_deq_bits_itype; // @[Proc.scala 208:21]
  assign brancher_io_dinst_op = issuer_io_deq_bits_op; // @[Proc.scala 208:21]
  assign brancher_io_rVal1 = issuer_io_deq_bits_tag ? rfileVec_1_rs1_data : rfileVec_0_rs1_data; // @[Proc.scala 209:21]
  assign brancher_io_rVal2 = issuer_io_deq_bits_tag ? rfileVec_1_rs2_data : rfileVec_0_rs2_data; // @[Proc.scala 210:21]
  assign brancher_io_cond = executer_io_condRes; // @[Proc.scala 211:20]
  assign brancher_io_pc = issuer_io_deq_bits_tag ? pregsVec_1_PC : pregsVec_0_PC; // @[Proc.scala 212:18]
  assign ldstU_io_dinst_rd_bits = issuer_io_deq_bits_rd_bits; // @[Proc.scala 215:18]
  assign ldstU_io_dinst_rs1 = issuer_io_deq_bits_rs1; // @[Proc.scala 215:18]
  assign ldstU_io_dinst_rs2 = issuer_io_deq_bits_rs2; // @[Proc.scala 215:18]
  assign ldstU_io_dinst_imm = issuer_io_deq_bits_imm; // @[Proc.scala 215:18]
  assign ldstU_io_dinst_shift_val_valid = issuer_io_deq_bits_shift_val_valid; // @[Proc.scala 215:18]
  assign ldstU_io_dinst_shift_val_bits = issuer_io_deq_bits_shift_val_bits; // @[Proc.scala 215:18]
  assign ldstU_io_dinst_itype = issuer_io_deq_bits_itype; // @[Proc.scala 215:18]
  assign ldstU_io_dinst_op = issuer_io_deq_bits_op; // @[Proc.scala 215:18]
  assign ldstU_io_rVal1 = issuer_io_deq_bits_tag ? rfileVec_1_rs1_data : rfileVec_0_rs1_data; // @[Proc.scala 216:18]
  assign ldstU_io_rVal2 = issuer_io_deq_bits_tag ? rfileVec_1_rs2_data : rfileVec_0_rs2_data; // @[Proc.scala 217:18]
  assign commitReg_clock = clock;
  assign commitReg_reset = reset;
  assign commitReg_io_enq_valid = issuer_io_deq_valid; // @[Proc.scala 233:26]
  assign commitReg_io_enq_bits_exe_valid = executer_io_einst_valid; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_exe_bits_rd_valid = executer_io_einst_bits_rd_valid; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_exe_bits_rd_bits = executer_io_einst_bits_rd_bits; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_exe_bits_nzcv_valid = executer_io_einst_bits_nzcv_valid; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_exe_bits_nzcv_bits = executer_io_einst_bits_nzcv_bits; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_exe_bits_res = executer_io_einst_bits_res; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_br_valid = brancher_io_binst_valid; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_br_bits_pc = brancher_io_binst_bits_pc; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_br_bits_unalignedExcp = brancher_io_binst_bits_unalignedExcp; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_pcrel_valid = brancher_io_pcrel_valid; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_pcrel_bits_rd = brancher_io_pcrel_bits_rd; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_pcrel_bits_res = brancher_io_pcrel_bits_res; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_mem_valid = ldstU_io_minst_valid; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_mem_bits_size = ldstU_io_minst_bits_size; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_mem_bits_isPair = ldstU_io_minst_bits_isPair; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_mem_bits_isLoad = ldstU_io_minst_bits_isLoad; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_mem_bits_memReq_0_addr = ldstU_io_minst_bits_memReq_0_addr; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_mem_bits_memReq_0_reg = ldstU_io_minst_bits_memReq_0_reg; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_mem_bits_memReq_1_addr = ldstU_io_minst_bits_memReq_1_addr; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_mem_bits_memReq_1_reg = ldstU_io_minst_bits_memReq_1_reg; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_mem_bits_rd_res = ldstU_io_minst_bits_rd_res; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_mem_bits_rd_valid = ldstU_io_minst_bits_rd_valid; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_mem_bits_rd_bits = ldstU_io_minst_bits_rd_bits; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_mem_bits_unalignedExcpSP = ldstU_io_minst_bits_unalignedExcpSP; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_undef = ~issuer_io_deq_bits_inst32_valid; // @[Proc.scala 232:25]
  assign commitReg_io_enq_bits_tag = issuer_io_deq_bits_tag; // @[Proc.scala 232:25]
  assign commitReg_io_deq_ready = ~memArbiterData_io_busy; // @[Proc.scala 317:26]
  assign commitReg_io_flush = tpu_io_tpu2cpu_flush_valid & _T_67; // @[Proc.scala 334:22 Proc.scala 339:24]
  assign memArbiterInst_clock = clock;
  assign memArbiterInst_reset = reset;
  assign memArbiterInst_io_vaddr_valid = fetch_io_pc_valid; // @[Proc.scala 141:33]
  assign memArbiterInst_io_vaddr_bits = fetch_io_pc_bits; // @[Proc.scala 140:32]
  assign memArbiterInst_io_fillTLB_valid = tpu_io_tpu2cpu_fillTLB_valid; // @[Proc.scala 144:29]
  assign memArbiterInst_io_fillTLB_bits_vaddr = tpu_io_tpu2cpu_fillTLB_bits_vaddr; // @[Proc.scala 144:29]
  assign memArbiterInst_io_tlbPort_paddr = insnTLB_io_iPort_paddr; // @[Proc.scala 142:20]
  assign memArbiterInst_io_tlbPort_miss_valid = insnTLB_io_iPort_miss_valid; // @[Proc.scala 142:20]
  assign memArbiterInst_io_tlbPort_miss_bits_vaddr = insnTLB_io_iPort_miss_bits_vaddr; // @[Proc.scala 142:20]
  assign memArbiterInst_io_tlbPort_miss_bits_tlbIdx = insnTLB_io_iPort_miss_bits_tlbIdx; // @[Proc.scala 142:20]
  assign memArbiterData_clock = clock;
  assign memArbiterData_reset = reset;
  assign memArbiterData_io_commitEnq_ready = commitReg_io_enq_ready; // @[Proc.scala 237:31]
  assign memArbiterData_io_commitEnq_valid = commitReg_io_enq_valid; // @[Proc.scala 237:31]
  assign memArbiterData_io_commitEnq_bits_mem_valid = commitReg_io_enq_bits_mem_valid; // @[Proc.scala 237:31]
  assign memArbiterData_io_commitEnq_bits_mem_bits_memReq_0_addr = commitReg_io_enq_bits_mem_bits_memReq_0_addr; // @[Proc.scala 237:31]
  assign memArbiterData_io_commitDeq_bits_mem_bits_size = commitReg_io_deq_bits_mem_bits_size; // @[Proc.scala 238:31]
  assign memArbiterData_io_commitDeq_bits_mem_bits_isPair = commitReg_io_deq_bits_mem_bits_isPair; // @[Proc.scala 238:31]
  assign memArbiterData_io_commitDeq_bits_mem_bits_isLoad = commitReg_io_deq_bits_mem_bits_isLoad; // @[Proc.scala 238:31]
  assign memArbiterData_io_commitDeq_bits_mem_bits_memReq_0_addr = commitReg_io_deq_bits_mem_bits_memReq_0_addr; // @[Proc.scala 238:31]
  assign memArbiterData_io_commitDeq_bits_mem_bits_memReq_0_reg = commitReg_io_deq_bits_mem_bits_memReq_0_reg; // @[Proc.scala 238:31]
  assign memArbiterData_io_commitDeq_bits_mem_bits_memReq_1_addr = commitReg_io_deq_bits_mem_bits_memReq_1_addr; // @[Proc.scala 238:31]
  assign memArbiterData_io_commitDeq_bits_mem_bits_memReq_1_reg = commitReg_io_deq_bits_mem_bits_memReq_1_reg; // @[Proc.scala 238:31]
  assign memArbiterData_io_commitDeq_bits_mem_bits_rd_res = commitReg_io_deq_bits_mem_bits_rd_res; // @[Proc.scala 238:31]
  assign memArbiterData_io_commitDeq_bits_mem_bits_rd_valid = commitReg_io_deq_bits_mem_bits_rd_valid; // @[Proc.scala 238:31]
  assign memArbiterData_io_commitDeq_bits_mem_bits_rd_bits = commitReg_io_deq_bits_mem_bits_rd_bits; // @[Proc.scala 238:31]
  assign memArbiterData_io_fillTLB_valid = tpu_io_tpu2cpu_fillTLB_valid; // @[Proc.scala 239:29]
  assign memArbiterData_io_fillTLB_bits_vaddr = tpu_io_tpu2cpu_fillTLB_bits_vaddr; // @[Proc.scala 239:29]
  assign memArbiterData_io_memPort_DO = memory_portA_DO; // @[BramModules.scala 204:8 BramModules.scala 204:8]
  assign memArbiterData_io_tlbPort_paddr = insnTLB_io_dPort_paddr; // @[Proc.scala 309:20]
  assign memArbiterData_io_tlbPort_miss_valid = insnTLB_io_dPort_miss_valid; // @[Proc.scala 309:20]
  assign memArbiterData_io_tlbPort_miss_bits_vaddr = insnTLB_io_dPort_miss_bits_vaddr; // @[Proc.scala 309:20]
  assign memArbiterData_io_tlbPort_miss_bits_tlbIdx = insnTLB_io_dPort_miss_bits_tlbIdx; // @[Proc.scala 309:20]
  assign memArbiterData_io_rfile_rs1_data = commitTag ? rfileVec_1_rs1_data : rfileVec_0_rs1_data; // @[Proc.scala 241:36]
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
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
  `ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {2{`RANDOM}};
  pregsVec_0_PC = _RAND_0[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {2{`RANDOM}};
  pregsVec_0_SP = _RAND_1[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  pregsVec_0_NZCV = _RAND_2[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {2{`RANDOM}};
  pregsVec_1_PC = _RAND_3[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {2{`RANDOM}};
  pregsVec_1_SP = _RAND_4[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  pregsVec_1_NZCV = _RAND_5[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  fetchEn_0 = _RAND_6[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  fetchEn_1 = _RAND_7[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  sel32bit = _RAND_8[0:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (reset) begin
      pregsVec_0_PC <= 64'h0;
    end else if (tpu_io_tpu2cpu_freeze_valid) begin
      if (tpu_io_tpu2cpuStateReg_valid) begin
        if (_T_71) begin
          if (~tpu_io_tpu2cpu_freeze_tag) begin
            pregsVec_0_PC <= _pregsVec_tpu_io_tpu2cpu_freeze_tag_PC_0;
          end else if (~tpu_io_tpu2cpu_freeze_tag) begin
            if (tpu_io_tpu2cpu_freeze_tag) begin
              pregsVec_0_PC <= pregsVec_1_PC;
            end
          end else if (_T_43) begin
            if (~commitTag) begin
              if (_T_43) begin
                if (commitBr_valid) begin
                  pregsVec_0_PC <= commitBr_bits_pc;
                end else begin
                  pregsVec_0_PC <= _T_56;
                end
              end else if (commitTag) begin
                pregsVec_0_PC <= pregsVec_1_PC;
              end
            end
          end
        end else if (~tpu_io_tpu2cpu_freeze_tag) begin
          if (tpu_io_tpu2cpu_freeze_tag) begin
            pregsVec_0_PC <= pregsVec_1_PC;
          end
        end else if (_T_43) begin
          if (~commitTag) begin
            if (_T_43) begin
              if (commitBr_valid) begin
                pregsVec_0_PC <= commitBr_bits_pc;
              end else begin
                pregsVec_0_PC <= _T_56;
              end
            end else if (commitTag) begin
              pregsVec_0_PC <= pregsVec_1_PC;
            end
          end
        end
      end else if (~tpu_io_tpu2cpu_freeze_tag) begin
        if (tpu_io_tpu2cpu_freeze_tag) begin
          pregsVec_0_PC <= pregsVec_1_PC;
        end
      end else if (_T_43) begin
        if (~commitTag) begin
          if (_T_43) begin
            if (commitBr_valid) begin
              pregsVec_0_PC <= commitBr_bits_pc;
            end else begin
              pregsVec_0_PC <= _T_56;
            end
          end else if (commitTag) begin
            pregsVec_0_PC <= pregsVec_1_PC;
          end
        end
      end
    end else if (_T_43) begin
      if (~commitTag) begin
        if (_T_43) begin
          if (commitBr_valid) begin
            pregsVec_0_PC <= commitBr_bits_pc;
          end else begin
            pregsVec_0_PC <= _T_56;
          end
        end else if (commitTag) begin
          pregsVec_0_PC <= pregsVec_1_PC;
        end
      end
    end
    if (reset) begin
      pregsVec_0_SP <= 64'h0;
    end else if (tpu_io_tpu2cpu_freeze_valid) begin
      if (tpu_io_tpu2cpuStateReg_valid) begin
        if (_T_71) begin
          if (~tpu_io_tpu2cpu_freeze_tag) begin
            if (tpu_io_tpu2cpu_freeze_tag) begin
              pregsVec_0_SP <= pregsVec_1_SP;
            end
          end
        end else if (_T_72) begin
          if (~tpu_io_tpu2cpu_freeze_tag) begin
            pregsVec_0_SP <= _pregsVec_tpu_io_tpu2cpu_freeze_tag_SP_0;
          end else if (~tpu_io_tpu2cpu_freeze_tag) begin
            if (tpu_io_tpu2cpu_freeze_tag) begin
              pregsVec_0_SP <= pregsVec_1_SP;
            end
          end
        end else if (~tpu_io_tpu2cpu_freeze_tag) begin
          if (tpu_io_tpu2cpu_freeze_tag) begin
            pregsVec_0_SP <= pregsVec_1_SP;
          end
        end
      end else if (~tpu_io_tpu2cpu_freeze_tag) begin
        if (tpu_io_tpu2cpu_freeze_tag) begin
          pregsVec_0_SP <= pregsVec_1_SP;
        end
      end
    end
    if (reset) begin
      pregsVec_0_NZCV <= 4'h0;
    end else if (tpu_io_tpu2cpu_freeze_valid) begin
      if (tpu_io_tpu2cpuStateReg_valid) begin
        if (_T_71) begin
          if (~tpu_io_tpu2cpu_freeze_tag) begin
            if (tpu_io_tpu2cpu_freeze_tag) begin
              pregsVec_0_NZCV <= pregsVec_1_NZCV;
            end
          end else if (_T_43) begin
            if (_T_50) begin
              if (~commitTag) begin
                pregsVec_0_NZCV <= commitExec_bits_nzcv_bits;
              end
            end
          end
        end else if (_T_72) begin
          if (~tpu_io_tpu2cpu_freeze_tag) begin
            if (tpu_io_tpu2cpu_freeze_tag) begin
              pregsVec_0_NZCV <= pregsVec_1_NZCV;
            end
          end else if (_T_43) begin
            if (_T_50) begin
              if (~commitTag) begin
                pregsVec_0_NZCV <= commitExec_bits_nzcv_bits;
              end
            end
          end
        end else if (_T_73) begin
          if (~tpu_io_tpu2cpu_freeze_tag) begin
            pregsVec_0_NZCV <= _pregsVec_tpu_io_tpu2cpu_freeze_tag_NZCV_0;
          end else if (~tpu_io_tpu2cpu_freeze_tag) begin
            if (tpu_io_tpu2cpu_freeze_tag) begin
              pregsVec_0_NZCV <= pregsVec_1_NZCV;
            end
          end else if (_T_43) begin
            if (_T_50) begin
              if (~commitTag) begin
                pregsVec_0_NZCV <= commitExec_bits_nzcv_bits;
              end
            end
          end
        end else if (~tpu_io_tpu2cpu_freeze_tag) begin
          if (tpu_io_tpu2cpu_freeze_tag) begin
            pregsVec_0_NZCV <= pregsVec_1_NZCV;
          end
        end else if (_T_43) begin
          if (_T_50) begin
            if (~commitTag) begin
              pregsVec_0_NZCV <= commitExec_bits_nzcv_bits;
            end
          end
        end
      end else begin
        pregsVec_0_NZCV <= _GEN_222;
      end
    end else begin
      pregsVec_0_NZCV <= _GEN_128;
    end
    if (reset) begin
      pregsVec_1_PC <= 64'h0;
    end else if (tpu_io_tpu2cpu_freeze_valid) begin
      if (tpu_io_tpu2cpuStateReg_valid) begin
        if (_T_71) begin
          if (tpu_io_tpu2cpu_freeze_tag) begin
            pregsVec_1_PC <= _pregsVec_tpu_io_tpu2cpu_freeze_tag_PC_0;
          end else if (tpu_io_tpu2cpu_freeze_tag) begin
            if (!(tpu_io_tpu2cpu_freeze_tag)) begin
              pregsVec_1_PC <= pregsVec_0_PC;
            end
          end else if (_T_43) begin
            if (commitTag) begin
              pregsVec_1_PC <= nextPC;
            end
          end
        end else if (tpu_io_tpu2cpu_freeze_tag) begin
          pregsVec_1_PC <= _GEN_181;
        end else if (_T_43) begin
          if (commitTag) begin
            pregsVec_1_PC <= nextPC;
          end
        end
      end else if (tpu_io_tpu2cpu_freeze_tag) begin
        pregsVec_1_PC <= _GEN_181;
      end else if (_T_43) begin
        if (commitTag) begin
          pregsVec_1_PC <= nextPC;
        end
      end
    end else if (_T_43) begin
      if (commitTag) begin
        pregsVec_1_PC <= nextPC;
      end
    end
    if (reset) begin
      pregsVec_1_SP <= 64'h0;
    end else if (tpu_io_tpu2cpu_freeze_valid) begin
      if (tpu_io_tpu2cpuStateReg_valid) begin
        if (_T_71) begin
          if (tpu_io_tpu2cpu_freeze_tag) begin
            pregsVec_1_SP <= _GEN_182;
          end
        end else if (_T_72) begin
          if (tpu_io_tpu2cpu_freeze_tag) begin
            pregsVec_1_SP <= _pregsVec_tpu_io_tpu2cpu_freeze_tag_SP_0;
          end else if (tpu_io_tpu2cpu_freeze_tag) begin
            pregsVec_1_SP <= _GEN_182;
          end
        end else if (tpu_io_tpu2cpu_freeze_tag) begin
          pregsVec_1_SP <= _GEN_182;
        end
      end else if (tpu_io_tpu2cpu_freeze_tag) begin
        pregsVec_1_SP <= _GEN_182;
      end
    end
    if (reset) begin
      pregsVec_1_NZCV <= 4'h0;
    end else if (tpu_io_tpu2cpu_freeze_valid) begin
      if (tpu_io_tpu2cpuStateReg_valid) begin
        if (_T_71) begin
          if (tpu_io_tpu2cpu_freeze_tag) begin
            pregsVec_1_NZCV <= _GEN_183;
          end else if (_T_43) begin
            if (_T_50) begin
              if (commitTag) begin
                pregsVec_1_NZCV <= commitExec_bits_nzcv_bits;
              end
            end
          end
        end else if (_T_72) begin
          if (tpu_io_tpu2cpu_freeze_tag) begin
            pregsVec_1_NZCV <= _GEN_183;
          end else if (_T_43) begin
            if (_T_50) begin
              if (commitTag) begin
                pregsVec_1_NZCV <= commitExec_bits_nzcv_bits;
              end
            end
          end
        end else if (_T_73) begin
          if (tpu_io_tpu2cpu_freeze_tag) begin
            pregsVec_1_NZCV <= _pregsVec_tpu_io_tpu2cpu_freeze_tag_NZCV_0;
          end else if (tpu_io_tpu2cpu_freeze_tag) begin
            pregsVec_1_NZCV <= _GEN_183;
          end else if (_T_43) begin
            if (_T_50) begin
              if (commitTag) begin
                pregsVec_1_NZCV <= commitExec_bits_nzcv_bits;
              end
            end
          end
        end else if (tpu_io_tpu2cpu_freeze_tag) begin
          pregsVec_1_NZCV <= _GEN_183;
        end else if (_T_43) begin
          if (_T_50) begin
            if (commitTag) begin
              pregsVec_1_NZCV <= commitExec_bits_nzcv_bits;
            end
          end
        end
      end else begin
        pregsVec_1_NZCV <= _GEN_223;
      end
    end else begin
      pregsVec_1_NZCV <= _GEN_129;
    end
    if (reset) begin
      fetchEn_0 <= 1'h0;
    end else if (tpu_io_tpu2cpu_flush_valid) begin
      if (~tpu_io_tpu2cpu_flush_tag) begin
        fetchEn_0 <= 1'h0;
      end else if (tpu_io_tpu2cpu_fire_valid) begin
        fetchEn_0 <= _GEN_161;
      end else if (tpu_io_tpu2cpu_freeze_valid) begin
        if (~tpu_io_tpu2cpu_freeze_tag) begin
          fetchEn_0 <= 1'h0;
        end else begin
          fetchEn_0 <= _GEN_156;
        end
      end else begin
        fetchEn_0 <= _GEN_156;
      end
    end else if (tpu_io_tpu2cpu_fire_valid) begin
      fetchEn_0 <= _GEN_161;
    end else if (tpu_io_tpu2cpu_freeze_valid) begin
      if (~tpu_io_tpu2cpu_freeze_tag) begin
        fetchEn_0 <= 1'h0;
      end else begin
        fetchEn_0 <= _GEN_156;
      end
    end else begin
      fetchEn_0 <= _GEN_156;
    end
    if (reset) begin
      fetchEn_1 <= 1'h0;
    end else if (tpu_io_tpu2cpu_flush_valid) begin
      if (tpu_io_tpu2cpu_flush_tag) begin
        fetchEn_1 <= 1'h0;
      end else if (tpu_io_tpu2cpu_fire_valid) begin
        fetchEn_1 <= _GEN_162;
      end else if (tpu_io_tpu2cpu_freeze_valid) begin
        if (tpu_io_tpu2cpu_freeze_tag) begin
          fetchEn_1 <= 1'h0;
        end else if (insnTLB_io_iPort_miss_valid) begin
          if (fetch_io_pc_tag) begin
            fetchEn_1 <= 1'h0;
          end
        end
      end else if (insnTLB_io_iPort_miss_valid) begin
        if (fetch_io_pc_tag) begin
          fetchEn_1 <= 1'h0;
        end
      end
    end else if (tpu_io_tpu2cpu_fire_valid) begin
      fetchEn_1 <= _GEN_162;
    end else if (tpu_io_tpu2cpu_freeze_valid) begin
      if (tpu_io_tpu2cpu_freeze_tag) begin
        fetchEn_1 <= 1'h0;
      end else if (insnTLB_io_iPort_miss_valid) begin
        if (fetch_io_pc_tag) begin
          fetchEn_1 <= 1'h0;
        end
      end
    end else if (insnTLB_io_iPort_miss_valid) begin
      if (fetch_io_pc_tag) begin
        fetchEn_1 <= 1'h0;
      end
    end
    sel32bit <= fetch_io_pc_bits[2];
  end
endmodule
