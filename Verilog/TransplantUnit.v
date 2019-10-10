module TransplantUnit( // @[:@256.2]
  input         clock, // @[:@257.4]
  input         reset, // @[:@258.4]
  input         io_host2tpu_fire_tag, // @[:@259.4]
  input         io_host2tpu_fire_valid, // @[:@259.4]
  output        io_host2tpu_done_tag, // @[:@259.4]
  output        io_host2tpu_done_valid, // @[:@259.4]
  input         io_host2tpu_getState_tag, // @[:@259.4]
  input         io_host2tpu_getState_valid, // @[:@259.4]
  output        io_tpu2cpu_flush_tag, // @[:@259.4]
  output        io_tpu2cpu_flush_valid, // @[:@259.4]
  output        io_tpu2cpu_fire_tag, // @[:@259.4]
  output        io_tpu2cpu_fire_valid, // @[:@259.4]
  output        io_tpu2cpu_freeze_tag, // @[:@259.4]
  output        io_tpu2cpu_freeze_valid, // @[:@259.4]
  input         io_tpu2cpu_done_tag, // @[:@259.4]
  input         io_tpu2cpu_done_valid, // @[:@259.4]
  output        io_tpu2cpuStateReg_valid, // @[:@259.4]
  output [1:0]  io_tpu2cpuStateReg_bits, // @[:@259.4]
  output [63:0] io_tpu2cpuState_PC, // @[:@259.4]
  output [31:0] io_tpu2cpuState_SP, // @[:@259.4]
  output [31:0] io_tpu2cpuState_EL, // @[:@259.4]
  output [3:0]  io_tpu2cpuState_NZCV, // @[:@259.4]
  input  [63:0] io_cpu2tpuState_PC, // @[:@259.4]
  input  [31:0] io_cpu2tpuState_SP, // @[:@259.4]
  input  [31:0] io_cpu2tpuState_EL, // @[:@259.4]
  input  [3:0]  io_cpu2tpuState_NZCV, // @[:@259.4]
  output [4:0]  io_rfile_rs1_addr, // @[:@259.4]
  input  [63:0] io_rfile_rs1_data, // @[:@259.4]
  output [4:0]  io_rfile_waddr, // @[:@259.4]
  output [63:0] io_rfile_wdata, // @[:@259.4]
  output        io_rfile_wen, // @[:@259.4]
  output        io_stateBRAM_writeEn, // @[:@259.4]
  output [9:0]  io_stateBRAM_addr, // @[:@259.4]
  output [35:0] io_stateBRAM_dataIn, // @[:@259.4]
  input  [35:0] io_stateBRAM_dataOut // @[:@259.4]
);
  reg [6:0] bramOFFST; // @[transplant.scala 83:26:@261.4]
  reg [31:0] _RAND_0;
  wire [31:0] bramOut; // @[transplant.scala 84:50:@262.4]
  reg [31:0] bramOut1CD; // @[transplant.scala 86:27:@265.4]
  reg [31:0] _RAND_1;
  wire [31:0] regDataInMSB; // @[transplant.scala 91:31:@270.4]
  wire [31:0] regDataInLSB; // @[transplant.scala 92:31:@271.4]
  wire [6:0] regAddr; // @[transplant.scala 95:27:@272.4]
  reg  state; // @[transplant.scala 101:22:@275.4]
  reg [31:0] _RAND_2;
  reg  stateDir; // @[transplant.scala 102:25:@276.4]
  reg [31:0] _RAND_3;
  reg [1:0] stateRegType; // @[transplant.scala 103:29:@277.4]
  reg [31:0] _RAND_4;
  reg  freeze; // @[transplant.scala 106:23:@278.4]
  reg [31:0] _RAND_5;
  reg  freezeTag; // @[transplant.scala 107:26:@279.4]
  reg [31:0] _RAND_6;
  wire  _T_148; // @[Conditional.scala 37:30:@287.4]
  wire  _GEN_0; // @[transplant.scala 143:46:@308.10]
  wire  _GEN_1; // @[transplant.scala 143:46:@308.10]
  wire  _GEN_2; // @[transplant.scala 143:46:@308.10]
  wire [1:0] _GEN_3; // @[transplant.scala 143:46:@308.10]
  wire [6:0] _GEN_4; // @[transplant.scala 143:46:@308.10]
  wire  _GEN_6; // @[transplant.scala 143:46:@308.10]
  wire  _GEN_7; // @[transplant.scala 134:41:@298.8]
  wire  _GEN_8; // @[transplant.scala 134:41:@298.8]
  wire  _GEN_9; // @[transplant.scala 134:41:@298.8]
  wire [1:0] _GEN_10; // @[transplant.scala 134:41:@298.8]
  wire [6:0] _GEN_11; // @[transplant.scala 134:41:@298.8]
  wire  _GEN_12; // @[transplant.scala 134:41:@298.8]
  wire  _GEN_13; // @[transplant.scala 134:41:@298.8]
  wire  _GEN_14; // @[transplant.scala 126:36:@289.6]
  wire  _GEN_15; // @[transplant.scala 126:36:@289.6]
  wire  _GEN_16; // @[transplant.scala 126:36:@289.6]
  wire [1:0] _GEN_17; // @[transplant.scala 126:36:@289.6]
  wire [6:0] _GEN_18; // @[transplant.scala 126:36:@289.6]
  wire  _GEN_19; // @[transplant.scala 126:36:@289.6]
  wire  _GEN_20; // @[transplant.scala 126:36:@289.6]
  wire [7:0] _T_159; // @[transplant.scala 156:30:@321.8]
  wire [6:0] _T_160; // @[transplant.scala 156:30:@322.8]
  wire  _T_162; // @[transplant.scala 157:22:@324.8]
  wire  _T_164; // @[transplant.scala 159:29:@329.10]
  wire  _T_166; // @[transplant.scala 161:29:@334.12]
  wire  _T_167; // @[transplant.scala 163:32:@339.14]
  wire  _T_168; // @[transplant.scala 164:23:@341.16]
  wire  _GEN_23; // @[transplant.scala 164:39:@342.16]
  wire  _GEN_24; // @[transplant.scala 163:44:@340.14]
  wire  _GEN_25; // @[transplant.scala 163:44:@340.14]
  wire [6:0] _GEN_26; // @[transplant.scala 163:44:@340.14]
  wire [1:0] _GEN_27; // @[transplant.scala 163:44:@340.14]
  wire  _GEN_28; // @[transplant.scala 163:44:@340.14]
  wire  _GEN_29; // @[transplant.scala 163:44:@340.14]
  wire [1:0] _GEN_30; // @[transplant.scala 161:58:@335.12]
  wire  _GEN_31; // @[transplant.scala 161:58:@335.12]
  wire  _GEN_32; // @[transplant.scala 161:58:@335.12]
  wire [6:0] _GEN_33; // @[transplant.scala 161:58:@335.12]
  wire  _GEN_34; // @[transplant.scala 161:58:@335.12]
  wire  _GEN_35; // @[transplant.scala 161:58:@335.12]
  wire [1:0] _GEN_36; // @[transplant.scala 159:60:@330.10]
  wire  _GEN_37; // @[transplant.scala 159:60:@330.10]
  wire  _GEN_38; // @[transplant.scala 159:60:@330.10]
  wire [6:0] _GEN_39; // @[transplant.scala 159:60:@330.10]
  wire  _GEN_40; // @[transplant.scala 159:60:@330.10]
  wire  _GEN_41; // @[transplant.scala 159:60:@330.10]
  wire [1:0] _GEN_42; // @[transplant.scala 157:49:@325.8]
  wire  _GEN_43; // @[transplant.scala 157:49:@325.8]
  wire  _GEN_44; // @[transplant.scala 157:49:@325.8]
  wire [6:0] _GEN_45; // @[transplant.scala 157:49:@325.8]
  wire  _GEN_46; // @[transplant.scala 157:49:@325.8]
  wire  _GEN_47; // @[transplant.scala 157:49:@325.8]
  wire [6:0] _GEN_48; // @[Conditional.scala 39:67:@320.6]
  wire [1:0] _GEN_49; // @[Conditional.scala 39:67:@320.6]
  wire  _GEN_50; // @[Conditional.scala 39:67:@320.6]
  wire  _GEN_51; // @[Conditional.scala 39:67:@320.6]
  wire  _GEN_52; // @[Conditional.scala 39:67:@320.6]
  wire  _GEN_53; // @[Conditional.scala 39:67:@320.6]
  wire  _GEN_54; // @[Conditional.scala 40:58:@288.4]
  wire  _GEN_55; // @[Conditional.scala 40:58:@288.4]
  wire  _GEN_56; // @[Conditional.scala 40:58:@288.4]
  wire [1:0] _GEN_57; // @[Conditional.scala 40:58:@288.4]
  wire [6:0] _GEN_58; // @[Conditional.scala 40:58:@288.4]
  wire  _GEN_59; // @[Conditional.scala 40:58:@288.4]
  wire  _T_176; // @[transplant.scala 175:71:@358.4]
  wire  _T_178; // @[transplant.scala 177:21:@362.4]
  wire  _T_179; // @[transplant.scala 178:45:@364.6]
  wire [31:0] _T_180; // @[transplant.scala 178:35:@365.6]
  wire  _T_181; // @[transplant.scala 179:27:@369.6]
  wire [31:0] _T_183; // @[transplant.scala 180:68:@372.8]
  wire [31:0] _T_184; // @[transplant.scala 180:94:@373.8]
  wire [31:0] _T_185; // @[transplant.scala 180:35:@374.8]
  wire  _T_186; // @[transplant.scala 181:27:@378.8]
  wire  _T_188; // @[transplant.scala 182:59:@380.10]
  wire  _T_189; // @[transplant.scala 182:82:@381.10]
  wire [6:0] _T_193; // @[Cat.scala 30:58:@385.10]
  wire [6:0] _GEN_63; // @[transplant.scala 181:45:@379.8]
  wire [31:0] _GEN_64; // @[transplant.scala 179:37:@370.6]
  wire [31:0] _GEN_65; // @[transplant.scala 177:34:@363.4]
  reg  _T_199; // @[transplant.scala 188:26:@394.4]
  reg [31:0] _RAND_7;
  reg [6:0] _T_201; // @[transplant.scala 189:28:@397.4]
  reg [31:0] _RAND_8;
  wire  _T_202; // @[transplant.scala 222:44:@402.4]
  wire  _T_203; // @[transplant.scala 221:44:@404.4]
  reg [1:0] _T_206; // @[transplant.scala 197:37:@408.4]
  reg [31:0] _RAND_9;
  reg  _T_209; // @[transplant.scala 198:38:@412.4]
  reg [31:0] _RAND_10;
  reg  freezeTag1D; // @[transplant.scala 201:28:@415.4]
  reg [31:0] _RAND_11;
  reg  _T_212; // @[transplant.scala 202:35:@417.4]
  reg [31:0] _RAND_12;
  reg  _T_214; // @[transplant.scala 206:36:@423.4]
  reg [31:0] _RAND_13;
  reg  _T_216; // @[transplant.scala 208:36:@427.4]
  reg [31:0] _RAND_14;
  assign bramOut = io_stateBRAM_dataOut[31:0]; // @[transplant.scala 84:50:@262.4]
  assign regDataInMSB = io_rfile_rs1_data[63:32]; // @[transplant.scala 91:31:@270.4]
  assign regDataInLSB = io_rfile_rs1_data[31:0]; // @[transplant.scala 92:31:@271.4]
  assign regAddr = bramOFFST >> 1'h1; // @[transplant.scala 95:27:@272.4]
  assign _T_148 = 1'h0 == state; // @[Conditional.scala 37:30:@287.4]
  assign _GEN_0 = io_host2tpu_getState_valid ? 1'h1 : freeze; // @[transplant.scala 143:46:@308.10]
  assign _GEN_1 = io_host2tpu_getState_valid ? io_host2tpu_getState_tag : freezeTag; // @[transplant.scala 143:46:@308.10]
  assign _GEN_2 = io_host2tpu_getState_valid ? 1'h1 : stateDir; // @[transplant.scala 143:46:@308.10]
  assign _GEN_3 = io_host2tpu_getState_valid ? 2'h1 : stateRegType; // @[transplant.scala 143:46:@308.10]
  assign _GEN_4 = io_host2tpu_getState_valid ? 7'h0 : bramOFFST; // @[transplant.scala 143:46:@308.10]
  assign _GEN_6 = io_host2tpu_getState_valid ? 1'h1 : state; // @[transplant.scala 143:46:@308.10]
  assign _GEN_7 = io_tpu2cpu_done_valid ? 1'h1 : _GEN_0; // @[transplant.scala 134:41:@298.8]
  assign _GEN_8 = io_tpu2cpu_done_valid ? io_tpu2cpu_done_tag : _GEN_1; // @[transplant.scala 134:41:@298.8]
  assign _GEN_9 = io_tpu2cpu_done_valid ? 1'h1 : _GEN_2; // @[transplant.scala 134:41:@298.8]
  assign _GEN_10 = io_tpu2cpu_done_valid ? 2'h1 : _GEN_3; // @[transplant.scala 134:41:@298.8]
  assign _GEN_11 = io_tpu2cpu_done_valid ? 7'h0 : _GEN_4; // @[transplant.scala 134:41:@298.8]
  assign _GEN_12 = io_tpu2cpu_done_valid ? 1'h1 : io_host2tpu_getState_valid; // @[transplant.scala 134:41:@298.8]
  assign _GEN_13 = io_tpu2cpu_done_valid ? 1'h1 : _GEN_6; // @[transplant.scala 134:41:@298.8]
  assign _GEN_14 = io_host2tpu_fire_valid ? 1'h1 : _GEN_7; // @[transplant.scala 126:36:@289.6]
  assign _GEN_15 = io_host2tpu_fire_valid ? io_host2tpu_fire_tag : _GEN_8; // @[transplant.scala 126:36:@289.6]
  assign _GEN_16 = io_host2tpu_fire_valid ? 1'h0 : _GEN_9; // @[transplant.scala 126:36:@289.6]
  assign _GEN_17 = io_host2tpu_fire_valid ? 2'h1 : _GEN_10; // @[transplant.scala 126:36:@289.6]
  assign _GEN_18 = io_host2tpu_fire_valid ? 7'h0 : _GEN_11; // @[transplant.scala 126:36:@289.6]
  assign _GEN_19 = io_host2tpu_fire_valid ? 1'h1 : _GEN_13; // @[transplant.scala 126:36:@289.6]
  assign _GEN_20 = io_host2tpu_fire_valid ? 1'h0 : _GEN_12; // @[transplant.scala 126:36:@289.6]
  assign _T_159 = bramOFFST + 7'h1; // @[transplant.scala 156:30:@321.8]
  assign _T_160 = bramOFFST + 7'h1; // @[transplant.scala 156:30:@322.8]
  assign _T_162 = bramOFFST == 7'h3f; // @[transplant.scala 157:22:@324.8]
  assign _T_164 = bramOFFST == 7'h41; // @[transplant.scala 159:29:@329.10]
  assign _T_166 = bramOFFST == 7'h42; // @[transplant.scala 161:29:@334.12]
  assign _T_167 = stateRegType == 2'h0; // @[transplant.scala 163:32:@339.14]
  assign _T_168 = stateDir == 1'h0; // @[transplant.scala 164:23:@341.16]
  assign _GEN_23 = _T_168 ? 1'h0 : stateDir; // @[transplant.scala 164:39:@342.16]
  assign _GEN_24 = _T_167 ? _T_168 : 1'h0; // @[transplant.scala 163:44:@340.14]
  assign _GEN_25 = _T_167 ? _GEN_23 : 1'h0; // @[transplant.scala 163:44:@340.14]
  assign _GEN_26 = _T_167 ? 7'h0 : _T_160; // @[transplant.scala 163:44:@340.14]
  assign _GEN_27 = _T_167 ? 2'h0 : stateRegType; // @[transplant.scala 163:44:@340.14]
  assign _GEN_28 = _T_167 ? 1'h0 : freeze; // @[transplant.scala 163:44:@340.14]
  assign _GEN_29 = _T_167 ? 1'h0 : state; // @[transplant.scala 163:44:@340.14]
  assign _GEN_30 = _T_166 ? 2'h0 : _GEN_27; // @[transplant.scala 161:58:@335.12]
  assign _GEN_31 = _T_166 ? 1'h0 : _GEN_24; // @[transplant.scala 161:58:@335.12]
  assign _GEN_32 = _T_166 ? 1'h0 : _GEN_25; // @[transplant.scala 161:58:@335.12]
  assign _GEN_33 = _T_166 ? _T_160 : _GEN_26; // @[transplant.scala 161:58:@335.12]
  assign _GEN_34 = _T_166 ? freeze : _GEN_28; // @[transplant.scala 161:58:@335.12]
  assign _GEN_35 = _T_166 ? state : _GEN_29; // @[transplant.scala 161:58:@335.12]
  assign _GEN_36 = _T_164 ? 2'h3 : _GEN_30; // @[transplant.scala 159:60:@330.10]
  assign _GEN_37 = _T_164 ? 1'h0 : _GEN_31; // @[transplant.scala 159:60:@330.10]
  assign _GEN_38 = _T_164 ? 1'h0 : _GEN_32; // @[transplant.scala 159:60:@330.10]
  assign _GEN_39 = _T_164 ? _T_160 : _GEN_33; // @[transplant.scala 159:60:@330.10]
  assign _GEN_40 = _T_164 ? freeze : _GEN_34; // @[transplant.scala 159:60:@330.10]
  assign _GEN_41 = _T_164 ? state : _GEN_35; // @[transplant.scala 159:60:@330.10]
  assign _GEN_42 = _T_162 ? 2'h2 : _GEN_36; // @[transplant.scala 157:49:@325.8]
  assign _GEN_43 = _T_162 ? 1'h0 : _GEN_37; // @[transplant.scala 157:49:@325.8]
  assign _GEN_44 = _T_162 ? 1'h0 : _GEN_38; // @[transplant.scala 157:49:@325.8]
  assign _GEN_45 = _T_162 ? _T_160 : _GEN_39; // @[transplant.scala 157:49:@325.8]
  assign _GEN_46 = _T_162 ? freeze : _GEN_40; // @[transplant.scala 157:49:@325.8]
  assign _GEN_47 = _T_162 ? state : _GEN_41; // @[transplant.scala 157:49:@325.8]
  assign _GEN_48 = state ? _GEN_45 : bramOFFST; // @[Conditional.scala 39:67:@320.6]
  assign _GEN_49 = state ? _GEN_42 : stateRegType; // @[Conditional.scala 39:67:@320.6]
  assign _GEN_50 = state ? _GEN_43 : 1'h0; // @[Conditional.scala 39:67:@320.6]
  assign _GEN_51 = state ? _GEN_44 : 1'h0; // @[Conditional.scala 39:67:@320.6]
  assign _GEN_52 = state ? _GEN_46 : freeze; // @[Conditional.scala 39:67:@320.6]
  assign _GEN_53 = state ? _GEN_47 : state; // @[Conditional.scala 39:67:@320.6]
  assign _GEN_54 = _T_148 ? _GEN_14 : _GEN_52; // @[Conditional.scala 40:58:@288.4]
  assign _GEN_55 = _T_148 ? _GEN_15 : freezeTag; // @[Conditional.scala 40:58:@288.4]
  assign _GEN_56 = _T_148 ? _GEN_16 : stateDir; // @[Conditional.scala 40:58:@288.4]
  assign _GEN_57 = _T_148 ? _GEN_17 : _GEN_49; // @[Conditional.scala 40:58:@288.4]
  assign _GEN_58 = _T_148 ? _GEN_18 : _GEN_48; // @[Conditional.scala 40:58:@288.4]
  assign _GEN_59 = _T_148 ? _GEN_19 : _GEN_53; // @[Conditional.scala 40:58:@288.4]
  assign _T_176 = stateRegType != 2'h0; // @[transplant.scala 175:71:@358.4]
  assign _T_178 = stateRegType == 2'h1; // @[transplant.scala 177:21:@362.4]
  assign _T_179 = bramOFFST[0]; // @[transplant.scala 178:45:@364.6]
  assign _T_180 = _T_179 ? regDataInLSB : regDataInMSB; // @[transplant.scala 178:35:@365.6]
  assign _T_181 = stateRegType == 2'h2; // @[transplant.scala 179:27:@369.6]
  assign _T_183 = io_cpu2tpuState_PC[31:0]; // @[transplant.scala 180:68:@372.8]
  assign _T_184 = io_cpu2tpuState_PC[63:32]; // @[transplant.scala 180:94:@373.8]
  assign _T_185 = _T_179 ? _T_183 : _T_184; // @[transplant.scala 180:35:@374.8]
  assign _T_186 = stateRegType == 2'h3; // @[transplant.scala 181:27:@378.8]
  assign _T_188 = io_cpu2tpuState_SP[0]; // @[transplant.scala 182:59:@380.10]
  assign _T_189 = io_cpu2tpuState_EL[0]; // @[transplant.scala 182:82:@381.10]
  assign _T_193 = {1'h0,_T_188,_T_189,io_cpu2tpuState_NZCV}; // @[Cat.scala 30:58:@385.10]
  assign _GEN_63 = _T_186 ? _T_193 : 7'h0; // @[transplant.scala 181:45:@379.8]
  assign _GEN_64 = _T_181 ? _T_185 : {{25'd0}, _GEN_63}; // @[transplant.scala 179:37:@370.6]
  assign _GEN_65 = _T_178 ? _T_180 : _GEN_64; // @[transplant.scala 177:34:@363.4]
  assign _T_202 = bramOut[5]; // @[transplant.scala 222:44:@402.4]
  assign _T_203 = bramOut[4]; // @[transplant.scala 221:44:@404.4]
  assign io_host2tpu_done_tag = freezeTag1D; // @[transplant.scala 209:24:@430.4]
  assign io_host2tpu_done_valid = _T_216; // @[transplant.scala 208:26:@429.4]
  assign io_tpu2cpu_flush_tag = freezeTag1D; // @[transplant.scala 207:24:@426.4]
  assign io_tpu2cpu_flush_valid = _T_214; // @[transplant.scala 206:26:@425.4]
  assign io_tpu2cpu_fire_tag = freezeTag1D; // @[transplant.scala 203:23:@420.4]
  assign io_tpu2cpu_fire_valid = _T_212; // @[transplant.scala 202:25:@419.4]
  assign io_tpu2cpu_freeze_tag = freezeTag; // @[transplant.scala 205:25:@422.4]
  assign io_tpu2cpu_freeze_valid = freeze; // @[transplant.scala 204:27:@421.4]
  assign io_tpu2cpuStateReg_valid = _T_209; // @[transplant.scala 198:28:@414.4]
  assign io_tpu2cpuStateReg_bits = _T_206; // @[transplant.scala 197:27:@410.4]
  assign io_tpu2cpuState_PC = {bramOut1CD,bramOut}; // @[transplant.scala 192:22:@401.4]
  assign io_tpu2cpuState_SP = {{31'd0}, _T_202}; // @[transplant.scala 193:22:@403.4]
  assign io_tpu2cpuState_EL = {{31'd0}, _T_203}; // @[transplant.scala 194:22:@405.4]
  assign io_tpu2cpuState_NZCV = bramOut[3:0]; // @[transplant.scala 195:24:@407.4]
  assign io_rfile_rs1_addr = regAddr[4:0]; // @[transplant.scala 96:21:@273.4]
  assign io_rfile_waddr = _T_201[4:0]; // @[transplant.scala 189:18:@399.4]
  assign io_rfile_wdata = {bramOut1CD,bramOut}; // @[transplant.scala 190:18:@400.4]
  assign io_rfile_wen = _T_199; // @[transplant.scala 188:16:@396.4]
  assign io_stateBRAM_writeEn = stateDir & _T_176; // @[transplant.scala 175:28:@360.4]
  assign io_stateBRAM_addr = {{3'd0}, bramOFFST}; // @[transplant.scala 176:21:@361.4]
  assign io_stateBRAM_dataIn = {{4'd0}, _GEN_65}; // @[transplant.scala 178:29:@366.6 transplant.scala 180:29:@375.8 transplant.scala 182:29:@386.10 transplant.scala 184:29:@389.10]
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
  bramOFFST = _RAND_0[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  bramOut1CD = _RAND_1[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  state = _RAND_2[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  stateDir = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  stateRegType = _RAND_4[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  freeze = _RAND_5[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  freezeTag = _RAND_6[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  _T_199 = _RAND_7[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  _T_201 = _RAND_8[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  _T_206 = _RAND_9[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  _T_209 = _RAND_10[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {1{`RANDOM}};
  freezeTag1D = _RAND_11[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_12 = {1{`RANDOM}};
  _T_212 = _RAND_12[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_13 = {1{`RANDOM}};
  _T_214 = _RAND_13[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_14 = {1{`RANDOM}};
  _T_216 = _RAND_14[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (reset) begin
      bramOFFST <= 7'h0;
    end else begin
      if (_T_148) begin
        if (io_host2tpu_fire_valid) begin
          bramOFFST <= 7'h0;
        end else begin
          if (io_tpu2cpu_done_valid) begin
            bramOFFST <= 7'h0;
          end else begin
            if (io_host2tpu_getState_valid) begin
              bramOFFST <= 7'h0;
            end
          end
        end
      end else begin
        if (state) begin
          if (_T_162) begin
            bramOFFST <= _T_160;
          end else begin
            if (_T_164) begin
              bramOFFST <= _T_160;
            end else begin
              if (_T_166) begin
                bramOFFST <= _T_160;
              end else begin
                if (_T_167) begin
                  bramOFFST <= 7'h0;
                end else begin
                  bramOFFST <= _T_160;
                end
              end
            end
          end
        end
      end
    end
    bramOut1CD <= io_stateBRAM_dataOut[31:0];
    if (reset) begin
      state <= 1'h0;
    end else begin
      if (_T_148) begin
        if (io_host2tpu_fire_valid) begin
          state <= 1'h1;
        end else begin
          if (io_tpu2cpu_done_valid) begin
            state <= 1'h1;
          end else begin
            if (io_host2tpu_getState_valid) begin
              state <= 1'h1;
            end
          end
        end
      end else begin
        if (state) begin
          if (!(_T_162)) begin
            if (!(_T_164)) begin
              if (!(_T_166)) begin
                if (_T_167) begin
                  state <= 1'h0;
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      stateDir <= 1'h0;
    end else begin
      if (_T_148) begin
        if (io_host2tpu_fire_valid) begin
          stateDir <= 1'h0;
        end else begin
          if (io_tpu2cpu_done_valid) begin
            stateDir <= 1'h1;
          end else begin
            if (io_host2tpu_getState_valid) begin
              stateDir <= 1'h1;
            end
          end
        end
      end
    end
    if (reset) begin
      stateRegType <= 2'h0;
    end else begin
      if (_T_148) begin
        if (io_host2tpu_fire_valid) begin
          stateRegType <= 2'h1;
        end else begin
          if (io_tpu2cpu_done_valid) begin
            stateRegType <= 2'h1;
          end else begin
            if (io_host2tpu_getState_valid) begin
              stateRegType <= 2'h1;
            end
          end
        end
      end else begin
        if (state) begin
          if (_T_162) begin
            stateRegType <= 2'h2;
          end else begin
            if (_T_164) begin
              stateRegType <= 2'h3;
            end else begin
              if (_T_166) begin
                stateRegType <= 2'h0;
              end else begin
                if (_T_167) begin
                  stateRegType <= 2'h0;
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      freeze <= 1'h0;
    end else begin
      if (_T_148) begin
        if (io_host2tpu_fire_valid) begin
          freeze <= 1'h1;
        end else begin
          if (io_tpu2cpu_done_valid) begin
            freeze <= 1'h1;
          end else begin
            if (io_host2tpu_getState_valid) begin
              freeze <= 1'h1;
            end
          end
        end
      end else begin
        if (state) begin
          if (!(_T_162)) begin
            if (!(_T_164)) begin
              if (!(_T_166)) begin
                if (_T_167) begin
                  freeze <= 1'h0;
                end
              end
            end
          end
        end
      end
    end
    if (reset) begin
      freezeTag <= 1'h0;
    end else begin
      if (_T_148) begin
        if (io_host2tpu_fire_valid) begin
          freezeTag <= io_host2tpu_fire_tag;
        end else begin
          if (io_tpu2cpu_done_valid) begin
            freezeTag <= io_tpu2cpu_done_tag;
          end else begin
            if (io_host2tpu_getState_valid) begin
              freezeTag <= io_host2tpu_getState_tag;
            end
          end
        end
      end
    end
    _T_199 <= _T_168 & _T_178;
    _T_201 <= bramOFFST >> 1'h1;
    _T_206 <= stateRegType;
    _T_209 <= stateDir == 1'h0;
    freezeTag1D <= freezeTag;
    if (_T_148) begin
      _T_212 <= 1'h0;
    end else begin
      if (state) begin
        if (_T_162) begin
          _T_212 <= 1'h0;
        end else begin
          if (_T_164) begin
            _T_212 <= 1'h0;
          end else begin
            if (_T_166) begin
              _T_212 <= 1'h0;
            end else begin
              if (_T_167) begin
                _T_212 <= _T_168;
              end else begin
                _T_212 <= 1'h0;
              end
            end
          end
        end
      end else begin
        _T_212 <= 1'h0;
      end
    end
    if (_T_148) begin
      if (io_host2tpu_fire_valid) begin
        _T_214 <= 1'h0;
      end else begin
        if (io_tpu2cpu_done_valid) begin
          _T_214 <= 1'h1;
        end else begin
          _T_214 <= io_host2tpu_getState_valid;
        end
      end
    end else begin
      _T_214 <= 1'h0;
    end
    if (_T_148) begin
      _T_216 <= 1'h0;
    end else begin
      if (state) begin
        if (_T_162) begin
          _T_216 <= 1'h0;
        end else begin
          if (_T_164) begin
            _T_216 <= 1'h0;
          end else begin
            if (_T_166) begin
              _T_216 <= 1'h0;
            end else begin
              if (_T_167) begin
                if (_T_168) begin
                  _T_216 <= 1'h0;
                end else begin
                  _T_216 <= stateDir;
                end
              end else begin
                _T_216 <= 1'h0;
              end
            end
          end
        end
      end else begin
        _T_216 <= 1'h0;
      end
    end
  end
endmodule
