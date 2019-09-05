module TransplantUnit( // @[:@256.2]
  input         clock, // @[:@257.4]
  input         reset, // @[:@258.4]
  input         io_host2tpu_fire, // @[:@259.4]
  input         io_host2tpu_fireTag, // @[:@259.4]
  output        io_host2tpu_done, // @[:@259.4]
  output        io_host2tpu_doneTag, // @[:@259.4]
  output        io_tpu2cpu_flush, // @[:@259.4]
  output        io_tpu2cpu_freeze, // @[:@259.4]
  output        io_tpu2cpu_fire, // @[:@259.4]
  input         io_tpu2cpu_done, // @[:@259.4]
  output        io_tpu2cpu_flushTag, // @[:@259.4]
  output        io_tpu2cpu_fireTag, // @[:@259.4]
  output        io_tpu2cpu_freezeTag, // @[:@259.4]
  input         io_tpu2cpu_doneTag, // @[:@259.4]
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
  wire  _T_124; // @[Conditional.scala 37:30:@287.4]
  wire  _GEN_0; // @[transplant.scala 134:35:@298.8]
  wire  _GEN_1; // @[transplant.scala 134:35:@298.8]
  wire  _GEN_2; // @[transplant.scala 134:35:@298.8]
  wire [1:0] _GEN_3; // @[transplant.scala 134:35:@298.8]
  wire [6:0] _GEN_4; // @[transplant.scala 134:35:@298.8]
  wire  _GEN_6; // @[transplant.scala 134:35:@298.8]
  wire  _GEN_7; // @[transplant.scala 126:30:@289.6]
  wire  _GEN_8; // @[transplant.scala 126:30:@289.6]
  wire  _GEN_9; // @[transplant.scala 126:30:@289.6]
  wire [1:0] _GEN_10; // @[transplant.scala 126:30:@289.6]
  wire [6:0] _GEN_11; // @[transplant.scala 126:30:@289.6]
  wire  _GEN_12; // @[transplant.scala 126:30:@289.6]
  wire  _GEN_13; // @[transplant.scala 126:30:@289.6]
  wire [7:0] _T_132; // @[transplant.scala 147:30:@311.8]
  wire [6:0] _T_133; // @[transplant.scala 147:30:@312.8]
  wire  _T_135; // @[transplant.scala 148:22:@314.8]
  wire  _T_137; // @[transplant.scala 150:29:@319.10]
  wire  _T_139; // @[transplant.scala 152:29:@324.12]
  wire  _T_140; // @[transplant.scala 154:32:@329.14]
  wire  _T_141; // @[transplant.scala 155:23:@331.16]
  wire  _GEN_16; // @[transplant.scala 155:39:@332.16]
  wire  _GEN_17; // @[transplant.scala 154:44:@330.14]
  wire  _GEN_18; // @[transplant.scala 154:44:@330.14]
  wire [6:0] _GEN_19; // @[transplant.scala 154:44:@330.14]
  wire [1:0] _GEN_20; // @[transplant.scala 154:44:@330.14]
  wire  _GEN_21; // @[transplant.scala 154:44:@330.14]
  wire  _GEN_22; // @[transplant.scala 154:44:@330.14]
  wire [1:0] _GEN_23; // @[transplant.scala 152:58:@325.12]
  wire  _GEN_24; // @[transplant.scala 152:58:@325.12]
  wire  _GEN_25; // @[transplant.scala 152:58:@325.12]
  wire [6:0] _GEN_26; // @[transplant.scala 152:58:@325.12]
  wire  _GEN_27; // @[transplant.scala 152:58:@325.12]
  wire  _GEN_28; // @[transplant.scala 152:58:@325.12]
  wire [1:0] _GEN_29; // @[transplant.scala 150:60:@320.10]
  wire  _GEN_30; // @[transplant.scala 150:60:@320.10]
  wire  _GEN_31; // @[transplant.scala 150:60:@320.10]
  wire [6:0] _GEN_32; // @[transplant.scala 150:60:@320.10]
  wire  _GEN_33; // @[transplant.scala 150:60:@320.10]
  wire  _GEN_34; // @[transplant.scala 150:60:@320.10]
  wire [1:0] _GEN_35; // @[transplant.scala 148:49:@315.8]
  wire  _GEN_36; // @[transplant.scala 148:49:@315.8]
  wire  _GEN_37; // @[transplant.scala 148:49:@315.8]
  wire [6:0] _GEN_38; // @[transplant.scala 148:49:@315.8]
  wire  _GEN_39; // @[transplant.scala 148:49:@315.8]
  wire  _GEN_40; // @[transplant.scala 148:49:@315.8]
  wire [6:0] _GEN_41; // @[Conditional.scala 39:67:@310.6]
  wire [1:0] _GEN_42; // @[Conditional.scala 39:67:@310.6]
  wire  _GEN_43; // @[Conditional.scala 39:67:@310.6]
  wire  _GEN_44; // @[Conditional.scala 39:67:@310.6]
  wire  _GEN_45; // @[Conditional.scala 39:67:@310.6]
  wire  _GEN_46; // @[Conditional.scala 39:67:@310.6]
  wire  _GEN_47; // @[Conditional.scala 40:58:@288.4]
  wire  _GEN_48; // @[Conditional.scala 40:58:@288.4]
  wire  _GEN_49; // @[Conditional.scala 40:58:@288.4]
  wire [1:0] _GEN_50; // @[Conditional.scala 40:58:@288.4]
  wire [6:0] _GEN_51; // @[Conditional.scala 40:58:@288.4]
  wire  _GEN_52; // @[Conditional.scala 40:58:@288.4]
  wire  _T_149; // @[transplant.scala 166:71:@348.4]
  wire  _T_151; // @[transplant.scala 168:21:@352.4]
  wire  _T_152; // @[transplant.scala 169:45:@354.6]
  wire [31:0] _T_153; // @[transplant.scala 169:35:@355.6]
  wire  _T_154; // @[transplant.scala 170:27:@359.6]
  wire [31:0] _T_156; // @[transplant.scala 171:68:@362.8]
  wire [31:0] _T_157; // @[transplant.scala 171:94:@363.8]
  wire [31:0] _T_158; // @[transplant.scala 171:35:@364.8]
  wire  _T_159; // @[transplant.scala 172:27:@368.8]
  wire  _T_161; // @[transplant.scala 173:59:@370.10]
  wire  _T_162; // @[transplant.scala 173:82:@371.10]
  wire [6:0] _T_166; // @[Cat.scala 30:58:@375.10]
  wire [6:0] _GEN_56; // @[transplant.scala 172:45:@369.8]
  wire [31:0] _GEN_57; // @[transplant.scala 170:37:@360.6]
  wire [31:0] _GEN_58; // @[transplant.scala 168:34:@353.4]
  reg  _T_172; // @[transplant.scala 179:26:@384.4]
  reg [31:0] _RAND_7;
  reg [6:0] _T_174; // @[transplant.scala 180:28:@387.4]
  reg [31:0] _RAND_8;
  wire  _T_175; // @[transplant.scala 209:44:@392.4]
  wire  _T_176; // @[transplant.scala 208:44:@394.4]
  reg [1:0] _T_179; // @[transplant.scala 188:37:@398.4]
  reg [31:0] _RAND_9;
  reg  _T_182; // @[transplant.scala 189:38:@402.4]
  reg [31:0] _RAND_10;
  reg  freezeTag1D; // @[transplant.scala 192:28:@405.4]
  reg [31:0] _RAND_11;
  reg  _T_185; // @[transplant.scala 193:29:@407.4]
  reg [31:0] _RAND_12;
  reg  _T_187; // @[transplant.scala 197:30:@413.4]
  reg [31:0] _RAND_13;
  reg  _T_189; // @[transplant.scala 199:30:@417.4]
  reg [31:0] _RAND_14;
  assign bramOut = io_stateBRAM_dataOut[31:0]; // @[transplant.scala 84:50:@262.4]
  assign regDataInMSB = io_rfile_rs1_data[63:32]; // @[transplant.scala 91:31:@270.4]
  assign regDataInLSB = io_rfile_rs1_data[31:0]; // @[transplant.scala 92:31:@271.4]
  assign regAddr = bramOFFST >> 1'h1; // @[transplant.scala 95:27:@272.4]
  assign _T_124 = 1'h0 == state; // @[Conditional.scala 37:30:@287.4]
  assign _GEN_0 = io_tpu2cpu_done ? 1'h1 : freeze; // @[transplant.scala 134:35:@298.8]
  assign _GEN_1 = io_tpu2cpu_done ? io_tpu2cpu_doneTag : freezeTag; // @[transplant.scala 134:35:@298.8]
  assign _GEN_2 = io_tpu2cpu_done ? 1'h1 : stateDir; // @[transplant.scala 134:35:@298.8]
  assign _GEN_3 = io_tpu2cpu_done ? 2'h1 : stateRegType; // @[transplant.scala 134:35:@298.8]
  assign _GEN_4 = io_tpu2cpu_done ? 7'h0 : bramOFFST; // @[transplant.scala 134:35:@298.8]
  assign _GEN_6 = io_tpu2cpu_done ? 1'h1 : state; // @[transplant.scala 134:35:@298.8]
  assign _GEN_7 = io_host2tpu_fire ? 1'h1 : _GEN_0; // @[transplant.scala 126:30:@289.6]
  assign _GEN_8 = io_host2tpu_fire ? io_host2tpu_fireTag : _GEN_1; // @[transplant.scala 126:30:@289.6]
  assign _GEN_9 = io_host2tpu_fire ? 1'h0 : _GEN_2; // @[transplant.scala 126:30:@289.6]
  assign _GEN_10 = io_host2tpu_fire ? 2'h1 : _GEN_3; // @[transplant.scala 126:30:@289.6]
  assign _GEN_11 = io_host2tpu_fire ? 7'h0 : _GEN_4; // @[transplant.scala 126:30:@289.6]
  assign _GEN_12 = io_host2tpu_fire ? 1'h1 : _GEN_6; // @[transplant.scala 126:30:@289.6]
  assign _GEN_13 = io_host2tpu_fire ? 1'h0 : io_tpu2cpu_done; // @[transplant.scala 126:30:@289.6]
  assign _T_132 = bramOFFST + 7'h1; // @[transplant.scala 147:30:@311.8]
  assign _T_133 = bramOFFST + 7'h1; // @[transplant.scala 147:30:@312.8]
  assign _T_135 = bramOFFST == 7'h3f; // @[transplant.scala 148:22:@314.8]
  assign _T_137 = bramOFFST == 7'h41; // @[transplant.scala 150:29:@319.10]
  assign _T_139 = bramOFFST == 7'h42; // @[transplant.scala 152:29:@324.12]
  assign _T_140 = stateRegType == 2'h0; // @[transplant.scala 154:32:@329.14]
  assign _T_141 = stateDir == 1'h0; // @[transplant.scala 155:23:@331.16]
  assign _GEN_16 = _T_141 ? 1'h0 : stateDir; // @[transplant.scala 155:39:@332.16]
  assign _GEN_17 = _T_140 ? _T_141 : 1'h0; // @[transplant.scala 154:44:@330.14]
  assign _GEN_18 = _T_140 ? _GEN_16 : 1'h0; // @[transplant.scala 154:44:@330.14]
  assign _GEN_19 = _T_140 ? 7'h0 : _T_133; // @[transplant.scala 154:44:@330.14]
  assign _GEN_20 = _T_140 ? 2'h0 : stateRegType; // @[transplant.scala 154:44:@330.14]
  assign _GEN_21 = _T_140 ? 1'h0 : freeze; // @[transplant.scala 154:44:@330.14]
  assign _GEN_22 = _T_140 ? 1'h0 : state; // @[transplant.scala 154:44:@330.14]
  assign _GEN_23 = _T_139 ? 2'h0 : _GEN_20; // @[transplant.scala 152:58:@325.12]
  assign _GEN_24 = _T_139 ? 1'h0 : _GEN_17; // @[transplant.scala 152:58:@325.12]
  assign _GEN_25 = _T_139 ? 1'h0 : _GEN_18; // @[transplant.scala 152:58:@325.12]
  assign _GEN_26 = _T_139 ? _T_133 : _GEN_19; // @[transplant.scala 152:58:@325.12]
  assign _GEN_27 = _T_139 ? freeze : _GEN_21; // @[transplant.scala 152:58:@325.12]
  assign _GEN_28 = _T_139 ? state : _GEN_22; // @[transplant.scala 152:58:@325.12]
  assign _GEN_29 = _T_137 ? 2'h3 : _GEN_23; // @[transplant.scala 150:60:@320.10]
  assign _GEN_30 = _T_137 ? 1'h0 : _GEN_24; // @[transplant.scala 150:60:@320.10]
  assign _GEN_31 = _T_137 ? 1'h0 : _GEN_25; // @[transplant.scala 150:60:@320.10]
  assign _GEN_32 = _T_137 ? _T_133 : _GEN_26; // @[transplant.scala 150:60:@320.10]
  assign _GEN_33 = _T_137 ? freeze : _GEN_27; // @[transplant.scala 150:60:@320.10]
  assign _GEN_34 = _T_137 ? state : _GEN_28; // @[transplant.scala 150:60:@320.10]
  assign _GEN_35 = _T_135 ? 2'h2 : _GEN_29; // @[transplant.scala 148:49:@315.8]
  assign _GEN_36 = _T_135 ? 1'h0 : _GEN_30; // @[transplant.scala 148:49:@315.8]
  assign _GEN_37 = _T_135 ? 1'h0 : _GEN_31; // @[transplant.scala 148:49:@315.8]
  assign _GEN_38 = _T_135 ? _T_133 : _GEN_32; // @[transplant.scala 148:49:@315.8]
  assign _GEN_39 = _T_135 ? freeze : _GEN_33; // @[transplant.scala 148:49:@315.8]
  assign _GEN_40 = _T_135 ? state : _GEN_34; // @[transplant.scala 148:49:@315.8]
  assign _GEN_41 = state ? _GEN_38 : bramOFFST; // @[Conditional.scala 39:67:@310.6]
  assign _GEN_42 = state ? _GEN_35 : stateRegType; // @[Conditional.scala 39:67:@310.6]
  assign _GEN_43 = state ? _GEN_36 : 1'h0; // @[Conditional.scala 39:67:@310.6]
  assign _GEN_44 = state ? _GEN_37 : 1'h0; // @[Conditional.scala 39:67:@310.6]
  assign _GEN_45 = state ? _GEN_39 : freeze; // @[Conditional.scala 39:67:@310.6]
  assign _GEN_46 = state ? _GEN_40 : state; // @[Conditional.scala 39:67:@310.6]
  assign _GEN_47 = _T_124 ? _GEN_7 : _GEN_45; // @[Conditional.scala 40:58:@288.4]
  assign _GEN_48 = _T_124 ? _GEN_8 : freezeTag; // @[Conditional.scala 40:58:@288.4]
  assign _GEN_49 = _T_124 ? _GEN_9 : stateDir; // @[Conditional.scala 40:58:@288.4]
  assign _GEN_50 = _T_124 ? _GEN_10 : _GEN_42; // @[Conditional.scala 40:58:@288.4]
  assign _GEN_51 = _T_124 ? _GEN_11 : _GEN_41; // @[Conditional.scala 40:58:@288.4]
  assign _GEN_52 = _T_124 ? _GEN_12 : _GEN_46; // @[Conditional.scala 40:58:@288.4]
  assign _T_149 = stateRegType != 2'h0; // @[transplant.scala 166:71:@348.4]
  assign _T_151 = stateRegType == 2'h1; // @[transplant.scala 168:21:@352.4]
  assign _T_152 = bramOFFST[0]; // @[transplant.scala 169:45:@354.6]
  assign _T_153 = _T_152 ? regDataInLSB : regDataInMSB; // @[transplant.scala 169:35:@355.6]
  assign _T_154 = stateRegType == 2'h2; // @[transplant.scala 170:27:@359.6]
  assign _T_156 = io_cpu2tpuState_PC[31:0]; // @[transplant.scala 171:68:@362.8]
  assign _T_157 = io_cpu2tpuState_PC[63:32]; // @[transplant.scala 171:94:@363.8]
  assign _T_158 = _T_152 ? _T_156 : _T_157; // @[transplant.scala 171:35:@364.8]
  assign _T_159 = stateRegType == 2'h3; // @[transplant.scala 172:27:@368.8]
  assign _T_161 = io_cpu2tpuState_SP[0]; // @[transplant.scala 173:59:@370.10]
  assign _T_162 = io_cpu2tpuState_EL[0]; // @[transplant.scala 173:82:@371.10]
  assign _T_166 = {1'h0,_T_161,_T_162,io_cpu2tpuState_NZCV}; // @[Cat.scala 30:58:@375.10]
  assign _GEN_56 = _T_159 ? _T_166 : 7'h0; // @[transplant.scala 172:45:@369.8]
  assign _GEN_57 = _T_154 ? _T_158 : {{25'd0}, _GEN_56}; // @[transplant.scala 170:37:@360.6]
  assign _GEN_58 = _T_151 ? _T_153 : _GEN_57; // @[transplant.scala 168:34:@353.4]
  assign _T_175 = bramOut[5]; // @[transplant.scala 209:44:@392.4]
  assign _T_176 = bramOut[4]; // @[transplant.scala 208:44:@394.4]
  assign io_host2tpu_done = _T_189; // @[transplant.scala 199:20:@419.4]
  assign io_host2tpu_doneTag = freezeTag1D; // @[transplant.scala 200:23:@420.4]
  assign io_tpu2cpu_flush = _T_187; // @[transplant.scala 197:20:@415.4]
  assign io_tpu2cpu_freeze = freeze; // @[transplant.scala 195:21:@411.4]
  assign io_tpu2cpu_fire = _T_185; // @[transplant.scala 193:19:@409.4]
  assign io_tpu2cpu_flushTag = freezeTag1D; // @[transplant.scala 198:23:@416.4]
  assign io_tpu2cpu_fireTag = freezeTag1D; // @[transplant.scala 194:22:@410.4]
  assign io_tpu2cpu_freezeTag = freezeTag; // @[transplant.scala 196:24:@412.4]
  assign io_tpu2cpuStateReg_valid = _T_182; // @[transplant.scala 189:28:@404.4]
  assign io_tpu2cpuStateReg_bits = _T_179; // @[transplant.scala 188:27:@400.4]
  assign io_tpu2cpuState_PC = {bramOut1CD,bramOut}; // @[transplant.scala 183:22:@391.4]
  assign io_tpu2cpuState_SP = {{31'd0}, _T_175}; // @[transplant.scala 184:22:@393.4]
  assign io_tpu2cpuState_EL = {{31'd0}, _T_176}; // @[transplant.scala 185:22:@395.4]
  assign io_tpu2cpuState_NZCV = bramOut[3:0]; // @[transplant.scala 186:24:@397.4]
  assign io_rfile_rs1_addr = regAddr[4:0]; // @[transplant.scala 96:21:@273.4]
  assign io_rfile_waddr = _T_174[4:0]; // @[transplant.scala 180:18:@389.4]
  assign io_rfile_wdata = {bramOut1CD,bramOut}; // @[transplant.scala 181:18:@390.4]
  assign io_rfile_wen = _T_172; // @[transplant.scala 179:16:@386.4]
  assign io_stateBRAM_writeEn = stateDir & _T_149; // @[transplant.scala 166:28:@350.4]
  assign io_stateBRAM_addr = {{3'd0}, bramOFFST}; // @[transplant.scala 167:21:@351.4]
  assign io_stateBRAM_dataIn = {{4'd0}, _GEN_58}; // @[transplant.scala 169:29:@356.6 transplant.scala 171:29:@365.8 transplant.scala 173:29:@376.10 transplant.scala 175:29:@379.10]
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
  _T_172 = _RAND_7[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  _T_174 = _RAND_8[6:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  _T_179 = _RAND_9[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  _T_182 = _RAND_10[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {1{`RANDOM}};
  freezeTag1D = _RAND_11[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_12 = {1{`RANDOM}};
  _T_185 = _RAND_12[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_13 = {1{`RANDOM}};
  _T_187 = _RAND_13[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_14 = {1{`RANDOM}};
  _T_189 = _RAND_14[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (reset) begin
      bramOFFST <= 7'h0;
    end else begin
      if (_T_124) begin
        if (io_host2tpu_fire) begin
          bramOFFST <= 7'h0;
        end else begin
          if (io_tpu2cpu_done) begin
            bramOFFST <= 7'h0;
          end
        end
      end else begin
        if (state) begin
          if (_T_135) begin
            bramOFFST <= _T_133;
          end else begin
            if (_T_137) begin
              bramOFFST <= _T_133;
            end else begin
              if (_T_139) begin
                bramOFFST <= _T_133;
              end else begin
                if (_T_140) begin
                  bramOFFST <= 7'h0;
                end else begin
                  bramOFFST <= _T_133;
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
      if (_T_124) begin
        if (io_host2tpu_fire) begin
          state <= 1'h1;
        end else begin
          if (io_tpu2cpu_done) begin
            state <= 1'h1;
          end
        end
      end else begin
        if (state) begin
          if (!(_T_135)) begin
            if (!(_T_137)) begin
              if (!(_T_139)) begin
                if (_T_140) begin
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
      if (_T_124) begin
        if (io_host2tpu_fire) begin
          stateDir <= 1'h0;
        end else begin
          if (io_tpu2cpu_done) begin
            stateDir <= 1'h1;
          end
        end
      end
    end
    if (reset) begin
      stateRegType <= 2'h0;
    end else begin
      if (_T_124) begin
        if (io_host2tpu_fire) begin
          stateRegType <= 2'h1;
        end else begin
          if (io_tpu2cpu_done) begin
            stateRegType <= 2'h1;
          end
        end
      end else begin
        if (state) begin
          if (_T_135) begin
            stateRegType <= 2'h2;
          end else begin
            if (_T_137) begin
              stateRegType <= 2'h3;
            end else begin
              if (_T_139) begin
                stateRegType <= 2'h0;
              end else begin
                if (_T_140) begin
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
      if (_T_124) begin
        if (io_host2tpu_fire) begin
          freeze <= 1'h1;
        end else begin
          if (io_tpu2cpu_done) begin
            freeze <= 1'h1;
          end
        end
      end else begin
        if (state) begin
          if (!(_T_135)) begin
            if (!(_T_137)) begin
              if (!(_T_139)) begin
                if (_T_140) begin
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
      if (_T_124) begin
        if (io_host2tpu_fire) begin
          freezeTag <= io_host2tpu_fireTag;
        end else begin
          if (io_tpu2cpu_done) begin
            freezeTag <= io_tpu2cpu_doneTag;
          end
        end
      end
    end
    _T_172 <= _T_141 & _T_151;
    _T_174 <= bramOFFST >> 1'h1;
    _T_179 <= stateRegType;
    _T_182 <= stateDir == 1'h0;
    freezeTag1D <= freezeTag;
    if (_T_124) begin
      _T_185 <= 1'h0;
    end else begin
      if (state) begin
        if (_T_135) begin
          _T_185 <= 1'h0;
        end else begin
          if (_T_137) begin
            _T_185 <= 1'h0;
          end else begin
            if (_T_139) begin
              _T_185 <= 1'h0;
            end else begin
              if (_T_140) begin
                _T_185 <= _T_141;
              end else begin
                _T_185 <= 1'h0;
              end
            end
          end
        end
      end else begin
        _T_185 <= 1'h0;
      end
    end
    if (_T_124) begin
      if (io_host2tpu_fire) begin
        _T_187 <= 1'h0;
      end else begin
        _T_187 <= io_tpu2cpu_done;
      end
    end else begin
      _T_187 <= 1'h0;
    end
    if (_T_124) begin
      _T_189 <= 1'h0;
    end else begin
      if (state) begin
        if (_T_135) begin
          _T_189 <= 1'h0;
        end else begin
          if (_T_137) begin
            _T_189 <= 1'h0;
          end else begin
            if (_T_139) begin
              _T_189 <= 1'h0;
            end else begin
              if (_T_140) begin
                if (_T_141) begin
                  _T_189 <= 1'h0;
                end else begin
                  _T_189 <= stateDir;
                end
              end else begin
                _T_189 <= 1'h0;
              end
            end
          end
        end
      end else begin
        _T_189 <= 1'h0;
      end
    end
  end
endmodule
