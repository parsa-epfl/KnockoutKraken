module DataProcessing(
  input  [63:0] io_a,
  input  [3:0]  io_op,
  input         io_is32bit,
  output [63:0] io_res
);
  wire  _T; // @[Execute.scala 330:40]
  wire [62:0] _T_3; // @[Execute.scala 127:78]
  wire [63:0] _T_4; // @[Execute.scala 330:33]
  wire [1:0] _T_70; // @[Mux.scala 47:69]
  wire [1:0] _T_71; // @[Mux.scala 47:69]
  wire [2:0] _T_72; // @[Mux.scala 47:69]
  wire [2:0] _T_73; // @[Mux.scala 47:69]
  wire [2:0] _T_74; // @[Mux.scala 47:69]
  wire [2:0] _T_75; // @[Mux.scala 47:69]
  wire [3:0] _T_76; // @[Mux.scala 47:69]
  wire [3:0] _T_77; // @[Mux.scala 47:69]
  wire [3:0] _T_78; // @[Mux.scala 47:69]
  wire [3:0] _T_79; // @[Mux.scala 47:69]
  wire [3:0] _T_80; // @[Mux.scala 47:69]
  wire [3:0] _T_81; // @[Mux.scala 47:69]
  wire [3:0] _T_82; // @[Mux.scala 47:69]
  wire [3:0] _T_83; // @[Mux.scala 47:69]
  wire [4:0] _T_84; // @[Mux.scala 47:69]
  wire [4:0] _T_85; // @[Mux.scala 47:69]
  wire [4:0] _T_86; // @[Mux.scala 47:69]
  wire [4:0] _T_87; // @[Mux.scala 47:69]
  wire [4:0] _T_88; // @[Mux.scala 47:69]
  wire [4:0] _T_89; // @[Mux.scala 47:69]
  wire [4:0] _T_90; // @[Mux.scala 47:69]
  wire [4:0] _T_91; // @[Mux.scala 47:69]
  wire [4:0] _T_92; // @[Mux.scala 47:69]
  wire [4:0] _T_93; // @[Mux.scala 47:69]
  wire [4:0] _T_94; // @[Mux.scala 47:69]
  wire [4:0] _T_95; // @[Mux.scala 47:69]
  wire [4:0] _T_96; // @[Mux.scala 47:69]
  wire [4:0] _T_97; // @[Mux.scala 47:69]
  wire [4:0] _T_98; // @[Mux.scala 47:69]
  wire [4:0] _T_99; // @[Mux.scala 47:69]
  wire [5:0] _T_100; // @[Mux.scala 47:69]
  wire [5:0] _T_101; // @[Mux.scala 47:69]
  wire [5:0] _T_102; // @[Mux.scala 47:69]
  wire [5:0] _T_103; // @[Mux.scala 47:69]
  wire [5:0] _T_104; // @[Mux.scala 47:69]
  wire [5:0] _T_105; // @[Mux.scala 47:69]
  wire [5:0] _T_106; // @[Mux.scala 47:69]
  wire [5:0] _T_107; // @[Mux.scala 47:69]
  wire [5:0] _T_108; // @[Mux.scala 47:69]
  wire [5:0] _T_109; // @[Mux.scala 47:69]
  wire [5:0] _T_110; // @[Mux.scala 47:69]
  wire [5:0] _T_111; // @[Mux.scala 47:69]
  wire [5:0] _T_112; // @[Mux.scala 47:69]
  wire [5:0] _T_113; // @[Mux.scala 47:69]
  wire [5:0] _T_114; // @[Mux.scala 47:69]
  wire [5:0] _T_115; // @[Mux.scala 47:69]
  wire [5:0] _T_116; // @[Mux.scala 47:69]
  wire [5:0] _T_117; // @[Mux.scala 47:69]
  wire [5:0] _T_118; // @[Mux.scala 47:69]
  wire [5:0] _T_119; // @[Mux.scala 47:69]
  wire [5:0] _T_120; // @[Mux.scala 47:69]
  wire [5:0] _T_121; // @[Mux.scala 47:69]
  wire [5:0] _T_122; // @[Mux.scala 47:69]
  wire [5:0] _T_123; // @[Mux.scala 47:69]
  wire [5:0] _T_124; // @[Mux.scala 47:69]
  wire [5:0] _T_125; // @[Mux.scala 47:69]
  wire [5:0] _T_126; // @[Mux.scala 47:69]
  wire [5:0] _T_127; // @[Mux.scala 47:69]
  wire [5:0] _T_128; // @[Mux.scala 47:69]
  wire [5:0] _T_129; // @[Mux.scala 47:69]
  wire [5:0] _T_130; // @[Mux.scala 47:69]
  wire [5:0] _T_131; // @[Mux.scala 47:69]
  wire [5:0] _T_133; // @[Execute.scala 128:70]
  wire [63:0] _T_154; // @[Cat.scala 29:58]
  wire [63:0] _T_171; // @[Cat.scala 29:58]
  wire [31:0] _T_182; // @[Cat.scala 29:58]
  wire [63:0] _T_186; // @[Cat.scala 29:58]
  wire  _T_187; // @[Mux.scala 80:60]
  wire [63:0] _T_188; // @[Mux.scala 80:57]
  wire  _T_189; // @[Mux.scala 80:60]
  wire [63:0] _T_190; // @[Mux.scala 80:57]
  wire  _T_191; // @[Mux.scala 80:60]
  wire [63:0] _T_192; // @[Mux.scala 80:57]
  wire  _T_193; // @[Mux.scala 80:60]
  wire [30:0] _T_207; // @[Execute.scala 127:78]
  wire [31:0] _T_209; // @[Execute.scala 373:33]
  wire [1:0] _T_243; // @[Mux.scala 47:69]
  wire [1:0] _T_244; // @[Mux.scala 47:69]
  wire [2:0] _T_245; // @[Mux.scala 47:69]
  wire [2:0] _T_246; // @[Mux.scala 47:69]
  wire [2:0] _T_247; // @[Mux.scala 47:69]
  wire [2:0] _T_248; // @[Mux.scala 47:69]
  wire [3:0] _T_249; // @[Mux.scala 47:69]
  wire [3:0] _T_250; // @[Mux.scala 47:69]
  wire [3:0] _T_251; // @[Mux.scala 47:69]
  wire [3:0] _T_252; // @[Mux.scala 47:69]
  wire [3:0] _T_253; // @[Mux.scala 47:69]
  wire [3:0] _T_254; // @[Mux.scala 47:69]
  wire [3:0] _T_255; // @[Mux.scala 47:69]
  wire [3:0] _T_256; // @[Mux.scala 47:69]
  wire [4:0] _T_257; // @[Mux.scala 47:69]
  wire [4:0] _T_258; // @[Mux.scala 47:69]
  wire [4:0] _T_259; // @[Mux.scala 47:69]
  wire [4:0] _T_260; // @[Mux.scala 47:69]
  wire [4:0] _T_261; // @[Mux.scala 47:69]
  wire [4:0] _T_262; // @[Mux.scala 47:69]
  wire [4:0] _T_263; // @[Mux.scala 47:69]
  wire [4:0] _T_264; // @[Mux.scala 47:69]
  wire [4:0] _T_265; // @[Mux.scala 47:69]
  wire [4:0] _T_266; // @[Mux.scala 47:69]
  wire [4:0] _T_267; // @[Mux.scala 47:69]
  wire [4:0] _T_268; // @[Mux.scala 47:69]
  wire [4:0] _T_269; // @[Mux.scala 47:69]
  wire [4:0] _T_270; // @[Mux.scala 47:69]
  wire [4:0] _T_271; // @[Mux.scala 47:69]
  wire [4:0] _T_272; // @[Mux.scala 47:69]
  wire [4:0] _T_274; // @[Execute.scala 128:70]
  wire [5:0] _GEN_0; // @[Execute.scala 371:20]
  wire [63:0] countLeadingBits; // @[Execute.scala 328:30 Execute.scala 329:20 Execute.scala 372:22]
  wire [63:0] _T_194; // @[Mux.scala 80:57]
  wire  _T_195; // @[Mux.scala 80:60]
  wire [63:0] _T_196; // @[Mux.scala 80:57]
  wire [31:0] _T_283; // @[Cat.scala 29:58]
  wire [63:0] _T_292; // @[Mux.scala 80:57]
  wire [63:0] _T_294; // @[Mux.scala 80:57]
  wire [63:0] rev; // @[Execute.scala 371:20]
  wire [63:0] _T_198; // @[Mux.scala 80:57]
  wire [63:0] _T_200; // @[Mux.scala 80:57]
  assign _T = io_op == 4'h5; // @[Execute.scala 330:40]
  assign _T_3 = io_a[63:1] ^ io_a[62:0]; // @[Execute.scala 127:78]
  assign _T_4 = _T ? {{1'd0}, _T_3} : io_a; // @[Execute.scala 330:33]
  assign _T_70 = _T_4[2] ? 2'h2 : {{1'd0}, _T_4[1]}; // @[Mux.scala 47:69]
  assign _T_71 = _T_4[3] ? 2'h3 : _T_70; // @[Mux.scala 47:69]
  assign _T_72 = _T_4[4] ? 3'h4 : {{1'd0}, _T_71}; // @[Mux.scala 47:69]
  assign _T_73 = _T_4[5] ? 3'h5 : _T_72; // @[Mux.scala 47:69]
  assign _T_74 = _T_4[6] ? 3'h6 : _T_73; // @[Mux.scala 47:69]
  assign _T_75 = _T_4[7] ? 3'h7 : _T_74; // @[Mux.scala 47:69]
  assign _T_76 = _T_4[8] ? 4'h8 : {{1'd0}, _T_75}; // @[Mux.scala 47:69]
  assign _T_77 = _T_4[9] ? 4'h9 : _T_76; // @[Mux.scala 47:69]
  assign _T_78 = _T_4[10] ? 4'ha : _T_77; // @[Mux.scala 47:69]
  assign _T_79 = _T_4[11] ? 4'hb : _T_78; // @[Mux.scala 47:69]
  assign _T_80 = _T_4[12] ? 4'hc : _T_79; // @[Mux.scala 47:69]
  assign _T_81 = _T_4[13] ? 4'hd : _T_80; // @[Mux.scala 47:69]
  assign _T_82 = _T_4[14] ? 4'he : _T_81; // @[Mux.scala 47:69]
  assign _T_83 = _T_4[15] ? 4'hf : _T_82; // @[Mux.scala 47:69]
  assign _T_84 = _T_4[16] ? 5'h10 : {{1'd0}, _T_83}; // @[Mux.scala 47:69]
  assign _T_85 = _T_4[17] ? 5'h11 : _T_84; // @[Mux.scala 47:69]
  assign _T_86 = _T_4[18] ? 5'h12 : _T_85; // @[Mux.scala 47:69]
  assign _T_87 = _T_4[19] ? 5'h13 : _T_86; // @[Mux.scala 47:69]
  assign _T_88 = _T_4[20] ? 5'h14 : _T_87; // @[Mux.scala 47:69]
  assign _T_89 = _T_4[21] ? 5'h15 : _T_88; // @[Mux.scala 47:69]
  assign _T_90 = _T_4[22] ? 5'h16 : _T_89; // @[Mux.scala 47:69]
  assign _T_91 = _T_4[23] ? 5'h17 : _T_90; // @[Mux.scala 47:69]
  assign _T_92 = _T_4[24] ? 5'h18 : _T_91; // @[Mux.scala 47:69]
  assign _T_93 = _T_4[25] ? 5'h19 : _T_92; // @[Mux.scala 47:69]
  assign _T_94 = _T_4[26] ? 5'h1a : _T_93; // @[Mux.scala 47:69]
  assign _T_95 = _T_4[27] ? 5'h1b : _T_94; // @[Mux.scala 47:69]
  assign _T_96 = _T_4[28] ? 5'h1c : _T_95; // @[Mux.scala 47:69]
  assign _T_97 = _T_4[29] ? 5'h1d : _T_96; // @[Mux.scala 47:69]
  assign _T_98 = _T_4[30] ? 5'h1e : _T_97; // @[Mux.scala 47:69]
  assign _T_99 = _T_4[31] ? 5'h1f : _T_98; // @[Mux.scala 47:69]
  assign _T_100 = _T_4[32] ? 6'h20 : {{1'd0}, _T_99}; // @[Mux.scala 47:69]
  assign _T_101 = _T_4[33] ? 6'h21 : _T_100; // @[Mux.scala 47:69]
  assign _T_102 = _T_4[34] ? 6'h22 : _T_101; // @[Mux.scala 47:69]
  assign _T_103 = _T_4[35] ? 6'h23 : _T_102; // @[Mux.scala 47:69]
  assign _T_104 = _T_4[36] ? 6'h24 : _T_103; // @[Mux.scala 47:69]
  assign _T_105 = _T_4[37] ? 6'h25 : _T_104; // @[Mux.scala 47:69]
  assign _T_106 = _T_4[38] ? 6'h26 : _T_105; // @[Mux.scala 47:69]
  assign _T_107 = _T_4[39] ? 6'h27 : _T_106; // @[Mux.scala 47:69]
  assign _T_108 = _T_4[40] ? 6'h28 : _T_107; // @[Mux.scala 47:69]
  assign _T_109 = _T_4[41] ? 6'h29 : _T_108; // @[Mux.scala 47:69]
  assign _T_110 = _T_4[42] ? 6'h2a : _T_109; // @[Mux.scala 47:69]
  assign _T_111 = _T_4[43] ? 6'h2b : _T_110; // @[Mux.scala 47:69]
  assign _T_112 = _T_4[44] ? 6'h2c : _T_111; // @[Mux.scala 47:69]
  assign _T_113 = _T_4[45] ? 6'h2d : _T_112; // @[Mux.scala 47:69]
  assign _T_114 = _T_4[46] ? 6'h2e : _T_113; // @[Mux.scala 47:69]
  assign _T_115 = _T_4[47] ? 6'h2f : _T_114; // @[Mux.scala 47:69]
  assign _T_116 = _T_4[48] ? 6'h30 : _T_115; // @[Mux.scala 47:69]
  assign _T_117 = _T_4[49] ? 6'h31 : _T_116; // @[Mux.scala 47:69]
  assign _T_118 = _T_4[50] ? 6'h32 : _T_117; // @[Mux.scala 47:69]
  assign _T_119 = _T_4[51] ? 6'h33 : _T_118; // @[Mux.scala 47:69]
  assign _T_120 = _T_4[52] ? 6'h34 : _T_119; // @[Mux.scala 47:69]
  assign _T_121 = _T_4[53] ? 6'h35 : _T_120; // @[Mux.scala 47:69]
  assign _T_122 = _T_4[54] ? 6'h36 : _T_121; // @[Mux.scala 47:69]
  assign _T_123 = _T_4[55] ? 6'h37 : _T_122; // @[Mux.scala 47:69]
  assign _T_124 = _T_4[56] ? 6'h38 : _T_123; // @[Mux.scala 47:69]
  assign _T_125 = _T_4[57] ? 6'h39 : _T_124; // @[Mux.scala 47:69]
  assign _T_126 = _T_4[58] ? 6'h3a : _T_125; // @[Mux.scala 47:69]
  assign _T_127 = _T_4[59] ? 6'h3b : _T_126; // @[Mux.scala 47:69]
  assign _T_128 = _T_4[60] ? 6'h3c : _T_127; // @[Mux.scala 47:69]
  assign _T_129 = _T_4[61] ? 6'h3d : _T_128; // @[Mux.scala 47:69]
  assign _T_130 = _T_4[62] ? 6'h3e : _T_129; // @[Mux.scala 47:69]
  assign _T_131 = _T_4[63] ? 6'h3f : _T_130; // @[Mux.scala 47:69]
  assign _T_133 = 6'h3f - _T_131; // @[Execute.scala 128:70]
  assign _T_154 = {io_a[63:56],io_a[55:48],io_a[47:40],io_a[39:32],io_a[31:24],io_a[23:16],io_a[15:8],io_a[7:0]}; // @[Cat.scala 29:58]
  assign _T_171 = {io_a[63:56],io_a[55:48],io_a[47:40],io_a[39:32],io_a[31:24],io_a[23:16],io_a[15:8],io_a[7:0]}; // @[Cat.scala 29:58]
  assign _T_182 = {io_a[31:24],io_a[23:16],io_a[15:8],io_a[7:0]}; // @[Cat.scala 29:58]
  assign _T_186 = {io_a[63:56],io_a[55:48],io_a[47:40],io_a[39:32],io_a[31:24],io_a[23:16],io_a[15:8],io_a[7:0]}; // @[Cat.scala 29:58]
  assign _T_187 = 4'h1 == io_op; // @[Mux.scala 80:60]
  assign _T_188 = _T_187 ? _T_154 : io_a; // @[Mux.scala 80:57]
  assign _T_189 = 4'h2 == io_op; // @[Mux.scala 80:60]
  assign _T_190 = _T_189 ? _T_171 : _T_188; // @[Mux.scala 80:57]
  assign _T_191 = 4'h3 == io_op; // @[Mux.scala 80:60]
  assign _T_192 = _T_191 ? _T_186 : _T_190; // @[Mux.scala 80:57]
  assign _T_193 = 4'h5 == io_op; // @[Mux.scala 80:60]
  assign _T_207 = io_a[31:1] ^ io_a[30:0]; // @[Execute.scala 127:78]
  assign _T_209 = _T ? {{1'd0}, _T_207} : io_a[31:0]; // @[Execute.scala 373:33]
  assign _T_243 = _T_209[2] ? 2'h2 : {{1'd0}, _T_209[1]}; // @[Mux.scala 47:69]
  assign _T_244 = _T_209[3] ? 2'h3 : _T_243; // @[Mux.scala 47:69]
  assign _T_245 = _T_209[4] ? 3'h4 : {{1'd0}, _T_244}; // @[Mux.scala 47:69]
  assign _T_246 = _T_209[5] ? 3'h5 : _T_245; // @[Mux.scala 47:69]
  assign _T_247 = _T_209[6] ? 3'h6 : _T_246; // @[Mux.scala 47:69]
  assign _T_248 = _T_209[7] ? 3'h7 : _T_247; // @[Mux.scala 47:69]
  assign _T_249 = _T_209[8] ? 4'h8 : {{1'd0}, _T_248}; // @[Mux.scala 47:69]
  assign _T_250 = _T_209[9] ? 4'h9 : _T_249; // @[Mux.scala 47:69]
  assign _T_251 = _T_209[10] ? 4'ha : _T_250; // @[Mux.scala 47:69]
  assign _T_252 = _T_209[11] ? 4'hb : _T_251; // @[Mux.scala 47:69]
  assign _T_253 = _T_209[12] ? 4'hc : _T_252; // @[Mux.scala 47:69]
  assign _T_254 = _T_209[13] ? 4'hd : _T_253; // @[Mux.scala 47:69]
  assign _T_255 = _T_209[14] ? 4'he : _T_254; // @[Mux.scala 47:69]
  assign _T_256 = _T_209[15] ? 4'hf : _T_255; // @[Mux.scala 47:69]
  assign _T_257 = _T_209[16] ? 5'h10 : {{1'd0}, _T_256}; // @[Mux.scala 47:69]
  assign _T_258 = _T_209[17] ? 5'h11 : _T_257; // @[Mux.scala 47:69]
  assign _T_259 = _T_209[18] ? 5'h12 : _T_258; // @[Mux.scala 47:69]
  assign _T_260 = _T_209[19] ? 5'h13 : _T_259; // @[Mux.scala 47:69]
  assign _T_261 = _T_209[20] ? 5'h14 : _T_260; // @[Mux.scala 47:69]
  assign _T_262 = _T_209[21] ? 5'h15 : _T_261; // @[Mux.scala 47:69]
  assign _T_263 = _T_209[22] ? 5'h16 : _T_262; // @[Mux.scala 47:69]
  assign _T_264 = _T_209[23] ? 5'h17 : _T_263; // @[Mux.scala 47:69]
  assign _T_265 = _T_209[24] ? 5'h18 : _T_264; // @[Mux.scala 47:69]
  assign _T_266 = _T_209[25] ? 5'h19 : _T_265; // @[Mux.scala 47:69]
  assign _T_267 = _T_209[26] ? 5'h1a : _T_266; // @[Mux.scala 47:69]
  assign _T_268 = _T_209[27] ? 5'h1b : _T_267; // @[Mux.scala 47:69]
  assign _T_269 = _T_209[28] ? 5'h1c : _T_268; // @[Mux.scala 47:69]
  assign _T_270 = _T_209[29] ? 5'h1d : _T_269; // @[Mux.scala 47:69]
  assign _T_271 = _T_209[30] ? 5'h1e : _T_270; // @[Mux.scala 47:69]
  assign _T_272 = _T_209[31] ? 5'h1f : _T_271; // @[Mux.scala 47:69]
  assign _T_274 = 5'h1f - _T_272; // @[Execute.scala 128:70]
  assign _GEN_0 = io_is32bit ? {{1'd0}, _T_274} : _T_133; // @[Execute.scala 371:20]
  assign countLeadingBits = {{58'd0}, _GEN_0}; // @[Execute.scala 328:30 Execute.scala 329:20 Execute.scala 372:22]
  assign _T_194 = _T_193 ? countLeadingBits : io_a; // @[Mux.scala 80:57]
  assign _T_195 = 4'h4 == io_op; // @[Mux.scala 80:60]
  assign _T_196 = _T_195 ? countLeadingBits : _T_194; // @[Mux.scala 80:57]
  assign _T_283 = {io_a[31:24],io_a[23:16],io_a[15:8],io_a[7:0]}; // @[Cat.scala 29:58]
  assign _T_292 = _T_187 ? {{32'd0}, _T_283} : io_a; // @[Mux.scala 80:57]
  assign _T_294 = _T_189 ? {{32'd0}, _T_182} : _T_292; // @[Mux.scala 80:57]
  assign rev = io_is32bit ? _T_294 : _T_192; // @[Execute.scala 371:20]
  assign _T_198 = _T_191 ? rev : _T_196; // @[Mux.scala 80:57]
  assign _T_200 = _T_189 ? rev : _T_198; // @[Mux.scala 80:57]
  assign io_res = _T_187 ? rev : _T_200; // @[Execute.scala 369:10]
endmodule