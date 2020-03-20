module ExecuteUnit(
  input         clock,
  input         reset,
  input         io_dinst_rd_valid,
  input  [4:0]  io_dinst_rd_bits,
  input  [4:0]  io_dinst_rs1,
  input  [4:0]  io_dinst_rs2,
  input  [25:0] io_dinst_imm,
  input         io_dinst_shift_val_valid,
  input  [5:0]  io_dinst_shift_val_bits,
  input  [1:0]  io_dinst_shift_type,
  input  [3:0]  io_dinst_cond_bits,
  input         io_dinst_is32bit,
  input  [4:0]  io_dinst_itype,
  input  [3:0]  io_dinst_op,
  input         io_dinst_nzcv_valid,
  input  [3:0]  io_dinst_nzcv_bits,
  input  [63:0] io_rVal1,
  input  [63:0] io_rVal2,
  input  [63:0] io_rVal3,
  input  [3:0]  io_nzcv,
  output        io_condRes,
  output        io_einst_valid,
  output        io_einst_bits_rd_valid,
  output [4:0]  io_einst_bits_rd_bits,
  output        io_einst_bits_nzcv_valid,
  output [3:0]  io_einst_bits_nzcv_bits,
  output [63:0] io_einst_bits_res
);
  wire [63:0] shiftALU_io_word; // @[Execute.scala 424:24]
  wire [5:0] shiftALU_io_amount; // @[Execute.scala 424:24]
  wire [1:0] shiftALU_io_opcode; // @[Execute.scala 424:24]
  wire [63:0] shiftALU_io_res; // @[Execute.scala 424:24]
  wire  shiftALU_io_is32bit; // @[Execute.scala 424:24]
  wire [3:0] condHolds_io_cond; // @[Execute.scala 446:25]
  wire [3:0] condHolds_io_nzcv; // @[Execute.scala 446:25]
  wire  condHolds_io_res; // @[Execute.scala 446:25]
  wire  decodeBitMask_io_immn; // @[Execute.scala 453:29]
  wire [5:0] decodeBitMask_io_imms; // @[Execute.scala 453:29]
  wire [5:0] decodeBitMask_io_immr; // @[Execute.scala 453:29]
  wire [63:0] decodeBitMask_io_wmask; // @[Execute.scala 453:29]
  wire [63:0] decodeBitMask_io_tmask; // @[Execute.scala 453:29]
  wire  decodeBitMask_io_is32bit; // @[Execute.scala 453:29]
  wire [3:0] bitfield_io_op; // @[Execute.scala 461:24]
  wire [5:0] bitfield_io_imms; // @[Execute.scala 461:24]
  wire [63:0] bitfield_io_src; // @[Execute.scala 461:24]
  wire [63:0] bitfield_io_dst; // @[Execute.scala 461:24]
  wire [63:0] bitfield_io_wmask; // @[Execute.scala 461:24]
  wire [63:0] bitfield_io_tmask; // @[Execute.scala 461:24]
  wire [63:0] bitfield_io_rorSrcR; // @[Execute.scala 461:24]
  wire [63:0] bitfield_io_res; // @[Execute.scala 461:24]
  wire [3:0] move_io_op; // @[Execute.scala 472:20]
  wire [1:0] move_io_hw; // @[Execute.scala 472:20]
  wire [15:0] move_io_imm; // @[Execute.scala 472:20]
  wire [63:0] move_io_rd; // @[Execute.scala 472:20]
  wire [63:0] move_io_res; // @[Execute.scala 472:20]
  wire [63:0] logicALU_io_a; // @[Execute.scala 502:24]
  wire [63:0] logicALU_io_b; // @[Execute.scala 502:24]
  wire [3:0] logicALU_io_opcode; // @[Execute.scala 502:24]
  wire [63:0] logicALU_io_res; // @[Execute.scala 502:24]
  wire [3:0] logicALU_io_nzcv; // @[Execute.scala 502:24]
  wire  logicALU_io_is32bit; // @[Execute.scala 502:24]
  wire [63:0] dataProcessing_io_a; // @[Execute.scala 509:30]
  wire [3:0] dataProcessing_io_op; // @[Execute.scala 509:30]
  wire  dataProcessing_io_is32bit; // @[Execute.scala 509:30]
  wire [63:0] dataProcessing_io_res; // @[Execute.scala 509:30]
  wire  dataProc3S_clock; // @[Execute.scala 516:26]
  wire  dataProc3S_reset; // @[Execute.scala 516:26]
  wire [63:0] dataProc3S_io_rVal1; // @[Execute.scala 516:26]
  wire [63:0] dataProc3S_io_rVal2; // @[Execute.scala 516:26]
  wire [63:0] dataProc3S_io_rVal3; // @[Execute.scala 516:26]
  wire [63:0] dataProc3S_io_res; // @[Execute.scala 516:26]
  wire  dataProc3S_io_is32bit; // @[Execute.scala 516:26]
  wire  addWithCarry_io_is32bit; // @[Execute.scala 523:28]
  wire [63:0] addWithCarry_io_a; // @[Execute.scala 523:28]
  wire [63:0] addWithCarry_io_b; // @[Execute.scala 523:28]
  wire  addWithCarry_io_carry; // @[Execute.scala 523:28]
  wire [63:0] addWithCarry_io_res; // @[Execute.scala 523:28]
  wire [3:0] addWithCarry_io_nzcv; // @[Execute.scala 523:28]
  wire  _T; // @[Execute.scala 414:41]
  wire [63:0] _T_1; // @[Execute.scala 414:27]
  wire  _T_2; // @[Execute.scala 415:41]
  wire [63:0] rVal2; // @[Execute.scala 415:27]
  wire  _T_5; // @[Execute.scala 416:46]
  wire  _T_7; // @[Execute.scala 418:23]
  wire  _T_8; // @[Execute.scala 418:50]
  wire  _T_9; // @[Execute.scala 418:35]
  wire  _T_11; // @[Execute.scala 418:61]
  wire [63:0] rVal1; // @[Execute.scala 418:87]
  wire  _T_12; // @[Mux.scala 80:60]
  wire  _T_14; // @[Mux.scala 80:60]
  wire [63:0] _T_15; // @[Mux.scala 80:57]
  wire  _T_16; // @[Mux.scala 80:60]
  wire [63:0] _T_17; // @[Mux.scala 80:57]
  wire  _T_18; // @[Mux.scala 80:60]
  wire [63:0] _T_19; // @[Mux.scala 80:57]
  wire  _T_20; // @[Mux.scala 80:60]
  wire [63:0] _T_21; // @[Mux.scala 80:57]
  wire  _T_22; // @[Mux.scala 80:60]
  wire [63:0] _T_23; // @[Mux.scala 80:57]
  wire  _T_24; // @[Mux.scala 80:60]
  wire [63:0] _T_25; // @[Mux.scala 80:57]
  wire  _T_26; // @[Mux.scala 80:60]
  wire [63:0] _T_27; // @[Mux.scala 80:57]
  wire  _T_28; // @[Mux.scala 80:60]
  wire [63:0] _T_29; // @[Mux.scala 80:57]
  wire  _T_30; // @[Mux.scala 80:60]
  wire  _T_32; // @[Execute.scala 440:24]
  wire [5:0] _T_34; // @[Execute.scala 440:8]
  wire [63:0] _T_58; // @[Mux.scala 80:57]
  wire [63:0] _T_60; // @[Mux.scala 80:57]
  wire [63:0] _T_62; // @[Mux.scala 80:57]
  wire [63:0] _T_64; // @[Mux.scala 80:57]
  wire [63:0] _T_66; // @[Mux.scala 80:57]
  wire [63:0] _T_68; // @[Mux.scala 80:57]
  wire [63:0] aluVal2; // @[Mux.scala 80:57]
  wire [63:0] _T_76; // @[Execute.scala 504:40]
  wire [63:0] _T_77; // @[Execute.scala 504:23]
  wire  _T_81; // @[Execute.scala 528:44]
  wire  _T_83; // @[Execute.scala 529:23]
  wire  _T_84; // @[Execute.scala 530:30]
  wire  _T_85; // @[Execute.scala 530:58]
  wire  _T_86; // @[Execute.scala 530:43]
  wire  _T_89; // @[Execute.scala 531:45]
  wire [63:0] _T_91; // @[Execute.scala 533:29]
  wire  _T_93; // @[Execute.scala 535:29]
  wire  _T_94; // @[Execute.scala 535:59]
  wire  _T_95; // @[Execute.scala 535:41]
  wire [63:0] _GEN_2; // @[Execute.scala 535:72]
  wire  _GEN_3; // @[Execute.scala 535:72]
  wire [63:0] _T_101; // @[Execute.scala 554:19]
  wire [63:0] _T_103; // @[Mux.scala 80:57]
  wire [63:0] _T_105; // @[Mux.scala 80:57]
  wire [63:0] _T_107; // @[Mux.scala 80:57]
  wire  _T_108; // @[Mux.scala 80:60]
  wire [63:0] _T_109; // @[Mux.scala 80:57]
  wire [63:0] _T_111; // @[Mux.scala 80:57]
  wire  _T_112; // @[Mux.scala 80:60]
  wire [63:0] _T_113; // @[Mux.scala 80:57]
  wire [63:0] _T_115; // @[Mux.scala 80:57]
  wire [63:0] _T_117; // @[Mux.scala 80:57]
  wire  _T_118; // @[Mux.scala 80:60]
  wire [63:0] _T_119; // @[Mux.scala 80:57]
  wire [63:0] res; // @[Mux.scala 80:57]
  wire [32:0] _T_123; // @[Cat.scala 29:58]
  wire  _T_125; // @[Execute.scala 558:23]
  wire  _T_126; // @[Execute.scala 558:53]
  wire  _T_127; // @[Execute.scala 558:35]
  wire  _T_128; // @[Execute.scala 559:40]
  wire [3:0] _T_129; // @[Execute.scala 564:42]
  wire [3:0] _T_132; // @[Mux.scala 80:57]
  wire [3:0] _T_134; // @[Mux.scala 80:57]
  wire [3:0] _T_136; // @[Mux.scala 80:57]
  wire  _T_142; // @[Mux.scala 80:57]
  wire  _T_144; // @[Mux.scala 80:57]
  wire  _T_146; // @[Mux.scala 80:57]
  wire  _T_148; // @[Mux.scala 80:57]
  wire  _T_150; // @[Mux.scala 80:57]
  wire  _T_152; // @[Mux.scala 80:57]
  wire  _T_154; // @[Mux.scala 80:57]
  wire  _T_156; // @[Mux.scala 80:57]
  wire  _T_158; // @[Mux.scala 80:57]
  wire  _T_160; // @[Mux.scala 80:57]
  ShiftALU shiftALU ( // @[Execute.scala 424:24]
    .io_word(shiftALU_io_word),
    .io_amount(shiftALU_io_amount),
    .io_opcode(shiftALU_io_opcode),
    .io_res(shiftALU_io_res),
    .io_is32bit(shiftALU_io_is32bit)
  );
  ConditionHolds condHolds ( // @[Execute.scala 446:25]
    .io_cond(condHolds_io_cond),
    .io_nzcv(condHolds_io_nzcv),
    .io_res(condHolds_io_res)
  );
  DecodeBitMasks decodeBitMask ( // @[Execute.scala 453:29]
    .io_immn(decodeBitMask_io_immn),
    .io_imms(decodeBitMask_io_imms),
    .io_immr(decodeBitMask_io_immr),
    .io_wmask(decodeBitMask_io_wmask),
    .io_tmask(decodeBitMask_io_tmask),
    .io_is32bit(decodeBitMask_io_is32bit)
  );
  BitfieldALU bitfield ( // @[Execute.scala 461:24]
    .io_op(bitfield_io_op),
    .io_imms(bitfield_io_imms),
    .io_src(bitfield_io_src),
    .io_dst(bitfield_io_dst),
    .io_wmask(bitfield_io_wmask),
    .io_tmask(bitfield_io_tmask),
    .io_rorSrcR(bitfield_io_rorSrcR),
    .io_res(bitfield_io_res)
  );
  Move move ( // @[Execute.scala 472:20]
    .io_op(move_io_op),
    .io_hw(move_io_hw),
    .io_imm(move_io_imm),
    .io_rd(move_io_rd),
    .io_res(move_io_res)
  );
  LogicALU logicALU ( // @[Execute.scala 502:24]
    .io_a(logicALU_io_a),
    .io_b(logicALU_io_b),
    .io_opcode(logicALU_io_opcode),
    .io_res(logicALU_io_res),
    .io_nzcv(logicALU_io_nzcv),
    .io_is32bit(logicALU_io_is32bit)
  );
  DataProcessing dataProcessing ( // @[Execute.scala 509:30]
    .io_a(dataProcessing_io_a),
    .io_op(dataProcessing_io_op),
    .io_is32bit(dataProcessing_io_is32bit),
    .io_res(dataProcessing_io_res)
  );
  DataProc3S dataProc3S ( // @[Execute.scala 516:26]
    .clock(dataProc3S_clock),
    .reset(dataProc3S_reset),
    .io_rVal1(dataProc3S_io_rVal1),
    .io_rVal2(dataProc3S_io_rVal2),
    .io_rVal3(dataProc3S_io_rVal3),
    .io_res(dataProc3S_io_res),
    .io_is32bit(dataProc3S_io_is32bit)
  );
  AddWithCarry addWithCarry ( // @[Execute.scala 523:28]
    .io_is32bit(addWithCarry_io_is32bit),
    .io_a(addWithCarry_io_a),
    .io_b(addWithCarry_io_b),
    .io_carry(addWithCarry_io_carry),
    .io_res(addWithCarry_io_res),
    .io_nzcv(addWithCarry_io_nzcv)
  );
  assign _T = io_dinst_rs1 == 5'h1f; // @[Execute.scala 414:41]
  assign _T_1 = _T ? 64'h0 : io_rVal1; // @[Execute.scala 414:27]
  assign _T_2 = io_dinst_rs2 == 5'h1f; // @[Execute.scala 415:41]
  assign rVal2 = _T_2 ? 64'h0 : io_rVal2; // @[Execute.scala 415:27]
  assign _T_5 = io_dinst_imm[4:0] == 5'h1f; // @[Execute.scala 416:46]
  assign _T_7 = io_dinst_itype == 5'h8; // @[Execute.scala 418:23]
  assign _T_8 = io_dinst_op == 4'h0; // @[Execute.scala 418:50]
  assign _T_9 = _T_7 & _T_8; // @[Execute.scala 418:35]
  assign _T_11 = _T_9 & _T; // @[Execute.scala 418:61]
  assign rVal1 = _T_11 ? io_rVal1 : _T_1; // @[Execute.scala 418:87]
  assign _T_12 = 5'h9 == io_dinst_itype; // @[Mux.scala 80:60]
  assign _T_14 = 5'h3 == io_dinst_itype; // @[Mux.scala 80:60]
  assign _T_15 = _T_14 ? {{38'd0}, io_dinst_imm} : rVal2; // @[Mux.scala 80:57]
  assign _T_16 = 5'h8 == io_dinst_itype; // @[Mux.scala 80:60]
  assign _T_17 = _T_16 ? {{38'd0}, io_dinst_imm} : _T_15; // @[Mux.scala 80:57]
  assign _T_18 = 5'h4 == io_dinst_itype; // @[Mux.scala 80:60]
  assign _T_19 = _T_18 ? {{38'd0}, io_dinst_imm} : _T_17; // @[Mux.scala 80:57]
  assign _T_20 = 5'h5 == io_dinst_itype; // @[Mux.scala 80:60]
  assign _T_21 = _T_20 ? rVal2 : _T_19; // @[Mux.scala 80:57]
  assign _T_22 = 5'h6 == io_dinst_itype; // @[Mux.scala 80:60]
  assign _T_23 = _T_22 ? rVal2 : _T_21; // @[Mux.scala 80:57]
  assign _T_24 = 5'h1 == io_dinst_itype; // @[Mux.scala 80:60]
  assign _T_25 = _T_24 ? rVal2 : _T_23; // @[Mux.scala 80:57]
  assign _T_26 = 5'hd == io_dinst_itype; // @[Mux.scala 80:60]
  assign _T_27 = _T_26 ? rVal1 : _T_25; // @[Mux.scala 80:57]
  assign _T_28 = 5'h2 == io_dinst_itype; // @[Mux.scala 80:60]
  assign _T_29 = _T_28 ? rVal1 : _T_27; // @[Mux.scala 80:57]
  assign _T_30 = 5'hb == io_dinst_itype; // @[Mux.scala 80:60]
  assign _T_32 = io_dinst_itype == 5'hd; // @[Execute.scala 440:24]
  assign _T_34 = _T_32 ? io_rVal2[5:0] : io_dinst_shift_val_bits; // @[Execute.scala 440:8]
  assign _T_58 = shiftALU_io_res; // @[Mux.scala 80:57]
  assign _T_60 = _T_16 ? shiftALU_io_res : _T_58; // @[Mux.scala 80:57]
  assign _T_62 = _T_18 ? shiftALU_io_res : _T_60; // @[Mux.scala 80:57]
  assign _T_64 = _T_20 ? shiftALU_io_res : _T_62; // @[Mux.scala 80:57]
  assign _T_66 = _T_22 ? shiftALU_io_res : _T_64; // @[Mux.scala 80:57]
  assign _T_68 = _T_24 ? shiftALU_io_res : _T_66; // @[Mux.scala 80:57]
  assign aluVal2 = _T_28 ? decodeBitMask_io_wmask : _T_68; // @[Mux.scala 80:57]
  assign _T_76 = ~aluVal2; // @[Execute.scala 504:40]
  assign _T_77 = io_dinst_op[0] ? _T_76 : aluVal2; // @[Execute.scala 504:23]
  assign _T_81 = io_dinst_op == 4'h1; // @[Execute.scala 528:44]
  assign _T_83 = io_dinst_itype == 5'h6; // @[Execute.scala 529:23]
  assign _T_84 = io_dinst_op == 4'h2; // @[Execute.scala 530:30]
  assign _T_85 = io_dinst_op == 4'h3; // @[Execute.scala 530:58]
  assign _T_86 = _T_84 | _T_85; // @[Execute.scala 530:43]
  assign _T_89 = _T_81 | _T_85; // @[Execute.scala 531:45]
  assign _T_91 = _T_86 ? _T_76 : aluVal2; // @[Execute.scala 533:29]
  assign _T_93 = io_dinst_itype == 5'h4; // @[Execute.scala 535:29]
  assign _T_94 = io_dinst_itype == 5'h5; // @[Execute.scala 535:59]
  assign _T_95 = _T_93 | _T_94; // @[Execute.scala 535:41]
  assign _GEN_2 = _T_95 ? _T_77 : _T_77; // @[Execute.scala 535:72]
  assign _GEN_3 = _T_95 ? _T_81 : _T_81; // @[Execute.scala 535:72]
  assign _T_101 = condHolds_io_res ? rVal1 : addWithCarry_io_res; // @[Execute.scala 554:19]
  assign _T_103 = _T_30 ? bitfield_io_res : logicALU_io_res; // @[Mux.scala 80:57]
  assign _T_105 = _T_24 ? logicALU_io_res : _T_103; // @[Mux.scala 80:57]
  assign _T_107 = _T_28 ? logicALU_io_res : _T_105; // @[Mux.scala 80:57]
  assign _T_108 = 5'hc == io_dinst_itype; // @[Mux.scala 80:60]
  assign _T_109 = _T_108 ? dataProcessing_io_res : _T_107; // @[Mux.scala 80:57]
  assign _T_111 = _T_26 ? shiftALU_io_res : _T_109; // @[Mux.scala 80:57]
  assign _T_112 = 5'he == io_dinst_itype; // @[Mux.scala 80:60]
  assign _T_113 = _T_112 ? dataProc3S_io_res : _T_111; // @[Mux.scala 80:57]
  assign _T_115 = _T_12 ? addWithCarry_io_res : _T_113; // @[Mux.scala 80:57]
  assign _T_117 = _T_16 ? addWithCarry_io_res : _T_115; // @[Mux.scala 80:57]
  assign _T_118 = 5'ha == io_dinst_itype; // @[Mux.scala 80:60]
  assign _T_119 = _T_118 ? move_io_res : _T_117; // @[Mux.scala 80:57]
  assign res = _T_22 ? _T_101 : _T_119; // @[Mux.scala 80:57]
  assign _T_123 = {1'h0,res[31:0]}; // @[Cat.scala 29:58]
  assign _T_125 = io_dinst_itype == 5'h1; // @[Execute.scala 558:23]
  assign _T_126 = io_dinst_itype == 5'h2; // @[Execute.scala 558:53]
  assign _T_127 = _T_125 | _T_126; // @[Execute.scala 558:35]
  assign _T_128 = io_dinst_rd_bits != 5'h1f; // @[Execute.scala 559:40]
  assign _T_129 = condHolds_io_res ? addWithCarry_io_nzcv : io_dinst_nzcv_bits; // @[Execute.scala 564:42]
  assign _T_132 = _T_24 ? logicALU_io_nzcv : addWithCarry_io_nzcv; // @[Mux.scala 80:57]
  assign _T_134 = _T_28 ? logicALU_io_nzcv : _T_132; // @[Mux.scala 80:57]
  assign _T_136 = _T_18 ? _T_129 : _T_134; // @[Mux.scala 80:57]
  assign _T_142 = _T_28 | _T_24; // @[Mux.scala 80:57]
  assign _T_144 = _T_30 | _T_142; // @[Mux.scala 80:57]
  assign _T_146 = _T_108 | _T_144; // @[Mux.scala 80:57]
  assign _T_148 = _T_26 | _T_146; // @[Mux.scala 80:57]
  assign _T_150 = _T_112 | _T_148; // @[Mux.scala 80:57]
  assign _T_152 = _T_18 | _T_150; // @[Mux.scala 80:57]
  assign _T_154 = _T_20 | _T_152; // @[Mux.scala 80:57]
  assign _T_156 = _T_16 | _T_154; // @[Mux.scala 80:57]
  assign _T_158 = _T_12 | _T_156; // @[Mux.scala 80:57]
  assign _T_160 = _T_118 | _T_158; // @[Mux.scala 80:57]
  assign io_condRes = condHolds_io_res; // @[Execute.scala 449:14]
  assign io_einst_valid = _T_22 | _T_160; // @[Execute.scala 570:18]
  assign io_einst_bits_rd_valid = _T_127 ? _T_128 : io_dinst_rd_valid; // @[Execute.scala 569:17]
  assign io_einst_bits_rd_bits = io_dinst_rd_bits; // @[Execute.scala 569:17]
  assign io_einst_bits_nzcv_valid = io_dinst_nzcv_valid; // @[Execute.scala 569:17]
  assign io_einst_bits_nzcv_bits = _T_20 ? _T_129 : _T_136; // @[Execute.scala 569:17]
  assign io_einst_bits_res = io_dinst_is32bit ? {{31'd0}, _T_123} : res; // @[Execute.scala 569:17]
  assign shiftALU_io_word = _T_30 ? rVal1 : _T_29; // @[Execute.scala 425:20]
  assign shiftALU_io_amount = io_dinst_shift_val_valid ? _T_34 : 6'h0; // @[Execute.scala 439:22]
  assign shiftALU_io_opcode = io_dinst_shift_type; // @[Execute.scala 442:22]
  assign shiftALU_io_is32bit = io_dinst_is32bit; // @[Execute.scala 443:23]
  assign condHolds_io_cond = io_dinst_cond_bits; // @[Execute.scala 447:21]
  assign condHolds_io_nzcv = io_nzcv; // @[Execute.scala 448:21]
  assign decodeBitMask_io_immn = io_dinst_imm[12]; // @[Execute.scala 454:25]
  assign decodeBitMask_io_imms = io_dinst_imm[5:0]; // @[Execute.scala 456:25]
  assign decodeBitMask_io_immr = io_dinst_imm[11:6]; // @[Execute.scala 455:25]
  assign decodeBitMask_io_is32bit = io_dinst_is32bit; // @[Execute.scala 457:28]
  assign bitfield_io_op = io_dinst_op; // @[Execute.scala 462:18]
  assign bitfield_io_imms = io_dinst_imm[5:0]; // @[Execute.scala 464:20]
  assign bitfield_io_src = _T_11 ? io_rVal1 : _T_1; // @[Execute.scala 465:19]
  assign bitfield_io_dst = _T_2 ? 64'h0 : io_rVal2; // @[Execute.scala 466:19]
  assign bitfield_io_wmask = decodeBitMask_io_wmask; // @[Execute.scala 467:21]
  assign bitfield_io_tmask = decodeBitMask_io_tmask; // @[Execute.scala 468:21]
  assign bitfield_io_rorSrcR = shiftALU_io_res; // @[Execute.scala 469:23]
  assign move_io_op = io_dinst_op; // @[Execute.scala 473:14]
  assign move_io_hw = io_dinst_imm[17:16]; // @[Execute.scala 474:14]
  assign move_io_imm = io_dinst_imm[15:0]; // @[Execute.scala 475:15]
  assign move_io_rd = _T_2 ? 64'h0 : io_rVal2; // @[Execute.scala 476:14]
  assign logicALU_io_a = _T_11 ? io_rVal1 : _T_1; // @[Execute.scala 503:17]
  assign logicALU_io_b = io_dinst_op[0] ? _T_76 : aluVal2; // @[Execute.scala 504:17]
  assign logicALU_io_opcode = io_dinst_op; // @[Execute.scala 505:22]
  assign logicALU_io_is32bit = io_dinst_is32bit; // @[Execute.scala 506:23]
  assign dataProcessing_io_a = _T_11 ? io_rVal1 : _T_1; // @[Execute.scala 510:23]
  assign dataProcessing_io_op = io_dinst_op; // @[Execute.scala 512:24]
  assign dataProcessing_io_is32bit = io_dinst_is32bit; // @[Execute.scala 513:29]
  assign dataProc3S_clock = clock;
  assign dataProc3S_reset = reset;
  assign dataProc3S_io_rVal1 = _T_11 ? io_rVal1 : _T_1; // @[Execute.scala 518:23]
  assign dataProc3S_io_rVal2 = _T_2 ? 64'h0 : io_rVal2; // @[Execute.scala 519:23]
  assign dataProc3S_io_rVal3 = _T_5 ? 64'h0 : io_rVal3; // @[Execute.scala 520:23]
  assign dataProc3S_io_is32bit = io_dinst_is32bit; // @[Execute.scala 521:25]
  assign addWithCarry_io_is32bit = io_dinst_is32bit; // @[Execute.scala 525:27]
  assign addWithCarry_io_a = _T_83 ? 64'h0 : rVal1; // @[Execute.scala 526:21 Execute.scala 532:23 Execute.scala 536:23]
  assign addWithCarry_io_b = _T_83 ? _T_91 : _GEN_2; // @[Execute.scala 527:21 Execute.scala 533:23 Execute.scala 537:23]
  assign addWithCarry_io_carry = _T_83 ? _T_89 : _GEN_3; // @[Execute.scala 528:25 Execute.scala 534:27 Execute.scala 538:27]
endmodule
