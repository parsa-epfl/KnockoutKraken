module AxiMemoryMappedRegFile(
  input         clock,
  input         reset,
  input  [5:0]  io_axiLite_awaddr,
  input         io_axiLite_awvalid,
  input  [31:0] io_axiLite_wdata,
  input  [3:0]  io_axiLite_wstrb,
  input         io_axiLite_wvalid,
  output [1:0]  io_axiLite_bresp,
  output        io_axiLite_bvalid,
  input  [5:0]  io_axiLite_araddr,
  input         io_axiLite_arvalid,
  output [31:0] io_axiLite_rdata,
  output        io_axiLite_rvalid,
  input         io_axiLite_rready,
  input  [31:0] io_regsInput_1,
  input  [31:0] io_regsInput_3,
  input  [31:0] io_regsInput_4,
  input  [31:0] io_regsInput_5,
  output [31:0] io_regsOutput_0,
  output [31:0] io_regsOutput_8,
  output [31:0] io_regsOutput_9,
  output [31:0] io_regsOutput_10
);
  reg [31:0] regFile_0; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_0;
  reg [31:0] regFile_1; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_1;
  reg [31:0] regFile_2; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_2;
  reg [31:0] regFile_3; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_3;
  reg [31:0] regFile_4; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_4;
  reg [31:0] regFile_5; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_5;
  reg [31:0] regFile_6; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_6;
  reg [31:0] regFile_7; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_7;
  reg [31:0] regFile_8; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_8;
  reg [31:0] regFile_9; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_9;
  reg [31:0] regFile_10; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_10;
  reg [31:0] regFile_11; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_11;
  reg [31:0] regFile_12; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_12;
  reg [31:0] regFile_13; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_13;
  reg [31:0] regFile_14; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_14;
  reg [31:0] regFile_15; // @[AxiMMRegs.scala 31:24]
  reg [31:0] _RAND_15;
  wire [5:0] axiLite_awaddr; // @[AxiMMRegs.scala 34:42]
  wire [31:0] _GEN_1; // @[AxiMMRegs.scala 41:43]
  wire [31:0] _GEN_2; // @[AxiMMRegs.scala 41:43]
  wire [31:0] _GEN_3; // @[AxiMMRegs.scala 41:43]
  wire [31:0] _GEN_4; // @[AxiMMRegs.scala 41:43]
  wire [31:0] _GEN_5; // @[AxiMMRegs.scala 41:43]
  wire [31:0] _GEN_6; // @[AxiMMRegs.scala 41:43]
  wire [31:0] _GEN_7; // @[AxiMMRegs.scala 41:43]
  wire [31:0] _GEN_8; // @[AxiMMRegs.scala 41:43]
  wire [31:0] _GEN_9; // @[AxiMMRegs.scala 41:43]
  wire [31:0] _GEN_10; // @[AxiMMRegs.scala 41:43]
  wire [31:0] _GEN_11; // @[AxiMMRegs.scala 41:43]
  wire [31:0] _GEN_12; // @[AxiMMRegs.scala 41:43]
  wire [31:0] _GEN_13; // @[AxiMMRegs.scala 41:43]
  wire [31:0] _GEN_14; // @[AxiMMRegs.scala 41:43]
  wire [31:0] _GEN_15; // @[AxiMMRegs.scala 41:43]
  wire [7:0] byteVecReg_0; // @[AxiMMRegs.scala 41:43]
  wire [7:0] byteVecIn_0; // @[AxiMMRegs.scala 41:43]
  wire [7:0] byteVecOut_0; // @[AxiMMRegs.scala 48:25]
  wire [7:0] byteVecReg_1; // @[AxiMMRegs.scala 41:43]
  wire [7:0] byteVecIn_1; // @[AxiMMRegs.scala 41:43]
  wire [7:0] byteVecOut_1; // @[AxiMMRegs.scala 48:25]
  wire [7:0] byteVecReg_2; // @[AxiMMRegs.scala 41:43]
  wire [7:0] byteVecIn_2; // @[AxiMMRegs.scala 41:43]
  wire [7:0] byteVecOut_2; // @[AxiMMRegs.scala 48:25]
  wire [7:0] byteVecReg_3; // @[AxiMMRegs.scala 41:43]
  wire [7:0] byteVecIn_3; // @[AxiMMRegs.scala 41:43]
  wire [7:0] byteVecOut_3; // @[AxiMMRegs.scala 48:25]
  wire  _T_21; // @[AxiMMRegs.scala 51:27]
  wire [31:0] _T_25; // @[AxiMMRegs.scala 52:42]
  wire  _T_26; // @[AxiMMRegs.scala 54:57]
  wire  _T_27; // @[AxiMMRegs.scala 54:54]
  reg [1:0] _T_29; // @[AxiMMRegs.scala 54:30]
  reg [31:0] _RAND_16;
  reg  _T_31; // @[AxiMMRegs.scala 55:31]
  reg [31:0] _RAND_17;
  wire  _T_33; // @[AxiMMRegs.scala 58:77]
  wire  _T_34; // @[AxiMMRegs.scala 58:59]
  reg [31:0] regsOutput_0; // @[AxiMMRegs.scala 61:31]
  reg [31:0] _RAND_18;
  wire  _T_38; // @[AxiMMRegs.scala 58:77]
  wire  _T_39; // @[AxiMMRegs.scala 58:59]
  wire [1:0] _GEN_96; // @[AxiMMRegs.scala 66:21]
  wire  _T_42; // @[AxiMMRegs.scala 58:77]
  wire  _T_43; // @[AxiMMRegs.scala 58:59]
  wire [1:0] _GEN_97; // @[AxiMMRegs.scala 66:21]
  wire  _T_46; // @[AxiMMRegs.scala 58:77]
  wire  _T_47; // @[AxiMMRegs.scala 58:59]
  wire [1:0] _GEN_98; // @[AxiMMRegs.scala 66:21]
  wire  _T_50; // @[AxiMMRegs.scala 58:77]
  wire  _T_51; // @[AxiMMRegs.scala 58:59]
  wire [1:0] _GEN_99; // @[AxiMMRegs.scala 66:21]
  wire  _T_54; // @[AxiMMRegs.scala 58:77]
  wire  _T_55; // @[AxiMMRegs.scala 58:59]
  wire [1:0] _GEN_100; // @[AxiMMRegs.scala 66:21]
  wire  _T_58; // @[AxiMMRegs.scala 58:77]
  wire  _T_59; // @[AxiMMRegs.scala 58:59]
  wire [1:0] _GEN_101; // @[AxiMMRegs.scala 66:21]
  wire  _T_62; // @[AxiMMRegs.scala 58:77]
  wire  _T_63; // @[AxiMMRegs.scala 58:59]
  reg [5:0] _T_89; // @[AxiMMRegs.scala 77:41]
  reg [31:0] _RAND_19;
  wire [31:0] _GEN_104; // @[AxiMMRegs.scala 77:20]
  wire [31:0] _GEN_105; // @[AxiMMRegs.scala 77:20]
  wire [31:0] _GEN_106; // @[AxiMMRegs.scala 77:20]
  wire [31:0] _GEN_107; // @[AxiMMRegs.scala 77:20]
  wire [31:0] _GEN_108; // @[AxiMMRegs.scala 77:20]
  wire [31:0] _GEN_109; // @[AxiMMRegs.scala 77:20]
  wire [31:0] _GEN_110; // @[AxiMMRegs.scala 77:20]
  wire [31:0] _GEN_111; // @[AxiMMRegs.scala 77:20]
  wire [31:0] _GEN_112; // @[AxiMMRegs.scala 77:20]
  wire [31:0] _GEN_113; // @[AxiMMRegs.scala 77:20]
  wire [31:0] _GEN_114; // @[AxiMMRegs.scala 77:20]
  wire [31:0] _GEN_115; // @[AxiMMRegs.scala 77:20]
  wire [31:0] _GEN_116; // @[AxiMMRegs.scala 77:20]
  wire [31:0] _GEN_117; // @[AxiMMRegs.scala 77:20]
  reg  _T_93; // @[AxiMMRegs.scala 78:31]
  reg [31:0] _RAND_20;
  assign axiLite_awaddr = {{2'd0}, io_axiLite_awaddr[5:2]}; // @[AxiMMRegs.scala 34:42]
  assign _GEN_1 = 4'h1 == axiLite_awaddr[3:0] ? regFile_1 : regFile_0; // @[AxiMMRegs.scala 41:43]
  assign _GEN_2 = 4'h2 == axiLite_awaddr[3:0] ? regFile_2 : _GEN_1; // @[AxiMMRegs.scala 41:43]
  assign _GEN_3 = 4'h3 == axiLite_awaddr[3:0] ? regFile_3 : _GEN_2; // @[AxiMMRegs.scala 41:43]
  assign _GEN_4 = 4'h4 == axiLite_awaddr[3:0] ? regFile_4 : _GEN_3; // @[AxiMMRegs.scala 41:43]
  assign _GEN_5 = 4'h5 == axiLite_awaddr[3:0] ? regFile_5 : _GEN_4; // @[AxiMMRegs.scala 41:43]
  assign _GEN_6 = 4'h6 == axiLite_awaddr[3:0] ? regFile_6 : _GEN_5; // @[AxiMMRegs.scala 41:43]
  assign _GEN_7 = 4'h7 == axiLite_awaddr[3:0] ? regFile_7 : _GEN_6; // @[AxiMMRegs.scala 41:43]
  assign _GEN_8 = 4'h8 == axiLite_awaddr[3:0] ? regFile_8 : _GEN_7; // @[AxiMMRegs.scala 41:43]
  assign _GEN_9 = 4'h9 == axiLite_awaddr[3:0] ? regFile_9 : _GEN_8; // @[AxiMMRegs.scala 41:43]
  assign _GEN_10 = 4'ha == axiLite_awaddr[3:0] ? regFile_10 : _GEN_9; // @[AxiMMRegs.scala 41:43]
  assign _GEN_11 = 4'hb == axiLite_awaddr[3:0] ? regFile_11 : _GEN_10; // @[AxiMMRegs.scala 41:43]
  assign _GEN_12 = 4'hc == axiLite_awaddr[3:0] ? regFile_12 : _GEN_11; // @[AxiMMRegs.scala 41:43]
  assign _GEN_13 = 4'hd == axiLite_awaddr[3:0] ? regFile_13 : _GEN_12; // @[AxiMMRegs.scala 41:43]
  assign _GEN_14 = 4'he == axiLite_awaddr[3:0] ? regFile_14 : _GEN_13; // @[AxiMMRegs.scala 41:43]
  assign _GEN_15 = 4'hf == axiLite_awaddr[3:0] ? regFile_15 : _GEN_14; // @[AxiMMRegs.scala 41:43]
  assign byteVecReg_0 = _GEN_15[7:0]; // @[AxiMMRegs.scala 41:43]
  assign byteVecIn_0 = io_axiLite_wdata[7:0]; // @[AxiMMRegs.scala 41:43]
  assign byteVecOut_0 = io_axiLite_wstrb[0] ? byteVecIn_0 : byteVecReg_0; // @[AxiMMRegs.scala 48:25]
  assign byteVecReg_1 = _GEN_15[15:8]; // @[AxiMMRegs.scala 41:43]
  assign byteVecIn_1 = io_axiLite_wdata[15:8]; // @[AxiMMRegs.scala 41:43]
  assign byteVecOut_1 = io_axiLite_wstrb[1] ? byteVecIn_1 : byteVecReg_1; // @[AxiMMRegs.scala 48:25]
  assign byteVecReg_2 = _GEN_15[23:16]; // @[AxiMMRegs.scala 41:43]
  assign byteVecIn_2 = io_axiLite_wdata[23:16]; // @[AxiMMRegs.scala 41:43]
  assign byteVecOut_2 = io_axiLite_wstrb[2] ? byteVecIn_2 : byteVecReg_2; // @[AxiMMRegs.scala 48:25]
  assign byteVecReg_3 = _GEN_15[31:24]; // @[AxiMMRegs.scala 41:43]
  assign byteVecIn_3 = io_axiLite_wdata[31:24]; // @[AxiMMRegs.scala 41:43]
  assign byteVecOut_3 = io_axiLite_wstrb[3] ? byteVecIn_3 : byteVecReg_3; // @[AxiMMRegs.scala 48:25]
  assign _T_21 = io_axiLite_awvalid & io_axiLite_wvalid; // @[AxiMMRegs.scala 51:27]
  assign _T_25 = {byteVecOut_3,byteVecOut_2,byteVecOut_1,byteVecOut_0}; // @[AxiMMRegs.scala 52:42]
  assign _T_26 = ~io_axiLite_awvalid; // @[AxiMMRegs.scala 54:57]
  assign _T_27 = io_axiLite_wvalid & _T_26; // @[AxiMMRegs.scala 54:54]
  assign _T_33 = axiLite_awaddr == 6'h0; // @[AxiMMRegs.scala 58:77]
  assign _T_34 = _T_21 & _T_33; // @[AxiMMRegs.scala 58:59]
  assign _T_38 = axiLite_awaddr == 6'h1; // @[AxiMMRegs.scala 58:77]
  assign _T_39 = _T_21 & _T_38; // @[AxiMMRegs.scala 58:59]
  assign _GEN_96 = _T_39 ? 2'h2 : _T_29; // @[AxiMMRegs.scala 66:21]
  assign _T_42 = axiLite_awaddr == 6'h2; // @[AxiMMRegs.scala 58:77]
  assign _T_43 = _T_21 & _T_42; // @[AxiMMRegs.scala 58:59]
  assign _GEN_97 = _T_43 ? 2'h2 : _GEN_96; // @[AxiMMRegs.scala 66:21]
  assign _T_46 = axiLite_awaddr == 6'h3; // @[AxiMMRegs.scala 58:77]
  assign _T_47 = _T_21 & _T_46; // @[AxiMMRegs.scala 58:59]
  assign _GEN_98 = _T_47 ? 2'h2 : _GEN_97; // @[AxiMMRegs.scala 66:21]
  assign _T_50 = axiLite_awaddr == 6'h4; // @[AxiMMRegs.scala 58:77]
  assign _T_51 = _T_21 & _T_50; // @[AxiMMRegs.scala 58:59]
  assign _GEN_99 = _T_51 ? 2'h2 : _GEN_98; // @[AxiMMRegs.scala 66:21]
  assign _T_54 = axiLite_awaddr == 6'h5; // @[AxiMMRegs.scala 58:77]
  assign _T_55 = _T_21 & _T_54; // @[AxiMMRegs.scala 58:59]
  assign _GEN_100 = _T_55 ? 2'h2 : _GEN_99; // @[AxiMMRegs.scala 66:21]
  assign _T_58 = axiLite_awaddr == 6'h6; // @[AxiMMRegs.scala 58:77]
  assign _T_59 = _T_21 & _T_58; // @[AxiMMRegs.scala 58:59]
  assign _GEN_101 = _T_59 ? 2'h2 : _GEN_100; // @[AxiMMRegs.scala 66:21]
  assign _T_62 = axiLite_awaddr == 6'h7; // @[AxiMMRegs.scala 58:77]
  assign _T_63 = _T_21 & _T_62; // @[AxiMMRegs.scala 58:59]
  assign _GEN_104 = 4'h1 == _T_89[3:0] ? io_regsInput_1 : regsOutput_0; // @[AxiMMRegs.scala 77:20]
  assign _GEN_105 = 4'h2 == _T_89[3:0] ? 32'h0 : _GEN_104; // @[AxiMMRegs.scala 77:20]
  assign _GEN_106 = 4'h3 == _T_89[3:0] ? io_regsInput_3 : _GEN_105; // @[AxiMMRegs.scala 77:20]
  assign _GEN_107 = 4'h4 == _T_89[3:0] ? io_regsInput_4 : _GEN_106; // @[AxiMMRegs.scala 77:20]
  assign _GEN_108 = 4'h5 == _T_89[3:0] ? io_regsInput_5 : _GEN_107; // @[AxiMMRegs.scala 77:20]
  assign _GEN_109 = 4'h6 == _T_89[3:0] ? 32'h0 : _GEN_108; // @[AxiMMRegs.scala 77:20]
  assign _GEN_110 = 4'h7 == _T_89[3:0] ? 32'h0 : _GEN_109; // @[AxiMMRegs.scala 77:20]
  assign _GEN_111 = 4'h8 == _T_89[3:0] ? regFile_8 : _GEN_110; // @[AxiMMRegs.scala 77:20]
  assign _GEN_112 = 4'h9 == _T_89[3:0] ? regFile_9 : _GEN_111; // @[AxiMMRegs.scala 77:20]
  assign _GEN_113 = 4'ha == _T_89[3:0] ? regFile_10 : _GEN_112; // @[AxiMMRegs.scala 77:20]
  assign _GEN_114 = 4'hb == _T_89[3:0] ? regFile_11 : _GEN_113; // @[AxiMMRegs.scala 77:20]
  assign _GEN_115 = 4'hc == _T_89[3:0] ? regFile_12 : _GEN_114; // @[AxiMMRegs.scala 77:20]
  assign _GEN_116 = 4'hd == _T_89[3:0] ? regFile_13 : _GEN_115; // @[AxiMMRegs.scala 77:20]
  assign _GEN_117 = 4'he == _T_89[3:0] ? regFile_14 : _GEN_116; // @[AxiMMRegs.scala 77:20]
  assign io_axiLite_bresp = _T_63 ? 2'h2 : _GEN_101; // @[AxiMMRegs.scala 54:20 AxiMMRegs.scala 66:40 AxiMMRegs.scala 66:40 AxiMMRegs.scala 66:40 AxiMMRegs.scala 66:40 AxiMMRegs.scala 66:40 AxiMMRegs.scala 66:40 AxiMMRegs.scala 66:40]
  assign io_axiLite_bvalid = _T_31; // @[AxiMMRegs.scala 55:21]
  assign io_axiLite_rdata = 4'hf == _T_89[3:0] ? regFile_15 : _GEN_117; // @[AxiMMRegs.scala 77:20]
  assign io_axiLite_rvalid = _T_93; // @[AxiMMRegs.scala 78:21]
  assign io_regsOutput_0 = regsOutput_0; // @[AxiMMRegs.scala 81:17]
  assign io_regsOutput_8 = regFile_8; // @[AxiMMRegs.scala 81:17]
  assign io_regsOutput_9 = regFile_9; // @[AxiMMRegs.scala 81:17]
  assign io_regsOutput_10 = regFile_10; // @[AxiMMRegs.scala 81:17]
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
  _RAND_0 = {1{`RANDOM}};
  regFile_0 = _RAND_0[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  regFile_1 = _RAND_1[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  regFile_2 = _RAND_2[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  regFile_3 = _RAND_3[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  regFile_4 = _RAND_4[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  regFile_5 = _RAND_5[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  regFile_6 = _RAND_6[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  regFile_7 = _RAND_7[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  regFile_8 = _RAND_8[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  regFile_9 = _RAND_9[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  regFile_10 = _RAND_10[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {1{`RANDOM}};
  regFile_11 = _RAND_11[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_12 = {1{`RANDOM}};
  regFile_12 = _RAND_12[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_13 = {1{`RANDOM}};
  regFile_13 = _RAND_13[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_14 = {1{`RANDOM}};
  regFile_14 = _RAND_14[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_15 = {1{`RANDOM}};
  regFile_15 = _RAND_15[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_16 = {1{`RANDOM}};
  _T_29 = _RAND_16[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_17 = {1{`RANDOM}};
  _T_31 = _RAND_17[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_18 = {1{`RANDOM}};
  regsOutput_0 = _RAND_18[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_19 = {1{`RANDOM}};
  _T_89 = _RAND_19[5:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_20 = {1{`RANDOM}};
  _T_93 = _RAND_20[0:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (reset) begin
      regFile_0 <= 32'h0;
    end else if (_T_21) begin
      if (4'h0 == axiLite_awaddr[3:0]) begin
        regFile_0 <= _T_25;
      end
    end
    if (reset) begin
      regFile_1 <= 32'h0;
    end else if (_T_21) begin
      if (4'h1 == axiLite_awaddr[3:0]) begin
        regFile_1 <= _T_25;
      end
    end
    if (reset) begin
      regFile_2 <= 32'h0;
    end else if (_T_21) begin
      if (4'h2 == axiLite_awaddr[3:0]) begin
        regFile_2 <= _T_25;
      end
    end
    if (reset) begin
      regFile_3 <= 32'h0;
    end else if (_T_21) begin
      if (4'h3 == axiLite_awaddr[3:0]) begin
        regFile_3 <= _T_25;
      end
    end
    if (reset) begin
      regFile_4 <= 32'h0;
    end else if (_T_21) begin
      if (4'h4 == axiLite_awaddr[3:0]) begin
        regFile_4 <= _T_25;
      end
    end
    if (reset) begin
      regFile_5 <= 32'h0;
    end else if (_T_21) begin
      if (4'h5 == axiLite_awaddr[3:0]) begin
        regFile_5 <= _T_25;
      end
    end
    if (reset) begin
      regFile_6 <= 32'h0;
    end else if (_T_21) begin
      if (4'h6 == axiLite_awaddr[3:0]) begin
        regFile_6 <= _T_25;
      end
    end
    if (reset) begin
      regFile_7 <= 32'h0;
    end else if (_T_21) begin
      if (4'h7 == axiLite_awaddr[3:0]) begin
        regFile_7 <= _T_25;
      end
    end
    if (reset) begin
      regFile_8 <= 32'h0;
    end else if (_T_21) begin
      if (4'h8 == axiLite_awaddr[3:0]) begin
        regFile_8 <= _T_25;
      end
    end
    if (reset) begin
      regFile_9 <= 32'h0;
    end else if (_T_21) begin
      if (4'h9 == axiLite_awaddr[3:0]) begin
        regFile_9 <= _T_25;
      end
    end
    if (reset) begin
      regFile_10 <= 32'h0;
    end else if (_T_21) begin
      if (4'ha == axiLite_awaddr[3:0]) begin
        regFile_10 <= _T_25;
      end
    end
    if (reset) begin
      regFile_11 <= 32'h0;
    end else if (_T_21) begin
      if (4'hb == axiLite_awaddr[3:0]) begin
        regFile_11 <= _T_25;
      end
    end
    if (reset) begin
      regFile_12 <= 32'h0;
    end else if (_T_21) begin
      if (4'hc == axiLite_awaddr[3:0]) begin
        regFile_12 <= _T_25;
      end
    end
    if (reset) begin
      regFile_13 <= 32'h0;
    end else if (_T_21) begin
      if (4'hd == axiLite_awaddr[3:0]) begin
        regFile_13 <= _T_25;
      end
    end
    if (reset) begin
      regFile_14 <= 32'h0;
    end else if (_T_21) begin
      if (4'he == axiLite_awaddr[3:0]) begin
        regFile_14 <= _T_25;
      end
    end
    if (reset) begin
      regFile_15 <= 32'h0;
    end else if (_T_21) begin
      if (4'hf == axiLite_awaddr[3:0]) begin
        regFile_15 <= _T_25;
      end
    end
    if (_T_27) begin
      _T_29 <= 2'h2;
    end else begin
      _T_29 <= 2'h0;
    end
    _T_31 <= io_axiLite_awvalid & io_axiLite_wvalid;
    if (_T_34) begin
      regsOutput_0 <= io_axiLite_wdata;
    end else begin
      regsOutput_0 <= 32'h0;
    end
    _T_89 <= {{2'd0}, io_axiLite_araddr[5:2]};
    _T_93 <= io_axiLite_arvalid & io_axiLite_rready;
  end
endmodule
