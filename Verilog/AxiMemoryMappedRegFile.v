module AxiMemoryMappedRegFile( // @[:@3.2]
  input         clock, // @[:@4.4]
  input         reset, // @[:@5.4]
  input  [3:0]  io_axiLite_awaddr, // @[:@6.4]
  input         io_axiLite_awvalid, // @[:@6.4]
  input  [31:0] io_axiLite_wdata, // @[:@6.4]
  input  [3:0]  io_axiLite_wstrb, // @[:@6.4]
  input         io_axiLite_wvalid, // @[:@6.4]
  output [1:0]  io_axiLite_bresp, // @[:@6.4]
  output        io_axiLite_bvalid, // @[:@6.4]
  input  [3:0]  io_axiLite_araddr, // @[:@6.4]
  input         io_axiLite_arvalid, // @[:@6.4]
  output [31:0] io_axiLite_rdata, // @[:@6.4]
  output        io_axiLite_rvalid, // @[:@6.4]
  input         io_axiLite_rready, // @[:@6.4]
  input  [31:0] io_regsInput_1, // @[:@6.4]
  output [31:0] io_regsOutput_0 // @[:@6.4]
);
  reg [31:0] regFile_0; // @[AxiMMRegs.scala 30:24:@13.4]
  reg [31:0] _RAND_0;
  reg [31:0] regFile_1; // @[AxiMMRegs.scala 30:24:@13.4]
  reg [31:0] _RAND_1;
  reg [31:0] regFile_2; // @[AxiMMRegs.scala 30:24:@13.4]
  reg [31:0] _RAND_2;
  reg [31:0] regFile_3; // @[AxiMMRegs.scala 30:24:@13.4]
  reg [31:0] _RAND_3;
  wire [3:0] axiLite_awaddr; // @[AxiMMRegs.scala 33:42:@19.4]
  wire [1:0] _T_172; // @[:@26.4]
  wire [31:0] _GEN_1; // @[AxiMMRegs.scala 40:43:@27.4]
  wire [31:0] _GEN_2; // @[AxiMMRegs.scala 40:43:@27.4]
  wire [31:0] _GEN_3; // @[AxiMMRegs.scala 40:43:@27.4]
  wire [7:0] byteVecReg_0; // @[AxiMMRegs.scala 40:43:@27.4]
  wire [7:0] byteVecIn_0; // @[AxiMMRegs.scala 40:43:@29.4]
  wire  _T_175; // @[AxiMMRegs.scala 47:42:@31.4]
  wire [7:0] byteVecOut_0; // @[AxiMMRegs.scala 47:25:@32.4]
  wire [7:0] byteVecReg_1; // @[AxiMMRegs.scala 40:43:@35.4]
  wire [7:0] byteVecIn_1; // @[AxiMMRegs.scala 40:43:@37.4]
  wire  _T_183; // @[AxiMMRegs.scala 47:42:@39.4]
  wire [7:0] byteVecOut_1; // @[AxiMMRegs.scala 47:25:@40.4]
  wire [7:0] byteVecReg_2; // @[AxiMMRegs.scala 40:43:@43.4]
  wire [7:0] byteVecIn_2; // @[AxiMMRegs.scala 40:43:@45.4]
  wire  _T_191; // @[AxiMMRegs.scala 47:42:@47.4]
  wire [7:0] byteVecOut_2; // @[AxiMMRegs.scala 47:25:@48.4]
  wire [7:0] byteVecReg_3; // @[AxiMMRegs.scala 40:43:@51.4]
  wire [7:0] byteVecIn_3; // @[AxiMMRegs.scala 40:43:@53.4]
  wire  _T_199; // @[AxiMMRegs.scala 47:42:@55.4]
  wire [7:0] byteVecOut_3; // @[AxiMMRegs.scala 47:25:@56.4]
  wire  _T_201; // @[AxiMMRegs.scala 50:27:@58.4]
  wire [31:0] _T_208; // @[AxiMMRegs.scala 51:42:@63.6]
  wire [31:0] _GEN_16; // @[AxiMMRegs.scala 51:28:@64.6]
  wire [31:0] _GEN_17; // @[AxiMMRegs.scala 51:28:@64.6]
  wire [31:0] _GEN_18; // @[AxiMMRegs.scala 51:28:@64.6]
  wire [31:0] _GEN_19; // @[AxiMMRegs.scala 51:28:@64.6]
  wire [31:0] _GEN_20; // @[AxiMMRegs.scala 50:49:@59.4]
  wire [31:0] _GEN_21; // @[AxiMMRegs.scala 50:49:@59.4]
  wire [31:0] _GEN_22; // @[AxiMMRegs.scala 50:49:@59.4]
  wire [31:0] _GEN_23; // @[AxiMMRegs.scala 50:49:@59.4]
  wire  _T_210; // @[AxiMMRegs.scala 53:57:@66.4]
  wire  _T_211; // @[AxiMMRegs.scala 53:54:@67.4]
  reg [1:0] _T_214; // @[AxiMMRegs.scala 53:30:@69.4]
  reg [31:0] _RAND_4;
  reg  _T_217; // @[AxiMMRegs.scala 54:31:@73.4]
  reg [31:0] _RAND_5;
  wire  _T_220; // @[AxiMMRegs.scala 57:77:@77.4]
  wire  _T_221; // @[AxiMMRegs.scala 57:59:@78.4]
  reg [31:0] regsOutput_0; // @[AxiMMRegs.scala 60:31:@80.4]
  reg [31:0] _RAND_6;
  wire  _T_228; // @[AxiMMRegs.scala 57:77:@84.4]
  wire  _T_229; // @[AxiMMRegs.scala 57:59:@85.4]
  reg [3:0] _T_242; // @[AxiMMRegs.scala 76:41:@101.4]
  reg [31:0] _RAND_7;
  wire [1:0] _T_246; // @[:@104.4]
  wire [31:0] _GEN_26; // @[AxiMMRegs.scala 76:20:@105.4]
  wire [31:0] _GEN_27; // @[AxiMMRegs.scala 76:20:@105.4]
  reg  _T_249; // @[AxiMMRegs.scala 77:31:@107.4]
  reg [31:0] _RAND_8;
  assign axiLite_awaddr = io_axiLite_awaddr >> 2'h2; // @[AxiMMRegs.scala 33:42:@19.4]
  assign _T_172 = axiLite_awaddr[1:0]; // @[:@26.4]
  assign _GEN_1 = 2'h1 == _T_172 ? regFile_1 : regFile_0; // @[AxiMMRegs.scala 40:43:@27.4]
  assign _GEN_2 = 2'h2 == _T_172 ? regFile_2 : _GEN_1; // @[AxiMMRegs.scala 40:43:@27.4]
  assign _GEN_3 = 2'h3 == _T_172 ? regFile_3 : _GEN_2; // @[AxiMMRegs.scala 40:43:@27.4]
  assign byteVecReg_0 = _GEN_3[7:0]; // @[AxiMMRegs.scala 40:43:@27.4]
  assign byteVecIn_0 = io_axiLite_wdata[7:0]; // @[AxiMMRegs.scala 40:43:@29.4]
  assign _T_175 = io_axiLite_wstrb[0]; // @[AxiMMRegs.scala 47:42:@31.4]
  assign byteVecOut_0 = _T_175 ? byteVecIn_0 : byteVecReg_0; // @[AxiMMRegs.scala 47:25:@32.4]
  assign byteVecReg_1 = _GEN_3[15:8]; // @[AxiMMRegs.scala 40:43:@35.4]
  assign byteVecIn_1 = io_axiLite_wdata[15:8]; // @[AxiMMRegs.scala 40:43:@37.4]
  assign _T_183 = io_axiLite_wstrb[1]; // @[AxiMMRegs.scala 47:42:@39.4]
  assign byteVecOut_1 = _T_183 ? byteVecIn_1 : byteVecReg_1; // @[AxiMMRegs.scala 47:25:@40.4]
  assign byteVecReg_2 = _GEN_3[23:16]; // @[AxiMMRegs.scala 40:43:@43.4]
  assign byteVecIn_2 = io_axiLite_wdata[23:16]; // @[AxiMMRegs.scala 40:43:@45.4]
  assign _T_191 = io_axiLite_wstrb[2]; // @[AxiMMRegs.scala 47:42:@47.4]
  assign byteVecOut_2 = _T_191 ? byteVecIn_2 : byteVecReg_2; // @[AxiMMRegs.scala 47:25:@48.4]
  assign byteVecReg_3 = _GEN_3[31:24]; // @[AxiMMRegs.scala 40:43:@51.4]
  assign byteVecIn_3 = io_axiLite_wdata[31:24]; // @[AxiMMRegs.scala 40:43:@53.4]
  assign _T_199 = io_axiLite_wstrb[3]; // @[AxiMMRegs.scala 47:42:@55.4]
  assign byteVecOut_3 = _T_199 ? byteVecIn_3 : byteVecReg_3; // @[AxiMMRegs.scala 47:25:@56.4]
  assign _T_201 = io_axiLite_awvalid & io_axiLite_wvalid; // @[AxiMMRegs.scala 50:27:@58.4]
  assign _T_208 = {byteVecOut_3,byteVecOut_2,byteVecOut_1,byteVecOut_0}; // @[AxiMMRegs.scala 51:42:@63.6]
  assign _GEN_16 = 2'h0 == _T_172 ? _T_208 : regFile_0; // @[AxiMMRegs.scala 51:28:@64.6]
  assign _GEN_17 = 2'h1 == _T_172 ? _T_208 : regFile_1; // @[AxiMMRegs.scala 51:28:@64.6]
  assign _GEN_18 = 2'h2 == _T_172 ? _T_208 : regFile_2; // @[AxiMMRegs.scala 51:28:@64.6]
  assign _GEN_19 = 2'h3 == _T_172 ? _T_208 : regFile_3; // @[AxiMMRegs.scala 51:28:@64.6]
  assign _GEN_20 = _T_201 ? _GEN_16 : regFile_0; // @[AxiMMRegs.scala 50:49:@59.4]
  assign _GEN_21 = _T_201 ? _GEN_17 : regFile_1; // @[AxiMMRegs.scala 50:49:@59.4]
  assign _GEN_22 = _T_201 ? _GEN_18 : regFile_2; // @[AxiMMRegs.scala 50:49:@59.4]
  assign _GEN_23 = _T_201 ? _GEN_19 : regFile_3; // @[AxiMMRegs.scala 50:49:@59.4]
  assign _T_210 = io_axiLite_awvalid == 1'h0; // @[AxiMMRegs.scala 53:57:@66.4]
  assign _T_211 = io_axiLite_wvalid & _T_210; // @[AxiMMRegs.scala 53:54:@67.4]
  assign _T_220 = axiLite_awaddr == 4'h0; // @[AxiMMRegs.scala 57:77:@77.4]
  assign _T_221 = _T_201 & _T_220; // @[AxiMMRegs.scala 57:59:@78.4]
  assign _T_228 = axiLite_awaddr == 4'h1; // @[AxiMMRegs.scala 57:77:@84.4]
  assign _T_229 = _T_201 & _T_228; // @[AxiMMRegs.scala 57:59:@85.4]
  assign _T_246 = _T_242[1:0]; // @[:@104.4]
  assign _GEN_26 = 2'h1 == _T_246 ? io_regsInput_1 : regsOutput_0; // @[AxiMMRegs.scala 76:20:@105.4]
  assign _GEN_27 = 2'h2 == _T_246 ? regFile_2 : _GEN_26; // @[AxiMMRegs.scala 76:20:@105.4]
  assign io_axiLite_bresp = _T_229 ? 2'h2 : _T_214; // @[AxiMMRegs.scala 53:20:@71.4 AxiMMRegs.scala 65:40:@90.6]
  assign io_axiLite_bvalid = _T_217; // @[AxiMMRegs.scala 54:21:@75.4]
  assign io_axiLite_rdata = 2'h3 == _T_246 ? regFile_3 : _GEN_27; // @[AxiMMRegs.scala 76:20:@105.4]
  assign io_axiLite_rvalid = _T_249; // @[AxiMMRegs.scala 77:21:@109.4]
  assign io_regsOutput_0 = regsOutput_0; // @[AxiMMRegs.scala 80:17:@113.4]
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
  _T_214 = _RAND_4[1:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  _T_217 = _RAND_5[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  regsOutput_0 = _RAND_6[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  _T_242 = _RAND_7[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  _T_249 = _RAND_8[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (reset) begin
      regFile_0 <= 32'h0;
    end else begin
      if (_T_201) begin
        if (2'h0 == _T_172) begin
          regFile_0 <= _T_208;
        end
      end
    end
    if (reset) begin
      regFile_1 <= 32'h0;
    end else begin
      if (_T_201) begin
        if (2'h1 == _T_172) begin
          regFile_1 <= _T_208;
        end
      end
    end
    if (reset) begin
      regFile_2 <= 32'h0;
    end else begin
      if (_T_201) begin
        if (2'h2 == _T_172) begin
          regFile_2 <= _T_208;
        end
      end
    end
    if (reset) begin
      regFile_3 <= 32'h0;
    end else begin
      if (_T_201) begin
        if (2'h3 == _T_172) begin
          regFile_3 <= _T_208;
        end
      end
    end
    if (_T_211) begin
      _T_214 <= 2'h2;
    end else begin
      _T_214 <= 2'h0;
    end
    _T_217 <= io_axiLite_awvalid & io_axiLite_wvalid;
    if (_T_221) begin
      regsOutput_0 <= io_axiLite_wdata;
    end else begin
      regsOutput_0 <= 32'h0;
    end
    _T_242 <= io_axiLite_araddr >> 2'h2;
    _T_249 <= io_axiLite_arvalid & io_axiLite_rready;
  end
endmodule
