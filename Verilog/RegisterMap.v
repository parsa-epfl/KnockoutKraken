module RegisterMap( // @[:@143.2]
  input         clock, // @[:@144.4]
  input         io_port_writeValid, // @[:@146.4]
  input  [1:0]  io_port_writeAddr, // @[:@146.4]
  input  [31:0] io_port_writeData, // @[:@146.4]
  input  [3:0]  io_port_writeStrobe, // @[:@146.4]
  output        io_port_writeError, // @[:@146.4]
  input  [1:0]  io_port_readAddr, // @[:@146.4]
  output [31:0] io_port_readData, // @[:@146.4]
  input  [31:0] io_moduleInputs_1, // @[:@146.4]
  output [31:0] io_regsValues_0 // @[:@146.4]
);
  reg [31:0] regs_0; // @[registerMap.scala 42:25:@149.4]
  reg [31:0] _RAND_0;
  reg [31:0] regs_1; // @[registerMap.scala 42:25:@149.4]
  reg [31:0] _RAND_1;
  reg [31:0] regs_2; // @[registerMap.scala 42:25:@149.4]
  reg [31:0] _RAND_2;
  reg [31:0] regs_3; // @[registerMap.scala 42:25:@149.4]
  reg [31:0] _RAND_3;
  wire  _T_108; // @[registerMap.scala 48:63:@163.4]
  wire  writeValid; // @[registerMap.scala 48:39:@164.4]
  wire  _GEN_1; // @[registerMap.scala 53:57:@166.4]
  wire  _GEN_2; // @[registerMap.scala 53:57:@166.4]
  wire  writeRegReadOnly; // @[registerMap.scala 53:57:@166.4]
  wire  writeErrorNext; // @[registerMap.scala 54:37:@169.4]
  reg  writeErrorReg; // @[Reg.scala 11:16:@170.4]
  reg [31:0] _RAND_4;
  wire [31:0] _GEN_6; // @[registerMap.scala 61:21:@178.4]
  wire [31:0] _GEN_7; // @[registerMap.scala 61:21:@178.4]
  wire  _T_124; // @[registerMap.scala 67:47:@181.4]
  wire  _T_125; // @[registerMap.scala 67:34:@182.4]
  wire [3:0] _T_129; // @[OneHot.scala 52:12:@188.6]
  wire [3:0] writeAddressOH; // @[registerMap.scala 67:59:@183.4]
  wire  _T_140; // @[registerMap.scala 77:56:@193.4]
  wire [7:0] _T_141; // @[registerMap.scala 75:49:@194.4]
  wire [31:0] _GEN_11; // @[registerMap.scala 75:49:@195.4]
  wire [31:0] _GEN_12; // @[registerMap.scala 75:49:@195.4]
  wire [31:0] _GEN_13; // @[registerMap.scala 75:49:@195.4]
  wire [7:0] _T_145; // @[registerMap.scala 75:49:@195.4]
  wire [7:0] strobedWriteDataBytes_0; // @[registerMap.scala 77:36:@196.4]
  wire  _T_147; // @[registerMap.scala 77:56:@198.4]
  wire [7:0] _T_148; // @[registerMap.scala 75:49:@199.4]
  wire [7:0] _T_152; // @[registerMap.scala 75:49:@200.4]
  wire [7:0] strobedWriteDataBytes_1; // @[registerMap.scala 77:36:@201.4]
  wire  _T_154; // @[registerMap.scala 77:56:@203.4]
  wire [7:0] _T_155; // @[registerMap.scala 75:49:@204.4]
  wire [7:0] _T_159; // @[registerMap.scala 75:49:@205.4]
  wire [7:0] strobedWriteDataBytes_2; // @[registerMap.scala 77:36:@206.4]
  wire  _T_161; // @[registerMap.scala 77:56:@208.4]
  wire [7:0] _T_162; // @[registerMap.scala 75:49:@209.4]
  wire [7:0] _T_166; // @[registerMap.scala 75:49:@210.4]
  wire [7:0] strobedWriteDataBytes_3; // @[registerMap.scala 77:36:@211.4]
  wire [31:0] strobedWriteData; // @[registerMap.scala 81:48:@215.4]
  wire  _T_170; // @[registerMap.scala 92:26:@216.4]
  wire  _T_177; // @[registerMap.scala 92:26:@222.4]
  wire  _T_184; // @[registerMap.scala 92:26:@227.4]
  assign _T_108 = io_port_writeStrobe != 4'h0; // @[registerMap.scala 48:63:@163.4]
  assign writeValid = io_port_writeValid & _T_108; // @[registerMap.scala 48:39:@164.4]
  assign _GEN_1 = 2'h1 == io_port_writeAddr; // @[registerMap.scala 53:57:@166.4]
  assign _GEN_2 = 2'h2 == io_port_writeAddr ? 1'h0 : _GEN_1; // @[registerMap.scala 53:57:@166.4]
  assign writeRegReadOnly = 2'h3 == io_port_writeAddr ? 1'h0 : _GEN_2; // @[registerMap.scala 53:57:@166.4]
  assign writeErrorNext = writeValid & writeRegReadOnly; // @[registerMap.scala 54:37:@169.4]
  assign _GEN_6 = 2'h1 == io_port_readAddr ? regs_1 : regs_0; // @[registerMap.scala 61:21:@178.4]
  assign _GEN_7 = 2'h2 == io_port_readAddr ? regs_2 : _GEN_6; // @[registerMap.scala 61:21:@178.4]
  assign _T_124 = writeValid == 1'h0; // @[registerMap.scala 67:47:@181.4]
  assign _T_125 = writeErrorNext | _T_124; // @[registerMap.scala 67:34:@182.4]
  assign _T_129 = 4'h1 << io_port_writeAddr; // @[OneHot.scala 52:12:@188.6]
  assign writeAddressOH = _T_125 ? 4'h0 : _T_129; // @[registerMap.scala 67:59:@183.4]
  assign _T_140 = io_port_writeStrobe[0]; // @[registerMap.scala 77:56:@193.4]
  assign _T_141 = io_port_writeData[7:0]; // @[registerMap.scala 75:49:@194.4]
  assign _GEN_11 = 2'h1 == io_port_writeAddr ? regs_1 : regs_0; // @[registerMap.scala 75:49:@195.4]
  assign _GEN_12 = 2'h2 == io_port_writeAddr ? regs_2 : _GEN_11; // @[registerMap.scala 75:49:@195.4]
  assign _GEN_13 = 2'h3 == io_port_writeAddr ? regs_3 : _GEN_12; // @[registerMap.scala 75:49:@195.4]
  assign _T_145 = _GEN_13[7:0]; // @[registerMap.scala 75:49:@195.4]
  assign strobedWriteDataBytes_0 = _T_140 ? _T_141 : _T_145; // @[registerMap.scala 77:36:@196.4]
  assign _T_147 = io_port_writeStrobe[1]; // @[registerMap.scala 77:56:@198.4]
  assign _T_148 = io_port_writeData[15:8]; // @[registerMap.scala 75:49:@199.4]
  assign _T_152 = _GEN_13[15:8]; // @[registerMap.scala 75:49:@200.4]
  assign strobedWriteDataBytes_1 = _T_147 ? _T_148 : _T_152; // @[registerMap.scala 77:36:@201.4]
  assign _T_154 = io_port_writeStrobe[2]; // @[registerMap.scala 77:56:@203.4]
  assign _T_155 = io_port_writeData[23:16]; // @[registerMap.scala 75:49:@204.4]
  assign _T_159 = _GEN_13[23:16]; // @[registerMap.scala 75:49:@205.4]
  assign strobedWriteDataBytes_2 = _T_154 ? _T_155 : _T_159; // @[registerMap.scala 77:36:@206.4]
  assign _T_161 = io_port_writeStrobe[3]; // @[registerMap.scala 77:56:@208.4]
  assign _T_162 = io_port_writeData[31:24]; // @[registerMap.scala 75:49:@209.4]
  assign _T_166 = _GEN_13[31:24]; // @[registerMap.scala 75:49:@210.4]
  assign strobedWriteDataBytes_3 = _T_161 ? _T_162 : _T_166; // @[registerMap.scala 77:36:@211.4]
  assign strobedWriteData = {strobedWriteDataBytes_3,strobedWriteDataBytes_2,strobedWriteDataBytes_1,strobedWriteDataBytes_0}; // @[registerMap.scala 81:48:@215.4]
  assign _T_170 = writeAddressOH[0]; // @[registerMap.scala 92:26:@216.4]
  assign _T_177 = writeAddressOH[2]; // @[registerMap.scala 92:26:@222.4]
  assign _T_184 = writeAddressOH[3]; // @[registerMap.scala 92:26:@227.4]
  assign io_port_writeError = writeErrorReg; // @[registerMap.scala 56:22:@174.4]
  assign io_port_readData = 2'h3 == io_port_readAddr ? regs_3 : _GEN_7; // @[registerMap.scala 61:21:@178.4]
  assign io_regsValues_0 = regs_0; // @[registerMap.scala 44:17:@154.4]
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
  regs_0 = _RAND_0[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  regs_1 = _RAND_1[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  regs_2 = _RAND_2[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  regs_3 = _RAND_3[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  writeErrorReg = _RAND_4[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (_T_170) begin
      regs_0 <= strobedWriteData;
    end else begin
      regs_0 <= 32'h0;
    end
    regs_1 <= io_moduleInputs_1;
    if (_T_177) begin
      regs_2 <= strobedWriteData;
    end
    if (_T_184) begin
      regs_3 <= strobedWriteData;
    end
    if (io_port_writeValid) begin
      writeErrorReg <= writeErrorNext;
    end
  end
endmodule
