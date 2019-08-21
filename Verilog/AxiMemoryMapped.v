module AxiMemoryMapped( // @[:@3.2]
  input         clock, // @[:@4.4]
  input         reset, // @[:@5.4]
  input  [3:0]  io_axiLite_awaddr, // @[:@6.4]
  input         io_axiLite_awvalid, // @[:@6.4]
  input  [31:0] io_axiLite_wdata, // @[:@6.4]
  input  [3:0]  io_axiLite_wstrb, // @[:@6.4]
  input         io_axiLite_wvalid, // @[:@6.4]
  output        io_axiLite_wready, // @[:@6.4]
  output [1:0]  io_axiLite_bresp, // @[:@6.4]
  output        io_axiLite_bvalid, // @[:@6.4]
  input         io_axiLite_bready, // @[:@6.4]
  input  [3:0]  io_axiLite_araddr, // @[:@6.4]
  input         io_axiLite_arvalid, // @[:@6.4]
  output [31:0] io_axiLite_rdata, // @[:@6.4]
  output        io_axiLite_rvalid, // @[:@6.4]
  input         io_axiLite_rready, // @[:@6.4]
  output        io_regPort_writeValid, // @[:@6.4]
  output [1:0]  io_regPort_writeAddr, // @[:@6.4]
  output [31:0] io_regPort_writeData, // @[:@6.4]
  output [3:0]  io_regPort_writeStrobe, // @[:@6.4]
  input         io_regPort_writeError, // @[:@6.4]
  output [1:0]  io_regPort_readAddr, // @[:@6.4]
  input  [31:0] io_regPort_readData // @[:@6.4]
);
  reg [3:0] writeAddr; // @[Reg.scala 19:20:@59.4]
  reg [31:0] _RAND_0;
  wire [3:0] _GEN_0; // @[Reg.scala 20:19:@60.4]
  reg  validWriteAddress; // @[Reg.scala 19:20:@66.4]
  reg [31:0] _RAND_1;
  wire  _T_145; // @[axiMemoryMapped.scala 53:31:@70.4]
  reg  validWriteResp; // @[axiMemoryMapped.scala 72:31:@105.4]
  reg [31:0] _RAND_2;
  wire  _T_146; // @[Decoupled.scala 37:37:@71.4]
  wire  validWriteAddressEnable; // @[axiMemoryMapped.scala 53:51:@72.4]
  wire  _GEN_1; // @[Reg.scala 20:19:@67.4]
  wire  _T_149; // @[Decoupled.scala 37:37:@75.4]
  reg  validWrite; // @[Reg.scala 19:20:@76.4]
  reg [31:0] _RAND_3;
  wire  _T_152; // @[axiMemoryMapped.scala 56:24:@80.4]
  wire  validWriteEnable; // @[axiMemoryMapped.scala 56:37:@82.4]
  wire  _GEN_2; // @[Reg.scala 20:19:@77.4]
  reg  writeValid; // @[axiMemoryMapped.scala 60:28:@85.4]
  reg [31:0] _RAND_4;
  reg [31:0] writeData; // @[Reg.scala 19:20:@88.4]
  reg [31:0] _RAND_5;
  wire [31:0] _GEN_3; // @[Reg.scala 20:19:@89.4]
  reg [3:0] writeStrobe; // @[Reg.scala 19:20:@93.4]
  reg [31:0] _RAND_6;
  wire [3:0] _GEN_4; // @[Reg.scala 20:19:@94.4]
  reg [3:0] readAddr; // @[Reg.scala 19:20:@115.4]
  reg [31:0] _RAND_7;
  wire [3:0] _GEN_6; // @[Reg.scala 20:19:@116.4]
  reg  readValid; // @[axiMemoryMapped.scala 91:26:@123.4]
  reg [31:0] _RAND_8;
  wire  _T_175; // @[axiMemoryMapped.scala 92:20:@125.4]
  wire  _T_176; // @[axiMemoryMapped.scala 92:18:@126.4]
  assign _GEN_0 = io_axiLite_awvalid ? io_axiLite_awaddr : writeAddr; // @[Reg.scala 20:19:@60.4]
  assign _T_145 = ~ validWriteAddress; // @[axiMemoryMapped.scala 53:31:@70.4]
  assign _T_146 = io_axiLite_bready & validWriteResp; // @[Decoupled.scala 37:37:@71.4]
  assign validWriteAddressEnable = _T_145 | _T_146; // @[axiMemoryMapped.scala 53:51:@72.4]
  assign _GEN_1 = validWriteAddressEnable ? io_axiLite_awvalid : validWriteAddress; // @[Reg.scala 20:19:@67.4]
  assign _T_149 = validWriteAddress & io_axiLite_wvalid; // @[Decoupled.scala 37:37:@75.4]
  assign _T_152 = ~ validWrite; // @[axiMemoryMapped.scala 56:24:@80.4]
  assign validWriteEnable = _T_152 | _T_146; // @[axiMemoryMapped.scala 56:37:@82.4]
  assign _GEN_2 = validWriteEnable ? _T_149 : validWrite; // @[Reg.scala 20:19:@77.4]
  assign _GEN_3 = _T_149 ? io_axiLite_wdata : writeData; // @[Reg.scala 20:19:@89.4]
  assign _GEN_4 = _T_149 ? io_axiLite_wstrb : writeStrobe; // @[Reg.scala 20:19:@94.4]
  assign _GEN_6 = io_axiLite_arvalid ? io_axiLite_araddr : readAddr; // @[Reg.scala 20:19:@116.4]
  assign _T_175 = ~ io_axiLite_rready; // @[axiMemoryMapped.scala 92:20:@125.4]
  assign _T_176 = readValid & _T_175; // @[axiMemoryMapped.scala 92:18:@126.4]
  assign io_axiLite_wready = validWriteAddress; // @[axiLite.scala 108:17:@23.4]
  assign io_axiLite_bresp = io_regPort_writeError ? 2'h2 : 2'h0; // @[axiLite.scala 110:17:@31.4]
  assign io_axiLite_bvalid = validWriteResp; // @[axiLite.scala 111:17:@32.4]
  assign io_axiLite_rdata = io_regPort_readData; // @[axiLite.scala 118:17:@51.4]
  assign io_axiLite_rvalid = readValid; // @[axiLite.scala 119:17:@52.4]
  assign io_regPort_writeValid = writeValid & validWriteAddress; // @[axiMemoryMapped.scala 66:26:@99.4]
  assign io_regPort_writeAddr = writeAddr[3:2]; // @[axiMemoryMapped.scala 67:26:@101.4]
  assign io_regPort_writeData = writeData; // @[axiMemoryMapped.scala 68:26:@102.4]
  assign io_regPort_writeStrobe = writeStrobe; // @[axiMemoryMapped.scala 69:26:@103.4]
  assign io_regPort_readAddr = readAddr[3:2]; // @[axiMemoryMapped.scala 84:24:@120.4]
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
  writeAddr = _RAND_0[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  validWriteAddress = _RAND_1[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  validWriteResp = _RAND_2[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  validWrite = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  writeValid = _RAND_4[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  writeData = _RAND_5[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  writeStrobe = _RAND_6[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  readAddr = _RAND_7[3:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  readValid = _RAND_8[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (reset) begin
      writeAddr <= 4'h0;
    end else begin
      if (io_axiLite_awvalid) begin
        writeAddr <= io_axiLite_awaddr;
      end
    end
    if (reset) begin
      validWriteAddress <= 1'h0;
    end else begin
      if (validWriteAddressEnable) begin
        validWriteAddress <= io_axiLite_awvalid;
      end
    end
    validWriteResp <= validWriteAddress & validWrite;
    if (reset) begin
      validWrite <= 1'h0;
    end else begin
      if (validWriteEnable) begin
        validWrite <= _T_149;
      end
    end
    writeValid <= validWriteAddress & io_axiLite_wvalid;
    if (reset) begin
      writeData <= 32'h0;
    end else begin
      if (_T_149) begin
        writeData <= io_axiLite_wdata;
      end
    end
    if (reset) begin
      writeStrobe <= 4'h0;
    end else begin
      if (_T_149) begin
        writeStrobe <= io_axiLite_wstrb;
      end
    end
    if (reset) begin
      readAddr <= 4'h0;
    end else begin
      if (io_axiLite_arvalid) begin
        readAddr <= io_axiLite_araddr;
      end
    end
    if (!(_T_176)) begin
      readValid <= io_axiLite_arvalid;
    end
  end
endmodule
