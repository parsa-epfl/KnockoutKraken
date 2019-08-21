module AxiMemoryMappedWithRegs( // @[:@233.2]
  input         clock, // @[:@234.4]
  input         reset, // @[:@235.4]
  input  [3:0]  io_axiLite_awaddr, // @[:@236.4]
  input         io_axiLite_awvalid, // @[:@236.4]
  input  [31:0] io_axiLite_wdata, // @[:@236.4]
  input  [3:0]  io_axiLite_wstrb, // @[:@236.4]
  input         io_axiLite_wvalid, // @[:@236.4]
  output        io_axiLite_wready, // @[:@236.4]
  output [1:0]  io_axiLite_bresp, // @[:@236.4]
  output        io_axiLite_bvalid, // @[:@236.4]
  input         io_axiLite_bready, // @[:@236.4]
  input  [3:0]  io_axiLite_araddr, // @[:@236.4]
  input         io_axiLite_arvalid, // @[:@236.4]
  output [31:0] io_axiLite_rdata, // @[:@236.4]
  output        io_axiLite_rvalid, // @[:@236.4]
  input         io_axiLite_rready, // @[:@236.4]
  input  [31:0] io_moduleInputs_1, // @[:@236.4]
  output [31:0] io_regsValues_0 // @[:@236.4]
);
  wire  axiMemoryMapped_clock; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire  axiMemoryMapped_reset; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire [3:0] axiMemoryMapped_io_axiLite_awaddr; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire  axiMemoryMapped_io_axiLite_awvalid; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire [31:0] axiMemoryMapped_io_axiLite_wdata; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire [3:0] axiMemoryMapped_io_axiLite_wstrb; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire  axiMemoryMapped_io_axiLite_wvalid; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire  axiMemoryMapped_io_axiLite_wready; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire [1:0] axiMemoryMapped_io_axiLite_bresp; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire  axiMemoryMapped_io_axiLite_bvalid; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire  axiMemoryMapped_io_axiLite_bready; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire [3:0] axiMemoryMapped_io_axiLite_araddr; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire  axiMemoryMapped_io_axiLite_arvalid; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire [31:0] axiMemoryMapped_io_axiLite_rdata; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire  axiMemoryMapped_io_axiLite_rvalid; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire  axiMemoryMapped_io_axiLite_rready; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire  axiMemoryMapped_io_regPort_writeValid; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire [1:0] axiMemoryMapped_io_regPort_writeAddr; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire [31:0] axiMemoryMapped_io_regPort_writeData; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire [3:0] axiMemoryMapped_io_regPort_writeStrobe; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire  axiMemoryMapped_io_regPort_writeError; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire [1:0] axiMemoryMapped_io_regPort_readAddr; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire [31:0] axiMemoryMapped_io_regPort_readData; // @[axiMemoryMapped.scala 122:33:@238.4]
  wire  registerMap_clock; // @[axiMemoryMapped.scala 123:33:@241.4]
  wire  registerMap_io_port_writeValid; // @[axiMemoryMapped.scala 123:33:@241.4]
  wire [1:0] registerMap_io_port_writeAddr; // @[axiMemoryMapped.scala 123:33:@241.4]
  wire [31:0] registerMap_io_port_writeData; // @[axiMemoryMapped.scala 123:33:@241.4]
  wire [3:0] registerMap_io_port_writeStrobe; // @[axiMemoryMapped.scala 123:33:@241.4]
  wire  registerMap_io_port_writeError; // @[axiMemoryMapped.scala 123:33:@241.4]
  wire [1:0] registerMap_io_port_readAddr; // @[axiMemoryMapped.scala 123:33:@241.4]
  wire [31:0] registerMap_io_port_readData; // @[axiMemoryMapped.scala 123:33:@241.4]
  wire [31:0] registerMap_io_moduleInputs_1; // @[axiMemoryMapped.scala 123:33:@241.4]
  wire [31:0] registerMap_io_regsValues_0; // @[axiMemoryMapped.scala 123:33:@241.4]
  AxiMemoryMapped axiMemoryMapped ( // @[axiMemoryMapped.scala 122:33:@238.4]
    .clock(axiMemoryMapped_clock),
    .reset(axiMemoryMapped_reset),
    .io_axiLite_awaddr(axiMemoryMapped_io_axiLite_awaddr),
    .io_axiLite_awvalid(axiMemoryMapped_io_axiLite_awvalid),
    .io_axiLite_wdata(axiMemoryMapped_io_axiLite_wdata),
    .io_axiLite_wstrb(axiMemoryMapped_io_axiLite_wstrb),
    .io_axiLite_wvalid(axiMemoryMapped_io_axiLite_wvalid),
    .io_axiLite_wready(axiMemoryMapped_io_axiLite_wready),
    .io_axiLite_bresp(axiMemoryMapped_io_axiLite_bresp),
    .io_axiLite_bvalid(axiMemoryMapped_io_axiLite_bvalid),
    .io_axiLite_bready(axiMemoryMapped_io_axiLite_bready),
    .io_axiLite_araddr(axiMemoryMapped_io_axiLite_araddr),
    .io_axiLite_arvalid(axiMemoryMapped_io_axiLite_arvalid),
    .io_axiLite_rdata(axiMemoryMapped_io_axiLite_rdata),
    .io_axiLite_rvalid(axiMemoryMapped_io_axiLite_rvalid),
    .io_axiLite_rready(axiMemoryMapped_io_axiLite_rready),
    .io_regPort_writeValid(axiMemoryMapped_io_regPort_writeValid),
    .io_regPort_writeAddr(axiMemoryMapped_io_regPort_writeAddr),
    .io_regPort_writeData(axiMemoryMapped_io_regPort_writeData),
    .io_regPort_writeStrobe(axiMemoryMapped_io_regPort_writeStrobe),
    .io_regPort_writeError(axiMemoryMapped_io_regPort_writeError),
    .io_regPort_readAddr(axiMemoryMapped_io_regPort_readAddr),
    .io_regPort_readData(axiMemoryMapped_io_regPort_readData)
  );
  RegisterMap registerMap ( // @[axiMemoryMapped.scala 123:33:@241.4]
    .clock(registerMap_clock),
    .io_port_writeValid(registerMap_io_port_writeValid),
    .io_port_writeAddr(registerMap_io_port_writeAddr),
    .io_port_writeData(registerMap_io_port_writeData),
    .io_port_writeStrobe(registerMap_io_port_writeStrobe),
    .io_port_writeError(registerMap_io_port_writeError),
    .io_port_readAddr(registerMap_io_port_readAddr),
    .io_port_readData(registerMap_io_port_readData),
    .io_moduleInputs_1(registerMap_io_moduleInputs_1),
    .io_regsValues_0(registerMap_io_regsValues_0)
  );
  assign io_axiLite_wready = axiMemoryMapped_io_axiLite_wready; // @[axiMemoryMapped.scala 125:25:@255.4]
  assign io_axiLite_bresp = axiMemoryMapped_io_axiLite_bresp; // @[axiMemoryMapped.scala 125:25:@254.4]
  assign io_axiLite_bvalid = axiMemoryMapped_io_axiLite_bvalid; // @[axiMemoryMapped.scala 125:25:@253.4]
  assign io_axiLite_rdata = axiMemoryMapped_io_axiLite_rdata; // @[axiMemoryMapped.scala 125:25:@247.4]
  assign io_axiLite_rvalid = axiMemoryMapped_io_axiLite_rvalid; // @[axiMemoryMapped.scala 125:25:@245.4]
  assign io_regsValues_0 = registerMap_io_regsValues_0; // @[axiMemoryMapped.scala 128:25:@275.4]
  assign axiMemoryMapped_clock = clock; // @[:@239.4]
  assign axiMemoryMapped_reset = reset; // @[:@240.4]
  assign axiMemoryMapped_io_axiLite_awaddr = io_axiLite_awaddr; // @[axiMemoryMapped.scala 125:25:@262.4]
  assign axiMemoryMapped_io_axiLite_awvalid = io_axiLite_awvalid; // @[axiMemoryMapped.scala 125:25:@260.4]
  assign axiMemoryMapped_io_axiLite_wdata = io_axiLite_wdata; // @[axiMemoryMapped.scala 125:25:@258.4]
  assign axiMemoryMapped_io_axiLite_wstrb = io_axiLite_wstrb; // @[axiMemoryMapped.scala 125:25:@257.4]
  assign axiMemoryMapped_io_axiLite_wvalid = io_axiLite_wvalid; // @[axiMemoryMapped.scala 125:25:@256.4]
  assign axiMemoryMapped_io_axiLite_bready = io_axiLite_bready; // @[axiMemoryMapped.scala 125:25:@252.4]
  assign axiMemoryMapped_io_axiLite_araddr = io_axiLite_araddr; // @[axiMemoryMapped.scala 125:25:@251.4]
  assign axiMemoryMapped_io_axiLite_arvalid = io_axiLite_arvalid; // @[axiMemoryMapped.scala 125:25:@249.4]
  assign axiMemoryMapped_io_axiLite_rready = io_axiLite_rready; // @[axiMemoryMapped.scala 125:25:@244.4]
  assign axiMemoryMapped_io_regPort_writeError = registerMap_io_port_writeError; // @[axiMemoryMapped.scala 126:25:@266.4]
  assign axiMemoryMapped_io_regPort_readData = registerMap_io_port_readData; // @[axiMemoryMapped.scala 126:25:@264.4]
  assign registerMap_clock = clock; // @[:@242.4]
  assign registerMap_io_port_writeValid = axiMemoryMapped_io_regPort_writeValid; // @[axiMemoryMapped.scala 126:25:@270.4]
  assign registerMap_io_port_writeAddr = axiMemoryMapped_io_regPort_writeAddr; // @[axiMemoryMapped.scala 126:25:@269.4]
  assign registerMap_io_port_writeData = axiMemoryMapped_io_regPort_writeData; // @[axiMemoryMapped.scala 126:25:@268.4]
  assign registerMap_io_port_writeStrobe = axiMemoryMapped_io_regPort_writeStrobe; // @[axiMemoryMapped.scala 126:25:@267.4]
  assign registerMap_io_port_readAddr = axiMemoryMapped_io_regPort_readAddr; // @[axiMemoryMapped.scala 126:25:@265.4]
  assign registerMap_io_moduleInputs_1 = io_moduleInputs_1; // @[axiMemoryMapped.scala 127:25:@272.4]
endmodule
