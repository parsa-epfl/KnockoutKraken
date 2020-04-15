module ProcAxiWrapStatic(
  input         clock,
  input         reset,
  input  [5:0]  io_axiLite_awaddr,
  input  [2:0]  io_axiLite_awprot,
  input         io_axiLite_awvalid,
  output        io_axiLite_awready,
  input  [31:0] io_axiLite_wdata,
  input  [3:0]  io_axiLite_wstrb,
  input         io_axiLite_wvalid,
  output        io_axiLite_wready,
  output [1:0]  io_axiLite_bresp,
  output        io_axiLite_bvalid,
  input         io_axiLite_bready,
  input  [5:0]  io_axiLite_araddr,
  input  [2:0]  io_axiLite_arprot,
  input         io_axiLite_arvalid,
  output        io_axiLite_arready,
  output [31:0] io_axiLite_rdata,
  output [1:0]  io_axiLite_rresp,
  output        io_axiLite_rvalid,
  input         io_axiLite_rready,
  (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 memoryBRAM CLK" *)
  input         io_memoryBRAM_CLK,
  (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 memoryBRAM RST" *)
  input         io_memoryBRAM_RST,
  (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 memoryBRAM EN" *)
  input         io_memoryBRAM_EN,
  (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 memoryBRAM WE" *)
  input  [7:0]  io_memoryBRAM_WE,
  (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 memoryBRAM ADDR" *)
  input  [17:0] io_memoryBRAM_ADDR,
  (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 memoryBRAM DIN" *)
  input  [63:0] io_memoryBRAM_DI,
  (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 memoryBRAM DOUT" *)
  output [63:0] io_memoryBRAM_DO,
  (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 stateBRAM CLK" *)
  input         io_stateBRAM_CLK,
  (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 stateBRAM RST" *)
  input         io_stateBRAM_RST,
  (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 stateBRAM EN" *)
  input         io_stateBRAM_EN,
  (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 stateBRAM WE" *)
  input  [7:0]  io_stateBRAM_WE,
  (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 stateBRAM ADDR" *)
  input  [12:0] io_stateBRAM_ADDR,
  (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 stateBRAM DIN" *)
  input  [63:0] io_stateBRAM_DI,
  (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 stateBRAM DOUT" *)
  output [63:0] io_stateBRAM_DO
);
  wire  proc_clock; // @[ProcAxi.scala 29:20]
  wire  proc_reset; // @[ProcAxi.scala 29:20]
  wire [5:0] proc_io_axiLite_awaddr; // @[ProcAxi.scala 29:20]
  wire  proc_io_axiLite_awvalid; // @[ProcAxi.scala 29:20]
  wire [31:0] proc_io_axiLite_wdata; // @[ProcAxi.scala 29:20]
  wire [3:0] proc_io_axiLite_wstrb; // @[ProcAxi.scala 29:20]
  wire  proc_io_axiLite_wvalid; // @[ProcAxi.scala 29:20]
  wire [1:0] proc_io_axiLite_bresp; // @[ProcAxi.scala 29:20]
  wire  proc_io_axiLite_bvalid; // @[ProcAxi.scala 29:20]
  wire [5:0] proc_io_axiLite_araddr; // @[ProcAxi.scala 29:20]
  wire  proc_io_axiLite_arvalid; // @[ProcAxi.scala 29:20]
  wire [31:0] proc_io_axiLite_rdata; // @[ProcAxi.scala 29:20]
  wire  proc_io_axiLite_rvalid; // @[ProcAxi.scala 29:20]
  wire  proc_io_axiLite_rready; // @[ProcAxi.scala 29:20]
  wire  proc_io_memoryBRAM_EN; // @[ProcAxi.scala 29:20]
  wire [7:0] proc_io_memoryBRAM_WE; // @[ProcAxi.scala 29:20]
  wire [17:0] proc_io_memoryBRAM_ADDR; // @[ProcAxi.scala 29:20]
  wire [63:0] proc_io_memoryBRAM_DI; // @[ProcAxi.scala 29:20]
  wire [63:0] proc_io_memoryBRAM_DO; // @[ProcAxi.scala 29:20]
  wire  proc_io_stateBRAM_EN; // @[ProcAxi.scala 29:20]
  wire [7:0] proc_io_stateBRAM_WE; // @[ProcAxi.scala 29:20]
  wire [12:0] proc_io_stateBRAM_ADDR; // @[ProcAxi.scala 29:20]
  wire [63:0] proc_io_stateBRAM_DI; // @[ProcAxi.scala 29:20]
  wire [63:0] proc_io_stateBRAM_DO; // @[ProcAxi.scala 29:20]
  ProcAxiWrap proc ( // @[ProcAxi.scala 29:20]
    .clock(proc_clock),
    .reset(proc_reset),
    .io_axiLite_awaddr(proc_io_axiLite_awaddr),
    .io_axiLite_awvalid(proc_io_axiLite_awvalid),
    .io_axiLite_wdata(proc_io_axiLite_wdata),
    .io_axiLite_wstrb(proc_io_axiLite_wstrb),
    .io_axiLite_wvalid(proc_io_axiLite_wvalid),
    .io_axiLite_bresp(proc_io_axiLite_bresp),
    .io_axiLite_bvalid(proc_io_axiLite_bvalid),
    .io_axiLite_araddr(proc_io_axiLite_araddr),
    .io_axiLite_arvalid(proc_io_axiLite_arvalid),
    .io_axiLite_rdata(proc_io_axiLite_rdata),
    .io_axiLite_rvalid(proc_io_axiLite_rvalid),
    .io_axiLite_rready(proc_io_axiLite_rready),
    .io_memoryBRAM_EN(proc_io_memoryBRAM_EN),
    .io_memoryBRAM_WE(proc_io_memoryBRAM_WE),
    .io_memoryBRAM_ADDR(proc_io_memoryBRAM_ADDR),
    .io_memoryBRAM_DI(proc_io_memoryBRAM_DI),
    .io_memoryBRAM_DO(proc_io_memoryBRAM_DO),
    .io_stateBRAM_EN(proc_io_stateBRAM_EN),
    .io_stateBRAM_WE(proc_io_stateBRAM_WE),
    .io_stateBRAM_ADDR(proc_io_stateBRAM_ADDR),
    .io_stateBRAM_DI(proc_io_stateBRAM_DI),
    .io_stateBRAM_DO(proc_io_stateBRAM_DO)
  );
  assign io_axiLite_awready = 1'h1; // @[ProcAxi.scala 30:6]
  assign io_axiLite_wready = 1'h1; // @[ProcAxi.scala 30:6]
  assign io_axiLite_bresp = proc_io_axiLite_bresp; // @[ProcAxi.scala 30:6]
  assign io_axiLite_bvalid = proc_io_axiLite_bvalid; // @[ProcAxi.scala 30:6]
  assign io_axiLite_arready = 1'h1; // @[ProcAxi.scala 30:6]
  assign io_axiLite_rdata = proc_io_axiLite_rdata; // @[ProcAxi.scala 30:6]
  assign io_axiLite_rresp = 2'h0; // @[ProcAxi.scala 30:6]
  assign io_axiLite_rvalid = proc_io_axiLite_rvalid; // @[ProcAxi.scala 30:6]
  assign io_memoryBRAM_DO = proc_io_memoryBRAM_DO; // @[ProcAxi.scala 30:6]
  assign io_stateBRAM_DO = proc_io_stateBRAM_DO; // @[ProcAxi.scala 30:6]
  assign proc_clock = clock;
  assign proc_reset = reset;
  assign proc_io_axiLite_awaddr = io_axiLite_awaddr; // @[ProcAxi.scala 30:6]
  assign proc_io_axiLite_awvalid = io_axiLite_awvalid; // @[ProcAxi.scala 30:6]
  assign proc_io_axiLite_wdata = io_axiLite_wdata; // @[ProcAxi.scala 30:6]
  assign proc_io_axiLite_wstrb = io_axiLite_wstrb; // @[ProcAxi.scala 30:6]
  assign proc_io_axiLite_wvalid = io_axiLite_wvalid; // @[ProcAxi.scala 30:6]
  assign proc_io_axiLite_araddr = io_axiLite_araddr; // @[ProcAxi.scala 30:6]
  assign proc_io_axiLite_arvalid = io_axiLite_arvalid; // @[ProcAxi.scala 30:6]
  assign proc_io_axiLite_rready = io_axiLite_rready; // @[ProcAxi.scala 30:6]
  assign proc_io_memoryBRAM_EN = io_memoryBRAM_EN; // @[ProcAxi.scala 30:6]
  assign proc_io_memoryBRAM_WE = io_memoryBRAM_WE; // @[ProcAxi.scala 30:6]
  assign proc_io_memoryBRAM_ADDR = io_memoryBRAM_ADDR; // @[ProcAxi.scala 30:6]
  assign proc_io_memoryBRAM_DI = io_memoryBRAM_DI; // @[ProcAxi.scala 30:6]
  assign proc_io_stateBRAM_EN = io_stateBRAM_EN; // @[ProcAxi.scala 30:6]
  assign proc_io_stateBRAM_WE = io_stateBRAM_WE; // @[ProcAxi.scala 30:6]
  assign proc_io_stateBRAM_ADDR = io_stateBRAM_ADDR; // @[ProcAxi.scala 30:6]
  assign proc_io_stateBRAM_DI = io_stateBRAM_DI; // @[ProcAxi.scala 30:6]
endmodule
