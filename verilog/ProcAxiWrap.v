module ProcAxiWrap(
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
  input         io_memoryBRAM_CLK,
  input         io_memoryBRAM_RST,
  input         io_memoryBRAM_EN,
  input  [7:0]  io_memoryBRAM_WE,
  input  [16:0] io_memoryBRAM_ADDR,
  input  [63:0] io_memoryBRAM_DI,
  output [63:0] io_memoryBRAM_DO,
  input         io_stateBRAM_CLK,
  input         io_stateBRAM_RST,
  input         io_stateBRAM_EN,
  input  [7:0]  io_stateBRAM_WE,
  input  [10:0] io_stateBRAM_ADDR,
  input  [63:0] io_stateBRAM_DI,
  output [63:0] io_stateBRAM_DO
);
  wire  regFile_clock; // @[ProcAxi.scala 34:24]
  wire  regFile_reset; // @[ProcAxi.scala 34:24]
  wire [5:0] regFile_io_axiLite_awaddr; // @[ProcAxi.scala 34:24]
  wire  regFile_io_axiLite_awvalid; // @[ProcAxi.scala 34:24]
  wire [31:0] regFile_io_axiLite_wdata; // @[ProcAxi.scala 34:24]
  wire [3:0] regFile_io_axiLite_wstrb; // @[ProcAxi.scala 34:24]
  wire  regFile_io_axiLite_wvalid; // @[ProcAxi.scala 34:24]
  wire [1:0] regFile_io_axiLite_bresp; // @[ProcAxi.scala 34:24]
  wire  regFile_io_axiLite_bvalid; // @[ProcAxi.scala 34:24]
  wire [5:0] regFile_io_axiLite_araddr; // @[ProcAxi.scala 34:24]
  wire  regFile_io_axiLite_arvalid; // @[ProcAxi.scala 34:24]
  wire [31:0] regFile_io_axiLite_rdata; // @[ProcAxi.scala 34:24]
  wire  regFile_io_axiLite_rvalid; // @[ProcAxi.scala 34:24]
  wire  regFile_io_axiLite_rready; // @[ProcAxi.scala 34:24]
  wire [31:0] regFile_io_regsInput_1; // @[ProcAxi.scala 34:24]
  wire [31:0] regFile_io_regsInput_3; // @[ProcAxi.scala 34:24]
  wire [31:0] regFile_io_regsInput_4; // @[ProcAxi.scala 34:24]
  wire [31:0] regFile_io_regsInput_5; // @[ProcAxi.scala 34:24]
  wire [31:0] regFile_io_regsOutput_0; // @[ProcAxi.scala 34:24]
  wire [31:0] regFile_io_regsOutput_8; // @[ProcAxi.scala 34:24]
  wire [31:0] regFile_io_regsOutput_9; // @[ProcAxi.scala 34:24]
  wire [31:0] regFile_io_regsOutput_10; // @[ProcAxi.scala 34:24]
  wire  proc_clock; // @[ProcAxi.scala 36:20]
  wire  proc_reset; // @[ProcAxi.scala 36:20]
  wire  proc_io_memoryBRAM_EN; // @[ProcAxi.scala 36:20]
  wire [7:0] proc_io_memoryBRAM_WE; // @[ProcAxi.scala 36:20]
  wire [14:0] proc_io_memoryBRAM_ADDR; // @[ProcAxi.scala 36:20]
  wire [63:0] proc_io_memoryBRAM_DI; // @[ProcAxi.scala 36:20]
  wire [63:0] proc_io_memoryBRAM_DO; // @[ProcAxi.scala 36:20]
  wire  proc_io_stateBRAM_EN; // @[ProcAxi.scala 36:20]
  wire [7:0] proc_io_stateBRAM_WE; // @[ProcAxi.scala 36:20]
  wire [8:0] proc_io_stateBRAM_ADDR; // @[ProcAxi.scala 36:20]
  wire [63:0] proc_io_stateBRAM_DI; // @[ProcAxi.scala 36:20]
  wire [63:0] proc_io_stateBRAM_DO; // @[ProcAxi.scala 36:20]
  wire  proc_io_host2tpu_fire_tag; // @[ProcAxi.scala 36:20]
  wire  proc_io_host2tpu_fire_valid; // @[ProcAxi.scala 36:20]
  wire  proc_io_host2tpu_done_tag; // @[ProcAxi.scala 36:20]
  wire  proc_io_host2tpu_done_valid; // @[ProcAxi.scala 36:20]
  wire [63:0] proc_io_host2tpu_missTLB_bits_vaddr; // @[ProcAxi.scala 36:20]
  wire [5:0] proc_io_host2tpu_missTLB_bits_tlbIdx; // @[ProcAxi.scala 36:20]
  wire  proc_io_host2tpu_missTLB_valid; // @[ProcAxi.scala 36:20]
  wire  proc_io_host2tpu_fillTLB_valid; // @[ProcAxi.scala 36:20]
  wire  proc_io_host2tpu_fillTLB_bits_tlbEntry_wrEn; // @[ProcAxi.scala 36:20]
  wire [51:0] proc_io_host2tpu_fillTLB_bits_tlbEntry_tag; // @[ProcAxi.scala 36:20]
  wire [63:0] proc_io_host2tpu_fillTLB_bits_vaddr; // @[ProcAxi.scala 36:20]
  wire [5:0] proc_io_host2tpu_fillTLB_bits_tlbIdx; // @[ProcAxi.scala 36:20]
  wire  proc_io_host2tpu_getState_tag; // @[ProcAxi.scala 36:20]
  wire  proc_io_host2tpu_getState_valid; // @[ProcAxi.scala 36:20]
  wire [16:0] _T; // @[BramModules.scala 206:34]
  wire [31:0] _T_13; // @[ProcAxi.scala 24:14]
  wire [31:0] _T_16; // @[ProcAxi.scala 24:14]
  wire [31:0] _T_29; // @[ProcAxi.scala 24:14]
  wire [31:0] _T_32; // @[ProcAxi.scala 24:14]
  wire [10:0] _T_33; // @[BramModules.scala 206:34]
  wire [1:0] tagReg; // @[ProcAxi.scala 90:72]
  wire  fireReg; // @[ProcAxi.scala 90:72]
  reg  doneVec_0; // @[ProcAxi.scala 126:24]
  reg [31:0] _RAND_0;
  reg  doneVec_1; // @[ProcAxi.scala 126:24]
  reg [31:0] _RAND_1;
  wire  tlbMiss;
  wire [17:0] _T_43; // @[Cat.scala 29:58]
  wire  _GEN_0; // @[ProcAxi.scala 131:21]
  wire  _GEN_1; // @[ProcAxi.scala 131:21]
  wire  _GEN_2; // @[ProcAxi.scala 130:17]
  wire  _GEN_3; // @[ProcAxi.scala 130:17]
  wire  _GEN_12; // @[ProcAxi.scala 134:40]
  wire  _GEN_4; // @[ProcAxi.scala 134:40]
  wire  _GEN_5; // @[ProcAxi.scala 134:40]
  reg [63:0] missTLB_vaddr; // @[ProcAxi.scala 158:30]
  reg [63:0] _RAND_2;
  reg [31:0] missTLB_tlbIdx; // @[ProcAxi.scala 176:31]
  reg [31:0] _RAND_3;
  wire [31:0] _T_47; // @[ProcAxi.scala 90:72]
  wire [31:0] _T_48; // @[ProcAxi.scala 90:72]
  wire [63:0] fillTLB_vaddr; // @[Cat.scala 29:58]
  wire [31:0] fillTLB_tlbIdx; // @[ProcAxi.scala 90:72]
  AxiMemoryMappedRegFile regFile ( // @[ProcAxi.scala 34:24]
    .clock(regFile_clock),
    .reset(regFile_reset),
    .io_axiLite_awaddr(regFile_io_axiLite_awaddr),
    .io_axiLite_awvalid(regFile_io_axiLite_awvalid),
    .io_axiLite_wdata(regFile_io_axiLite_wdata),
    .io_axiLite_wstrb(regFile_io_axiLite_wstrb),
    .io_axiLite_wvalid(regFile_io_axiLite_wvalid),
    .io_axiLite_bresp(regFile_io_axiLite_bresp),
    .io_axiLite_bvalid(regFile_io_axiLite_bvalid),
    .io_axiLite_araddr(regFile_io_axiLite_araddr),
    .io_axiLite_arvalid(regFile_io_axiLite_arvalid),
    .io_axiLite_rdata(regFile_io_axiLite_rdata),
    .io_axiLite_rvalid(regFile_io_axiLite_rvalid),
    .io_axiLite_rready(regFile_io_axiLite_rready),
    .io_regsInput_1(regFile_io_regsInput_1),
    .io_regsInput_3(regFile_io_regsInput_3),
    .io_regsInput_4(regFile_io_regsInput_4),
    .io_regsInput_5(regFile_io_regsInput_5),
    .io_regsOutput_0(regFile_io_regsOutput_0),
    .io_regsOutput_8(regFile_io_regsOutput_8),
    .io_regsOutput_9(regFile_io_regsOutput_9),
    .io_regsOutput_10(regFile_io_regsOutput_10)
  );
  Proc proc ( // @[ProcAxi.scala 36:20]
    .clock(proc_clock),
    .reset(proc_reset),
    .io_memoryBRAM_EN(proc_io_memoryBRAM_EN),
    .io_memoryBRAM_WE(proc_io_memoryBRAM_WE),
    .io_memoryBRAM_ADDR(proc_io_memoryBRAM_ADDR),
    .io_memoryBRAM_DI(proc_io_memoryBRAM_DI),
    .io_memoryBRAM_DO(proc_io_memoryBRAM_DO),
    .io_stateBRAM_EN(proc_io_stateBRAM_EN),
    .io_stateBRAM_WE(proc_io_stateBRAM_WE),
    .io_stateBRAM_ADDR(proc_io_stateBRAM_ADDR),
    .io_stateBRAM_DI(proc_io_stateBRAM_DI),
    .io_stateBRAM_DO(proc_io_stateBRAM_DO),
    .io_host2tpu_fire_tag(proc_io_host2tpu_fire_tag),
    .io_host2tpu_fire_valid(proc_io_host2tpu_fire_valid),
    .io_host2tpu_done_tag(proc_io_host2tpu_done_tag),
    .io_host2tpu_done_valid(proc_io_host2tpu_done_valid),
    .io_host2tpu_missTLB_bits_vaddr(proc_io_host2tpu_missTLB_bits_vaddr),
    .io_host2tpu_missTLB_bits_tlbIdx(proc_io_host2tpu_missTLB_bits_tlbIdx),
    .io_host2tpu_missTLB_valid(proc_io_host2tpu_missTLB_valid),
    .io_host2tpu_fillTLB_valid(proc_io_host2tpu_fillTLB_valid),
    .io_host2tpu_fillTLB_bits_tlbEntry_wrEn(proc_io_host2tpu_fillTLB_bits_tlbEntry_wrEn),
    .io_host2tpu_fillTLB_bits_tlbEntry_tag(proc_io_host2tpu_fillTLB_bits_tlbEntry_tag),
    .io_host2tpu_fillTLB_bits_vaddr(proc_io_host2tpu_fillTLB_bits_vaddr),
    .io_host2tpu_fillTLB_bits_tlbIdx(proc_io_host2tpu_fillTLB_bits_tlbIdx),
    .io_host2tpu_getState_tag(proc_io_host2tpu_getState_tag),
    .io_host2tpu_getState_valid(proc_io_host2tpu_getState_valid)
  );
  assign _T = {{2'd0}, io_memoryBRAM_ADDR[16:2]}; // @[BramModules.scala 206:34]
  assign _T_13 = {io_memoryBRAM_DI[39:32],io_memoryBRAM_DI[47:40],io_memoryBRAM_DI[55:48],io_memoryBRAM_DI[63:56]}; // @[ProcAxi.scala 24:14]
  assign _T_16 = {io_memoryBRAM_DI[7:0],io_memoryBRAM_DI[15:8],io_memoryBRAM_DI[23:16],io_memoryBRAM_DI[31:24]}; // @[ProcAxi.scala 24:14]
  assign _T_29 = {proc_io_memoryBRAM_DO[39:32],proc_io_memoryBRAM_DO[47:40],proc_io_memoryBRAM_DO[55:48],proc_io_memoryBRAM_DO[63:56]}; // @[ProcAxi.scala 24:14]
  assign _T_32 = {proc_io_memoryBRAM_DO[7:0],proc_io_memoryBRAM_DO[15:8],proc_io_memoryBRAM_DO[23:16],proc_io_memoryBRAM_DO[31:24]}; // @[ProcAxi.scala 24:14]
  assign _T_33 = {{2'd0}, io_stateBRAM_ADDR[10:2]}; // @[BramModules.scala 206:34]
  assign tagReg = regFile_io_regsOutput_0[1:0]; // @[ProcAxi.scala 90:72]
  assign fireReg = regFile_io_regsOutput_0[31]; // @[ProcAxi.scala 90:72]
  assign tlbMiss = proc_io_host2tpu_missTLB_valid;
  assign _T_43 = {1'h0,tlbMiss,14'h0,doneVec_1,doneVec_0}; // @[Cat.scala 29:58]
  assign _GEN_0 = ~tagReg[0] ? 1'h0 : doneVec_0; // @[ProcAxi.scala 131:21]
  assign _GEN_1 = tagReg[0] ? 1'h0 : doneVec_1; // @[ProcAxi.scala 131:21]
  assign _GEN_2 = fireReg ? _GEN_0 : doneVec_0; // @[ProcAxi.scala 130:17]
  assign _GEN_3 = fireReg ? _GEN_1 : doneVec_1; // @[ProcAxi.scala 130:17]
  assign _GEN_12 = ~proc_io_host2tpu_done_tag; // @[ProcAxi.scala 134:40]
  assign _GEN_4 = _GEN_12 | _GEN_2; // @[ProcAxi.scala 134:40]
  assign _GEN_5 = proc_io_host2tpu_done_tag | _GEN_3; // @[ProcAxi.scala 134:40]
  assign _T_47 = regFile_io_regsOutput_9; // @[ProcAxi.scala 90:72]
  assign _T_48 = regFile_io_regsOutput_8; // @[ProcAxi.scala 90:72]
  assign fillTLB_vaddr = {_T_47,_T_48}; // @[Cat.scala 29:58]
  assign fillTLB_tlbIdx = regFile_io_regsOutput_10; // @[ProcAxi.scala 90:72]
  assign io_axiLite_awready = 1'h1; // @[ProcAxi.scala 86:14]
  assign io_axiLite_wready = 1'h1; // @[ProcAxi.scala 86:14]
  assign io_axiLite_bresp = regFile_io_axiLite_bresp; // @[ProcAxi.scala 86:14]
  assign io_axiLite_bvalid = regFile_io_axiLite_bvalid; // @[ProcAxi.scala 86:14]
  assign io_axiLite_arready = 1'h1; // @[ProcAxi.scala 86:14]
  assign io_axiLite_rdata = regFile_io_axiLite_rdata; // @[ProcAxi.scala 86:14]
  assign io_axiLite_rresp = 2'h0; // @[ProcAxi.scala 86:14]
  assign io_axiLite_rvalid = regFile_io_axiLite_rvalid; // @[ProcAxi.scala 86:14]
  assign io_memoryBRAM_DO = {_T_32,_T_29}; // @[BramModules.scala 204:8 ProcAxi.scala 83:20]
  assign io_stateBRAM_DO = proc_io_stateBRAM_DO; // @[BramModules.scala 204:8]
  assign regFile_clock = clock;
  assign regFile_reset = reset;
  assign regFile_io_axiLite_awaddr = io_axiLite_awaddr; // @[ProcAxi.scala 86:14]
  assign regFile_io_axiLite_awvalid = io_axiLite_awvalid; // @[ProcAxi.scala 86:14]
  assign regFile_io_axiLite_wdata = io_axiLite_wdata; // @[ProcAxi.scala 86:14]
  assign regFile_io_axiLite_wstrb = io_axiLite_wstrb; // @[ProcAxi.scala 86:14]
  assign regFile_io_axiLite_wvalid = io_axiLite_wvalid; // @[ProcAxi.scala 86:14]
  assign regFile_io_axiLite_araddr = io_axiLite_araddr; // @[ProcAxi.scala 86:14]
  assign regFile_io_axiLite_arvalid = io_axiLite_arvalid; // @[ProcAxi.scala 86:14]
  assign regFile_io_axiLite_rready = io_axiLite_rready; // @[ProcAxi.scala 86:14]
  assign regFile_io_regsInput_1 = {{14'd0}, _T_43}; // @[ProcAxi.scala 92:24]
  assign regFile_io_regsInput_3 = missTLB_vaddr[31:0]; // @[ProcAxi.scala 92:24]
  assign regFile_io_regsInput_4 = missTLB_vaddr[63:32]; // @[ProcAxi.scala 92:24]
  assign regFile_io_regsInput_5 = missTLB_tlbIdx; // @[ProcAxi.scala 92:24]
  assign proc_clock = clock;
  assign proc_reset = reset;
  assign proc_io_memoryBRAM_EN = io_memoryBRAM_EN; // @[BramModules.scala 201:8]
  assign proc_io_memoryBRAM_WE = io_memoryBRAM_WE; // @[BramModules.scala 202:8]
  assign proc_io_memoryBRAM_ADDR = _T[14:0]; // @[BramModules.scala 206:42]
  assign proc_io_memoryBRAM_DI = {_T_16,_T_13}; // @[BramModules.scala 203:8 ProcAxi.scala 82:25]
  assign proc_io_stateBRAM_EN = io_stateBRAM_EN; // @[BramModules.scala 201:8]
  assign proc_io_stateBRAM_WE = io_stateBRAM_WE; // @[BramModules.scala 202:8]
  assign proc_io_stateBRAM_ADDR = _T_33[8:0]; // @[BramModules.scala 206:42]
  assign proc_io_stateBRAM_DI = io_stateBRAM_DI; // @[BramModules.scala 203:8]
  assign proc_io_host2tpu_fire_tag = tagReg[0]; // @[ProcAxi.scala 107:29]
  assign proc_io_host2tpu_fire_valid = regFile_io_regsOutput_0[31]; // @[ProcAxi.scala 106:31]
  assign proc_io_host2tpu_fillTLB_valid = regFile_io_regsOutput_0[29]; // @[ProcAxi.scala 205:34]
  assign proc_io_host2tpu_fillTLB_bits_tlbEntry_wrEn = regFile_io_regsOutput_0[29]; // @[ProcAxi.scala 209:47]
  assign proc_io_host2tpu_fillTLB_bits_tlbEntry_tag = fillTLB_vaddr[63:12]; // @[ProcAxi.scala 210:46]
  assign proc_io_host2tpu_fillTLB_bits_vaddr = {_T_47,_T_48}; // @[ProcAxi.scala 206:39]
  assign proc_io_host2tpu_fillTLB_bits_tlbIdx = fillTLB_tlbIdx[5:0]; // @[ProcAxi.scala 207:40]
  assign proc_io_host2tpu_getState_tag = tagReg[0]; // @[ProcAxi.scala 111:33]
  assign proc_io_host2tpu_getState_valid = regFile_io_regsOutput_0[30]; // @[ProcAxi.scala 110:35]
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
  doneVec_0 = _RAND_0[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  doneVec_1 = _RAND_1[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {2{`RANDOM}};
  missTLB_vaddr = _RAND_2[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  missTLB_tlbIdx = _RAND_3[31:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (reset) begin
      doneVec_0 <= 1'h0;
    end else if (proc_io_host2tpu_done_valid) begin
      doneVec_0 <= _GEN_4;
    end else if (fireReg) begin
      if (~tagReg[0]) begin
        doneVec_0 <= 1'h0;
      end
    end
    if (reset) begin
      doneVec_1 <= 1'h0;
    end else if (proc_io_host2tpu_done_valid) begin
      doneVec_1 <= _GEN_5;
    end else if (fireReg) begin
      if (tagReg[0]) begin
        doneVec_1 <= 1'h0;
      end
    end
    if (reset) begin
      missTLB_vaddr <= 64'h0;
    end else if (proc_io_host2tpu_missTLB_valid) begin
      missTLB_vaddr <= proc_io_host2tpu_missTLB_bits_vaddr;
    end
    if (reset) begin
      missTLB_tlbIdx <= 32'h0;
    end else if (proc_io_host2tpu_missTLB_valid) begin
      missTLB_tlbIdx <= {{26'd0}, proc_io_host2tpu_missTLB_bits_tlbIdx};
    end
  end
endmodule
