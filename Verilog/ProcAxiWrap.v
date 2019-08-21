module ProcAxiWrap( // @[:@3427.2]
  input         clock, // @[:@3428.4]
  input         reset, // @[:@3429.4]
  input  [3:0]  io_axiLite_awaddr, // @[:@3430.4]
  input  [2:0]  io_axiLite_awprot, // @[:@3430.4]
  input         io_axiLite_awvalid, // @[:@3430.4]
  output        io_axiLite_awready, // @[:@3430.4]
  input  [31:0] io_axiLite_wdata, // @[:@3430.4]
  input  [3:0]  io_axiLite_wstrb, // @[:@3430.4]
  input         io_axiLite_wvalid, // @[:@3430.4]
  output        io_axiLite_wready, // @[:@3430.4]
  output [1:0]  io_axiLite_bresp, // @[:@3430.4]
  output        io_axiLite_bvalid, // @[:@3430.4]
  input         io_axiLite_bready, // @[:@3430.4]
  input  [3:0]  io_axiLite_araddr, // @[:@3430.4]
  input  [2:0]  io_axiLite_arprot, // @[:@3430.4]
  input         io_axiLite_arvalid, // @[:@3430.4]
  output        io_axiLite_arready, // @[:@3430.4]
  output [31:0] io_axiLite_rdata, // @[:@3430.4]
  output [1:0]  io_axiLite_rresp, // @[:@3430.4]
  output        io_axiLite_rvalid, // @[:@3430.4]
  input         io_axiLite_rready, // @[:@3430.4]
  input         io_ppageBRAM_writeEn, // @[:@3430.4]
  input         io_ppageBRAM_en, // @[:@3430.4]
  input  [9:0]  io_ppageBRAM_addr, // @[:@3430.4]
  input  [35:0] io_ppageBRAM_dataIn, // @[:@3430.4]
  output [35:0] io_ppageBRAM_dataOut, // @[:@3430.4]
  input         io_stateBRAM_writeEn, // @[:@3430.4]
  input         io_stateBRAM_en, // @[:@3430.4]
  input  [9:0]  io_stateBRAM_addr, // @[:@3430.4]
  input  [35:0] io_stateBRAM_dataIn, // @[:@3430.4]
  output [35:0] io_stateBRAM_dataOut // @[:@3430.4]
);
  wire  regFile_clock; // @[ProcAxi.scala 22:24:@3432.4]
  wire  regFile_reset; // @[ProcAxi.scala 22:24:@3432.4]
  wire [3:0] regFile_io_axiLite_awaddr; // @[ProcAxi.scala 22:24:@3432.4]
  wire  regFile_io_axiLite_awvalid; // @[ProcAxi.scala 22:24:@3432.4]
  wire [31:0] regFile_io_axiLite_wdata; // @[ProcAxi.scala 22:24:@3432.4]
  wire [3:0] regFile_io_axiLite_wstrb; // @[ProcAxi.scala 22:24:@3432.4]
  wire  regFile_io_axiLite_wvalid; // @[ProcAxi.scala 22:24:@3432.4]
  wire  regFile_io_axiLite_wready; // @[ProcAxi.scala 22:24:@3432.4]
  wire [1:0] regFile_io_axiLite_bresp; // @[ProcAxi.scala 22:24:@3432.4]
  wire  regFile_io_axiLite_bvalid; // @[ProcAxi.scala 22:24:@3432.4]
  wire  regFile_io_axiLite_bready; // @[ProcAxi.scala 22:24:@3432.4]
  wire [3:0] regFile_io_axiLite_araddr; // @[ProcAxi.scala 22:24:@3432.4]
  wire  regFile_io_axiLite_arvalid; // @[ProcAxi.scala 22:24:@3432.4]
  wire [31:0] regFile_io_axiLite_rdata; // @[ProcAxi.scala 22:24:@3432.4]
  wire  regFile_io_axiLite_rvalid; // @[ProcAxi.scala 22:24:@3432.4]
  wire  regFile_io_axiLite_rready; // @[ProcAxi.scala 22:24:@3432.4]
  wire [31:0] regFile_io_moduleInputs_1; // @[ProcAxi.scala 22:24:@3432.4]
  wire [31:0] regFile_io_regsValues_0; // @[ProcAxi.scala 22:24:@3432.4]
  wire  proc_clock; // @[ProcAxi.scala 23:20:@3435.4]
  wire  proc_reset; // @[ProcAxi.scala 23:20:@3435.4]
  wire  proc_io_ppageBRAM_writeEn; // @[ProcAxi.scala 23:20:@3435.4]
  wire  proc_io_ppageBRAM_en; // @[ProcAxi.scala 23:20:@3435.4]
  wire [9:0] proc_io_ppageBRAM_addr; // @[ProcAxi.scala 23:20:@3435.4]
  wire [35:0] proc_io_ppageBRAM_dataIn; // @[ProcAxi.scala 23:20:@3435.4]
  wire [35:0] proc_io_ppageBRAM_dataOut; // @[ProcAxi.scala 23:20:@3435.4]
  wire  proc_io_stateBRAM_writeEn; // @[ProcAxi.scala 23:20:@3435.4]
  wire  proc_io_stateBRAM_en; // @[ProcAxi.scala 23:20:@3435.4]
  wire [9:0] proc_io_stateBRAM_addr; // @[ProcAxi.scala 23:20:@3435.4]
  wire [35:0] proc_io_stateBRAM_dataIn; // @[ProcAxi.scala 23:20:@3435.4]
  wire [35:0] proc_io_stateBRAM_dataOut; // @[ProcAxi.scala 23:20:@3435.4]
  wire  proc_io_host2tpu_fire; // @[ProcAxi.scala 23:20:@3435.4]
  wire  proc_io_host2tpu_fireTag; // @[ProcAxi.scala 23:20:@3435.4]
  wire  proc_io_host2tpu_done; // @[ProcAxi.scala 23:20:@3435.4]
  wire  proc_io_host2tpu_doneTag; // @[ProcAxi.scala 23:20:@3435.4]
  wire  fireReg; // @[ProcAxi.scala 39:72:@3481.4]
  wire [1:0] fireTagReg; // @[ProcAxi.scala 39:72:@3483.4]
  reg  doneVec_0; // @[ProcAxi.scala 68:24:@3489.4]
  reg [31:0] _RAND_0;
  reg  doneVec_1; // @[ProcAxi.scala 68:24:@3489.4]
  reg [31:0] _RAND_1;
  wire [2:0] _T_136; // @[Cat.scala 30:58:@3491.4]
  wire  _GEN_0; // @[ProcAxi.scala 72:39:@3494.6]
  wire  _GEN_1; // @[ProcAxi.scala 72:39:@3494.6]
  wire  _GEN_2; // @[ProcAxi.scala 71:31:@3493.4]
  wire  _GEN_3; // @[ProcAxi.scala 71:31:@3493.4]
  wire  _T_144; // @[:@3497.6]
  wire  _GEN_4; // @[ProcAxi.scala 75:25:@3498.6]
  wire  _GEN_5; // @[ProcAxi.scala 75:25:@3498.6]
  wire  _GEN_6; // @[ProcAxi.scala 74:17:@3496.4]
  wire  _GEN_7; // @[ProcAxi.scala 74:17:@3496.4]
  AxiMemoryMappedWithRegs regFile ( // @[ProcAxi.scala 22:24:@3432.4]
    .clock(regFile_clock),
    .reset(regFile_reset),
    .io_axiLite_awaddr(regFile_io_axiLite_awaddr),
    .io_axiLite_awvalid(regFile_io_axiLite_awvalid),
    .io_axiLite_wdata(regFile_io_axiLite_wdata),
    .io_axiLite_wstrb(regFile_io_axiLite_wstrb),
    .io_axiLite_wvalid(regFile_io_axiLite_wvalid),
    .io_axiLite_wready(regFile_io_axiLite_wready),
    .io_axiLite_bresp(regFile_io_axiLite_bresp),
    .io_axiLite_bvalid(regFile_io_axiLite_bvalid),
    .io_axiLite_bready(regFile_io_axiLite_bready),
    .io_axiLite_araddr(regFile_io_axiLite_araddr),
    .io_axiLite_arvalid(regFile_io_axiLite_arvalid),
    .io_axiLite_rdata(regFile_io_axiLite_rdata),
    .io_axiLite_rvalid(regFile_io_axiLite_rvalid),
    .io_axiLite_rready(regFile_io_axiLite_rready),
    .io_moduleInputs_1(regFile_io_moduleInputs_1),
    .io_regsValues_0(regFile_io_regsValues_0)
  );
  Proc proc ( // @[ProcAxi.scala 23:20:@3435.4]
    .clock(proc_clock),
    .reset(proc_reset),
    .io_ppageBRAM_writeEn(proc_io_ppageBRAM_writeEn),
    .io_ppageBRAM_en(proc_io_ppageBRAM_en),
    .io_ppageBRAM_addr(proc_io_ppageBRAM_addr),
    .io_ppageBRAM_dataIn(proc_io_ppageBRAM_dataIn),
    .io_ppageBRAM_dataOut(proc_io_ppageBRAM_dataOut),
    .io_stateBRAM_writeEn(proc_io_stateBRAM_writeEn),
    .io_stateBRAM_en(proc_io_stateBRAM_en),
    .io_stateBRAM_addr(proc_io_stateBRAM_addr),
    .io_stateBRAM_dataIn(proc_io_stateBRAM_dataIn),
    .io_stateBRAM_dataOut(proc_io_stateBRAM_dataOut),
    .io_host2tpu_fire(proc_io_host2tpu_fire),
    .io_host2tpu_fireTag(proc_io_host2tpu_fireTag),
    .io_host2tpu_done(proc_io_host2tpu_done),
    .io_host2tpu_doneTag(proc_io_host2tpu_doneTag)
  );
  assign fireReg = regFile_io_regsValues_0[31]; // @[ProcAxi.scala 39:72:@3481.4]
  assign fireTagReg = regFile_io_regsValues_0[1:0]; // @[ProcAxi.scala 39:72:@3483.4]
  assign _T_136 = {1'h0,doneVec_1,doneVec_0}; // @[Cat.scala 30:58:@3491.4]
  assign _GEN_0 = 1'h0 == proc_io_host2tpu_doneTag ? 1'h1 : doneVec_0; // @[ProcAxi.scala 72:39:@3494.6]
  assign _GEN_1 = proc_io_host2tpu_doneTag ? 1'h1 : doneVec_1; // @[ProcAxi.scala 72:39:@3494.6]
  assign _GEN_2 = proc_io_host2tpu_done ? _GEN_0 : doneVec_0; // @[ProcAxi.scala 71:31:@3493.4]
  assign _GEN_3 = proc_io_host2tpu_done ? _GEN_1 : doneVec_1; // @[ProcAxi.scala 71:31:@3493.4]
  assign _T_144 = fireTagReg[0]; // @[:@3497.6]
  assign _GEN_4 = 1'h0 == _T_144 ? 1'h0 : _GEN_2; // @[ProcAxi.scala 75:25:@3498.6]
  assign _GEN_5 = _T_144 ? 1'h0 : _GEN_3; // @[ProcAxi.scala 75:25:@3498.6]
  assign _GEN_6 = fireReg ? _GEN_4 : _GEN_2; // @[ProcAxi.scala 74:17:@3496.4]
  assign _GEN_7 = fireReg ? _GEN_5 : _GEN_3; // @[ProcAxi.scala 74:17:@3496.4]
  assign io_axiLite_awready = 1'h1; // @[ProcAxi.scala 35:14:@3463.4]
  assign io_axiLite_wready = regFile_io_axiLite_wready; // @[ProcAxi.scala 35:14:@3459.4]
  assign io_axiLite_bresp = regFile_io_axiLite_bresp; // @[ProcAxi.scala 35:14:@3458.4]
  assign io_axiLite_bvalid = regFile_io_axiLite_bvalid; // @[ProcAxi.scala 35:14:@3457.4]
  assign io_axiLite_arready = 1'h1; // @[ProcAxi.scala 35:14:@3452.4]
  assign io_axiLite_rdata = regFile_io_axiLite_rdata; // @[ProcAxi.scala 35:14:@3451.4]
  assign io_axiLite_rresp = 2'h0; // @[ProcAxi.scala 35:14:@3450.4]
  assign io_axiLite_rvalid = regFile_io_axiLite_rvalid; // @[ProcAxi.scala 35:14:@3449.4]
  assign io_ppageBRAM_dataOut = proc_io_ppageBRAM_dataOut; // @[ProcAxi.scala 33:16:@3438.4]
  assign io_stateBRAM_dataOut = proc_io_stateBRAM_dataOut; // @[ProcAxi.scala 34:16:@3443.4]
  assign regFile_clock = clock; // @[:@3433.4]
  assign regFile_reset = reset; // @[:@3434.4]
  assign regFile_io_axiLite_awaddr = io_axiLite_awaddr; // @[ProcAxi.scala 35:14:@3466.4]
  assign regFile_io_axiLite_awvalid = io_axiLite_awvalid; // @[ProcAxi.scala 35:14:@3464.4]
  assign regFile_io_axiLite_wdata = io_axiLite_wdata; // @[ProcAxi.scala 35:14:@3462.4]
  assign regFile_io_axiLite_wstrb = io_axiLite_wstrb; // @[ProcAxi.scala 35:14:@3461.4]
  assign regFile_io_axiLite_wvalid = io_axiLite_wvalid; // @[ProcAxi.scala 35:14:@3460.4]
  assign regFile_io_axiLite_bready = io_axiLite_bready; // @[ProcAxi.scala 35:14:@3456.4]
  assign regFile_io_axiLite_araddr = io_axiLite_araddr; // @[ProcAxi.scala 35:14:@3455.4]
  assign regFile_io_axiLite_arvalid = io_axiLite_arvalid; // @[ProcAxi.scala 35:14:@3453.4]
  assign regFile_io_axiLite_rready = io_axiLite_rready; // @[ProcAxi.scala 35:14:@3448.4]
  assign regFile_io_moduleInputs_1 = {{29'd0}, _T_136}; // @[ProcAxi.scala 41:27:@3478.4]
  assign proc_clock = clock; // @[:@3436.4]
  assign proc_reset = reset; // @[:@3437.4]
  assign proc_io_ppageBRAM_writeEn = io_ppageBRAM_writeEn; // @[ProcAxi.scala 33:16:@3442.4]
  assign proc_io_ppageBRAM_en = io_ppageBRAM_en; // @[ProcAxi.scala 33:16:@3441.4]
  assign proc_io_ppageBRAM_addr = io_ppageBRAM_addr; // @[ProcAxi.scala 33:16:@3440.4]
  assign proc_io_ppageBRAM_dataIn = io_ppageBRAM_dataIn; // @[ProcAxi.scala 33:16:@3439.4]
  assign proc_io_stateBRAM_writeEn = io_stateBRAM_writeEn; // @[ProcAxi.scala 34:16:@3447.4]
  assign proc_io_stateBRAM_en = io_stateBRAM_en; // @[ProcAxi.scala 34:16:@3446.4]
  assign proc_io_stateBRAM_addr = io_stateBRAM_addr; // @[ProcAxi.scala 34:16:@3445.4]
  assign proc_io_stateBRAM_dataIn = io_stateBRAM_dataIn; // @[ProcAxi.scala 34:16:@3444.4]
  assign proc_io_host2tpu_fire = regFile_io_regsValues_0[31]; // @[ProcAxi.scala 55:28:@3484.4]
  assign proc_io_host2tpu_fireTag = fireTagReg[0]; // @[ProcAxi.scala 56:28:@3485.4]
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
  doneVec_0 = _RAND_0[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  doneVec_1 = _RAND_1[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (reset) begin
      doneVec_0 <= 1'h0;
    end else begin
      if (fireReg) begin
        if (1'h0 == _T_144) begin
          doneVec_0 <= 1'h0;
        end else begin
          if (proc_io_host2tpu_done) begin
            if (1'h0 == proc_io_host2tpu_doneTag) begin
              doneVec_0 <= 1'h1;
            end
          end
        end
      end else begin
        if (proc_io_host2tpu_done) begin
          if (1'h0 == proc_io_host2tpu_doneTag) begin
            doneVec_0 <= 1'h1;
          end
        end
      end
    end
    if (reset) begin
      doneVec_1 <= 1'h0;
    end else begin
      if (fireReg) begin
        if (_T_144) begin
          doneVec_1 <= 1'h0;
        end else begin
          if (proc_io_host2tpu_done) begin
            if (proc_io_host2tpu_doneTag) begin
              doneVec_1 <= 1'h1;
            end
          end
        end
      end else begin
        if (proc_io_host2tpu_done) begin
          if (proc_io_host2tpu_doneTag) begin
            doneVec_1 <= 1'h1;
          end
        end
      end
    end
  end
endmodule
