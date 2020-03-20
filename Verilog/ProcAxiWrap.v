module ProcAxiWrap( // @[:@3629.2]
  input         clock, // @[:@3630.4]
  input         reset, // @[:@3631.4]
  input  [3:0]  io_axiLite_awaddr, // @[:@3632.4]
  input  [2:0]  io_axiLite_awprot, // @[:@3632.4]
  input         io_axiLite_awvalid, // @[:@3632.4]
  output        io_axiLite_awready, // @[:@3632.4]
  input  [31:0] io_axiLite_wdata, // @[:@3632.4]
  input  [3:0]  io_axiLite_wstrb, // @[:@3632.4]
  input         io_axiLite_wvalid, // @[:@3632.4]
  output        io_axiLite_wready, // @[:@3632.4]
  output [1:0]  io_axiLite_bresp, // @[:@3632.4]
  output        io_axiLite_bvalid, // @[:@3632.4]
  input         io_axiLite_bready, // @[:@3632.4]
  input  [3:0]  io_axiLite_araddr, // @[:@3632.4]
  input  [2:0]  io_axiLite_arprot, // @[:@3632.4]
  input         io_axiLite_arvalid, // @[:@3632.4]
  output        io_axiLite_arready, // @[:@3632.4]
  output [31:0] io_axiLite_rdata, // @[:@3632.4]
  output [1:0]  io_axiLite_rresp, // @[:@3632.4]
  output        io_axiLite_rvalid, // @[:@3632.4]
  input         io_axiLite_rready, // @[:@3632.4]
  input         io_ppageBRAM_CLK, // @[:@3632.4]
  input         io_ppageBRAM_RST, // @[:@3632.4]
  input         io_ppageBRAM_WE, // @[:@3632.4]
  input         io_ppageBRAM_EN, // @[:@3632.4]
  input  [9:0]  io_ppageBRAM_ADDR, // @[:@3632.4]
  input  [35:0] io_ppageBRAM_DI, // @[:@3632.4]
  output [35:0] io_ppageBRAM_DO, // @[:@3632.4]
  input         io_stateBRAM_CLK, // @[:@3632.4]
  input         io_stateBRAM_RST, // @[:@3632.4]
  input         io_stateBRAM_WE, // @[:@3632.4]
  input         io_stateBRAM_EN, // @[:@3632.4]
  input  [9:0]  io_stateBRAM_ADDR, // @[:@3632.4]
  input  [35:0] io_stateBRAM_DI, // @[:@3632.4]
  output [35:0] io_stateBRAM_DO // @[:@3632.4]
);
  wire  regFile_clock; // @[ProcAxi.scala 22:24:@3634.4]
  wire  regFile_reset; // @[ProcAxi.scala 22:24:@3634.4]
  wire [3:0] regFile_io_axiLite_awaddr; // @[ProcAxi.scala 22:24:@3634.4]
  wire  regFile_io_axiLite_awvalid; // @[ProcAxi.scala 22:24:@3634.4]
  wire [31:0] regFile_io_axiLite_wdata; // @[ProcAxi.scala 22:24:@3634.4]
  wire [3:0] regFile_io_axiLite_wstrb; // @[ProcAxi.scala 22:24:@3634.4]
  wire  regFile_io_axiLite_wvalid; // @[ProcAxi.scala 22:24:@3634.4]
  wire [1:0] regFile_io_axiLite_bresp; // @[ProcAxi.scala 22:24:@3634.4]
  wire  regFile_io_axiLite_bvalid; // @[ProcAxi.scala 22:24:@3634.4]
  wire [3:0] regFile_io_axiLite_araddr; // @[ProcAxi.scala 22:24:@3634.4]
  wire  regFile_io_axiLite_arvalid; // @[ProcAxi.scala 22:24:@3634.4]
  wire [31:0] regFile_io_axiLite_rdata; // @[ProcAxi.scala 22:24:@3634.4]
  wire  regFile_io_axiLite_rvalid; // @[ProcAxi.scala 22:24:@3634.4]
  wire  regFile_io_axiLite_rready; // @[ProcAxi.scala 22:24:@3634.4]
  wire [31:0] regFile_io_regsInput_1; // @[ProcAxi.scala 22:24:@3634.4]
  wire [31:0] regFile_io_regsOutput_0; // @[ProcAxi.scala 22:24:@3634.4]
  wire  proc_clock; // @[ProcAxi.scala 23:20:@3637.4]
  wire  proc_reset; // @[ProcAxi.scala 23:20:@3637.4]
  wire  proc_io_ppageBRAM_WE; // @[ProcAxi.scala 23:20:@3637.4]
  wire  proc_io_ppageBRAM_EN; // @[ProcAxi.scala 23:20:@3637.4]
  wire [9:0] proc_io_ppageBRAM_ADDR; // @[ProcAxi.scala 23:20:@3637.4]
  wire [35:0] proc_io_ppageBRAM_DI; // @[ProcAxi.scala 23:20:@3637.4]
  wire [35:0] proc_io_ppageBRAM_DO; // @[ProcAxi.scala 23:20:@3637.4]
  wire  proc_io_stateBRAM_WE; // @[ProcAxi.scala 23:20:@3637.4]
  wire  proc_io_stateBRAM_EN; // @[ProcAxi.scala 23:20:@3637.4]
  wire [9:0] proc_io_stateBRAM_ADDR; // @[ProcAxi.scala 23:20:@3637.4]
  wire [35:0] proc_io_stateBRAM_DI; // @[ProcAxi.scala 23:20:@3637.4]
  wire [35:0] proc_io_stateBRAM_DO; // @[ProcAxi.scala 23:20:@3637.4]
  wire  proc_io_host2tpu_fire_tag; // @[ProcAxi.scala 23:20:@3637.4]
  wire  proc_io_host2tpu_fire_valid; // @[ProcAxi.scala 23:20:@3637.4]
  wire  proc_io_host2tpu_done_tag; // @[ProcAxi.scala 23:20:@3637.4]
  wire  proc_io_host2tpu_done_valid; // @[ProcAxi.scala 23:20:@3637.4]
  wire  proc_io_host2tpu_getState_tag; // @[ProcAxi.scala 23:20:@3637.4]
  wire  proc_io_host2tpu_getState_valid; // @[ProcAxi.scala 23:20:@3637.4]
  wire [1:0] tagReg; // @[ProcAxi.scala 72:72:@3687.4]
  wire  fireReg; // @[ProcAxi.scala 72:72:@3688.4]
  reg  doneVec_0; // @[ProcAxi.scala 108:24:@3699.4]
  reg [31:0] _RAND_0;
  reg  doneVec_1; // @[ProcAxi.scala 108:24:@3699.4]
  reg [31:0] _RAND_1;
  wire [2:0] _T_145; // @[Cat.scala 30:58:@3701.4]
  wire  _T_149; // @[:@3704.6]
  wire  _GEN_0; // @[ProcAxi.scala 112:21:@3705.6]
  wire  _GEN_1; // @[ProcAxi.scala 112:21:@3705.6]
  wire  _GEN_2; // @[ProcAxi.scala 111:17:@3703.4]
  wire  _GEN_3; // @[ProcAxi.scala 111:17:@3703.4]
  wire  _GEN_4; // @[ProcAxi.scala 115:40:@3708.6]
  wire  _GEN_5; // @[ProcAxi.scala 115:40:@3708.6]
  wire  _GEN_6; // @[ProcAxi.scala 114:37:@3707.4]
  wire  _GEN_7; // @[ProcAxi.scala 114:37:@3707.4]
  AxiMemoryMappedRegFile regFile ( // @[ProcAxi.scala 22:24:@3634.4]
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
    .io_regsOutput_0(regFile_io_regsOutput_0)
  );
  Proc proc ( // @[ProcAxi.scala 23:20:@3637.4]
    .clock(proc_clock),
    .reset(proc_reset),
    .io_ppageBRAM_WE(proc_io_ppageBRAM_WE),
    .io_ppageBRAM_EN(proc_io_ppageBRAM_EN),
    .io_ppageBRAM_ADDR(proc_io_ppageBRAM_ADDR),
    .io_ppageBRAM_DI(proc_io_ppageBRAM_DI),
    .io_ppageBRAM_DO(proc_io_ppageBRAM_DO),
    .io_stateBRAM_WE(proc_io_stateBRAM_WE),
    .io_stateBRAM_EN(proc_io_stateBRAM_EN),
    .io_stateBRAM_ADDR(proc_io_stateBRAM_ADDR),
    .io_stateBRAM_DI(proc_io_stateBRAM_DI),
    .io_stateBRAM_DO(proc_io_stateBRAM_DO),
    .io_host2tpu_fire_tag(proc_io_host2tpu_fire_tag),
    .io_host2tpu_fire_valid(proc_io_host2tpu_fire_valid),
    .io_host2tpu_done_tag(proc_io_host2tpu_done_tag),
    .io_host2tpu_done_valid(proc_io_host2tpu_done_valid),
    .io_host2tpu_getState_tag(proc_io_host2tpu_getState_tag),
    .io_host2tpu_getState_valid(proc_io_host2tpu_getState_valid)
  );
  assign tagReg = regFile_io_regsOutput_0[1:0]; // @[ProcAxi.scala 72:72:@3687.4]
  assign fireReg = regFile_io_regsOutput_0[31]; // @[ProcAxi.scala 72:72:@3688.4]
  assign _T_145 = {1'h0,doneVec_1,doneVec_0}; // @[Cat.scala 30:58:@3701.4]
  assign _T_149 = tagReg[0]; // @[:@3704.6]
  assign _GEN_0 = 1'h0 == _T_149 ? 1'h0 : doneVec_0; // @[ProcAxi.scala 112:21:@3705.6]
  assign _GEN_1 = _T_149 ? 1'h0 : doneVec_1; // @[ProcAxi.scala 112:21:@3705.6]
  assign _GEN_2 = fireReg ? _GEN_0 : doneVec_0; // @[ProcAxi.scala 111:17:@3703.4]
  assign _GEN_3 = fireReg ? _GEN_1 : doneVec_1; // @[ProcAxi.scala 111:17:@3703.4]
  assign _GEN_4 = 1'h0 == proc_io_host2tpu_done_tag ? 1'h1 : _GEN_2; // @[ProcAxi.scala 115:40:@3708.6]
  assign _GEN_5 = proc_io_host2tpu_done_tag ? 1'h1 : _GEN_3; // @[ProcAxi.scala 115:40:@3708.6]
  assign _GEN_6 = proc_io_host2tpu_done_valid ? _GEN_4 : _GEN_2; // @[ProcAxi.scala 114:37:@3707.4]
  assign _GEN_7 = proc_io_host2tpu_done_valid ? _GEN_5 : _GEN_3; // @[ProcAxi.scala 114:37:@3707.4]
  assign io_axiLite_awready = 1'h1; // @[ProcAxi.scala 68:14:@3669.4]
  assign io_axiLite_wready = 1'h1; // @[ProcAxi.scala 68:14:@3665.4]
  assign io_axiLite_bresp = regFile_io_axiLite_bresp; // @[ProcAxi.scala 68:14:@3664.4]
  assign io_axiLite_bvalid = regFile_io_axiLite_bvalid; // @[ProcAxi.scala 68:14:@3663.4]
  assign io_axiLite_arready = 1'h1; // @[ProcAxi.scala 68:14:@3658.4]
  assign io_axiLite_rdata = regFile_io_axiLite_rdata; // @[ProcAxi.scala 68:14:@3657.4]
  assign io_axiLite_rresp = 2'h0; // @[ProcAxi.scala 68:14:@3656.4]
  assign io_axiLite_rvalid = regFile_io_axiLite_rvalid; // @[ProcAxi.scala 68:14:@3655.4]
  assign io_ppageBRAM_DO = proc_io_ppageBRAM_DO; // @[ProcAxi.scala 66:16:@3640.4]
  assign io_stateBRAM_DO = proc_io_stateBRAM_DO; // @[ProcAxi.scala 67:16:@3647.4]
  assign regFile_clock = clock; // @[:@3635.4]
  assign regFile_reset = reset; // @[:@3636.4]
  assign regFile_io_axiLite_awaddr = io_axiLite_awaddr; // @[ProcAxi.scala 68:14:@3672.4]
  assign regFile_io_axiLite_awvalid = io_axiLite_awvalid; // @[ProcAxi.scala 68:14:@3670.4]
  assign regFile_io_axiLite_wdata = io_axiLite_wdata; // @[ProcAxi.scala 68:14:@3668.4]
  assign regFile_io_axiLite_wstrb = io_axiLite_wstrb; // @[ProcAxi.scala 68:14:@3667.4]
  assign regFile_io_axiLite_wvalid = io_axiLite_wvalid; // @[ProcAxi.scala 68:14:@3666.4]
  assign regFile_io_axiLite_araddr = io_axiLite_araddr; // @[ProcAxi.scala 68:14:@3661.4]
  assign regFile_io_axiLite_arvalid = io_axiLite_arvalid; // @[ProcAxi.scala 68:14:@3659.4]
  assign regFile_io_axiLite_rready = io_axiLite_rready; // @[ProcAxi.scala 68:14:@3654.4]
  assign regFile_io_regsInput_1 = {{29'd0}, _T_145}; // @[ProcAxi.scala 74:24:@3684.4]
  assign proc_clock = clock; // @[:@3638.4]
  assign proc_reset = reset; // @[:@3639.4]
  assign proc_io_ppageBRAM_WE = io_ppageBRAM_WE; // @[ProcAxi.scala 66:16:@3644.4]
  assign proc_io_ppageBRAM_EN = io_ppageBRAM_EN; // @[ProcAxi.scala 66:16:@3643.4]
  assign proc_io_ppageBRAM_ADDR = io_ppageBRAM_ADDR; // @[ProcAxi.scala 66:16:@3642.4]
  assign proc_io_ppageBRAM_DI = io_ppageBRAM_DI; // @[ProcAxi.scala 66:16:@3641.4]
  assign proc_io_stateBRAM_WE = io_stateBRAM_WE; // @[ProcAxi.scala 67:16:@3651.4]
  assign proc_io_stateBRAM_EN = io_stateBRAM_EN; // @[ProcAxi.scala 67:16:@3650.4]
  assign proc_io_stateBRAM_ADDR = io_stateBRAM_ADDR; // @[ProcAxi.scala 67:16:@3649.4]
  assign proc_io_stateBRAM_DI = io_stateBRAM_DI; // @[ProcAxi.scala 67:16:@3648.4]
  assign proc_io_host2tpu_fire_tag = tagReg[0]; // @[ProcAxi.scala 90:29:@3691.4]
  assign proc_io_host2tpu_fire_valid = regFile_io_regsOutput_0[31]; // @[ProcAxi.scala 89:31:@3690.4]
  assign proc_io_host2tpu_getState_tag = tagReg[0]; // @[ProcAxi.scala 94:33:@3695.4]
  assign proc_io_host2tpu_getState_valid = regFile_io_regsOutput_0[30]; // @[ProcAxi.scala 93:35:@3694.4]
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
      if (proc_io_host2tpu_done_valid) begin
        if (1'h0 == proc_io_host2tpu_done_tag) begin
          doneVec_0 <= 1'h1;
        end else begin
          if (fireReg) begin
            if (1'h0 == _T_149) begin
              doneVec_0 <= 1'h0;
            end
          end
        end
      end else begin
        if (fireReg) begin
          if (1'h0 == _T_149) begin
            doneVec_0 <= 1'h0;
          end
        end
      end
    end
    if (reset) begin
      doneVec_1 <= 1'h0;
    end else begin
      if (proc_io_host2tpu_done_valid) begin
        if (proc_io_host2tpu_done_tag) begin
          doneVec_1 <= 1'h1;
        end else begin
          if (fireReg) begin
            if (_T_149) begin
              doneVec_1 <= 1'h0;
            end
          end
        end
      end else begin
        if (fireReg) begin
          if (_T_149) begin
            doneVec_1 <= 1'h0;
          end
        end
      end
    end
  end
endmodule
