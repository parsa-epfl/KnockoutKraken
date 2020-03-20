module TransplantUnit(
  input         clock,
  input         reset,
  input         io_host2tpu_fire_tag,
  input         io_host2tpu_fire_valid,
  output        io_host2tpu_done_tag,
  output        io_host2tpu_done_valid,
  output [63:0] io_host2tpu_missTLB_bits_vaddr,
  output [5:0]  io_host2tpu_missTLB_bits_tlbIdx,
  output        io_host2tpu_missTLB_valid,
  input         io_host2tpu_fillTLB_valid,
  input         io_host2tpu_fillTLB_bits_tlbEntry_wrEn,
  input  [51:0] io_host2tpu_fillTLB_bits_tlbEntry_tag,
  input  [63:0] io_host2tpu_fillTLB_bits_vaddr,
  input  [5:0]  io_host2tpu_fillTLB_bits_tlbIdx,
  input         io_host2tpu_getState_tag,
  input         io_host2tpu_getState_valid,
  output        io_tpu2cpu_flush_tag,
  output        io_tpu2cpu_flush_valid,
  output        io_tpu2cpu_fire_tag,
  output        io_tpu2cpu_fire_valid,
  output        io_tpu2cpu_freeze_tag,
  output        io_tpu2cpu_freeze_valid,
  input         io_tpu2cpu_done_tag,
  input         io_tpu2cpu_done_valid,
  input  [63:0] io_tpu2cpu_missTLB_bits_vaddr,
  input  [5:0]  io_tpu2cpu_missTLB_bits_tlbIdx,
  input         io_tpu2cpu_missTLB_valid,
  output        io_tpu2cpu_fillTLB_valid,
  output        io_tpu2cpu_fillTLB_bits_tlbEntry_wrEn,
  output [51:0] io_tpu2cpu_fillTLB_bits_tlbEntry_tag,
  output [63:0] io_tpu2cpu_fillTLB_bits_vaddr,
  output [5:0]  io_tpu2cpu_fillTLB_bits_tlbIdx,
  output        io_tpu2cpuStateReg_valid,
  output [2:0]  io_tpu2cpuStateReg_bits,
  output [63:0] io_tpu2cpuState_PC,
  output [63:0] io_tpu2cpuState_SP,
  output [3:0]  io_tpu2cpuState_NZCV,
  input  [63:0] io_cpu2tpuState_PC,
  input  [63:0] io_cpu2tpuState_SP,
  input  [3:0]  io_cpu2tpuState_NZCV,
  output [4:0]  io_rfile_rs1_addr,
  input  [63:0] io_rfile_rs1_data,
  output [4:0]  io_rfile_w1_addr,
  output [63:0] io_rfile_w1_data,
  output        io_rfile_w1_en,
  output [7:0]  io_stateBRAM_WE,
  output [8:0]  io_stateBRAM_ADDR,
  output [63:0] io_stateBRAM_DI,
  input  [63:0] io_stateBRAM_DO
);
  reg [5:0] bramOFFST; // @[Transplant.scala 83:26]
  reg [31:0] _RAND_0;
  reg  state; // @[Transplant.scala 94:22]
  reg [31:0] _RAND_1;
  reg  stateDir; // @[Transplant.scala 95:25]
  reg [31:0] _RAND_2;
  reg [2:0] stateRegType; // @[Transplant.scala 96:29]
  reg [31:0] _RAND_3;
  reg  freeze; // @[Transplant.scala 99:23]
  reg [31:0] _RAND_4;
  reg  freezeTag; // @[Transplant.scala 100:26]
  reg [31:0] _RAND_5;
  wire  _T_11; // @[Conditional.scala 37:30]
  wire  _GEN_0; // @[Transplant.scala 136:46]
  wire  _GEN_2; // @[Transplant.scala 136:46]
  wire  _GEN_6; // @[Transplant.scala 136:46]
  wire  _GEN_7; // @[Transplant.scala 127:41]
  wire  _GEN_9; // @[Transplant.scala 127:41]
  wire  _GEN_12; // @[Transplant.scala 127:41]
  wire  _GEN_13; // @[Transplant.scala 127:41]
  wire  _GEN_14; // @[Transplant.scala 119:36]
  wire  _GEN_19; // @[Transplant.scala 119:36]
  wire  _GEN_20; // @[Transplant.scala 119:36]
  wire [5:0] _T_14; // @[Transplant.scala 149:30]
  wire  _T_15; // @[Transplant.scala 150:22]
  wire  _T_16; // @[Transplant.scala 152:28]
  wire  _T_17; // @[Transplant.scala 154:29]
  wire  _T_18; // @[Transplant.scala 156:29]
  wire  _T_19; // @[Transplant.scala 158:32]
  wire  _T_20; // @[Transplant.scala 159:23]
  wire  _GEN_23; // @[Transplant.scala 159:39]
  wire  _GEN_24; // @[Transplant.scala 158:44]
  wire  _GEN_25; // @[Transplant.scala 158:44]
  wire  _GEN_31; // @[Transplant.scala 156:58]
  wire  _GEN_32; // @[Transplant.scala 156:58]
  wire  _GEN_37; // @[Transplant.scala 154:60]
  wire  _GEN_38; // @[Transplant.scala 154:60]
  wire  _GEN_43; // @[Transplant.scala 152:55]
  wire  _GEN_44; // @[Transplant.scala 152:55]
  wire  _GEN_49; // @[Transplant.scala 150:49]
  wire  _GEN_50; // @[Transplant.scala 150:49]
  wire  _GEN_56; // @[Conditional.scala 39:67]
  wire  _GEN_57; // @[Conditional.scala 39:67]
  wire  _T_23; // @[Transplant.scala 170:70]
  wire  _T_24; // @[Transplant.scala 170:54]
  wire  _T_27; // @[Transplant.scala 172:21]
  wire  _T_28; // @[Transplant.scala 174:27]
  wire  _T_29; // @[Transplant.scala 176:27]
  wire  _T_30; // @[Transplant.scala 178:27]
  wire [4:0] _T_32; // @[Cat.scala 29:58]
  wire [4:0] _GEN_69; // @[Transplant.scala 178:39]
  wire [63:0] _GEN_70; // @[Transplant.scala 176:37]
  wire [63:0] _GEN_71; // @[Transplant.scala 174:37]
  reg  _T_36; // @[Transplant.scala 185:28]
  reg [31:0] _RAND_6;
  reg [5:0] _T_37; // @[Transplant.scala 186:30]
  reg [31:0] _RAND_7;
  reg [2:0] _T_39; // @[Transplant.scala 197:37]
  reg [31:0] _RAND_8;
  reg  _T_41; // @[Transplant.scala 198:38]
  reg [31:0] _RAND_9;
  reg  freezeTag1D; // @[Transplant.scala 201:28]
  reg [31:0] _RAND_10;
  reg  _T_42; // @[Transplant.scala 202:35]
  reg [31:0] _RAND_11;
  reg  _T_43; // @[Transplant.scala 206:36]
  reg [31:0] _RAND_12;
  reg  _T_44; // @[Transplant.scala 208:36]
  reg [31:0] _RAND_13;
  assign _T_11 = ~state; // @[Conditional.scala 37:30]
  assign _GEN_0 = io_host2tpu_getState_valid | freeze; // @[Transplant.scala 136:46]
  assign _GEN_2 = io_host2tpu_getState_valid | stateDir; // @[Transplant.scala 136:46]
  assign _GEN_6 = io_host2tpu_getState_valid | state; // @[Transplant.scala 136:46]
  assign _GEN_7 = io_tpu2cpu_done_valid | _GEN_0; // @[Transplant.scala 127:41]
  assign _GEN_9 = io_tpu2cpu_done_valid | _GEN_2; // @[Transplant.scala 127:41]
  assign _GEN_12 = io_tpu2cpu_done_valid | io_host2tpu_getState_valid; // @[Transplant.scala 127:41]
  assign _GEN_13 = io_tpu2cpu_done_valid | _GEN_6; // @[Transplant.scala 127:41]
  assign _GEN_14 = io_host2tpu_fire_valid | _GEN_7; // @[Transplant.scala 119:36]
  assign _GEN_19 = io_host2tpu_fire_valid | _GEN_13; // @[Transplant.scala 119:36]
  assign _GEN_20 = io_host2tpu_fire_valid ? 1'h0 : _GEN_12; // @[Transplant.scala 119:36]
  assign _T_14 = bramOFFST + 6'h1; // @[Transplant.scala 149:30]
  assign _T_15 = bramOFFST == 6'h1f; // @[Transplant.scala 150:22]
  assign _T_16 = bramOFFST == 6'h20; // @[Transplant.scala 152:28]
  assign _T_17 = bramOFFST == 6'h21; // @[Transplant.scala 154:29]
  assign _T_18 = bramOFFST == 6'h22; // @[Transplant.scala 156:29]
  assign _T_19 = stateRegType == 3'h0; // @[Transplant.scala 158:32]
  assign _T_20 = ~stateDir; // @[Transplant.scala 159:23]
  assign _GEN_23 = _T_20 ? 1'h0 : stateDir; // @[Transplant.scala 159:39]
  assign _GEN_24 = _T_19 & _T_20; // @[Transplant.scala 158:44]
  assign _GEN_25 = _T_19 & _GEN_23; // @[Transplant.scala 158:44]
  assign _GEN_31 = _T_18 ? 1'h0 : _GEN_24; // @[Transplant.scala 156:58]
  assign _GEN_32 = _T_18 ? 1'h0 : _GEN_25; // @[Transplant.scala 156:58]
  assign _GEN_37 = _T_17 ? 1'h0 : _GEN_31; // @[Transplant.scala 154:60]
  assign _GEN_38 = _T_17 ? 1'h0 : _GEN_32; // @[Transplant.scala 154:60]
  assign _GEN_43 = _T_16 ? 1'h0 : _GEN_37; // @[Transplant.scala 152:55]
  assign _GEN_44 = _T_16 ? 1'h0 : _GEN_38; // @[Transplant.scala 152:55]
  assign _GEN_49 = _T_15 ? 1'h0 : _GEN_43; // @[Transplant.scala 150:49]
  assign _GEN_50 = _T_15 ? 1'h0 : _GEN_44; // @[Transplant.scala 150:49]
  assign _GEN_56 = state & _GEN_49; // @[Conditional.scala 39:67]
  assign _GEN_57 = state & _GEN_50; // @[Conditional.scala 39:67]
  assign _T_23 = stateRegType != 3'h0; // @[Transplant.scala 170:70]
  assign _T_24 = stateDir & _T_23; // @[Transplant.scala 170:54]
  assign _T_27 = stateRegType == 3'h1; // @[Transplant.scala 172:21]
  assign _T_28 = stateRegType == 3'h2; // @[Transplant.scala 174:27]
  assign _T_29 = stateRegType == 3'h3; // @[Transplant.scala 176:27]
  assign _T_30 = stateRegType == 3'h4; // @[Transplant.scala 178:27]
  assign _T_32 = {1'h0,io_cpu2tpuState_NZCV}; // @[Cat.scala 29:58]
  assign _GEN_69 = _T_30 ? _T_32 : 5'h0; // @[Transplant.scala 178:39]
  assign _GEN_70 = _T_29 ? io_cpu2tpuState_SP : {{59'd0}, _GEN_69}; // @[Transplant.scala 176:37]
  assign _GEN_71 = _T_28 ? io_cpu2tpuState_PC : _GEN_70; // @[Transplant.scala 174:37]
  assign io_host2tpu_done_tag = freezeTag1D; // @[Transplant.scala 209:24]
  assign io_host2tpu_done_valid = _T_44; // @[Transplant.scala 208:26]
  assign io_host2tpu_missTLB_bits_vaddr = io_tpu2cpu_missTLB_bits_vaddr; // @[Transplant.scala 212:23]
  assign io_host2tpu_missTLB_bits_tlbIdx = io_tpu2cpu_missTLB_bits_tlbIdx; // @[Transplant.scala 212:23]
  assign io_host2tpu_missTLB_valid = io_tpu2cpu_missTLB_valid; // @[Transplant.scala 212:23]
  assign io_tpu2cpu_flush_tag = freezeTag1D; // @[Transplant.scala 207:24]
  assign io_tpu2cpu_flush_valid = _T_43; // @[Transplant.scala 206:26]
  assign io_tpu2cpu_fire_tag = freezeTag1D; // @[Transplant.scala 203:23]
  assign io_tpu2cpu_fire_valid = _T_42; // @[Transplant.scala 202:25]
  assign io_tpu2cpu_freeze_tag = freezeTag; // @[Transplant.scala 205:25]
  assign io_tpu2cpu_freeze_valid = freeze; // @[Transplant.scala 204:27]
  assign io_tpu2cpu_fillTLB_valid = io_host2tpu_fillTLB_valid; // @[Transplant.scala 213:23]
  assign io_tpu2cpu_fillTLB_bits_tlbEntry_wrEn = io_host2tpu_fillTLB_bits_tlbEntry_wrEn; // @[Transplant.scala 213:23]
  assign io_tpu2cpu_fillTLB_bits_tlbEntry_tag = io_host2tpu_fillTLB_bits_tlbEntry_tag; // @[Transplant.scala 213:23]
  assign io_tpu2cpu_fillTLB_bits_vaddr = io_host2tpu_fillTLB_bits_vaddr; // @[Transplant.scala 213:23]
  assign io_tpu2cpu_fillTLB_bits_tlbIdx = io_host2tpu_fillTLB_bits_tlbIdx; // @[Transplant.scala 213:23]
  assign io_tpu2cpuStateReg_valid = _T_41; // @[Transplant.scala 198:28]
  assign io_tpu2cpuStateReg_bits = _T_39; // @[Transplant.scala 197:27]
  assign io_tpu2cpuState_PC = io_stateBRAM_DO; // @[Transplant.scala 193:22]
  assign io_tpu2cpuState_SP = io_stateBRAM_DO; // @[Transplant.scala 194:22]
  assign io_tpu2cpuState_NZCV = io_stateBRAM_DO[3:0]; // @[Transplant.scala 195:24]
  assign io_rfile_rs1_addr = bramOFFST[4:0]; // @[Transplant.scala 89:21]
  assign io_rfile_w1_addr = _T_37[4:0]; // @[Transplant.scala 186:20]
  assign io_rfile_w1_data = io_stateBRAM_DO; // @[Transplant.scala 187:20]
  assign io_rfile_w1_en = _T_36; // @[Transplant.scala 185:18]
  assign io_stateBRAM_WE = _T_24 ? 8'hff : 8'h0; // @[Transplant.scala 170:19]
  assign io_stateBRAM_ADDR = {{3'd0}, bramOFFST}; // @[Transplant.scala 171:21]
  assign io_stateBRAM_DI = _T_27 ? io_rfile_rs1_data : _GEN_71; // @[Transplant.scala 173:21 Transplant.scala 175:21 Transplant.scala 177:21 Transplant.scala 179:21 Transplant.scala 181:21]
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
  bramOFFST = _RAND_0[5:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  state = _RAND_1[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  stateDir = _RAND_2[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  stateRegType = _RAND_3[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {1{`RANDOM}};
  freeze = _RAND_4[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  freezeTag = _RAND_5[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  _T_36 = _RAND_6[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  _T_37 = _RAND_7[5:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_8 = {1{`RANDOM}};
  _T_39 = _RAND_8[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_9 = {1{`RANDOM}};
  _T_41 = _RAND_9[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_10 = {1{`RANDOM}};
  freezeTag1D = _RAND_10[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_11 = {1{`RANDOM}};
  _T_42 = _RAND_11[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_12 = {1{`RANDOM}};
  _T_43 = _RAND_12[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_13 = {1{`RANDOM}};
  _T_44 = _RAND_13[0:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (reset) begin
      bramOFFST <= 6'h0;
    end else if (_T_11) begin
      if (io_host2tpu_fire_valid) begin
        bramOFFST <= 6'h0;
      end else if (io_tpu2cpu_done_valid) begin
        bramOFFST <= 6'h0;
      end else if (io_host2tpu_getState_valid) begin
        bramOFFST <= 6'h0;
      end
    end else if (state) begin
      if (_T_15) begin
        bramOFFST <= _T_14;
      end else if (_T_16) begin
        bramOFFST <= _T_14;
      end else if (_T_17) begin
        bramOFFST <= _T_14;
      end else if (_T_18) begin
        bramOFFST <= _T_14;
      end else if (_T_19) begin
        bramOFFST <= 6'h0;
      end else begin
        bramOFFST <= _T_14;
      end
    end
    if (reset) begin
      state <= 1'h0;
    end else if (_T_11) begin
      state <= _GEN_19;
    end else if (state) begin
      if (!(_T_15)) begin
        if (!(_T_16)) begin
          if (!(_T_17)) begin
            if (!(_T_18)) begin
              if (_T_19) begin
                state <= 1'h0;
              end
            end
          end
        end
      end
    end
    if (reset) begin
      stateDir <= 1'h0;
    end else if (_T_11) begin
      if (io_host2tpu_fire_valid) begin
        stateDir <= 1'h0;
      end else begin
        stateDir <= _GEN_9;
      end
    end
    if (reset) begin
      stateRegType <= 3'h0;
    end else if (_T_11) begin
      if (io_host2tpu_fire_valid) begin
        stateRegType <= 3'h1;
      end else if (io_tpu2cpu_done_valid) begin
        stateRegType <= 3'h1;
      end else if (io_host2tpu_getState_valid) begin
        stateRegType <= 3'h1;
      end
    end else if (state) begin
      if (_T_15) begin
        stateRegType <= 3'h2;
      end else if (_T_16) begin
        stateRegType <= 3'h3;
      end else if (_T_17) begin
        stateRegType <= 3'h4;
      end else if (_T_18) begin
        stateRegType <= 3'h0;
      end else if (_T_19) begin
        stateRegType <= 3'h0;
      end
    end
    if (reset) begin
      freeze <= 1'h0;
    end else if (_T_11) begin
      freeze <= _GEN_14;
    end else if (state) begin
      if (!(_T_15)) begin
        if (!(_T_16)) begin
          if (!(_T_17)) begin
            if (!(_T_18)) begin
              if (_T_19) begin
                freeze <= 1'h0;
              end
            end
          end
        end
      end
    end
    if (reset) begin
      freezeTag <= 1'h0;
    end else if (_T_11) begin
      if (io_host2tpu_fire_valid) begin
        freezeTag <= io_host2tpu_fire_tag;
      end else if (io_tpu2cpu_done_valid) begin
        freezeTag <= io_tpu2cpu_done_tag;
      end else if (io_host2tpu_getState_valid) begin
        freezeTag <= io_host2tpu_getState_tag;
      end
    end
    _T_36 <= _T_20 & _T_27;
    _T_37 <= bramOFFST;
    _T_39 <= stateRegType;
    _T_41 <= ~stateDir;
    freezeTag1D <= freezeTag;
    if (_T_11) begin
      _T_42 <= 1'h0;
    end else begin
      _T_42 <= _GEN_56;
    end
    _T_43 <= _T_11 & _GEN_20;
    if (_T_11) begin
      _T_44 <= 1'h0;
    end else begin
      _T_44 <= _GEN_57;
    end
  end
endmodule
