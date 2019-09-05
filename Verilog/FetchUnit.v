module FetchUnit( // @[:@546.2]
  input         clock, // @[:@547.4]
  input         reset, // @[:@548.4]
  input         io_en, // @[:@549.4]
  input  [63:0] io_vecPC_0, // @[:@549.4]
  input  [63:0] io_vecPC_1, // @[:@549.4]
  input         io_tagIn, // @[:@549.4]
  input         io_flush, // @[:@549.4]
  input         io_fire_valid, // @[:@549.4]
  input         io_fire_bits, // @[:@549.4]
  input         io_branch_valid, // @[:@549.4]
  input  [63:0] io_branch_bits_offset, // @[:@549.4]
  input         io_branch_bits_tag, // @[:@549.4]
  output [9:0]  io_ppageBRAM_addr, // @[:@549.4]
  input  [35:0] io_ppageBRAM_dataOut, // @[:@549.4]
  input         io_deq_ready, // @[:@549.4]
  output        io_deq_valid, // @[:@549.4]
  output [31:0] io_deq_bits_inst, // @[:@549.4]
  output        io_deq_bits_tag, // @[:@549.4]
  output [63:0] io_deq_bits_pc // @[:@549.4]
);
  wire  pcTLB_clock; // @[fetch.scala 40:21:@555.4]
  wire  pcTLB_reset; // @[fetch.scala 40:21:@555.4]
  wire [63:0] pcTLB_io_vaddrFill; // @[fetch.scala 40:21:@555.4]
  wire  pcTLB_io_fill; // @[fetch.scala 40:21:@555.4]
  wire  pcTLB_io_vaddr_valid; // @[fetch.scala 40:21:@555.4]
  wire [63:0] pcTLB_io_vaddr_bits; // @[fetch.scala 40:21:@555.4]
  wire  pcTLB_io_hit; // @[fetch.scala 40:21:@555.4]
  wire  pcTLB_io_paddr_valid; // @[fetch.scala 40:21:@555.4]
  wire [63:0] pcTLB_io_paddr_bits; // @[fetch.scala 40:21:@555.4]
  reg [63:0] fake_PC_0; // @[fetch.scala 39:24:@554.4]
  reg [63:0] _RAND_0;
  reg [63:0] fake_PC_1; // @[fetch.scala 39:24:@554.4]
  reg [63:0] _RAND_1;
  wire [63:0] _GEN_1; // @[fetch.scala 41:23:@558.4]
  wire [63:0] _GEN_3; // @[fetch.scala 44:22:@561.4]
  reg  valid; // @[fetch.scala 52:22:@566.4]
  reg [31:0] _RAND_2;
  reg  tag; // @[fetch.scala 53:20:@567.4]
  reg [31:0] _RAND_3;
  reg [63:0] pc; // @[fetch.scala 54:19:@568.4]
  reg [63:0] _RAND_4;
  wire  readIns; // @[fetch.scala 55:30:@569.4]
  wire  _T_122; // @[fetch.scala 57:20:@571.6]
  wire  _T_123; // @[fetch.scala 57:44:@572.6]
  wire  _GEN_4; // @[fetch.scala 56:17:@570.4]
  wire  _GEN_5; // @[fetch.scala 56:17:@570.4]
  wire [63:0] _GEN_6; // @[fetch.scala 56:17:@570.4]
  reg  instV_valid; // @[fetch.scala 74:22:@585.4]
  reg [31:0] _RAND_5;
  reg [31:0] instV_bits; // @[fetch.scala 74:22:@585.4]
  reg [31:0] _RAND_6;
  wire  _T_138; // @[fetch.scala 75:8:@586.4]
  reg  _T_140; // @[fetch.scala 75:32:@587.4]
  reg [31:0] _RAND_7;
  wire  _T_141; // @[fetch.scala 75:22:@589.4]
  wire  _GEN_7; // @[fetch.scala 75:48:@590.4]
  wire [35:0] _GEN_8; // @[fetch.scala 75:48:@590.4]
  wire  _GEN_9; // @[fetch.scala 80:22:@594.4]
  wire  _T_145; // @[fetch.scala 84:27:@597.4]
  wire  _T_146; // @[fetch.scala 85:22:@599.4]
  wire [35:0] _T_147; // @[fetch.scala 90:26:@604.4]
  wire [64:0] _T_156; // @[fetch.scala 93:44:@608.6]
  wire [63:0] _T_157; // @[fetch.scala 93:44:@609.6]
  wire [63:0] _GEN_10; // @[fetch.scala 93:23:@610.6]
  wire [63:0] _GEN_11; // @[fetch.scala 93:23:@610.6]
  wire [63:0] _GEN_12; // @[fetch.scala 92:26:@607.4]
  wire [63:0] _GEN_13; // @[fetch.scala 92:26:@607.4]
  wire [63:0] _GEN_15; // @[fetch.scala 96:66:@613.6]
  wire [64:0] _T_162; // @[fetch.scala 96:66:@613.6]
  wire [63:0] _T_163; // @[fetch.scala 96:95:@614.6]
  wire [64:0] _GEN_24; // @[fetch.scala 96:71:@615.6]
  wire [65:0] _T_164; // @[fetch.scala 96:71:@615.6]
  wire [64:0] _T_165; // @[fetch.scala 96:71:@616.6]
  wire [64:0] _T_166; // @[fetch.scala 96:71:@617.6]
  wire [64:0] _T_167; // @[fetch.scala 96:109:@618.6]
  wire [63:0] _fake_PC_io_branch_bits_tag; // @[fetch.scala 96:33:@619.6 fetch.scala 96:33:@619.6]
  wire [63:0] _GEN_16; // @[fetch.scala 96:33:@619.6]
  wire [63:0] _GEN_17; // @[fetch.scala 96:33:@619.6]
  wire [63:0] _GEN_18; // @[fetch.scala 95:25:@612.4]
  wire [63:0] _GEN_19; // @[fetch.scala 95:25:@612.4]
  wire [63:0] _GEN_20; // @[fetch.scala 99:27:@622.6]
  wire [63:0] _GEN_21; // @[fetch.scala 99:27:@622.6]
  wire [63:0] _GEN_22; // @[fetch.scala 98:23:@621.4]
  wire [63:0] _GEN_23; // @[fetch.scala 98:23:@621.4]
  TLBUnit pcTLB ( // @[fetch.scala 40:21:@555.4]
    .clock(pcTLB_clock),
    .reset(pcTLB_reset),
    .io_vaddrFill(pcTLB_io_vaddrFill),
    .io_fill(pcTLB_io_fill),
    .io_vaddr_valid(pcTLB_io_vaddr_valid),
    .io_vaddr_bits(pcTLB_io_vaddr_bits),
    .io_hit(pcTLB_io_hit),
    .io_paddr_valid(pcTLB_io_paddr_valid),
    .io_paddr_bits(pcTLB_io_paddr_bits)
  );
  assign _GEN_1 = io_tagIn ? fake_PC_1 : fake_PC_0; // @[fetch.scala 41:23:@558.4]
  assign _GEN_3 = io_fire_bits ? io_vecPC_1 : io_vecPC_0; // @[fetch.scala 44:22:@561.4]
  assign readIns = io_deq_ready | io_flush; // @[fetch.scala 55:30:@569.4]
  assign _T_122 = io_en & pcTLB_io_paddr_valid; // @[fetch.scala 57:20:@571.6]
  assign _T_123 = _T_122 & pcTLB_io_hit; // @[fetch.scala 57:44:@572.6]
  assign _GEN_4 = readIns ? _T_123 : valid; // @[fetch.scala 56:17:@570.4]
  assign _GEN_5 = readIns ? io_tagIn : tag; // @[fetch.scala 56:17:@570.4]
  assign _GEN_6 = readIns ? _GEN_1 : pc; // @[fetch.scala 56:17:@570.4]
  assign _T_138 = io_deq_ready == 1'h0; // @[fetch.scala 75:8:@586.4]
  assign _T_141 = _T_138 & _T_140; // @[fetch.scala 75:22:@589.4]
  assign _GEN_7 = _T_141 ? 1'h1 : instV_valid; // @[fetch.scala 75:48:@590.4]
  assign _GEN_8 = _T_141 ? io_ppageBRAM_dataOut : {{4'd0}, instV_bits}; // @[fetch.scala 75:48:@590.4]
  assign _GEN_9 = io_deq_ready ? 1'h0 : _GEN_7; // @[fetch.scala 80:22:@594.4]
  assign _T_145 = io_flush == 1'h0; // @[fetch.scala 84:27:@597.4]
  assign _T_146 = readIns & io_en; // @[fetch.scala 85:22:@599.4]
  assign _T_147 = instV_valid ? {{4'd0}, instV_bits} : io_ppageBRAM_dataOut; // @[fetch.scala 90:26:@604.4]
  assign _T_156 = _GEN_1 + 64'h4; // @[fetch.scala 93:44:@608.6]
  assign _T_157 = _GEN_1 + 64'h4; // @[fetch.scala 93:44:@609.6]
  assign _GEN_10 = 1'h0 == io_tagIn ? _T_157 : fake_PC_0; // @[fetch.scala 93:23:@610.6]
  assign _GEN_11 = io_tagIn ? _T_157 : fake_PC_1; // @[fetch.scala 93:23:@610.6]
  assign _GEN_12 = _T_146 ? _GEN_10 : fake_PC_0; // @[fetch.scala 92:26:@607.4]
  assign _GEN_13 = _T_146 ? _GEN_11 : fake_PC_1; // @[fetch.scala 92:26:@607.4]
  assign _GEN_15 = io_branch_bits_tag ? io_vecPC_1 : io_vecPC_0; // @[fetch.scala 96:66:@613.6]
  assign _T_162 = {1'b0,$signed(_GEN_15)}; // @[fetch.scala 96:66:@613.6]
  assign _T_163 = $signed(io_branch_bits_offset); // @[fetch.scala 96:95:@614.6]
  assign _GEN_24 = {{1{_T_163[63]}},_T_163}; // @[fetch.scala 96:71:@615.6]
  assign _T_164 = $signed(_T_162) + $signed(_GEN_24); // @[fetch.scala 96:71:@615.6]
  assign _T_165 = $signed(_T_162) + $signed(_GEN_24); // @[fetch.scala 96:71:@616.6]
  assign _T_166 = $signed(_T_165); // @[fetch.scala 96:71:@617.6]
  assign _T_167 = $unsigned(_T_166); // @[fetch.scala 96:109:@618.6]
  assign _fake_PC_io_branch_bits_tag = _T_167[63:0]; // @[fetch.scala 96:33:@619.6 fetch.scala 96:33:@619.6]
  assign _GEN_16 = 1'h0 == io_branch_bits_tag ? _fake_PC_io_branch_bits_tag : _GEN_12; // @[fetch.scala 96:33:@619.6]
  assign _GEN_17 = io_branch_bits_tag ? _fake_PC_io_branch_bits_tag : _GEN_13; // @[fetch.scala 96:33:@619.6]
  assign _GEN_18 = io_branch_valid ? _GEN_16 : _GEN_12; // @[fetch.scala 95:25:@612.4]
  assign _GEN_19 = io_branch_valid ? _GEN_17 : _GEN_13; // @[fetch.scala 95:25:@612.4]
  assign _GEN_20 = 1'h0 == io_fire_bits ? _GEN_3 : _GEN_18; // @[fetch.scala 99:27:@622.6]
  assign _GEN_21 = io_fire_bits ? _GEN_3 : _GEN_19; // @[fetch.scala 99:27:@622.6]
  assign _GEN_22 = io_fire_valid ? _GEN_20 : _GEN_18; // @[fetch.scala 98:23:@621.4]
  assign _GEN_23 = io_fire_valid ? _GEN_21 : _GEN_19; // @[fetch.scala 98:23:@621.4]
  assign io_ppageBRAM_addr = pcTLB_io_paddr_bits[9:0]; // @[fetch.scala 47:21:@563.4]
  assign io_deq_valid = valid & _T_145; // @[fetch.scala 87:16:@601.4]
  assign io_deq_bits_inst = _T_147[31:0]; // @[fetch.scala 90:20:@605.4]
  assign io_deq_bits_tag = tag; // @[fetch.scala 88:19:@602.4]
  assign io_deq_bits_pc = pc; // @[fetch.scala 89:18:@603.4]
  assign pcTLB_clock = clock; // @[:@556.4]
  assign pcTLB_reset = reset; // @[:@557.4]
  assign pcTLB_io_vaddrFill = io_fire_bits ? io_vecPC_1 : io_vecPC_0; // @[fetch.scala 44:22:@561.4]
  assign pcTLB_io_fill = io_fire_valid; // @[fetch.scala 43:17:@560.4]
  assign pcTLB_io_vaddr_valid = io_en; // @[fetch.scala 42:24:@559.4]
  assign pcTLB_io_vaddr_bits = io_tagIn ? fake_PC_1 : fake_PC_0; // @[fetch.scala 41:23:@558.4]
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
  _RAND_0 = {2{`RANDOM}};
  fake_PC_0 = _RAND_0[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {2{`RANDOM}};
  fake_PC_1 = _RAND_1[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  valid = _RAND_2[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  tag = _RAND_3[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_4 = {2{`RANDOM}};
  pc = _RAND_4[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_5 = {1{`RANDOM}};
  instV_valid = _RAND_5[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_6 = {1{`RANDOM}};
  instV_bits = _RAND_6[31:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_7 = {1{`RANDOM}};
  _T_140 = _RAND_7[0:0];
  `endif // RANDOMIZE_REG_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if (reset) begin
      fake_PC_0 <= 64'h0;
    end else begin
      if (io_fire_valid) begin
        if (1'h0 == io_fire_bits) begin
          if (io_fire_bits) begin
            fake_PC_0 <= io_vecPC_1;
          end else begin
            fake_PC_0 <= io_vecPC_0;
          end
        end else begin
          if (io_branch_valid) begin
            if (1'h0 == io_branch_bits_tag) begin
              fake_PC_0 <= _fake_PC_io_branch_bits_tag;
            end else begin
              if (_T_146) begin
                if (1'h0 == io_tagIn) begin
                  fake_PC_0 <= _T_157;
                end
              end
            end
          end else begin
            if (_T_146) begin
              if (1'h0 == io_tagIn) begin
                fake_PC_0 <= _T_157;
              end
            end
          end
        end
      end else begin
        if (io_branch_valid) begin
          if (1'h0 == io_branch_bits_tag) begin
            fake_PC_0 <= _fake_PC_io_branch_bits_tag;
          end else begin
            if (_T_146) begin
              if (1'h0 == io_tagIn) begin
                fake_PC_0 <= _T_157;
              end
            end
          end
        end else begin
          if (_T_146) begin
            if (1'h0 == io_tagIn) begin
              fake_PC_0 <= _T_157;
            end
          end
        end
      end
    end
    if (reset) begin
      fake_PC_1 <= 64'h0;
    end else begin
      if (io_fire_valid) begin
        if (io_fire_bits) begin
          if (io_fire_bits) begin
            fake_PC_1 <= io_vecPC_1;
          end else begin
            fake_PC_1 <= io_vecPC_0;
          end
        end else begin
          if (io_branch_valid) begin
            if (io_branch_bits_tag) begin
              fake_PC_1 <= _fake_PC_io_branch_bits_tag;
            end else begin
              if (_T_146) begin
                if (io_tagIn) begin
                  fake_PC_1 <= _T_157;
                end
              end
            end
          end else begin
            if (_T_146) begin
              if (io_tagIn) begin
                fake_PC_1 <= _T_157;
              end
            end
          end
        end
      end else begin
        if (io_branch_valid) begin
          if (io_branch_bits_tag) begin
            fake_PC_1 <= _fake_PC_io_branch_bits_tag;
          end else begin
            if (_T_146) begin
              if (io_tagIn) begin
                fake_PC_1 <= _T_157;
              end
            end
          end
        end else begin
          if (_T_146) begin
            if (io_tagIn) begin
              fake_PC_1 <= _T_157;
            end
          end
        end
      end
    end
    if (reset) begin
      valid <= 1'h0;
    end else begin
      if (readIns) begin
        valid <= _T_123;
      end
    end
    if (reset) begin
      tag <= 1'h0;
    end else begin
      if (readIns) begin
        tag <= io_tagIn;
      end
    end
    if (reset) begin
      pc <= 64'h0;
    end else begin
      if (readIns) begin
        if (io_tagIn) begin
          pc <= fake_PC_1;
        end else begin
          pc <= fake_PC_0;
        end
      end
    end
    if (reset) begin
      instV_valid <= 1'h0;
    end else begin
      if (io_deq_ready) begin
        instV_valid <= 1'h0;
      end else begin
        if (_T_141) begin
          instV_valid <= 1'h1;
        end
      end
    end
    if (reset) begin
      instV_bits <= 32'h0;
    end else begin
      instV_bits <= _GEN_8[31:0];
    end
    _T_140 <= io_deq_ready;
  end
endmodule
