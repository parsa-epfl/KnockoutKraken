module MemArbiterData(
  input         clock,
  input         reset,
  output        io_selHost,
  output        io_selMem,
  input         io_commitEnq_ready,
  input         io_commitEnq_valid,
  input         io_commitEnq_bits_mem_valid,
  input  [63:0] io_commitEnq_bits_mem_bits_memReq_0_addr,
  input  [1:0]  io_commitDeq_bits_mem_bits_size,
  input         io_commitDeq_bits_mem_bits_isPair,
  input         io_commitDeq_bits_mem_bits_isLoad,
  input  [63:0] io_commitDeq_bits_mem_bits_memReq_0_addr,
  input  [4:0]  io_commitDeq_bits_mem_bits_memReq_0_reg,
  input  [63:0] io_commitDeq_bits_mem_bits_memReq_1_addr,
  input  [4:0]  io_commitDeq_bits_mem_bits_memReq_1_reg,
  input  [63:0] io_commitDeq_bits_mem_bits_rd_res,
  input         io_commitDeq_bits_mem_bits_rd_valid,
  input  [4:0]  io_commitDeq_bits_mem_bits_rd_bits,
  input         io_fillTLB_valid,
  input  [63:0] io_fillTLB_bits_vaddr,
  output [7:0]  io_memPort_WE,
  output [14:0] io_memPort_ADDR,
  output [63:0] io_memPort_DI,
  input  [63:0] io_memPort_DO,
  output        io_tlbPort_vaddr_valid,
  output [63:0] io_tlbPort_vaddr_bits,
  input  [63:0] io_tlbPort_paddr,
  input         io_tlbPort_miss_valid,
  input  [63:0] io_tlbPort_miss_bits_vaddr,
  input  [5:0]  io_tlbPort_miss_bits_tlbIdx,
  output [4:0]  io_rfile_rs1_addr,
  input  [63:0] io_rfile_rs1_data,
  output [4:0]  io_rfile_w1_addr,
  output [63:0] io_rfile_w1_data,
  output        io_rfile_w1_en,
  output [4:0]  io_rfile_rw_addr,
  output [63:0] io_rfile_rw_di,
  output        io_rfile_rw_wen,
  output        io_rfileWr,
  output        io_rfileRd,
  output        io_busy,
  output [63:0] io_reqMiss_bits_vaddr,
  output [5:0]  io_reqMiss_bits_tlbIdx,
  output        io_reqMiss_valid,
  output        io_unalignedExcp
);
  wire  dataAligner_io_currReq; // @[MemoryArbiter.scala 48:27]
  wire [1:0] dataAligner_io_minst_size; // @[MemoryArbiter.scala 48:27]
  wire  dataAligner_io_minst_isPair; // @[MemoryArbiter.scala 48:27]
  wire  dataAligner_io_minst_isLoad; // @[MemoryArbiter.scala 48:27]
  wire [63:0] dataAligner_io_minst_memReq_0_addr; // @[MemoryArbiter.scala 48:27]
  wire [63:0] dataAligner_io_minst_memReq_1_addr; // @[MemoryArbiter.scala 48:27]
  wire [63:0] dataAligner_io_data; // @[MemoryArbiter.scala 48:27]
  wire [63:0] dataAligner_io_aligned; // @[MemoryArbiter.scala 48:27]
  wire [7:0] dataAligner_io_byteEn; // @[MemoryArbiter.scala 48:27]
  wire  dataAligner_io_unalignedExcp; // @[MemoryArbiter.scala 48:27]
  reg [2:0] commitingStage; // @[MemoryArbiter.scala 40:31]
  reg [31:0] _RAND_0;
  reg [63:0] tlbpaddr; // @[MemoryArbiter.scala 44:25]
  reg [63:0] _RAND_1;
  reg [63:0] missTLB_vaddr; // @[MemoryArbiter.scala 45:24]
  reg [63:0] _RAND_2;
  reg [5:0] missTLB_tlbIdx; // @[MemoryArbiter.scala 45:24]
  reg [31:0] _RAND_3;
  wire  _T_4; // @[MemoryArbiter.scala 53:32]
  wire  _T_7; // @[MemoryArbiter.scala 64:31]
  wire  _T_11; // @[Conditional.scala 37:30]
  wire  _T_13; // @[Decoupled.scala 40:37]
  wire  _T_14; // @[MemoryArbiter.scala 87:54]
  wire  _T_16; // @[MemoryArbiter.scala 94:32]
  wire  _T_17; // @[Conditional.scala 37:30]
  wire  _T_18; // @[MemoryArbiter.scala 109:21]
  wire  _T_20; // @[MemoryArbiter.scala 132:32]
  wire [63:0] _GEN_5; // @[MemoryArbiter.scala 132:42]
  wire [63:0] _T_21; // @[MemoryArbiter.scala 136:29]
  wire  _GEN_6; // @[MemoryArbiter.scala 144:35]
  wire  _GEN_7; // @[MemoryArbiter.scala 144:35]
  wire [7:0] _GEN_8; // @[MemoryArbiter.scala 144:35]
  wire  _T_23; // @[Conditional.scala 37:30]
  wire  _T_25; // @[MemoryArbiter.scala 173:32]
  wire [63:0] _GEN_10; // @[MemoryArbiter.scala 173:42]
  wire  _T_26; // @[Conditional.scala 37:30]
  wire  _T_27; // @[MemoryArbiter.scala 182:54]
  wire  _T_28; // @[MemoryArbiter.scala 182:29]
  wire  _T_29; // @[Conditional.scala 37:30]
  wire  _GEN_24; // @[Conditional.scala 39:67]
  wire  _GEN_31; // @[Conditional.scala 39:67]
  wire  _GEN_34; // @[Conditional.scala 39:67]
  wire [63:0] _GEN_36; // @[Conditional.scala 39:67]
  wire  _GEN_37; // @[Conditional.scala 39:67]
  wire [7:0] _GEN_40; // @[Conditional.scala 39:67]
  wire [63:0] _GEN_41; // @[Conditional.scala 39:67]
  wire  _GEN_45; // @[Conditional.scala 39:67]
  wire  _GEN_46; // @[Conditional.scala 39:67]
  wire [63:0] _GEN_48; // @[Conditional.scala 39:67]
  wire  _GEN_49; // @[Conditional.scala 39:67]
  wire  _GEN_52; // @[Conditional.scala 39:67]
  wire  _GEN_55; // @[Conditional.scala 39:67]
  wire [7:0] _GEN_58; // @[Conditional.scala 39:67]
  wire [63:0] _GEN_59; // @[Conditional.scala 39:67]
  wire [63:0] _GEN_70; // @[Conditional.scala 40:58]
  DataAlignByte dataAligner ( // @[MemoryArbiter.scala 48:27]
    .io_currReq(dataAligner_io_currReq),
    .io_minst_size(dataAligner_io_minst_size),
    .io_minst_isPair(dataAligner_io_minst_isPair),
    .io_minst_isLoad(dataAligner_io_minst_isLoad),
    .io_minst_memReq_0_addr(dataAligner_io_minst_memReq_0_addr),
    .io_minst_memReq_1_addr(dataAligner_io_minst_memReq_1_addr),
    .io_data(dataAligner_io_data),
    .io_aligned(dataAligner_io_aligned),
    .io_byteEn(dataAligner_io_byteEn),
    .io_unalignedExcp(dataAligner_io_unalignedExcp)
  );
  assign _T_4 = commitingStage == 3'h3; // @[MemoryArbiter.scala 53:32]
  assign _T_7 = commitingStage == 3'h0; // @[MemoryArbiter.scala 64:31]
  assign _T_11 = 3'h0 == commitingStage; // @[Conditional.scala 37:30]
  assign _T_13 = io_commitEnq_ready & io_commitEnq_valid; // @[Decoupled.scala 40:37]
  assign _T_14 = io_commitEnq_bits_mem_valid & _T_13; // @[MemoryArbiter.scala 87:54]
  assign _T_16 = _T_13 & io_commitEnq_bits_mem_valid; // @[MemoryArbiter.scala 94:32]
  assign _T_17 = 3'h1 == commitingStage; // @[Conditional.scala 37:30]
  assign _T_18 = ~io_commitDeq_bits_mem_bits_isLoad; // @[MemoryArbiter.scala 109:21]
  assign _T_20 = io_commitDeq_bits_mem_bits_memReq_0_reg == 5'h1f; // @[MemoryArbiter.scala 132:32]
  assign _GEN_5 = _T_20 ? 64'h0 : dataAligner_io_aligned; // @[MemoryArbiter.scala 132:42]
  assign _T_21 = io_commitDeq_bits_mem_bits_isLoad ? io_tlbPort_paddr : tlbpaddr; // @[MemoryArbiter.scala 136:29]
  assign _GEN_6 = io_tlbPort_miss_valid ? 1'h0 : io_commitDeq_bits_mem_bits_isLoad; // @[MemoryArbiter.scala 144:35]
  assign _GEN_7 = io_tlbPort_miss_valid ? 1'h0 : io_commitDeq_bits_mem_bits_rd_valid; // @[MemoryArbiter.scala 144:35]
  assign _GEN_8 = io_tlbPort_miss_valid ? 8'h0 : dataAligner_io_byteEn; // @[MemoryArbiter.scala 144:35]
  assign _T_23 = 3'h2 == commitingStage; // @[Conditional.scala 37:30]
  assign _T_25 = io_commitDeq_bits_mem_bits_memReq_1_reg == 5'h1f; // @[MemoryArbiter.scala 173:32]
  assign _GEN_10 = _T_25 ? 64'h0 : dataAligner_io_aligned; // @[MemoryArbiter.scala 173:42]
  assign _T_26 = 3'h3 == commitingStage; // @[Conditional.scala 37:30]
  assign _T_27 = io_fillTLB_bits_vaddr == missTLB_vaddr; // @[MemoryArbiter.scala 182:54]
  assign _T_28 = io_fillTLB_valid & _T_27; // @[MemoryArbiter.scala 182:29]
  assign _T_29 = 3'h4 == commitingStage; // @[Conditional.scala 37:30]
  assign _GEN_24 = _T_26 ? 1'h0 : _T_29; // @[Conditional.scala 39:67]
  assign _GEN_31 = _T_23 & _T_18; // @[Conditional.scala 39:67]
  assign _GEN_34 = _T_23 ? 1'h0 : _GEN_24; // @[Conditional.scala 39:67]
  assign _GEN_36 = dataAligner_io_aligned; // @[Conditional.scala 39:67]
  assign _GEN_37 = _T_23 & io_commitDeq_bits_mem_bits_isLoad; // @[Conditional.scala 39:67]
  assign _GEN_40 = _T_23 ? dataAligner_io_byteEn : 8'h0; // @[Conditional.scala 39:67]
  assign _GEN_41 = _T_23 ? tlbpaddr : io_tlbPort_paddr; // @[Conditional.scala 39:67]
  assign _GEN_45 = _T_17 | _T_23; // @[Conditional.scala 39:67]
  assign _GEN_46 = _T_17 ? _T_18 : _GEN_31; // @[Conditional.scala 39:67]
  assign _GEN_48 = _T_17 ? io_commitDeq_bits_mem_bits_memReq_1_addr : io_commitDeq_bits_mem_bits_memReq_0_addr; // @[Conditional.scala 39:67]
  assign _GEN_49 = _T_17 ? io_commitDeq_bits_mem_bits_isPair : _GEN_34; // @[Conditional.scala 39:67]
  assign _GEN_52 = _T_17 & _GEN_7; // @[Conditional.scala 39:67]
  assign _GEN_55 = _T_17 ? _GEN_6 : _GEN_37; // @[Conditional.scala 39:67]
  assign _GEN_58 = _T_17 ? _GEN_8 : _GEN_40; // @[Conditional.scala 39:67]
  assign _GEN_59 = _T_17 ? _T_21 : _GEN_41; // @[Conditional.scala 39:67]
  assign _GEN_70 = _T_11 ? io_tlbPort_paddr : _GEN_59; // @[Conditional.scala 40:58]
  assign io_selHost = commitingStage == 3'h3; // @[MemoryArbiter.scala 53:14]
  assign io_selMem = ~_T_4; // @[MemoryArbiter.scala 54:13]
  assign io_memPort_WE = _T_11 ? 8'h0 : _GEN_58; // @[MemoryArbiter.scala 77:17 MemoryArbiter.scala 91:21 MemoryArbiter.scala 135:21 MemoryArbiter.scala 147:23 MemoryArbiter.scala 176:21 MemoryArbiter.scala 195:21]
  assign io_memPort_ADDR = _GEN_70[14:0]; // @[MemoryArbiter.scala 92:23 MemoryArbiter.scala 136:23 MemoryArbiter.scala 177:23 MemoryArbiter.scala 196:23]
  assign io_memPort_DI = _T_17 ? _GEN_5 : _GEN_10; // @[MemoryArbiter.scala 130:21 MemoryArbiter.scala 133:23 MemoryArbiter.scala 172:21 MemoryArbiter.scala 174:23]
  assign io_tlbPort_vaddr_valid = _T_11 ? _T_14 : _GEN_49; // @[MemoryArbiter.scala 58:26 MemoryArbiter.scala 87:30 MemoryArbiter.scala 114:30 MemoryArbiter.scala 161:30 MemoryArbiter.scala 191:30]
  assign io_tlbPort_vaddr_bits = _T_11 ? io_commitEnq_bits_mem_bits_memReq_0_addr : _GEN_48; // @[MemoryArbiter.scala 86:29 MemoryArbiter.scala 113:29 MemoryArbiter.scala 190:29]
  assign io_rfile_rs1_addr = _T_17 ? io_commitDeq_bits_mem_bits_memReq_0_reg : io_commitDeq_bits_mem_bits_memReq_1_reg; // @[MemoryArbiter.scala 127:25 MemoryArbiter.scala 169:25]
  assign io_rfile_w1_addr = _T_17 ? io_commitDeq_bits_mem_bits_memReq_0_reg : io_commitDeq_bits_mem_bits_memReq_1_reg; // @[MemoryArbiter.scala 122:24 MemoryArbiter.scala 164:24]
  assign io_rfile_w1_data = _T_17 ? dataAligner_io_aligned : _GEN_36; // @[MemoryArbiter.scala 123:24 MemoryArbiter.scala 165:24]
  assign io_rfile_w1_en = _T_11 ? 1'h0 : _GEN_55; // @[MemoryArbiter.scala 61:18 MemoryArbiter.scala 124:22 MemoryArbiter.scala 145:24 MemoryArbiter.scala 166:22]
  assign io_rfile_rw_addr = io_commitDeq_bits_mem_bits_rd_bits; // @[MemoryArbiter.scala 117:24]
  assign io_rfile_rw_di = io_commitDeq_bits_mem_bits_rd_res; // @[MemoryArbiter.scala 118:22]
  assign io_rfile_rw_wen = _T_11 ? 1'h0 : _GEN_52; // @[MemoryArbiter.scala 62:19 MemoryArbiter.scala 119:23 MemoryArbiter.scala 146:25]
  assign io_rfileWr = _T_11 ? 1'h0 : _GEN_45; // @[MemoryArbiter.scala 70:14 MemoryArbiter.scala 108:18 MemoryArbiter.scala 156:18]
  assign io_rfileRd = _T_11 ? 1'h0 : _GEN_46; // @[MemoryArbiter.scala 71:14 MemoryArbiter.scala 109:18 MemoryArbiter.scala 157:18]
  assign io_busy = ~_T_7; // @[MemoryArbiter.scala 64:11]
  assign io_reqMiss_bits_vaddr = missTLB_vaddr; // @[MemoryArbiter.scala 68:23]
  assign io_reqMiss_bits_tlbIdx = missTLB_tlbIdx; // @[MemoryArbiter.scala 68:23]
  assign io_reqMiss_valid = commitingStage == 3'h3; // @[MemoryArbiter.scala 66:20]
  assign io_unalignedExcp = dataAligner_io_unalignedExcp; // @[MemoryArbiter.scala 73:20]
  assign dataAligner_io_currReq = _T_17 ? 1'h0 : 1'h1; // @[MemoryArbiter.scala 106:30 MemoryArbiter.scala 154:30]
  assign dataAligner_io_minst_size = io_commitDeq_bits_mem_bits_size; // @[MemoryArbiter.scala 49:24]
  assign dataAligner_io_minst_isPair = io_commitDeq_bits_mem_bits_isPair; // @[MemoryArbiter.scala 49:24]
  assign dataAligner_io_minst_isLoad = io_commitDeq_bits_mem_bits_isLoad; // @[MemoryArbiter.scala 49:24]
  assign dataAligner_io_minst_memReq_0_addr = io_commitDeq_bits_mem_bits_memReq_0_addr; // @[MemoryArbiter.scala 49:24]
  assign dataAligner_io_minst_memReq_1_addr = io_commitDeq_bits_mem_bits_memReq_1_addr; // @[MemoryArbiter.scala 49:24]
  assign dataAligner_io_data = io_commitDeq_bits_mem_bits_isLoad ? io_memPort_DO : io_rfile_rs1_data; // @[MemoryArbiter.scala 51:23]
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
  commitingStage = _RAND_0[2:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {2{`RANDOM}};
  tlbpaddr = _RAND_1[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {2{`RANDOM}};
  missTLB_vaddr = _RAND_2[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_3 = {1{`RANDOM}};
  missTLB_tlbIdx = _RAND_3[5:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (reset) begin
      commitingStage <= 3'h0;
    end else if (_T_11) begin
      if (io_tlbPort_miss_valid) begin
        commitingStage <= 3'h3;
      end else if (_T_16) begin
        commitingStage <= 3'h1;
      end
    end else if (_T_17) begin
      if (io_tlbPort_miss_valid) begin
        commitingStage <= 3'h3;
      end else if (io_commitDeq_bits_mem_bits_isPair) begin
        commitingStage <= 3'h2;
      end else begin
        commitingStage <= 3'h0;
      end
    end else if (_T_23) begin
      commitingStage <= 3'h0;
    end else if (_T_26) begin
      if (_T_28) begin
        commitingStage <= 3'h4;
      end
    end else if (_T_29) begin
      if (io_tlbPort_miss_valid) begin
        commitingStage <= 3'h3;
      end else begin
        commitingStage <= 3'h1;
      end
    end
    if (reset) begin
      tlbpaddr <= 64'h0;
    end else if (_T_11) begin
      if (_T_16) begin
        tlbpaddr <= io_tlbPort_paddr;
      end
    end else if (_T_17) begin
      tlbpaddr <= io_tlbPort_paddr;
    end else if (!(_T_23)) begin
      if (!(_T_26)) begin
        if (_T_29) begin
          tlbpaddr <= io_tlbPort_paddr;
        end
      end
    end
    if (reset) begin
      missTLB_vaddr <= 64'h0;
    end else if (_T_11) begin
      if (io_tlbPort_miss_valid) begin
        missTLB_vaddr <= io_tlbPort_miss_bits_vaddr;
      end
    end else if (_T_17) begin
      if (io_tlbPort_miss_valid) begin
        missTLB_vaddr <= io_tlbPort_miss_bits_vaddr;
      end
    end
    if (reset) begin
      missTLB_tlbIdx <= 6'h0;
    end else if (_T_11) begin
      if (io_tlbPort_miss_valid) begin
        missTLB_tlbIdx <= io_tlbPort_miss_bits_tlbIdx;
      end
    end else if (_T_17) begin
      if (io_tlbPort_miss_valid) begin
        missTLB_tlbIdx <= io_tlbPort_miss_bits_tlbIdx;
      end
    end
  end
endmodule
