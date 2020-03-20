module MemArbiterInst(
  input         clock,
  input         reset,
  input         io_vaddr_valid,
  input  [63:0] io_vaddr_bits,
  output        io_selHost,
  output        io_selMem,
  input         io_fillTLB_valid,
  input  [63:0] io_fillTLB_bits_vaddr,
  output [14:0] io_memPort_ADDR,
  output        io_tlbPort_vaddr_valid,
  output [63:0] io_tlbPort_vaddr_bits,
  input  [63:0] io_tlbPort_paddr,
  input         io_tlbPort_miss_valid,
  input  [63:0] io_tlbPort_miss_bits_vaddr,
  input  [5:0]  io_tlbPort_miss_bits_tlbIdx,
  output        io_reqMiss_valid,
  output [63:0] io_reqMiss_bits_vaddr,
  output [5:0]  io_reqMiss_bits_tlbIdx
);
  reg  fetchStage; // @[MemoryArbiter.scala 230:27]
  reg [31:0] _RAND_0;
  reg [63:0] missTLB_vaddr; // @[MemoryArbiter.scala 232:24]
  reg [63:0] _RAND_1;
  reg [5:0] missTLB_tlbIdx; // @[MemoryArbiter.scala 232:24]
  reg [31:0] _RAND_2;
  wire  _T_3; // @[MemoryArbiter.scala 235:16]
  wire  _GEN_0; // @[MemoryArbiter.scala 251:35]
  wire  _T_9; // @[MemoryArbiter.scala 258:54]
  wire  _T_10; // @[MemoryArbiter.scala 258:29]
  assign _T_3 = ~fetchStage; // @[MemoryArbiter.scala 235:16]
  assign _GEN_0 = io_tlbPort_miss_valid | fetchStage; // @[MemoryArbiter.scala 251:35]
  assign _T_9 = io_fillTLB_bits_vaddr == missTLB_vaddr; // @[MemoryArbiter.scala 258:54]
  assign _T_10 = io_fillTLB_valid & _T_9; // @[MemoryArbiter.scala 258:29]
  assign io_selHost = fetchStage; // @[MemoryArbiter.scala 234:14]
  assign io_selMem = ~fetchStage; // @[MemoryArbiter.scala 235:13]
  assign io_memPort_ADDR = io_tlbPort_paddr[14:0]; // @[MemoryArbiter.scala 247:19]
  assign io_tlbPort_vaddr_valid = io_vaddr_valid; // @[MemoryArbiter.scala 238:20]
  assign io_tlbPort_vaddr_bits = io_vaddr_bits; // @[MemoryArbiter.scala 238:20]
  assign io_reqMiss_valid = fetchStage; // @[MemoryArbiter.scala 241:20]
  assign io_reqMiss_bits_vaddr = missTLB_vaddr; // @[MemoryArbiter.scala 242:19]
  assign io_reqMiss_bits_tlbIdx = missTLB_tlbIdx; // @[MemoryArbiter.scala 242:19]
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
  fetchStage = _RAND_0[0:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {2{`RANDOM}};
  missTLB_vaddr = _RAND_1[63:0];
  `endif // RANDOMIZE_REG_INIT
  `ifdef RANDOMIZE_REG_INIT
  _RAND_2 = {1{`RANDOM}};
  missTLB_tlbIdx = _RAND_2[5:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (reset) begin
      fetchStage <= 1'h0;
    end else if (_T_3) begin
      fetchStage <= _GEN_0;
    end else if (fetchStage) begin
      if (_T_10) begin
        fetchStage <= 1'h0;
      end
    end
    if (reset) begin
      missTLB_vaddr <= 64'h0;
    end else if (_T_3) begin
      if (io_tlbPort_miss_valid) begin
        missTLB_vaddr <= io_tlbPort_miss_bits_vaddr;
      end
    end
    if (reset) begin
      missTLB_tlbIdx <= 6'h0;
    end else if (_T_3) begin
      if (io_tlbPort_miss_valid) begin
        missTLB_tlbIdx <= io_tlbPort_miss_bits_tlbIdx;
      end
    end
  end
endmodule
