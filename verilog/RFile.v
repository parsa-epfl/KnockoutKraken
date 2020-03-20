module RFile(
  input         clock,
  input  [4:0]  io_rs1_addr,
  output [63:0] io_rs1_data,
  input  [4:0]  io_rs2_addr,
  output [63:0] io_rs2_data,
  input  [4:0]  io_w1_addr,
  input  [63:0] io_w1_data,
  input         io_w1_en,
  input  [4:0]  io_rw_addr,
  input  [63:0] io_rw_di,
  input         io_rw_wen,
  output [63:0] io_rw_do
);
  reg [63:0] regfile [0:31]; // @[PState.scala 88:20]
  reg [63:0] _RAND_0;
  wire [63:0] regfile__T_data; // @[PState.scala 88:20]
  wire [4:0] regfile__T_addr; // @[PState.scala 88:20]
  wire [63:0] regfile__T_1_data; // @[PState.scala 88:20]
  wire [4:0] regfile__T_1_addr; // @[PState.scala 88:20]
  wire [63:0] regfile__T_4_data; // @[PState.scala 88:20]
  wire [4:0] regfile__T_4_addr; // @[PState.scala 88:20]
  wire [63:0] regfile__T_2_data; // @[PState.scala 88:20]
  wire [4:0] regfile__T_2_addr; // @[PState.scala 88:20]
  wire  regfile__T_2_mask; // @[PState.scala 88:20]
  wire  regfile__T_2_en; // @[PState.scala 88:20]
  wire [63:0] regfile__T_3_data; // @[PState.scala 88:20]
  wire [4:0] regfile__T_3_addr; // @[PState.scala 88:20]
  wire  regfile__T_3_mask; // @[PState.scala 88:20]
  wire  regfile__T_3_en; // @[PState.scala 88:20]
  assign regfile__T_addr = io_rs1_addr;
  assign regfile__T_data = regfile[regfile__T_addr]; // @[PState.scala 88:20]
  assign regfile__T_1_addr = io_rs2_addr;
  assign regfile__T_1_data = regfile[regfile__T_1_addr]; // @[PState.scala 88:20]
  assign regfile__T_4_addr = io_rw_addr;
  assign regfile__T_4_data = regfile[regfile__T_4_addr]; // @[PState.scala 88:20]
  assign regfile__T_2_data = io_w1_data;
  assign regfile__T_2_addr = io_w1_addr;
  assign regfile__T_2_mask = 1'h1;
  assign regfile__T_2_en = io_w1_en;
  assign regfile__T_3_data = io_rw_di;
  assign regfile__T_3_addr = io_rw_addr;
  assign regfile__T_3_mask = 1'h1;
  assign regfile__T_3_en = io_rw_wen;
  assign io_rs1_data = regfile__T_data; // @[PState.scala 90:15]
  assign io_rs2_data = regfile__T_1_data; // @[PState.scala 91:15]
  assign io_rw_do = regfile__T_4_data; // @[PState.scala 101:12]
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
  _RAND_0 = {2{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 32; initvar = initvar+1)
    regfile[initvar] = _RAND_0[63:0];
  `endif // RANDOMIZE_MEM_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if(regfile__T_2_en & regfile__T_2_mask) begin
      regfile[regfile__T_2_addr] <= regfile__T_2_data; // @[PState.scala 88:20]
    end
    if(regfile__T_3_en & regfile__T_3_mask) begin
      regfile[regfile__T_3_addr] <= regfile__T_3_data; // @[PState.scala 88:20]
    end
  end
endmodule
