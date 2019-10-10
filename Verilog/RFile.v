module RFile( // @[:@439.2]
  input         clock, // @[:@440.4]
  input  [4:0]  io_rs1_addr, // @[:@442.4]
  output [63:0] io_rs1_data, // @[:@442.4]
  input  [4:0]  io_rs2_addr, // @[:@442.4]
  output [63:0] io_rs2_data, // @[:@442.4]
  input  [4:0]  io_waddr, // @[:@442.4]
  input  [63:0] io_wdata, // @[:@442.4]
  input         io_wen // @[:@442.4]
);
  reg [63:0] regfile [0:31]; // @[pstate.scala 81:20:@444.4]
  reg [63:0] _RAND_0;
  wire [63:0] regfile__T_22_data; // @[pstate.scala 81:20:@444.4]
  wire [4:0] regfile__T_22_addr; // @[pstate.scala 81:20:@444.4]
  wire [63:0] regfile__T_23_data; // @[pstate.scala 81:20:@444.4]
  wire [4:0] regfile__T_23_addr; // @[pstate.scala 81:20:@444.4]
  wire [63:0] regfile__T_21_data; // @[pstate.scala 81:20:@444.4]
  wire [4:0] regfile__T_21_addr; // @[pstate.scala 81:20:@444.4]
  wire  regfile__T_21_mask; // @[pstate.scala 81:20:@444.4]
  wire  regfile__T_21_en; // @[pstate.scala 81:20:@444.4]
  assign regfile__T_22_addr = io_rs1_addr;
  assign regfile__T_22_data = regfile[regfile__T_22_addr]; // @[pstate.scala 81:20:@444.4]
  assign regfile__T_23_addr = io_rs2_addr;
  assign regfile__T_23_data = regfile[regfile__T_23_addr]; // @[pstate.scala 81:20:@444.4]
  assign regfile__T_21_data = io_wdata;
  assign regfile__T_21_addr = io_waddr;
  assign regfile__T_21_mask = 1'h1;
  assign regfile__T_21_en = io_wen;
  assign io_rs1_data = regfile__T_22_data; // @[pstate.scala 88:15:@450.4]
  assign io_rs2_data = regfile__T_23_data; // @[pstate.scala 89:15:@452.4]
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
  _RAND_0 = {2{`RANDOM}};
  `ifdef RANDOMIZE_MEM_INIT
  for (initvar = 0; initvar < 32; initvar = initvar+1)
    regfile[initvar] = _RAND_0[63:0];
  `endif // RANDOMIZE_MEM_INIT
  end
`endif // RANDOMIZE
  always @(posedge clock) begin
    if(regfile__T_21_en & regfile__T_21_mask) begin
      regfile[regfile__T_21_addr] <= regfile__T_21_data; // @[pstate.scala 81:20:@444.4]
    end
  end
endmodule
