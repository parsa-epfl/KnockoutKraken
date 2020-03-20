module MACC(
  input          clock,
  input          reset,
  input  [63:0]  io_mult1,
  input  [63:0]  io_mult2,
  input  [127:0] io_add,
  output [128:0] io_res
);
  wire  MultAdd_3input_clk; // @[DSPModules.scala 14:28]
  wire  MultAdd_3input_rst; // @[DSPModules.scala 14:28]
  wire  MultAdd_3input_en; // @[DSPModules.scala 14:28]
  wire [63:0] MultAdd_3input_a; // @[DSPModules.scala 14:28]
  wire [63:0] MultAdd_3input_b; // @[DSPModules.scala 14:28]
  wire [127:0] MultAdd_3input_c; // @[DSPModules.scala 14:28]
  wire [128:0] MultAdd_3input_p; // @[DSPModules.scala 14:28]
  MultAdd_3input #(.AWIDTH(64), .BWIDTH(64), .CWIDTH(128), .PWIDTH(129)) MultAdd_3input ( // @[DSPModules.scala 14:28]
    .clk(MultAdd_3input_clk),
    .rst(MultAdd_3input_rst),
    .en(MultAdd_3input_en),
    .a(MultAdd_3input_a),
    .b(MultAdd_3input_b),
    .c(MultAdd_3input_c),
    .p(MultAdd_3input_p)
  );
  assign io_res = MultAdd_3input_p; // @[DSPModules.scala 21:13]
  assign MultAdd_3input_clk = clock; // @[DSPModules.scala 15:15]
  assign MultAdd_3input_rst = reset; // @[DSPModules.scala 16:15]
  assign MultAdd_3input_en = 1'h1; // @[DSPModules.scala 17:14]
  assign MultAdd_3input_a = io_mult1; // @[DSPModules.scala 18:13]
  assign MultAdd_3input_b = io_mult2; // @[DSPModules.scala 19:13]
  assign MultAdd_3input_c = io_add; // @[DSPModules.scala 20:13]
endmodule
