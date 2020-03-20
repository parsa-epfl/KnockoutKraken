module DataProc3S(
  input         clock,
  input         reset,
  input  [63:0] io_rVal1,
  input  [63:0] io_rVal2,
  input  [63:0] io_rVal3,
  output [63:0] io_res,
  input         io_is32bit
);
  wire  macc32_clock; // @[Execute.scala 393:22]
  wire  macc32_reset; // @[Execute.scala 393:22]
  wire [63:0] macc32_io_mult1; // @[Execute.scala 393:22]
  wire [63:0] macc32_io_mult2; // @[Execute.scala 393:22]
  wire [127:0] macc32_io_add; // @[Execute.scala 393:22]
  wire [128:0] macc32_io_res; // @[Execute.scala 393:22]
  wire [63:0] _T_8; // @[Execute.scala 396:51]
  wire [128:0] _T_10; // @[Execute.scala 397:16]
  MACC macc32 ( // @[Execute.scala 393:22]
    .clock(macc32_clock),
    .reset(macc32_reset),
    .io_mult1(macc32_io_mult1),
    .io_mult2(macc32_io_mult2),
    .io_add(macc32_io_add),
    .io_res(macc32_io_res)
  );
  assign _T_8 = {32'h0,io_rVal3[31:0]}; // @[Execute.scala 396:51]
  assign _T_10 = io_is32bit ? macc32_io_res : 129'h0; // @[Execute.scala 397:16]
  assign io_res = _T_10[63:0]; // @[Execute.scala 397:10]
  assign macc32_clock = clock;
  assign macc32_reset = reset;
  assign macc32_io_mult1 = {32'h0,io_rVal1[31:0]}; // @[Execute.scala 394:19]
  assign macc32_io_mult2 = {32'h0,io_rVal2[31:0]}; // @[Execute.scala 395:19]
  assign macc32_io_add = {{64{_T_8[63]}},_T_8}; // @[Execute.scala 396:17]
endmodule
