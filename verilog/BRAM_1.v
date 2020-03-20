module BRAM_1(
  input         clock,
  input         reset,
  input         portA_EN,
  input  [7:0]  portA_WE,
  input  [8:0]  portA_ADDR,
  input  [63:0] portA_DI,
  output [63:0] portA_DO,
  input  [7:0]  portB_WE,
  input  [8:0]  portB_ADDR,
  input  [63:0] portB_DI,
  output [63:0] portB_DO
);
  wire  BRAMTDP_clk; // @[BramModules.scala 217:31]
  wire  BRAMTDP_rst; // @[BramModules.scala 217:31]
  wire [8:0] BRAMTDP_addra; // @[BramModules.scala 217:31]
  wire [8:0] BRAMTDP_addrb; // @[BramModules.scala 217:31]
  wire [63:0] BRAMTDP_dina; // @[BramModules.scala 217:31]
  wire [63:0] BRAMTDP_dinb; // @[BramModules.scala 217:31]
  wire [7:0] BRAMTDP_wea; // @[BramModules.scala 217:31]
  wire [7:0] BRAMTDP_web; // @[BramModules.scala 217:31]
  wire  BRAMTDP_ena; // @[BramModules.scala 217:31]
  wire  BRAMTDP_enb; // @[BramModules.scala 217:31]
  wire  BRAMTDP_regcea; // @[BramModules.scala 217:31]
  wire  BRAMTDP_regceb; // @[BramModules.scala 217:31]
  wire [63:0] BRAMTDP_douta; // @[BramModules.scala 217:31]
  wire [63:0] BRAMTDP_doutb; // @[BramModules.scala 217:31]
  BRAMTDP #(.NB_COL(8), .COL_WIDTH(8), .RAM_DEPTH(512), .RAM_PERFORMANCE("LOW_LATENCY"), .INIT_FILE("")) BRAMTDP ( // @[BramModules.scala 217:31]
    .clk(BRAMTDP_clk),
    .rst(BRAMTDP_rst),
    .addra(BRAMTDP_addra),
    .addrb(BRAMTDP_addrb),
    .dina(BRAMTDP_dina),
    .dinb(BRAMTDP_dinb),
    .wea(BRAMTDP_wea),
    .web(BRAMTDP_web),
    .ena(BRAMTDP_ena),
    .enb(BRAMTDP_enb),
    .regcea(BRAMTDP_regcea),
    .regceb(BRAMTDP_regceb),
    .douta(BRAMTDP_douta),
    .doutb(BRAMTDP_doutb)
  );
  assign portA_DO = BRAMTDP_douta; // @[BramModules.scala 227:20]
  assign portB_DO = BRAMTDP_doutb; // @[BramModules.scala 234:20]
  assign BRAMTDP_clk = clock; // @[BramModules.scala 219:18]
  assign BRAMTDP_rst = reset; // @[BramModules.scala 220:18]
  assign BRAMTDP_addra = portA_ADDR; // @[BramModules.scala 222:20]
  assign BRAMTDP_addrb = portB_ADDR; // @[BramModules.scala 229:20]
  assign BRAMTDP_dina = portA_DI; // @[BramModules.scala 223:19]
  assign BRAMTDP_dinb = portB_DI; // @[BramModules.scala 230:19]
  assign BRAMTDP_wea = portA_WE; // @[BramModules.scala 224:18]
  assign BRAMTDP_web = portB_WE; // @[BramModules.scala 231:18]
  assign BRAMTDP_ena = portA_EN; // @[BramModules.scala 225:18]
  assign BRAMTDP_enb = 1'h1; // @[BramModules.scala 232:18]
  assign BRAMTDP_regcea = 1'h1; // @[BramModules.scala 226:21]
  assign BRAMTDP_regceb = 1'h1; // @[BramModules.scala 233:21]
endmodule
