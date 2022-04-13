//Copyright 1986-2021 Xilinx, Inc. All Rights Reserved.
//--------------------------------------------------------------------------------
//Tool Version: Vivado v.2021.1 (lin64) Build 3247384 Thu Jun 10 19:36:07 MDT 2021
//Date        : Mon Apr 11 00:18:24 2022
//Host        : ulises-OptiPlex-7060 running 64-bit Ubuntu 20.04.4 LTS
//Command     : generate_target cl_xbar.bd
//Design      : cl_xbar
//Purpose     : IP block netlist
//--------------------------------------------------------------------------------
`timescale 1 ps / 1 ps

(* CORE_GENERATION_INFO = "cl_xbar,IP_Integrator,{x_ipVendor=xilinx.com,x_ipLibrary=BlockDiagram,x_ipName=cl_xbar,x_ipVersion=1.00.a,x_ipLanguage=VERILOG,numBlks=22,numReposBlks=11,numNonXlnxBlks=0,numHierBlks=11,maxHierDepth=0,numSysgenBlks=0,numHlsBlks=0,numHdlrefBlks=0,numPkgbdBlks=0,bdsource=USER,synth_mode=OOC_per_BD}" *) (* HW_HANDOFF = "cl_xbar.hwdef" *) 
module cl_xbar
   (ACLK,
    ARESETN,
    M_00G_16G_AXI_araddr,
    M_00G_16G_AXI_arburst,
    M_00G_16G_AXI_arcache,
    M_00G_16G_AXI_arid,
    M_00G_16G_AXI_arlen,
    M_00G_16G_AXI_arlock,
    M_00G_16G_AXI_arprot,
    M_00G_16G_AXI_arqos,
    M_00G_16G_AXI_arready,
    M_00G_16G_AXI_arregion,
    M_00G_16G_AXI_arsize,
    M_00G_16G_AXI_arvalid,
    M_00G_16G_AXI_awaddr,
    M_00G_16G_AXI_awburst,
    M_00G_16G_AXI_awcache,
    M_00G_16G_AXI_awid,
    M_00G_16G_AXI_awlen,
    M_00G_16G_AXI_awlock,
    M_00G_16G_AXI_awprot,
    M_00G_16G_AXI_awqos,
    M_00G_16G_AXI_awready,
    M_00G_16G_AXI_awregion,
    M_00G_16G_AXI_awsize,
    M_00G_16G_AXI_awvalid,
    M_00G_16G_AXI_bid,
    M_00G_16G_AXI_bready,
    M_00G_16G_AXI_bresp,
    M_00G_16G_AXI_bvalid,
    M_00G_16G_AXI_rdata,
    M_00G_16G_AXI_rid,
    M_00G_16G_AXI_rlast,
    M_00G_16G_AXI_rready,
    M_00G_16G_AXI_rresp,
    M_00G_16G_AXI_rvalid,
    M_00G_16G_AXI_wdata,
    M_00G_16G_AXI_wlast,
    M_00G_16G_AXI_wready,
    M_00G_16G_AXI_wstrb,
    M_00G_16G_AXI_wvalid,
    M_16G_32G_AXI_araddr,
    M_16G_32G_AXI_arburst,
    M_16G_32G_AXI_arcache,
    M_16G_32G_AXI_arid,
    M_16G_32G_AXI_arlen,
    M_16G_32G_AXI_arlock,
    M_16G_32G_AXI_arprot,
    M_16G_32G_AXI_arqos,
    M_16G_32G_AXI_arready,
    M_16G_32G_AXI_arregion,
    M_16G_32G_AXI_arsize,
    M_16G_32G_AXI_arvalid,
    M_16G_32G_AXI_awaddr,
    M_16G_32G_AXI_awburst,
    M_16G_32G_AXI_awcache,
    M_16G_32G_AXI_awid,
    M_16G_32G_AXI_awlen,
    M_16G_32G_AXI_awlock,
    M_16G_32G_AXI_awprot,
    M_16G_32G_AXI_awqos,
    M_16G_32G_AXI_awready,
    M_16G_32G_AXI_awregion,
    M_16G_32G_AXI_awsize,
    M_16G_32G_AXI_awvalid,
    M_16G_32G_AXI_bid,
    M_16G_32G_AXI_bready,
    M_16G_32G_AXI_bresp,
    M_16G_32G_AXI_bvalid,
    M_16G_32G_AXI_rdata,
    M_16G_32G_AXI_rid,
    M_16G_32G_AXI_rlast,
    M_16G_32G_AXI_rready,
    M_16G_32G_AXI_rresp,
    M_16G_32G_AXI_rvalid,
    M_16G_32G_AXI_wdata,
    M_16G_32G_AXI_wlast,
    M_16G_32G_AXI_wready,
    M_16G_32G_AXI_wstrb,
    M_16G_32G_AXI_wvalid,
    M_32G_48G_AXI_araddr,
    M_32G_48G_AXI_arburst,
    M_32G_48G_AXI_arcache,
    M_32G_48G_AXI_arid,
    M_32G_48G_AXI_arlen,
    M_32G_48G_AXI_arlock,
    M_32G_48G_AXI_arprot,
    M_32G_48G_AXI_arqos,
    M_32G_48G_AXI_arready,
    M_32G_48G_AXI_arregion,
    M_32G_48G_AXI_arsize,
    M_32G_48G_AXI_arvalid,
    M_32G_48G_AXI_awaddr,
    M_32G_48G_AXI_awburst,
    M_32G_48G_AXI_awcache,
    M_32G_48G_AXI_awid,
    M_32G_48G_AXI_awlen,
    M_32G_48G_AXI_awlock,
    M_32G_48G_AXI_awprot,
    M_32G_48G_AXI_awqos,
    M_32G_48G_AXI_awready,
    M_32G_48G_AXI_awregion,
    M_32G_48G_AXI_awsize,
    M_32G_48G_AXI_awvalid,
    M_32G_48G_AXI_bid,
    M_32G_48G_AXI_bready,
    M_32G_48G_AXI_bresp,
    M_32G_48G_AXI_bvalid,
    M_32G_48G_AXI_rdata,
    M_32G_48G_AXI_rid,
    M_32G_48G_AXI_rlast,
    M_32G_48G_AXI_rready,
    M_32G_48G_AXI_rresp,
    M_32G_48G_AXI_rvalid,
    M_32G_48G_AXI_wdata,
    M_32G_48G_AXI_wlast,
    M_32G_48G_AXI_wready,
    M_32G_48G_AXI_wstrb,
    M_32G_48G_AXI_wvalid,
    M_48G_64G_AXI_araddr,
    M_48G_64G_AXI_arburst,
    M_48G_64G_AXI_arcache,
    M_48G_64G_AXI_arid,
    M_48G_64G_AXI_arlen,
    M_48G_64G_AXI_arlock,
    M_48G_64G_AXI_arprot,
    M_48G_64G_AXI_arqos,
    M_48G_64G_AXI_arready,
    M_48G_64G_AXI_arregion,
    M_48G_64G_AXI_arsize,
    M_48G_64G_AXI_arvalid,
    M_48G_64G_AXI_awaddr,
    M_48G_64G_AXI_awburst,
    M_48G_64G_AXI_awcache,
    M_48G_64G_AXI_awid,
    M_48G_64G_AXI_awlen,
    M_48G_64G_AXI_awlock,
    M_48G_64G_AXI_awprot,
    M_48G_64G_AXI_awqos,
    M_48G_64G_AXI_awready,
    M_48G_64G_AXI_awregion,
    M_48G_64G_AXI_awsize,
    M_48G_64G_AXI_awvalid,
    M_48G_64G_AXI_bid,
    M_48G_64G_AXI_bready,
    M_48G_64G_AXI_bresp,
    M_48G_64G_AXI_bvalid,
    M_48G_64G_AXI_rdata,
    M_48G_64G_AXI_rid,
    M_48G_64G_AXI_rlast,
    M_48G_64G_AXI_rready,
    M_48G_64G_AXI_rresp,
    M_48G_64G_AXI_rvalid,
    M_48G_64G_AXI_wdata,
    M_48G_64G_AXI_wlast,
    M_48G_64G_AXI_wready,
    M_48G_64G_AXI_wstrb,
    M_48G_64G_AXI_wvalid,
    M_AXI_RTL_araddr,
    M_AXI_RTL_arburst,
    M_AXI_RTL_arcache,
    M_AXI_RTL_arid,
    M_AXI_RTL_arlen,
    M_AXI_RTL_arlock,
    M_AXI_RTL_arprot,
    M_AXI_RTL_arqos,
    M_AXI_RTL_arready,
    M_AXI_RTL_arregion,
    M_AXI_RTL_arsize,
    M_AXI_RTL_arvalid,
    M_AXI_RTL_awaddr,
    M_AXI_RTL_awburst,
    M_AXI_RTL_awcache,
    M_AXI_RTL_awid,
    M_AXI_RTL_awlen,
    M_AXI_RTL_awlock,
    M_AXI_RTL_awprot,
    M_AXI_RTL_awqos,
    M_AXI_RTL_awready,
    M_AXI_RTL_awregion,
    M_AXI_RTL_awsize,
    M_AXI_RTL_awvalid,
    M_AXI_RTL_bid,
    M_AXI_RTL_bready,
    M_AXI_RTL_bresp,
    M_AXI_RTL_bvalid,
    M_AXI_RTL_rdata,
    M_AXI_RTL_rid,
    M_AXI_RTL_rlast,
    M_AXI_RTL_rready,
    M_AXI_RTL_rresp,
    M_AXI_RTL_rvalid,
    M_AXI_RTL_wdata,
    M_AXI_RTL_wlast,
    M_AXI_RTL_wready,
    M_AXI_RTL_wstrb,
    M_AXI_RTL_wvalid,
    S_AXI_PCIS_araddr,
    S_AXI_PCIS_arburst,
    S_AXI_PCIS_arcache,
    S_AXI_PCIS_arid,
    S_AXI_PCIS_arlen,
    S_AXI_PCIS_arlock,
    S_AXI_PCIS_arprot,
    S_AXI_PCIS_arqos,
    S_AXI_PCIS_arready,
    S_AXI_PCIS_arregion,
    S_AXI_PCIS_arsize,
    S_AXI_PCIS_arvalid,
    S_AXI_PCIS_awaddr,
    S_AXI_PCIS_awburst,
    S_AXI_PCIS_awcache,
    S_AXI_PCIS_awid,
    S_AXI_PCIS_awlen,
    S_AXI_PCIS_awlock,
    S_AXI_PCIS_awprot,
    S_AXI_PCIS_awqos,
    S_AXI_PCIS_awready,
    S_AXI_PCIS_awregion,
    S_AXI_PCIS_awsize,
    S_AXI_PCIS_awvalid,
    S_AXI_PCIS_bid,
    S_AXI_PCIS_bready,
    S_AXI_PCIS_bresp,
    S_AXI_PCIS_bvalid,
    S_AXI_PCIS_rdata,
    S_AXI_PCIS_rid,
    S_AXI_PCIS_rlast,
    S_AXI_PCIS_rready,
    S_AXI_PCIS_rresp,
    S_AXI_PCIS_rvalid,
    S_AXI_PCIS_wdata,
    S_AXI_PCIS_wlast,
    S_AXI_PCIS_wready,
    S_AXI_PCIS_wstrb,
    S_AXI_PCIS_wvalid,
    S_RTL_DRAM_AXI_araddr,
    S_RTL_DRAM_AXI_arburst,
    S_RTL_DRAM_AXI_arcache,
    S_RTL_DRAM_AXI_arid,
    S_RTL_DRAM_AXI_arlen,
    S_RTL_DRAM_AXI_arlock,
    S_RTL_DRAM_AXI_arprot,
    S_RTL_DRAM_AXI_arqos,
    S_RTL_DRAM_AXI_arready,
    S_RTL_DRAM_AXI_arregion,
    S_RTL_DRAM_AXI_arsize,
    S_RTL_DRAM_AXI_arvalid,
    S_RTL_DRAM_AXI_awaddr,
    S_RTL_DRAM_AXI_awburst,
    S_RTL_DRAM_AXI_awcache,
    S_RTL_DRAM_AXI_awid,
    S_RTL_DRAM_AXI_awlen,
    S_RTL_DRAM_AXI_awlock,
    S_RTL_DRAM_AXI_awprot,
    S_RTL_DRAM_AXI_awqos,
    S_RTL_DRAM_AXI_awready,
    S_RTL_DRAM_AXI_awregion,
    S_RTL_DRAM_AXI_awsize,
    S_RTL_DRAM_AXI_awvalid,
    S_RTL_DRAM_AXI_bid,
    S_RTL_DRAM_AXI_bready,
    S_RTL_DRAM_AXI_bresp,
    S_RTL_DRAM_AXI_bvalid,
    S_RTL_DRAM_AXI_rdata,
    S_RTL_DRAM_AXI_rid,
    S_RTL_DRAM_AXI_rlast,
    S_RTL_DRAM_AXI_rready,
    S_RTL_DRAM_AXI_rresp,
    S_RTL_DRAM_AXI_rvalid,
    S_RTL_DRAM_AXI_wdata,
    S_RTL_DRAM_AXI_wlast,
    S_RTL_DRAM_AXI_wready,
    S_RTL_DRAM_AXI_wstrb,
    S_RTL_DRAM_AXI_wvalid);
  (* X_INTERFACE_INFO = "xilinx.com:signal:clock:1.0 CLK.ACLK CLK" *) (* X_INTERFACE_PARAMETER = "XIL_INTERFACENAME CLK.ACLK, ASSOCIATED_BUSIF S_RTL_DRAM_AXI:M_00G_16G_AXI:M_16G_32G_AXI:M_32G_48G_AXI:M_48G_64G_AXI:M_AXI_RTL:S_AXI_PCIS, ASSOCIATED_RESET ARESETN, CLK_DOMAIN cl_xbar_ACLK, FREQ_HZ 250000000, FREQ_TOLERANCE_HZ 0, INSERT_VIP 0, PHASE 0.0" *) input ACLK;
  (* X_INTERFACE_INFO = "xilinx.com:signal:reset:1.0 RST.ARESETN RST" *) (* X_INTERFACE_PARAMETER = "XIL_INTERFACENAME RST.ARESETN, INSERT_VIP 0, POLARITY ACTIVE_LOW" *) input ARESETN;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI ARADDR" *) (* X_INTERFACE_PARAMETER = "XIL_INTERFACENAME M_00G_16G_AXI, ADDR_WIDTH 64, ARUSER_WIDTH 0, AWUSER_WIDTH 0, BUSER_WIDTH 0, CLK_DOMAIN cl_xbar_ACLK, DATA_WIDTH 512, FREQ_HZ 250000000, HAS_BRESP 1, HAS_BURST 1, HAS_CACHE 1, HAS_LOCK 1, HAS_PROT 1, HAS_QOS 1, HAS_REGION 1, HAS_RRESP 1, HAS_WSTRB 1, ID_WIDTH 7, INSERT_VIP 0, MAX_BURST_LENGTH 256, NUM_READ_OUTSTANDING 16, NUM_READ_THREADS 1, NUM_WRITE_OUTSTANDING 16, NUM_WRITE_THREADS 1, PHASE 0.0, PROTOCOL AXI4, READ_WRITE_MODE READ_WRITE, RUSER_BITS_PER_BYTE 0, RUSER_WIDTH 0, SUPPORTS_NARROW_BURST 1, WUSER_BITS_PER_BYTE 0, WUSER_WIDTH 0" *) output [63:0]M_00G_16G_AXI_araddr;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI ARBURST" *) output [1:0]M_00G_16G_AXI_arburst;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI ARCACHE" *) output [3:0]M_00G_16G_AXI_arcache;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI ARID" *) output [6:0]M_00G_16G_AXI_arid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI ARLEN" *) output [7:0]M_00G_16G_AXI_arlen;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI ARLOCK" *) output [0:0]M_00G_16G_AXI_arlock;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI ARPROT" *) output [2:0]M_00G_16G_AXI_arprot;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI ARQOS" *) output [3:0]M_00G_16G_AXI_arqos;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI ARREADY" *) input M_00G_16G_AXI_arready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI ARREGION" *) output [3:0]M_00G_16G_AXI_arregion;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI ARSIZE" *) output [2:0]M_00G_16G_AXI_arsize;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI ARVALID" *) output M_00G_16G_AXI_arvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI AWADDR" *) output [63:0]M_00G_16G_AXI_awaddr;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI AWBURST" *) output [1:0]M_00G_16G_AXI_awburst;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI AWCACHE" *) output [3:0]M_00G_16G_AXI_awcache;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI AWID" *) output [6:0]M_00G_16G_AXI_awid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI AWLEN" *) output [7:0]M_00G_16G_AXI_awlen;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI AWLOCK" *) output [0:0]M_00G_16G_AXI_awlock;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI AWPROT" *) output [2:0]M_00G_16G_AXI_awprot;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI AWQOS" *) output [3:0]M_00G_16G_AXI_awqos;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI AWREADY" *) input M_00G_16G_AXI_awready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI AWREGION" *) output [3:0]M_00G_16G_AXI_awregion;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI AWSIZE" *) output [2:0]M_00G_16G_AXI_awsize;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI AWVALID" *) output M_00G_16G_AXI_awvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI BID" *) input [6:0]M_00G_16G_AXI_bid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI BREADY" *) output M_00G_16G_AXI_bready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI BRESP" *) input [1:0]M_00G_16G_AXI_bresp;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI BVALID" *) input M_00G_16G_AXI_bvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI RDATA" *) input [511:0]M_00G_16G_AXI_rdata;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI RID" *) input [6:0]M_00G_16G_AXI_rid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI RLAST" *) input M_00G_16G_AXI_rlast;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI RREADY" *) output M_00G_16G_AXI_rready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI RRESP" *) input [1:0]M_00G_16G_AXI_rresp;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI RVALID" *) input M_00G_16G_AXI_rvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI WDATA" *) output [511:0]M_00G_16G_AXI_wdata;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI WLAST" *) output M_00G_16G_AXI_wlast;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI WREADY" *) input M_00G_16G_AXI_wready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI WSTRB" *) output [63:0]M_00G_16G_AXI_wstrb;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_00G_16G_AXI WVALID" *) output M_00G_16G_AXI_wvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI ARADDR" *) (* X_INTERFACE_PARAMETER = "XIL_INTERFACENAME M_16G_32G_AXI, ADDR_WIDTH 64, ARUSER_WIDTH 0, AWUSER_WIDTH 0, BUSER_WIDTH 0, CLK_DOMAIN cl_xbar_ACLK, DATA_WIDTH 512, FREQ_HZ 250000000, HAS_BRESP 1, HAS_BURST 1, HAS_CACHE 1, HAS_LOCK 1, HAS_PROT 1, HAS_QOS 1, HAS_REGION 1, HAS_RRESP 1, HAS_WSTRB 1, ID_WIDTH 7, INSERT_VIP 0, MAX_BURST_LENGTH 256, NUM_READ_OUTSTANDING 16, NUM_READ_THREADS 1, NUM_WRITE_OUTSTANDING 16, NUM_WRITE_THREADS 1, PHASE 0.0, PROTOCOL AXI4, READ_WRITE_MODE READ_WRITE, RUSER_BITS_PER_BYTE 0, RUSER_WIDTH 0, SUPPORTS_NARROW_BURST 1, WUSER_BITS_PER_BYTE 0, WUSER_WIDTH 0" *) output [63:0]M_16G_32G_AXI_araddr;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI ARBURST" *) output [1:0]M_16G_32G_AXI_arburst;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI ARCACHE" *) output [3:0]M_16G_32G_AXI_arcache;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI ARID" *) output [6:0]M_16G_32G_AXI_arid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI ARLEN" *) output [7:0]M_16G_32G_AXI_arlen;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI ARLOCK" *) output [0:0]M_16G_32G_AXI_arlock;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI ARPROT" *) output [2:0]M_16G_32G_AXI_arprot;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI ARQOS" *) output [3:0]M_16G_32G_AXI_arqos;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI ARREADY" *) input M_16G_32G_AXI_arready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI ARREGION" *) output [3:0]M_16G_32G_AXI_arregion;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI ARSIZE" *) output [2:0]M_16G_32G_AXI_arsize;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI ARVALID" *) output M_16G_32G_AXI_arvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI AWADDR" *) output [63:0]M_16G_32G_AXI_awaddr;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI AWBURST" *) output [1:0]M_16G_32G_AXI_awburst;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI AWCACHE" *) output [3:0]M_16G_32G_AXI_awcache;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI AWID" *) output [6:0]M_16G_32G_AXI_awid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI AWLEN" *) output [7:0]M_16G_32G_AXI_awlen;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI AWLOCK" *) output [0:0]M_16G_32G_AXI_awlock;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI AWPROT" *) output [2:0]M_16G_32G_AXI_awprot;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI AWQOS" *) output [3:0]M_16G_32G_AXI_awqos;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI AWREADY" *) input M_16G_32G_AXI_awready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI AWREGION" *) output [3:0]M_16G_32G_AXI_awregion;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI AWSIZE" *) output [2:0]M_16G_32G_AXI_awsize;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI AWVALID" *) output M_16G_32G_AXI_awvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI BID" *) input [6:0]M_16G_32G_AXI_bid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI BREADY" *) output M_16G_32G_AXI_bready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI BRESP" *) input [1:0]M_16G_32G_AXI_bresp;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI BVALID" *) input M_16G_32G_AXI_bvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI RDATA" *) input [511:0]M_16G_32G_AXI_rdata;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI RID" *) input [6:0]M_16G_32G_AXI_rid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI RLAST" *) input M_16G_32G_AXI_rlast;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI RREADY" *) output M_16G_32G_AXI_rready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI RRESP" *) input [1:0]M_16G_32G_AXI_rresp;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI RVALID" *) input M_16G_32G_AXI_rvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI WDATA" *) output [511:0]M_16G_32G_AXI_wdata;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI WLAST" *) output M_16G_32G_AXI_wlast;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI WREADY" *) input M_16G_32G_AXI_wready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI WSTRB" *) output [63:0]M_16G_32G_AXI_wstrb;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_16G_32G_AXI WVALID" *) output M_16G_32G_AXI_wvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI ARADDR" *) (* X_INTERFACE_PARAMETER = "XIL_INTERFACENAME M_32G_48G_AXI, ADDR_WIDTH 64, ARUSER_WIDTH 0, AWUSER_WIDTH 0, BUSER_WIDTH 0, CLK_DOMAIN cl_xbar_ACLK, DATA_WIDTH 512, FREQ_HZ 250000000, HAS_BRESP 1, HAS_BURST 1, HAS_CACHE 1, HAS_LOCK 1, HAS_PROT 1, HAS_QOS 1, HAS_REGION 1, HAS_RRESP 1, HAS_WSTRB 1, ID_WIDTH 7, INSERT_VIP 0, MAX_BURST_LENGTH 256, NUM_READ_OUTSTANDING 16, NUM_READ_THREADS 1, NUM_WRITE_OUTSTANDING 16, NUM_WRITE_THREADS 1, PHASE 0.0, PROTOCOL AXI4, READ_WRITE_MODE READ_WRITE, RUSER_BITS_PER_BYTE 0, RUSER_WIDTH 0, SUPPORTS_NARROW_BURST 1, WUSER_BITS_PER_BYTE 0, WUSER_WIDTH 0" *) output [63:0]M_32G_48G_AXI_araddr;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI ARBURST" *) output [1:0]M_32G_48G_AXI_arburst;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI ARCACHE" *) output [3:0]M_32G_48G_AXI_arcache;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI ARID" *) output [6:0]M_32G_48G_AXI_arid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI ARLEN" *) output [7:0]M_32G_48G_AXI_arlen;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI ARLOCK" *) output [0:0]M_32G_48G_AXI_arlock;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI ARPROT" *) output [2:0]M_32G_48G_AXI_arprot;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI ARQOS" *) output [3:0]M_32G_48G_AXI_arqos;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI ARREADY" *) input M_32G_48G_AXI_arready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI ARREGION" *) output [3:0]M_32G_48G_AXI_arregion;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI ARSIZE" *) output [2:0]M_32G_48G_AXI_arsize;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI ARVALID" *) output M_32G_48G_AXI_arvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI AWADDR" *) output [63:0]M_32G_48G_AXI_awaddr;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI AWBURST" *) output [1:0]M_32G_48G_AXI_awburst;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI AWCACHE" *) output [3:0]M_32G_48G_AXI_awcache;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI AWID" *) output [6:0]M_32G_48G_AXI_awid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI AWLEN" *) output [7:0]M_32G_48G_AXI_awlen;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI AWLOCK" *) output [0:0]M_32G_48G_AXI_awlock;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI AWPROT" *) output [2:0]M_32G_48G_AXI_awprot;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI AWQOS" *) output [3:0]M_32G_48G_AXI_awqos;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI AWREADY" *) input M_32G_48G_AXI_awready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI AWREGION" *) output [3:0]M_32G_48G_AXI_awregion;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI AWSIZE" *) output [2:0]M_32G_48G_AXI_awsize;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI AWVALID" *) output M_32G_48G_AXI_awvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI BID" *) input [6:0]M_32G_48G_AXI_bid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI BREADY" *) output M_32G_48G_AXI_bready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI BRESP" *) input [1:0]M_32G_48G_AXI_bresp;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI BVALID" *) input M_32G_48G_AXI_bvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI RDATA" *) input [511:0]M_32G_48G_AXI_rdata;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI RID" *) input [6:0]M_32G_48G_AXI_rid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI RLAST" *) input M_32G_48G_AXI_rlast;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI RREADY" *) output M_32G_48G_AXI_rready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI RRESP" *) input [1:0]M_32G_48G_AXI_rresp;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI RVALID" *) input M_32G_48G_AXI_rvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI WDATA" *) output [511:0]M_32G_48G_AXI_wdata;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI WLAST" *) output M_32G_48G_AXI_wlast;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI WREADY" *) input M_32G_48G_AXI_wready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI WSTRB" *) output [63:0]M_32G_48G_AXI_wstrb;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_32G_48G_AXI WVALID" *) output M_32G_48G_AXI_wvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI ARADDR" *) (* X_INTERFACE_PARAMETER = "XIL_INTERFACENAME M_48G_64G_AXI, ADDR_WIDTH 64, ARUSER_WIDTH 0, AWUSER_WIDTH 0, BUSER_WIDTH 0, CLK_DOMAIN cl_xbar_ACLK, DATA_WIDTH 512, FREQ_HZ 250000000, HAS_BRESP 1, HAS_BURST 1, HAS_CACHE 1, HAS_LOCK 1, HAS_PROT 1, HAS_QOS 1, HAS_REGION 1, HAS_RRESP 1, HAS_WSTRB 1, ID_WIDTH 7, INSERT_VIP 0, MAX_BURST_LENGTH 256, NUM_READ_OUTSTANDING 16, NUM_READ_THREADS 1, NUM_WRITE_OUTSTANDING 16, NUM_WRITE_THREADS 1, PHASE 0.0, PROTOCOL AXI4, READ_WRITE_MODE READ_WRITE, RUSER_BITS_PER_BYTE 0, RUSER_WIDTH 0, SUPPORTS_NARROW_BURST 1, WUSER_BITS_PER_BYTE 0, WUSER_WIDTH 0" *) output [63:0]M_48G_64G_AXI_araddr;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI ARBURST" *) output [1:0]M_48G_64G_AXI_arburst;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI ARCACHE" *) output [3:0]M_48G_64G_AXI_arcache;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI ARID" *) output [6:0]M_48G_64G_AXI_arid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI ARLEN" *) output [7:0]M_48G_64G_AXI_arlen;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI ARLOCK" *) output [0:0]M_48G_64G_AXI_arlock;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI ARPROT" *) output [2:0]M_48G_64G_AXI_arprot;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI ARQOS" *) output [3:0]M_48G_64G_AXI_arqos;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI ARREADY" *) input M_48G_64G_AXI_arready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI ARREGION" *) output [3:0]M_48G_64G_AXI_arregion;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI ARSIZE" *) output [2:0]M_48G_64G_AXI_arsize;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI ARVALID" *) output M_48G_64G_AXI_arvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI AWADDR" *) output [63:0]M_48G_64G_AXI_awaddr;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI AWBURST" *) output [1:0]M_48G_64G_AXI_awburst;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI AWCACHE" *) output [3:0]M_48G_64G_AXI_awcache;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI AWID" *) output [6:0]M_48G_64G_AXI_awid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI AWLEN" *) output [7:0]M_48G_64G_AXI_awlen;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI AWLOCK" *) output [0:0]M_48G_64G_AXI_awlock;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI AWPROT" *) output [2:0]M_48G_64G_AXI_awprot;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI AWQOS" *) output [3:0]M_48G_64G_AXI_awqos;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI AWREADY" *) input M_48G_64G_AXI_awready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI AWREGION" *) output [3:0]M_48G_64G_AXI_awregion;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI AWSIZE" *) output [2:0]M_48G_64G_AXI_awsize;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI AWVALID" *) output M_48G_64G_AXI_awvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI BID" *) input [6:0]M_48G_64G_AXI_bid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI BREADY" *) output M_48G_64G_AXI_bready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI BRESP" *) input [1:0]M_48G_64G_AXI_bresp;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI BVALID" *) input M_48G_64G_AXI_bvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI RDATA" *) input [511:0]M_48G_64G_AXI_rdata;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI RID" *) input [6:0]M_48G_64G_AXI_rid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI RLAST" *) input M_48G_64G_AXI_rlast;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI RREADY" *) output M_48G_64G_AXI_rready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI RRESP" *) input [1:0]M_48G_64G_AXI_rresp;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI RVALID" *) input M_48G_64G_AXI_rvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI WDATA" *) output [511:0]M_48G_64G_AXI_wdata;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI WLAST" *) output M_48G_64G_AXI_wlast;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI WREADY" *) input M_48G_64G_AXI_wready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI WSTRB" *) output [63:0]M_48G_64G_AXI_wstrb;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_48G_64G_AXI WVALID" *) output M_48G_64G_AXI_wvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL ARADDR" *) (* X_INTERFACE_PARAMETER = "XIL_INTERFACENAME M_AXI_RTL, ADDR_WIDTH 64, ARUSER_WIDTH 0, AWUSER_WIDTH 0, BUSER_WIDTH 0, CLK_DOMAIN cl_xbar_ACLK, DATA_WIDTH 512, FREQ_HZ 250000000, HAS_BRESP 1, HAS_BURST 1, HAS_CACHE 1, HAS_LOCK 1, HAS_PROT 1, HAS_QOS 1, HAS_REGION 1, HAS_RRESP 1, HAS_WSTRB 1, ID_WIDTH 6, INSERT_VIP 0, MAX_BURST_LENGTH 256, NUM_READ_OUTSTANDING 16, NUM_READ_THREADS 1, NUM_WRITE_OUTSTANDING 16, NUM_WRITE_THREADS 1, PHASE 0.0, PROTOCOL AXI4, READ_WRITE_MODE READ_WRITE, RUSER_BITS_PER_BYTE 0, RUSER_WIDTH 0, SUPPORTS_NARROW_BURST 1, WUSER_BITS_PER_BYTE 0, WUSER_WIDTH 0" *) output [63:0]M_AXI_RTL_araddr;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL ARBURST" *) output [1:0]M_AXI_RTL_arburst;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL ARCACHE" *) output [3:0]M_AXI_RTL_arcache;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL ARID" *) output [5:0]M_AXI_RTL_arid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL ARLEN" *) output [7:0]M_AXI_RTL_arlen;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL ARLOCK" *) output [0:0]M_AXI_RTL_arlock;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL ARPROT" *) output [2:0]M_AXI_RTL_arprot;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL ARQOS" *) output [3:0]M_AXI_RTL_arqos;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL ARREADY" *) input M_AXI_RTL_arready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL ARREGION" *) output [3:0]M_AXI_RTL_arregion;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL ARSIZE" *) output [2:0]M_AXI_RTL_arsize;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL ARVALID" *) output M_AXI_RTL_arvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL AWADDR" *) output [63:0]M_AXI_RTL_awaddr;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL AWBURST" *) output [1:0]M_AXI_RTL_awburst;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL AWCACHE" *) output [3:0]M_AXI_RTL_awcache;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL AWID" *) output [5:0]M_AXI_RTL_awid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL AWLEN" *) output [7:0]M_AXI_RTL_awlen;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL AWLOCK" *) output [0:0]M_AXI_RTL_awlock;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL AWPROT" *) output [2:0]M_AXI_RTL_awprot;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL AWQOS" *) output [3:0]M_AXI_RTL_awqos;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL AWREADY" *) input M_AXI_RTL_awready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL AWREGION" *) output [3:0]M_AXI_RTL_awregion;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL AWSIZE" *) output [2:0]M_AXI_RTL_awsize;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL AWVALID" *) output M_AXI_RTL_awvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL BID" *) input [5:0]M_AXI_RTL_bid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL BREADY" *) output M_AXI_RTL_bready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL BRESP" *) input [1:0]M_AXI_RTL_bresp;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL BVALID" *) input M_AXI_RTL_bvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL RDATA" *) input [511:0]M_AXI_RTL_rdata;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL RID" *) input [5:0]M_AXI_RTL_rid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL RLAST" *) input M_AXI_RTL_rlast;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL RREADY" *) output M_AXI_RTL_rready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL RRESP" *) input [1:0]M_AXI_RTL_rresp;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL RVALID" *) input M_AXI_RTL_rvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL WDATA" *) output [511:0]M_AXI_RTL_wdata;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL WLAST" *) output M_AXI_RTL_wlast;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL WREADY" *) input M_AXI_RTL_wready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL WSTRB" *) output [63:0]M_AXI_RTL_wstrb;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 M_AXI_RTL WVALID" *) output M_AXI_RTL_wvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS ARADDR" *) (* X_INTERFACE_PARAMETER = "XIL_INTERFACENAME S_AXI_PCIS, ADDR_WIDTH 64, ARUSER_WIDTH 0, AWUSER_WIDTH 0, BUSER_WIDTH 0, CLK_DOMAIN cl_xbar_ACLK, DATA_WIDTH 512, FREQ_HZ 250000000, HAS_BRESP 1, HAS_BURST 1, HAS_CACHE 1, HAS_LOCK 1, HAS_PROT 1, HAS_QOS 1, HAS_REGION 1, HAS_RRESP 1, HAS_WSTRB 1, ID_WIDTH 6, INSERT_VIP 0, MAX_BURST_LENGTH 256, NUM_READ_OUTSTANDING 32, NUM_READ_THREADS 16, NUM_WRITE_OUTSTANDING 32, NUM_WRITE_THREADS 16, PHASE 0.0, PROTOCOL AXI4, READ_WRITE_MODE READ_WRITE, RUSER_BITS_PER_BYTE 0, RUSER_WIDTH 0, SUPPORTS_NARROW_BURST 1, WUSER_BITS_PER_BYTE 0, WUSER_WIDTH 0" *) input [63:0]S_AXI_PCIS_araddr;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS ARBURST" *) input [1:0]S_AXI_PCIS_arburst;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS ARCACHE" *) input [3:0]S_AXI_PCIS_arcache;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS ARID" *) input [5:0]S_AXI_PCIS_arid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS ARLEN" *) input [7:0]S_AXI_PCIS_arlen;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS ARLOCK" *) input [0:0]S_AXI_PCIS_arlock;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS ARPROT" *) input [2:0]S_AXI_PCIS_arprot;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS ARQOS" *) input [3:0]S_AXI_PCIS_arqos;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS ARREADY" *) output S_AXI_PCIS_arready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS ARREGION" *) input [3:0]S_AXI_PCIS_arregion;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS ARSIZE" *) input [2:0]S_AXI_PCIS_arsize;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS ARVALID" *) input S_AXI_PCIS_arvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS AWADDR" *) input [63:0]S_AXI_PCIS_awaddr;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS AWBURST" *) input [1:0]S_AXI_PCIS_awburst;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS AWCACHE" *) input [3:0]S_AXI_PCIS_awcache;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS AWID" *) input [5:0]S_AXI_PCIS_awid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS AWLEN" *) input [7:0]S_AXI_PCIS_awlen;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS AWLOCK" *) input [0:0]S_AXI_PCIS_awlock;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS AWPROT" *) input [2:0]S_AXI_PCIS_awprot;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS AWQOS" *) input [3:0]S_AXI_PCIS_awqos;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS AWREADY" *) output S_AXI_PCIS_awready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS AWREGION" *) input [3:0]S_AXI_PCIS_awregion;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS AWSIZE" *) input [2:0]S_AXI_PCIS_awsize;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS AWVALID" *) input S_AXI_PCIS_awvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS BID" *) output [5:0]S_AXI_PCIS_bid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS BREADY" *) input S_AXI_PCIS_bready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS BRESP" *) output [1:0]S_AXI_PCIS_bresp;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS BVALID" *) output S_AXI_PCIS_bvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS RDATA" *) output [511:0]S_AXI_PCIS_rdata;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS RID" *) output [5:0]S_AXI_PCIS_rid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS RLAST" *) output S_AXI_PCIS_rlast;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS RREADY" *) input S_AXI_PCIS_rready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS RRESP" *) output [1:0]S_AXI_PCIS_rresp;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS RVALID" *) output S_AXI_PCIS_rvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS WDATA" *) input [511:0]S_AXI_PCIS_wdata;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS WLAST" *) input S_AXI_PCIS_wlast;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS WREADY" *) output S_AXI_PCIS_wready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS WSTRB" *) input [63:0]S_AXI_PCIS_wstrb;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_AXI_PCIS WVALID" *) input S_AXI_PCIS_wvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI ARADDR" *) (* X_INTERFACE_PARAMETER = "XIL_INTERFACENAME S_RTL_DRAM_AXI, ADDR_WIDTH 64, ARUSER_WIDTH 0, AWUSER_WIDTH 0, BUSER_WIDTH 0, CLK_DOMAIN cl_xbar_ACLK, DATA_WIDTH 512, FREQ_HZ 250000000, HAS_BRESP 1, HAS_BURST 1, HAS_CACHE 1, HAS_LOCK 1, HAS_PROT 1, HAS_QOS 1, HAS_REGION 1, HAS_RRESP 1, HAS_WSTRB 1, ID_WIDTH 6, INSERT_VIP 0, MAX_BURST_LENGTH 256, NUM_READ_OUTSTANDING 32, NUM_READ_THREADS 16, NUM_WRITE_OUTSTANDING 32, NUM_WRITE_THREADS 16, PHASE 0.0, PROTOCOL AXI4, READ_WRITE_MODE READ_WRITE, RUSER_BITS_PER_BYTE 0, RUSER_WIDTH 0, SUPPORTS_NARROW_BURST 1, WUSER_BITS_PER_BYTE 0, WUSER_WIDTH 0" *) input [63:0]S_RTL_DRAM_AXI_araddr;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI ARBURST" *) input [1:0]S_RTL_DRAM_AXI_arburst;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI ARCACHE" *) input [3:0]S_RTL_DRAM_AXI_arcache;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI ARID" *) input [5:0]S_RTL_DRAM_AXI_arid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI ARLEN" *) input [7:0]S_RTL_DRAM_AXI_arlen;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI ARLOCK" *) input [0:0]S_RTL_DRAM_AXI_arlock;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI ARPROT" *) input [2:0]S_RTL_DRAM_AXI_arprot;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI ARQOS" *) input [3:0]S_RTL_DRAM_AXI_arqos;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI ARREADY" *) output S_RTL_DRAM_AXI_arready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI ARREGION" *) input [3:0]S_RTL_DRAM_AXI_arregion;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI ARSIZE" *) input [2:0]S_RTL_DRAM_AXI_arsize;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI ARVALID" *) input S_RTL_DRAM_AXI_arvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI AWADDR" *) input [63:0]S_RTL_DRAM_AXI_awaddr;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI AWBURST" *) input [1:0]S_RTL_DRAM_AXI_awburst;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI AWCACHE" *) input [3:0]S_RTL_DRAM_AXI_awcache;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI AWID" *) input [5:0]S_RTL_DRAM_AXI_awid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI AWLEN" *) input [7:0]S_RTL_DRAM_AXI_awlen;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI AWLOCK" *) input [0:0]S_RTL_DRAM_AXI_awlock;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI AWPROT" *) input [2:0]S_RTL_DRAM_AXI_awprot;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI AWQOS" *) input [3:0]S_RTL_DRAM_AXI_awqos;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI AWREADY" *) output S_RTL_DRAM_AXI_awready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI AWREGION" *) input [3:0]S_RTL_DRAM_AXI_awregion;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI AWSIZE" *) input [2:0]S_RTL_DRAM_AXI_awsize;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI AWVALID" *) input S_RTL_DRAM_AXI_awvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI BID" *) output [5:0]S_RTL_DRAM_AXI_bid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI BREADY" *) input S_RTL_DRAM_AXI_bready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI BRESP" *) output [1:0]S_RTL_DRAM_AXI_bresp;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI BVALID" *) output S_RTL_DRAM_AXI_bvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI RDATA" *) output [511:0]S_RTL_DRAM_AXI_rdata;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI RID" *) output [5:0]S_RTL_DRAM_AXI_rid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI RLAST" *) output S_RTL_DRAM_AXI_rlast;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI RREADY" *) input S_RTL_DRAM_AXI_rready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI RRESP" *) output [1:0]S_RTL_DRAM_AXI_rresp;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI RVALID" *) output S_RTL_DRAM_AXI_rvalid;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI WDATA" *) input [511:0]S_RTL_DRAM_AXI_wdata;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI WLAST" *) input S_RTL_DRAM_AXI_wlast;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI WREADY" *) output S_RTL_DRAM_AXI_wready;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI WSTRB" *) input [63:0]S_RTL_DRAM_AXI_wstrb;
  (* X_INTERFACE_INFO = "xilinx.com:interface:aximm:1.0 S_RTL_DRAM_AXI WVALID" *) input S_RTL_DRAM_AXI_wvalid;

  wire ACLK_WIRE;
  wire ARESETN_WIRE;
  wire [63:0]M_00G_16G_AXI_IO_ARADDR;
  wire [1:0]M_00G_16G_AXI_IO_ARBURST;
  wire [3:0]M_00G_16G_AXI_IO_ARCACHE;
  wire [6:0]M_00G_16G_AXI_IO_ARID;
  wire [7:0]M_00G_16G_AXI_IO_ARLEN;
  wire [0:0]M_00G_16G_AXI_IO_ARLOCK;
  wire [2:0]M_00G_16G_AXI_IO_ARPROT;
  wire [3:0]M_00G_16G_AXI_IO_ARQOS;
  wire M_00G_16G_AXI_IO_ARREADY;
  wire [3:0]M_00G_16G_AXI_IO_ARREGION;
  wire [2:0]M_00G_16G_AXI_IO_ARSIZE;
  wire M_00G_16G_AXI_IO_ARVALID;
  wire [63:0]M_00G_16G_AXI_IO_AWADDR;
  wire [1:0]M_00G_16G_AXI_IO_AWBURST;
  wire [3:0]M_00G_16G_AXI_IO_AWCACHE;
  wire [6:0]M_00G_16G_AXI_IO_AWID;
  wire [7:0]M_00G_16G_AXI_IO_AWLEN;
  wire [0:0]M_00G_16G_AXI_IO_AWLOCK;
  wire [2:0]M_00G_16G_AXI_IO_AWPROT;
  wire [3:0]M_00G_16G_AXI_IO_AWQOS;
  wire M_00G_16G_AXI_IO_AWREADY;
  wire [3:0]M_00G_16G_AXI_IO_AWREGION;
  wire [2:0]M_00G_16G_AXI_IO_AWSIZE;
  wire M_00G_16G_AXI_IO_AWVALID;
  wire [6:0]M_00G_16G_AXI_IO_BID;
  wire M_00G_16G_AXI_IO_BREADY;
  wire [1:0]M_00G_16G_AXI_IO_BRESP;
  wire M_00G_16G_AXI_IO_BVALID;
  wire [511:0]M_00G_16G_AXI_IO_RDATA;
  wire [6:0]M_00G_16G_AXI_IO_RID;
  wire M_00G_16G_AXI_IO_RLAST;
  wire M_00G_16G_AXI_IO_RREADY;
  wire [1:0]M_00G_16G_AXI_IO_RRESP;
  wire M_00G_16G_AXI_IO_RVALID;
  wire [511:0]M_00G_16G_AXI_IO_WDATA;
  wire M_00G_16G_AXI_IO_WLAST;
  wire M_00G_16G_AXI_IO_WREADY;
  wire [63:0]M_00G_16G_AXI_IO_WSTRB;
  wire M_00G_16G_AXI_IO_WVALID;
  wire [63:0]M_16G_32G_AXI_IO_ARADDR;
  wire [1:0]M_16G_32G_AXI_IO_ARBURST;
  wire [3:0]M_16G_32G_AXI_IO_ARCACHE;
  wire [6:0]M_16G_32G_AXI_IO_ARID;
  wire [7:0]M_16G_32G_AXI_IO_ARLEN;
  wire [0:0]M_16G_32G_AXI_IO_ARLOCK;
  wire [2:0]M_16G_32G_AXI_IO_ARPROT;
  wire [3:0]M_16G_32G_AXI_IO_ARQOS;
  wire M_16G_32G_AXI_IO_ARREADY;
  wire [3:0]M_16G_32G_AXI_IO_ARREGION;
  wire [2:0]M_16G_32G_AXI_IO_ARSIZE;
  wire M_16G_32G_AXI_IO_ARVALID;
  wire [63:0]M_16G_32G_AXI_IO_AWADDR;
  wire [1:0]M_16G_32G_AXI_IO_AWBURST;
  wire [3:0]M_16G_32G_AXI_IO_AWCACHE;
  wire [6:0]M_16G_32G_AXI_IO_AWID;
  wire [7:0]M_16G_32G_AXI_IO_AWLEN;
  wire [0:0]M_16G_32G_AXI_IO_AWLOCK;
  wire [2:0]M_16G_32G_AXI_IO_AWPROT;
  wire [3:0]M_16G_32G_AXI_IO_AWQOS;
  wire M_16G_32G_AXI_IO_AWREADY;
  wire [3:0]M_16G_32G_AXI_IO_AWREGION;
  wire [2:0]M_16G_32G_AXI_IO_AWSIZE;
  wire M_16G_32G_AXI_IO_AWVALID;
  wire [6:0]M_16G_32G_AXI_IO_BID;
  wire M_16G_32G_AXI_IO_BREADY;
  wire [1:0]M_16G_32G_AXI_IO_BRESP;
  wire M_16G_32G_AXI_IO_BVALID;
  wire [511:0]M_16G_32G_AXI_IO_RDATA;
  wire [6:0]M_16G_32G_AXI_IO_RID;
  wire M_16G_32G_AXI_IO_RLAST;
  wire M_16G_32G_AXI_IO_RREADY;
  wire [1:0]M_16G_32G_AXI_IO_RRESP;
  wire M_16G_32G_AXI_IO_RVALID;
  wire [511:0]M_16G_32G_AXI_IO_WDATA;
  wire M_16G_32G_AXI_IO_WLAST;
  wire M_16G_32G_AXI_IO_WREADY;
  wire [63:0]M_16G_32G_AXI_IO_WSTRB;
  wire M_16G_32G_AXI_IO_WVALID;
  wire [63:0]M_32G_48G_AXI_IO_ARADDR;
  wire [1:0]M_32G_48G_AXI_IO_ARBURST;
  wire [3:0]M_32G_48G_AXI_IO_ARCACHE;
  wire [6:0]M_32G_48G_AXI_IO_ARID;
  wire [7:0]M_32G_48G_AXI_IO_ARLEN;
  wire [0:0]M_32G_48G_AXI_IO_ARLOCK;
  wire [2:0]M_32G_48G_AXI_IO_ARPROT;
  wire [3:0]M_32G_48G_AXI_IO_ARQOS;
  wire M_32G_48G_AXI_IO_ARREADY;
  wire [3:0]M_32G_48G_AXI_IO_ARREGION;
  wire [2:0]M_32G_48G_AXI_IO_ARSIZE;
  wire M_32G_48G_AXI_IO_ARVALID;
  wire [63:0]M_32G_48G_AXI_IO_AWADDR;
  wire [1:0]M_32G_48G_AXI_IO_AWBURST;
  wire [3:0]M_32G_48G_AXI_IO_AWCACHE;
  wire [6:0]M_32G_48G_AXI_IO_AWID;
  wire [7:0]M_32G_48G_AXI_IO_AWLEN;
  wire [0:0]M_32G_48G_AXI_IO_AWLOCK;
  wire [2:0]M_32G_48G_AXI_IO_AWPROT;
  wire [3:0]M_32G_48G_AXI_IO_AWQOS;
  wire M_32G_48G_AXI_IO_AWREADY;
  wire [3:0]M_32G_48G_AXI_IO_AWREGION;
  wire [2:0]M_32G_48G_AXI_IO_AWSIZE;
  wire M_32G_48G_AXI_IO_AWVALID;
  wire [6:0]M_32G_48G_AXI_IO_BID;
  wire M_32G_48G_AXI_IO_BREADY;
  wire [1:0]M_32G_48G_AXI_IO_BRESP;
  wire M_32G_48G_AXI_IO_BVALID;
  wire [511:0]M_32G_48G_AXI_IO_RDATA;
  wire [6:0]M_32G_48G_AXI_IO_RID;
  wire M_32G_48G_AXI_IO_RLAST;
  wire M_32G_48G_AXI_IO_RREADY;
  wire [1:0]M_32G_48G_AXI_IO_RRESP;
  wire M_32G_48G_AXI_IO_RVALID;
  wire [511:0]M_32G_48G_AXI_IO_WDATA;
  wire M_32G_48G_AXI_IO_WLAST;
  wire M_32G_48G_AXI_IO_WREADY;
  wire [63:0]M_32G_48G_AXI_IO_WSTRB;
  wire M_32G_48G_AXI_IO_WVALID;
  wire [63:0]M_48G_64G_AXI_IO_ARADDR;
  wire [1:0]M_48G_64G_AXI_IO_ARBURST;
  wire [3:0]M_48G_64G_AXI_IO_ARCACHE;
  wire [6:0]M_48G_64G_AXI_IO_ARID;
  wire [7:0]M_48G_64G_AXI_IO_ARLEN;
  wire [0:0]M_48G_64G_AXI_IO_ARLOCK;
  wire [2:0]M_48G_64G_AXI_IO_ARPROT;
  wire [3:0]M_48G_64G_AXI_IO_ARQOS;
  wire M_48G_64G_AXI_IO_ARREADY;
  wire [3:0]M_48G_64G_AXI_IO_ARREGION;
  wire [2:0]M_48G_64G_AXI_IO_ARSIZE;
  wire M_48G_64G_AXI_IO_ARVALID;
  wire [63:0]M_48G_64G_AXI_IO_AWADDR;
  wire [1:0]M_48G_64G_AXI_IO_AWBURST;
  wire [3:0]M_48G_64G_AXI_IO_AWCACHE;
  wire [6:0]M_48G_64G_AXI_IO_AWID;
  wire [7:0]M_48G_64G_AXI_IO_AWLEN;
  wire [0:0]M_48G_64G_AXI_IO_AWLOCK;
  wire [2:0]M_48G_64G_AXI_IO_AWPROT;
  wire [3:0]M_48G_64G_AXI_IO_AWQOS;
  wire M_48G_64G_AXI_IO_AWREADY;
  wire [3:0]M_48G_64G_AXI_IO_AWREGION;
  wire [2:0]M_48G_64G_AXI_IO_AWSIZE;
  wire M_48G_64G_AXI_IO_AWVALID;
  wire [6:0]M_48G_64G_AXI_IO_BID;
  wire M_48G_64G_AXI_IO_BREADY;
  wire [1:0]M_48G_64G_AXI_IO_BRESP;
  wire M_48G_64G_AXI_IO_BVALID;
  wire [511:0]M_48G_64G_AXI_IO_RDATA;
  wire [6:0]M_48G_64G_AXI_IO_RID;
  wire M_48G_64G_AXI_IO_RLAST;
  wire M_48G_64G_AXI_IO_RREADY;
  wire [1:0]M_48G_64G_AXI_IO_RRESP;
  wire M_48G_64G_AXI_IO_RVALID;
  wire [511:0]M_48G_64G_AXI_IO_WDATA;
  wire M_48G_64G_AXI_IO_WLAST;
  wire M_48G_64G_AXI_IO_WREADY;
  wire [63:0]M_48G_64G_AXI_IO_WSTRB;
  wire M_48G_64G_AXI_IO_WVALID;
  wire [63:0]M_AXI_RTL_IO_ARADDR;
  wire [1:0]M_AXI_RTL_IO_ARBURST;
  wire [3:0]M_AXI_RTL_IO_ARCACHE;
  wire [5:0]M_AXI_RTL_IO_ARID;
  wire [7:0]M_AXI_RTL_IO_ARLEN;
  wire [0:0]M_AXI_RTL_IO_ARLOCK;
  wire [2:0]M_AXI_RTL_IO_ARPROT;
  wire [3:0]M_AXI_RTL_IO_ARQOS;
  wire M_AXI_RTL_IO_ARREADY;
  wire [3:0]M_AXI_RTL_IO_ARREGION;
  wire [2:0]M_AXI_RTL_IO_ARSIZE;
  wire M_AXI_RTL_IO_ARVALID;
  wire [63:0]M_AXI_RTL_IO_AWADDR;
  wire [1:0]M_AXI_RTL_IO_AWBURST;
  wire [3:0]M_AXI_RTL_IO_AWCACHE;
  wire [5:0]M_AXI_RTL_IO_AWID;
  wire [7:0]M_AXI_RTL_IO_AWLEN;
  wire [0:0]M_AXI_RTL_IO_AWLOCK;
  wire [2:0]M_AXI_RTL_IO_AWPROT;
  wire [3:0]M_AXI_RTL_IO_AWQOS;
  wire M_AXI_RTL_IO_AWREADY;
  wire [3:0]M_AXI_RTL_IO_AWREGION;
  wire [2:0]M_AXI_RTL_IO_AWSIZE;
  wire M_AXI_RTL_IO_AWVALID;
  wire [5:0]M_AXI_RTL_IO_BID;
  wire M_AXI_RTL_IO_BREADY;
  wire [1:0]M_AXI_RTL_IO_BRESP;
  wire M_AXI_RTL_IO_BVALID;
  wire [511:0]M_AXI_RTL_IO_RDATA;
  wire [5:0]M_AXI_RTL_IO_RID;
  wire M_AXI_RTL_IO_RLAST;
  wire M_AXI_RTL_IO_RREADY;
  wire [1:0]M_AXI_RTL_IO_RRESP;
  wire M_AXI_RTL_IO_RVALID;
  wire [511:0]M_AXI_RTL_IO_WDATA;
  wire M_AXI_RTL_IO_WLAST;
  wire M_AXI_RTL_IO_WREADY;
  wire [63:0]M_AXI_RTL_IO_WSTRB;
  wire M_AXI_RTL_IO_WVALID;
  wire [63:0]S_AXI_PCIS_IO_ARADDR;
  wire [1:0]S_AXI_PCIS_IO_ARBURST;
  wire [3:0]S_AXI_PCIS_IO_ARCACHE;
  wire [5:0]S_AXI_PCIS_IO_ARID;
  wire [7:0]S_AXI_PCIS_IO_ARLEN;
  wire [0:0]S_AXI_PCIS_IO_ARLOCK;
  wire [2:0]S_AXI_PCIS_IO_ARPROT;
  wire [3:0]S_AXI_PCIS_IO_ARQOS;
  wire S_AXI_PCIS_IO_ARREADY;
  wire [3:0]S_AXI_PCIS_IO_ARREGION;
  wire [2:0]S_AXI_PCIS_IO_ARSIZE;
  wire S_AXI_PCIS_IO_ARVALID;
  wire [63:0]S_AXI_PCIS_IO_AWADDR;
  wire [1:0]S_AXI_PCIS_IO_AWBURST;
  wire [3:0]S_AXI_PCIS_IO_AWCACHE;
  wire [5:0]S_AXI_PCIS_IO_AWID;
  wire [7:0]S_AXI_PCIS_IO_AWLEN;
  wire [0:0]S_AXI_PCIS_IO_AWLOCK;
  wire [2:0]S_AXI_PCIS_IO_AWPROT;
  wire [3:0]S_AXI_PCIS_IO_AWQOS;
  wire S_AXI_PCIS_IO_AWREADY;
  wire [3:0]S_AXI_PCIS_IO_AWREGION;
  wire [2:0]S_AXI_PCIS_IO_AWSIZE;
  wire S_AXI_PCIS_IO_AWVALID;
  wire [5:0]S_AXI_PCIS_IO_BID;
  wire S_AXI_PCIS_IO_BREADY;
  wire [1:0]S_AXI_PCIS_IO_BRESP;
  wire S_AXI_PCIS_IO_BVALID;
  wire [511:0]S_AXI_PCIS_IO_RDATA;
  wire [5:0]S_AXI_PCIS_IO_RID;
  wire S_AXI_PCIS_IO_RLAST;
  wire S_AXI_PCIS_IO_RREADY;
  wire [1:0]S_AXI_PCIS_IO_RRESP;
  wire S_AXI_PCIS_IO_RVALID;
  wire [511:0]S_AXI_PCIS_IO_WDATA;
  wire S_AXI_PCIS_IO_WLAST;
  wire S_AXI_PCIS_IO_WREADY;
  wire [63:0]S_AXI_PCIS_IO_WSTRB;
  wire S_AXI_PCIS_IO_WVALID;
  wire [63:0]S_PCIS_DRAM_AXI_IO_ARADDR;
  wire [1:0]S_PCIS_DRAM_AXI_IO_ARBURST;
  wire [3:0]S_PCIS_DRAM_AXI_IO_ARCACHE;
  wire [5:0]S_PCIS_DRAM_AXI_IO_ARID;
  wire [7:0]S_PCIS_DRAM_AXI_IO_ARLEN;
  wire [0:0]S_PCIS_DRAM_AXI_IO_ARLOCK;
  wire [2:0]S_PCIS_DRAM_AXI_IO_ARPROT;
  wire [3:0]S_PCIS_DRAM_AXI_IO_ARQOS;
  wire S_PCIS_DRAM_AXI_IO_ARREADY;
  wire [3:0]S_PCIS_DRAM_AXI_IO_ARREGION;
  wire [2:0]S_PCIS_DRAM_AXI_IO_ARSIZE;
  wire S_PCIS_DRAM_AXI_IO_ARVALID;
  wire [63:0]S_PCIS_DRAM_AXI_IO_AWADDR;
  wire [1:0]S_PCIS_DRAM_AXI_IO_AWBURST;
  wire [3:0]S_PCIS_DRAM_AXI_IO_AWCACHE;
  wire [5:0]S_PCIS_DRAM_AXI_IO_AWID;
  wire [7:0]S_PCIS_DRAM_AXI_IO_AWLEN;
  wire [0:0]S_PCIS_DRAM_AXI_IO_AWLOCK;
  wire [2:0]S_PCIS_DRAM_AXI_IO_AWPROT;
  wire [3:0]S_PCIS_DRAM_AXI_IO_AWQOS;
  wire S_PCIS_DRAM_AXI_IO_AWREADY;
  wire [3:0]S_PCIS_DRAM_AXI_IO_AWREGION;
  wire [2:0]S_PCIS_DRAM_AXI_IO_AWSIZE;
  wire S_PCIS_DRAM_AXI_IO_AWVALID;
  wire [5:0]S_PCIS_DRAM_AXI_IO_BID;
  wire S_PCIS_DRAM_AXI_IO_BREADY;
  wire [1:0]S_PCIS_DRAM_AXI_IO_BRESP;
  wire S_PCIS_DRAM_AXI_IO_BVALID;
  wire [511:0]S_PCIS_DRAM_AXI_IO_RDATA;
  wire [5:0]S_PCIS_DRAM_AXI_IO_RID;
  wire S_PCIS_DRAM_AXI_IO_RLAST;
  wire S_PCIS_DRAM_AXI_IO_RREADY;
  wire [1:0]S_PCIS_DRAM_AXI_IO_RRESP;
  wire S_PCIS_DRAM_AXI_IO_RVALID;
  wire [511:0]S_PCIS_DRAM_AXI_IO_WDATA;
  wire S_PCIS_DRAM_AXI_IO_WLAST;
  wire S_PCIS_DRAM_AXI_IO_WREADY;
  wire [63:0]S_PCIS_DRAM_AXI_IO_WSTRB;
  wire S_PCIS_DRAM_AXI_IO_WVALID;
  wire [63:0]S_RTL_DRAM_AXI_IO_ARADDR;
  wire [1:0]S_RTL_DRAM_AXI_IO_ARBURST;
  wire [3:0]S_RTL_DRAM_AXI_IO_ARCACHE;
  wire [5:0]S_RTL_DRAM_AXI_IO_ARID;
  wire [7:0]S_RTL_DRAM_AXI_IO_ARLEN;
  wire [0:0]S_RTL_DRAM_AXI_IO_ARLOCK;
  wire [2:0]S_RTL_DRAM_AXI_IO_ARPROT;
  wire [3:0]S_RTL_DRAM_AXI_IO_ARQOS;
  wire S_RTL_DRAM_AXI_IO_ARREADY;
  wire [3:0]S_RTL_DRAM_AXI_IO_ARREGION;
  wire [2:0]S_RTL_DRAM_AXI_IO_ARSIZE;
  wire S_RTL_DRAM_AXI_IO_ARVALID;
  wire [63:0]S_RTL_DRAM_AXI_IO_AWADDR;
  wire [1:0]S_RTL_DRAM_AXI_IO_AWBURST;
  wire [3:0]S_RTL_DRAM_AXI_IO_AWCACHE;
  wire [5:0]S_RTL_DRAM_AXI_IO_AWID;
  wire [7:0]S_RTL_DRAM_AXI_IO_AWLEN;
  wire [0:0]S_RTL_DRAM_AXI_IO_AWLOCK;
  wire [2:0]S_RTL_DRAM_AXI_IO_AWPROT;
  wire [3:0]S_RTL_DRAM_AXI_IO_AWQOS;
  wire S_RTL_DRAM_AXI_IO_AWREADY;
  wire [3:0]S_RTL_DRAM_AXI_IO_AWREGION;
  wire [2:0]S_RTL_DRAM_AXI_IO_AWSIZE;
  wire S_RTL_DRAM_AXI_IO_AWVALID;
  wire [5:0]S_RTL_DRAM_AXI_IO_BID;
  wire S_RTL_DRAM_AXI_IO_BREADY;
  wire [1:0]S_RTL_DRAM_AXI_IO_BRESP;
  wire S_RTL_DRAM_AXI_IO_BVALID;
  wire [511:0]S_RTL_DRAM_AXI_IO_RDATA;
  wire [5:0]S_RTL_DRAM_AXI_IO_RID;
  wire S_RTL_DRAM_AXI_IO_RLAST;
  wire S_RTL_DRAM_AXI_IO_RREADY;
  wire [1:0]S_RTL_DRAM_AXI_IO_RRESP;
  wire S_RTL_DRAM_AXI_IO_RVALID;
  wire [511:0]S_RTL_DRAM_AXI_IO_WDATA;
  wire S_RTL_DRAM_AXI_IO_WLAST;
  wire S_RTL_DRAM_AXI_IO_WREADY;
  wire [63:0]S_RTL_DRAM_AXI_IO_WSTRB;
  wire S_RTL_DRAM_AXI_IO_WVALID;

  assign ACLK_WIRE = ACLK;
  assign ARESETN_WIRE = ARESETN;
  assign M_00G_16G_AXI_IO_ARREADY = M_00G_16G_AXI_arready;
  assign M_00G_16G_AXI_IO_AWREADY = M_00G_16G_AXI_awready;
  assign M_00G_16G_AXI_IO_BID = M_00G_16G_AXI_bid[6:0];
  assign M_00G_16G_AXI_IO_BRESP = M_00G_16G_AXI_bresp[1:0];
  assign M_00G_16G_AXI_IO_BVALID = M_00G_16G_AXI_bvalid;
  assign M_00G_16G_AXI_IO_RDATA = M_00G_16G_AXI_rdata[511:0];
  assign M_00G_16G_AXI_IO_RID = M_00G_16G_AXI_rid[6:0];
  assign M_00G_16G_AXI_IO_RLAST = M_00G_16G_AXI_rlast;
  assign M_00G_16G_AXI_IO_RRESP = M_00G_16G_AXI_rresp[1:0];
  assign M_00G_16G_AXI_IO_RVALID = M_00G_16G_AXI_rvalid;
  assign M_00G_16G_AXI_IO_WREADY = M_00G_16G_AXI_wready;
  assign M_00G_16G_AXI_araddr[63:0] = M_00G_16G_AXI_IO_ARADDR;
  assign M_00G_16G_AXI_arburst[1:0] = M_00G_16G_AXI_IO_ARBURST;
  assign M_00G_16G_AXI_arcache[3:0] = M_00G_16G_AXI_IO_ARCACHE;
  assign M_00G_16G_AXI_arid[6:0] = M_00G_16G_AXI_IO_ARID;
  assign M_00G_16G_AXI_arlen[7:0] = M_00G_16G_AXI_IO_ARLEN;
  assign M_00G_16G_AXI_arlock[0] = M_00G_16G_AXI_IO_ARLOCK;
  assign M_00G_16G_AXI_arprot[2:0] = M_00G_16G_AXI_IO_ARPROT;
  assign M_00G_16G_AXI_arqos[3:0] = M_00G_16G_AXI_IO_ARQOS;
  assign M_00G_16G_AXI_arregion[3:0] = M_00G_16G_AXI_IO_ARREGION;
  assign M_00G_16G_AXI_arsize[2:0] = M_00G_16G_AXI_IO_ARSIZE;
  assign M_00G_16G_AXI_arvalid = M_00G_16G_AXI_IO_ARVALID;
  assign M_00G_16G_AXI_awaddr[63:0] = M_00G_16G_AXI_IO_AWADDR;
  assign M_00G_16G_AXI_awburst[1:0] = M_00G_16G_AXI_IO_AWBURST;
  assign M_00G_16G_AXI_awcache[3:0] = M_00G_16G_AXI_IO_AWCACHE;
  assign M_00G_16G_AXI_awid[6:0] = M_00G_16G_AXI_IO_AWID;
  assign M_00G_16G_AXI_awlen[7:0] = M_00G_16G_AXI_IO_AWLEN;
  assign M_00G_16G_AXI_awlock[0] = M_00G_16G_AXI_IO_AWLOCK;
  assign M_00G_16G_AXI_awprot[2:0] = M_00G_16G_AXI_IO_AWPROT;
  assign M_00G_16G_AXI_awqos[3:0] = M_00G_16G_AXI_IO_AWQOS;
  assign M_00G_16G_AXI_awregion[3:0] = M_00G_16G_AXI_IO_AWREGION;
  assign M_00G_16G_AXI_awsize[2:0] = M_00G_16G_AXI_IO_AWSIZE;
  assign M_00G_16G_AXI_awvalid = M_00G_16G_AXI_IO_AWVALID;
  assign M_00G_16G_AXI_bready = M_00G_16G_AXI_IO_BREADY;
  assign M_00G_16G_AXI_rready = M_00G_16G_AXI_IO_RREADY;
  assign M_00G_16G_AXI_wdata[511:0] = M_00G_16G_AXI_IO_WDATA;
  assign M_00G_16G_AXI_wlast = M_00G_16G_AXI_IO_WLAST;
  assign M_00G_16G_AXI_wstrb[63:0] = M_00G_16G_AXI_IO_WSTRB;
  assign M_00G_16G_AXI_wvalid = M_00G_16G_AXI_IO_WVALID;
  assign M_16G_32G_AXI_IO_ARREADY = M_16G_32G_AXI_arready;
  assign M_16G_32G_AXI_IO_AWREADY = M_16G_32G_AXI_awready;
  assign M_16G_32G_AXI_IO_BID = M_16G_32G_AXI_bid[6:0];
  assign M_16G_32G_AXI_IO_BRESP = M_16G_32G_AXI_bresp[1:0];
  assign M_16G_32G_AXI_IO_BVALID = M_16G_32G_AXI_bvalid;
  assign M_16G_32G_AXI_IO_RDATA = M_16G_32G_AXI_rdata[511:0];
  assign M_16G_32G_AXI_IO_RID = M_16G_32G_AXI_rid[6:0];
  assign M_16G_32G_AXI_IO_RLAST = M_16G_32G_AXI_rlast;
  assign M_16G_32G_AXI_IO_RRESP = M_16G_32G_AXI_rresp[1:0];
  assign M_16G_32G_AXI_IO_RVALID = M_16G_32G_AXI_rvalid;
  assign M_16G_32G_AXI_IO_WREADY = M_16G_32G_AXI_wready;
  assign M_16G_32G_AXI_araddr[63:0] = M_16G_32G_AXI_IO_ARADDR;
  assign M_16G_32G_AXI_arburst[1:0] = M_16G_32G_AXI_IO_ARBURST;
  assign M_16G_32G_AXI_arcache[3:0] = M_16G_32G_AXI_IO_ARCACHE;
  assign M_16G_32G_AXI_arid[6:0] = M_16G_32G_AXI_IO_ARID;
  assign M_16G_32G_AXI_arlen[7:0] = M_16G_32G_AXI_IO_ARLEN;
  assign M_16G_32G_AXI_arlock[0] = M_16G_32G_AXI_IO_ARLOCK;
  assign M_16G_32G_AXI_arprot[2:0] = M_16G_32G_AXI_IO_ARPROT;
  assign M_16G_32G_AXI_arqos[3:0] = M_16G_32G_AXI_IO_ARQOS;
  assign M_16G_32G_AXI_arregion[3:0] = M_16G_32G_AXI_IO_ARREGION;
  assign M_16G_32G_AXI_arsize[2:0] = M_16G_32G_AXI_IO_ARSIZE;
  assign M_16G_32G_AXI_arvalid = M_16G_32G_AXI_IO_ARVALID;
  assign M_16G_32G_AXI_awaddr[63:0] = M_16G_32G_AXI_IO_AWADDR;
  assign M_16G_32G_AXI_awburst[1:0] = M_16G_32G_AXI_IO_AWBURST;
  assign M_16G_32G_AXI_awcache[3:0] = M_16G_32G_AXI_IO_AWCACHE;
  assign M_16G_32G_AXI_awid[6:0] = M_16G_32G_AXI_IO_AWID;
  assign M_16G_32G_AXI_awlen[7:0] = M_16G_32G_AXI_IO_AWLEN;
  assign M_16G_32G_AXI_awlock[0] = M_16G_32G_AXI_IO_AWLOCK;
  assign M_16G_32G_AXI_awprot[2:0] = M_16G_32G_AXI_IO_AWPROT;
  assign M_16G_32G_AXI_awqos[3:0] = M_16G_32G_AXI_IO_AWQOS;
  assign M_16G_32G_AXI_awregion[3:0] = M_16G_32G_AXI_IO_AWREGION;
  assign M_16G_32G_AXI_awsize[2:0] = M_16G_32G_AXI_IO_AWSIZE;
  assign M_16G_32G_AXI_awvalid = M_16G_32G_AXI_IO_AWVALID;
  assign M_16G_32G_AXI_bready = M_16G_32G_AXI_IO_BREADY;
  assign M_16G_32G_AXI_rready = M_16G_32G_AXI_IO_RREADY;
  assign M_16G_32G_AXI_wdata[511:0] = M_16G_32G_AXI_IO_WDATA;
  assign M_16G_32G_AXI_wlast = M_16G_32G_AXI_IO_WLAST;
  assign M_16G_32G_AXI_wstrb[63:0] = M_16G_32G_AXI_IO_WSTRB;
  assign M_16G_32G_AXI_wvalid = M_16G_32G_AXI_IO_WVALID;
  assign M_32G_48G_AXI_IO_ARREADY = M_32G_48G_AXI_arready;
  assign M_32G_48G_AXI_IO_AWREADY = M_32G_48G_AXI_awready;
  assign M_32G_48G_AXI_IO_BID = M_32G_48G_AXI_bid[6:0];
  assign M_32G_48G_AXI_IO_BRESP = M_32G_48G_AXI_bresp[1:0];
  assign M_32G_48G_AXI_IO_BVALID = M_32G_48G_AXI_bvalid;
  assign M_32G_48G_AXI_IO_RDATA = M_32G_48G_AXI_rdata[511:0];
  assign M_32G_48G_AXI_IO_RID = M_32G_48G_AXI_rid[6:0];
  assign M_32G_48G_AXI_IO_RLAST = M_32G_48G_AXI_rlast;
  assign M_32G_48G_AXI_IO_RRESP = M_32G_48G_AXI_rresp[1:0];
  assign M_32G_48G_AXI_IO_RVALID = M_32G_48G_AXI_rvalid;
  assign M_32G_48G_AXI_IO_WREADY = M_32G_48G_AXI_wready;
  assign M_32G_48G_AXI_araddr[63:0] = M_32G_48G_AXI_IO_ARADDR;
  assign M_32G_48G_AXI_arburst[1:0] = M_32G_48G_AXI_IO_ARBURST;
  assign M_32G_48G_AXI_arcache[3:0] = M_32G_48G_AXI_IO_ARCACHE;
  assign M_32G_48G_AXI_arid[6:0] = M_32G_48G_AXI_IO_ARID;
  assign M_32G_48G_AXI_arlen[7:0] = M_32G_48G_AXI_IO_ARLEN;
  assign M_32G_48G_AXI_arlock[0] = M_32G_48G_AXI_IO_ARLOCK;
  assign M_32G_48G_AXI_arprot[2:0] = M_32G_48G_AXI_IO_ARPROT;
  assign M_32G_48G_AXI_arqos[3:0] = M_32G_48G_AXI_IO_ARQOS;
  assign M_32G_48G_AXI_arregion[3:0] = M_32G_48G_AXI_IO_ARREGION;
  assign M_32G_48G_AXI_arsize[2:0] = M_32G_48G_AXI_IO_ARSIZE;
  assign M_32G_48G_AXI_arvalid = M_32G_48G_AXI_IO_ARVALID;
  assign M_32G_48G_AXI_awaddr[63:0] = M_32G_48G_AXI_IO_AWADDR;
  assign M_32G_48G_AXI_awburst[1:0] = M_32G_48G_AXI_IO_AWBURST;
  assign M_32G_48G_AXI_awcache[3:0] = M_32G_48G_AXI_IO_AWCACHE;
  assign M_32G_48G_AXI_awid[6:0] = M_32G_48G_AXI_IO_AWID;
  assign M_32G_48G_AXI_awlen[7:0] = M_32G_48G_AXI_IO_AWLEN;
  assign M_32G_48G_AXI_awlock[0] = M_32G_48G_AXI_IO_AWLOCK;
  assign M_32G_48G_AXI_awprot[2:0] = M_32G_48G_AXI_IO_AWPROT;
  assign M_32G_48G_AXI_awqos[3:0] = M_32G_48G_AXI_IO_AWQOS;
  assign M_32G_48G_AXI_awregion[3:0] = M_32G_48G_AXI_IO_AWREGION;
  assign M_32G_48G_AXI_awsize[2:0] = M_32G_48G_AXI_IO_AWSIZE;
  assign M_32G_48G_AXI_awvalid = M_32G_48G_AXI_IO_AWVALID;
  assign M_32G_48G_AXI_bready = M_32G_48G_AXI_IO_BREADY;
  assign M_32G_48G_AXI_rready = M_32G_48G_AXI_IO_RREADY;
  assign M_32G_48G_AXI_wdata[511:0] = M_32G_48G_AXI_IO_WDATA;
  assign M_32G_48G_AXI_wlast = M_32G_48G_AXI_IO_WLAST;
  assign M_32G_48G_AXI_wstrb[63:0] = M_32G_48G_AXI_IO_WSTRB;
  assign M_32G_48G_AXI_wvalid = M_32G_48G_AXI_IO_WVALID;
  assign M_48G_64G_AXI_IO_ARREADY = M_48G_64G_AXI_arready;
  assign M_48G_64G_AXI_IO_AWREADY = M_48G_64G_AXI_awready;
  assign M_48G_64G_AXI_IO_BID = M_48G_64G_AXI_bid[6:0];
  assign M_48G_64G_AXI_IO_BRESP = M_48G_64G_AXI_bresp[1:0];
  assign M_48G_64G_AXI_IO_BVALID = M_48G_64G_AXI_bvalid;
  assign M_48G_64G_AXI_IO_RDATA = M_48G_64G_AXI_rdata[511:0];
  assign M_48G_64G_AXI_IO_RID = M_48G_64G_AXI_rid[6:0];
  assign M_48G_64G_AXI_IO_RLAST = M_48G_64G_AXI_rlast;
  assign M_48G_64G_AXI_IO_RRESP = M_48G_64G_AXI_rresp[1:0];
  assign M_48G_64G_AXI_IO_RVALID = M_48G_64G_AXI_rvalid;
  assign M_48G_64G_AXI_IO_WREADY = M_48G_64G_AXI_wready;
  assign M_48G_64G_AXI_araddr[63:0] = M_48G_64G_AXI_IO_ARADDR;
  assign M_48G_64G_AXI_arburst[1:0] = M_48G_64G_AXI_IO_ARBURST;
  assign M_48G_64G_AXI_arcache[3:0] = M_48G_64G_AXI_IO_ARCACHE;
  assign M_48G_64G_AXI_arid[6:0] = M_48G_64G_AXI_IO_ARID;
  assign M_48G_64G_AXI_arlen[7:0] = M_48G_64G_AXI_IO_ARLEN;
  assign M_48G_64G_AXI_arlock[0] = M_48G_64G_AXI_IO_ARLOCK;
  assign M_48G_64G_AXI_arprot[2:0] = M_48G_64G_AXI_IO_ARPROT;
  assign M_48G_64G_AXI_arqos[3:0] = M_48G_64G_AXI_IO_ARQOS;
  assign M_48G_64G_AXI_arregion[3:0] = M_48G_64G_AXI_IO_ARREGION;
  assign M_48G_64G_AXI_arsize[2:0] = M_48G_64G_AXI_IO_ARSIZE;
  assign M_48G_64G_AXI_arvalid = M_48G_64G_AXI_IO_ARVALID;
  assign M_48G_64G_AXI_awaddr[63:0] = M_48G_64G_AXI_IO_AWADDR;
  assign M_48G_64G_AXI_awburst[1:0] = M_48G_64G_AXI_IO_AWBURST;
  assign M_48G_64G_AXI_awcache[3:0] = M_48G_64G_AXI_IO_AWCACHE;
  assign M_48G_64G_AXI_awid[6:0] = M_48G_64G_AXI_IO_AWID;
  assign M_48G_64G_AXI_awlen[7:0] = M_48G_64G_AXI_IO_AWLEN;
  assign M_48G_64G_AXI_awlock[0] = M_48G_64G_AXI_IO_AWLOCK;
  assign M_48G_64G_AXI_awprot[2:0] = M_48G_64G_AXI_IO_AWPROT;
  assign M_48G_64G_AXI_awqos[3:0] = M_48G_64G_AXI_IO_AWQOS;
  assign M_48G_64G_AXI_awregion[3:0] = M_48G_64G_AXI_IO_AWREGION;
  assign M_48G_64G_AXI_awsize[2:0] = M_48G_64G_AXI_IO_AWSIZE;
  assign M_48G_64G_AXI_awvalid = M_48G_64G_AXI_IO_AWVALID;
  assign M_48G_64G_AXI_bready = M_48G_64G_AXI_IO_BREADY;
  assign M_48G_64G_AXI_rready = M_48G_64G_AXI_IO_RREADY;
  assign M_48G_64G_AXI_wdata[511:0] = M_48G_64G_AXI_IO_WDATA;
  assign M_48G_64G_AXI_wlast = M_48G_64G_AXI_IO_WLAST;
  assign M_48G_64G_AXI_wstrb[63:0] = M_48G_64G_AXI_IO_WSTRB;
  assign M_48G_64G_AXI_wvalid = M_48G_64G_AXI_IO_WVALID;
  assign M_AXI_RTL_IO_ARREADY = M_AXI_RTL_arready;
  assign M_AXI_RTL_IO_AWREADY = M_AXI_RTL_awready;
  assign M_AXI_RTL_IO_BID = M_AXI_RTL_bid[5:0];
  assign M_AXI_RTL_IO_BRESP = M_AXI_RTL_bresp[1:0];
  assign M_AXI_RTL_IO_BVALID = M_AXI_RTL_bvalid;
  assign M_AXI_RTL_IO_RDATA = M_AXI_RTL_rdata[511:0];
  assign M_AXI_RTL_IO_RID = M_AXI_RTL_rid[5:0];
  assign M_AXI_RTL_IO_RLAST = M_AXI_RTL_rlast;
  assign M_AXI_RTL_IO_RRESP = M_AXI_RTL_rresp[1:0];
  assign M_AXI_RTL_IO_RVALID = M_AXI_RTL_rvalid;
  assign M_AXI_RTL_IO_WREADY = M_AXI_RTL_wready;
  assign M_AXI_RTL_araddr[63:0] = M_AXI_RTL_IO_ARADDR;
  assign M_AXI_RTL_arburst[1:0] = M_AXI_RTL_IO_ARBURST;
  assign M_AXI_RTL_arcache[3:0] = M_AXI_RTL_IO_ARCACHE;
  assign M_AXI_RTL_arid[5:0] = M_AXI_RTL_IO_ARID;
  assign M_AXI_RTL_arlen[7:0] = M_AXI_RTL_IO_ARLEN;
  assign M_AXI_RTL_arlock[0] = M_AXI_RTL_IO_ARLOCK;
  assign M_AXI_RTL_arprot[2:0] = M_AXI_RTL_IO_ARPROT;
  assign M_AXI_RTL_arqos[3:0] = M_AXI_RTL_IO_ARQOS;
  assign M_AXI_RTL_arregion[3:0] = M_AXI_RTL_IO_ARREGION;
  assign M_AXI_RTL_arsize[2:0] = M_AXI_RTL_IO_ARSIZE;
  assign M_AXI_RTL_arvalid = M_AXI_RTL_IO_ARVALID;
  assign M_AXI_RTL_awaddr[63:0] = M_AXI_RTL_IO_AWADDR;
  assign M_AXI_RTL_awburst[1:0] = M_AXI_RTL_IO_AWBURST;
  assign M_AXI_RTL_awcache[3:0] = M_AXI_RTL_IO_AWCACHE;
  assign M_AXI_RTL_awid[5:0] = M_AXI_RTL_IO_AWID;
  assign M_AXI_RTL_awlen[7:0] = M_AXI_RTL_IO_AWLEN;
  assign M_AXI_RTL_awlock[0] = M_AXI_RTL_IO_AWLOCK;
  assign M_AXI_RTL_awprot[2:0] = M_AXI_RTL_IO_AWPROT;
  assign M_AXI_RTL_awqos[3:0] = M_AXI_RTL_IO_AWQOS;
  assign M_AXI_RTL_awregion[3:0] = M_AXI_RTL_IO_AWREGION;
  assign M_AXI_RTL_awsize[2:0] = M_AXI_RTL_IO_AWSIZE;
  assign M_AXI_RTL_awvalid = M_AXI_RTL_IO_AWVALID;
  assign M_AXI_RTL_bready = M_AXI_RTL_IO_BREADY;
  assign M_AXI_RTL_rready = M_AXI_RTL_IO_RREADY;
  assign M_AXI_RTL_wdata[511:0] = M_AXI_RTL_IO_WDATA;
  assign M_AXI_RTL_wlast = M_AXI_RTL_IO_WLAST;
  assign M_AXI_RTL_wstrb[63:0] = M_AXI_RTL_IO_WSTRB;
  assign M_AXI_RTL_wvalid = M_AXI_RTL_IO_WVALID;
  assign S_AXI_PCIS_IO_ARADDR = S_AXI_PCIS_araddr[63:0];
  assign S_AXI_PCIS_IO_ARBURST = S_AXI_PCIS_arburst[1:0];
  assign S_AXI_PCIS_IO_ARCACHE = S_AXI_PCIS_arcache[3:0];
  assign S_AXI_PCIS_IO_ARID = S_AXI_PCIS_arid[5:0];
  assign S_AXI_PCIS_IO_ARLEN = S_AXI_PCIS_arlen[7:0];
  assign S_AXI_PCIS_IO_ARLOCK = S_AXI_PCIS_arlock[0];
  assign S_AXI_PCIS_IO_ARPROT = S_AXI_PCIS_arprot[2:0];
  assign S_AXI_PCIS_IO_ARQOS = S_AXI_PCIS_arqos[3:0];
  assign S_AXI_PCIS_IO_ARREGION = S_AXI_PCIS_arregion[3:0];
  assign S_AXI_PCIS_IO_ARSIZE = S_AXI_PCIS_arsize[2:0];
  assign S_AXI_PCIS_IO_ARVALID = S_AXI_PCIS_arvalid;
  assign S_AXI_PCIS_IO_AWADDR = S_AXI_PCIS_awaddr[63:0];
  assign S_AXI_PCIS_IO_AWBURST = S_AXI_PCIS_awburst[1:0];
  assign S_AXI_PCIS_IO_AWCACHE = S_AXI_PCIS_awcache[3:0];
  assign S_AXI_PCIS_IO_AWID = S_AXI_PCIS_awid[5:0];
  assign S_AXI_PCIS_IO_AWLEN = S_AXI_PCIS_awlen[7:0];
  assign S_AXI_PCIS_IO_AWLOCK = S_AXI_PCIS_awlock[0];
  assign S_AXI_PCIS_IO_AWPROT = S_AXI_PCIS_awprot[2:0];
  assign S_AXI_PCIS_IO_AWQOS = S_AXI_PCIS_awqos[3:0];
  assign S_AXI_PCIS_IO_AWREGION = S_AXI_PCIS_awregion[3:0];
  assign S_AXI_PCIS_IO_AWSIZE = S_AXI_PCIS_awsize[2:0];
  assign S_AXI_PCIS_IO_AWVALID = S_AXI_PCIS_awvalid;
  assign S_AXI_PCIS_IO_BREADY = S_AXI_PCIS_bready;
  assign S_AXI_PCIS_IO_RREADY = S_AXI_PCIS_rready;
  assign S_AXI_PCIS_IO_WDATA = S_AXI_PCIS_wdata[511:0];
  assign S_AXI_PCIS_IO_WLAST = S_AXI_PCIS_wlast;
  assign S_AXI_PCIS_IO_WSTRB = S_AXI_PCIS_wstrb[63:0];
  assign S_AXI_PCIS_IO_WVALID = S_AXI_PCIS_wvalid;
  assign S_AXI_PCIS_arready = S_AXI_PCIS_IO_ARREADY;
  assign S_AXI_PCIS_awready = S_AXI_PCIS_IO_AWREADY;
  assign S_AXI_PCIS_bid[5:0] = S_AXI_PCIS_IO_BID;
  assign S_AXI_PCIS_bresp[1:0] = S_AXI_PCIS_IO_BRESP;
  assign S_AXI_PCIS_bvalid = S_AXI_PCIS_IO_BVALID;
  assign S_AXI_PCIS_rdata[511:0] = S_AXI_PCIS_IO_RDATA;
  assign S_AXI_PCIS_rid[5:0] = S_AXI_PCIS_IO_RID;
  assign S_AXI_PCIS_rlast = S_AXI_PCIS_IO_RLAST;
  assign S_AXI_PCIS_rresp[1:0] = S_AXI_PCIS_IO_RRESP;
  assign S_AXI_PCIS_rvalid = S_AXI_PCIS_IO_RVALID;
  assign S_AXI_PCIS_wready = S_AXI_PCIS_IO_WREADY;
  assign S_RTL_DRAM_AXI_IO_ARADDR = S_RTL_DRAM_AXI_araddr[63:0];
  assign S_RTL_DRAM_AXI_IO_ARBURST = S_RTL_DRAM_AXI_arburst[1:0];
  assign S_RTL_DRAM_AXI_IO_ARCACHE = S_RTL_DRAM_AXI_arcache[3:0];
  assign S_RTL_DRAM_AXI_IO_ARID = S_RTL_DRAM_AXI_arid[5:0];
  assign S_RTL_DRAM_AXI_IO_ARLEN = S_RTL_DRAM_AXI_arlen[7:0];
  assign S_RTL_DRAM_AXI_IO_ARLOCK = S_RTL_DRAM_AXI_arlock[0];
  assign S_RTL_DRAM_AXI_IO_ARPROT = S_RTL_DRAM_AXI_arprot[2:0];
  assign S_RTL_DRAM_AXI_IO_ARQOS = S_RTL_DRAM_AXI_arqos[3:0];
  assign S_RTL_DRAM_AXI_IO_ARREGION = S_RTL_DRAM_AXI_arregion[3:0];
  assign S_RTL_DRAM_AXI_IO_ARSIZE = S_RTL_DRAM_AXI_arsize[2:0];
  assign S_RTL_DRAM_AXI_IO_ARVALID = S_RTL_DRAM_AXI_arvalid;
  assign S_RTL_DRAM_AXI_IO_AWADDR = S_RTL_DRAM_AXI_awaddr[63:0];
  assign S_RTL_DRAM_AXI_IO_AWBURST = S_RTL_DRAM_AXI_awburst[1:0];
  assign S_RTL_DRAM_AXI_IO_AWCACHE = S_RTL_DRAM_AXI_awcache[3:0];
  assign S_RTL_DRAM_AXI_IO_AWID = S_RTL_DRAM_AXI_awid[5:0];
  assign S_RTL_DRAM_AXI_IO_AWLEN = S_RTL_DRAM_AXI_awlen[7:0];
  assign S_RTL_DRAM_AXI_IO_AWLOCK = S_RTL_DRAM_AXI_awlock[0];
  assign S_RTL_DRAM_AXI_IO_AWPROT = S_RTL_DRAM_AXI_awprot[2:0];
  assign S_RTL_DRAM_AXI_IO_AWQOS = S_RTL_DRAM_AXI_awqos[3:0];
  assign S_RTL_DRAM_AXI_IO_AWREGION = S_RTL_DRAM_AXI_awregion[3:0];
  assign S_RTL_DRAM_AXI_IO_AWSIZE = S_RTL_DRAM_AXI_awsize[2:0];
  assign S_RTL_DRAM_AXI_IO_AWVALID = S_RTL_DRAM_AXI_awvalid;
  assign S_RTL_DRAM_AXI_IO_BREADY = S_RTL_DRAM_AXI_bready;
  assign S_RTL_DRAM_AXI_IO_RREADY = S_RTL_DRAM_AXI_rready;
  assign S_RTL_DRAM_AXI_IO_WDATA = S_RTL_DRAM_AXI_wdata[511:0];
  assign S_RTL_DRAM_AXI_IO_WLAST = S_RTL_DRAM_AXI_wlast;
  assign S_RTL_DRAM_AXI_IO_WSTRB = S_RTL_DRAM_AXI_wstrb[63:0];
  assign S_RTL_DRAM_AXI_IO_WVALID = S_RTL_DRAM_AXI_wvalid;
  assign S_RTL_DRAM_AXI_arready = S_RTL_DRAM_AXI_IO_ARREADY;
  assign S_RTL_DRAM_AXI_awready = S_RTL_DRAM_AXI_IO_AWREADY;
  assign S_RTL_DRAM_AXI_bid[5:0] = S_RTL_DRAM_AXI_IO_BID;
  assign S_RTL_DRAM_AXI_bresp[1:0] = S_RTL_DRAM_AXI_IO_BRESP;
  assign S_RTL_DRAM_AXI_bvalid = S_RTL_DRAM_AXI_IO_BVALID;
  assign S_RTL_DRAM_AXI_rdata[511:0] = S_RTL_DRAM_AXI_IO_RDATA;
  assign S_RTL_DRAM_AXI_rid[5:0] = S_RTL_DRAM_AXI_IO_RID;
  assign S_RTL_DRAM_AXI_rlast = S_RTL_DRAM_AXI_IO_RLAST;
  assign S_RTL_DRAM_AXI_rresp[1:0] = S_RTL_DRAM_AXI_IO_RRESP;
  assign S_RTL_DRAM_AXI_rvalid = S_RTL_DRAM_AXI_IO_RVALID;
  assign S_RTL_DRAM_AXI_wready = S_RTL_DRAM_AXI_IO_WREADY;
  cl_xbar_axi_dram_xbar_0 axi_dram_xbar
       (.ACLK(ACLK_WIRE),
        .ARESETN(ARESETN_WIRE),
        .M00_ACLK(ACLK_WIRE),
        .M00_ARESETN(ARESETN_WIRE),
        .M00_AXI_araddr(M_00G_16G_AXI_IO_ARADDR),
        .M00_AXI_arburst(M_00G_16G_AXI_IO_ARBURST),
        .M00_AXI_arcache(M_00G_16G_AXI_IO_ARCACHE),
        .M00_AXI_arid(M_00G_16G_AXI_IO_ARID),
        .M00_AXI_arlen(M_00G_16G_AXI_IO_ARLEN),
        .M00_AXI_arlock(M_00G_16G_AXI_IO_ARLOCK),
        .M00_AXI_arprot(M_00G_16G_AXI_IO_ARPROT),
        .M00_AXI_arqos(M_00G_16G_AXI_IO_ARQOS),
        .M00_AXI_arready(M_00G_16G_AXI_IO_ARREADY),
        .M00_AXI_arregion(M_00G_16G_AXI_IO_ARREGION),
        .M00_AXI_arsize(M_00G_16G_AXI_IO_ARSIZE),
        .M00_AXI_arvalid(M_00G_16G_AXI_IO_ARVALID),
        .M00_AXI_awaddr(M_00G_16G_AXI_IO_AWADDR),
        .M00_AXI_awburst(M_00G_16G_AXI_IO_AWBURST),
        .M00_AXI_awcache(M_00G_16G_AXI_IO_AWCACHE),
        .M00_AXI_awid(M_00G_16G_AXI_IO_AWID),
        .M00_AXI_awlen(M_00G_16G_AXI_IO_AWLEN),
        .M00_AXI_awlock(M_00G_16G_AXI_IO_AWLOCK),
        .M00_AXI_awprot(M_00G_16G_AXI_IO_AWPROT),
        .M00_AXI_awqos(M_00G_16G_AXI_IO_AWQOS),
        .M00_AXI_awready(M_00G_16G_AXI_IO_AWREADY),
        .M00_AXI_awregion(M_00G_16G_AXI_IO_AWREGION),
        .M00_AXI_awsize(M_00G_16G_AXI_IO_AWSIZE),
        .M00_AXI_awvalid(M_00G_16G_AXI_IO_AWVALID),
        .M00_AXI_bid(M_00G_16G_AXI_IO_BID),
        .M00_AXI_bready(M_00G_16G_AXI_IO_BREADY),
        .M00_AXI_bresp(M_00G_16G_AXI_IO_BRESP),
        .M00_AXI_bvalid(M_00G_16G_AXI_IO_BVALID),
        .M00_AXI_rdata(M_00G_16G_AXI_IO_RDATA),
        .M00_AXI_rid(M_00G_16G_AXI_IO_RID),
        .M00_AXI_rlast(M_00G_16G_AXI_IO_RLAST),
        .M00_AXI_rready(M_00G_16G_AXI_IO_RREADY),
        .M00_AXI_rresp(M_00G_16G_AXI_IO_RRESP),
        .M00_AXI_rvalid(M_00G_16G_AXI_IO_RVALID),
        .M00_AXI_wdata(M_00G_16G_AXI_IO_WDATA),
        .M00_AXI_wlast(M_00G_16G_AXI_IO_WLAST),
        .M00_AXI_wready(M_00G_16G_AXI_IO_WREADY),
        .M00_AXI_wstrb(M_00G_16G_AXI_IO_WSTRB),
        .M00_AXI_wvalid(M_00G_16G_AXI_IO_WVALID),
        .M01_ACLK(ACLK_WIRE),
        .M01_ARESETN(ARESETN_WIRE),
        .M01_AXI_araddr(M_16G_32G_AXI_IO_ARADDR),
        .M01_AXI_arburst(M_16G_32G_AXI_IO_ARBURST),
        .M01_AXI_arcache(M_16G_32G_AXI_IO_ARCACHE),
        .M01_AXI_arid(M_16G_32G_AXI_IO_ARID),
        .M01_AXI_arlen(M_16G_32G_AXI_IO_ARLEN),
        .M01_AXI_arlock(M_16G_32G_AXI_IO_ARLOCK),
        .M01_AXI_arprot(M_16G_32G_AXI_IO_ARPROT),
        .M01_AXI_arqos(M_16G_32G_AXI_IO_ARQOS),
        .M01_AXI_arready(M_16G_32G_AXI_IO_ARREADY),
        .M01_AXI_arregion(M_16G_32G_AXI_IO_ARREGION),
        .M01_AXI_arsize(M_16G_32G_AXI_IO_ARSIZE),
        .M01_AXI_arvalid(M_16G_32G_AXI_IO_ARVALID),
        .M01_AXI_awaddr(M_16G_32G_AXI_IO_AWADDR),
        .M01_AXI_awburst(M_16G_32G_AXI_IO_AWBURST),
        .M01_AXI_awcache(M_16G_32G_AXI_IO_AWCACHE),
        .M01_AXI_awid(M_16G_32G_AXI_IO_AWID),
        .M01_AXI_awlen(M_16G_32G_AXI_IO_AWLEN),
        .M01_AXI_awlock(M_16G_32G_AXI_IO_AWLOCK),
        .M01_AXI_awprot(M_16G_32G_AXI_IO_AWPROT),
        .M01_AXI_awqos(M_16G_32G_AXI_IO_AWQOS),
        .M01_AXI_awready(M_16G_32G_AXI_IO_AWREADY),
        .M01_AXI_awregion(M_16G_32G_AXI_IO_AWREGION),
        .M01_AXI_awsize(M_16G_32G_AXI_IO_AWSIZE),
        .M01_AXI_awvalid(M_16G_32G_AXI_IO_AWVALID),
        .M01_AXI_bid(M_16G_32G_AXI_IO_BID),
        .M01_AXI_bready(M_16G_32G_AXI_IO_BREADY),
        .M01_AXI_bresp(M_16G_32G_AXI_IO_BRESP),
        .M01_AXI_bvalid(M_16G_32G_AXI_IO_BVALID),
        .M01_AXI_rdata(M_16G_32G_AXI_IO_RDATA),
        .M01_AXI_rid(M_16G_32G_AXI_IO_RID),
        .M01_AXI_rlast(M_16G_32G_AXI_IO_RLAST),
        .M01_AXI_rready(M_16G_32G_AXI_IO_RREADY),
        .M01_AXI_rresp(M_16G_32G_AXI_IO_RRESP),
        .M01_AXI_rvalid(M_16G_32G_AXI_IO_RVALID),
        .M01_AXI_wdata(M_16G_32G_AXI_IO_WDATA),
        .M01_AXI_wlast(M_16G_32G_AXI_IO_WLAST),
        .M01_AXI_wready(M_16G_32G_AXI_IO_WREADY),
        .M01_AXI_wstrb(M_16G_32G_AXI_IO_WSTRB),
        .M01_AXI_wvalid(M_16G_32G_AXI_IO_WVALID),
        .M02_ACLK(ACLK_WIRE),
        .M02_ARESETN(ARESETN_WIRE),
        .M02_AXI_araddr(M_32G_48G_AXI_IO_ARADDR),
        .M02_AXI_arburst(M_32G_48G_AXI_IO_ARBURST),
        .M02_AXI_arcache(M_32G_48G_AXI_IO_ARCACHE),
        .M02_AXI_arid(M_32G_48G_AXI_IO_ARID),
        .M02_AXI_arlen(M_32G_48G_AXI_IO_ARLEN),
        .M02_AXI_arlock(M_32G_48G_AXI_IO_ARLOCK),
        .M02_AXI_arprot(M_32G_48G_AXI_IO_ARPROT),
        .M02_AXI_arqos(M_32G_48G_AXI_IO_ARQOS),
        .M02_AXI_arready(M_32G_48G_AXI_IO_ARREADY),
        .M02_AXI_arregion(M_32G_48G_AXI_IO_ARREGION),
        .M02_AXI_arsize(M_32G_48G_AXI_IO_ARSIZE),
        .M02_AXI_arvalid(M_32G_48G_AXI_IO_ARVALID),
        .M02_AXI_awaddr(M_32G_48G_AXI_IO_AWADDR),
        .M02_AXI_awburst(M_32G_48G_AXI_IO_AWBURST),
        .M02_AXI_awcache(M_32G_48G_AXI_IO_AWCACHE),
        .M02_AXI_awid(M_32G_48G_AXI_IO_AWID),
        .M02_AXI_awlen(M_32G_48G_AXI_IO_AWLEN),
        .M02_AXI_awlock(M_32G_48G_AXI_IO_AWLOCK),
        .M02_AXI_awprot(M_32G_48G_AXI_IO_AWPROT),
        .M02_AXI_awqos(M_32G_48G_AXI_IO_AWQOS),
        .M02_AXI_awready(M_32G_48G_AXI_IO_AWREADY),
        .M02_AXI_awregion(M_32G_48G_AXI_IO_AWREGION),
        .M02_AXI_awsize(M_32G_48G_AXI_IO_AWSIZE),
        .M02_AXI_awvalid(M_32G_48G_AXI_IO_AWVALID),
        .M02_AXI_bid(M_32G_48G_AXI_IO_BID),
        .M02_AXI_bready(M_32G_48G_AXI_IO_BREADY),
        .M02_AXI_bresp(M_32G_48G_AXI_IO_BRESP),
        .M02_AXI_bvalid(M_32G_48G_AXI_IO_BVALID),
        .M02_AXI_rdata(M_32G_48G_AXI_IO_RDATA),
        .M02_AXI_rid(M_32G_48G_AXI_IO_RID),
        .M02_AXI_rlast(M_32G_48G_AXI_IO_RLAST),
        .M02_AXI_rready(M_32G_48G_AXI_IO_RREADY),
        .M02_AXI_rresp(M_32G_48G_AXI_IO_RRESP),
        .M02_AXI_rvalid(M_32G_48G_AXI_IO_RVALID),
        .M02_AXI_wdata(M_32G_48G_AXI_IO_WDATA),
        .M02_AXI_wlast(M_32G_48G_AXI_IO_WLAST),
        .M02_AXI_wready(M_32G_48G_AXI_IO_WREADY),
        .M02_AXI_wstrb(M_32G_48G_AXI_IO_WSTRB),
        .M02_AXI_wvalid(M_32G_48G_AXI_IO_WVALID),
        .M03_ACLK(ACLK_WIRE),
        .M03_ARESETN(ARESETN_WIRE),
        .M03_AXI_araddr(M_48G_64G_AXI_IO_ARADDR),
        .M03_AXI_arburst(M_48G_64G_AXI_IO_ARBURST),
        .M03_AXI_arcache(M_48G_64G_AXI_IO_ARCACHE),
        .M03_AXI_arid(M_48G_64G_AXI_IO_ARID),
        .M03_AXI_arlen(M_48G_64G_AXI_IO_ARLEN),
        .M03_AXI_arlock(M_48G_64G_AXI_IO_ARLOCK),
        .M03_AXI_arprot(M_48G_64G_AXI_IO_ARPROT),
        .M03_AXI_arqos(M_48G_64G_AXI_IO_ARQOS),
        .M03_AXI_arready(M_48G_64G_AXI_IO_ARREADY),
        .M03_AXI_arregion(M_48G_64G_AXI_IO_ARREGION),
        .M03_AXI_arsize(M_48G_64G_AXI_IO_ARSIZE),
        .M03_AXI_arvalid(M_48G_64G_AXI_IO_ARVALID),
        .M03_AXI_awaddr(M_48G_64G_AXI_IO_AWADDR),
        .M03_AXI_awburst(M_48G_64G_AXI_IO_AWBURST),
        .M03_AXI_awcache(M_48G_64G_AXI_IO_AWCACHE),
        .M03_AXI_awid(M_48G_64G_AXI_IO_AWID),
        .M03_AXI_awlen(M_48G_64G_AXI_IO_AWLEN),
        .M03_AXI_awlock(M_48G_64G_AXI_IO_AWLOCK),
        .M03_AXI_awprot(M_48G_64G_AXI_IO_AWPROT),
        .M03_AXI_awqos(M_48G_64G_AXI_IO_AWQOS),
        .M03_AXI_awready(M_48G_64G_AXI_IO_AWREADY),
        .M03_AXI_awregion(M_48G_64G_AXI_IO_AWREGION),
        .M03_AXI_awsize(M_48G_64G_AXI_IO_AWSIZE),
        .M03_AXI_awvalid(M_48G_64G_AXI_IO_AWVALID),
        .M03_AXI_bid(M_48G_64G_AXI_IO_BID),
        .M03_AXI_bready(M_48G_64G_AXI_IO_BREADY),
        .M03_AXI_bresp(M_48G_64G_AXI_IO_BRESP),
        .M03_AXI_bvalid(M_48G_64G_AXI_IO_BVALID),
        .M03_AXI_rdata(M_48G_64G_AXI_IO_RDATA),
        .M03_AXI_rid(M_48G_64G_AXI_IO_RID),
        .M03_AXI_rlast(M_48G_64G_AXI_IO_RLAST),
        .M03_AXI_rready(M_48G_64G_AXI_IO_RREADY),
        .M03_AXI_rresp(M_48G_64G_AXI_IO_RRESP),
        .M03_AXI_rvalid(M_48G_64G_AXI_IO_RVALID),
        .M03_AXI_wdata(M_48G_64G_AXI_IO_WDATA),
        .M03_AXI_wlast(M_48G_64G_AXI_IO_WLAST),
        .M03_AXI_wready(M_48G_64G_AXI_IO_WREADY),
        .M03_AXI_wstrb(M_48G_64G_AXI_IO_WSTRB),
        .M03_AXI_wvalid(M_48G_64G_AXI_IO_WVALID),
        .S00_ACLK(ACLK_WIRE),
        .S00_ARESETN(ARESETN_WIRE),
        .S00_AXI_araddr(S_PCIS_DRAM_AXI_IO_ARADDR),
        .S00_AXI_arburst(S_PCIS_DRAM_AXI_IO_ARBURST),
        .S00_AXI_arcache(S_PCIS_DRAM_AXI_IO_ARCACHE),
        .S00_AXI_arid(S_PCIS_DRAM_AXI_IO_ARID),
        .S00_AXI_arlen(S_PCIS_DRAM_AXI_IO_ARLEN),
        .S00_AXI_arlock(S_PCIS_DRAM_AXI_IO_ARLOCK),
        .S00_AXI_arprot(S_PCIS_DRAM_AXI_IO_ARPROT),
        .S00_AXI_arqos(S_PCIS_DRAM_AXI_IO_ARQOS),
        .S00_AXI_arready(S_PCIS_DRAM_AXI_IO_ARREADY),
        .S00_AXI_arregion(S_PCIS_DRAM_AXI_IO_ARREGION),
        .S00_AXI_arsize(S_PCIS_DRAM_AXI_IO_ARSIZE),
        .S00_AXI_arvalid(S_PCIS_DRAM_AXI_IO_ARVALID),
        .S00_AXI_awaddr(S_PCIS_DRAM_AXI_IO_AWADDR),
        .S00_AXI_awburst(S_PCIS_DRAM_AXI_IO_AWBURST),
        .S00_AXI_awcache(S_PCIS_DRAM_AXI_IO_AWCACHE),
        .S00_AXI_awid(S_PCIS_DRAM_AXI_IO_AWID),
        .S00_AXI_awlen(S_PCIS_DRAM_AXI_IO_AWLEN),
        .S00_AXI_awlock(S_PCIS_DRAM_AXI_IO_AWLOCK),
        .S00_AXI_awprot(S_PCIS_DRAM_AXI_IO_AWPROT),
        .S00_AXI_awqos(S_PCIS_DRAM_AXI_IO_AWQOS),
        .S00_AXI_awready(S_PCIS_DRAM_AXI_IO_AWREADY),
        .S00_AXI_awregion(S_PCIS_DRAM_AXI_IO_AWREGION),
        .S00_AXI_awsize(S_PCIS_DRAM_AXI_IO_AWSIZE),
        .S00_AXI_awvalid(S_PCIS_DRAM_AXI_IO_AWVALID),
        .S00_AXI_bid(S_PCIS_DRAM_AXI_IO_BID),
        .S00_AXI_bready(S_PCIS_DRAM_AXI_IO_BREADY),
        .S00_AXI_bresp(S_PCIS_DRAM_AXI_IO_BRESP),
        .S00_AXI_bvalid(S_PCIS_DRAM_AXI_IO_BVALID),
        .S00_AXI_rdata(S_PCIS_DRAM_AXI_IO_RDATA),
        .S00_AXI_rid(S_PCIS_DRAM_AXI_IO_RID),
        .S00_AXI_rlast(S_PCIS_DRAM_AXI_IO_RLAST),
        .S00_AXI_rready(S_PCIS_DRAM_AXI_IO_RREADY),
        .S00_AXI_rresp(S_PCIS_DRAM_AXI_IO_RRESP),
        .S00_AXI_rvalid(S_PCIS_DRAM_AXI_IO_RVALID),
        .S00_AXI_wdata(S_PCIS_DRAM_AXI_IO_WDATA),
        .S00_AXI_wlast(S_PCIS_DRAM_AXI_IO_WLAST),
        .S00_AXI_wready(S_PCIS_DRAM_AXI_IO_WREADY),
        .S00_AXI_wstrb(S_PCIS_DRAM_AXI_IO_WSTRB),
        .S00_AXI_wvalid(S_PCIS_DRAM_AXI_IO_WVALID),
        .S01_ACLK(ACLK_WIRE),
        .S01_ARESETN(ARESETN_WIRE),
        .S01_AXI_araddr(S_RTL_DRAM_AXI_IO_ARADDR),
        .S01_AXI_arburst(S_RTL_DRAM_AXI_IO_ARBURST),
        .S01_AXI_arcache(S_RTL_DRAM_AXI_IO_ARCACHE),
        .S01_AXI_arid(S_RTL_DRAM_AXI_IO_ARID),
        .S01_AXI_arlen(S_RTL_DRAM_AXI_IO_ARLEN),
        .S01_AXI_arlock(S_RTL_DRAM_AXI_IO_ARLOCK),
        .S01_AXI_arprot(S_RTL_DRAM_AXI_IO_ARPROT),
        .S01_AXI_arqos(S_RTL_DRAM_AXI_IO_ARQOS),
        .S01_AXI_arready(S_RTL_DRAM_AXI_IO_ARREADY),
        .S01_AXI_arregion(S_RTL_DRAM_AXI_IO_ARREGION),
        .S01_AXI_arsize(S_RTL_DRAM_AXI_IO_ARSIZE),
        .S01_AXI_arvalid(S_RTL_DRAM_AXI_IO_ARVALID),
        .S01_AXI_awaddr(S_RTL_DRAM_AXI_IO_AWADDR),
        .S01_AXI_awburst(S_RTL_DRAM_AXI_IO_AWBURST),
        .S01_AXI_awcache(S_RTL_DRAM_AXI_IO_AWCACHE),
        .S01_AXI_awid(S_RTL_DRAM_AXI_IO_AWID),
        .S01_AXI_awlen(S_RTL_DRAM_AXI_IO_AWLEN),
        .S01_AXI_awlock(S_RTL_DRAM_AXI_IO_AWLOCK),
        .S01_AXI_awprot(S_RTL_DRAM_AXI_IO_AWPROT),
        .S01_AXI_awqos(S_RTL_DRAM_AXI_IO_AWQOS),
        .S01_AXI_awready(S_RTL_DRAM_AXI_IO_AWREADY),
        .S01_AXI_awregion(S_RTL_DRAM_AXI_IO_AWREGION),
        .S01_AXI_awsize(S_RTL_DRAM_AXI_IO_AWSIZE),
        .S01_AXI_awvalid(S_RTL_DRAM_AXI_IO_AWVALID),
        .S01_AXI_bid(S_RTL_DRAM_AXI_IO_BID),
        .S01_AXI_bready(S_RTL_DRAM_AXI_IO_BREADY),
        .S01_AXI_bresp(S_RTL_DRAM_AXI_IO_BRESP),
        .S01_AXI_bvalid(S_RTL_DRAM_AXI_IO_BVALID),
        .S01_AXI_rdata(S_RTL_DRAM_AXI_IO_RDATA),
        .S01_AXI_rid(S_RTL_DRAM_AXI_IO_RID),
        .S01_AXI_rlast(S_RTL_DRAM_AXI_IO_RLAST),
        .S01_AXI_rready(S_RTL_DRAM_AXI_IO_RREADY),
        .S01_AXI_rresp(S_RTL_DRAM_AXI_IO_RRESP),
        .S01_AXI_rvalid(S_RTL_DRAM_AXI_IO_RVALID),
        .S01_AXI_wdata(S_RTL_DRAM_AXI_IO_WDATA),
        .S01_AXI_wlast(S_RTL_DRAM_AXI_IO_WLAST),
        .S01_AXI_wready(S_RTL_DRAM_AXI_IO_WREADY),
        .S01_AXI_wstrb(S_RTL_DRAM_AXI_IO_WSTRB),
        .S01_AXI_wvalid(S_RTL_DRAM_AXI_IO_WVALID));
  cl_xbar_axi_pcis_xbar_0 axi_pcis_xbar
       (.ACLK(ACLK_WIRE),
        .ARESETN(ARESETN_WIRE),
        .M00_ACLK(ACLK_WIRE),
        .M00_ARESETN(ARESETN_WIRE),
        .M00_AXI_araddr(S_PCIS_DRAM_AXI_IO_ARADDR),
        .M00_AXI_arburst(S_PCIS_DRAM_AXI_IO_ARBURST),
        .M00_AXI_arcache(S_PCIS_DRAM_AXI_IO_ARCACHE),
        .M00_AXI_arid(S_PCIS_DRAM_AXI_IO_ARID),
        .M00_AXI_arlen(S_PCIS_DRAM_AXI_IO_ARLEN),
        .M00_AXI_arlock(S_PCIS_DRAM_AXI_IO_ARLOCK),
        .M00_AXI_arprot(S_PCIS_DRAM_AXI_IO_ARPROT),
        .M00_AXI_arqos(S_PCIS_DRAM_AXI_IO_ARQOS),
        .M00_AXI_arready(S_PCIS_DRAM_AXI_IO_ARREADY),
        .M00_AXI_arregion(S_PCIS_DRAM_AXI_IO_ARREGION),
        .M00_AXI_arsize(S_PCIS_DRAM_AXI_IO_ARSIZE),
        .M00_AXI_arvalid(S_PCIS_DRAM_AXI_IO_ARVALID),
        .M00_AXI_awaddr(S_PCIS_DRAM_AXI_IO_AWADDR),
        .M00_AXI_awburst(S_PCIS_DRAM_AXI_IO_AWBURST),
        .M00_AXI_awcache(S_PCIS_DRAM_AXI_IO_AWCACHE),
        .M00_AXI_awid(S_PCIS_DRAM_AXI_IO_AWID),
        .M00_AXI_awlen(S_PCIS_DRAM_AXI_IO_AWLEN),
        .M00_AXI_awlock(S_PCIS_DRAM_AXI_IO_AWLOCK),
        .M00_AXI_awprot(S_PCIS_DRAM_AXI_IO_AWPROT),
        .M00_AXI_awqos(S_PCIS_DRAM_AXI_IO_AWQOS),
        .M00_AXI_awready(S_PCIS_DRAM_AXI_IO_AWREADY),
        .M00_AXI_awregion(S_PCIS_DRAM_AXI_IO_AWREGION),
        .M00_AXI_awsize(S_PCIS_DRAM_AXI_IO_AWSIZE),
        .M00_AXI_awvalid(S_PCIS_DRAM_AXI_IO_AWVALID),
        .M00_AXI_bid(S_PCIS_DRAM_AXI_IO_BID),
        .M00_AXI_bready(S_PCIS_DRAM_AXI_IO_BREADY),
        .M00_AXI_bresp(S_PCIS_DRAM_AXI_IO_BRESP),
        .M00_AXI_bvalid(S_PCIS_DRAM_AXI_IO_BVALID),
        .M00_AXI_rdata(S_PCIS_DRAM_AXI_IO_RDATA),
        .M00_AXI_rid(S_PCIS_DRAM_AXI_IO_RID),
        .M00_AXI_rlast(S_PCIS_DRAM_AXI_IO_RLAST),
        .M00_AXI_rready(S_PCIS_DRAM_AXI_IO_RREADY),
        .M00_AXI_rresp(S_PCIS_DRAM_AXI_IO_RRESP),
        .M00_AXI_rvalid(S_PCIS_DRAM_AXI_IO_RVALID),
        .M00_AXI_wdata(S_PCIS_DRAM_AXI_IO_WDATA),
        .M00_AXI_wlast(S_PCIS_DRAM_AXI_IO_WLAST),
        .M00_AXI_wready(S_PCIS_DRAM_AXI_IO_WREADY),
        .M00_AXI_wstrb(S_PCIS_DRAM_AXI_IO_WSTRB),
        .M00_AXI_wvalid(S_PCIS_DRAM_AXI_IO_WVALID),
        .M01_ACLK(ACLK_WIRE),
        .M01_ARESETN(ARESETN_WIRE),
        .M01_AXI_araddr(M_AXI_RTL_IO_ARADDR),
        .M01_AXI_arburst(M_AXI_RTL_IO_ARBURST),
        .M01_AXI_arcache(M_AXI_RTL_IO_ARCACHE),
        .M01_AXI_arid(M_AXI_RTL_IO_ARID),
        .M01_AXI_arlen(M_AXI_RTL_IO_ARLEN),
        .M01_AXI_arlock(M_AXI_RTL_IO_ARLOCK),
        .M01_AXI_arprot(M_AXI_RTL_IO_ARPROT),
        .M01_AXI_arqos(M_AXI_RTL_IO_ARQOS),
        .M01_AXI_arready(M_AXI_RTL_IO_ARREADY),
        .M01_AXI_arregion(M_AXI_RTL_IO_ARREGION),
        .M01_AXI_arsize(M_AXI_RTL_IO_ARSIZE),
        .M01_AXI_arvalid(M_AXI_RTL_IO_ARVALID),
        .M01_AXI_awaddr(M_AXI_RTL_IO_AWADDR),
        .M01_AXI_awburst(M_AXI_RTL_IO_AWBURST),
        .M01_AXI_awcache(M_AXI_RTL_IO_AWCACHE),
        .M01_AXI_awid(M_AXI_RTL_IO_AWID),
        .M01_AXI_awlen(M_AXI_RTL_IO_AWLEN),
        .M01_AXI_awlock(M_AXI_RTL_IO_AWLOCK),
        .M01_AXI_awprot(M_AXI_RTL_IO_AWPROT),
        .M01_AXI_awqos(M_AXI_RTL_IO_AWQOS),
        .M01_AXI_awready(M_AXI_RTL_IO_AWREADY),
        .M01_AXI_awregion(M_AXI_RTL_IO_AWREGION),
        .M01_AXI_awsize(M_AXI_RTL_IO_AWSIZE),
        .M01_AXI_awvalid(M_AXI_RTL_IO_AWVALID),
        .M01_AXI_bid(M_AXI_RTL_IO_BID),
        .M01_AXI_bready(M_AXI_RTL_IO_BREADY),
        .M01_AXI_bresp(M_AXI_RTL_IO_BRESP),
        .M01_AXI_bvalid(M_AXI_RTL_IO_BVALID),
        .M01_AXI_rdata(M_AXI_RTL_IO_RDATA),
        .M01_AXI_rid(M_AXI_RTL_IO_RID),
        .M01_AXI_rlast(M_AXI_RTL_IO_RLAST),
        .M01_AXI_rready(M_AXI_RTL_IO_RREADY),
        .M01_AXI_rresp(M_AXI_RTL_IO_RRESP),
        .M01_AXI_rvalid(M_AXI_RTL_IO_RVALID),
        .M01_AXI_wdata(M_AXI_RTL_IO_WDATA),
        .M01_AXI_wlast(M_AXI_RTL_IO_WLAST),
        .M01_AXI_wready(M_AXI_RTL_IO_WREADY),
        .M01_AXI_wstrb(M_AXI_RTL_IO_WSTRB),
        .M01_AXI_wvalid(M_AXI_RTL_IO_WVALID),
        .S00_ACLK(ACLK_WIRE),
        .S00_ARESETN(ARESETN_WIRE),
        .S00_AXI_araddr(S_AXI_PCIS_IO_ARADDR),
        .S00_AXI_arburst(S_AXI_PCIS_IO_ARBURST),
        .S00_AXI_arcache(S_AXI_PCIS_IO_ARCACHE),
        .S00_AXI_arid(S_AXI_PCIS_IO_ARID),
        .S00_AXI_arlen(S_AXI_PCIS_IO_ARLEN),
        .S00_AXI_arlock(S_AXI_PCIS_IO_ARLOCK),
        .S00_AXI_arprot(S_AXI_PCIS_IO_ARPROT),
        .S00_AXI_arqos(S_AXI_PCIS_IO_ARQOS),
        .S00_AXI_arready(S_AXI_PCIS_IO_ARREADY),
        .S00_AXI_arregion(S_AXI_PCIS_IO_ARREGION),
        .S00_AXI_arsize(S_AXI_PCIS_IO_ARSIZE),
        .S00_AXI_arvalid(S_AXI_PCIS_IO_ARVALID),
        .S00_AXI_awaddr(S_AXI_PCIS_IO_AWADDR),
        .S00_AXI_awburst(S_AXI_PCIS_IO_AWBURST),
        .S00_AXI_awcache(S_AXI_PCIS_IO_AWCACHE),
        .S00_AXI_awid(S_AXI_PCIS_IO_AWID),
        .S00_AXI_awlen(S_AXI_PCIS_IO_AWLEN),
        .S00_AXI_awlock(S_AXI_PCIS_IO_AWLOCK),
        .S00_AXI_awprot(S_AXI_PCIS_IO_AWPROT),
        .S00_AXI_awqos(S_AXI_PCIS_IO_AWQOS),
        .S00_AXI_awready(S_AXI_PCIS_IO_AWREADY),
        .S00_AXI_awregion(S_AXI_PCIS_IO_AWREGION),
        .S00_AXI_awsize(S_AXI_PCIS_IO_AWSIZE),
        .S00_AXI_awvalid(S_AXI_PCIS_IO_AWVALID),
        .S00_AXI_bid(S_AXI_PCIS_IO_BID),
        .S00_AXI_bready(S_AXI_PCIS_IO_BREADY),
        .S00_AXI_bresp(S_AXI_PCIS_IO_BRESP),
        .S00_AXI_bvalid(S_AXI_PCIS_IO_BVALID),
        .S00_AXI_rdata(S_AXI_PCIS_IO_RDATA),
        .S00_AXI_rid(S_AXI_PCIS_IO_RID),
        .S00_AXI_rlast(S_AXI_PCIS_IO_RLAST),
        .S00_AXI_rready(S_AXI_PCIS_IO_RREADY),
        .S00_AXI_rresp(S_AXI_PCIS_IO_RRESP),
        .S00_AXI_rvalid(S_AXI_PCIS_IO_RVALID),
        .S00_AXI_wdata(S_AXI_PCIS_IO_WDATA),
        .S00_AXI_wlast(S_AXI_PCIS_IO_WLAST),
        .S00_AXI_wready(S_AXI_PCIS_IO_WREADY),
        .S00_AXI_wstrb(S_AXI_PCIS_IO_WSTRB),
        .S00_AXI_wvalid(S_AXI_PCIS_IO_WVALID));
endmodule

module cl_xbar_axi_dram_xbar_0
   (ACLK,
    ARESETN,
    M00_ACLK,
    M00_ARESETN,
    M00_AXI_araddr,
    M00_AXI_arburst,
    M00_AXI_arcache,
    M00_AXI_arid,
    M00_AXI_arlen,
    M00_AXI_arlock,
    M00_AXI_arprot,
    M00_AXI_arqos,
    M00_AXI_arready,
    M00_AXI_arregion,
    M00_AXI_arsize,
    M00_AXI_arvalid,
    M00_AXI_awaddr,
    M00_AXI_awburst,
    M00_AXI_awcache,
    M00_AXI_awid,
    M00_AXI_awlen,
    M00_AXI_awlock,
    M00_AXI_awprot,
    M00_AXI_awqos,
    M00_AXI_awready,
    M00_AXI_awregion,
    M00_AXI_awsize,
    M00_AXI_awvalid,
    M00_AXI_bid,
    M00_AXI_bready,
    M00_AXI_bresp,
    M00_AXI_bvalid,
    M00_AXI_rdata,
    M00_AXI_rid,
    M00_AXI_rlast,
    M00_AXI_rready,
    M00_AXI_rresp,
    M00_AXI_rvalid,
    M00_AXI_wdata,
    M00_AXI_wlast,
    M00_AXI_wready,
    M00_AXI_wstrb,
    M00_AXI_wvalid,
    M01_ACLK,
    M01_ARESETN,
    M01_AXI_araddr,
    M01_AXI_arburst,
    M01_AXI_arcache,
    M01_AXI_arid,
    M01_AXI_arlen,
    M01_AXI_arlock,
    M01_AXI_arprot,
    M01_AXI_arqos,
    M01_AXI_arready,
    M01_AXI_arregion,
    M01_AXI_arsize,
    M01_AXI_arvalid,
    M01_AXI_awaddr,
    M01_AXI_awburst,
    M01_AXI_awcache,
    M01_AXI_awid,
    M01_AXI_awlen,
    M01_AXI_awlock,
    M01_AXI_awprot,
    M01_AXI_awqos,
    M01_AXI_awready,
    M01_AXI_awregion,
    M01_AXI_awsize,
    M01_AXI_awvalid,
    M01_AXI_bid,
    M01_AXI_bready,
    M01_AXI_bresp,
    M01_AXI_bvalid,
    M01_AXI_rdata,
    M01_AXI_rid,
    M01_AXI_rlast,
    M01_AXI_rready,
    M01_AXI_rresp,
    M01_AXI_rvalid,
    M01_AXI_wdata,
    M01_AXI_wlast,
    M01_AXI_wready,
    M01_AXI_wstrb,
    M01_AXI_wvalid,
    M02_ACLK,
    M02_ARESETN,
    M02_AXI_araddr,
    M02_AXI_arburst,
    M02_AXI_arcache,
    M02_AXI_arid,
    M02_AXI_arlen,
    M02_AXI_arlock,
    M02_AXI_arprot,
    M02_AXI_arqos,
    M02_AXI_arready,
    M02_AXI_arregion,
    M02_AXI_arsize,
    M02_AXI_arvalid,
    M02_AXI_awaddr,
    M02_AXI_awburst,
    M02_AXI_awcache,
    M02_AXI_awid,
    M02_AXI_awlen,
    M02_AXI_awlock,
    M02_AXI_awprot,
    M02_AXI_awqos,
    M02_AXI_awready,
    M02_AXI_awregion,
    M02_AXI_awsize,
    M02_AXI_awvalid,
    M02_AXI_bid,
    M02_AXI_bready,
    M02_AXI_bresp,
    M02_AXI_bvalid,
    M02_AXI_rdata,
    M02_AXI_rid,
    M02_AXI_rlast,
    M02_AXI_rready,
    M02_AXI_rresp,
    M02_AXI_rvalid,
    M02_AXI_wdata,
    M02_AXI_wlast,
    M02_AXI_wready,
    M02_AXI_wstrb,
    M02_AXI_wvalid,
    M03_ACLK,
    M03_ARESETN,
    M03_AXI_araddr,
    M03_AXI_arburst,
    M03_AXI_arcache,
    M03_AXI_arid,
    M03_AXI_arlen,
    M03_AXI_arlock,
    M03_AXI_arprot,
    M03_AXI_arqos,
    M03_AXI_arready,
    M03_AXI_arregion,
    M03_AXI_arsize,
    M03_AXI_arvalid,
    M03_AXI_awaddr,
    M03_AXI_awburst,
    M03_AXI_awcache,
    M03_AXI_awid,
    M03_AXI_awlen,
    M03_AXI_awlock,
    M03_AXI_awprot,
    M03_AXI_awqos,
    M03_AXI_awready,
    M03_AXI_awregion,
    M03_AXI_awsize,
    M03_AXI_awvalid,
    M03_AXI_bid,
    M03_AXI_bready,
    M03_AXI_bresp,
    M03_AXI_bvalid,
    M03_AXI_rdata,
    M03_AXI_rid,
    M03_AXI_rlast,
    M03_AXI_rready,
    M03_AXI_rresp,
    M03_AXI_rvalid,
    M03_AXI_wdata,
    M03_AXI_wlast,
    M03_AXI_wready,
    M03_AXI_wstrb,
    M03_AXI_wvalid,
    S00_ACLK,
    S00_ARESETN,
    S00_AXI_araddr,
    S00_AXI_arburst,
    S00_AXI_arcache,
    S00_AXI_arid,
    S00_AXI_arlen,
    S00_AXI_arlock,
    S00_AXI_arprot,
    S00_AXI_arqos,
    S00_AXI_arready,
    S00_AXI_arregion,
    S00_AXI_arsize,
    S00_AXI_arvalid,
    S00_AXI_awaddr,
    S00_AXI_awburst,
    S00_AXI_awcache,
    S00_AXI_awid,
    S00_AXI_awlen,
    S00_AXI_awlock,
    S00_AXI_awprot,
    S00_AXI_awqos,
    S00_AXI_awready,
    S00_AXI_awregion,
    S00_AXI_awsize,
    S00_AXI_awvalid,
    S00_AXI_bid,
    S00_AXI_bready,
    S00_AXI_bresp,
    S00_AXI_bvalid,
    S00_AXI_rdata,
    S00_AXI_rid,
    S00_AXI_rlast,
    S00_AXI_rready,
    S00_AXI_rresp,
    S00_AXI_rvalid,
    S00_AXI_wdata,
    S00_AXI_wlast,
    S00_AXI_wready,
    S00_AXI_wstrb,
    S00_AXI_wvalid,
    S01_ACLK,
    S01_ARESETN,
    S01_AXI_araddr,
    S01_AXI_arburst,
    S01_AXI_arcache,
    S01_AXI_arid,
    S01_AXI_arlen,
    S01_AXI_arlock,
    S01_AXI_arprot,
    S01_AXI_arqos,
    S01_AXI_arready,
    S01_AXI_arregion,
    S01_AXI_arsize,
    S01_AXI_arvalid,
    S01_AXI_awaddr,
    S01_AXI_awburst,
    S01_AXI_awcache,
    S01_AXI_awid,
    S01_AXI_awlen,
    S01_AXI_awlock,
    S01_AXI_awprot,
    S01_AXI_awqos,
    S01_AXI_awready,
    S01_AXI_awregion,
    S01_AXI_awsize,
    S01_AXI_awvalid,
    S01_AXI_bid,
    S01_AXI_bready,
    S01_AXI_bresp,
    S01_AXI_bvalid,
    S01_AXI_rdata,
    S01_AXI_rid,
    S01_AXI_rlast,
    S01_AXI_rready,
    S01_AXI_rresp,
    S01_AXI_rvalid,
    S01_AXI_wdata,
    S01_AXI_wlast,
    S01_AXI_wready,
    S01_AXI_wstrb,
    S01_AXI_wvalid);
  input ACLK;
  input ARESETN;
  input M00_ACLK;
  input M00_ARESETN;
  output [63:0]M00_AXI_araddr;
  output [1:0]M00_AXI_arburst;
  output [3:0]M00_AXI_arcache;
  output [6:0]M00_AXI_arid;
  output [7:0]M00_AXI_arlen;
  output [0:0]M00_AXI_arlock;
  output [2:0]M00_AXI_arprot;
  output [3:0]M00_AXI_arqos;
  input M00_AXI_arready;
  output [3:0]M00_AXI_arregion;
  output [2:0]M00_AXI_arsize;
  output M00_AXI_arvalid;
  output [63:0]M00_AXI_awaddr;
  output [1:0]M00_AXI_awburst;
  output [3:0]M00_AXI_awcache;
  output [6:0]M00_AXI_awid;
  output [7:0]M00_AXI_awlen;
  output [0:0]M00_AXI_awlock;
  output [2:0]M00_AXI_awprot;
  output [3:0]M00_AXI_awqos;
  input M00_AXI_awready;
  output [3:0]M00_AXI_awregion;
  output [2:0]M00_AXI_awsize;
  output M00_AXI_awvalid;
  input [6:0]M00_AXI_bid;
  output M00_AXI_bready;
  input [1:0]M00_AXI_bresp;
  input M00_AXI_bvalid;
  input [511:0]M00_AXI_rdata;
  input [6:0]M00_AXI_rid;
  input M00_AXI_rlast;
  output M00_AXI_rready;
  input [1:0]M00_AXI_rresp;
  input M00_AXI_rvalid;
  output [511:0]M00_AXI_wdata;
  output M00_AXI_wlast;
  input M00_AXI_wready;
  output [63:0]M00_AXI_wstrb;
  output M00_AXI_wvalid;
  input M01_ACLK;
  input M01_ARESETN;
  output [63:0]M01_AXI_araddr;
  output [1:0]M01_AXI_arburst;
  output [3:0]M01_AXI_arcache;
  output [6:0]M01_AXI_arid;
  output [7:0]M01_AXI_arlen;
  output [0:0]M01_AXI_arlock;
  output [2:0]M01_AXI_arprot;
  output [3:0]M01_AXI_arqos;
  input M01_AXI_arready;
  output [3:0]M01_AXI_arregion;
  output [2:0]M01_AXI_arsize;
  output M01_AXI_arvalid;
  output [63:0]M01_AXI_awaddr;
  output [1:0]M01_AXI_awburst;
  output [3:0]M01_AXI_awcache;
  output [6:0]M01_AXI_awid;
  output [7:0]M01_AXI_awlen;
  output [0:0]M01_AXI_awlock;
  output [2:0]M01_AXI_awprot;
  output [3:0]M01_AXI_awqos;
  input M01_AXI_awready;
  output [3:0]M01_AXI_awregion;
  output [2:0]M01_AXI_awsize;
  output M01_AXI_awvalid;
  input [6:0]M01_AXI_bid;
  output M01_AXI_bready;
  input [1:0]M01_AXI_bresp;
  input M01_AXI_bvalid;
  input [511:0]M01_AXI_rdata;
  input [6:0]M01_AXI_rid;
  input M01_AXI_rlast;
  output M01_AXI_rready;
  input [1:0]M01_AXI_rresp;
  input M01_AXI_rvalid;
  output [511:0]M01_AXI_wdata;
  output M01_AXI_wlast;
  input M01_AXI_wready;
  output [63:0]M01_AXI_wstrb;
  output M01_AXI_wvalid;
  input M02_ACLK;
  input M02_ARESETN;
  output [63:0]M02_AXI_araddr;
  output [1:0]M02_AXI_arburst;
  output [3:0]M02_AXI_arcache;
  output [6:0]M02_AXI_arid;
  output [7:0]M02_AXI_arlen;
  output [0:0]M02_AXI_arlock;
  output [2:0]M02_AXI_arprot;
  output [3:0]M02_AXI_arqos;
  input M02_AXI_arready;
  output [3:0]M02_AXI_arregion;
  output [2:0]M02_AXI_arsize;
  output M02_AXI_arvalid;
  output [63:0]M02_AXI_awaddr;
  output [1:0]M02_AXI_awburst;
  output [3:0]M02_AXI_awcache;
  output [6:0]M02_AXI_awid;
  output [7:0]M02_AXI_awlen;
  output [0:0]M02_AXI_awlock;
  output [2:0]M02_AXI_awprot;
  output [3:0]M02_AXI_awqos;
  input M02_AXI_awready;
  output [3:0]M02_AXI_awregion;
  output [2:0]M02_AXI_awsize;
  output M02_AXI_awvalid;
  input [6:0]M02_AXI_bid;
  output M02_AXI_bready;
  input [1:0]M02_AXI_bresp;
  input M02_AXI_bvalid;
  input [511:0]M02_AXI_rdata;
  input [6:0]M02_AXI_rid;
  input M02_AXI_rlast;
  output M02_AXI_rready;
  input [1:0]M02_AXI_rresp;
  input M02_AXI_rvalid;
  output [511:0]M02_AXI_wdata;
  output M02_AXI_wlast;
  input M02_AXI_wready;
  output [63:0]M02_AXI_wstrb;
  output M02_AXI_wvalid;
  input M03_ACLK;
  input M03_ARESETN;
  output [63:0]M03_AXI_araddr;
  output [1:0]M03_AXI_arburst;
  output [3:0]M03_AXI_arcache;
  output [6:0]M03_AXI_arid;
  output [7:0]M03_AXI_arlen;
  output [0:0]M03_AXI_arlock;
  output [2:0]M03_AXI_arprot;
  output [3:0]M03_AXI_arqos;
  input M03_AXI_arready;
  output [3:0]M03_AXI_arregion;
  output [2:0]M03_AXI_arsize;
  output M03_AXI_arvalid;
  output [63:0]M03_AXI_awaddr;
  output [1:0]M03_AXI_awburst;
  output [3:0]M03_AXI_awcache;
  output [6:0]M03_AXI_awid;
  output [7:0]M03_AXI_awlen;
  output [0:0]M03_AXI_awlock;
  output [2:0]M03_AXI_awprot;
  output [3:0]M03_AXI_awqos;
  input M03_AXI_awready;
  output [3:0]M03_AXI_awregion;
  output [2:0]M03_AXI_awsize;
  output M03_AXI_awvalid;
  input [6:0]M03_AXI_bid;
  output M03_AXI_bready;
  input [1:0]M03_AXI_bresp;
  input M03_AXI_bvalid;
  input [511:0]M03_AXI_rdata;
  input [6:0]M03_AXI_rid;
  input M03_AXI_rlast;
  output M03_AXI_rready;
  input [1:0]M03_AXI_rresp;
  input M03_AXI_rvalid;
  output [511:0]M03_AXI_wdata;
  output M03_AXI_wlast;
  input M03_AXI_wready;
  output [63:0]M03_AXI_wstrb;
  output M03_AXI_wvalid;
  input S00_ACLK;
  input S00_ARESETN;
  input [63:0]S00_AXI_araddr;
  input [1:0]S00_AXI_arburst;
  input [3:0]S00_AXI_arcache;
  input [5:0]S00_AXI_arid;
  input [7:0]S00_AXI_arlen;
  input [0:0]S00_AXI_arlock;
  input [2:0]S00_AXI_arprot;
  input [3:0]S00_AXI_arqos;
  output S00_AXI_arready;
  input [3:0]S00_AXI_arregion;
  input [2:0]S00_AXI_arsize;
  input S00_AXI_arvalid;
  input [63:0]S00_AXI_awaddr;
  input [1:0]S00_AXI_awburst;
  input [3:0]S00_AXI_awcache;
  input [5:0]S00_AXI_awid;
  input [7:0]S00_AXI_awlen;
  input [0:0]S00_AXI_awlock;
  input [2:0]S00_AXI_awprot;
  input [3:0]S00_AXI_awqos;
  output S00_AXI_awready;
  input [3:0]S00_AXI_awregion;
  input [2:0]S00_AXI_awsize;
  input S00_AXI_awvalid;
  output [5:0]S00_AXI_bid;
  input S00_AXI_bready;
  output [1:0]S00_AXI_bresp;
  output S00_AXI_bvalid;
  output [511:0]S00_AXI_rdata;
  output [5:0]S00_AXI_rid;
  output S00_AXI_rlast;
  input S00_AXI_rready;
  output [1:0]S00_AXI_rresp;
  output S00_AXI_rvalid;
  input [511:0]S00_AXI_wdata;
  input S00_AXI_wlast;
  output S00_AXI_wready;
  input [63:0]S00_AXI_wstrb;
  input S00_AXI_wvalid;
  input S01_ACLK;
  input S01_ARESETN;
  input [63:0]S01_AXI_araddr;
  input [1:0]S01_AXI_arburst;
  input [3:0]S01_AXI_arcache;
  input [5:0]S01_AXI_arid;
  input [7:0]S01_AXI_arlen;
  input [0:0]S01_AXI_arlock;
  input [2:0]S01_AXI_arprot;
  input [3:0]S01_AXI_arqos;
  output S01_AXI_arready;
  input [3:0]S01_AXI_arregion;
  input [2:0]S01_AXI_arsize;
  input S01_AXI_arvalid;
  input [63:0]S01_AXI_awaddr;
  input [1:0]S01_AXI_awburst;
  input [3:0]S01_AXI_awcache;
  input [5:0]S01_AXI_awid;
  input [7:0]S01_AXI_awlen;
  input [0:0]S01_AXI_awlock;
  input [2:0]S01_AXI_awprot;
  input [3:0]S01_AXI_awqos;
  output S01_AXI_awready;
  input [3:0]S01_AXI_awregion;
  input [2:0]S01_AXI_awsize;
  input S01_AXI_awvalid;
  output [5:0]S01_AXI_bid;
  input S01_AXI_bready;
  output [1:0]S01_AXI_bresp;
  output S01_AXI_bvalid;
  output [511:0]S01_AXI_rdata;
  output [5:0]S01_AXI_rid;
  output S01_AXI_rlast;
  input S01_AXI_rready;
  output [1:0]S01_AXI_rresp;
  output S01_AXI_rvalid;
  input [511:0]S01_AXI_wdata;
  input S01_AXI_wlast;
  output S01_AXI_wready;
  input [63:0]S01_AXI_wstrb;
  input S01_AXI_wvalid;

  wire axi_dram_xbar_ACLK_net;
  wire axi_dram_xbar_ARESETN_net;
  wire [63:0]axi_dram_xbar_to_s00_couplers_ARADDR;
  wire [1:0]axi_dram_xbar_to_s00_couplers_ARBURST;
  wire [3:0]axi_dram_xbar_to_s00_couplers_ARCACHE;
  wire [5:0]axi_dram_xbar_to_s00_couplers_ARID;
  wire [7:0]axi_dram_xbar_to_s00_couplers_ARLEN;
  wire [0:0]axi_dram_xbar_to_s00_couplers_ARLOCK;
  wire [2:0]axi_dram_xbar_to_s00_couplers_ARPROT;
  wire [3:0]axi_dram_xbar_to_s00_couplers_ARQOS;
  wire axi_dram_xbar_to_s00_couplers_ARREADY;
  wire [3:0]axi_dram_xbar_to_s00_couplers_ARREGION;
  wire [2:0]axi_dram_xbar_to_s00_couplers_ARSIZE;
  wire axi_dram_xbar_to_s00_couplers_ARVALID;
  wire [63:0]axi_dram_xbar_to_s00_couplers_AWADDR;
  wire [1:0]axi_dram_xbar_to_s00_couplers_AWBURST;
  wire [3:0]axi_dram_xbar_to_s00_couplers_AWCACHE;
  wire [5:0]axi_dram_xbar_to_s00_couplers_AWID;
  wire [7:0]axi_dram_xbar_to_s00_couplers_AWLEN;
  wire [0:0]axi_dram_xbar_to_s00_couplers_AWLOCK;
  wire [2:0]axi_dram_xbar_to_s00_couplers_AWPROT;
  wire [3:0]axi_dram_xbar_to_s00_couplers_AWQOS;
  wire axi_dram_xbar_to_s00_couplers_AWREADY;
  wire [3:0]axi_dram_xbar_to_s00_couplers_AWREGION;
  wire [2:0]axi_dram_xbar_to_s00_couplers_AWSIZE;
  wire axi_dram_xbar_to_s00_couplers_AWVALID;
  wire [5:0]axi_dram_xbar_to_s00_couplers_BID;
  wire axi_dram_xbar_to_s00_couplers_BREADY;
  wire [1:0]axi_dram_xbar_to_s00_couplers_BRESP;
  wire axi_dram_xbar_to_s00_couplers_BVALID;
  wire [511:0]axi_dram_xbar_to_s00_couplers_RDATA;
  wire [5:0]axi_dram_xbar_to_s00_couplers_RID;
  wire axi_dram_xbar_to_s00_couplers_RLAST;
  wire axi_dram_xbar_to_s00_couplers_RREADY;
  wire [1:0]axi_dram_xbar_to_s00_couplers_RRESP;
  wire axi_dram_xbar_to_s00_couplers_RVALID;
  wire [511:0]axi_dram_xbar_to_s00_couplers_WDATA;
  wire axi_dram_xbar_to_s00_couplers_WLAST;
  wire axi_dram_xbar_to_s00_couplers_WREADY;
  wire [63:0]axi_dram_xbar_to_s00_couplers_WSTRB;
  wire axi_dram_xbar_to_s00_couplers_WVALID;
  wire [63:0]axi_dram_xbar_to_s01_couplers_ARADDR;
  wire [1:0]axi_dram_xbar_to_s01_couplers_ARBURST;
  wire [3:0]axi_dram_xbar_to_s01_couplers_ARCACHE;
  wire [5:0]axi_dram_xbar_to_s01_couplers_ARID;
  wire [7:0]axi_dram_xbar_to_s01_couplers_ARLEN;
  wire [0:0]axi_dram_xbar_to_s01_couplers_ARLOCK;
  wire [2:0]axi_dram_xbar_to_s01_couplers_ARPROT;
  wire [3:0]axi_dram_xbar_to_s01_couplers_ARQOS;
  wire axi_dram_xbar_to_s01_couplers_ARREADY;
  wire [3:0]axi_dram_xbar_to_s01_couplers_ARREGION;
  wire [2:0]axi_dram_xbar_to_s01_couplers_ARSIZE;
  wire axi_dram_xbar_to_s01_couplers_ARVALID;
  wire [63:0]axi_dram_xbar_to_s01_couplers_AWADDR;
  wire [1:0]axi_dram_xbar_to_s01_couplers_AWBURST;
  wire [3:0]axi_dram_xbar_to_s01_couplers_AWCACHE;
  wire [5:0]axi_dram_xbar_to_s01_couplers_AWID;
  wire [7:0]axi_dram_xbar_to_s01_couplers_AWLEN;
  wire [0:0]axi_dram_xbar_to_s01_couplers_AWLOCK;
  wire [2:0]axi_dram_xbar_to_s01_couplers_AWPROT;
  wire [3:0]axi_dram_xbar_to_s01_couplers_AWQOS;
  wire axi_dram_xbar_to_s01_couplers_AWREADY;
  wire [3:0]axi_dram_xbar_to_s01_couplers_AWREGION;
  wire [2:0]axi_dram_xbar_to_s01_couplers_AWSIZE;
  wire axi_dram_xbar_to_s01_couplers_AWVALID;
  wire [5:0]axi_dram_xbar_to_s01_couplers_BID;
  wire axi_dram_xbar_to_s01_couplers_BREADY;
  wire [1:0]axi_dram_xbar_to_s01_couplers_BRESP;
  wire axi_dram_xbar_to_s01_couplers_BVALID;
  wire [511:0]axi_dram_xbar_to_s01_couplers_RDATA;
  wire [5:0]axi_dram_xbar_to_s01_couplers_RID;
  wire axi_dram_xbar_to_s01_couplers_RLAST;
  wire axi_dram_xbar_to_s01_couplers_RREADY;
  wire [1:0]axi_dram_xbar_to_s01_couplers_RRESP;
  wire axi_dram_xbar_to_s01_couplers_RVALID;
  wire [511:0]axi_dram_xbar_to_s01_couplers_WDATA;
  wire axi_dram_xbar_to_s01_couplers_WLAST;
  wire axi_dram_xbar_to_s01_couplers_WREADY;
  wire [63:0]axi_dram_xbar_to_s01_couplers_WSTRB;
  wire axi_dram_xbar_to_s01_couplers_WVALID;
  wire [63:0]m00_couplers_to_axi_dram_xbar_ARADDR;
  wire [1:0]m00_couplers_to_axi_dram_xbar_ARBURST;
  wire [3:0]m00_couplers_to_axi_dram_xbar_ARCACHE;
  wire [6:0]m00_couplers_to_axi_dram_xbar_ARID;
  wire [7:0]m00_couplers_to_axi_dram_xbar_ARLEN;
  wire [0:0]m00_couplers_to_axi_dram_xbar_ARLOCK;
  wire [2:0]m00_couplers_to_axi_dram_xbar_ARPROT;
  wire [3:0]m00_couplers_to_axi_dram_xbar_ARQOS;
  wire m00_couplers_to_axi_dram_xbar_ARREADY;
  wire [3:0]m00_couplers_to_axi_dram_xbar_ARREGION;
  wire [2:0]m00_couplers_to_axi_dram_xbar_ARSIZE;
  wire m00_couplers_to_axi_dram_xbar_ARVALID;
  wire [63:0]m00_couplers_to_axi_dram_xbar_AWADDR;
  wire [1:0]m00_couplers_to_axi_dram_xbar_AWBURST;
  wire [3:0]m00_couplers_to_axi_dram_xbar_AWCACHE;
  wire [6:0]m00_couplers_to_axi_dram_xbar_AWID;
  wire [7:0]m00_couplers_to_axi_dram_xbar_AWLEN;
  wire [0:0]m00_couplers_to_axi_dram_xbar_AWLOCK;
  wire [2:0]m00_couplers_to_axi_dram_xbar_AWPROT;
  wire [3:0]m00_couplers_to_axi_dram_xbar_AWQOS;
  wire m00_couplers_to_axi_dram_xbar_AWREADY;
  wire [3:0]m00_couplers_to_axi_dram_xbar_AWREGION;
  wire [2:0]m00_couplers_to_axi_dram_xbar_AWSIZE;
  wire m00_couplers_to_axi_dram_xbar_AWVALID;
  wire [6:0]m00_couplers_to_axi_dram_xbar_BID;
  wire m00_couplers_to_axi_dram_xbar_BREADY;
  wire [1:0]m00_couplers_to_axi_dram_xbar_BRESP;
  wire m00_couplers_to_axi_dram_xbar_BVALID;
  wire [511:0]m00_couplers_to_axi_dram_xbar_RDATA;
  wire [6:0]m00_couplers_to_axi_dram_xbar_RID;
  wire m00_couplers_to_axi_dram_xbar_RLAST;
  wire m00_couplers_to_axi_dram_xbar_RREADY;
  wire [1:0]m00_couplers_to_axi_dram_xbar_RRESP;
  wire m00_couplers_to_axi_dram_xbar_RVALID;
  wire [511:0]m00_couplers_to_axi_dram_xbar_WDATA;
  wire m00_couplers_to_axi_dram_xbar_WLAST;
  wire m00_couplers_to_axi_dram_xbar_WREADY;
  wire [63:0]m00_couplers_to_axi_dram_xbar_WSTRB;
  wire m00_couplers_to_axi_dram_xbar_WVALID;
  wire [63:0]m01_couplers_to_axi_dram_xbar_ARADDR;
  wire [1:0]m01_couplers_to_axi_dram_xbar_ARBURST;
  wire [3:0]m01_couplers_to_axi_dram_xbar_ARCACHE;
  wire [6:0]m01_couplers_to_axi_dram_xbar_ARID;
  wire [7:0]m01_couplers_to_axi_dram_xbar_ARLEN;
  wire [0:0]m01_couplers_to_axi_dram_xbar_ARLOCK;
  wire [2:0]m01_couplers_to_axi_dram_xbar_ARPROT;
  wire [3:0]m01_couplers_to_axi_dram_xbar_ARQOS;
  wire m01_couplers_to_axi_dram_xbar_ARREADY;
  wire [3:0]m01_couplers_to_axi_dram_xbar_ARREGION;
  wire [2:0]m01_couplers_to_axi_dram_xbar_ARSIZE;
  wire m01_couplers_to_axi_dram_xbar_ARVALID;
  wire [63:0]m01_couplers_to_axi_dram_xbar_AWADDR;
  wire [1:0]m01_couplers_to_axi_dram_xbar_AWBURST;
  wire [3:0]m01_couplers_to_axi_dram_xbar_AWCACHE;
  wire [6:0]m01_couplers_to_axi_dram_xbar_AWID;
  wire [7:0]m01_couplers_to_axi_dram_xbar_AWLEN;
  wire [0:0]m01_couplers_to_axi_dram_xbar_AWLOCK;
  wire [2:0]m01_couplers_to_axi_dram_xbar_AWPROT;
  wire [3:0]m01_couplers_to_axi_dram_xbar_AWQOS;
  wire m01_couplers_to_axi_dram_xbar_AWREADY;
  wire [3:0]m01_couplers_to_axi_dram_xbar_AWREGION;
  wire [2:0]m01_couplers_to_axi_dram_xbar_AWSIZE;
  wire m01_couplers_to_axi_dram_xbar_AWVALID;
  wire [6:0]m01_couplers_to_axi_dram_xbar_BID;
  wire m01_couplers_to_axi_dram_xbar_BREADY;
  wire [1:0]m01_couplers_to_axi_dram_xbar_BRESP;
  wire m01_couplers_to_axi_dram_xbar_BVALID;
  wire [511:0]m01_couplers_to_axi_dram_xbar_RDATA;
  wire [6:0]m01_couplers_to_axi_dram_xbar_RID;
  wire m01_couplers_to_axi_dram_xbar_RLAST;
  wire m01_couplers_to_axi_dram_xbar_RREADY;
  wire [1:0]m01_couplers_to_axi_dram_xbar_RRESP;
  wire m01_couplers_to_axi_dram_xbar_RVALID;
  wire [511:0]m01_couplers_to_axi_dram_xbar_WDATA;
  wire m01_couplers_to_axi_dram_xbar_WLAST;
  wire m01_couplers_to_axi_dram_xbar_WREADY;
  wire [63:0]m01_couplers_to_axi_dram_xbar_WSTRB;
  wire m01_couplers_to_axi_dram_xbar_WVALID;
  wire [63:0]m02_couplers_to_axi_dram_xbar_ARADDR;
  wire [1:0]m02_couplers_to_axi_dram_xbar_ARBURST;
  wire [3:0]m02_couplers_to_axi_dram_xbar_ARCACHE;
  wire [6:0]m02_couplers_to_axi_dram_xbar_ARID;
  wire [7:0]m02_couplers_to_axi_dram_xbar_ARLEN;
  wire [0:0]m02_couplers_to_axi_dram_xbar_ARLOCK;
  wire [2:0]m02_couplers_to_axi_dram_xbar_ARPROT;
  wire [3:0]m02_couplers_to_axi_dram_xbar_ARQOS;
  wire m02_couplers_to_axi_dram_xbar_ARREADY;
  wire [3:0]m02_couplers_to_axi_dram_xbar_ARREGION;
  wire [2:0]m02_couplers_to_axi_dram_xbar_ARSIZE;
  wire m02_couplers_to_axi_dram_xbar_ARVALID;
  wire [63:0]m02_couplers_to_axi_dram_xbar_AWADDR;
  wire [1:0]m02_couplers_to_axi_dram_xbar_AWBURST;
  wire [3:0]m02_couplers_to_axi_dram_xbar_AWCACHE;
  wire [6:0]m02_couplers_to_axi_dram_xbar_AWID;
  wire [7:0]m02_couplers_to_axi_dram_xbar_AWLEN;
  wire [0:0]m02_couplers_to_axi_dram_xbar_AWLOCK;
  wire [2:0]m02_couplers_to_axi_dram_xbar_AWPROT;
  wire [3:0]m02_couplers_to_axi_dram_xbar_AWQOS;
  wire m02_couplers_to_axi_dram_xbar_AWREADY;
  wire [3:0]m02_couplers_to_axi_dram_xbar_AWREGION;
  wire [2:0]m02_couplers_to_axi_dram_xbar_AWSIZE;
  wire m02_couplers_to_axi_dram_xbar_AWVALID;
  wire [6:0]m02_couplers_to_axi_dram_xbar_BID;
  wire m02_couplers_to_axi_dram_xbar_BREADY;
  wire [1:0]m02_couplers_to_axi_dram_xbar_BRESP;
  wire m02_couplers_to_axi_dram_xbar_BVALID;
  wire [511:0]m02_couplers_to_axi_dram_xbar_RDATA;
  wire [6:0]m02_couplers_to_axi_dram_xbar_RID;
  wire m02_couplers_to_axi_dram_xbar_RLAST;
  wire m02_couplers_to_axi_dram_xbar_RREADY;
  wire [1:0]m02_couplers_to_axi_dram_xbar_RRESP;
  wire m02_couplers_to_axi_dram_xbar_RVALID;
  wire [511:0]m02_couplers_to_axi_dram_xbar_WDATA;
  wire m02_couplers_to_axi_dram_xbar_WLAST;
  wire m02_couplers_to_axi_dram_xbar_WREADY;
  wire [63:0]m02_couplers_to_axi_dram_xbar_WSTRB;
  wire m02_couplers_to_axi_dram_xbar_WVALID;
  wire [63:0]m03_couplers_to_axi_dram_xbar_ARADDR;
  wire [1:0]m03_couplers_to_axi_dram_xbar_ARBURST;
  wire [3:0]m03_couplers_to_axi_dram_xbar_ARCACHE;
  wire [6:0]m03_couplers_to_axi_dram_xbar_ARID;
  wire [7:0]m03_couplers_to_axi_dram_xbar_ARLEN;
  wire [0:0]m03_couplers_to_axi_dram_xbar_ARLOCK;
  wire [2:0]m03_couplers_to_axi_dram_xbar_ARPROT;
  wire [3:0]m03_couplers_to_axi_dram_xbar_ARQOS;
  wire m03_couplers_to_axi_dram_xbar_ARREADY;
  wire [3:0]m03_couplers_to_axi_dram_xbar_ARREGION;
  wire [2:0]m03_couplers_to_axi_dram_xbar_ARSIZE;
  wire m03_couplers_to_axi_dram_xbar_ARVALID;
  wire [63:0]m03_couplers_to_axi_dram_xbar_AWADDR;
  wire [1:0]m03_couplers_to_axi_dram_xbar_AWBURST;
  wire [3:0]m03_couplers_to_axi_dram_xbar_AWCACHE;
  wire [6:0]m03_couplers_to_axi_dram_xbar_AWID;
  wire [7:0]m03_couplers_to_axi_dram_xbar_AWLEN;
  wire [0:0]m03_couplers_to_axi_dram_xbar_AWLOCK;
  wire [2:0]m03_couplers_to_axi_dram_xbar_AWPROT;
  wire [3:0]m03_couplers_to_axi_dram_xbar_AWQOS;
  wire m03_couplers_to_axi_dram_xbar_AWREADY;
  wire [3:0]m03_couplers_to_axi_dram_xbar_AWREGION;
  wire [2:0]m03_couplers_to_axi_dram_xbar_AWSIZE;
  wire m03_couplers_to_axi_dram_xbar_AWVALID;
  wire [6:0]m03_couplers_to_axi_dram_xbar_BID;
  wire m03_couplers_to_axi_dram_xbar_BREADY;
  wire [1:0]m03_couplers_to_axi_dram_xbar_BRESP;
  wire m03_couplers_to_axi_dram_xbar_BVALID;
  wire [511:0]m03_couplers_to_axi_dram_xbar_RDATA;
  wire [6:0]m03_couplers_to_axi_dram_xbar_RID;
  wire m03_couplers_to_axi_dram_xbar_RLAST;
  wire m03_couplers_to_axi_dram_xbar_RREADY;
  wire [1:0]m03_couplers_to_axi_dram_xbar_RRESP;
  wire m03_couplers_to_axi_dram_xbar_RVALID;
  wire [511:0]m03_couplers_to_axi_dram_xbar_WDATA;
  wire m03_couplers_to_axi_dram_xbar_WLAST;
  wire m03_couplers_to_axi_dram_xbar_WREADY;
  wire [63:0]m03_couplers_to_axi_dram_xbar_WSTRB;
  wire m03_couplers_to_axi_dram_xbar_WVALID;
  wire [63:0]s00_couplers_to_xbar_ARADDR;
  wire [1:0]s00_couplers_to_xbar_ARBURST;
  wire [3:0]s00_couplers_to_xbar_ARCACHE;
  wire [5:0]s00_couplers_to_xbar_ARID;
  wire [7:0]s00_couplers_to_xbar_ARLEN;
  wire [0:0]s00_couplers_to_xbar_ARLOCK;
  wire [2:0]s00_couplers_to_xbar_ARPROT;
  wire [3:0]s00_couplers_to_xbar_ARQOS;
  wire [0:0]s00_couplers_to_xbar_ARREADY;
  wire [2:0]s00_couplers_to_xbar_ARSIZE;
  wire s00_couplers_to_xbar_ARVALID;
  wire [63:0]s00_couplers_to_xbar_AWADDR;
  wire [1:0]s00_couplers_to_xbar_AWBURST;
  wire [3:0]s00_couplers_to_xbar_AWCACHE;
  wire [5:0]s00_couplers_to_xbar_AWID;
  wire [7:0]s00_couplers_to_xbar_AWLEN;
  wire [0:0]s00_couplers_to_xbar_AWLOCK;
  wire [2:0]s00_couplers_to_xbar_AWPROT;
  wire [3:0]s00_couplers_to_xbar_AWQOS;
  wire [0:0]s00_couplers_to_xbar_AWREADY;
  wire [2:0]s00_couplers_to_xbar_AWSIZE;
  wire s00_couplers_to_xbar_AWVALID;
  wire [6:0]s00_couplers_to_xbar_BID;
  wire s00_couplers_to_xbar_BREADY;
  wire [1:0]s00_couplers_to_xbar_BRESP;
  wire [0:0]s00_couplers_to_xbar_BVALID;
  wire [511:0]s00_couplers_to_xbar_RDATA;
  wire [6:0]s00_couplers_to_xbar_RID;
  wire [0:0]s00_couplers_to_xbar_RLAST;
  wire s00_couplers_to_xbar_RREADY;
  wire [1:0]s00_couplers_to_xbar_RRESP;
  wire [0:0]s00_couplers_to_xbar_RVALID;
  wire [511:0]s00_couplers_to_xbar_WDATA;
  wire s00_couplers_to_xbar_WLAST;
  wire [0:0]s00_couplers_to_xbar_WREADY;
  wire [63:0]s00_couplers_to_xbar_WSTRB;
  wire s00_couplers_to_xbar_WVALID;
  wire [63:0]s01_couplers_to_xbar_ARADDR;
  wire [1:0]s01_couplers_to_xbar_ARBURST;
  wire [3:0]s01_couplers_to_xbar_ARCACHE;
  wire [5:0]s01_couplers_to_xbar_ARID;
  wire [7:0]s01_couplers_to_xbar_ARLEN;
  wire [0:0]s01_couplers_to_xbar_ARLOCK;
  wire [2:0]s01_couplers_to_xbar_ARPROT;
  wire [3:0]s01_couplers_to_xbar_ARQOS;
  wire [1:1]s01_couplers_to_xbar_ARREADY;
  wire [2:0]s01_couplers_to_xbar_ARSIZE;
  wire s01_couplers_to_xbar_ARVALID;
  wire [63:0]s01_couplers_to_xbar_AWADDR;
  wire [1:0]s01_couplers_to_xbar_AWBURST;
  wire [3:0]s01_couplers_to_xbar_AWCACHE;
  wire [5:0]s01_couplers_to_xbar_AWID;
  wire [7:0]s01_couplers_to_xbar_AWLEN;
  wire [0:0]s01_couplers_to_xbar_AWLOCK;
  wire [2:0]s01_couplers_to_xbar_AWPROT;
  wire [3:0]s01_couplers_to_xbar_AWQOS;
  wire [1:1]s01_couplers_to_xbar_AWREADY;
  wire [2:0]s01_couplers_to_xbar_AWSIZE;
  wire s01_couplers_to_xbar_AWVALID;
  wire [13:7]s01_couplers_to_xbar_BID;
  wire s01_couplers_to_xbar_BREADY;
  wire [3:2]s01_couplers_to_xbar_BRESP;
  wire [1:1]s01_couplers_to_xbar_BVALID;
  wire [1023:512]s01_couplers_to_xbar_RDATA;
  wire [13:7]s01_couplers_to_xbar_RID;
  wire [1:1]s01_couplers_to_xbar_RLAST;
  wire s01_couplers_to_xbar_RREADY;
  wire [3:2]s01_couplers_to_xbar_RRESP;
  wire [1:1]s01_couplers_to_xbar_RVALID;
  wire [511:0]s01_couplers_to_xbar_WDATA;
  wire s01_couplers_to_xbar_WLAST;
  wire [1:1]s01_couplers_to_xbar_WREADY;
  wire [63:0]s01_couplers_to_xbar_WSTRB;
  wire s01_couplers_to_xbar_WVALID;
  wire [63:0]xbar_to_m00_couplers_ARADDR;
  wire [1:0]xbar_to_m00_couplers_ARBURST;
  wire [3:0]xbar_to_m00_couplers_ARCACHE;
  wire [6:0]xbar_to_m00_couplers_ARID;
  wire [7:0]xbar_to_m00_couplers_ARLEN;
  wire [0:0]xbar_to_m00_couplers_ARLOCK;
  wire [2:0]xbar_to_m00_couplers_ARPROT;
  wire [3:0]xbar_to_m00_couplers_ARQOS;
  wire xbar_to_m00_couplers_ARREADY;
  wire [3:0]xbar_to_m00_couplers_ARREGION;
  wire [2:0]xbar_to_m00_couplers_ARSIZE;
  wire [0:0]xbar_to_m00_couplers_ARVALID;
  wire [63:0]xbar_to_m00_couplers_AWADDR;
  wire [1:0]xbar_to_m00_couplers_AWBURST;
  wire [3:0]xbar_to_m00_couplers_AWCACHE;
  wire [6:0]xbar_to_m00_couplers_AWID;
  wire [7:0]xbar_to_m00_couplers_AWLEN;
  wire [0:0]xbar_to_m00_couplers_AWLOCK;
  wire [2:0]xbar_to_m00_couplers_AWPROT;
  wire [3:0]xbar_to_m00_couplers_AWQOS;
  wire xbar_to_m00_couplers_AWREADY;
  wire [3:0]xbar_to_m00_couplers_AWREGION;
  wire [2:0]xbar_to_m00_couplers_AWSIZE;
  wire [0:0]xbar_to_m00_couplers_AWVALID;
  wire [6:0]xbar_to_m00_couplers_BID;
  wire [0:0]xbar_to_m00_couplers_BREADY;
  wire [1:0]xbar_to_m00_couplers_BRESP;
  wire xbar_to_m00_couplers_BVALID;
  wire [511:0]xbar_to_m00_couplers_RDATA;
  wire [6:0]xbar_to_m00_couplers_RID;
  wire xbar_to_m00_couplers_RLAST;
  wire [0:0]xbar_to_m00_couplers_RREADY;
  wire [1:0]xbar_to_m00_couplers_RRESP;
  wire xbar_to_m00_couplers_RVALID;
  wire [511:0]xbar_to_m00_couplers_WDATA;
  wire [0:0]xbar_to_m00_couplers_WLAST;
  wire xbar_to_m00_couplers_WREADY;
  wire [63:0]xbar_to_m00_couplers_WSTRB;
  wire [0:0]xbar_to_m00_couplers_WVALID;
  wire [127:64]xbar_to_m01_couplers_ARADDR;
  wire [3:2]xbar_to_m01_couplers_ARBURST;
  wire [7:4]xbar_to_m01_couplers_ARCACHE;
  wire [13:7]xbar_to_m01_couplers_ARID;
  wire [15:8]xbar_to_m01_couplers_ARLEN;
  wire [1:1]xbar_to_m01_couplers_ARLOCK;
  wire [5:3]xbar_to_m01_couplers_ARPROT;
  wire [7:4]xbar_to_m01_couplers_ARQOS;
  wire xbar_to_m01_couplers_ARREADY;
  wire [7:4]xbar_to_m01_couplers_ARREGION;
  wire [5:3]xbar_to_m01_couplers_ARSIZE;
  wire [1:1]xbar_to_m01_couplers_ARVALID;
  wire [127:64]xbar_to_m01_couplers_AWADDR;
  wire [3:2]xbar_to_m01_couplers_AWBURST;
  wire [7:4]xbar_to_m01_couplers_AWCACHE;
  wire [13:7]xbar_to_m01_couplers_AWID;
  wire [15:8]xbar_to_m01_couplers_AWLEN;
  wire [1:1]xbar_to_m01_couplers_AWLOCK;
  wire [5:3]xbar_to_m01_couplers_AWPROT;
  wire [7:4]xbar_to_m01_couplers_AWQOS;
  wire xbar_to_m01_couplers_AWREADY;
  wire [7:4]xbar_to_m01_couplers_AWREGION;
  wire [5:3]xbar_to_m01_couplers_AWSIZE;
  wire [1:1]xbar_to_m01_couplers_AWVALID;
  wire [6:0]xbar_to_m01_couplers_BID;
  wire [1:1]xbar_to_m01_couplers_BREADY;
  wire [1:0]xbar_to_m01_couplers_BRESP;
  wire xbar_to_m01_couplers_BVALID;
  wire [511:0]xbar_to_m01_couplers_RDATA;
  wire [6:0]xbar_to_m01_couplers_RID;
  wire xbar_to_m01_couplers_RLAST;
  wire [1:1]xbar_to_m01_couplers_RREADY;
  wire [1:0]xbar_to_m01_couplers_RRESP;
  wire xbar_to_m01_couplers_RVALID;
  wire [1023:512]xbar_to_m01_couplers_WDATA;
  wire [1:1]xbar_to_m01_couplers_WLAST;
  wire xbar_to_m01_couplers_WREADY;
  wire [127:64]xbar_to_m01_couplers_WSTRB;
  wire [1:1]xbar_to_m01_couplers_WVALID;
  wire [191:128]xbar_to_m02_couplers_ARADDR;
  wire [5:4]xbar_to_m02_couplers_ARBURST;
  wire [11:8]xbar_to_m02_couplers_ARCACHE;
  wire [20:14]xbar_to_m02_couplers_ARID;
  wire [23:16]xbar_to_m02_couplers_ARLEN;
  wire [2:2]xbar_to_m02_couplers_ARLOCK;
  wire [8:6]xbar_to_m02_couplers_ARPROT;
  wire [11:8]xbar_to_m02_couplers_ARQOS;
  wire xbar_to_m02_couplers_ARREADY;
  wire [11:8]xbar_to_m02_couplers_ARREGION;
  wire [8:6]xbar_to_m02_couplers_ARSIZE;
  wire [2:2]xbar_to_m02_couplers_ARVALID;
  wire [191:128]xbar_to_m02_couplers_AWADDR;
  wire [5:4]xbar_to_m02_couplers_AWBURST;
  wire [11:8]xbar_to_m02_couplers_AWCACHE;
  wire [20:14]xbar_to_m02_couplers_AWID;
  wire [23:16]xbar_to_m02_couplers_AWLEN;
  wire [2:2]xbar_to_m02_couplers_AWLOCK;
  wire [8:6]xbar_to_m02_couplers_AWPROT;
  wire [11:8]xbar_to_m02_couplers_AWQOS;
  wire xbar_to_m02_couplers_AWREADY;
  wire [11:8]xbar_to_m02_couplers_AWREGION;
  wire [8:6]xbar_to_m02_couplers_AWSIZE;
  wire [2:2]xbar_to_m02_couplers_AWVALID;
  wire [6:0]xbar_to_m02_couplers_BID;
  wire [2:2]xbar_to_m02_couplers_BREADY;
  wire [1:0]xbar_to_m02_couplers_BRESP;
  wire xbar_to_m02_couplers_BVALID;
  wire [511:0]xbar_to_m02_couplers_RDATA;
  wire [6:0]xbar_to_m02_couplers_RID;
  wire xbar_to_m02_couplers_RLAST;
  wire [2:2]xbar_to_m02_couplers_RREADY;
  wire [1:0]xbar_to_m02_couplers_RRESP;
  wire xbar_to_m02_couplers_RVALID;
  wire [1535:1024]xbar_to_m02_couplers_WDATA;
  wire [2:2]xbar_to_m02_couplers_WLAST;
  wire xbar_to_m02_couplers_WREADY;
  wire [191:128]xbar_to_m02_couplers_WSTRB;
  wire [2:2]xbar_to_m02_couplers_WVALID;
  wire [255:192]xbar_to_m03_couplers_ARADDR;
  wire [7:6]xbar_to_m03_couplers_ARBURST;
  wire [15:12]xbar_to_m03_couplers_ARCACHE;
  wire [27:21]xbar_to_m03_couplers_ARID;
  wire [31:24]xbar_to_m03_couplers_ARLEN;
  wire [3:3]xbar_to_m03_couplers_ARLOCK;
  wire [11:9]xbar_to_m03_couplers_ARPROT;
  wire [15:12]xbar_to_m03_couplers_ARQOS;
  wire xbar_to_m03_couplers_ARREADY;
  wire [15:12]xbar_to_m03_couplers_ARREGION;
  wire [11:9]xbar_to_m03_couplers_ARSIZE;
  wire [3:3]xbar_to_m03_couplers_ARVALID;
  wire [255:192]xbar_to_m03_couplers_AWADDR;
  wire [7:6]xbar_to_m03_couplers_AWBURST;
  wire [15:12]xbar_to_m03_couplers_AWCACHE;
  wire [27:21]xbar_to_m03_couplers_AWID;
  wire [31:24]xbar_to_m03_couplers_AWLEN;
  wire [3:3]xbar_to_m03_couplers_AWLOCK;
  wire [11:9]xbar_to_m03_couplers_AWPROT;
  wire [15:12]xbar_to_m03_couplers_AWQOS;
  wire xbar_to_m03_couplers_AWREADY;
  wire [15:12]xbar_to_m03_couplers_AWREGION;
  wire [11:9]xbar_to_m03_couplers_AWSIZE;
  wire [3:3]xbar_to_m03_couplers_AWVALID;
  wire [6:0]xbar_to_m03_couplers_BID;
  wire [3:3]xbar_to_m03_couplers_BREADY;
  wire [1:0]xbar_to_m03_couplers_BRESP;
  wire xbar_to_m03_couplers_BVALID;
  wire [511:0]xbar_to_m03_couplers_RDATA;
  wire [6:0]xbar_to_m03_couplers_RID;
  wire xbar_to_m03_couplers_RLAST;
  wire [3:3]xbar_to_m03_couplers_RREADY;
  wire [1:0]xbar_to_m03_couplers_RRESP;
  wire xbar_to_m03_couplers_RVALID;
  wire [2047:1536]xbar_to_m03_couplers_WDATA;
  wire [3:3]xbar_to_m03_couplers_WLAST;
  wire xbar_to_m03_couplers_WREADY;
  wire [255:192]xbar_to_m03_couplers_WSTRB;
  wire [3:3]xbar_to_m03_couplers_WVALID;

  assign M00_AXI_araddr[63:0] = m00_couplers_to_axi_dram_xbar_ARADDR;
  assign M00_AXI_arburst[1:0] = m00_couplers_to_axi_dram_xbar_ARBURST;
  assign M00_AXI_arcache[3:0] = m00_couplers_to_axi_dram_xbar_ARCACHE;
  assign M00_AXI_arid[6:0] = m00_couplers_to_axi_dram_xbar_ARID;
  assign M00_AXI_arlen[7:0] = m00_couplers_to_axi_dram_xbar_ARLEN;
  assign M00_AXI_arlock[0] = m00_couplers_to_axi_dram_xbar_ARLOCK;
  assign M00_AXI_arprot[2:0] = m00_couplers_to_axi_dram_xbar_ARPROT;
  assign M00_AXI_arqos[3:0] = m00_couplers_to_axi_dram_xbar_ARQOS;
  assign M00_AXI_arregion[3:0] = m00_couplers_to_axi_dram_xbar_ARREGION;
  assign M00_AXI_arsize[2:0] = m00_couplers_to_axi_dram_xbar_ARSIZE;
  assign M00_AXI_arvalid = m00_couplers_to_axi_dram_xbar_ARVALID;
  assign M00_AXI_awaddr[63:0] = m00_couplers_to_axi_dram_xbar_AWADDR;
  assign M00_AXI_awburst[1:0] = m00_couplers_to_axi_dram_xbar_AWBURST;
  assign M00_AXI_awcache[3:0] = m00_couplers_to_axi_dram_xbar_AWCACHE;
  assign M00_AXI_awid[6:0] = m00_couplers_to_axi_dram_xbar_AWID;
  assign M00_AXI_awlen[7:0] = m00_couplers_to_axi_dram_xbar_AWLEN;
  assign M00_AXI_awlock[0] = m00_couplers_to_axi_dram_xbar_AWLOCK;
  assign M00_AXI_awprot[2:0] = m00_couplers_to_axi_dram_xbar_AWPROT;
  assign M00_AXI_awqos[3:0] = m00_couplers_to_axi_dram_xbar_AWQOS;
  assign M00_AXI_awregion[3:0] = m00_couplers_to_axi_dram_xbar_AWREGION;
  assign M00_AXI_awsize[2:0] = m00_couplers_to_axi_dram_xbar_AWSIZE;
  assign M00_AXI_awvalid = m00_couplers_to_axi_dram_xbar_AWVALID;
  assign M00_AXI_bready = m00_couplers_to_axi_dram_xbar_BREADY;
  assign M00_AXI_rready = m00_couplers_to_axi_dram_xbar_RREADY;
  assign M00_AXI_wdata[511:0] = m00_couplers_to_axi_dram_xbar_WDATA;
  assign M00_AXI_wlast = m00_couplers_to_axi_dram_xbar_WLAST;
  assign M00_AXI_wstrb[63:0] = m00_couplers_to_axi_dram_xbar_WSTRB;
  assign M00_AXI_wvalid = m00_couplers_to_axi_dram_xbar_WVALID;
  assign M01_AXI_araddr[63:0] = m01_couplers_to_axi_dram_xbar_ARADDR;
  assign M01_AXI_arburst[1:0] = m01_couplers_to_axi_dram_xbar_ARBURST;
  assign M01_AXI_arcache[3:0] = m01_couplers_to_axi_dram_xbar_ARCACHE;
  assign M01_AXI_arid[6:0] = m01_couplers_to_axi_dram_xbar_ARID;
  assign M01_AXI_arlen[7:0] = m01_couplers_to_axi_dram_xbar_ARLEN;
  assign M01_AXI_arlock[0] = m01_couplers_to_axi_dram_xbar_ARLOCK;
  assign M01_AXI_arprot[2:0] = m01_couplers_to_axi_dram_xbar_ARPROT;
  assign M01_AXI_arqos[3:0] = m01_couplers_to_axi_dram_xbar_ARQOS;
  assign M01_AXI_arregion[3:0] = m01_couplers_to_axi_dram_xbar_ARREGION;
  assign M01_AXI_arsize[2:0] = m01_couplers_to_axi_dram_xbar_ARSIZE;
  assign M01_AXI_arvalid = m01_couplers_to_axi_dram_xbar_ARVALID;
  assign M01_AXI_awaddr[63:0] = m01_couplers_to_axi_dram_xbar_AWADDR;
  assign M01_AXI_awburst[1:0] = m01_couplers_to_axi_dram_xbar_AWBURST;
  assign M01_AXI_awcache[3:0] = m01_couplers_to_axi_dram_xbar_AWCACHE;
  assign M01_AXI_awid[6:0] = m01_couplers_to_axi_dram_xbar_AWID;
  assign M01_AXI_awlen[7:0] = m01_couplers_to_axi_dram_xbar_AWLEN;
  assign M01_AXI_awlock[0] = m01_couplers_to_axi_dram_xbar_AWLOCK;
  assign M01_AXI_awprot[2:0] = m01_couplers_to_axi_dram_xbar_AWPROT;
  assign M01_AXI_awqos[3:0] = m01_couplers_to_axi_dram_xbar_AWQOS;
  assign M01_AXI_awregion[3:0] = m01_couplers_to_axi_dram_xbar_AWREGION;
  assign M01_AXI_awsize[2:0] = m01_couplers_to_axi_dram_xbar_AWSIZE;
  assign M01_AXI_awvalid = m01_couplers_to_axi_dram_xbar_AWVALID;
  assign M01_AXI_bready = m01_couplers_to_axi_dram_xbar_BREADY;
  assign M01_AXI_rready = m01_couplers_to_axi_dram_xbar_RREADY;
  assign M01_AXI_wdata[511:0] = m01_couplers_to_axi_dram_xbar_WDATA;
  assign M01_AXI_wlast = m01_couplers_to_axi_dram_xbar_WLAST;
  assign M01_AXI_wstrb[63:0] = m01_couplers_to_axi_dram_xbar_WSTRB;
  assign M01_AXI_wvalid = m01_couplers_to_axi_dram_xbar_WVALID;
  assign M02_AXI_araddr[63:0] = m02_couplers_to_axi_dram_xbar_ARADDR;
  assign M02_AXI_arburst[1:0] = m02_couplers_to_axi_dram_xbar_ARBURST;
  assign M02_AXI_arcache[3:0] = m02_couplers_to_axi_dram_xbar_ARCACHE;
  assign M02_AXI_arid[6:0] = m02_couplers_to_axi_dram_xbar_ARID;
  assign M02_AXI_arlen[7:0] = m02_couplers_to_axi_dram_xbar_ARLEN;
  assign M02_AXI_arlock[0] = m02_couplers_to_axi_dram_xbar_ARLOCK;
  assign M02_AXI_arprot[2:0] = m02_couplers_to_axi_dram_xbar_ARPROT;
  assign M02_AXI_arqos[3:0] = m02_couplers_to_axi_dram_xbar_ARQOS;
  assign M02_AXI_arregion[3:0] = m02_couplers_to_axi_dram_xbar_ARREGION;
  assign M02_AXI_arsize[2:0] = m02_couplers_to_axi_dram_xbar_ARSIZE;
  assign M02_AXI_arvalid = m02_couplers_to_axi_dram_xbar_ARVALID;
  assign M02_AXI_awaddr[63:0] = m02_couplers_to_axi_dram_xbar_AWADDR;
  assign M02_AXI_awburst[1:0] = m02_couplers_to_axi_dram_xbar_AWBURST;
  assign M02_AXI_awcache[3:0] = m02_couplers_to_axi_dram_xbar_AWCACHE;
  assign M02_AXI_awid[6:0] = m02_couplers_to_axi_dram_xbar_AWID;
  assign M02_AXI_awlen[7:0] = m02_couplers_to_axi_dram_xbar_AWLEN;
  assign M02_AXI_awlock[0] = m02_couplers_to_axi_dram_xbar_AWLOCK;
  assign M02_AXI_awprot[2:0] = m02_couplers_to_axi_dram_xbar_AWPROT;
  assign M02_AXI_awqos[3:0] = m02_couplers_to_axi_dram_xbar_AWQOS;
  assign M02_AXI_awregion[3:0] = m02_couplers_to_axi_dram_xbar_AWREGION;
  assign M02_AXI_awsize[2:0] = m02_couplers_to_axi_dram_xbar_AWSIZE;
  assign M02_AXI_awvalid = m02_couplers_to_axi_dram_xbar_AWVALID;
  assign M02_AXI_bready = m02_couplers_to_axi_dram_xbar_BREADY;
  assign M02_AXI_rready = m02_couplers_to_axi_dram_xbar_RREADY;
  assign M02_AXI_wdata[511:0] = m02_couplers_to_axi_dram_xbar_WDATA;
  assign M02_AXI_wlast = m02_couplers_to_axi_dram_xbar_WLAST;
  assign M02_AXI_wstrb[63:0] = m02_couplers_to_axi_dram_xbar_WSTRB;
  assign M02_AXI_wvalid = m02_couplers_to_axi_dram_xbar_WVALID;
  assign M03_AXI_araddr[63:0] = m03_couplers_to_axi_dram_xbar_ARADDR;
  assign M03_AXI_arburst[1:0] = m03_couplers_to_axi_dram_xbar_ARBURST;
  assign M03_AXI_arcache[3:0] = m03_couplers_to_axi_dram_xbar_ARCACHE;
  assign M03_AXI_arid[6:0] = m03_couplers_to_axi_dram_xbar_ARID;
  assign M03_AXI_arlen[7:0] = m03_couplers_to_axi_dram_xbar_ARLEN;
  assign M03_AXI_arlock[0] = m03_couplers_to_axi_dram_xbar_ARLOCK;
  assign M03_AXI_arprot[2:0] = m03_couplers_to_axi_dram_xbar_ARPROT;
  assign M03_AXI_arqos[3:0] = m03_couplers_to_axi_dram_xbar_ARQOS;
  assign M03_AXI_arregion[3:0] = m03_couplers_to_axi_dram_xbar_ARREGION;
  assign M03_AXI_arsize[2:0] = m03_couplers_to_axi_dram_xbar_ARSIZE;
  assign M03_AXI_arvalid = m03_couplers_to_axi_dram_xbar_ARVALID;
  assign M03_AXI_awaddr[63:0] = m03_couplers_to_axi_dram_xbar_AWADDR;
  assign M03_AXI_awburst[1:0] = m03_couplers_to_axi_dram_xbar_AWBURST;
  assign M03_AXI_awcache[3:0] = m03_couplers_to_axi_dram_xbar_AWCACHE;
  assign M03_AXI_awid[6:0] = m03_couplers_to_axi_dram_xbar_AWID;
  assign M03_AXI_awlen[7:0] = m03_couplers_to_axi_dram_xbar_AWLEN;
  assign M03_AXI_awlock[0] = m03_couplers_to_axi_dram_xbar_AWLOCK;
  assign M03_AXI_awprot[2:0] = m03_couplers_to_axi_dram_xbar_AWPROT;
  assign M03_AXI_awqos[3:0] = m03_couplers_to_axi_dram_xbar_AWQOS;
  assign M03_AXI_awregion[3:0] = m03_couplers_to_axi_dram_xbar_AWREGION;
  assign M03_AXI_awsize[2:0] = m03_couplers_to_axi_dram_xbar_AWSIZE;
  assign M03_AXI_awvalid = m03_couplers_to_axi_dram_xbar_AWVALID;
  assign M03_AXI_bready = m03_couplers_to_axi_dram_xbar_BREADY;
  assign M03_AXI_rready = m03_couplers_to_axi_dram_xbar_RREADY;
  assign M03_AXI_wdata[511:0] = m03_couplers_to_axi_dram_xbar_WDATA;
  assign M03_AXI_wlast = m03_couplers_to_axi_dram_xbar_WLAST;
  assign M03_AXI_wstrb[63:0] = m03_couplers_to_axi_dram_xbar_WSTRB;
  assign M03_AXI_wvalid = m03_couplers_to_axi_dram_xbar_WVALID;
  assign S00_AXI_arready = axi_dram_xbar_to_s00_couplers_ARREADY;
  assign S00_AXI_awready = axi_dram_xbar_to_s00_couplers_AWREADY;
  assign S00_AXI_bid[5:0] = axi_dram_xbar_to_s00_couplers_BID;
  assign S00_AXI_bresp[1:0] = axi_dram_xbar_to_s00_couplers_BRESP;
  assign S00_AXI_bvalid = axi_dram_xbar_to_s00_couplers_BVALID;
  assign S00_AXI_rdata[511:0] = axi_dram_xbar_to_s00_couplers_RDATA;
  assign S00_AXI_rid[5:0] = axi_dram_xbar_to_s00_couplers_RID;
  assign S00_AXI_rlast = axi_dram_xbar_to_s00_couplers_RLAST;
  assign S00_AXI_rresp[1:0] = axi_dram_xbar_to_s00_couplers_RRESP;
  assign S00_AXI_rvalid = axi_dram_xbar_to_s00_couplers_RVALID;
  assign S00_AXI_wready = axi_dram_xbar_to_s00_couplers_WREADY;
  assign S01_AXI_arready = axi_dram_xbar_to_s01_couplers_ARREADY;
  assign S01_AXI_awready = axi_dram_xbar_to_s01_couplers_AWREADY;
  assign S01_AXI_bid[5:0] = axi_dram_xbar_to_s01_couplers_BID;
  assign S01_AXI_bresp[1:0] = axi_dram_xbar_to_s01_couplers_BRESP;
  assign S01_AXI_bvalid = axi_dram_xbar_to_s01_couplers_BVALID;
  assign S01_AXI_rdata[511:0] = axi_dram_xbar_to_s01_couplers_RDATA;
  assign S01_AXI_rid[5:0] = axi_dram_xbar_to_s01_couplers_RID;
  assign S01_AXI_rlast = axi_dram_xbar_to_s01_couplers_RLAST;
  assign S01_AXI_rresp[1:0] = axi_dram_xbar_to_s01_couplers_RRESP;
  assign S01_AXI_rvalid = axi_dram_xbar_to_s01_couplers_RVALID;
  assign S01_AXI_wready = axi_dram_xbar_to_s01_couplers_WREADY;
  assign axi_dram_xbar_ACLK_net = ACLK;
  assign axi_dram_xbar_ARESETN_net = ARESETN;
  assign axi_dram_xbar_to_s00_couplers_ARADDR = S00_AXI_araddr[63:0];
  assign axi_dram_xbar_to_s00_couplers_ARBURST = S00_AXI_arburst[1:0];
  assign axi_dram_xbar_to_s00_couplers_ARCACHE = S00_AXI_arcache[3:0];
  assign axi_dram_xbar_to_s00_couplers_ARID = S00_AXI_arid[5:0];
  assign axi_dram_xbar_to_s00_couplers_ARLEN = S00_AXI_arlen[7:0];
  assign axi_dram_xbar_to_s00_couplers_ARLOCK = S00_AXI_arlock[0];
  assign axi_dram_xbar_to_s00_couplers_ARPROT = S00_AXI_arprot[2:0];
  assign axi_dram_xbar_to_s00_couplers_ARQOS = S00_AXI_arqos[3:0];
  assign axi_dram_xbar_to_s00_couplers_ARREGION = S00_AXI_arregion[3:0];
  assign axi_dram_xbar_to_s00_couplers_ARSIZE = S00_AXI_arsize[2:0];
  assign axi_dram_xbar_to_s00_couplers_ARVALID = S00_AXI_arvalid;
  assign axi_dram_xbar_to_s00_couplers_AWADDR = S00_AXI_awaddr[63:0];
  assign axi_dram_xbar_to_s00_couplers_AWBURST = S00_AXI_awburst[1:0];
  assign axi_dram_xbar_to_s00_couplers_AWCACHE = S00_AXI_awcache[3:0];
  assign axi_dram_xbar_to_s00_couplers_AWID = S00_AXI_awid[5:0];
  assign axi_dram_xbar_to_s00_couplers_AWLEN = S00_AXI_awlen[7:0];
  assign axi_dram_xbar_to_s00_couplers_AWLOCK = S00_AXI_awlock[0];
  assign axi_dram_xbar_to_s00_couplers_AWPROT = S00_AXI_awprot[2:0];
  assign axi_dram_xbar_to_s00_couplers_AWQOS = S00_AXI_awqos[3:0];
  assign axi_dram_xbar_to_s00_couplers_AWREGION = S00_AXI_awregion[3:0];
  assign axi_dram_xbar_to_s00_couplers_AWSIZE = S00_AXI_awsize[2:0];
  assign axi_dram_xbar_to_s00_couplers_AWVALID = S00_AXI_awvalid;
  assign axi_dram_xbar_to_s00_couplers_BREADY = S00_AXI_bready;
  assign axi_dram_xbar_to_s00_couplers_RREADY = S00_AXI_rready;
  assign axi_dram_xbar_to_s00_couplers_WDATA = S00_AXI_wdata[511:0];
  assign axi_dram_xbar_to_s00_couplers_WLAST = S00_AXI_wlast;
  assign axi_dram_xbar_to_s00_couplers_WSTRB = S00_AXI_wstrb[63:0];
  assign axi_dram_xbar_to_s00_couplers_WVALID = S00_AXI_wvalid;
  assign axi_dram_xbar_to_s01_couplers_ARADDR = S01_AXI_araddr[63:0];
  assign axi_dram_xbar_to_s01_couplers_ARBURST = S01_AXI_arburst[1:0];
  assign axi_dram_xbar_to_s01_couplers_ARCACHE = S01_AXI_arcache[3:0];
  assign axi_dram_xbar_to_s01_couplers_ARID = S01_AXI_arid[5:0];
  assign axi_dram_xbar_to_s01_couplers_ARLEN = S01_AXI_arlen[7:0];
  assign axi_dram_xbar_to_s01_couplers_ARLOCK = S01_AXI_arlock[0];
  assign axi_dram_xbar_to_s01_couplers_ARPROT = S01_AXI_arprot[2:0];
  assign axi_dram_xbar_to_s01_couplers_ARQOS = S01_AXI_arqos[3:0];
  assign axi_dram_xbar_to_s01_couplers_ARREGION = S01_AXI_arregion[3:0];
  assign axi_dram_xbar_to_s01_couplers_ARSIZE = S01_AXI_arsize[2:0];
  assign axi_dram_xbar_to_s01_couplers_ARVALID = S01_AXI_arvalid;
  assign axi_dram_xbar_to_s01_couplers_AWADDR = S01_AXI_awaddr[63:0];
  assign axi_dram_xbar_to_s01_couplers_AWBURST = S01_AXI_awburst[1:0];
  assign axi_dram_xbar_to_s01_couplers_AWCACHE = S01_AXI_awcache[3:0];
  assign axi_dram_xbar_to_s01_couplers_AWID = S01_AXI_awid[5:0];
  assign axi_dram_xbar_to_s01_couplers_AWLEN = S01_AXI_awlen[7:0];
  assign axi_dram_xbar_to_s01_couplers_AWLOCK = S01_AXI_awlock[0];
  assign axi_dram_xbar_to_s01_couplers_AWPROT = S01_AXI_awprot[2:0];
  assign axi_dram_xbar_to_s01_couplers_AWQOS = S01_AXI_awqos[3:0];
  assign axi_dram_xbar_to_s01_couplers_AWREGION = S01_AXI_awregion[3:0];
  assign axi_dram_xbar_to_s01_couplers_AWSIZE = S01_AXI_awsize[2:0];
  assign axi_dram_xbar_to_s01_couplers_AWVALID = S01_AXI_awvalid;
  assign axi_dram_xbar_to_s01_couplers_BREADY = S01_AXI_bready;
  assign axi_dram_xbar_to_s01_couplers_RREADY = S01_AXI_rready;
  assign axi_dram_xbar_to_s01_couplers_WDATA = S01_AXI_wdata[511:0];
  assign axi_dram_xbar_to_s01_couplers_WLAST = S01_AXI_wlast;
  assign axi_dram_xbar_to_s01_couplers_WSTRB = S01_AXI_wstrb[63:0];
  assign axi_dram_xbar_to_s01_couplers_WVALID = S01_AXI_wvalid;
  assign m00_couplers_to_axi_dram_xbar_ARREADY = M00_AXI_arready;
  assign m00_couplers_to_axi_dram_xbar_AWREADY = M00_AXI_awready;
  assign m00_couplers_to_axi_dram_xbar_BID = M00_AXI_bid[6:0];
  assign m00_couplers_to_axi_dram_xbar_BRESP = M00_AXI_bresp[1:0];
  assign m00_couplers_to_axi_dram_xbar_BVALID = M00_AXI_bvalid;
  assign m00_couplers_to_axi_dram_xbar_RDATA = M00_AXI_rdata[511:0];
  assign m00_couplers_to_axi_dram_xbar_RID = M00_AXI_rid[6:0];
  assign m00_couplers_to_axi_dram_xbar_RLAST = M00_AXI_rlast;
  assign m00_couplers_to_axi_dram_xbar_RRESP = M00_AXI_rresp[1:0];
  assign m00_couplers_to_axi_dram_xbar_RVALID = M00_AXI_rvalid;
  assign m00_couplers_to_axi_dram_xbar_WREADY = M00_AXI_wready;
  assign m01_couplers_to_axi_dram_xbar_ARREADY = M01_AXI_arready;
  assign m01_couplers_to_axi_dram_xbar_AWREADY = M01_AXI_awready;
  assign m01_couplers_to_axi_dram_xbar_BID = M01_AXI_bid[6:0];
  assign m01_couplers_to_axi_dram_xbar_BRESP = M01_AXI_bresp[1:0];
  assign m01_couplers_to_axi_dram_xbar_BVALID = M01_AXI_bvalid;
  assign m01_couplers_to_axi_dram_xbar_RDATA = M01_AXI_rdata[511:0];
  assign m01_couplers_to_axi_dram_xbar_RID = M01_AXI_rid[6:0];
  assign m01_couplers_to_axi_dram_xbar_RLAST = M01_AXI_rlast;
  assign m01_couplers_to_axi_dram_xbar_RRESP = M01_AXI_rresp[1:0];
  assign m01_couplers_to_axi_dram_xbar_RVALID = M01_AXI_rvalid;
  assign m01_couplers_to_axi_dram_xbar_WREADY = M01_AXI_wready;
  assign m02_couplers_to_axi_dram_xbar_ARREADY = M02_AXI_arready;
  assign m02_couplers_to_axi_dram_xbar_AWREADY = M02_AXI_awready;
  assign m02_couplers_to_axi_dram_xbar_BID = M02_AXI_bid[6:0];
  assign m02_couplers_to_axi_dram_xbar_BRESP = M02_AXI_bresp[1:0];
  assign m02_couplers_to_axi_dram_xbar_BVALID = M02_AXI_bvalid;
  assign m02_couplers_to_axi_dram_xbar_RDATA = M02_AXI_rdata[511:0];
  assign m02_couplers_to_axi_dram_xbar_RID = M02_AXI_rid[6:0];
  assign m02_couplers_to_axi_dram_xbar_RLAST = M02_AXI_rlast;
  assign m02_couplers_to_axi_dram_xbar_RRESP = M02_AXI_rresp[1:0];
  assign m02_couplers_to_axi_dram_xbar_RVALID = M02_AXI_rvalid;
  assign m02_couplers_to_axi_dram_xbar_WREADY = M02_AXI_wready;
  assign m03_couplers_to_axi_dram_xbar_ARREADY = M03_AXI_arready;
  assign m03_couplers_to_axi_dram_xbar_AWREADY = M03_AXI_awready;
  assign m03_couplers_to_axi_dram_xbar_BID = M03_AXI_bid[6:0];
  assign m03_couplers_to_axi_dram_xbar_BRESP = M03_AXI_bresp[1:0];
  assign m03_couplers_to_axi_dram_xbar_BVALID = M03_AXI_bvalid;
  assign m03_couplers_to_axi_dram_xbar_RDATA = M03_AXI_rdata[511:0];
  assign m03_couplers_to_axi_dram_xbar_RID = M03_AXI_rid[6:0];
  assign m03_couplers_to_axi_dram_xbar_RLAST = M03_AXI_rlast;
  assign m03_couplers_to_axi_dram_xbar_RRESP = M03_AXI_rresp[1:0];
  assign m03_couplers_to_axi_dram_xbar_RVALID = M03_AXI_rvalid;
  assign m03_couplers_to_axi_dram_xbar_WREADY = M03_AXI_wready;
  m00_couplers_imp_J5PC3E m00_couplers
       (.M_ACLK(axi_dram_xbar_ACLK_net),
        .M_ARESETN(axi_dram_xbar_ARESETN_net),
        .M_AXI_araddr(m00_couplers_to_axi_dram_xbar_ARADDR),
        .M_AXI_arburst(m00_couplers_to_axi_dram_xbar_ARBURST),
        .M_AXI_arcache(m00_couplers_to_axi_dram_xbar_ARCACHE),
        .M_AXI_arid(m00_couplers_to_axi_dram_xbar_ARID),
        .M_AXI_arlen(m00_couplers_to_axi_dram_xbar_ARLEN),
        .M_AXI_arlock(m00_couplers_to_axi_dram_xbar_ARLOCK),
        .M_AXI_arprot(m00_couplers_to_axi_dram_xbar_ARPROT),
        .M_AXI_arqos(m00_couplers_to_axi_dram_xbar_ARQOS),
        .M_AXI_arready(m00_couplers_to_axi_dram_xbar_ARREADY),
        .M_AXI_arregion(m00_couplers_to_axi_dram_xbar_ARREGION),
        .M_AXI_arsize(m00_couplers_to_axi_dram_xbar_ARSIZE),
        .M_AXI_arvalid(m00_couplers_to_axi_dram_xbar_ARVALID),
        .M_AXI_awaddr(m00_couplers_to_axi_dram_xbar_AWADDR),
        .M_AXI_awburst(m00_couplers_to_axi_dram_xbar_AWBURST),
        .M_AXI_awcache(m00_couplers_to_axi_dram_xbar_AWCACHE),
        .M_AXI_awid(m00_couplers_to_axi_dram_xbar_AWID),
        .M_AXI_awlen(m00_couplers_to_axi_dram_xbar_AWLEN),
        .M_AXI_awlock(m00_couplers_to_axi_dram_xbar_AWLOCK),
        .M_AXI_awprot(m00_couplers_to_axi_dram_xbar_AWPROT),
        .M_AXI_awqos(m00_couplers_to_axi_dram_xbar_AWQOS),
        .M_AXI_awready(m00_couplers_to_axi_dram_xbar_AWREADY),
        .M_AXI_awregion(m00_couplers_to_axi_dram_xbar_AWREGION),
        .M_AXI_awsize(m00_couplers_to_axi_dram_xbar_AWSIZE),
        .M_AXI_awvalid(m00_couplers_to_axi_dram_xbar_AWVALID),
        .M_AXI_bid(m00_couplers_to_axi_dram_xbar_BID),
        .M_AXI_bready(m00_couplers_to_axi_dram_xbar_BREADY),
        .M_AXI_bresp(m00_couplers_to_axi_dram_xbar_BRESP),
        .M_AXI_bvalid(m00_couplers_to_axi_dram_xbar_BVALID),
        .M_AXI_rdata(m00_couplers_to_axi_dram_xbar_RDATA),
        .M_AXI_rid(m00_couplers_to_axi_dram_xbar_RID),
        .M_AXI_rlast(m00_couplers_to_axi_dram_xbar_RLAST),
        .M_AXI_rready(m00_couplers_to_axi_dram_xbar_RREADY),
        .M_AXI_rresp(m00_couplers_to_axi_dram_xbar_RRESP),
        .M_AXI_rvalid(m00_couplers_to_axi_dram_xbar_RVALID),
        .M_AXI_wdata(m00_couplers_to_axi_dram_xbar_WDATA),
        .M_AXI_wlast(m00_couplers_to_axi_dram_xbar_WLAST),
        .M_AXI_wready(m00_couplers_to_axi_dram_xbar_WREADY),
        .M_AXI_wstrb(m00_couplers_to_axi_dram_xbar_WSTRB),
        .M_AXI_wvalid(m00_couplers_to_axi_dram_xbar_WVALID),
        .S_ACLK(axi_dram_xbar_ACLK_net),
        .S_ARESETN(axi_dram_xbar_ARESETN_net),
        .S_AXI_araddr(xbar_to_m00_couplers_ARADDR),
        .S_AXI_arburst(xbar_to_m00_couplers_ARBURST),
        .S_AXI_arcache(xbar_to_m00_couplers_ARCACHE),
        .S_AXI_arid(xbar_to_m00_couplers_ARID),
        .S_AXI_arlen(xbar_to_m00_couplers_ARLEN),
        .S_AXI_arlock(xbar_to_m00_couplers_ARLOCK),
        .S_AXI_arprot(xbar_to_m00_couplers_ARPROT),
        .S_AXI_arqos(xbar_to_m00_couplers_ARQOS),
        .S_AXI_arready(xbar_to_m00_couplers_ARREADY),
        .S_AXI_arregion(xbar_to_m00_couplers_ARREGION),
        .S_AXI_arsize(xbar_to_m00_couplers_ARSIZE),
        .S_AXI_arvalid(xbar_to_m00_couplers_ARVALID),
        .S_AXI_awaddr(xbar_to_m00_couplers_AWADDR),
        .S_AXI_awburst(xbar_to_m00_couplers_AWBURST),
        .S_AXI_awcache(xbar_to_m00_couplers_AWCACHE),
        .S_AXI_awid(xbar_to_m00_couplers_AWID),
        .S_AXI_awlen(xbar_to_m00_couplers_AWLEN),
        .S_AXI_awlock(xbar_to_m00_couplers_AWLOCK),
        .S_AXI_awprot(xbar_to_m00_couplers_AWPROT),
        .S_AXI_awqos(xbar_to_m00_couplers_AWQOS),
        .S_AXI_awready(xbar_to_m00_couplers_AWREADY),
        .S_AXI_awregion(xbar_to_m00_couplers_AWREGION),
        .S_AXI_awsize(xbar_to_m00_couplers_AWSIZE),
        .S_AXI_awvalid(xbar_to_m00_couplers_AWVALID),
        .S_AXI_bid(xbar_to_m00_couplers_BID),
        .S_AXI_bready(xbar_to_m00_couplers_BREADY),
        .S_AXI_bresp(xbar_to_m00_couplers_BRESP),
        .S_AXI_bvalid(xbar_to_m00_couplers_BVALID),
        .S_AXI_rdata(xbar_to_m00_couplers_RDATA),
        .S_AXI_rid(xbar_to_m00_couplers_RID),
        .S_AXI_rlast(xbar_to_m00_couplers_RLAST),
        .S_AXI_rready(xbar_to_m00_couplers_RREADY),
        .S_AXI_rresp(xbar_to_m00_couplers_RRESP),
        .S_AXI_rvalid(xbar_to_m00_couplers_RVALID),
        .S_AXI_wdata(xbar_to_m00_couplers_WDATA),
        .S_AXI_wlast(xbar_to_m00_couplers_WLAST),
        .S_AXI_wready(xbar_to_m00_couplers_WREADY),
        .S_AXI_wstrb(xbar_to_m00_couplers_WSTRB),
        .S_AXI_wvalid(xbar_to_m00_couplers_WVALID));
  m01_couplers_imp_1I0GTH5 m01_couplers
       (.M_ACLK(axi_dram_xbar_ACLK_net),
        .M_ARESETN(axi_dram_xbar_ARESETN_net),
        .M_AXI_araddr(m01_couplers_to_axi_dram_xbar_ARADDR),
        .M_AXI_arburst(m01_couplers_to_axi_dram_xbar_ARBURST),
        .M_AXI_arcache(m01_couplers_to_axi_dram_xbar_ARCACHE),
        .M_AXI_arid(m01_couplers_to_axi_dram_xbar_ARID),
        .M_AXI_arlen(m01_couplers_to_axi_dram_xbar_ARLEN),
        .M_AXI_arlock(m01_couplers_to_axi_dram_xbar_ARLOCK),
        .M_AXI_arprot(m01_couplers_to_axi_dram_xbar_ARPROT),
        .M_AXI_arqos(m01_couplers_to_axi_dram_xbar_ARQOS),
        .M_AXI_arready(m01_couplers_to_axi_dram_xbar_ARREADY),
        .M_AXI_arregion(m01_couplers_to_axi_dram_xbar_ARREGION),
        .M_AXI_arsize(m01_couplers_to_axi_dram_xbar_ARSIZE),
        .M_AXI_arvalid(m01_couplers_to_axi_dram_xbar_ARVALID),
        .M_AXI_awaddr(m01_couplers_to_axi_dram_xbar_AWADDR),
        .M_AXI_awburst(m01_couplers_to_axi_dram_xbar_AWBURST),
        .M_AXI_awcache(m01_couplers_to_axi_dram_xbar_AWCACHE),
        .M_AXI_awid(m01_couplers_to_axi_dram_xbar_AWID),
        .M_AXI_awlen(m01_couplers_to_axi_dram_xbar_AWLEN),
        .M_AXI_awlock(m01_couplers_to_axi_dram_xbar_AWLOCK),
        .M_AXI_awprot(m01_couplers_to_axi_dram_xbar_AWPROT),
        .M_AXI_awqos(m01_couplers_to_axi_dram_xbar_AWQOS),
        .M_AXI_awready(m01_couplers_to_axi_dram_xbar_AWREADY),
        .M_AXI_awregion(m01_couplers_to_axi_dram_xbar_AWREGION),
        .M_AXI_awsize(m01_couplers_to_axi_dram_xbar_AWSIZE),
        .M_AXI_awvalid(m01_couplers_to_axi_dram_xbar_AWVALID),
        .M_AXI_bid(m01_couplers_to_axi_dram_xbar_BID),
        .M_AXI_bready(m01_couplers_to_axi_dram_xbar_BREADY),
        .M_AXI_bresp(m01_couplers_to_axi_dram_xbar_BRESP),
        .M_AXI_bvalid(m01_couplers_to_axi_dram_xbar_BVALID),
        .M_AXI_rdata(m01_couplers_to_axi_dram_xbar_RDATA),
        .M_AXI_rid(m01_couplers_to_axi_dram_xbar_RID),
        .M_AXI_rlast(m01_couplers_to_axi_dram_xbar_RLAST),
        .M_AXI_rready(m01_couplers_to_axi_dram_xbar_RREADY),
        .M_AXI_rresp(m01_couplers_to_axi_dram_xbar_RRESP),
        .M_AXI_rvalid(m01_couplers_to_axi_dram_xbar_RVALID),
        .M_AXI_wdata(m01_couplers_to_axi_dram_xbar_WDATA),
        .M_AXI_wlast(m01_couplers_to_axi_dram_xbar_WLAST),
        .M_AXI_wready(m01_couplers_to_axi_dram_xbar_WREADY),
        .M_AXI_wstrb(m01_couplers_to_axi_dram_xbar_WSTRB),
        .M_AXI_wvalid(m01_couplers_to_axi_dram_xbar_WVALID),
        .S_ACLK(axi_dram_xbar_ACLK_net),
        .S_ARESETN(axi_dram_xbar_ARESETN_net),
        .S_AXI_araddr(xbar_to_m01_couplers_ARADDR),
        .S_AXI_arburst(xbar_to_m01_couplers_ARBURST),
        .S_AXI_arcache(xbar_to_m01_couplers_ARCACHE),
        .S_AXI_arid(xbar_to_m01_couplers_ARID),
        .S_AXI_arlen(xbar_to_m01_couplers_ARLEN),
        .S_AXI_arlock(xbar_to_m01_couplers_ARLOCK),
        .S_AXI_arprot(xbar_to_m01_couplers_ARPROT),
        .S_AXI_arqos(xbar_to_m01_couplers_ARQOS),
        .S_AXI_arready(xbar_to_m01_couplers_ARREADY),
        .S_AXI_arregion(xbar_to_m01_couplers_ARREGION),
        .S_AXI_arsize(xbar_to_m01_couplers_ARSIZE),
        .S_AXI_arvalid(xbar_to_m01_couplers_ARVALID),
        .S_AXI_awaddr(xbar_to_m01_couplers_AWADDR),
        .S_AXI_awburst(xbar_to_m01_couplers_AWBURST),
        .S_AXI_awcache(xbar_to_m01_couplers_AWCACHE),
        .S_AXI_awid(xbar_to_m01_couplers_AWID),
        .S_AXI_awlen(xbar_to_m01_couplers_AWLEN),
        .S_AXI_awlock(xbar_to_m01_couplers_AWLOCK),
        .S_AXI_awprot(xbar_to_m01_couplers_AWPROT),
        .S_AXI_awqos(xbar_to_m01_couplers_AWQOS),
        .S_AXI_awready(xbar_to_m01_couplers_AWREADY),
        .S_AXI_awregion(xbar_to_m01_couplers_AWREGION),
        .S_AXI_awsize(xbar_to_m01_couplers_AWSIZE),
        .S_AXI_awvalid(xbar_to_m01_couplers_AWVALID),
        .S_AXI_bid(xbar_to_m01_couplers_BID),
        .S_AXI_bready(xbar_to_m01_couplers_BREADY),
        .S_AXI_bresp(xbar_to_m01_couplers_BRESP),
        .S_AXI_bvalid(xbar_to_m01_couplers_BVALID),
        .S_AXI_rdata(xbar_to_m01_couplers_RDATA),
        .S_AXI_rid(xbar_to_m01_couplers_RID),
        .S_AXI_rlast(xbar_to_m01_couplers_RLAST),
        .S_AXI_rready(xbar_to_m01_couplers_RREADY),
        .S_AXI_rresp(xbar_to_m01_couplers_RRESP),
        .S_AXI_rvalid(xbar_to_m01_couplers_RVALID),
        .S_AXI_wdata(xbar_to_m01_couplers_WDATA),
        .S_AXI_wlast(xbar_to_m01_couplers_WLAST),
        .S_AXI_wready(xbar_to_m01_couplers_WREADY),
        .S_AXI_wstrb(xbar_to_m01_couplers_WSTRB),
        .S_AXI_wvalid(xbar_to_m01_couplers_WVALID));
  m02_couplers_imp_14A9NST m02_couplers
       (.M_ACLK(axi_dram_xbar_ACLK_net),
        .M_ARESETN(axi_dram_xbar_ARESETN_net),
        .M_AXI_araddr(m02_couplers_to_axi_dram_xbar_ARADDR),
        .M_AXI_arburst(m02_couplers_to_axi_dram_xbar_ARBURST),
        .M_AXI_arcache(m02_couplers_to_axi_dram_xbar_ARCACHE),
        .M_AXI_arid(m02_couplers_to_axi_dram_xbar_ARID),
        .M_AXI_arlen(m02_couplers_to_axi_dram_xbar_ARLEN),
        .M_AXI_arlock(m02_couplers_to_axi_dram_xbar_ARLOCK),
        .M_AXI_arprot(m02_couplers_to_axi_dram_xbar_ARPROT),
        .M_AXI_arqos(m02_couplers_to_axi_dram_xbar_ARQOS),
        .M_AXI_arready(m02_couplers_to_axi_dram_xbar_ARREADY),
        .M_AXI_arregion(m02_couplers_to_axi_dram_xbar_ARREGION),
        .M_AXI_arsize(m02_couplers_to_axi_dram_xbar_ARSIZE),
        .M_AXI_arvalid(m02_couplers_to_axi_dram_xbar_ARVALID),
        .M_AXI_awaddr(m02_couplers_to_axi_dram_xbar_AWADDR),
        .M_AXI_awburst(m02_couplers_to_axi_dram_xbar_AWBURST),
        .M_AXI_awcache(m02_couplers_to_axi_dram_xbar_AWCACHE),
        .M_AXI_awid(m02_couplers_to_axi_dram_xbar_AWID),
        .M_AXI_awlen(m02_couplers_to_axi_dram_xbar_AWLEN),
        .M_AXI_awlock(m02_couplers_to_axi_dram_xbar_AWLOCK),
        .M_AXI_awprot(m02_couplers_to_axi_dram_xbar_AWPROT),
        .M_AXI_awqos(m02_couplers_to_axi_dram_xbar_AWQOS),
        .M_AXI_awready(m02_couplers_to_axi_dram_xbar_AWREADY),
        .M_AXI_awregion(m02_couplers_to_axi_dram_xbar_AWREGION),
        .M_AXI_awsize(m02_couplers_to_axi_dram_xbar_AWSIZE),
        .M_AXI_awvalid(m02_couplers_to_axi_dram_xbar_AWVALID),
        .M_AXI_bid(m02_couplers_to_axi_dram_xbar_BID),
        .M_AXI_bready(m02_couplers_to_axi_dram_xbar_BREADY),
        .M_AXI_bresp(m02_couplers_to_axi_dram_xbar_BRESP),
        .M_AXI_bvalid(m02_couplers_to_axi_dram_xbar_BVALID),
        .M_AXI_rdata(m02_couplers_to_axi_dram_xbar_RDATA),
        .M_AXI_rid(m02_couplers_to_axi_dram_xbar_RID),
        .M_AXI_rlast(m02_couplers_to_axi_dram_xbar_RLAST),
        .M_AXI_rready(m02_couplers_to_axi_dram_xbar_RREADY),
        .M_AXI_rresp(m02_couplers_to_axi_dram_xbar_RRESP),
        .M_AXI_rvalid(m02_couplers_to_axi_dram_xbar_RVALID),
        .M_AXI_wdata(m02_couplers_to_axi_dram_xbar_WDATA),
        .M_AXI_wlast(m02_couplers_to_axi_dram_xbar_WLAST),
        .M_AXI_wready(m02_couplers_to_axi_dram_xbar_WREADY),
        .M_AXI_wstrb(m02_couplers_to_axi_dram_xbar_WSTRB),
        .M_AXI_wvalid(m02_couplers_to_axi_dram_xbar_WVALID),
        .S_ACLK(axi_dram_xbar_ACLK_net),
        .S_ARESETN(axi_dram_xbar_ARESETN_net),
        .S_AXI_araddr(xbar_to_m02_couplers_ARADDR),
        .S_AXI_arburst(xbar_to_m02_couplers_ARBURST),
        .S_AXI_arcache(xbar_to_m02_couplers_ARCACHE),
        .S_AXI_arid(xbar_to_m02_couplers_ARID),
        .S_AXI_arlen(xbar_to_m02_couplers_ARLEN),
        .S_AXI_arlock(xbar_to_m02_couplers_ARLOCK),
        .S_AXI_arprot(xbar_to_m02_couplers_ARPROT),
        .S_AXI_arqos(xbar_to_m02_couplers_ARQOS),
        .S_AXI_arready(xbar_to_m02_couplers_ARREADY),
        .S_AXI_arregion(xbar_to_m02_couplers_ARREGION),
        .S_AXI_arsize(xbar_to_m02_couplers_ARSIZE),
        .S_AXI_arvalid(xbar_to_m02_couplers_ARVALID),
        .S_AXI_awaddr(xbar_to_m02_couplers_AWADDR),
        .S_AXI_awburst(xbar_to_m02_couplers_AWBURST),
        .S_AXI_awcache(xbar_to_m02_couplers_AWCACHE),
        .S_AXI_awid(xbar_to_m02_couplers_AWID),
        .S_AXI_awlen(xbar_to_m02_couplers_AWLEN),
        .S_AXI_awlock(xbar_to_m02_couplers_AWLOCK),
        .S_AXI_awprot(xbar_to_m02_couplers_AWPROT),
        .S_AXI_awqos(xbar_to_m02_couplers_AWQOS),
        .S_AXI_awready(xbar_to_m02_couplers_AWREADY),
        .S_AXI_awregion(xbar_to_m02_couplers_AWREGION),
        .S_AXI_awsize(xbar_to_m02_couplers_AWSIZE),
        .S_AXI_awvalid(xbar_to_m02_couplers_AWVALID),
        .S_AXI_bid(xbar_to_m02_couplers_BID),
        .S_AXI_bready(xbar_to_m02_couplers_BREADY),
        .S_AXI_bresp(xbar_to_m02_couplers_BRESP),
        .S_AXI_bvalid(xbar_to_m02_couplers_BVALID),
        .S_AXI_rdata(xbar_to_m02_couplers_RDATA),
        .S_AXI_rid(xbar_to_m02_couplers_RID),
        .S_AXI_rlast(xbar_to_m02_couplers_RLAST),
        .S_AXI_rready(xbar_to_m02_couplers_RREADY),
        .S_AXI_rresp(xbar_to_m02_couplers_RRESP),
        .S_AXI_rvalid(xbar_to_m02_couplers_RVALID),
        .S_AXI_wdata(xbar_to_m02_couplers_WDATA),
        .S_AXI_wlast(xbar_to_m02_couplers_WLAST),
        .S_AXI_wready(xbar_to_m02_couplers_WREADY),
        .S_AXI_wstrb(xbar_to_m02_couplers_WSTRB),
        .S_AXI_wvalid(xbar_to_m02_couplers_WVALID));
  m03_couplers_imp_69LRZY m03_couplers
       (.M_ACLK(axi_dram_xbar_ACLK_net),
        .M_ARESETN(axi_dram_xbar_ARESETN_net),
        .M_AXI_araddr(m03_couplers_to_axi_dram_xbar_ARADDR),
        .M_AXI_arburst(m03_couplers_to_axi_dram_xbar_ARBURST),
        .M_AXI_arcache(m03_couplers_to_axi_dram_xbar_ARCACHE),
        .M_AXI_arid(m03_couplers_to_axi_dram_xbar_ARID),
        .M_AXI_arlen(m03_couplers_to_axi_dram_xbar_ARLEN),
        .M_AXI_arlock(m03_couplers_to_axi_dram_xbar_ARLOCK),
        .M_AXI_arprot(m03_couplers_to_axi_dram_xbar_ARPROT),
        .M_AXI_arqos(m03_couplers_to_axi_dram_xbar_ARQOS),
        .M_AXI_arready(m03_couplers_to_axi_dram_xbar_ARREADY),
        .M_AXI_arregion(m03_couplers_to_axi_dram_xbar_ARREGION),
        .M_AXI_arsize(m03_couplers_to_axi_dram_xbar_ARSIZE),
        .M_AXI_arvalid(m03_couplers_to_axi_dram_xbar_ARVALID),
        .M_AXI_awaddr(m03_couplers_to_axi_dram_xbar_AWADDR),
        .M_AXI_awburst(m03_couplers_to_axi_dram_xbar_AWBURST),
        .M_AXI_awcache(m03_couplers_to_axi_dram_xbar_AWCACHE),
        .M_AXI_awid(m03_couplers_to_axi_dram_xbar_AWID),
        .M_AXI_awlen(m03_couplers_to_axi_dram_xbar_AWLEN),
        .M_AXI_awlock(m03_couplers_to_axi_dram_xbar_AWLOCK),
        .M_AXI_awprot(m03_couplers_to_axi_dram_xbar_AWPROT),
        .M_AXI_awqos(m03_couplers_to_axi_dram_xbar_AWQOS),
        .M_AXI_awready(m03_couplers_to_axi_dram_xbar_AWREADY),
        .M_AXI_awregion(m03_couplers_to_axi_dram_xbar_AWREGION),
        .M_AXI_awsize(m03_couplers_to_axi_dram_xbar_AWSIZE),
        .M_AXI_awvalid(m03_couplers_to_axi_dram_xbar_AWVALID),
        .M_AXI_bid(m03_couplers_to_axi_dram_xbar_BID),
        .M_AXI_bready(m03_couplers_to_axi_dram_xbar_BREADY),
        .M_AXI_bresp(m03_couplers_to_axi_dram_xbar_BRESP),
        .M_AXI_bvalid(m03_couplers_to_axi_dram_xbar_BVALID),
        .M_AXI_rdata(m03_couplers_to_axi_dram_xbar_RDATA),
        .M_AXI_rid(m03_couplers_to_axi_dram_xbar_RID),
        .M_AXI_rlast(m03_couplers_to_axi_dram_xbar_RLAST),
        .M_AXI_rready(m03_couplers_to_axi_dram_xbar_RREADY),
        .M_AXI_rresp(m03_couplers_to_axi_dram_xbar_RRESP),
        .M_AXI_rvalid(m03_couplers_to_axi_dram_xbar_RVALID),
        .M_AXI_wdata(m03_couplers_to_axi_dram_xbar_WDATA),
        .M_AXI_wlast(m03_couplers_to_axi_dram_xbar_WLAST),
        .M_AXI_wready(m03_couplers_to_axi_dram_xbar_WREADY),
        .M_AXI_wstrb(m03_couplers_to_axi_dram_xbar_WSTRB),
        .M_AXI_wvalid(m03_couplers_to_axi_dram_xbar_WVALID),
        .S_ACLK(axi_dram_xbar_ACLK_net),
        .S_ARESETN(axi_dram_xbar_ARESETN_net),
        .S_AXI_araddr(xbar_to_m03_couplers_ARADDR),
        .S_AXI_arburst(xbar_to_m03_couplers_ARBURST),
        .S_AXI_arcache(xbar_to_m03_couplers_ARCACHE),
        .S_AXI_arid(xbar_to_m03_couplers_ARID),
        .S_AXI_arlen(xbar_to_m03_couplers_ARLEN),
        .S_AXI_arlock(xbar_to_m03_couplers_ARLOCK),
        .S_AXI_arprot(xbar_to_m03_couplers_ARPROT),
        .S_AXI_arqos(xbar_to_m03_couplers_ARQOS),
        .S_AXI_arready(xbar_to_m03_couplers_ARREADY),
        .S_AXI_arregion(xbar_to_m03_couplers_ARREGION),
        .S_AXI_arsize(xbar_to_m03_couplers_ARSIZE),
        .S_AXI_arvalid(xbar_to_m03_couplers_ARVALID),
        .S_AXI_awaddr(xbar_to_m03_couplers_AWADDR),
        .S_AXI_awburst(xbar_to_m03_couplers_AWBURST),
        .S_AXI_awcache(xbar_to_m03_couplers_AWCACHE),
        .S_AXI_awid(xbar_to_m03_couplers_AWID),
        .S_AXI_awlen(xbar_to_m03_couplers_AWLEN),
        .S_AXI_awlock(xbar_to_m03_couplers_AWLOCK),
        .S_AXI_awprot(xbar_to_m03_couplers_AWPROT),
        .S_AXI_awqos(xbar_to_m03_couplers_AWQOS),
        .S_AXI_awready(xbar_to_m03_couplers_AWREADY),
        .S_AXI_awregion(xbar_to_m03_couplers_AWREGION),
        .S_AXI_awsize(xbar_to_m03_couplers_AWSIZE),
        .S_AXI_awvalid(xbar_to_m03_couplers_AWVALID),
        .S_AXI_bid(xbar_to_m03_couplers_BID),
        .S_AXI_bready(xbar_to_m03_couplers_BREADY),
        .S_AXI_bresp(xbar_to_m03_couplers_BRESP),
        .S_AXI_bvalid(xbar_to_m03_couplers_BVALID),
        .S_AXI_rdata(xbar_to_m03_couplers_RDATA),
        .S_AXI_rid(xbar_to_m03_couplers_RID),
        .S_AXI_rlast(xbar_to_m03_couplers_RLAST),
        .S_AXI_rready(xbar_to_m03_couplers_RREADY),
        .S_AXI_rresp(xbar_to_m03_couplers_RRESP),
        .S_AXI_rvalid(xbar_to_m03_couplers_RVALID),
        .S_AXI_wdata(xbar_to_m03_couplers_WDATA),
        .S_AXI_wlast(xbar_to_m03_couplers_WLAST),
        .S_AXI_wready(xbar_to_m03_couplers_WREADY),
        .S_AXI_wstrb(xbar_to_m03_couplers_WSTRB),
        .S_AXI_wvalid(xbar_to_m03_couplers_WVALID));
  s00_couplers_imp_1HVTIW6 s00_couplers
       (.M_ACLK(axi_dram_xbar_ACLK_net),
        .M_ARESETN(axi_dram_xbar_ARESETN_net),
        .M_AXI_araddr(s00_couplers_to_xbar_ARADDR),
        .M_AXI_arburst(s00_couplers_to_xbar_ARBURST),
        .M_AXI_arcache(s00_couplers_to_xbar_ARCACHE),
        .M_AXI_arid(s00_couplers_to_xbar_ARID),
        .M_AXI_arlen(s00_couplers_to_xbar_ARLEN),
        .M_AXI_arlock(s00_couplers_to_xbar_ARLOCK),
        .M_AXI_arprot(s00_couplers_to_xbar_ARPROT),
        .M_AXI_arqos(s00_couplers_to_xbar_ARQOS),
        .M_AXI_arready(s00_couplers_to_xbar_ARREADY),
        .M_AXI_arsize(s00_couplers_to_xbar_ARSIZE),
        .M_AXI_arvalid(s00_couplers_to_xbar_ARVALID),
        .M_AXI_awaddr(s00_couplers_to_xbar_AWADDR),
        .M_AXI_awburst(s00_couplers_to_xbar_AWBURST),
        .M_AXI_awcache(s00_couplers_to_xbar_AWCACHE),
        .M_AXI_awid(s00_couplers_to_xbar_AWID),
        .M_AXI_awlen(s00_couplers_to_xbar_AWLEN),
        .M_AXI_awlock(s00_couplers_to_xbar_AWLOCK),
        .M_AXI_awprot(s00_couplers_to_xbar_AWPROT),
        .M_AXI_awqos(s00_couplers_to_xbar_AWQOS),
        .M_AXI_awready(s00_couplers_to_xbar_AWREADY),
        .M_AXI_awsize(s00_couplers_to_xbar_AWSIZE),
        .M_AXI_awvalid(s00_couplers_to_xbar_AWVALID),
        .M_AXI_bid(s00_couplers_to_xbar_BID),
        .M_AXI_bready(s00_couplers_to_xbar_BREADY),
        .M_AXI_bresp(s00_couplers_to_xbar_BRESP),
        .M_AXI_bvalid(s00_couplers_to_xbar_BVALID),
        .M_AXI_rdata(s00_couplers_to_xbar_RDATA),
        .M_AXI_rid(s00_couplers_to_xbar_RID),
        .M_AXI_rlast(s00_couplers_to_xbar_RLAST),
        .M_AXI_rready(s00_couplers_to_xbar_RREADY),
        .M_AXI_rresp(s00_couplers_to_xbar_RRESP),
        .M_AXI_rvalid(s00_couplers_to_xbar_RVALID),
        .M_AXI_wdata(s00_couplers_to_xbar_WDATA),
        .M_AXI_wlast(s00_couplers_to_xbar_WLAST),
        .M_AXI_wready(s00_couplers_to_xbar_WREADY),
        .M_AXI_wstrb(s00_couplers_to_xbar_WSTRB),
        .M_AXI_wvalid(s00_couplers_to_xbar_WVALID),
        .S_ACLK(axi_dram_xbar_ACLK_net),
        .S_ARESETN(axi_dram_xbar_ARESETN_net),
        .S_AXI_araddr(axi_dram_xbar_to_s00_couplers_ARADDR),
        .S_AXI_arburst(axi_dram_xbar_to_s00_couplers_ARBURST),
        .S_AXI_arcache(axi_dram_xbar_to_s00_couplers_ARCACHE),
        .S_AXI_arid(axi_dram_xbar_to_s00_couplers_ARID),
        .S_AXI_arlen(axi_dram_xbar_to_s00_couplers_ARLEN),
        .S_AXI_arlock(axi_dram_xbar_to_s00_couplers_ARLOCK),
        .S_AXI_arprot(axi_dram_xbar_to_s00_couplers_ARPROT),
        .S_AXI_arqos(axi_dram_xbar_to_s00_couplers_ARQOS),
        .S_AXI_arready(axi_dram_xbar_to_s00_couplers_ARREADY),
        .S_AXI_arregion(axi_dram_xbar_to_s00_couplers_ARREGION),
        .S_AXI_arsize(axi_dram_xbar_to_s00_couplers_ARSIZE),
        .S_AXI_arvalid(axi_dram_xbar_to_s00_couplers_ARVALID),
        .S_AXI_awaddr(axi_dram_xbar_to_s00_couplers_AWADDR),
        .S_AXI_awburst(axi_dram_xbar_to_s00_couplers_AWBURST),
        .S_AXI_awcache(axi_dram_xbar_to_s00_couplers_AWCACHE),
        .S_AXI_awid(axi_dram_xbar_to_s00_couplers_AWID),
        .S_AXI_awlen(axi_dram_xbar_to_s00_couplers_AWLEN),
        .S_AXI_awlock(axi_dram_xbar_to_s00_couplers_AWLOCK),
        .S_AXI_awprot(axi_dram_xbar_to_s00_couplers_AWPROT),
        .S_AXI_awqos(axi_dram_xbar_to_s00_couplers_AWQOS),
        .S_AXI_awready(axi_dram_xbar_to_s00_couplers_AWREADY),
        .S_AXI_awregion(axi_dram_xbar_to_s00_couplers_AWREGION),
        .S_AXI_awsize(axi_dram_xbar_to_s00_couplers_AWSIZE),
        .S_AXI_awvalid(axi_dram_xbar_to_s00_couplers_AWVALID),
        .S_AXI_bid(axi_dram_xbar_to_s00_couplers_BID),
        .S_AXI_bready(axi_dram_xbar_to_s00_couplers_BREADY),
        .S_AXI_bresp(axi_dram_xbar_to_s00_couplers_BRESP),
        .S_AXI_bvalid(axi_dram_xbar_to_s00_couplers_BVALID),
        .S_AXI_rdata(axi_dram_xbar_to_s00_couplers_RDATA),
        .S_AXI_rid(axi_dram_xbar_to_s00_couplers_RID),
        .S_AXI_rlast(axi_dram_xbar_to_s00_couplers_RLAST),
        .S_AXI_rready(axi_dram_xbar_to_s00_couplers_RREADY),
        .S_AXI_rresp(axi_dram_xbar_to_s00_couplers_RRESP),
        .S_AXI_rvalid(axi_dram_xbar_to_s00_couplers_RVALID),
        .S_AXI_wdata(axi_dram_xbar_to_s00_couplers_WDATA),
        .S_AXI_wlast(axi_dram_xbar_to_s00_couplers_WLAST),
        .S_AXI_wready(axi_dram_xbar_to_s00_couplers_WREADY),
        .S_AXI_wstrb(axi_dram_xbar_to_s00_couplers_WSTRB),
        .S_AXI_wvalid(axi_dram_xbar_to_s00_couplers_WVALID));
  s01_couplers_imp_JB2BAD s01_couplers
       (.M_ACLK(axi_dram_xbar_ACLK_net),
        .M_ARESETN(axi_dram_xbar_ARESETN_net),
        .M_AXI_araddr(s01_couplers_to_xbar_ARADDR),
        .M_AXI_arburst(s01_couplers_to_xbar_ARBURST),
        .M_AXI_arcache(s01_couplers_to_xbar_ARCACHE),
        .M_AXI_arid(s01_couplers_to_xbar_ARID),
        .M_AXI_arlen(s01_couplers_to_xbar_ARLEN),
        .M_AXI_arlock(s01_couplers_to_xbar_ARLOCK),
        .M_AXI_arprot(s01_couplers_to_xbar_ARPROT),
        .M_AXI_arqos(s01_couplers_to_xbar_ARQOS),
        .M_AXI_arready(s01_couplers_to_xbar_ARREADY),
        .M_AXI_arsize(s01_couplers_to_xbar_ARSIZE),
        .M_AXI_arvalid(s01_couplers_to_xbar_ARVALID),
        .M_AXI_awaddr(s01_couplers_to_xbar_AWADDR),
        .M_AXI_awburst(s01_couplers_to_xbar_AWBURST),
        .M_AXI_awcache(s01_couplers_to_xbar_AWCACHE),
        .M_AXI_awid(s01_couplers_to_xbar_AWID),
        .M_AXI_awlen(s01_couplers_to_xbar_AWLEN),
        .M_AXI_awlock(s01_couplers_to_xbar_AWLOCK),
        .M_AXI_awprot(s01_couplers_to_xbar_AWPROT),
        .M_AXI_awqos(s01_couplers_to_xbar_AWQOS),
        .M_AXI_awready(s01_couplers_to_xbar_AWREADY),
        .M_AXI_awsize(s01_couplers_to_xbar_AWSIZE),
        .M_AXI_awvalid(s01_couplers_to_xbar_AWVALID),
        .M_AXI_bid(s01_couplers_to_xbar_BID),
        .M_AXI_bready(s01_couplers_to_xbar_BREADY),
        .M_AXI_bresp(s01_couplers_to_xbar_BRESP),
        .M_AXI_bvalid(s01_couplers_to_xbar_BVALID),
        .M_AXI_rdata(s01_couplers_to_xbar_RDATA),
        .M_AXI_rid(s01_couplers_to_xbar_RID),
        .M_AXI_rlast(s01_couplers_to_xbar_RLAST),
        .M_AXI_rready(s01_couplers_to_xbar_RREADY),
        .M_AXI_rresp(s01_couplers_to_xbar_RRESP),
        .M_AXI_rvalid(s01_couplers_to_xbar_RVALID),
        .M_AXI_wdata(s01_couplers_to_xbar_WDATA),
        .M_AXI_wlast(s01_couplers_to_xbar_WLAST),
        .M_AXI_wready(s01_couplers_to_xbar_WREADY),
        .M_AXI_wstrb(s01_couplers_to_xbar_WSTRB),
        .M_AXI_wvalid(s01_couplers_to_xbar_WVALID),
        .S_ACLK(axi_dram_xbar_ACLK_net),
        .S_ARESETN(axi_dram_xbar_ARESETN_net),
        .S_AXI_araddr(axi_dram_xbar_to_s01_couplers_ARADDR),
        .S_AXI_arburst(axi_dram_xbar_to_s01_couplers_ARBURST),
        .S_AXI_arcache(axi_dram_xbar_to_s01_couplers_ARCACHE),
        .S_AXI_arid(axi_dram_xbar_to_s01_couplers_ARID),
        .S_AXI_arlen(axi_dram_xbar_to_s01_couplers_ARLEN),
        .S_AXI_arlock(axi_dram_xbar_to_s01_couplers_ARLOCK),
        .S_AXI_arprot(axi_dram_xbar_to_s01_couplers_ARPROT),
        .S_AXI_arqos(axi_dram_xbar_to_s01_couplers_ARQOS),
        .S_AXI_arready(axi_dram_xbar_to_s01_couplers_ARREADY),
        .S_AXI_arregion(axi_dram_xbar_to_s01_couplers_ARREGION),
        .S_AXI_arsize(axi_dram_xbar_to_s01_couplers_ARSIZE),
        .S_AXI_arvalid(axi_dram_xbar_to_s01_couplers_ARVALID),
        .S_AXI_awaddr(axi_dram_xbar_to_s01_couplers_AWADDR),
        .S_AXI_awburst(axi_dram_xbar_to_s01_couplers_AWBURST),
        .S_AXI_awcache(axi_dram_xbar_to_s01_couplers_AWCACHE),
        .S_AXI_awid(axi_dram_xbar_to_s01_couplers_AWID),
        .S_AXI_awlen(axi_dram_xbar_to_s01_couplers_AWLEN),
        .S_AXI_awlock(axi_dram_xbar_to_s01_couplers_AWLOCK),
        .S_AXI_awprot(axi_dram_xbar_to_s01_couplers_AWPROT),
        .S_AXI_awqos(axi_dram_xbar_to_s01_couplers_AWQOS),
        .S_AXI_awready(axi_dram_xbar_to_s01_couplers_AWREADY),
        .S_AXI_awregion(axi_dram_xbar_to_s01_couplers_AWREGION),
        .S_AXI_awsize(axi_dram_xbar_to_s01_couplers_AWSIZE),
        .S_AXI_awvalid(axi_dram_xbar_to_s01_couplers_AWVALID),
        .S_AXI_bid(axi_dram_xbar_to_s01_couplers_BID),
        .S_AXI_bready(axi_dram_xbar_to_s01_couplers_BREADY),
        .S_AXI_bresp(axi_dram_xbar_to_s01_couplers_BRESP),
        .S_AXI_bvalid(axi_dram_xbar_to_s01_couplers_BVALID),
        .S_AXI_rdata(axi_dram_xbar_to_s01_couplers_RDATA),
        .S_AXI_rid(axi_dram_xbar_to_s01_couplers_RID),
        .S_AXI_rlast(axi_dram_xbar_to_s01_couplers_RLAST),
        .S_AXI_rready(axi_dram_xbar_to_s01_couplers_RREADY),
        .S_AXI_rresp(axi_dram_xbar_to_s01_couplers_RRESP),
        .S_AXI_rvalid(axi_dram_xbar_to_s01_couplers_RVALID),
        .S_AXI_wdata(axi_dram_xbar_to_s01_couplers_WDATA),
        .S_AXI_wlast(axi_dram_xbar_to_s01_couplers_WLAST),
        .S_AXI_wready(axi_dram_xbar_to_s01_couplers_WREADY),
        .S_AXI_wstrb(axi_dram_xbar_to_s01_couplers_WSTRB),
        .S_AXI_wvalid(axi_dram_xbar_to_s01_couplers_WVALID));
  cl_xbar_xbar_1 xbar
       (.aclk(axi_dram_xbar_ACLK_net),
        .aresetn(axi_dram_xbar_ARESETN_net),
        .m_axi_araddr({xbar_to_m03_couplers_ARADDR,xbar_to_m02_couplers_ARADDR,xbar_to_m01_couplers_ARADDR,xbar_to_m00_couplers_ARADDR}),
        .m_axi_arburst({xbar_to_m03_couplers_ARBURST,xbar_to_m02_couplers_ARBURST,xbar_to_m01_couplers_ARBURST,xbar_to_m00_couplers_ARBURST}),
        .m_axi_arcache({xbar_to_m03_couplers_ARCACHE,xbar_to_m02_couplers_ARCACHE,xbar_to_m01_couplers_ARCACHE,xbar_to_m00_couplers_ARCACHE}),
        .m_axi_arid({xbar_to_m03_couplers_ARID,xbar_to_m02_couplers_ARID,xbar_to_m01_couplers_ARID,xbar_to_m00_couplers_ARID}),
        .m_axi_arlen({xbar_to_m03_couplers_ARLEN,xbar_to_m02_couplers_ARLEN,xbar_to_m01_couplers_ARLEN,xbar_to_m00_couplers_ARLEN}),
        .m_axi_arlock({xbar_to_m03_couplers_ARLOCK,xbar_to_m02_couplers_ARLOCK,xbar_to_m01_couplers_ARLOCK,xbar_to_m00_couplers_ARLOCK}),
        .m_axi_arprot({xbar_to_m03_couplers_ARPROT,xbar_to_m02_couplers_ARPROT,xbar_to_m01_couplers_ARPROT,xbar_to_m00_couplers_ARPROT}),
        .m_axi_arqos({xbar_to_m03_couplers_ARQOS,xbar_to_m02_couplers_ARQOS,xbar_to_m01_couplers_ARQOS,xbar_to_m00_couplers_ARQOS}),
        .m_axi_arready({xbar_to_m03_couplers_ARREADY,xbar_to_m02_couplers_ARREADY,xbar_to_m01_couplers_ARREADY,xbar_to_m00_couplers_ARREADY}),
        .m_axi_arregion({xbar_to_m03_couplers_ARREGION,xbar_to_m02_couplers_ARREGION,xbar_to_m01_couplers_ARREGION,xbar_to_m00_couplers_ARREGION}),
        .m_axi_arsize({xbar_to_m03_couplers_ARSIZE,xbar_to_m02_couplers_ARSIZE,xbar_to_m01_couplers_ARSIZE,xbar_to_m00_couplers_ARSIZE}),
        .m_axi_arvalid({xbar_to_m03_couplers_ARVALID,xbar_to_m02_couplers_ARVALID,xbar_to_m01_couplers_ARVALID,xbar_to_m00_couplers_ARVALID}),
        .m_axi_awaddr({xbar_to_m03_couplers_AWADDR,xbar_to_m02_couplers_AWADDR,xbar_to_m01_couplers_AWADDR,xbar_to_m00_couplers_AWADDR}),
        .m_axi_awburst({xbar_to_m03_couplers_AWBURST,xbar_to_m02_couplers_AWBURST,xbar_to_m01_couplers_AWBURST,xbar_to_m00_couplers_AWBURST}),
        .m_axi_awcache({xbar_to_m03_couplers_AWCACHE,xbar_to_m02_couplers_AWCACHE,xbar_to_m01_couplers_AWCACHE,xbar_to_m00_couplers_AWCACHE}),
        .m_axi_awid({xbar_to_m03_couplers_AWID,xbar_to_m02_couplers_AWID,xbar_to_m01_couplers_AWID,xbar_to_m00_couplers_AWID}),
        .m_axi_awlen({xbar_to_m03_couplers_AWLEN,xbar_to_m02_couplers_AWLEN,xbar_to_m01_couplers_AWLEN,xbar_to_m00_couplers_AWLEN}),
        .m_axi_awlock({xbar_to_m03_couplers_AWLOCK,xbar_to_m02_couplers_AWLOCK,xbar_to_m01_couplers_AWLOCK,xbar_to_m00_couplers_AWLOCK}),
        .m_axi_awprot({xbar_to_m03_couplers_AWPROT,xbar_to_m02_couplers_AWPROT,xbar_to_m01_couplers_AWPROT,xbar_to_m00_couplers_AWPROT}),
        .m_axi_awqos({xbar_to_m03_couplers_AWQOS,xbar_to_m02_couplers_AWQOS,xbar_to_m01_couplers_AWQOS,xbar_to_m00_couplers_AWQOS}),
        .m_axi_awready({xbar_to_m03_couplers_AWREADY,xbar_to_m02_couplers_AWREADY,xbar_to_m01_couplers_AWREADY,xbar_to_m00_couplers_AWREADY}),
        .m_axi_awregion({xbar_to_m03_couplers_AWREGION,xbar_to_m02_couplers_AWREGION,xbar_to_m01_couplers_AWREGION,xbar_to_m00_couplers_AWREGION}),
        .m_axi_awsize({xbar_to_m03_couplers_AWSIZE,xbar_to_m02_couplers_AWSIZE,xbar_to_m01_couplers_AWSIZE,xbar_to_m00_couplers_AWSIZE}),
        .m_axi_awvalid({xbar_to_m03_couplers_AWVALID,xbar_to_m02_couplers_AWVALID,xbar_to_m01_couplers_AWVALID,xbar_to_m00_couplers_AWVALID}),
        .m_axi_bid({xbar_to_m03_couplers_BID,xbar_to_m02_couplers_BID,xbar_to_m01_couplers_BID,xbar_to_m00_couplers_BID}),
        .m_axi_bready({xbar_to_m03_couplers_BREADY,xbar_to_m02_couplers_BREADY,xbar_to_m01_couplers_BREADY,xbar_to_m00_couplers_BREADY}),
        .m_axi_bresp({xbar_to_m03_couplers_BRESP,xbar_to_m02_couplers_BRESP,xbar_to_m01_couplers_BRESP,xbar_to_m00_couplers_BRESP}),
        .m_axi_bvalid({xbar_to_m03_couplers_BVALID,xbar_to_m02_couplers_BVALID,xbar_to_m01_couplers_BVALID,xbar_to_m00_couplers_BVALID}),
        .m_axi_rdata({xbar_to_m03_couplers_RDATA,xbar_to_m02_couplers_RDATA,xbar_to_m01_couplers_RDATA,xbar_to_m00_couplers_RDATA}),
        .m_axi_rid({xbar_to_m03_couplers_RID,xbar_to_m02_couplers_RID,xbar_to_m01_couplers_RID,xbar_to_m00_couplers_RID}),
        .m_axi_rlast({xbar_to_m03_couplers_RLAST,xbar_to_m02_couplers_RLAST,xbar_to_m01_couplers_RLAST,xbar_to_m00_couplers_RLAST}),
        .m_axi_rready({xbar_to_m03_couplers_RREADY,xbar_to_m02_couplers_RREADY,xbar_to_m01_couplers_RREADY,xbar_to_m00_couplers_RREADY}),
        .m_axi_rresp({xbar_to_m03_couplers_RRESP,xbar_to_m02_couplers_RRESP,xbar_to_m01_couplers_RRESP,xbar_to_m00_couplers_RRESP}),
        .m_axi_rvalid({xbar_to_m03_couplers_RVALID,xbar_to_m02_couplers_RVALID,xbar_to_m01_couplers_RVALID,xbar_to_m00_couplers_RVALID}),
        .m_axi_wdata({xbar_to_m03_couplers_WDATA,xbar_to_m02_couplers_WDATA,xbar_to_m01_couplers_WDATA,xbar_to_m00_couplers_WDATA}),
        .m_axi_wlast({xbar_to_m03_couplers_WLAST,xbar_to_m02_couplers_WLAST,xbar_to_m01_couplers_WLAST,xbar_to_m00_couplers_WLAST}),
        .m_axi_wready({xbar_to_m03_couplers_WREADY,xbar_to_m02_couplers_WREADY,xbar_to_m01_couplers_WREADY,xbar_to_m00_couplers_WREADY}),
        .m_axi_wstrb({xbar_to_m03_couplers_WSTRB,xbar_to_m02_couplers_WSTRB,xbar_to_m01_couplers_WSTRB,xbar_to_m00_couplers_WSTRB}),
        .m_axi_wvalid({xbar_to_m03_couplers_WVALID,xbar_to_m02_couplers_WVALID,xbar_to_m01_couplers_WVALID,xbar_to_m00_couplers_WVALID}),
        .s_axi_araddr({s01_couplers_to_xbar_ARADDR,s00_couplers_to_xbar_ARADDR}),
        .s_axi_arburst({s01_couplers_to_xbar_ARBURST,s00_couplers_to_xbar_ARBURST}),
        .s_axi_arcache({s01_couplers_to_xbar_ARCACHE,s00_couplers_to_xbar_ARCACHE}),
        .s_axi_arid({1'b0,s01_couplers_to_xbar_ARID,1'b0,s00_couplers_to_xbar_ARID}),
        .s_axi_arlen({s01_couplers_to_xbar_ARLEN,s00_couplers_to_xbar_ARLEN}),
        .s_axi_arlock({s01_couplers_to_xbar_ARLOCK,s00_couplers_to_xbar_ARLOCK}),
        .s_axi_arprot({s01_couplers_to_xbar_ARPROT,s00_couplers_to_xbar_ARPROT}),
        .s_axi_arqos({s01_couplers_to_xbar_ARQOS,s00_couplers_to_xbar_ARQOS}),
        .s_axi_arready({s01_couplers_to_xbar_ARREADY,s00_couplers_to_xbar_ARREADY}),
        .s_axi_arsize({s01_couplers_to_xbar_ARSIZE,s00_couplers_to_xbar_ARSIZE}),
        .s_axi_arvalid({s01_couplers_to_xbar_ARVALID,s00_couplers_to_xbar_ARVALID}),
        .s_axi_awaddr({s01_couplers_to_xbar_AWADDR,s00_couplers_to_xbar_AWADDR}),
        .s_axi_awburst({s01_couplers_to_xbar_AWBURST,s00_couplers_to_xbar_AWBURST}),
        .s_axi_awcache({s01_couplers_to_xbar_AWCACHE,s00_couplers_to_xbar_AWCACHE}),
        .s_axi_awid({1'b0,s01_couplers_to_xbar_AWID,1'b0,s00_couplers_to_xbar_AWID}),
        .s_axi_awlen({s01_couplers_to_xbar_AWLEN,s00_couplers_to_xbar_AWLEN}),
        .s_axi_awlock({s01_couplers_to_xbar_AWLOCK,s00_couplers_to_xbar_AWLOCK}),
        .s_axi_awprot({s01_couplers_to_xbar_AWPROT,s00_couplers_to_xbar_AWPROT}),
        .s_axi_awqos({s01_couplers_to_xbar_AWQOS,s00_couplers_to_xbar_AWQOS}),
        .s_axi_awready({s01_couplers_to_xbar_AWREADY,s00_couplers_to_xbar_AWREADY}),
        .s_axi_awsize({s01_couplers_to_xbar_AWSIZE,s00_couplers_to_xbar_AWSIZE}),
        .s_axi_awvalid({s01_couplers_to_xbar_AWVALID,s00_couplers_to_xbar_AWVALID}),
        .s_axi_bid({s01_couplers_to_xbar_BID,s00_couplers_to_xbar_BID}),
        .s_axi_bready({s01_couplers_to_xbar_BREADY,s00_couplers_to_xbar_BREADY}),
        .s_axi_bresp({s01_couplers_to_xbar_BRESP,s00_couplers_to_xbar_BRESP}),
        .s_axi_bvalid({s01_couplers_to_xbar_BVALID,s00_couplers_to_xbar_BVALID}),
        .s_axi_rdata({s01_couplers_to_xbar_RDATA,s00_couplers_to_xbar_RDATA}),
        .s_axi_rid({s01_couplers_to_xbar_RID,s00_couplers_to_xbar_RID}),
        .s_axi_rlast({s01_couplers_to_xbar_RLAST,s00_couplers_to_xbar_RLAST}),
        .s_axi_rready({s01_couplers_to_xbar_RREADY,s00_couplers_to_xbar_RREADY}),
        .s_axi_rresp({s01_couplers_to_xbar_RRESP,s00_couplers_to_xbar_RRESP}),
        .s_axi_rvalid({s01_couplers_to_xbar_RVALID,s00_couplers_to_xbar_RVALID}),
        .s_axi_wdata({s01_couplers_to_xbar_WDATA,s00_couplers_to_xbar_WDATA}),
        .s_axi_wlast({s01_couplers_to_xbar_WLAST,s00_couplers_to_xbar_WLAST}),
        .s_axi_wready({s01_couplers_to_xbar_WREADY,s00_couplers_to_xbar_WREADY}),
        .s_axi_wstrb({s01_couplers_to_xbar_WSTRB,s00_couplers_to_xbar_WSTRB}),
        .s_axi_wvalid({s01_couplers_to_xbar_WVALID,s00_couplers_to_xbar_WVALID}));
endmodule

module cl_xbar_axi_pcis_xbar_0
   (ACLK,
    ARESETN,
    M00_ACLK,
    M00_ARESETN,
    M00_AXI_araddr,
    M00_AXI_arburst,
    M00_AXI_arcache,
    M00_AXI_arid,
    M00_AXI_arlen,
    M00_AXI_arlock,
    M00_AXI_arprot,
    M00_AXI_arqos,
    M00_AXI_arready,
    M00_AXI_arregion,
    M00_AXI_arsize,
    M00_AXI_arvalid,
    M00_AXI_awaddr,
    M00_AXI_awburst,
    M00_AXI_awcache,
    M00_AXI_awid,
    M00_AXI_awlen,
    M00_AXI_awlock,
    M00_AXI_awprot,
    M00_AXI_awqos,
    M00_AXI_awready,
    M00_AXI_awregion,
    M00_AXI_awsize,
    M00_AXI_awvalid,
    M00_AXI_bid,
    M00_AXI_bready,
    M00_AXI_bresp,
    M00_AXI_bvalid,
    M00_AXI_rdata,
    M00_AXI_rid,
    M00_AXI_rlast,
    M00_AXI_rready,
    M00_AXI_rresp,
    M00_AXI_rvalid,
    M00_AXI_wdata,
    M00_AXI_wlast,
    M00_AXI_wready,
    M00_AXI_wstrb,
    M00_AXI_wvalid,
    M01_ACLK,
    M01_ARESETN,
    M01_AXI_araddr,
    M01_AXI_arburst,
    M01_AXI_arcache,
    M01_AXI_arid,
    M01_AXI_arlen,
    M01_AXI_arlock,
    M01_AXI_arprot,
    M01_AXI_arqos,
    M01_AXI_arready,
    M01_AXI_arregion,
    M01_AXI_arsize,
    M01_AXI_arvalid,
    M01_AXI_awaddr,
    M01_AXI_awburst,
    M01_AXI_awcache,
    M01_AXI_awid,
    M01_AXI_awlen,
    M01_AXI_awlock,
    M01_AXI_awprot,
    M01_AXI_awqos,
    M01_AXI_awready,
    M01_AXI_awregion,
    M01_AXI_awsize,
    M01_AXI_awvalid,
    M01_AXI_bid,
    M01_AXI_bready,
    M01_AXI_bresp,
    M01_AXI_bvalid,
    M01_AXI_rdata,
    M01_AXI_rid,
    M01_AXI_rlast,
    M01_AXI_rready,
    M01_AXI_rresp,
    M01_AXI_rvalid,
    M01_AXI_wdata,
    M01_AXI_wlast,
    M01_AXI_wready,
    M01_AXI_wstrb,
    M01_AXI_wvalid,
    S00_ACLK,
    S00_ARESETN,
    S00_AXI_araddr,
    S00_AXI_arburst,
    S00_AXI_arcache,
    S00_AXI_arid,
    S00_AXI_arlen,
    S00_AXI_arlock,
    S00_AXI_arprot,
    S00_AXI_arqos,
    S00_AXI_arready,
    S00_AXI_arregion,
    S00_AXI_arsize,
    S00_AXI_arvalid,
    S00_AXI_awaddr,
    S00_AXI_awburst,
    S00_AXI_awcache,
    S00_AXI_awid,
    S00_AXI_awlen,
    S00_AXI_awlock,
    S00_AXI_awprot,
    S00_AXI_awqos,
    S00_AXI_awready,
    S00_AXI_awregion,
    S00_AXI_awsize,
    S00_AXI_awvalid,
    S00_AXI_bid,
    S00_AXI_bready,
    S00_AXI_bresp,
    S00_AXI_bvalid,
    S00_AXI_rdata,
    S00_AXI_rid,
    S00_AXI_rlast,
    S00_AXI_rready,
    S00_AXI_rresp,
    S00_AXI_rvalid,
    S00_AXI_wdata,
    S00_AXI_wlast,
    S00_AXI_wready,
    S00_AXI_wstrb,
    S00_AXI_wvalid);
  input ACLK;
  input ARESETN;
  input M00_ACLK;
  input M00_ARESETN;
  output [63:0]M00_AXI_araddr;
  output [1:0]M00_AXI_arburst;
  output [3:0]M00_AXI_arcache;
  output [5:0]M00_AXI_arid;
  output [7:0]M00_AXI_arlen;
  output [0:0]M00_AXI_arlock;
  output [2:0]M00_AXI_arprot;
  output [3:0]M00_AXI_arqos;
  input M00_AXI_arready;
  output [3:0]M00_AXI_arregion;
  output [2:0]M00_AXI_arsize;
  output M00_AXI_arvalid;
  output [63:0]M00_AXI_awaddr;
  output [1:0]M00_AXI_awburst;
  output [3:0]M00_AXI_awcache;
  output [5:0]M00_AXI_awid;
  output [7:0]M00_AXI_awlen;
  output [0:0]M00_AXI_awlock;
  output [2:0]M00_AXI_awprot;
  output [3:0]M00_AXI_awqos;
  input M00_AXI_awready;
  output [3:0]M00_AXI_awregion;
  output [2:0]M00_AXI_awsize;
  output M00_AXI_awvalid;
  input [5:0]M00_AXI_bid;
  output M00_AXI_bready;
  input [1:0]M00_AXI_bresp;
  input M00_AXI_bvalid;
  input [511:0]M00_AXI_rdata;
  input [5:0]M00_AXI_rid;
  input M00_AXI_rlast;
  output M00_AXI_rready;
  input [1:0]M00_AXI_rresp;
  input M00_AXI_rvalid;
  output [511:0]M00_AXI_wdata;
  output M00_AXI_wlast;
  input M00_AXI_wready;
  output [63:0]M00_AXI_wstrb;
  output M00_AXI_wvalid;
  input M01_ACLK;
  input M01_ARESETN;
  output [63:0]M01_AXI_araddr;
  output [1:0]M01_AXI_arburst;
  output [3:0]M01_AXI_arcache;
  output [5:0]M01_AXI_arid;
  output [7:0]M01_AXI_arlen;
  output [0:0]M01_AXI_arlock;
  output [2:0]M01_AXI_arprot;
  output [3:0]M01_AXI_arqos;
  input M01_AXI_arready;
  output [3:0]M01_AXI_arregion;
  output [2:0]M01_AXI_arsize;
  output M01_AXI_arvalid;
  output [63:0]M01_AXI_awaddr;
  output [1:0]M01_AXI_awburst;
  output [3:0]M01_AXI_awcache;
  output [5:0]M01_AXI_awid;
  output [7:0]M01_AXI_awlen;
  output [0:0]M01_AXI_awlock;
  output [2:0]M01_AXI_awprot;
  output [3:0]M01_AXI_awqos;
  input M01_AXI_awready;
  output [3:0]M01_AXI_awregion;
  output [2:0]M01_AXI_awsize;
  output M01_AXI_awvalid;
  input [5:0]M01_AXI_bid;
  output M01_AXI_bready;
  input [1:0]M01_AXI_bresp;
  input M01_AXI_bvalid;
  input [511:0]M01_AXI_rdata;
  input [5:0]M01_AXI_rid;
  input M01_AXI_rlast;
  output M01_AXI_rready;
  input [1:0]M01_AXI_rresp;
  input M01_AXI_rvalid;
  output [511:0]M01_AXI_wdata;
  output M01_AXI_wlast;
  input M01_AXI_wready;
  output [63:0]M01_AXI_wstrb;
  output M01_AXI_wvalid;
  input S00_ACLK;
  input S00_ARESETN;
  input [63:0]S00_AXI_araddr;
  input [1:0]S00_AXI_arburst;
  input [3:0]S00_AXI_arcache;
  input [5:0]S00_AXI_arid;
  input [7:0]S00_AXI_arlen;
  input [0:0]S00_AXI_arlock;
  input [2:0]S00_AXI_arprot;
  input [3:0]S00_AXI_arqos;
  output S00_AXI_arready;
  input [3:0]S00_AXI_arregion;
  input [2:0]S00_AXI_arsize;
  input S00_AXI_arvalid;
  input [63:0]S00_AXI_awaddr;
  input [1:0]S00_AXI_awburst;
  input [3:0]S00_AXI_awcache;
  input [5:0]S00_AXI_awid;
  input [7:0]S00_AXI_awlen;
  input [0:0]S00_AXI_awlock;
  input [2:0]S00_AXI_awprot;
  input [3:0]S00_AXI_awqos;
  output S00_AXI_awready;
  input [3:0]S00_AXI_awregion;
  input [2:0]S00_AXI_awsize;
  input S00_AXI_awvalid;
  output [5:0]S00_AXI_bid;
  input S00_AXI_bready;
  output [1:0]S00_AXI_bresp;
  output S00_AXI_bvalid;
  output [511:0]S00_AXI_rdata;
  output [5:0]S00_AXI_rid;
  output S00_AXI_rlast;
  input S00_AXI_rready;
  output [1:0]S00_AXI_rresp;
  output S00_AXI_rvalid;
  input [511:0]S00_AXI_wdata;
  input S00_AXI_wlast;
  output S00_AXI_wready;
  input [63:0]S00_AXI_wstrb;
  input S00_AXI_wvalid;

  wire axi_pcis_xbar_ACLK_net;
  wire axi_pcis_xbar_ARESETN_net;
  wire [63:0]axi_pcis_xbar_to_s00_couplers_ARADDR;
  wire [1:0]axi_pcis_xbar_to_s00_couplers_ARBURST;
  wire [3:0]axi_pcis_xbar_to_s00_couplers_ARCACHE;
  wire [5:0]axi_pcis_xbar_to_s00_couplers_ARID;
  wire [7:0]axi_pcis_xbar_to_s00_couplers_ARLEN;
  wire [0:0]axi_pcis_xbar_to_s00_couplers_ARLOCK;
  wire [2:0]axi_pcis_xbar_to_s00_couplers_ARPROT;
  wire [3:0]axi_pcis_xbar_to_s00_couplers_ARQOS;
  wire axi_pcis_xbar_to_s00_couplers_ARREADY;
  wire [3:0]axi_pcis_xbar_to_s00_couplers_ARREGION;
  wire [2:0]axi_pcis_xbar_to_s00_couplers_ARSIZE;
  wire axi_pcis_xbar_to_s00_couplers_ARVALID;
  wire [63:0]axi_pcis_xbar_to_s00_couplers_AWADDR;
  wire [1:0]axi_pcis_xbar_to_s00_couplers_AWBURST;
  wire [3:0]axi_pcis_xbar_to_s00_couplers_AWCACHE;
  wire [5:0]axi_pcis_xbar_to_s00_couplers_AWID;
  wire [7:0]axi_pcis_xbar_to_s00_couplers_AWLEN;
  wire [0:0]axi_pcis_xbar_to_s00_couplers_AWLOCK;
  wire [2:0]axi_pcis_xbar_to_s00_couplers_AWPROT;
  wire [3:0]axi_pcis_xbar_to_s00_couplers_AWQOS;
  wire axi_pcis_xbar_to_s00_couplers_AWREADY;
  wire [3:0]axi_pcis_xbar_to_s00_couplers_AWREGION;
  wire [2:0]axi_pcis_xbar_to_s00_couplers_AWSIZE;
  wire axi_pcis_xbar_to_s00_couplers_AWVALID;
  wire [5:0]axi_pcis_xbar_to_s00_couplers_BID;
  wire axi_pcis_xbar_to_s00_couplers_BREADY;
  wire [1:0]axi_pcis_xbar_to_s00_couplers_BRESP;
  wire axi_pcis_xbar_to_s00_couplers_BVALID;
  wire [511:0]axi_pcis_xbar_to_s00_couplers_RDATA;
  wire [5:0]axi_pcis_xbar_to_s00_couplers_RID;
  wire axi_pcis_xbar_to_s00_couplers_RLAST;
  wire axi_pcis_xbar_to_s00_couplers_RREADY;
  wire [1:0]axi_pcis_xbar_to_s00_couplers_RRESP;
  wire axi_pcis_xbar_to_s00_couplers_RVALID;
  wire [511:0]axi_pcis_xbar_to_s00_couplers_WDATA;
  wire axi_pcis_xbar_to_s00_couplers_WLAST;
  wire axi_pcis_xbar_to_s00_couplers_WREADY;
  wire [63:0]axi_pcis_xbar_to_s00_couplers_WSTRB;
  wire axi_pcis_xbar_to_s00_couplers_WVALID;
  wire [63:0]m00_couplers_to_axi_pcis_xbar_ARADDR;
  wire [1:0]m00_couplers_to_axi_pcis_xbar_ARBURST;
  wire [3:0]m00_couplers_to_axi_pcis_xbar_ARCACHE;
  wire [5:0]m00_couplers_to_axi_pcis_xbar_ARID;
  wire [7:0]m00_couplers_to_axi_pcis_xbar_ARLEN;
  wire [0:0]m00_couplers_to_axi_pcis_xbar_ARLOCK;
  wire [2:0]m00_couplers_to_axi_pcis_xbar_ARPROT;
  wire [3:0]m00_couplers_to_axi_pcis_xbar_ARQOS;
  wire m00_couplers_to_axi_pcis_xbar_ARREADY;
  wire [3:0]m00_couplers_to_axi_pcis_xbar_ARREGION;
  wire [2:0]m00_couplers_to_axi_pcis_xbar_ARSIZE;
  wire m00_couplers_to_axi_pcis_xbar_ARVALID;
  wire [63:0]m00_couplers_to_axi_pcis_xbar_AWADDR;
  wire [1:0]m00_couplers_to_axi_pcis_xbar_AWBURST;
  wire [3:0]m00_couplers_to_axi_pcis_xbar_AWCACHE;
  wire [5:0]m00_couplers_to_axi_pcis_xbar_AWID;
  wire [7:0]m00_couplers_to_axi_pcis_xbar_AWLEN;
  wire [0:0]m00_couplers_to_axi_pcis_xbar_AWLOCK;
  wire [2:0]m00_couplers_to_axi_pcis_xbar_AWPROT;
  wire [3:0]m00_couplers_to_axi_pcis_xbar_AWQOS;
  wire m00_couplers_to_axi_pcis_xbar_AWREADY;
  wire [3:0]m00_couplers_to_axi_pcis_xbar_AWREGION;
  wire [2:0]m00_couplers_to_axi_pcis_xbar_AWSIZE;
  wire m00_couplers_to_axi_pcis_xbar_AWVALID;
  wire [5:0]m00_couplers_to_axi_pcis_xbar_BID;
  wire m00_couplers_to_axi_pcis_xbar_BREADY;
  wire [1:0]m00_couplers_to_axi_pcis_xbar_BRESP;
  wire m00_couplers_to_axi_pcis_xbar_BVALID;
  wire [511:0]m00_couplers_to_axi_pcis_xbar_RDATA;
  wire [5:0]m00_couplers_to_axi_pcis_xbar_RID;
  wire m00_couplers_to_axi_pcis_xbar_RLAST;
  wire m00_couplers_to_axi_pcis_xbar_RREADY;
  wire [1:0]m00_couplers_to_axi_pcis_xbar_RRESP;
  wire m00_couplers_to_axi_pcis_xbar_RVALID;
  wire [511:0]m00_couplers_to_axi_pcis_xbar_WDATA;
  wire m00_couplers_to_axi_pcis_xbar_WLAST;
  wire m00_couplers_to_axi_pcis_xbar_WREADY;
  wire [63:0]m00_couplers_to_axi_pcis_xbar_WSTRB;
  wire m00_couplers_to_axi_pcis_xbar_WVALID;
  wire [63:0]m01_couplers_to_axi_pcis_xbar_ARADDR;
  wire [1:0]m01_couplers_to_axi_pcis_xbar_ARBURST;
  wire [3:0]m01_couplers_to_axi_pcis_xbar_ARCACHE;
  wire [5:0]m01_couplers_to_axi_pcis_xbar_ARID;
  wire [7:0]m01_couplers_to_axi_pcis_xbar_ARLEN;
  wire [0:0]m01_couplers_to_axi_pcis_xbar_ARLOCK;
  wire [2:0]m01_couplers_to_axi_pcis_xbar_ARPROT;
  wire [3:0]m01_couplers_to_axi_pcis_xbar_ARQOS;
  wire m01_couplers_to_axi_pcis_xbar_ARREADY;
  wire [3:0]m01_couplers_to_axi_pcis_xbar_ARREGION;
  wire [2:0]m01_couplers_to_axi_pcis_xbar_ARSIZE;
  wire m01_couplers_to_axi_pcis_xbar_ARVALID;
  wire [63:0]m01_couplers_to_axi_pcis_xbar_AWADDR;
  wire [1:0]m01_couplers_to_axi_pcis_xbar_AWBURST;
  wire [3:0]m01_couplers_to_axi_pcis_xbar_AWCACHE;
  wire [5:0]m01_couplers_to_axi_pcis_xbar_AWID;
  wire [7:0]m01_couplers_to_axi_pcis_xbar_AWLEN;
  wire [0:0]m01_couplers_to_axi_pcis_xbar_AWLOCK;
  wire [2:0]m01_couplers_to_axi_pcis_xbar_AWPROT;
  wire [3:0]m01_couplers_to_axi_pcis_xbar_AWQOS;
  wire m01_couplers_to_axi_pcis_xbar_AWREADY;
  wire [3:0]m01_couplers_to_axi_pcis_xbar_AWREGION;
  wire [2:0]m01_couplers_to_axi_pcis_xbar_AWSIZE;
  wire m01_couplers_to_axi_pcis_xbar_AWVALID;
  wire [5:0]m01_couplers_to_axi_pcis_xbar_BID;
  wire m01_couplers_to_axi_pcis_xbar_BREADY;
  wire [1:0]m01_couplers_to_axi_pcis_xbar_BRESP;
  wire m01_couplers_to_axi_pcis_xbar_BVALID;
  wire [511:0]m01_couplers_to_axi_pcis_xbar_RDATA;
  wire [5:0]m01_couplers_to_axi_pcis_xbar_RID;
  wire m01_couplers_to_axi_pcis_xbar_RLAST;
  wire m01_couplers_to_axi_pcis_xbar_RREADY;
  wire [1:0]m01_couplers_to_axi_pcis_xbar_RRESP;
  wire m01_couplers_to_axi_pcis_xbar_RVALID;
  wire [511:0]m01_couplers_to_axi_pcis_xbar_WDATA;
  wire m01_couplers_to_axi_pcis_xbar_WLAST;
  wire m01_couplers_to_axi_pcis_xbar_WREADY;
  wire [63:0]m01_couplers_to_axi_pcis_xbar_WSTRB;
  wire m01_couplers_to_axi_pcis_xbar_WVALID;
  wire [63:0]s00_couplers_to_xbar_ARADDR;
  wire [1:0]s00_couplers_to_xbar_ARBURST;
  wire [3:0]s00_couplers_to_xbar_ARCACHE;
  wire [5:0]s00_couplers_to_xbar_ARID;
  wire [7:0]s00_couplers_to_xbar_ARLEN;
  wire [0:0]s00_couplers_to_xbar_ARLOCK;
  wire [2:0]s00_couplers_to_xbar_ARPROT;
  wire [3:0]s00_couplers_to_xbar_ARQOS;
  wire [0:0]s00_couplers_to_xbar_ARREADY;
  wire [2:0]s00_couplers_to_xbar_ARSIZE;
  wire s00_couplers_to_xbar_ARVALID;
  wire [63:0]s00_couplers_to_xbar_AWADDR;
  wire [1:0]s00_couplers_to_xbar_AWBURST;
  wire [3:0]s00_couplers_to_xbar_AWCACHE;
  wire [5:0]s00_couplers_to_xbar_AWID;
  wire [7:0]s00_couplers_to_xbar_AWLEN;
  wire [0:0]s00_couplers_to_xbar_AWLOCK;
  wire [2:0]s00_couplers_to_xbar_AWPROT;
  wire [3:0]s00_couplers_to_xbar_AWQOS;
  wire [0:0]s00_couplers_to_xbar_AWREADY;
  wire [2:0]s00_couplers_to_xbar_AWSIZE;
  wire s00_couplers_to_xbar_AWVALID;
  wire [5:0]s00_couplers_to_xbar_BID;
  wire s00_couplers_to_xbar_BREADY;
  wire [1:0]s00_couplers_to_xbar_BRESP;
  wire [0:0]s00_couplers_to_xbar_BVALID;
  wire [511:0]s00_couplers_to_xbar_RDATA;
  wire [5:0]s00_couplers_to_xbar_RID;
  wire [0:0]s00_couplers_to_xbar_RLAST;
  wire s00_couplers_to_xbar_RREADY;
  wire [1:0]s00_couplers_to_xbar_RRESP;
  wire [0:0]s00_couplers_to_xbar_RVALID;
  wire [511:0]s00_couplers_to_xbar_WDATA;
  wire s00_couplers_to_xbar_WLAST;
  wire [0:0]s00_couplers_to_xbar_WREADY;
  wire [63:0]s00_couplers_to_xbar_WSTRB;
  wire s00_couplers_to_xbar_WVALID;
  wire [63:0]xbar_to_m00_couplers_ARADDR;
  wire [1:0]xbar_to_m00_couplers_ARBURST;
  wire [3:0]xbar_to_m00_couplers_ARCACHE;
  wire [5:0]xbar_to_m00_couplers_ARID;
  wire [7:0]xbar_to_m00_couplers_ARLEN;
  wire [0:0]xbar_to_m00_couplers_ARLOCK;
  wire [2:0]xbar_to_m00_couplers_ARPROT;
  wire [3:0]xbar_to_m00_couplers_ARQOS;
  wire xbar_to_m00_couplers_ARREADY;
  wire [3:0]xbar_to_m00_couplers_ARREGION;
  wire [2:0]xbar_to_m00_couplers_ARSIZE;
  wire [0:0]xbar_to_m00_couplers_ARVALID;
  wire [63:0]xbar_to_m00_couplers_AWADDR;
  wire [1:0]xbar_to_m00_couplers_AWBURST;
  wire [3:0]xbar_to_m00_couplers_AWCACHE;
  wire [5:0]xbar_to_m00_couplers_AWID;
  wire [7:0]xbar_to_m00_couplers_AWLEN;
  wire [0:0]xbar_to_m00_couplers_AWLOCK;
  wire [2:0]xbar_to_m00_couplers_AWPROT;
  wire [3:0]xbar_to_m00_couplers_AWQOS;
  wire xbar_to_m00_couplers_AWREADY;
  wire [3:0]xbar_to_m00_couplers_AWREGION;
  wire [2:0]xbar_to_m00_couplers_AWSIZE;
  wire [0:0]xbar_to_m00_couplers_AWVALID;
  wire [5:0]xbar_to_m00_couplers_BID;
  wire [0:0]xbar_to_m00_couplers_BREADY;
  wire [1:0]xbar_to_m00_couplers_BRESP;
  wire xbar_to_m00_couplers_BVALID;
  wire [511:0]xbar_to_m00_couplers_RDATA;
  wire [5:0]xbar_to_m00_couplers_RID;
  wire xbar_to_m00_couplers_RLAST;
  wire [0:0]xbar_to_m00_couplers_RREADY;
  wire [1:0]xbar_to_m00_couplers_RRESP;
  wire xbar_to_m00_couplers_RVALID;
  wire [511:0]xbar_to_m00_couplers_WDATA;
  wire [0:0]xbar_to_m00_couplers_WLAST;
  wire xbar_to_m00_couplers_WREADY;
  wire [63:0]xbar_to_m00_couplers_WSTRB;
  wire [0:0]xbar_to_m00_couplers_WVALID;
  wire [127:64]xbar_to_m01_couplers_ARADDR;
  wire [3:2]xbar_to_m01_couplers_ARBURST;
  wire [7:4]xbar_to_m01_couplers_ARCACHE;
  wire [11:6]xbar_to_m01_couplers_ARID;
  wire [15:8]xbar_to_m01_couplers_ARLEN;
  wire [1:1]xbar_to_m01_couplers_ARLOCK;
  wire [5:3]xbar_to_m01_couplers_ARPROT;
  wire [7:4]xbar_to_m01_couplers_ARQOS;
  wire xbar_to_m01_couplers_ARREADY;
  wire [7:4]xbar_to_m01_couplers_ARREGION;
  wire [5:3]xbar_to_m01_couplers_ARSIZE;
  wire [1:1]xbar_to_m01_couplers_ARVALID;
  wire [127:64]xbar_to_m01_couplers_AWADDR;
  wire [3:2]xbar_to_m01_couplers_AWBURST;
  wire [7:4]xbar_to_m01_couplers_AWCACHE;
  wire [11:6]xbar_to_m01_couplers_AWID;
  wire [15:8]xbar_to_m01_couplers_AWLEN;
  wire [1:1]xbar_to_m01_couplers_AWLOCK;
  wire [5:3]xbar_to_m01_couplers_AWPROT;
  wire [7:4]xbar_to_m01_couplers_AWQOS;
  wire xbar_to_m01_couplers_AWREADY;
  wire [7:4]xbar_to_m01_couplers_AWREGION;
  wire [5:3]xbar_to_m01_couplers_AWSIZE;
  wire [1:1]xbar_to_m01_couplers_AWVALID;
  wire [5:0]xbar_to_m01_couplers_BID;
  wire [1:1]xbar_to_m01_couplers_BREADY;
  wire [1:0]xbar_to_m01_couplers_BRESP;
  wire xbar_to_m01_couplers_BVALID;
  wire [511:0]xbar_to_m01_couplers_RDATA;
  wire [5:0]xbar_to_m01_couplers_RID;
  wire xbar_to_m01_couplers_RLAST;
  wire [1:1]xbar_to_m01_couplers_RREADY;
  wire [1:0]xbar_to_m01_couplers_RRESP;
  wire xbar_to_m01_couplers_RVALID;
  wire [1023:512]xbar_to_m01_couplers_WDATA;
  wire [1:1]xbar_to_m01_couplers_WLAST;
  wire xbar_to_m01_couplers_WREADY;
  wire [127:64]xbar_to_m01_couplers_WSTRB;
  wire [1:1]xbar_to_m01_couplers_WVALID;

  assign M00_AXI_araddr[63:0] = m00_couplers_to_axi_pcis_xbar_ARADDR;
  assign M00_AXI_arburst[1:0] = m00_couplers_to_axi_pcis_xbar_ARBURST;
  assign M00_AXI_arcache[3:0] = m00_couplers_to_axi_pcis_xbar_ARCACHE;
  assign M00_AXI_arid[5:0] = m00_couplers_to_axi_pcis_xbar_ARID;
  assign M00_AXI_arlen[7:0] = m00_couplers_to_axi_pcis_xbar_ARLEN;
  assign M00_AXI_arlock[0] = m00_couplers_to_axi_pcis_xbar_ARLOCK;
  assign M00_AXI_arprot[2:0] = m00_couplers_to_axi_pcis_xbar_ARPROT;
  assign M00_AXI_arqos[3:0] = m00_couplers_to_axi_pcis_xbar_ARQOS;
  assign M00_AXI_arregion[3:0] = m00_couplers_to_axi_pcis_xbar_ARREGION;
  assign M00_AXI_arsize[2:0] = m00_couplers_to_axi_pcis_xbar_ARSIZE;
  assign M00_AXI_arvalid = m00_couplers_to_axi_pcis_xbar_ARVALID;
  assign M00_AXI_awaddr[63:0] = m00_couplers_to_axi_pcis_xbar_AWADDR;
  assign M00_AXI_awburst[1:0] = m00_couplers_to_axi_pcis_xbar_AWBURST;
  assign M00_AXI_awcache[3:0] = m00_couplers_to_axi_pcis_xbar_AWCACHE;
  assign M00_AXI_awid[5:0] = m00_couplers_to_axi_pcis_xbar_AWID;
  assign M00_AXI_awlen[7:0] = m00_couplers_to_axi_pcis_xbar_AWLEN;
  assign M00_AXI_awlock[0] = m00_couplers_to_axi_pcis_xbar_AWLOCK;
  assign M00_AXI_awprot[2:0] = m00_couplers_to_axi_pcis_xbar_AWPROT;
  assign M00_AXI_awqos[3:0] = m00_couplers_to_axi_pcis_xbar_AWQOS;
  assign M00_AXI_awregion[3:0] = m00_couplers_to_axi_pcis_xbar_AWREGION;
  assign M00_AXI_awsize[2:0] = m00_couplers_to_axi_pcis_xbar_AWSIZE;
  assign M00_AXI_awvalid = m00_couplers_to_axi_pcis_xbar_AWVALID;
  assign M00_AXI_bready = m00_couplers_to_axi_pcis_xbar_BREADY;
  assign M00_AXI_rready = m00_couplers_to_axi_pcis_xbar_RREADY;
  assign M00_AXI_wdata[511:0] = m00_couplers_to_axi_pcis_xbar_WDATA;
  assign M00_AXI_wlast = m00_couplers_to_axi_pcis_xbar_WLAST;
  assign M00_AXI_wstrb[63:0] = m00_couplers_to_axi_pcis_xbar_WSTRB;
  assign M00_AXI_wvalid = m00_couplers_to_axi_pcis_xbar_WVALID;
  assign M01_AXI_araddr[63:0] = m01_couplers_to_axi_pcis_xbar_ARADDR;
  assign M01_AXI_arburst[1:0] = m01_couplers_to_axi_pcis_xbar_ARBURST;
  assign M01_AXI_arcache[3:0] = m01_couplers_to_axi_pcis_xbar_ARCACHE;
  assign M01_AXI_arid[5:0] = m01_couplers_to_axi_pcis_xbar_ARID;
  assign M01_AXI_arlen[7:0] = m01_couplers_to_axi_pcis_xbar_ARLEN;
  assign M01_AXI_arlock[0] = m01_couplers_to_axi_pcis_xbar_ARLOCK;
  assign M01_AXI_arprot[2:0] = m01_couplers_to_axi_pcis_xbar_ARPROT;
  assign M01_AXI_arqos[3:0] = m01_couplers_to_axi_pcis_xbar_ARQOS;
  assign M01_AXI_arregion[3:0] = m01_couplers_to_axi_pcis_xbar_ARREGION;
  assign M01_AXI_arsize[2:0] = m01_couplers_to_axi_pcis_xbar_ARSIZE;
  assign M01_AXI_arvalid = m01_couplers_to_axi_pcis_xbar_ARVALID;
  assign M01_AXI_awaddr[63:0] = m01_couplers_to_axi_pcis_xbar_AWADDR;
  assign M01_AXI_awburst[1:0] = m01_couplers_to_axi_pcis_xbar_AWBURST;
  assign M01_AXI_awcache[3:0] = m01_couplers_to_axi_pcis_xbar_AWCACHE;
  assign M01_AXI_awid[5:0] = m01_couplers_to_axi_pcis_xbar_AWID;
  assign M01_AXI_awlen[7:0] = m01_couplers_to_axi_pcis_xbar_AWLEN;
  assign M01_AXI_awlock[0] = m01_couplers_to_axi_pcis_xbar_AWLOCK;
  assign M01_AXI_awprot[2:0] = m01_couplers_to_axi_pcis_xbar_AWPROT;
  assign M01_AXI_awqos[3:0] = m01_couplers_to_axi_pcis_xbar_AWQOS;
  assign M01_AXI_awregion[3:0] = m01_couplers_to_axi_pcis_xbar_AWREGION;
  assign M01_AXI_awsize[2:0] = m01_couplers_to_axi_pcis_xbar_AWSIZE;
  assign M01_AXI_awvalid = m01_couplers_to_axi_pcis_xbar_AWVALID;
  assign M01_AXI_bready = m01_couplers_to_axi_pcis_xbar_BREADY;
  assign M01_AXI_rready = m01_couplers_to_axi_pcis_xbar_RREADY;
  assign M01_AXI_wdata[511:0] = m01_couplers_to_axi_pcis_xbar_WDATA;
  assign M01_AXI_wlast = m01_couplers_to_axi_pcis_xbar_WLAST;
  assign M01_AXI_wstrb[63:0] = m01_couplers_to_axi_pcis_xbar_WSTRB;
  assign M01_AXI_wvalid = m01_couplers_to_axi_pcis_xbar_WVALID;
  assign S00_AXI_arready = axi_pcis_xbar_to_s00_couplers_ARREADY;
  assign S00_AXI_awready = axi_pcis_xbar_to_s00_couplers_AWREADY;
  assign S00_AXI_bid[5:0] = axi_pcis_xbar_to_s00_couplers_BID;
  assign S00_AXI_bresp[1:0] = axi_pcis_xbar_to_s00_couplers_BRESP;
  assign S00_AXI_bvalid = axi_pcis_xbar_to_s00_couplers_BVALID;
  assign S00_AXI_rdata[511:0] = axi_pcis_xbar_to_s00_couplers_RDATA;
  assign S00_AXI_rid[5:0] = axi_pcis_xbar_to_s00_couplers_RID;
  assign S00_AXI_rlast = axi_pcis_xbar_to_s00_couplers_RLAST;
  assign S00_AXI_rresp[1:0] = axi_pcis_xbar_to_s00_couplers_RRESP;
  assign S00_AXI_rvalid = axi_pcis_xbar_to_s00_couplers_RVALID;
  assign S00_AXI_wready = axi_pcis_xbar_to_s00_couplers_WREADY;
  assign axi_pcis_xbar_ACLK_net = ACLK;
  assign axi_pcis_xbar_ARESETN_net = ARESETN;
  assign axi_pcis_xbar_to_s00_couplers_ARADDR = S00_AXI_araddr[63:0];
  assign axi_pcis_xbar_to_s00_couplers_ARBURST = S00_AXI_arburst[1:0];
  assign axi_pcis_xbar_to_s00_couplers_ARCACHE = S00_AXI_arcache[3:0];
  assign axi_pcis_xbar_to_s00_couplers_ARID = S00_AXI_arid[5:0];
  assign axi_pcis_xbar_to_s00_couplers_ARLEN = S00_AXI_arlen[7:0];
  assign axi_pcis_xbar_to_s00_couplers_ARLOCK = S00_AXI_arlock[0];
  assign axi_pcis_xbar_to_s00_couplers_ARPROT = S00_AXI_arprot[2:0];
  assign axi_pcis_xbar_to_s00_couplers_ARQOS = S00_AXI_arqos[3:0];
  assign axi_pcis_xbar_to_s00_couplers_ARREGION = S00_AXI_arregion[3:0];
  assign axi_pcis_xbar_to_s00_couplers_ARSIZE = S00_AXI_arsize[2:0];
  assign axi_pcis_xbar_to_s00_couplers_ARVALID = S00_AXI_arvalid;
  assign axi_pcis_xbar_to_s00_couplers_AWADDR = S00_AXI_awaddr[63:0];
  assign axi_pcis_xbar_to_s00_couplers_AWBURST = S00_AXI_awburst[1:0];
  assign axi_pcis_xbar_to_s00_couplers_AWCACHE = S00_AXI_awcache[3:0];
  assign axi_pcis_xbar_to_s00_couplers_AWID = S00_AXI_awid[5:0];
  assign axi_pcis_xbar_to_s00_couplers_AWLEN = S00_AXI_awlen[7:0];
  assign axi_pcis_xbar_to_s00_couplers_AWLOCK = S00_AXI_awlock[0];
  assign axi_pcis_xbar_to_s00_couplers_AWPROT = S00_AXI_awprot[2:0];
  assign axi_pcis_xbar_to_s00_couplers_AWQOS = S00_AXI_awqos[3:0];
  assign axi_pcis_xbar_to_s00_couplers_AWREGION = S00_AXI_awregion[3:0];
  assign axi_pcis_xbar_to_s00_couplers_AWSIZE = S00_AXI_awsize[2:0];
  assign axi_pcis_xbar_to_s00_couplers_AWVALID = S00_AXI_awvalid;
  assign axi_pcis_xbar_to_s00_couplers_BREADY = S00_AXI_bready;
  assign axi_pcis_xbar_to_s00_couplers_RREADY = S00_AXI_rready;
  assign axi_pcis_xbar_to_s00_couplers_WDATA = S00_AXI_wdata[511:0];
  assign axi_pcis_xbar_to_s00_couplers_WLAST = S00_AXI_wlast;
  assign axi_pcis_xbar_to_s00_couplers_WSTRB = S00_AXI_wstrb[63:0];
  assign axi_pcis_xbar_to_s00_couplers_WVALID = S00_AXI_wvalid;
  assign m00_couplers_to_axi_pcis_xbar_ARREADY = M00_AXI_arready;
  assign m00_couplers_to_axi_pcis_xbar_AWREADY = M00_AXI_awready;
  assign m00_couplers_to_axi_pcis_xbar_BID = M00_AXI_bid[5:0];
  assign m00_couplers_to_axi_pcis_xbar_BRESP = M00_AXI_bresp[1:0];
  assign m00_couplers_to_axi_pcis_xbar_BVALID = M00_AXI_bvalid;
  assign m00_couplers_to_axi_pcis_xbar_RDATA = M00_AXI_rdata[511:0];
  assign m00_couplers_to_axi_pcis_xbar_RID = M00_AXI_rid[5:0];
  assign m00_couplers_to_axi_pcis_xbar_RLAST = M00_AXI_rlast;
  assign m00_couplers_to_axi_pcis_xbar_RRESP = M00_AXI_rresp[1:0];
  assign m00_couplers_to_axi_pcis_xbar_RVALID = M00_AXI_rvalid;
  assign m00_couplers_to_axi_pcis_xbar_WREADY = M00_AXI_wready;
  assign m01_couplers_to_axi_pcis_xbar_ARREADY = M01_AXI_arready;
  assign m01_couplers_to_axi_pcis_xbar_AWREADY = M01_AXI_awready;
  assign m01_couplers_to_axi_pcis_xbar_BID = M01_AXI_bid[5:0];
  assign m01_couplers_to_axi_pcis_xbar_BRESP = M01_AXI_bresp[1:0];
  assign m01_couplers_to_axi_pcis_xbar_BVALID = M01_AXI_bvalid;
  assign m01_couplers_to_axi_pcis_xbar_RDATA = M01_AXI_rdata[511:0];
  assign m01_couplers_to_axi_pcis_xbar_RID = M01_AXI_rid[5:0];
  assign m01_couplers_to_axi_pcis_xbar_RLAST = M01_AXI_rlast;
  assign m01_couplers_to_axi_pcis_xbar_RRESP = M01_AXI_rresp[1:0];
  assign m01_couplers_to_axi_pcis_xbar_RVALID = M01_AXI_rvalid;
  assign m01_couplers_to_axi_pcis_xbar_WREADY = M01_AXI_wready;
  m00_couplers_imp_12YH38C m00_couplers
       (.M_ACLK(axi_pcis_xbar_ACLK_net),
        .M_ARESETN(axi_pcis_xbar_ARESETN_net),
        .M_AXI_araddr(m00_couplers_to_axi_pcis_xbar_ARADDR),
        .M_AXI_arburst(m00_couplers_to_axi_pcis_xbar_ARBURST),
        .M_AXI_arcache(m00_couplers_to_axi_pcis_xbar_ARCACHE),
        .M_AXI_arid(m00_couplers_to_axi_pcis_xbar_ARID),
        .M_AXI_arlen(m00_couplers_to_axi_pcis_xbar_ARLEN),
        .M_AXI_arlock(m00_couplers_to_axi_pcis_xbar_ARLOCK),
        .M_AXI_arprot(m00_couplers_to_axi_pcis_xbar_ARPROT),
        .M_AXI_arqos(m00_couplers_to_axi_pcis_xbar_ARQOS),
        .M_AXI_arready(m00_couplers_to_axi_pcis_xbar_ARREADY),
        .M_AXI_arregion(m00_couplers_to_axi_pcis_xbar_ARREGION),
        .M_AXI_arsize(m00_couplers_to_axi_pcis_xbar_ARSIZE),
        .M_AXI_arvalid(m00_couplers_to_axi_pcis_xbar_ARVALID),
        .M_AXI_awaddr(m00_couplers_to_axi_pcis_xbar_AWADDR),
        .M_AXI_awburst(m00_couplers_to_axi_pcis_xbar_AWBURST),
        .M_AXI_awcache(m00_couplers_to_axi_pcis_xbar_AWCACHE),
        .M_AXI_awid(m00_couplers_to_axi_pcis_xbar_AWID),
        .M_AXI_awlen(m00_couplers_to_axi_pcis_xbar_AWLEN),
        .M_AXI_awlock(m00_couplers_to_axi_pcis_xbar_AWLOCK),
        .M_AXI_awprot(m00_couplers_to_axi_pcis_xbar_AWPROT),
        .M_AXI_awqos(m00_couplers_to_axi_pcis_xbar_AWQOS),
        .M_AXI_awready(m00_couplers_to_axi_pcis_xbar_AWREADY),
        .M_AXI_awregion(m00_couplers_to_axi_pcis_xbar_AWREGION),
        .M_AXI_awsize(m00_couplers_to_axi_pcis_xbar_AWSIZE),
        .M_AXI_awvalid(m00_couplers_to_axi_pcis_xbar_AWVALID),
        .M_AXI_bid(m00_couplers_to_axi_pcis_xbar_BID),
        .M_AXI_bready(m00_couplers_to_axi_pcis_xbar_BREADY),
        .M_AXI_bresp(m00_couplers_to_axi_pcis_xbar_BRESP),
        .M_AXI_bvalid(m00_couplers_to_axi_pcis_xbar_BVALID),
        .M_AXI_rdata(m00_couplers_to_axi_pcis_xbar_RDATA),
        .M_AXI_rid(m00_couplers_to_axi_pcis_xbar_RID),
        .M_AXI_rlast(m00_couplers_to_axi_pcis_xbar_RLAST),
        .M_AXI_rready(m00_couplers_to_axi_pcis_xbar_RREADY),
        .M_AXI_rresp(m00_couplers_to_axi_pcis_xbar_RRESP),
        .M_AXI_rvalid(m00_couplers_to_axi_pcis_xbar_RVALID),
        .M_AXI_wdata(m00_couplers_to_axi_pcis_xbar_WDATA),
        .M_AXI_wlast(m00_couplers_to_axi_pcis_xbar_WLAST),
        .M_AXI_wready(m00_couplers_to_axi_pcis_xbar_WREADY),
        .M_AXI_wstrb(m00_couplers_to_axi_pcis_xbar_WSTRB),
        .M_AXI_wvalid(m00_couplers_to_axi_pcis_xbar_WVALID),
        .S_ACLK(axi_pcis_xbar_ACLK_net),
        .S_ARESETN(axi_pcis_xbar_ARESETN_net),
        .S_AXI_araddr(xbar_to_m00_couplers_ARADDR),
        .S_AXI_arburst(xbar_to_m00_couplers_ARBURST),
        .S_AXI_arcache(xbar_to_m00_couplers_ARCACHE),
        .S_AXI_arid(xbar_to_m00_couplers_ARID),
        .S_AXI_arlen(xbar_to_m00_couplers_ARLEN),
        .S_AXI_arlock(xbar_to_m00_couplers_ARLOCK),
        .S_AXI_arprot(xbar_to_m00_couplers_ARPROT),
        .S_AXI_arqos(xbar_to_m00_couplers_ARQOS),
        .S_AXI_arready(xbar_to_m00_couplers_ARREADY),
        .S_AXI_arregion(xbar_to_m00_couplers_ARREGION),
        .S_AXI_arsize(xbar_to_m00_couplers_ARSIZE),
        .S_AXI_arvalid(xbar_to_m00_couplers_ARVALID),
        .S_AXI_awaddr(xbar_to_m00_couplers_AWADDR),
        .S_AXI_awburst(xbar_to_m00_couplers_AWBURST),
        .S_AXI_awcache(xbar_to_m00_couplers_AWCACHE),
        .S_AXI_awid(xbar_to_m00_couplers_AWID),
        .S_AXI_awlen(xbar_to_m00_couplers_AWLEN),
        .S_AXI_awlock(xbar_to_m00_couplers_AWLOCK),
        .S_AXI_awprot(xbar_to_m00_couplers_AWPROT),
        .S_AXI_awqos(xbar_to_m00_couplers_AWQOS),
        .S_AXI_awready(xbar_to_m00_couplers_AWREADY),
        .S_AXI_awregion(xbar_to_m00_couplers_AWREGION),
        .S_AXI_awsize(xbar_to_m00_couplers_AWSIZE),
        .S_AXI_awvalid(xbar_to_m00_couplers_AWVALID),
        .S_AXI_bid(xbar_to_m00_couplers_BID),
        .S_AXI_bready(xbar_to_m00_couplers_BREADY),
        .S_AXI_bresp(xbar_to_m00_couplers_BRESP),
        .S_AXI_bvalid(xbar_to_m00_couplers_BVALID),
        .S_AXI_rdata(xbar_to_m00_couplers_RDATA),
        .S_AXI_rid(xbar_to_m00_couplers_RID),
        .S_AXI_rlast(xbar_to_m00_couplers_RLAST),
        .S_AXI_rready(xbar_to_m00_couplers_RREADY),
        .S_AXI_rresp(xbar_to_m00_couplers_RRESP),
        .S_AXI_rvalid(xbar_to_m00_couplers_RVALID),
        .S_AXI_wdata(xbar_to_m00_couplers_WDATA),
        .S_AXI_wlast(xbar_to_m00_couplers_WLAST),
        .S_AXI_wready(xbar_to_m00_couplers_WREADY),
        .S_AXI_wstrb(xbar_to_m00_couplers_WSTRB),
        .S_AXI_wvalid(xbar_to_m00_couplers_WVALID));
  m01_couplers_imp_39EW9R m01_couplers
       (.M_ACLK(axi_pcis_xbar_ACLK_net),
        .M_ARESETN(axi_pcis_xbar_ARESETN_net),
        .M_AXI_araddr(m01_couplers_to_axi_pcis_xbar_ARADDR),
        .M_AXI_arburst(m01_couplers_to_axi_pcis_xbar_ARBURST),
        .M_AXI_arcache(m01_couplers_to_axi_pcis_xbar_ARCACHE),
        .M_AXI_arid(m01_couplers_to_axi_pcis_xbar_ARID),
        .M_AXI_arlen(m01_couplers_to_axi_pcis_xbar_ARLEN),
        .M_AXI_arlock(m01_couplers_to_axi_pcis_xbar_ARLOCK),
        .M_AXI_arprot(m01_couplers_to_axi_pcis_xbar_ARPROT),
        .M_AXI_arqos(m01_couplers_to_axi_pcis_xbar_ARQOS),
        .M_AXI_arready(m01_couplers_to_axi_pcis_xbar_ARREADY),
        .M_AXI_arregion(m01_couplers_to_axi_pcis_xbar_ARREGION),
        .M_AXI_arsize(m01_couplers_to_axi_pcis_xbar_ARSIZE),
        .M_AXI_arvalid(m01_couplers_to_axi_pcis_xbar_ARVALID),
        .M_AXI_awaddr(m01_couplers_to_axi_pcis_xbar_AWADDR),
        .M_AXI_awburst(m01_couplers_to_axi_pcis_xbar_AWBURST),
        .M_AXI_awcache(m01_couplers_to_axi_pcis_xbar_AWCACHE),
        .M_AXI_awid(m01_couplers_to_axi_pcis_xbar_AWID),
        .M_AXI_awlen(m01_couplers_to_axi_pcis_xbar_AWLEN),
        .M_AXI_awlock(m01_couplers_to_axi_pcis_xbar_AWLOCK),
        .M_AXI_awprot(m01_couplers_to_axi_pcis_xbar_AWPROT),
        .M_AXI_awqos(m01_couplers_to_axi_pcis_xbar_AWQOS),
        .M_AXI_awready(m01_couplers_to_axi_pcis_xbar_AWREADY),
        .M_AXI_awregion(m01_couplers_to_axi_pcis_xbar_AWREGION),
        .M_AXI_awsize(m01_couplers_to_axi_pcis_xbar_AWSIZE),
        .M_AXI_awvalid(m01_couplers_to_axi_pcis_xbar_AWVALID),
        .M_AXI_bid(m01_couplers_to_axi_pcis_xbar_BID),
        .M_AXI_bready(m01_couplers_to_axi_pcis_xbar_BREADY),
        .M_AXI_bresp(m01_couplers_to_axi_pcis_xbar_BRESP),
        .M_AXI_bvalid(m01_couplers_to_axi_pcis_xbar_BVALID),
        .M_AXI_rdata(m01_couplers_to_axi_pcis_xbar_RDATA),
        .M_AXI_rid(m01_couplers_to_axi_pcis_xbar_RID),
        .M_AXI_rlast(m01_couplers_to_axi_pcis_xbar_RLAST),
        .M_AXI_rready(m01_couplers_to_axi_pcis_xbar_RREADY),
        .M_AXI_rresp(m01_couplers_to_axi_pcis_xbar_RRESP),
        .M_AXI_rvalid(m01_couplers_to_axi_pcis_xbar_RVALID),
        .M_AXI_wdata(m01_couplers_to_axi_pcis_xbar_WDATA),
        .M_AXI_wlast(m01_couplers_to_axi_pcis_xbar_WLAST),
        .M_AXI_wready(m01_couplers_to_axi_pcis_xbar_WREADY),
        .M_AXI_wstrb(m01_couplers_to_axi_pcis_xbar_WSTRB),
        .M_AXI_wvalid(m01_couplers_to_axi_pcis_xbar_WVALID),
        .S_ACLK(axi_pcis_xbar_ACLK_net),
        .S_ARESETN(axi_pcis_xbar_ARESETN_net),
        .S_AXI_araddr(xbar_to_m01_couplers_ARADDR),
        .S_AXI_arburst(xbar_to_m01_couplers_ARBURST),
        .S_AXI_arcache(xbar_to_m01_couplers_ARCACHE),
        .S_AXI_arid(xbar_to_m01_couplers_ARID),
        .S_AXI_arlen(xbar_to_m01_couplers_ARLEN),
        .S_AXI_arlock(xbar_to_m01_couplers_ARLOCK),
        .S_AXI_arprot(xbar_to_m01_couplers_ARPROT),
        .S_AXI_arqos(xbar_to_m01_couplers_ARQOS),
        .S_AXI_arready(xbar_to_m01_couplers_ARREADY),
        .S_AXI_arregion(xbar_to_m01_couplers_ARREGION),
        .S_AXI_arsize(xbar_to_m01_couplers_ARSIZE),
        .S_AXI_arvalid(xbar_to_m01_couplers_ARVALID),
        .S_AXI_awaddr(xbar_to_m01_couplers_AWADDR),
        .S_AXI_awburst(xbar_to_m01_couplers_AWBURST),
        .S_AXI_awcache(xbar_to_m01_couplers_AWCACHE),
        .S_AXI_awid(xbar_to_m01_couplers_AWID),
        .S_AXI_awlen(xbar_to_m01_couplers_AWLEN),
        .S_AXI_awlock(xbar_to_m01_couplers_AWLOCK),
        .S_AXI_awprot(xbar_to_m01_couplers_AWPROT),
        .S_AXI_awqos(xbar_to_m01_couplers_AWQOS),
        .S_AXI_awready(xbar_to_m01_couplers_AWREADY),
        .S_AXI_awregion(xbar_to_m01_couplers_AWREGION),
        .S_AXI_awsize(xbar_to_m01_couplers_AWSIZE),
        .S_AXI_awvalid(xbar_to_m01_couplers_AWVALID),
        .S_AXI_bid(xbar_to_m01_couplers_BID),
        .S_AXI_bready(xbar_to_m01_couplers_BREADY),
        .S_AXI_bresp(xbar_to_m01_couplers_BRESP),
        .S_AXI_bvalid(xbar_to_m01_couplers_BVALID),
        .S_AXI_rdata(xbar_to_m01_couplers_RDATA),
        .S_AXI_rid(xbar_to_m01_couplers_RID),
        .S_AXI_rlast(xbar_to_m01_couplers_RLAST),
        .S_AXI_rready(xbar_to_m01_couplers_RREADY),
        .S_AXI_rresp(xbar_to_m01_couplers_RRESP),
        .S_AXI_rvalid(xbar_to_m01_couplers_RVALID),
        .S_AXI_wdata(xbar_to_m01_couplers_WDATA),
        .S_AXI_wlast(xbar_to_m01_couplers_WLAST),
        .S_AXI_wready(xbar_to_m01_couplers_WREADY),
        .S_AXI_wstrb(xbar_to_m01_couplers_WSTRB),
        .S_AXI_wvalid(xbar_to_m01_couplers_WVALID));
  s00_couplers_imp_35C8W0 s00_couplers
       (.M_ACLK(axi_pcis_xbar_ACLK_net),
        .M_ARESETN(axi_pcis_xbar_ARESETN_net),
        .M_AXI_araddr(s00_couplers_to_xbar_ARADDR),
        .M_AXI_arburst(s00_couplers_to_xbar_ARBURST),
        .M_AXI_arcache(s00_couplers_to_xbar_ARCACHE),
        .M_AXI_arid(s00_couplers_to_xbar_ARID),
        .M_AXI_arlen(s00_couplers_to_xbar_ARLEN),
        .M_AXI_arlock(s00_couplers_to_xbar_ARLOCK),
        .M_AXI_arprot(s00_couplers_to_xbar_ARPROT),
        .M_AXI_arqos(s00_couplers_to_xbar_ARQOS),
        .M_AXI_arready(s00_couplers_to_xbar_ARREADY),
        .M_AXI_arsize(s00_couplers_to_xbar_ARSIZE),
        .M_AXI_arvalid(s00_couplers_to_xbar_ARVALID),
        .M_AXI_awaddr(s00_couplers_to_xbar_AWADDR),
        .M_AXI_awburst(s00_couplers_to_xbar_AWBURST),
        .M_AXI_awcache(s00_couplers_to_xbar_AWCACHE),
        .M_AXI_awid(s00_couplers_to_xbar_AWID),
        .M_AXI_awlen(s00_couplers_to_xbar_AWLEN),
        .M_AXI_awlock(s00_couplers_to_xbar_AWLOCK),
        .M_AXI_awprot(s00_couplers_to_xbar_AWPROT),
        .M_AXI_awqos(s00_couplers_to_xbar_AWQOS),
        .M_AXI_awready(s00_couplers_to_xbar_AWREADY),
        .M_AXI_awsize(s00_couplers_to_xbar_AWSIZE),
        .M_AXI_awvalid(s00_couplers_to_xbar_AWVALID),
        .M_AXI_bid(s00_couplers_to_xbar_BID),
        .M_AXI_bready(s00_couplers_to_xbar_BREADY),
        .M_AXI_bresp(s00_couplers_to_xbar_BRESP),
        .M_AXI_bvalid(s00_couplers_to_xbar_BVALID),
        .M_AXI_rdata(s00_couplers_to_xbar_RDATA),
        .M_AXI_rid(s00_couplers_to_xbar_RID),
        .M_AXI_rlast(s00_couplers_to_xbar_RLAST),
        .M_AXI_rready(s00_couplers_to_xbar_RREADY),
        .M_AXI_rresp(s00_couplers_to_xbar_RRESP),
        .M_AXI_rvalid(s00_couplers_to_xbar_RVALID),
        .M_AXI_wdata(s00_couplers_to_xbar_WDATA),
        .M_AXI_wlast(s00_couplers_to_xbar_WLAST),
        .M_AXI_wready(s00_couplers_to_xbar_WREADY),
        .M_AXI_wstrb(s00_couplers_to_xbar_WSTRB),
        .M_AXI_wvalid(s00_couplers_to_xbar_WVALID),
        .S_ACLK(axi_pcis_xbar_ACLK_net),
        .S_ARESETN(axi_pcis_xbar_ARESETN_net),
        .S_AXI_araddr(axi_pcis_xbar_to_s00_couplers_ARADDR),
        .S_AXI_arburst(axi_pcis_xbar_to_s00_couplers_ARBURST),
        .S_AXI_arcache(axi_pcis_xbar_to_s00_couplers_ARCACHE),
        .S_AXI_arid(axi_pcis_xbar_to_s00_couplers_ARID),
        .S_AXI_arlen(axi_pcis_xbar_to_s00_couplers_ARLEN),
        .S_AXI_arlock(axi_pcis_xbar_to_s00_couplers_ARLOCK),
        .S_AXI_arprot(axi_pcis_xbar_to_s00_couplers_ARPROT),
        .S_AXI_arqos(axi_pcis_xbar_to_s00_couplers_ARQOS),
        .S_AXI_arready(axi_pcis_xbar_to_s00_couplers_ARREADY),
        .S_AXI_arregion(axi_pcis_xbar_to_s00_couplers_ARREGION),
        .S_AXI_arsize(axi_pcis_xbar_to_s00_couplers_ARSIZE),
        .S_AXI_arvalid(axi_pcis_xbar_to_s00_couplers_ARVALID),
        .S_AXI_awaddr(axi_pcis_xbar_to_s00_couplers_AWADDR),
        .S_AXI_awburst(axi_pcis_xbar_to_s00_couplers_AWBURST),
        .S_AXI_awcache(axi_pcis_xbar_to_s00_couplers_AWCACHE),
        .S_AXI_awid(axi_pcis_xbar_to_s00_couplers_AWID),
        .S_AXI_awlen(axi_pcis_xbar_to_s00_couplers_AWLEN),
        .S_AXI_awlock(axi_pcis_xbar_to_s00_couplers_AWLOCK),
        .S_AXI_awprot(axi_pcis_xbar_to_s00_couplers_AWPROT),
        .S_AXI_awqos(axi_pcis_xbar_to_s00_couplers_AWQOS),
        .S_AXI_awready(axi_pcis_xbar_to_s00_couplers_AWREADY),
        .S_AXI_awregion(axi_pcis_xbar_to_s00_couplers_AWREGION),
        .S_AXI_awsize(axi_pcis_xbar_to_s00_couplers_AWSIZE),
        .S_AXI_awvalid(axi_pcis_xbar_to_s00_couplers_AWVALID),
        .S_AXI_bid(axi_pcis_xbar_to_s00_couplers_BID),
        .S_AXI_bready(axi_pcis_xbar_to_s00_couplers_BREADY),
        .S_AXI_bresp(axi_pcis_xbar_to_s00_couplers_BRESP),
        .S_AXI_bvalid(axi_pcis_xbar_to_s00_couplers_BVALID),
        .S_AXI_rdata(axi_pcis_xbar_to_s00_couplers_RDATA),
        .S_AXI_rid(axi_pcis_xbar_to_s00_couplers_RID),
        .S_AXI_rlast(axi_pcis_xbar_to_s00_couplers_RLAST),
        .S_AXI_rready(axi_pcis_xbar_to_s00_couplers_RREADY),
        .S_AXI_rresp(axi_pcis_xbar_to_s00_couplers_RRESP),
        .S_AXI_rvalid(axi_pcis_xbar_to_s00_couplers_RVALID),
        .S_AXI_wdata(axi_pcis_xbar_to_s00_couplers_WDATA),
        .S_AXI_wlast(axi_pcis_xbar_to_s00_couplers_WLAST),
        .S_AXI_wready(axi_pcis_xbar_to_s00_couplers_WREADY),
        .S_AXI_wstrb(axi_pcis_xbar_to_s00_couplers_WSTRB),
        .S_AXI_wvalid(axi_pcis_xbar_to_s00_couplers_WVALID));
  cl_xbar_xbar_0 xbar
       (.aclk(axi_pcis_xbar_ACLK_net),
        .aresetn(axi_pcis_xbar_ARESETN_net),
        .m_axi_araddr({xbar_to_m01_couplers_ARADDR,xbar_to_m00_couplers_ARADDR}),
        .m_axi_arburst({xbar_to_m01_couplers_ARBURST,xbar_to_m00_couplers_ARBURST}),
        .m_axi_arcache({xbar_to_m01_couplers_ARCACHE,xbar_to_m00_couplers_ARCACHE}),
        .m_axi_arid({xbar_to_m01_couplers_ARID,xbar_to_m00_couplers_ARID}),
        .m_axi_arlen({xbar_to_m01_couplers_ARLEN,xbar_to_m00_couplers_ARLEN}),
        .m_axi_arlock({xbar_to_m01_couplers_ARLOCK,xbar_to_m00_couplers_ARLOCK}),
        .m_axi_arprot({xbar_to_m01_couplers_ARPROT,xbar_to_m00_couplers_ARPROT}),
        .m_axi_arqos({xbar_to_m01_couplers_ARQOS,xbar_to_m00_couplers_ARQOS}),
        .m_axi_arready({xbar_to_m01_couplers_ARREADY,xbar_to_m00_couplers_ARREADY}),
        .m_axi_arregion({xbar_to_m01_couplers_ARREGION,xbar_to_m00_couplers_ARREGION}),
        .m_axi_arsize({xbar_to_m01_couplers_ARSIZE,xbar_to_m00_couplers_ARSIZE}),
        .m_axi_arvalid({xbar_to_m01_couplers_ARVALID,xbar_to_m00_couplers_ARVALID}),
        .m_axi_awaddr({xbar_to_m01_couplers_AWADDR,xbar_to_m00_couplers_AWADDR}),
        .m_axi_awburst({xbar_to_m01_couplers_AWBURST,xbar_to_m00_couplers_AWBURST}),
        .m_axi_awcache({xbar_to_m01_couplers_AWCACHE,xbar_to_m00_couplers_AWCACHE}),
        .m_axi_awid({xbar_to_m01_couplers_AWID,xbar_to_m00_couplers_AWID}),
        .m_axi_awlen({xbar_to_m01_couplers_AWLEN,xbar_to_m00_couplers_AWLEN}),
        .m_axi_awlock({xbar_to_m01_couplers_AWLOCK,xbar_to_m00_couplers_AWLOCK}),
        .m_axi_awprot({xbar_to_m01_couplers_AWPROT,xbar_to_m00_couplers_AWPROT}),
        .m_axi_awqos({xbar_to_m01_couplers_AWQOS,xbar_to_m00_couplers_AWQOS}),
        .m_axi_awready({xbar_to_m01_couplers_AWREADY,xbar_to_m00_couplers_AWREADY}),
        .m_axi_awregion({xbar_to_m01_couplers_AWREGION,xbar_to_m00_couplers_AWREGION}),
        .m_axi_awsize({xbar_to_m01_couplers_AWSIZE,xbar_to_m00_couplers_AWSIZE}),
        .m_axi_awvalid({xbar_to_m01_couplers_AWVALID,xbar_to_m00_couplers_AWVALID}),
        .m_axi_bid({xbar_to_m01_couplers_BID,xbar_to_m00_couplers_BID}),
        .m_axi_bready({xbar_to_m01_couplers_BREADY,xbar_to_m00_couplers_BREADY}),
        .m_axi_bresp({xbar_to_m01_couplers_BRESP,xbar_to_m00_couplers_BRESP}),
        .m_axi_bvalid({xbar_to_m01_couplers_BVALID,xbar_to_m00_couplers_BVALID}),
        .m_axi_rdata({xbar_to_m01_couplers_RDATA,xbar_to_m00_couplers_RDATA}),
        .m_axi_rid({xbar_to_m01_couplers_RID,xbar_to_m00_couplers_RID}),
        .m_axi_rlast({xbar_to_m01_couplers_RLAST,xbar_to_m00_couplers_RLAST}),
        .m_axi_rready({xbar_to_m01_couplers_RREADY,xbar_to_m00_couplers_RREADY}),
        .m_axi_rresp({xbar_to_m01_couplers_RRESP,xbar_to_m00_couplers_RRESP}),
        .m_axi_rvalid({xbar_to_m01_couplers_RVALID,xbar_to_m00_couplers_RVALID}),
        .m_axi_wdata({xbar_to_m01_couplers_WDATA,xbar_to_m00_couplers_WDATA}),
        .m_axi_wlast({xbar_to_m01_couplers_WLAST,xbar_to_m00_couplers_WLAST}),
        .m_axi_wready({xbar_to_m01_couplers_WREADY,xbar_to_m00_couplers_WREADY}),
        .m_axi_wstrb({xbar_to_m01_couplers_WSTRB,xbar_to_m00_couplers_WSTRB}),
        .m_axi_wvalid({xbar_to_m01_couplers_WVALID,xbar_to_m00_couplers_WVALID}),
        .s_axi_araddr(s00_couplers_to_xbar_ARADDR),
        .s_axi_arburst(s00_couplers_to_xbar_ARBURST),
        .s_axi_arcache(s00_couplers_to_xbar_ARCACHE),
        .s_axi_arid(s00_couplers_to_xbar_ARID),
        .s_axi_arlen(s00_couplers_to_xbar_ARLEN),
        .s_axi_arlock(s00_couplers_to_xbar_ARLOCK),
        .s_axi_arprot(s00_couplers_to_xbar_ARPROT),
        .s_axi_arqos(s00_couplers_to_xbar_ARQOS),
        .s_axi_arready(s00_couplers_to_xbar_ARREADY),
        .s_axi_arsize(s00_couplers_to_xbar_ARSIZE),
        .s_axi_arvalid(s00_couplers_to_xbar_ARVALID),
        .s_axi_awaddr(s00_couplers_to_xbar_AWADDR),
        .s_axi_awburst(s00_couplers_to_xbar_AWBURST),
        .s_axi_awcache(s00_couplers_to_xbar_AWCACHE),
        .s_axi_awid(s00_couplers_to_xbar_AWID),
        .s_axi_awlen(s00_couplers_to_xbar_AWLEN),
        .s_axi_awlock(s00_couplers_to_xbar_AWLOCK),
        .s_axi_awprot(s00_couplers_to_xbar_AWPROT),
        .s_axi_awqos(s00_couplers_to_xbar_AWQOS),
        .s_axi_awready(s00_couplers_to_xbar_AWREADY),
        .s_axi_awsize(s00_couplers_to_xbar_AWSIZE),
        .s_axi_awvalid(s00_couplers_to_xbar_AWVALID),
        .s_axi_bid(s00_couplers_to_xbar_BID),
        .s_axi_bready(s00_couplers_to_xbar_BREADY),
        .s_axi_bresp(s00_couplers_to_xbar_BRESP),
        .s_axi_bvalid(s00_couplers_to_xbar_BVALID),
        .s_axi_rdata(s00_couplers_to_xbar_RDATA),
        .s_axi_rid(s00_couplers_to_xbar_RID),
        .s_axi_rlast(s00_couplers_to_xbar_RLAST),
        .s_axi_rready(s00_couplers_to_xbar_RREADY),
        .s_axi_rresp(s00_couplers_to_xbar_RRESP),
        .s_axi_rvalid(s00_couplers_to_xbar_RVALID),
        .s_axi_wdata(s00_couplers_to_xbar_WDATA),
        .s_axi_wlast(s00_couplers_to_xbar_WLAST),
        .s_axi_wready(s00_couplers_to_xbar_WREADY),
        .s_axi_wstrb(s00_couplers_to_xbar_WSTRB),
        .s_axi_wvalid(s00_couplers_to_xbar_WVALID));
endmodule

module m00_couplers_imp_12YH38C
   (M_ACLK,
    M_ARESETN,
    M_AXI_araddr,
    M_AXI_arburst,
    M_AXI_arcache,
    M_AXI_arid,
    M_AXI_arlen,
    M_AXI_arlock,
    M_AXI_arprot,
    M_AXI_arqos,
    M_AXI_arready,
    M_AXI_arregion,
    M_AXI_arsize,
    M_AXI_arvalid,
    M_AXI_awaddr,
    M_AXI_awburst,
    M_AXI_awcache,
    M_AXI_awid,
    M_AXI_awlen,
    M_AXI_awlock,
    M_AXI_awprot,
    M_AXI_awqos,
    M_AXI_awready,
    M_AXI_awregion,
    M_AXI_awsize,
    M_AXI_awvalid,
    M_AXI_bid,
    M_AXI_bready,
    M_AXI_bresp,
    M_AXI_bvalid,
    M_AXI_rdata,
    M_AXI_rid,
    M_AXI_rlast,
    M_AXI_rready,
    M_AXI_rresp,
    M_AXI_rvalid,
    M_AXI_wdata,
    M_AXI_wlast,
    M_AXI_wready,
    M_AXI_wstrb,
    M_AXI_wvalid,
    S_ACLK,
    S_ARESETN,
    S_AXI_araddr,
    S_AXI_arburst,
    S_AXI_arcache,
    S_AXI_arid,
    S_AXI_arlen,
    S_AXI_arlock,
    S_AXI_arprot,
    S_AXI_arqos,
    S_AXI_arready,
    S_AXI_arregion,
    S_AXI_arsize,
    S_AXI_arvalid,
    S_AXI_awaddr,
    S_AXI_awburst,
    S_AXI_awcache,
    S_AXI_awid,
    S_AXI_awlen,
    S_AXI_awlock,
    S_AXI_awprot,
    S_AXI_awqos,
    S_AXI_awready,
    S_AXI_awregion,
    S_AXI_awsize,
    S_AXI_awvalid,
    S_AXI_bid,
    S_AXI_bready,
    S_AXI_bresp,
    S_AXI_bvalid,
    S_AXI_rdata,
    S_AXI_rid,
    S_AXI_rlast,
    S_AXI_rready,
    S_AXI_rresp,
    S_AXI_rvalid,
    S_AXI_wdata,
    S_AXI_wlast,
    S_AXI_wready,
    S_AXI_wstrb,
    S_AXI_wvalid);
  input M_ACLK;
  input M_ARESETN;
  output [63:0]M_AXI_araddr;
  output [1:0]M_AXI_arburst;
  output [3:0]M_AXI_arcache;
  output [5:0]M_AXI_arid;
  output [7:0]M_AXI_arlen;
  output [0:0]M_AXI_arlock;
  output [2:0]M_AXI_arprot;
  output [3:0]M_AXI_arqos;
  input M_AXI_arready;
  output [3:0]M_AXI_arregion;
  output [2:0]M_AXI_arsize;
  output M_AXI_arvalid;
  output [63:0]M_AXI_awaddr;
  output [1:0]M_AXI_awburst;
  output [3:0]M_AXI_awcache;
  output [5:0]M_AXI_awid;
  output [7:0]M_AXI_awlen;
  output [0:0]M_AXI_awlock;
  output [2:0]M_AXI_awprot;
  output [3:0]M_AXI_awqos;
  input M_AXI_awready;
  output [3:0]M_AXI_awregion;
  output [2:0]M_AXI_awsize;
  output M_AXI_awvalid;
  input [5:0]M_AXI_bid;
  output M_AXI_bready;
  input [1:0]M_AXI_bresp;
  input M_AXI_bvalid;
  input [511:0]M_AXI_rdata;
  input [5:0]M_AXI_rid;
  input M_AXI_rlast;
  output M_AXI_rready;
  input [1:0]M_AXI_rresp;
  input M_AXI_rvalid;
  output [511:0]M_AXI_wdata;
  output M_AXI_wlast;
  input M_AXI_wready;
  output [63:0]M_AXI_wstrb;
  output M_AXI_wvalid;
  input S_ACLK;
  input S_ARESETN;
  input [63:0]S_AXI_araddr;
  input [1:0]S_AXI_arburst;
  input [3:0]S_AXI_arcache;
  input [5:0]S_AXI_arid;
  input [7:0]S_AXI_arlen;
  input [0:0]S_AXI_arlock;
  input [2:0]S_AXI_arprot;
  input [3:0]S_AXI_arqos;
  output S_AXI_arready;
  input [3:0]S_AXI_arregion;
  input [2:0]S_AXI_arsize;
  input S_AXI_arvalid;
  input [63:0]S_AXI_awaddr;
  input [1:0]S_AXI_awburst;
  input [3:0]S_AXI_awcache;
  input [5:0]S_AXI_awid;
  input [7:0]S_AXI_awlen;
  input [0:0]S_AXI_awlock;
  input [2:0]S_AXI_awprot;
  input [3:0]S_AXI_awqos;
  output S_AXI_awready;
  input [3:0]S_AXI_awregion;
  input [2:0]S_AXI_awsize;
  input S_AXI_awvalid;
  output [5:0]S_AXI_bid;
  input S_AXI_bready;
  output [1:0]S_AXI_bresp;
  output S_AXI_bvalid;
  output [511:0]S_AXI_rdata;
  output [5:0]S_AXI_rid;
  output S_AXI_rlast;
  input S_AXI_rready;
  output [1:0]S_AXI_rresp;
  output S_AXI_rvalid;
  input [511:0]S_AXI_wdata;
  input S_AXI_wlast;
  output S_AXI_wready;
  input [63:0]S_AXI_wstrb;
  input S_AXI_wvalid;

  wire M_ACLK_1;
  wire M_ARESETN_1;
  wire [63:0]m00_couplers_to_m00_regslice_ARADDR;
  wire [1:0]m00_couplers_to_m00_regslice_ARBURST;
  wire [3:0]m00_couplers_to_m00_regslice_ARCACHE;
  wire [5:0]m00_couplers_to_m00_regslice_ARID;
  wire [7:0]m00_couplers_to_m00_regslice_ARLEN;
  wire [0:0]m00_couplers_to_m00_regslice_ARLOCK;
  wire [2:0]m00_couplers_to_m00_regslice_ARPROT;
  wire [3:0]m00_couplers_to_m00_regslice_ARQOS;
  wire m00_couplers_to_m00_regslice_ARREADY;
  wire [3:0]m00_couplers_to_m00_regslice_ARREGION;
  wire [2:0]m00_couplers_to_m00_regslice_ARSIZE;
  wire m00_couplers_to_m00_regslice_ARVALID;
  wire [63:0]m00_couplers_to_m00_regslice_AWADDR;
  wire [1:0]m00_couplers_to_m00_regslice_AWBURST;
  wire [3:0]m00_couplers_to_m00_regslice_AWCACHE;
  wire [5:0]m00_couplers_to_m00_regslice_AWID;
  wire [7:0]m00_couplers_to_m00_regslice_AWLEN;
  wire [0:0]m00_couplers_to_m00_regslice_AWLOCK;
  wire [2:0]m00_couplers_to_m00_regslice_AWPROT;
  wire [3:0]m00_couplers_to_m00_regslice_AWQOS;
  wire m00_couplers_to_m00_regslice_AWREADY;
  wire [3:0]m00_couplers_to_m00_regslice_AWREGION;
  wire [2:0]m00_couplers_to_m00_regslice_AWSIZE;
  wire m00_couplers_to_m00_regslice_AWVALID;
  wire [5:0]m00_couplers_to_m00_regslice_BID;
  wire m00_couplers_to_m00_regslice_BREADY;
  wire [1:0]m00_couplers_to_m00_regslice_BRESP;
  wire m00_couplers_to_m00_regslice_BVALID;
  wire [511:0]m00_couplers_to_m00_regslice_RDATA;
  wire [5:0]m00_couplers_to_m00_regslice_RID;
  wire m00_couplers_to_m00_regslice_RLAST;
  wire m00_couplers_to_m00_regslice_RREADY;
  wire [1:0]m00_couplers_to_m00_regslice_RRESP;
  wire m00_couplers_to_m00_regslice_RVALID;
  wire [511:0]m00_couplers_to_m00_regslice_WDATA;
  wire m00_couplers_to_m00_regslice_WLAST;
  wire m00_couplers_to_m00_regslice_WREADY;
  wire [63:0]m00_couplers_to_m00_regslice_WSTRB;
  wire m00_couplers_to_m00_regslice_WVALID;
  wire [63:0]m00_regslice_to_m00_couplers_ARADDR;
  wire [1:0]m00_regslice_to_m00_couplers_ARBURST;
  wire [3:0]m00_regslice_to_m00_couplers_ARCACHE;
  wire [5:0]m00_regslice_to_m00_couplers_ARID;
  wire [7:0]m00_regslice_to_m00_couplers_ARLEN;
  wire [0:0]m00_regslice_to_m00_couplers_ARLOCK;
  wire [2:0]m00_regslice_to_m00_couplers_ARPROT;
  wire [3:0]m00_regslice_to_m00_couplers_ARQOS;
  wire m00_regslice_to_m00_couplers_ARREADY;
  wire [3:0]m00_regslice_to_m00_couplers_ARREGION;
  wire [2:0]m00_regslice_to_m00_couplers_ARSIZE;
  wire m00_regslice_to_m00_couplers_ARVALID;
  wire [63:0]m00_regslice_to_m00_couplers_AWADDR;
  wire [1:0]m00_regslice_to_m00_couplers_AWBURST;
  wire [3:0]m00_regslice_to_m00_couplers_AWCACHE;
  wire [5:0]m00_regslice_to_m00_couplers_AWID;
  wire [7:0]m00_regslice_to_m00_couplers_AWLEN;
  wire [0:0]m00_regslice_to_m00_couplers_AWLOCK;
  wire [2:0]m00_regslice_to_m00_couplers_AWPROT;
  wire [3:0]m00_regslice_to_m00_couplers_AWQOS;
  wire m00_regslice_to_m00_couplers_AWREADY;
  wire [3:0]m00_regslice_to_m00_couplers_AWREGION;
  wire [2:0]m00_regslice_to_m00_couplers_AWSIZE;
  wire m00_regslice_to_m00_couplers_AWVALID;
  wire [5:0]m00_regslice_to_m00_couplers_BID;
  wire m00_regslice_to_m00_couplers_BREADY;
  wire [1:0]m00_regslice_to_m00_couplers_BRESP;
  wire m00_regslice_to_m00_couplers_BVALID;
  wire [511:0]m00_regslice_to_m00_couplers_RDATA;
  wire [5:0]m00_regslice_to_m00_couplers_RID;
  wire m00_regslice_to_m00_couplers_RLAST;
  wire m00_regslice_to_m00_couplers_RREADY;
  wire [1:0]m00_regslice_to_m00_couplers_RRESP;
  wire m00_regslice_to_m00_couplers_RVALID;
  wire [511:0]m00_regslice_to_m00_couplers_WDATA;
  wire m00_regslice_to_m00_couplers_WLAST;
  wire m00_regslice_to_m00_couplers_WREADY;
  wire [63:0]m00_regslice_to_m00_couplers_WSTRB;
  wire m00_regslice_to_m00_couplers_WVALID;

  assign M_ACLK_1 = M_ACLK;
  assign M_ARESETN_1 = M_ARESETN;
  assign M_AXI_araddr[63:0] = m00_regslice_to_m00_couplers_ARADDR;
  assign M_AXI_arburst[1:0] = m00_regslice_to_m00_couplers_ARBURST;
  assign M_AXI_arcache[3:0] = m00_regslice_to_m00_couplers_ARCACHE;
  assign M_AXI_arid[5:0] = m00_regslice_to_m00_couplers_ARID;
  assign M_AXI_arlen[7:0] = m00_regslice_to_m00_couplers_ARLEN;
  assign M_AXI_arlock[0] = m00_regslice_to_m00_couplers_ARLOCK;
  assign M_AXI_arprot[2:0] = m00_regslice_to_m00_couplers_ARPROT;
  assign M_AXI_arqos[3:0] = m00_regslice_to_m00_couplers_ARQOS;
  assign M_AXI_arregion[3:0] = m00_regslice_to_m00_couplers_ARREGION;
  assign M_AXI_arsize[2:0] = m00_regslice_to_m00_couplers_ARSIZE;
  assign M_AXI_arvalid = m00_regslice_to_m00_couplers_ARVALID;
  assign M_AXI_awaddr[63:0] = m00_regslice_to_m00_couplers_AWADDR;
  assign M_AXI_awburst[1:0] = m00_regslice_to_m00_couplers_AWBURST;
  assign M_AXI_awcache[3:0] = m00_regslice_to_m00_couplers_AWCACHE;
  assign M_AXI_awid[5:0] = m00_regslice_to_m00_couplers_AWID;
  assign M_AXI_awlen[7:0] = m00_regslice_to_m00_couplers_AWLEN;
  assign M_AXI_awlock[0] = m00_regslice_to_m00_couplers_AWLOCK;
  assign M_AXI_awprot[2:0] = m00_regslice_to_m00_couplers_AWPROT;
  assign M_AXI_awqos[3:0] = m00_regslice_to_m00_couplers_AWQOS;
  assign M_AXI_awregion[3:0] = m00_regslice_to_m00_couplers_AWREGION;
  assign M_AXI_awsize[2:0] = m00_regslice_to_m00_couplers_AWSIZE;
  assign M_AXI_awvalid = m00_regslice_to_m00_couplers_AWVALID;
  assign M_AXI_bready = m00_regslice_to_m00_couplers_BREADY;
  assign M_AXI_rready = m00_regslice_to_m00_couplers_RREADY;
  assign M_AXI_wdata[511:0] = m00_regslice_to_m00_couplers_WDATA;
  assign M_AXI_wlast = m00_regslice_to_m00_couplers_WLAST;
  assign M_AXI_wstrb[63:0] = m00_regslice_to_m00_couplers_WSTRB;
  assign M_AXI_wvalid = m00_regslice_to_m00_couplers_WVALID;
  assign S_AXI_arready = m00_couplers_to_m00_regslice_ARREADY;
  assign S_AXI_awready = m00_couplers_to_m00_regslice_AWREADY;
  assign S_AXI_bid[5:0] = m00_couplers_to_m00_regslice_BID;
  assign S_AXI_bresp[1:0] = m00_couplers_to_m00_regslice_BRESP;
  assign S_AXI_bvalid = m00_couplers_to_m00_regslice_BVALID;
  assign S_AXI_rdata[511:0] = m00_couplers_to_m00_regslice_RDATA;
  assign S_AXI_rid[5:0] = m00_couplers_to_m00_regslice_RID;
  assign S_AXI_rlast = m00_couplers_to_m00_regslice_RLAST;
  assign S_AXI_rresp[1:0] = m00_couplers_to_m00_regslice_RRESP;
  assign S_AXI_rvalid = m00_couplers_to_m00_regslice_RVALID;
  assign S_AXI_wready = m00_couplers_to_m00_regslice_WREADY;
  assign m00_couplers_to_m00_regslice_ARADDR = S_AXI_araddr[63:0];
  assign m00_couplers_to_m00_regslice_ARBURST = S_AXI_arburst[1:0];
  assign m00_couplers_to_m00_regslice_ARCACHE = S_AXI_arcache[3:0];
  assign m00_couplers_to_m00_regslice_ARID = S_AXI_arid[5:0];
  assign m00_couplers_to_m00_regslice_ARLEN = S_AXI_arlen[7:0];
  assign m00_couplers_to_m00_regslice_ARLOCK = S_AXI_arlock[0];
  assign m00_couplers_to_m00_regslice_ARPROT = S_AXI_arprot[2:0];
  assign m00_couplers_to_m00_regslice_ARQOS = S_AXI_arqos[3:0];
  assign m00_couplers_to_m00_regslice_ARREGION = S_AXI_arregion[3:0];
  assign m00_couplers_to_m00_regslice_ARSIZE = S_AXI_arsize[2:0];
  assign m00_couplers_to_m00_regslice_ARVALID = S_AXI_arvalid;
  assign m00_couplers_to_m00_regslice_AWADDR = S_AXI_awaddr[63:0];
  assign m00_couplers_to_m00_regslice_AWBURST = S_AXI_awburst[1:0];
  assign m00_couplers_to_m00_regslice_AWCACHE = S_AXI_awcache[3:0];
  assign m00_couplers_to_m00_regslice_AWID = S_AXI_awid[5:0];
  assign m00_couplers_to_m00_regslice_AWLEN = S_AXI_awlen[7:0];
  assign m00_couplers_to_m00_regslice_AWLOCK = S_AXI_awlock[0];
  assign m00_couplers_to_m00_regslice_AWPROT = S_AXI_awprot[2:0];
  assign m00_couplers_to_m00_regslice_AWQOS = S_AXI_awqos[3:0];
  assign m00_couplers_to_m00_regslice_AWREGION = S_AXI_awregion[3:0];
  assign m00_couplers_to_m00_regslice_AWSIZE = S_AXI_awsize[2:0];
  assign m00_couplers_to_m00_regslice_AWVALID = S_AXI_awvalid;
  assign m00_couplers_to_m00_regslice_BREADY = S_AXI_bready;
  assign m00_couplers_to_m00_regslice_RREADY = S_AXI_rready;
  assign m00_couplers_to_m00_regslice_WDATA = S_AXI_wdata[511:0];
  assign m00_couplers_to_m00_regslice_WLAST = S_AXI_wlast;
  assign m00_couplers_to_m00_regslice_WSTRB = S_AXI_wstrb[63:0];
  assign m00_couplers_to_m00_regslice_WVALID = S_AXI_wvalid;
  assign m00_regslice_to_m00_couplers_ARREADY = M_AXI_arready;
  assign m00_regslice_to_m00_couplers_AWREADY = M_AXI_awready;
  assign m00_regslice_to_m00_couplers_BID = M_AXI_bid[5:0];
  assign m00_regslice_to_m00_couplers_BRESP = M_AXI_bresp[1:0];
  assign m00_regslice_to_m00_couplers_BVALID = M_AXI_bvalid;
  assign m00_regslice_to_m00_couplers_RDATA = M_AXI_rdata[511:0];
  assign m00_regslice_to_m00_couplers_RID = M_AXI_rid[5:0];
  assign m00_regslice_to_m00_couplers_RLAST = M_AXI_rlast;
  assign m00_regslice_to_m00_couplers_RRESP = M_AXI_rresp[1:0];
  assign m00_regslice_to_m00_couplers_RVALID = M_AXI_rvalid;
  assign m00_regslice_to_m00_couplers_WREADY = M_AXI_wready;
  cl_xbar_m00_regslice_2 m00_regslice
       (.aclk(M_ACLK_1),
        .aresetn(M_ARESETN_1),
        .m_axi_araddr(m00_regslice_to_m00_couplers_ARADDR),
        .m_axi_arburst(m00_regslice_to_m00_couplers_ARBURST),
        .m_axi_arcache(m00_regslice_to_m00_couplers_ARCACHE),
        .m_axi_arid(m00_regslice_to_m00_couplers_ARID),
        .m_axi_arlen(m00_regslice_to_m00_couplers_ARLEN),
        .m_axi_arlock(m00_regslice_to_m00_couplers_ARLOCK),
        .m_axi_arprot(m00_regslice_to_m00_couplers_ARPROT),
        .m_axi_arqos(m00_regslice_to_m00_couplers_ARQOS),
        .m_axi_arready(m00_regslice_to_m00_couplers_ARREADY),
        .m_axi_arregion(m00_regslice_to_m00_couplers_ARREGION),
        .m_axi_arsize(m00_regslice_to_m00_couplers_ARSIZE),
        .m_axi_arvalid(m00_regslice_to_m00_couplers_ARVALID),
        .m_axi_awaddr(m00_regslice_to_m00_couplers_AWADDR),
        .m_axi_awburst(m00_regslice_to_m00_couplers_AWBURST),
        .m_axi_awcache(m00_regslice_to_m00_couplers_AWCACHE),
        .m_axi_awid(m00_regslice_to_m00_couplers_AWID),
        .m_axi_awlen(m00_regslice_to_m00_couplers_AWLEN),
        .m_axi_awlock(m00_regslice_to_m00_couplers_AWLOCK),
        .m_axi_awprot(m00_regslice_to_m00_couplers_AWPROT),
        .m_axi_awqos(m00_regslice_to_m00_couplers_AWQOS),
        .m_axi_awready(m00_regslice_to_m00_couplers_AWREADY),
        .m_axi_awregion(m00_regslice_to_m00_couplers_AWREGION),
        .m_axi_awsize(m00_regslice_to_m00_couplers_AWSIZE),
        .m_axi_awvalid(m00_regslice_to_m00_couplers_AWVALID),
        .m_axi_bid(m00_regslice_to_m00_couplers_BID),
        .m_axi_bready(m00_regslice_to_m00_couplers_BREADY),
        .m_axi_bresp(m00_regslice_to_m00_couplers_BRESP),
        .m_axi_bvalid(m00_regslice_to_m00_couplers_BVALID),
        .m_axi_rdata(m00_regslice_to_m00_couplers_RDATA),
        .m_axi_rid(m00_regslice_to_m00_couplers_RID),
        .m_axi_rlast(m00_regslice_to_m00_couplers_RLAST),
        .m_axi_rready(m00_regslice_to_m00_couplers_RREADY),
        .m_axi_rresp(m00_regslice_to_m00_couplers_RRESP),
        .m_axi_rvalid(m00_regslice_to_m00_couplers_RVALID),
        .m_axi_wdata(m00_regslice_to_m00_couplers_WDATA),
        .m_axi_wlast(m00_regslice_to_m00_couplers_WLAST),
        .m_axi_wready(m00_regslice_to_m00_couplers_WREADY),
        .m_axi_wstrb(m00_regslice_to_m00_couplers_WSTRB),
        .m_axi_wvalid(m00_regslice_to_m00_couplers_WVALID),
        .s_axi_araddr(m00_couplers_to_m00_regslice_ARADDR),
        .s_axi_arburst(m00_couplers_to_m00_regslice_ARBURST),
        .s_axi_arcache(m00_couplers_to_m00_regslice_ARCACHE),
        .s_axi_arid(m00_couplers_to_m00_regslice_ARID),
        .s_axi_arlen(m00_couplers_to_m00_regslice_ARLEN),
        .s_axi_arlock(m00_couplers_to_m00_regslice_ARLOCK),
        .s_axi_arprot(m00_couplers_to_m00_regslice_ARPROT),
        .s_axi_arqos(m00_couplers_to_m00_regslice_ARQOS),
        .s_axi_arready(m00_couplers_to_m00_regslice_ARREADY),
        .s_axi_arregion(m00_couplers_to_m00_regslice_ARREGION),
        .s_axi_arsize(m00_couplers_to_m00_regslice_ARSIZE),
        .s_axi_arvalid(m00_couplers_to_m00_regslice_ARVALID),
        .s_axi_awaddr(m00_couplers_to_m00_regslice_AWADDR),
        .s_axi_awburst(m00_couplers_to_m00_regslice_AWBURST),
        .s_axi_awcache(m00_couplers_to_m00_regslice_AWCACHE),
        .s_axi_awid(m00_couplers_to_m00_regslice_AWID),
        .s_axi_awlen(m00_couplers_to_m00_regslice_AWLEN),
        .s_axi_awlock(m00_couplers_to_m00_regslice_AWLOCK),
        .s_axi_awprot(m00_couplers_to_m00_regslice_AWPROT),
        .s_axi_awqos(m00_couplers_to_m00_regslice_AWQOS),
        .s_axi_awready(m00_couplers_to_m00_regslice_AWREADY),
        .s_axi_awregion(m00_couplers_to_m00_regslice_AWREGION),
        .s_axi_awsize(m00_couplers_to_m00_regslice_AWSIZE),
        .s_axi_awvalid(m00_couplers_to_m00_regslice_AWVALID),
        .s_axi_bid(m00_couplers_to_m00_regslice_BID),
        .s_axi_bready(m00_couplers_to_m00_regslice_BREADY),
        .s_axi_bresp(m00_couplers_to_m00_regslice_BRESP),
        .s_axi_bvalid(m00_couplers_to_m00_regslice_BVALID),
        .s_axi_rdata(m00_couplers_to_m00_regslice_RDATA),
        .s_axi_rid(m00_couplers_to_m00_regslice_RID),
        .s_axi_rlast(m00_couplers_to_m00_regslice_RLAST),
        .s_axi_rready(m00_couplers_to_m00_regslice_RREADY),
        .s_axi_rresp(m00_couplers_to_m00_regslice_RRESP),
        .s_axi_rvalid(m00_couplers_to_m00_regslice_RVALID),
        .s_axi_wdata(m00_couplers_to_m00_regslice_WDATA),
        .s_axi_wlast(m00_couplers_to_m00_regslice_WLAST),
        .s_axi_wready(m00_couplers_to_m00_regslice_WREADY),
        .s_axi_wstrb(m00_couplers_to_m00_regslice_WSTRB),
        .s_axi_wvalid(m00_couplers_to_m00_regslice_WVALID));
endmodule

module m00_couplers_imp_J5PC3E
   (M_ACLK,
    M_ARESETN,
    M_AXI_araddr,
    M_AXI_arburst,
    M_AXI_arcache,
    M_AXI_arid,
    M_AXI_arlen,
    M_AXI_arlock,
    M_AXI_arprot,
    M_AXI_arqos,
    M_AXI_arready,
    M_AXI_arregion,
    M_AXI_arsize,
    M_AXI_arvalid,
    M_AXI_awaddr,
    M_AXI_awburst,
    M_AXI_awcache,
    M_AXI_awid,
    M_AXI_awlen,
    M_AXI_awlock,
    M_AXI_awprot,
    M_AXI_awqos,
    M_AXI_awready,
    M_AXI_awregion,
    M_AXI_awsize,
    M_AXI_awvalid,
    M_AXI_bid,
    M_AXI_bready,
    M_AXI_bresp,
    M_AXI_bvalid,
    M_AXI_rdata,
    M_AXI_rid,
    M_AXI_rlast,
    M_AXI_rready,
    M_AXI_rresp,
    M_AXI_rvalid,
    M_AXI_wdata,
    M_AXI_wlast,
    M_AXI_wready,
    M_AXI_wstrb,
    M_AXI_wvalid,
    S_ACLK,
    S_ARESETN,
    S_AXI_araddr,
    S_AXI_arburst,
    S_AXI_arcache,
    S_AXI_arid,
    S_AXI_arlen,
    S_AXI_arlock,
    S_AXI_arprot,
    S_AXI_arqos,
    S_AXI_arready,
    S_AXI_arregion,
    S_AXI_arsize,
    S_AXI_arvalid,
    S_AXI_awaddr,
    S_AXI_awburst,
    S_AXI_awcache,
    S_AXI_awid,
    S_AXI_awlen,
    S_AXI_awlock,
    S_AXI_awprot,
    S_AXI_awqos,
    S_AXI_awready,
    S_AXI_awregion,
    S_AXI_awsize,
    S_AXI_awvalid,
    S_AXI_bid,
    S_AXI_bready,
    S_AXI_bresp,
    S_AXI_bvalid,
    S_AXI_rdata,
    S_AXI_rid,
    S_AXI_rlast,
    S_AXI_rready,
    S_AXI_rresp,
    S_AXI_rvalid,
    S_AXI_wdata,
    S_AXI_wlast,
    S_AXI_wready,
    S_AXI_wstrb,
    S_AXI_wvalid);
  input M_ACLK;
  input M_ARESETN;
  output [63:0]M_AXI_araddr;
  output [1:0]M_AXI_arburst;
  output [3:0]M_AXI_arcache;
  output [6:0]M_AXI_arid;
  output [7:0]M_AXI_arlen;
  output [0:0]M_AXI_arlock;
  output [2:0]M_AXI_arprot;
  output [3:0]M_AXI_arqos;
  input M_AXI_arready;
  output [3:0]M_AXI_arregion;
  output [2:0]M_AXI_arsize;
  output M_AXI_arvalid;
  output [63:0]M_AXI_awaddr;
  output [1:0]M_AXI_awburst;
  output [3:0]M_AXI_awcache;
  output [6:0]M_AXI_awid;
  output [7:0]M_AXI_awlen;
  output [0:0]M_AXI_awlock;
  output [2:0]M_AXI_awprot;
  output [3:0]M_AXI_awqos;
  input M_AXI_awready;
  output [3:0]M_AXI_awregion;
  output [2:0]M_AXI_awsize;
  output M_AXI_awvalid;
  input [6:0]M_AXI_bid;
  output M_AXI_bready;
  input [1:0]M_AXI_bresp;
  input M_AXI_bvalid;
  input [511:0]M_AXI_rdata;
  input [6:0]M_AXI_rid;
  input M_AXI_rlast;
  output M_AXI_rready;
  input [1:0]M_AXI_rresp;
  input M_AXI_rvalid;
  output [511:0]M_AXI_wdata;
  output M_AXI_wlast;
  input M_AXI_wready;
  output [63:0]M_AXI_wstrb;
  output M_AXI_wvalid;
  input S_ACLK;
  input S_ARESETN;
  input [63:0]S_AXI_araddr;
  input [1:0]S_AXI_arburst;
  input [3:0]S_AXI_arcache;
  input [6:0]S_AXI_arid;
  input [7:0]S_AXI_arlen;
  input [0:0]S_AXI_arlock;
  input [2:0]S_AXI_arprot;
  input [3:0]S_AXI_arqos;
  output S_AXI_arready;
  input [3:0]S_AXI_arregion;
  input [2:0]S_AXI_arsize;
  input S_AXI_arvalid;
  input [63:0]S_AXI_awaddr;
  input [1:0]S_AXI_awburst;
  input [3:0]S_AXI_awcache;
  input [6:0]S_AXI_awid;
  input [7:0]S_AXI_awlen;
  input [0:0]S_AXI_awlock;
  input [2:0]S_AXI_awprot;
  input [3:0]S_AXI_awqos;
  output S_AXI_awready;
  input [3:0]S_AXI_awregion;
  input [2:0]S_AXI_awsize;
  input S_AXI_awvalid;
  output [6:0]S_AXI_bid;
  input S_AXI_bready;
  output [1:0]S_AXI_bresp;
  output S_AXI_bvalid;
  output [511:0]S_AXI_rdata;
  output [6:0]S_AXI_rid;
  output S_AXI_rlast;
  input S_AXI_rready;
  output [1:0]S_AXI_rresp;
  output S_AXI_rvalid;
  input [511:0]S_AXI_wdata;
  input S_AXI_wlast;
  output S_AXI_wready;
  input [63:0]S_AXI_wstrb;
  input S_AXI_wvalid;

  wire M_ACLK_1;
  wire M_ARESETN_1;
  wire [63:0]m00_couplers_to_m00_regslice_ARADDR;
  wire [1:0]m00_couplers_to_m00_regslice_ARBURST;
  wire [3:0]m00_couplers_to_m00_regslice_ARCACHE;
  wire [6:0]m00_couplers_to_m00_regslice_ARID;
  wire [7:0]m00_couplers_to_m00_regslice_ARLEN;
  wire [0:0]m00_couplers_to_m00_regslice_ARLOCK;
  wire [2:0]m00_couplers_to_m00_regslice_ARPROT;
  wire [3:0]m00_couplers_to_m00_regslice_ARQOS;
  wire m00_couplers_to_m00_regslice_ARREADY;
  wire [3:0]m00_couplers_to_m00_regslice_ARREGION;
  wire [2:0]m00_couplers_to_m00_regslice_ARSIZE;
  wire m00_couplers_to_m00_regslice_ARVALID;
  wire [63:0]m00_couplers_to_m00_regslice_AWADDR;
  wire [1:0]m00_couplers_to_m00_regslice_AWBURST;
  wire [3:0]m00_couplers_to_m00_regslice_AWCACHE;
  wire [6:0]m00_couplers_to_m00_regslice_AWID;
  wire [7:0]m00_couplers_to_m00_regslice_AWLEN;
  wire [0:0]m00_couplers_to_m00_regslice_AWLOCK;
  wire [2:0]m00_couplers_to_m00_regslice_AWPROT;
  wire [3:0]m00_couplers_to_m00_regslice_AWQOS;
  wire m00_couplers_to_m00_regslice_AWREADY;
  wire [3:0]m00_couplers_to_m00_regslice_AWREGION;
  wire [2:0]m00_couplers_to_m00_regslice_AWSIZE;
  wire m00_couplers_to_m00_regslice_AWVALID;
  wire [6:0]m00_couplers_to_m00_regslice_BID;
  wire m00_couplers_to_m00_regslice_BREADY;
  wire [1:0]m00_couplers_to_m00_regslice_BRESP;
  wire m00_couplers_to_m00_regslice_BVALID;
  wire [511:0]m00_couplers_to_m00_regslice_RDATA;
  wire [6:0]m00_couplers_to_m00_regslice_RID;
  wire m00_couplers_to_m00_regslice_RLAST;
  wire m00_couplers_to_m00_regslice_RREADY;
  wire [1:0]m00_couplers_to_m00_regslice_RRESP;
  wire m00_couplers_to_m00_regslice_RVALID;
  wire [511:0]m00_couplers_to_m00_regslice_WDATA;
  wire m00_couplers_to_m00_regslice_WLAST;
  wire m00_couplers_to_m00_regslice_WREADY;
  wire [63:0]m00_couplers_to_m00_regslice_WSTRB;
  wire m00_couplers_to_m00_regslice_WVALID;
  wire [63:0]m00_regslice_to_m00_couplers_ARADDR;
  wire [1:0]m00_regslice_to_m00_couplers_ARBURST;
  wire [3:0]m00_regslice_to_m00_couplers_ARCACHE;
  wire [6:0]m00_regslice_to_m00_couplers_ARID;
  wire [7:0]m00_regslice_to_m00_couplers_ARLEN;
  wire [0:0]m00_regslice_to_m00_couplers_ARLOCK;
  wire [2:0]m00_regslice_to_m00_couplers_ARPROT;
  wire [3:0]m00_regslice_to_m00_couplers_ARQOS;
  wire m00_regslice_to_m00_couplers_ARREADY;
  wire [3:0]m00_regslice_to_m00_couplers_ARREGION;
  wire [2:0]m00_regslice_to_m00_couplers_ARSIZE;
  wire m00_regslice_to_m00_couplers_ARVALID;
  wire [63:0]m00_regslice_to_m00_couplers_AWADDR;
  wire [1:0]m00_regslice_to_m00_couplers_AWBURST;
  wire [3:0]m00_regslice_to_m00_couplers_AWCACHE;
  wire [6:0]m00_regslice_to_m00_couplers_AWID;
  wire [7:0]m00_regslice_to_m00_couplers_AWLEN;
  wire [0:0]m00_regslice_to_m00_couplers_AWLOCK;
  wire [2:0]m00_regslice_to_m00_couplers_AWPROT;
  wire [3:0]m00_regslice_to_m00_couplers_AWQOS;
  wire m00_regslice_to_m00_couplers_AWREADY;
  wire [3:0]m00_regslice_to_m00_couplers_AWREGION;
  wire [2:0]m00_regslice_to_m00_couplers_AWSIZE;
  wire m00_regslice_to_m00_couplers_AWVALID;
  wire [6:0]m00_regslice_to_m00_couplers_BID;
  wire m00_regslice_to_m00_couplers_BREADY;
  wire [1:0]m00_regslice_to_m00_couplers_BRESP;
  wire m00_regslice_to_m00_couplers_BVALID;
  wire [511:0]m00_regslice_to_m00_couplers_RDATA;
  wire [6:0]m00_regslice_to_m00_couplers_RID;
  wire m00_regslice_to_m00_couplers_RLAST;
  wire m00_regslice_to_m00_couplers_RREADY;
  wire [1:0]m00_regslice_to_m00_couplers_RRESP;
  wire m00_regslice_to_m00_couplers_RVALID;
  wire [511:0]m00_regslice_to_m00_couplers_WDATA;
  wire m00_regslice_to_m00_couplers_WLAST;
  wire m00_regslice_to_m00_couplers_WREADY;
  wire [63:0]m00_regslice_to_m00_couplers_WSTRB;
  wire m00_regslice_to_m00_couplers_WVALID;

  assign M_ACLK_1 = M_ACLK;
  assign M_ARESETN_1 = M_ARESETN;
  assign M_AXI_araddr[63:0] = m00_regslice_to_m00_couplers_ARADDR;
  assign M_AXI_arburst[1:0] = m00_regslice_to_m00_couplers_ARBURST;
  assign M_AXI_arcache[3:0] = m00_regslice_to_m00_couplers_ARCACHE;
  assign M_AXI_arid[6:0] = m00_regslice_to_m00_couplers_ARID;
  assign M_AXI_arlen[7:0] = m00_regslice_to_m00_couplers_ARLEN;
  assign M_AXI_arlock[0] = m00_regslice_to_m00_couplers_ARLOCK;
  assign M_AXI_arprot[2:0] = m00_regslice_to_m00_couplers_ARPROT;
  assign M_AXI_arqos[3:0] = m00_regslice_to_m00_couplers_ARQOS;
  assign M_AXI_arregion[3:0] = m00_regslice_to_m00_couplers_ARREGION;
  assign M_AXI_arsize[2:0] = m00_regslice_to_m00_couplers_ARSIZE;
  assign M_AXI_arvalid = m00_regslice_to_m00_couplers_ARVALID;
  assign M_AXI_awaddr[63:0] = m00_regslice_to_m00_couplers_AWADDR;
  assign M_AXI_awburst[1:0] = m00_regslice_to_m00_couplers_AWBURST;
  assign M_AXI_awcache[3:0] = m00_regslice_to_m00_couplers_AWCACHE;
  assign M_AXI_awid[6:0] = m00_regslice_to_m00_couplers_AWID;
  assign M_AXI_awlen[7:0] = m00_regslice_to_m00_couplers_AWLEN;
  assign M_AXI_awlock[0] = m00_regslice_to_m00_couplers_AWLOCK;
  assign M_AXI_awprot[2:0] = m00_regslice_to_m00_couplers_AWPROT;
  assign M_AXI_awqos[3:0] = m00_regslice_to_m00_couplers_AWQOS;
  assign M_AXI_awregion[3:0] = m00_regslice_to_m00_couplers_AWREGION;
  assign M_AXI_awsize[2:0] = m00_regslice_to_m00_couplers_AWSIZE;
  assign M_AXI_awvalid = m00_regslice_to_m00_couplers_AWVALID;
  assign M_AXI_bready = m00_regslice_to_m00_couplers_BREADY;
  assign M_AXI_rready = m00_regslice_to_m00_couplers_RREADY;
  assign M_AXI_wdata[511:0] = m00_regslice_to_m00_couplers_WDATA;
  assign M_AXI_wlast = m00_regslice_to_m00_couplers_WLAST;
  assign M_AXI_wstrb[63:0] = m00_regslice_to_m00_couplers_WSTRB;
  assign M_AXI_wvalid = m00_regslice_to_m00_couplers_WVALID;
  assign S_AXI_arready = m00_couplers_to_m00_regslice_ARREADY;
  assign S_AXI_awready = m00_couplers_to_m00_regslice_AWREADY;
  assign S_AXI_bid[6:0] = m00_couplers_to_m00_regslice_BID;
  assign S_AXI_bresp[1:0] = m00_couplers_to_m00_regslice_BRESP;
  assign S_AXI_bvalid = m00_couplers_to_m00_regslice_BVALID;
  assign S_AXI_rdata[511:0] = m00_couplers_to_m00_regslice_RDATA;
  assign S_AXI_rid[6:0] = m00_couplers_to_m00_regslice_RID;
  assign S_AXI_rlast = m00_couplers_to_m00_regslice_RLAST;
  assign S_AXI_rresp[1:0] = m00_couplers_to_m00_regslice_RRESP;
  assign S_AXI_rvalid = m00_couplers_to_m00_regslice_RVALID;
  assign S_AXI_wready = m00_couplers_to_m00_regslice_WREADY;
  assign m00_couplers_to_m00_regslice_ARADDR = S_AXI_araddr[63:0];
  assign m00_couplers_to_m00_regslice_ARBURST = S_AXI_arburst[1:0];
  assign m00_couplers_to_m00_regslice_ARCACHE = S_AXI_arcache[3:0];
  assign m00_couplers_to_m00_regslice_ARID = S_AXI_arid[6:0];
  assign m00_couplers_to_m00_regslice_ARLEN = S_AXI_arlen[7:0];
  assign m00_couplers_to_m00_regslice_ARLOCK = S_AXI_arlock[0];
  assign m00_couplers_to_m00_regslice_ARPROT = S_AXI_arprot[2:0];
  assign m00_couplers_to_m00_regslice_ARQOS = S_AXI_arqos[3:0];
  assign m00_couplers_to_m00_regslice_ARREGION = S_AXI_arregion[3:0];
  assign m00_couplers_to_m00_regslice_ARSIZE = S_AXI_arsize[2:0];
  assign m00_couplers_to_m00_regslice_ARVALID = S_AXI_arvalid;
  assign m00_couplers_to_m00_regslice_AWADDR = S_AXI_awaddr[63:0];
  assign m00_couplers_to_m00_regslice_AWBURST = S_AXI_awburst[1:0];
  assign m00_couplers_to_m00_regslice_AWCACHE = S_AXI_awcache[3:0];
  assign m00_couplers_to_m00_regslice_AWID = S_AXI_awid[6:0];
  assign m00_couplers_to_m00_regslice_AWLEN = S_AXI_awlen[7:0];
  assign m00_couplers_to_m00_regslice_AWLOCK = S_AXI_awlock[0];
  assign m00_couplers_to_m00_regslice_AWPROT = S_AXI_awprot[2:0];
  assign m00_couplers_to_m00_regslice_AWQOS = S_AXI_awqos[3:0];
  assign m00_couplers_to_m00_regslice_AWREGION = S_AXI_awregion[3:0];
  assign m00_couplers_to_m00_regslice_AWSIZE = S_AXI_awsize[2:0];
  assign m00_couplers_to_m00_regslice_AWVALID = S_AXI_awvalid;
  assign m00_couplers_to_m00_regslice_BREADY = S_AXI_bready;
  assign m00_couplers_to_m00_regslice_RREADY = S_AXI_rready;
  assign m00_couplers_to_m00_regslice_WDATA = S_AXI_wdata[511:0];
  assign m00_couplers_to_m00_regslice_WLAST = S_AXI_wlast;
  assign m00_couplers_to_m00_regslice_WSTRB = S_AXI_wstrb[63:0];
  assign m00_couplers_to_m00_regslice_WVALID = S_AXI_wvalid;
  assign m00_regslice_to_m00_couplers_ARREADY = M_AXI_arready;
  assign m00_regslice_to_m00_couplers_AWREADY = M_AXI_awready;
  assign m00_regslice_to_m00_couplers_BID = M_AXI_bid[6:0];
  assign m00_regslice_to_m00_couplers_BRESP = M_AXI_bresp[1:0];
  assign m00_regslice_to_m00_couplers_BVALID = M_AXI_bvalid;
  assign m00_regslice_to_m00_couplers_RDATA = M_AXI_rdata[511:0];
  assign m00_regslice_to_m00_couplers_RID = M_AXI_rid[6:0];
  assign m00_regslice_to_m00_couplers_RLAST = M_AXI_rlast;
  assign m00_regslice_to_m00_couplers_RRESP = M_AXI_rresp[1:0];
  assign m00_regslice_to_m00_couplers_RVALID = M_AXI_rvalid;
  assign m00_regslice_to_m00_couplers_WREADY = M_AXI_wready;
  cl_xbar_m00_regslice_3 m00_regslice
       (.aclk(M_ACLK_1),
        .aresetn(M_ARESETN_1),
        .m_axi_araddr(m00_regslice_to_m00_couplers_ARADDR),
        .m_axi_arburst(m00_regslice_to_m00_couplers_ARBURST),
        .m_axi_arcache(m00_regslice_to_m00_couplers_ARCACHE),
        .m_axi_arid(m00_regslice_to_m00_couplers_ARID),
        .m_axi_arlen(m00_regslice_to_m00_couplers_ARLEN),
        .m_axi_arlock(m00_regslice_to_m00_couplers_ARLOCK),
        .m_axi_arprot(m00_regslice_to_m00_couplers_ARPROT),
        .m_axi_arqos(m00_regslice_to_m00_couplers_ARQOS),
        .m_axi_arready(m00_regslice_to_m00_couplers_ARREADY),
        .m_axi_arregion(m00_regslice_to_m00_couplers_ARREGION),
        .m_axi_arsize(m00_regslice_to_m00_couplers_ARSIZE),
        .m_axi_arvalid(m00_regslice_to_m00_couplers_ARVALID),
        .m_axi_awaddr(m00_regslice_to_m00_couplers_AWADDR),
        .m_axi_awburst(m00_regslice_to_m00_couplers_AWBURST),
        .m_axi_awcache(m00_regslice_to_m00_couplers_AWCACHE),
        .m_axi_awid(m00_regslice_to_m00_couplers_AWID),
        .m_axi_awlen(m00_regslice_to_m00_couplers_AWLEN),
        .m_axi_awlock(m00_regslice_to_m00_couplers_AWLOCK),
        .m_axi_awprot(m00_regslice_to_m00_couplers_AWPROT),
        .m_axi_awqos(m00_regslice_to_m00_couplers_AWQOS),
        .m_axi_awready(m00_regslice_to_m00_couplers_AWREADY),
        .m_axi_awregion(m00_regslice_to_m00_couplers_AWREGION),
        .m_axi_awsize(m00_regslice_to_m00_couplers_AWSIZE),
        .m_axi_awvalid(m00_regslice_to_m00_couplers_AWVALID),
        .m_axi_bid(m00_regslice_to_m00_couplers_BID),
        .m_axi_bready(m00_regslice_to_m00_couplers_BREADY),
        .m_axi_bresp(m00_regslice_to_m00_couplers_BRESP),
        .m_axi_bvalid(m00_regslice_to_m00_couplers_BVALID),
        .m_axi_rdata(m00_regslice_to_m00_couplers_RDATA),
        .m_axi_rid(m00_regslice_to_m00_couplers_RID),
        .m_axi_rlast(m00_regslice_to_m00_couplers_RLAST),
        .m_axi_rready(m00_regslice_to_m00_couplers_RREADY),
        .m_axi_rresp(m00_regslice_to_m00_couplers_RRESP),
        .m_axi_rvalid(m00_regslice_to_m00_couplers_RVALID),
        .m_axi_wdata(m00_regslice_to_m00_couplers_WDATA),
        .m_axi_wlast(m00_regslice_to_m00_couplers_WLAST),
        .m_axi_wready(m00_regslice_to_m00_couplers_WREADY),
        .m_axi_wstrb(m00_regslice_to_m00_couplers_WSTRB),
        .m_axi_wvalid(m00_regslice_to_m00_couplers_WVALID),
        .s_axi_araddr(m00_couplers_to_m00_regslice_ARADDR),
        .s_axi_arburst(m00_couplers_to_m00_regslice_ARBURST),
        .s_axi_arcache(m00_couplers_to_m00_regslice_ARCACHE),
        .s_axi_arid(m00_couplers_to_m00_regslice_ARID),
        .s_axi_arlen(m00_couplers_to_m00_regslice_ARLEN),
        .s_axi_arlock(m00_couplers_to_m00_regslice_ARLOCK),
        .s_axi_arprot(m00_couplers_to_m00_regslice_ARPROT),
        .s_axi_arqos(m00_couplers_to_m00_regslice_ARQOS),
        .s_axi_arready(m00_couplers_to_m00_regslice_ARREADY),
        .s_axi_arregion(m00_couplers_to_m00_regslice_ARREGION),
        .s_axi_arsize(m00_couplers_to_m00_regslice_ARSIZE),
        .s_axi_arvalid(m00_couplers_to_m00_regslice_ARVALID),
        .s_axi_awaddr(m00_couplers_to_m00_regslice_AWADDR),
        .s_axi_awburst(m00_couplers_to_m00_regslice_AWBURST),
        .s_axi_awcache(m00_couplers_to_m00_regslice_AWCACHE),
        .s_axi_awid(m00_couplers_to_m00_regslice_AWID),
        .s_axi_awlen(m00_couplers_to_m00_regslice_AWLEN),
        .s_axi_awlock(m00_couplers_to_m00_regslice_AWLOCK),
        .s_axi_awprot(m00_couplers_to_m00_regslice_AWPROT),
        .s_axi_awqos(m00_couplers_to_m00_regslice_AWQOS),
        .s_axi_awready(m00_couplers_to_m00_regslice_AWREADY),
        .s_axi_awregion(m00_couplers_to_m00_regslice_AWREGION),
        .s_axi_awsize(m00_couplers_to_m00_regslice_AWSIZE),
        .s_axi_awvalid(m00_couplers_to_m00_regslice_AWVALID),
        .s_axi_bid(m00_couplers_to_m00_regslice_BID),
        .s_axi_bready(m00_couplers_to_m00_regslice_BREADY),
        .s_axi_bresp(m00_couplers_to_m00_regslice_BRESP),
        .s_axi_bvalid(m00_couplers_to_m00_regslice_BVALID),
        .s_axi_rdata(m00_couplers_to_m00_regslice_RDATA),
        .s_axi_rid(m00_couplers_to_m00_regslice_RID),
        .s_axi_rlast(m00_couplers_to_m00_regslice_RLAST),
        .s_axi_rready(m00_couplers_to_m00_regslice_RREADY),
        .s_axi_rresp(m00_couplers_to_m00_regslice_RRESP),
        .s_axi_rvalid(m00_couplers_to_m00_regslice_RVALID),
        .s_axi_wdata(m00_couplers_to_m00_regslice_WDATA),
        .s_axi_wlast(m00_couplers_to_m00_regslice_WLAST),
        .s_axi_wready(m00_couplers_to_m00_regslice_WREADY),
        .s_axi_wstrb(m00_couplers_to_m00_regslice_WSTRB),
        .s_axi_wvalid(m00_couplers_to_m00_regslice_WVALID));
endmodule

module m01_couplers_imp_1I0GTH5
   (M_ACLK,
    M_ARESETN,
    M_AXI_araddr,
    M_AXI_arburst,
    M_AXI_arcache,
    M_AXI_arid,
    M_AXI_arlen,
    M_AXI_arlock,
    M_AXI_arprot,
    M_AXI_arqos,
    M_AXI_arready,
    M_AXI_arregion,
    M_AXI_arsize,
    M_AXI_arvalid,
    M_AXI_awaddr,
    M_AXI_awburst,
    M_AXI_awcache,
    M_AXI_awid,
    M_AXI_awlen,
    M_AXI_awlock,
    M_AXI_awprot,
    M_AXI_awqos,
    M_AXI_awready,
    M_AXI_awregion,
    M_AXI_awsize,
    M_AXI_awvalid,
    M_AXI_bid,
    M_AXI_bready,
    M_AXI_bresp,
    M_AXI_bvalid,
    M_AXI_rdata,
    M_AXI_rid,
    M_AXI_rlast,
    M_AXI_rready,
    M_AXI_rresp,
    M_AXI_rvalid,
    M_AXI_wdata,
    M_AXI_wlast,
    M_AXI_wready,
    M_AXI_wstrb,
    M_AXI_wvalid,
    S_ACLK,
    S_ARESETN,
    S_AXI_araddr,
    S_AXI_arburst,
    S_AXI_arcache,
    S_AXI_arid,
    S_AXI_arlen,
    S_AXI_arlock,
    S_AXI_arprot,
    S_AXI_arqos,
    S_AXI_arready,
    S_AXI_arregion,
    S_AXI_arsize,
    S_AXI_arvalid,
    S_AXI_awaddr,
    S_AXI_awburst,
    S_AXI_awcache,
    S_AXI_awid,
    S_AXI_awlen,
    S_AXI_awlock,
    S_AXI_awprot,
    S_AXI_awqos,
    S_AXI_awready,
    S_AXI_awregion,
    S_AXI_awsize,
    S_AXI_awvalid,
    S_AXI_bid,
    S_AXI_bready,
    S_AXI_bresp,
    S_AXI_bvalid,
    S_AXI_rdata,
    S_AXI_rid,
    S_AXI_rlast,
    S_AXI_rready,
    S_AXI_rresp,
    S_AXI_rvalid,
    S_AXI_wdata,
    S_AXI_wlast,
    S_AXI_wready,
    S_AXI_wstrb,
    S_AXI_wvalid);
  input M_ACLK;
  input M_ARESETN;
  output [63:0]M_AXI_araddr;
  output [1:0]M_AXI_arburst;
  output [3:0]M_AXI_arcache;
  output [6:0]M_AXI_arid;
  output [7:0]M_AXI_arlen;
  output [0:0]M_AXI_arlock;
  output [2:0]M_AXI_arprot;
  output [3:0]M_AXI_arqos;
  input M_AXI_arready;
  output [3:0]M_AXI_arregion;
  output [2:0]M_AXI_arsize;
  output M_AXI_arvalid;
  output [63:0]M_AXI_awaddr;
  output [1:0]M_AXI_awburst;
  output [3:0]M_AXI_awcache;
  output [6:0]M_AXI_awid;
  output [7:0]M_AXI_awlen;
  output [0:0]M_AXI_awlock;
  output [2:0]M_AXI_awprot;
  output [3:0]M_AXI_awqos;
  input M_AXI_awready;
  output [3:0]M_AXI_awregion;
  output [2:0]M_AXI_awsize;
  output M_AXI_awvalid;
  input [6:0]M_AXI_bid;
  output M_AXI_bready;
  input [1:0]M_AXI_bresp;
  input M_AXI_bvalid;
  input [511:0]M_AXI_rdata;
  input [6:0]M_AXI_rid;
  input M_AXI_rlast;
  output M_AXI_rready;
  input [1:0]M_AXI_rresp;
  input M_AXI_rvalid;
  output [511:0]M_AXI_wdata;
  output M_AXI_wlast;
  input M_AXI_wready;
  output [63:0]M_AXI_wstrb;
  output M_AXI_wvalid;
  input S_ACLK;
  input S_ARESETN;
  input [63:0]S_AXI_araddr;
  input [1:0]S_AXI_arburst;
  input [3:0]S_AXI_arcache;
  input [6:0]S_AXI_arid;
  input [7:0]S_AXI_arlen;
  input [0:0]S_AXI_arlock;
  input [2:0]S_AXI_arprot;
  input [3:0]S_AXI_arqos;
  output S_AXI_arready;
  input [3:0]S_AXI_arregion;
  input [2:0]S_AXI_arsize;
  input S_AXI_arvalid;
  input [63:0]S_AXI_awaddr;
  input [1:0]S_AXI_awburst;
  input [3:0]S_AXI_awcache;
  input [6:0]S_AXI_awid;
  input [7:0]S_AXI_awlen;
  input [0:0]S_AXI_awlock;
  input [2:0]S_AXI_awprot;
  input [3:0]S_AXI_awqos;
  output S_AXI_awready;
  input [3:0]S_AXI_awregion;
  input [2:0]S_AXI_awsize;
  input S_AXI_awvalid;
  output [6:0]S_AXI_bid;
  input S_AXI_bready;
  output [1:0]S_AXI_bresp;
  output S_AXI_bvalid;
  output [511:0]S_AXI_rdata;
  output [6:0]S_AXI_rid;
  output S_AXI_rlast;
  input S_AXI_rready;
  output [1:0]S_AXI_rresp;
  output S_AXI_rvalid;
  input [511:0]S_AXI_wdata;
  input S_AXI_wlast;
  output S_AXI_wready;
  input [63:0]S_AXI_wstrb;
  input S_AXI_wvalid;

  wire M_ACLK_1;
  wire M_ARESETN_1;
  wire [63:0]m01_couplers_to_m01_regslice_ARADDR;
  wire [1:0]m01_couplers_to_m01_regslice_ARBURST;
  wire [3:0]m01_couplers_to_m01_regslice_ARCACHE;
  wire [6:0]m01_couplers_to_m01_regslice_ARID;
  wire [7:0]m01_couplers_to_m01_regslice_ARLEN;
  wire [0:0]m01_couplers_to_m01_regslice_ARLOCK;
  wire [2:0]m01_couplers_to_m01_regslice_ARPROT;
  wire [3:0]m01_couplers_to_m01_regslice_ARQOS;
  wire m01_couplers_to_m01_regslice_ARREADY;
  wire [3:0]m01_couplers_to_m01_regslice_ARREGION;
  wire [2:0]m01_couplers_to_m01_regslice_ARSIZE;
  wire m01_couplers_to_m01_regslice_ARVALID;
  wire [63:0]m01_couplers_to_m01_regslice_AWADDR;
  wire [1:0]m01_couplers_to_m01_regslice_AWBURST;
  wire [3:0]m01_couplers_to_m01_regslice_AWCACHE;
  wire [6:0]m01_couplers_to_m01_regslice_AWID;
  wire [7:0]m01_couplers_to_m01_regslice_AWLEN;
  wire [0:0]m01_couplers_to_m01_regslice_AWLOCK;
  wire [2:0]m01_couplers_to_m01_regslice_AWPROT;
  wire [3:0]m01_couplers_to_m01_regslice_AWQOS;
  wire m01_couplers_to_m01_regslice_AWREADY;
  wire [3:0]m01_couplers_to_m01_regslice_AWREGION;
  wire [2:0]m01_couplers_to_m01_regslice_AWSIZE;
  wire m01_couplers_to_m01_regslice_AWVALID;
  wire [6:0]m01_couplers_to_m01_regslice_BID;
  wire m01_couplers_to_m01_regslice_BREADY;
  wire [1:0]m01_couplers_to_m01_regslice_BRESP;
  wire m01_couplers_to_m01_regslice_BVALID;
  wire [511:0]m01_couplers_to_m01_regslice_RDATA;
  wire [6:0]m01_couplers_to_m01_regslice_RID;
  wire m01_couplers_to_m01_regslice_RLAST;
  wire m01_couplers_to_m01_regslice_RREADY;
  wire [1:0]m01_couplers_to_m01_regslice_RRESP;
  wire m01_couplers_to_m01_regslice_RVALID;
  wire [511:0]m01_couplers_to_m01_regslice_WDATA;
  wire m01_couplers_to_m01_regslice_WLAST;
  wire m01_couplers_to_m01_regslice_WREADY;
  wire [63:0]m01_couplers_to_m01_regslice_WSTRB;
  wire m01_couplers_to_m01_regslice_WVALID;
  wire [63:0]m01_regslice_to_m01_couplers_ARADDR;
  wire [1:0]m01_regslice_to_m01_couplers_ARBURST;
  wire [3:0]m01_regslice_to_m01_couplers_ARCACHE;
  wire [6:0]m01_regslice_to_m01_couplers_ARID;
  wire [7:0]m01_regslice_to_m01_couplers_ARLEN;
  wire [0:0]m01_regslice_to_m01_couplers_ARLOCK;
  wire [2:0]m01_regslice_to_m01_couplers_ARPROT;
  wire [3:0]m01_regslice_to_m01_couplers_ARQOS;
  wire m01_regslice_to_m01_couplers_ARREADY;
  wire [3:0]m01_regslice_to_m01_couplers_ARREGION;
  wire [2:0]m01_regslice_to_m01_couplers_ARSIZE;
  wire m01_regslice_to_m01_couplers_ARVALID;
  wire [63:0]m01_regslice_to_m01_couplers_AWADDR;
  wire [1:0]m01_regslice_to_m01_couplers_AWBURST;
  wire [3:0]m01_regslice_to_m01_couplers_AWCACHE;
  wire [6:0]m01_regslice_to_m01_couplers_AWID;
  wire [7:0]m01_regslice_to_m01_couplers_AWLEN;
  wire [0:0]m01_regslice_to_m01_couplers_AWLOCK;
  wire [2:0]m01_regslice_to_m01_couplers_AWPROT;
  wire [3:0]m01_regslice_to_m01_couplers_AWQOS;
  wire m01_regslice_to_m01_couplers_AWREADY;
  wire [3:0]m01_regslice_to_m01_couplers_AWREGION;
  wire [2:0]m01_regslice_to_m01_couplers_AWSIZE;
  wire m01_regslice_to_m01_couplers_AWVALID;
  wire [6:0]m01_regslice_to_m01_couplers_BID;
  wire m01_regslice_to_m01_couplers_BREADY;
  wire [1:0]m01_regslice_to_m01_couplers_BRESP;
  wire m01_regslice_to_m01_couplers_BVALID;
  wire [511:0]m01_regslice_to_m01_couplers_RDATA;
  wire [6:0]m01_regslice_to_m01_couplers_RID;
  wire m01_regslice_to_m01_couplers_RLAST;
  wire m01_regslice_to_m01_couplers_RREADY;
  wire [1:0]m01_regslice_to_m01_couplers_RRESP;
  wire m01_regslice_to_m01_couplers_RVALID;
  wire [511:0]m01_regslice_to_m01_couplers_WDATA;
  wire m01_regslice_to_m01_couplers_WLAST;
  wire m01_regslice_to_m01_couplers_WREADY;
  wire [63:0]m01_regslice_to_m01_couplers_WSTRB;
  wire m01_regslice_to_m01_couplers_WVALID;

  assign M_ACLK_1 = M_ACLK;
  assign M_ARESETN_1 = M_ARESETN;
  assign M_AXI_araddr[63:0] = m01_regslice_to_m01_couplers_ARADDR;
  assign M_AXI_arburst[1:0] = m01_regslice_to_m01_couplers_ARBURST;
  assign M_AXI_arcache[3:0] = m01_regslice_to_m01_couplers_ARCACHE;
  assign M_AXI_arid[6:0] = m01_regslice_to_m01_couplers_ARID;
  assign M_AXI_arlen[7:0] = m01_regslice_to_m01_couplers_ARLEN;
  assign M_AXI_arlock[0] = m01_regslice_to_m01_couplers_ARLOCK;
  assign M_AXI_arprot[2:0] = m01_regslice_to_m01_couplers_ARPROT;
  assign M_AXI_arqos[3:0] = m01_regslice_to_m01_couplers_ARQOS;
  assign M_AXI_arregion[3:0] = m01_regslice_to_m01_couplers_ARREGION;
  assign M_AXI_arsize[2:0] = m01_regslice_to_m01_couplers_ARSIZE;
  assign M_AXI_arvalid = m01_regslice_to_m01_couplers_ARVALID;
  assign M_AXI_awaddr[63:0] = m01_regslice_to_m01_couplers_AWADDR;
  assign M_AXI_awburst[1:0] = m01_regslice_to_m01_couplers_AWBURST;
  assign M_AXI_awcache[3:0] = m01_regslice_to_m01_couplers_AWCACHE;
  assign M_AXI_awid[6:0] = m01_regslice_to_m01_couplers_AWID;
  assign M_AXI_awlen[7:0] = m01_regslice_to_m01_couplers_AWLEN;
  assign M_AXI_awlock[0] = m01_regslice_to_m01_couplers_AWLOCK;
  assign M_AXI_awprot[2:0] = m01_regslice_to_m01_couplers_AWPROT;
  assign M_AXI_awqos[3:0] = m01_regslice_to_m01_couplers_AWQOS;
  assign M_AXI_awregion[3:0] = m01_regslice_to_m01_couplers_AWREGION;
  assign M_AXI_awsize[2:0] = m01_regslice_to_m01_couplers_AWSIZE;
  assign M_AXI_awvalid = m01_regslice_to_m01_couplers_AWVALID;
  assign M_AXI_bready = m01_regslice_to_m01_couplers_BREADY;
  assign M_AXI_rready = m01_regslice_to_m01_couplers_RREADY;
  assign M_AXI_wdata[511:0] = m01_regslice_to_m01_couplers_WDATA;
  assign M_AXI_wlast = m01_regslice_to_m01_couplers_WLAST;
  assign M_AXI_wstrb[63:0] = m01_regslice_to_m01_couplers_WSTRB;
  assign M_AXI_wvalid = m01_regslice_to_m01_couplers_WVALID;
  assign S_AXI_arready = m01_couplers_to_m01_regslice_ARREADY;
  assign S_AXI_awready = m01_couplers_to_m01_regslice_AWREADY;
  assign S_AXI_bid[6:0] = m01_couplers_to_m01_regslice_BID;
  assign S_AXI_bresp[1:0] = m01_couplers_to_m01_regslice_BRESP;
  assign S_AXI_bvalid = m01_couplers_to_m01_regslice_BVALID;
  assign S_AXI_rdata[511:0] = m01_couplers_to_m01_regslice_RDATA;
  assign S_AXI_rid[6:0] = m01_couplers_to_m01_regslice_RID;
  assign S_AXI_rlast = m01_couplers_to_m01_regslice_RLAST;
  assign S_AXI_rresp[1:0] = m01_couplers_to_m01_regslice_RRESP;
  assign S_AXI_rvalid = m01_couplers_to_m01_regslice_RVALID;
  assign S_AXI_wready = m01_couplers_to_m01_regslice_WREADY;
  assign m01_couplers_to_m01_regslice_ARADDR = S_AXI_araddr[63:0];
  assign m01_couplers_to_m01_regslice_ARBURST = S_AXI_arburst[1:0];
  assign m01_couplers_to_m01_regslice_ARCACHE = S_AXI_arcache[3:0];
  assign m01_couplers_to_m01_regslice_ARID = S_AXI_arid[6:0];
  assign m01_couplers_to_m01_regslice_ARLEN = S_AXI_arlen[7:0];
  assign m01_couplers_to_m01_regslice_ARLOCK = S_AXI_arlock[0];
  assign m01_couplers_to_m01_regslice_ARPROT = S_AXI_arprot[2:0];
  assign m01_couplers_to_m01_regslice_ARQOS = S_AXI_arqos[3:0];
  assign m01_couplers_to_m01_regslice_ARREGION = S_AXI_arregion[3:0];
  assign m01_couplers_to_m01_regslice_ARSIZE = S_AXI_arsize[2:0];
  assign m01_couplers_to_m01_regslice_ARVALID = S_AXI_arvalid;
  assign m01_couplers_to_m01_regslice_AWADDR = S_AXI_awaddr[63:0];
  assign m01_couplers_to_m01_regslice_AWBURST = S_AXI_awburst[1:0];
  assign m01_couplers_to_m01_regslice_AWCACHE = S_AXI_awcache[3:0];
  assign m01_couplers_to_m01_regslice_AWID = S_AXI_awid[6:0];
  assign m01_couplers_to_m01_regslice_AWLEN = S_AXI_awlen[7:0];
  assign m01_couplers_to_m01_regslice_AWLOCK = S_AXI_awlock[0];
  assign m01_couplers_to_m01_regslice_AWPROT = S_AXI_awprot[2:0];
  assign m01_couplers_to_m01_regslice_AWQOS = S_AXI_awqos[3:0];
  assign m01_couplers_to_m01_regslice_AWREGION = S_AXI_awregion[3:0];
  assign m01_couplers_to_m01_regslice_AWSIZE = S_AXI_awsize[2:0];
  assign m01_couplers_to_m01_regslice_AWVALID = S_AXI_awvalid;
  assign m01_couplers_to_m01_regslice_BREADY = S_AXI_bready;
  assign m01_couplers_to_m01_regslice_RREADY = S_AXI_rready;
  assign m01_couplers_to_m01_regslice_WDATA = S_AXI_wdata[511:0];
  assign m01_couplers_to_m01_regslice_WLAST = S_AXI_wlast;
  assign m01_couplers_to_m01_regslice_WSTRB = S_AXI_wstrb[63:0];
  assign m01_couplers_to_m01_regslice_WVALID = S_AXI_wvalid;
  assign m01_regslice_to_m01_couplers_ARREADY = M_AXI_arready;
  assign m01_regslice_to_m01_couplers_AWREADY = M_AXI_awready;
  assign m01_regslice_to_m01_couplers_BID = M_AXI_bid[6:0];
  assign m01_regslice_to_m01_couplers_BRESP = M_AXI_bresp[1:0];
  assign m01_regslice_to_m01_couplers_BVALID = M_AXI_bvalid;
  assign m01_regslice_to_m01_couplers_RDATA = M_AXI_rdata[511:0];
  assign m01_regslice_to_m01_couplers_RID = M_AXI_rid[6:0];
  assign m01_regslice_to_m01_couplers_RLAST = M_AXI_rlast;
  assign m01_regslice_to_m01_couplers_RRESP = M_AXI_rresp[1:0];
  assign m01_regslice_to_m01_couplers_RVALID = M_AXI_rvalid;
  assign m01_regslice_to_m01_couplers_WREADY = M_AXI_wready;
  cl_xbar_m01_regslice_3 m01_regslice
       (.aclk(M_ACLK_1),
        .aresetn(M_ARESETN_1),
        .m_axi_araddr(m01_regslice_to_m01_couplers_ARADDR),
        .m_axi_arburst(m01_regslice_to_m01_couplers_ARBURST),
        .m_axi_arcache(m01_regslice_to_m01_couplers_ARCACHE),
        .m_axi_arid(m01_regslice_to_m01_couplers_ARID),
        .m_axi_arlen(m01_regslice_to_m01_couplers_ARLEN),
        .m_axi_arlock(m01_regslice_to_m01_couplers_ARLOCK),
        .m_axi_arprot(m01_regslice_to_m01_couplers_ARPROT),
        .m_axi_arqos(m01_regslice_to_m01_couplers_ARQOS),
        .m_axi_arready(m01_regslice_to_m01_couplers_ARREADY),
        .m_axi_arregion(m01_regslice_to_m01_couplers_ARREGION),
        .m_axi_arsize(m01_regslice_to_m01_couplers_ARSIZE),
        .m_axi_arvalid(m01_regslice_to_m01_couplers_ARVALID),
        .m_axi_awaddr(m01_regslice_to_m01_couplers_AWADDR),
        .m_axi_awburst(m01_regslice_to_m01_couplers_AWBURST),
        .m_axi_awcache(m01_regslice_to_m01_couplers_AWCACHE),
        .m_axi_awid(m01_regslice_to_m01_couplers_AWID),
        .m_axi_awlen(m01_regslice_to_m01_couplers_AWLEN),
        .m_axi_awlock(m01_regslice_to_m01_couplers_AWLOCK),
        .m_axi_awprot(m01_regslice_to_m01_couplers_AWPROT),
        .m_axi_awqos(m01_regslice_to_m01_couplers_AWQOS),
        .m_axi_awready(m01_regslice_to_m01_couplers_AWREADY),
        .m_axi_awregion(m01_regslice_to_m01_couplers_AWREGION),
        .m_axi_awsize(m01_regslice_to_m01_couplers_AWSIZE),
        .m_axi_awvalid(m01_regslice_to_m01_couplers_AWVALID),
        .m_axi_bid(m01_regslice_to_m01_couplers_BID),
        .m_axi_bready(m01_regslice_to_m01_couplers_BREADY),
        .m_axi_bresp(m01_regslice_to_m01_couplers_BRESP),
        .m_axi_bvalid(m01_regslice_to_m01_couplers_BVALID),
        .m_axi_rdata(m01_regslice_to_m01_couplers_RDATA),
        .m_axi_rid(m01_regslice_to_m01_couplers_RID),
        .m_axi_rlast(m01_regslice_to_m01_couplers_RLAST),
        .m_axi_rready(m01_regslice_to_m01_couplers_RREADY),
        .m_axi_rresp(m01_regslice_to_m01_couplers_RRESP),
        .m_axi_rvalid(m01_regslice_to_m01_couplers_RVALID),
        .m_axi_wdata(m01_regslice_to_m01_couplers_WDATA),
        .m_axi_wlast(m01_regslice_to_m01_couplers_WLAST),
        .m_axi_wready(m01_regslice_to_m01_couplers_WREADY),
        .m_axi_wstrb(m01_regslice_to_m01_couplers_WSTRB),
        .m_axi_wvalid(m01_regslice_to_m01_couplers_WVALID),
        .s_axi_araddr(m01_couplers_to_m01_regslice_ARADDR),
        .s_axi_arburst(m01_couplers_to_m01_regslice_ARBURST),
        .s_axi_arcache(m01_couplers_to_m01_regslice_ARCACHE),
        .s_axi_arid(m01_couplers_to_m01_regslice_ARID),
        .s_axi_arlen(m01_couplers_to_m01_regslice_ARLEN),
        .s_axi_arlock(m01_couplers_to_m01_regslice_ARLOCK),
        .s_axi_arprot(m01_couplers_to_m01_regslice_ARPROT),
        .s_axi_arqos(m01_couplers_to_m01_regslice_ARQOS),
        .s_axi_arready(m01_couplers_to_m01_regslice_ARREADY),
        .s_axi_arregion(m01_couplers_to_m01_regslice_ARREGION),
        .s_axi_arsize(m01_couplers_to_m01_regslice_ARSIZE),
        .s_axi_arvalid(m01_couplers_to_m01_regslice_ARVALID),
        .s_axi_awaddr(m01_couplers_to_m01_regslice_AWADDR),
        .s_axi_awburst(m01_couplers_to_m01_regslice_AWBURST),
        .s_axi_awcache(m01_couplers_to_m01_regslice_AWCACHE),
        .s_axi_awid(m01_couplers_to_m01_regslice_AWID),
        .s_axi_awlen(m01_couplers_to_m01_regslice_AWLEN),
        .s_axi_awlock(m01_couplers_to_m01_regslice_AWLOCK),
        .s_axi_awprot(m01_couplers_to_m01_regslice_AWPROT),
        .s_axi_awqos(m01_couplers_to_m01_regslice_AWQOS),
        .s_axi_awready(m01_couplers_to_m01_regslice_AWREADY),
        .s_axi_awregion(m01_couplers_to_m01_regslice_AWREGION),
        .s_axi_awsize(m01_couplers_to_m01_regslice_AWSIZE),
        .s_axi_awvalid(m01_couplers_to_m01_regslice_AWVALID),
        .s_axi_bid(m01_couplers_to_m01_regslice_BID),
        .s_axi_bready(m01_couplers_to_m01_regslice_BREADY),
        .s_axi_bresp(m01_couplers_to_m01_regslice_BRESP),
        .s_axi_bvalid(m01_couplers_to_m01_regslice_BVALID),
        .s_axi_rdata(m01_couplers_to_m01_regslice_RDATA),
        .s_axi_rid(m01_couplers_to_m01_regslice_RID),
        .s_axi_rlast(m01_couplers_to_m01_regslice_RLAST),
        .s_axi_rready(m01_couplers_to_m01_regslice_RREADY),
        .s_axi_rresp(m01_couplers_to_m01_regslice_RRESP),
        .s_axi_rvalid(m01_couplers_to_m01_regslice_RVALID),
        .s_axi_wdata(m01_couplers_to_m01_regslice_WDATA),
        .s_axi_wlast(m01_couplers_to_m01_regslice_WLAST),
        .s_axi_wready(m01_couplers_to_m01_regslice_WREADY),
        .s_axi_wstrb(m01_couplers_to_m01_regslice_WSTRB),
        .s_axi_wvalid(m01_couplers_to_m01_regslice_WVALID));
endmodule

module m01_couplers_imp_39EW9R
   (M_ACLK,
    M_ARESETN,
    M_AXI_araddr,
    M_AXI_arburst,
    M_AXI_arcache,
    M_AXI_arid,
    M_AXI_arlen,
    M_AXI_arlock,
    M_AXI_arprot,
    M_AXI_arqos,
    M_AXI_arready,
    M_AXI_arregion,
    M_AXI_arsize,
    M_AXI_arvalid,
    M_AXI_awaddr,
    M_AXI_awburst,
    M_AXI_awcache,
    M_AXI_awid,
    M_AXI_awlen,
    M_AXI_awlock,
    M_AXI_awprot,
    M_AXI_awqos,
    M_AXI_awready,
    M_AXI_awregion,
    M_AXI_awsize,
    M_AXI_awvalid,
    M_AXI_bid,
    M_AXI_bready,
    M_AXI_bresp,
    M_AXI_bvalid,
    M_AXI_rdata,
    M_AXI_rid,
    M_AXI_rlast,
    M_AXI_rready,
    M_AXI_rresp,
    M_AXI_rvalid,
    M_AXI_wdata,
    M_AXI_wlast,
    M_AXI_wready,
    M_AXI_wstrb,
    M_AXI_wvalid,
    S_ACLK,
    S_ARESETN,
    S_AXI_araddr,
    S_AXI_arburst,
    S_AXI_arcache,
    S_AXI_arid,
    S_AXI_arlen,
    S_AXI_arlock,
    S_AXI_arprot,
    S_AXI_arqos,
    S_AXI_arready,
    S_AXI_arregion,
    S_AXI_arsize,
    S_AXI_arvalid,
    S_AXI_awaddr,
    S_AXI_awburst,
    S_AXI_awcache,
    S_AXI_awid,
    S_AXI_awlen,
    S_AXI_awlock,
    S_AXI_awprot,
    S_AXI_awqos,
    S_AXI_awready,
    S_AXI_awregion,
    S_AXI_awsize,
    S_AXI_awvalid,
    S_AXI_bid,
    S_AXI_bready,
    S_AXI_bresp,
    S_AXI_bvalid,
    S_AXI_rdata,
    S_AXI_rid,
    S_AXI_rlast,
    S_AXI_rready,
    S_AXI_rresp,
    S_AXI_rvalid,
    S_AXI_wdata,
    S_AXI_wlast,
    S_AXI_wready,
    S_AXI_wstrb,
    S_AXI_wvalid);
  input M_ACLK;
  input M_ARESETN;
  output [63:0]M_AXI_araddr;
  output [1:0]M_AXI_arburst;
  output [3:0]M_AXI_arcache;
  output [5:0]M_AXI_arid;
  output [7:0]M_AXI_arlen;
  output [0:0]M_AXI_arlock;
  output [2:0]M_AXI_arprot;
  output [3:0]M_AXI_arqos;
  input M_AXI_arready;
  output [3:0]M_AXI_arregion;
  output [2:0]M_AXI_arsize;
  output M_AXI_arvalid;
  output [63:0]M_AXI_awaddr;
  output [1:0]M_AXI_awburst;
  output [3:0]M_AXI_awcache;
  output [5:0]M_AXI_awid;
  output [7:0]M_AXI_awlen;
  output [0:0]M_AXI_awlock;
  output [2:0]M_AXI_awprot;
  output [3:0]M_AXI_awqos;
  input M_AXI_awready;
  output [3:0]M_AXI_awregion;
  output [2:0]M_AXI_awsize;
  output M_AXI_awvalid;
  input [5:0]M_AXI_bid;
  output M_AXI_bready;
  input [1:0]M_AXI_bresp;
  input M_AXI_bvalid;
  input [511:0]M_AXI_rdata;
  input [5:0]M_AXI_rid;
  input M_AXI_rlast;
  output M_AXI_rready;
  input [1:0]M_AXI_rresp;
  input M_AXI_rvalid;
  output [511:0]M_AXI_wdata;
  output M_AXI_wlast;
  input M_AXI_wready;
  output [63:0]M_AXI_wstrb;
  output M_AXI_wvalid;
  input S_ACLK;
  input S_ARESETN;
  input [63:0]S_AXI_araddr;
  input [1:0]S_AXI_arburst;
  input [3:0]S_AXI_arcache;
  input [5:0]S_AXI_arid;
  input [7:0]S_AXI_arlen;
  input [0:0]S_AXI_arlock;
  input [2:0]S_AXI_arprot;
  input [3:0]S_AXI_arqos;
  output S_AXI_arready;
  input [3:0]S_AXI_arregion;
  input [2:0]S_AXI_arsize;
  input S_AXI_arvalid;
  input [63:0]S_AXI_awaddr;
  input [1:0]S_AXI_awburst;
  input [3:0]S_AXI_awcache;
  input [5:0]S_AXI_awid;
  input [7:0]S_AXI_awlen;
  input [0:0]S_AXI_awlock;
  input [2:0]S_AXI_awprot;
  input [3:0]S_AXI_awqos;
  output S_AXI_awready;
  input [3:0]S_AXI_awregion;
  input [2:0]S_AXI_awsize;
  input S_AXI_awvalid;
  output [5:0]S_AXI_bid;
  input S_AXI_bready;
  output [1:0]S_AXI_bresp;
  output S_AXI_bvalid;
  output [511:0]S_AXI_rdata;
  output [5:0]S_AXI_rid;
  output S_AXI_rlast;
  input S_AXI_rready;
  output [1:0]S_AXI_rresp;
  output S_AXI_rvalid;
  input [511:0]S_AXI_wdata;
  input S_AXI_wlast;
  output S_AXI_wready;
  input [63:0]S_AXI_wstrb;
  input S_AXI_wvalid;

  wire M_ACLK_1;
  wire M_ARESETN_1;
  wire [63:0]m01_couplers_to_m01_regslice_ARADDR;
  wire [1:0]m01_couplers_to_m01_regslice_ARBURST;
  wire [3:0]m01_couplers_to_m01_regslice_ARCACHE;
  wire [5:0]m01_couplers_to_m01_regslice_ARID;
  wire [7:0]m01_couplers_to_m01_regslice_ARLEN;
  wire [0:0]m01_couplers_to_m01_regslice_ARLOCK;
  wire [2:0]m01_couplers_to_m01_regslice_ARPROT;
  wire [3:0]m01_couplers_to_m01_regslice_ARQOS;
  wire m01_couplers_to_m01_regslice_ARREADY;
  wire [3:0]m01_couplers_to_m01_regslice_ARREGION;
  wire [2:0]m01_couplers_to_m01_regslice_ARSIZE;
  wire m01_couplers_to_m01_regslice_ARVALID;
  wire [63:0]m01_couplers_to_m01_regslice_AWADDR;
  wire [1:0]m01_couplers_to_m01_regslice_AWBURST;
  wire [3:0]m01_couplers_to_m01_regslice_AWCACHE;
  wire [5:0]m01_couplers_to_m01_regslice_AWID;
  wire [7:0]m01_couplers_to_m01_regslice_AWLEN;
  wire [0:0]m01_couplers_to_m01_regslice_AWLOCK;
  wire [2:0]m01_couplers_to_m01_regslice_AWPROT;
  wire [3:0]m01_couplers_to_m01_regslice_AWQOS;
  wire m01_couplers_to_m01_regslice_AWREADY;
  wire [3:0]m01_couplers_to_m01_regslice_AWREGION;
  wire [2:0]m01_couplers_to_m01_regslice_AWSIZE;
  wire m01_couplers_to_m01_regslice_AWVALID;
  wire [5:0]m01_couplers_to_m01_regslice_BID;
  wire m01_couplers_to_m01_regslice_BREADY;
  wire [1:0]m01_couplers_to_m01_regslice_BRESP;
  wire m01_couplers_to_m01_regslice_BVALID;
  wire [511:0]m01_couplers_to_m01_regslice_RDATA;
  wire [5:0]m01_couplers_to_m01_regslice_RID;
  wire m01_couplers_to_m01_regslice_RLAST;
  wire m01_couplers_to_m01_regslice_RREADY;
  wire [1:0]m01_couplers_to_m01_regslice_RRESP;
  wire m01_couplers_to_m01_regslice_RVALID;
  wire [511:0]m01_couplers_to_m01_regslice_WDATA;
  wire m01_couplers_to_m01_regslice_WLAST;
  wire m01_couplers_to_m01_regslice_WREADY;
  wire [63:0]m01_couplers_to_m01_regslice_WSTRB;
  wire m01_couplers_to_m01_regslice_WVALID;
  wire [63:0]m01_regslice_to_m01_couplers_ARADDR;
  wire [1:0]m01_regslice_to_m01_couplers_ARBURST;
  wire [3:0]m01_regslice_to_m01_couplers_ARCACHE;
  wire [5:0]m01_regslice_to_m01_couplers_ARID;
  wire [7:0]m01_regslice_to_m01_couplers_ARLEN;
  wire [0:0]m01_regslice_to_m01_couplers_ARLOCK;
  wire [2:0]m01_regslice_to_m01_couplers_ARPROT;
  wire [3:0]m01_regslice_to_m01_couplers_ARQOS;
  wire m01_regslice_to_m01_couplers_ARREADY;
  wire [3:0]m01_regslice_to_m01_couplers_ARREGION;
  wire [2:0]m01_regslice_to_m01_couplers_ARSIZE;
  wire m01_regslice_to_m01_couplers_ARVALID;
  wire [63:0]m01_regslice_to_m01_couplers_AWADDR;
  wire [1:0]m01_regslice_to_m01_couplers_AWBURST;
  wire [3:0]m01_regslice_to_m01_couplers_AWCACHE;
  wire [5:0]m01_regslice_to_m01_couplers_AWID;
  wire [7:0]m01_regslice_to_m01_couplers_AWLEN;
  wire [0:0]m01_regslice_to_m01_couplers_AWLOCK;
  wire [2:0]m01_regslice_to_m01_couplers_AWPROT;
  wire [3:0]m01_regslice_to_m01_couplers_AWQOS;
  wire m01_regslice_to_m01_couplers_AWREADY;
  wire [3:0]m01_regslice_to_m01_couplers_AWREGION;
  wire [2:0]m01_regslice_to_m01_couplers_AWSIZE;
  wire m01_regslice_to_m01_couplers_AWVALID;
  wire [5:0]m01_regslice_to_m01_couplers_BID;
  wire m01_regslice_to_m01_couplers_BREADY;
  wire [1:0]m01_regslice_to_m01_couplers_BRESP;
  wire m01_regslice_to_m01_couplers_BVALID;
  wire [511:0]m01_regslice_to_m01_couplers_RDATA;
  wire [5:0]m01_regslice_to_m01_couplers_RID;
  wire m01_regslice_to_m01_couplers_RLAST;
  wire m01_regslice_to_m01_couplers_RREADY;
  wire [1:0]m01_regslice_to_m01_couplers_RRESP;
  wire m01_regslice_to_m01_couplers_RVALID;
  wire [511:0]m01_regslice_to_m01_couplers_WDATA;
  wire m01_regslice_to_m01_couplers_WLAST;
  wire m01_regslice_to_m01_couplers_WREADY;
  wire [63:0]m01_regslice_to_m01_couplers_WSTRB;
  wire m01_regslice_to_m01_couplers_WVALID;

  assign M_ACLK_1 = M_ACLK;
  assign M_ARESETN_1 = M_ARESETN;
  assign M_AXI_araddr[63:0] = m01_regslice_to_m01_couplers_ARADDR;
  assign M_AXI_arburst[1:0] = m01_regslice_to_m01_couplers_ARBURST;
  assign M_AXI_arcache[3:0] = m01_regslice_to_m01_couplers_ARCACHE;
  assign M_AXI_arid[5:0] = m01_regslice_to_m01_couplers_ARID;
  assign M_AXI_arlen[7:0] = m01_regslice_to_m01_couplers_ARLEN;
  assign M_AXI_arlock[0] = m01_regslice_to_m01_couplers_ARLOCK;
  assign M_AXI_arprot[2:0] = m01_regslice_to_m01_couplers_ARPROT;
  assign M_AXI_arqos[3:0] = m01_regslice_to_m01_couplers_ARQOS;
  assign M_AXI_arregion[3:0] = m01_regslice_to_m01_couplers_ARREGION;
  assign M_AXI_arsize[2:0] = m01_regslice_to_m01_couplers_ARSIZE;
  assign M_AXI_arvalid = m01_regslice_to_m01_couplers_ARVALID;
  assign M_AXI_awaddr[63:0] = m01_regslice_to_m01_couplers_AWADDR;
  assign M_AXI_awburst[1:0] = m01_regslice_to_m01_couplers_AWBURST;
  assign M_AXI_awcache[3:0] = m01_regslice_to_m01_couplers_AWCACHE;
  assign M_AXI_awid[5:0] = m01_regslice_to_m01_couplers_AWID;
  assign M_AXI_awlen[7:0] = m01_regslice_to_m01_couplers_AWLEN;
  assign M_AXI_awlock[0] = m01_regslice_to_m01_couplers_AWLOCK;
  assign M_AXI_awprot[2:0] = m01_regslice_to_m01_couplers_AWPROT;
  assign M_AXI_awqos[3:0] = m01_regslice_to_m01_couplers_AWQOS;
  assign M_AXI_awregion[3:0] = m01_regslice_to_m01_couplers_AWREGION;
  assign M_AXI_awsize[2:0] = m01_regslice_to_m01_couplers_AWSIZE;
  assign M_AXI_awvalid = m01_regslice_to_m01_couplers_AWVALID;
  assign M_AXI_bready = m01_regslice_to_m01_couplers_BREADY;
  assign M_AXI_rready = m01_regslice_to_m01_couplers_RREADY;
  assign M_AXI_wdata[511:0] = m01_regslice_to_m01_couplers_WDATA;
  assign M_AXI_wlast = m01_regslice_to_m01_couplers_WLAST;
  assign M_AXI_wstrb[63:0] = m01_regslice_to_m01_couplers_WSTRB;
  assign M_AXI_wvalid = m01_regslice_to_m01_couplers_WVALID;
  assign S_AXI_arready = m01_couplers_to_m01_regslice_ARREADY;
  assign S_AXI_awready = m01_couplers_to_m01_regslice_AWREADY;
  assign S_AXI_bid[5:0] = m01_couplers_to_m01_regslice_BID;
  assign S_AXI_bresp[1:0] = m01_couplers_to_m01_regslice_BRESP;
  assign S_AXI_bvalid = m01_couplers_to_m01_regslice_BVALID;
  assign S_AXI_rdata[511:0] = m01_couplers_to_m01_regslice_RDATA;
  assign S_AXI_rid[5:0] = m01_couplers_to_m01_regslice_RID;
  assign S_AXI_rlast = m01_couplers_to_m01_regslice_RLAST;
  assign S_AXI_rresp[1:0] = m01_couplers_to_m01_regslice_RRESP;
  assign S_AXI_rvalid = m01_couplers_to_m01_regslice_RVALID;
  assign S_AXI_wready = m01_couplers_to_m01_regslice_WREADY;
  assign m01_couplers_to_m01_regslice_ARADDR = S_AXI_araddr[63:0];
  assign m01_couplers_to_m01_regslice_ARBURST = S_AXI_arburst[1:0];
  assign m01_couplers_to_m01_regslice_ARCACHE = S_AXI_arcache[3:0];
  assign m01_couplers_to_m01_regslice_ARID = S_AXI_arid[5:0];
  assign m01_couplers_to_m01_regslice_ARLEN = S_AXI_arlen[7:0];
  assign m01_couplers_to_m01_regslice_ARLOCK = S_AXI_arlock[0];
  assign m01_couplers_to_m01_regslice_ARPROT = S_AXI_arprot[2:0];
  assign m01_couplers_to_m01_regslice_ARQOS = S_AXI_arqos[3:0];
  assign m01_couplers_to_m01_regslice_ARREGION = S_AXI_arregion[3:0];
  assign m01_couplers_to_m01_regslice_ARSIZE = S_AXI_arsize[2:0];
  assign m01_couplers_to_m01_regslice_ARVALID = S_AXI_arvalid;
  assign m01_couplers_to_m01_regslice_AWADDR = S_AXI_awaddr[63:0];
  assign m01_couplers_to_m01_regslice_AWBURST = S_AXI_awburst[1:0];
  assign m01_couplers_to_m01_regslice_AWCACHE = S_AXI_awcache[3:0];
  assign m01_couplers_to_m01_regslice_AWID = S_AXI_awid[5:0];
  assign m01_couplers_to_m01_regslice_AWLEN = S_AXI_awlen[7:0];
  assign m01_couplers_to_m01_regslice_AWLOCK = S_AXI_awlock[0];
  assign m01_couplers_to_m01_regslice_AWPROT = S_AXI_awprot[2:0];
  assign m01_couplers_to_m01_regslice_AWQOS = S_AXI_awqos[3:0];
  assign m01_couplers_to_m01_regslice_AWREGION = S_AXI_awregion[3:0];
  assign m01_couplers_to_m01_regslice_AWSIZE = S_AXI_awsize[2:0];
  assign m01_couplers_to_m01_regslice_AWVALID = S_AXI_awvalid;
  assign m01_couplers_to_m01_regslice_BREADY = S_AXI_bready;
  assign m01_couplers_to_m01_regslice_RREADY = S_AXI_rready;
  assign m01_couplers_to_m01_regslice_WDATA = S_AXI_wdata[511:0];
  assign m01_couplers_to_m01_regslice_WLAST = S_AXI_wlast;
  assign m01_couplers_to_m01_regslice_WSTRB = S_AXI_wstrb[63:0];
  assign m01_couplers_to_m01_regslice_WVALID = S_AXI_wvalid;
  assign m01_regslice_to_m01_couplers_ARREADY = M_AXI_arready;
  assign m01_regslice_to_m01_couplers_AWREADY = M_AXI_awready;
  assign m01_regslice_to_m01_couplers_BID = M_AXI_bid[5:0];
  assign m01_regslice_to_m01_couplers_BRESP = M_AXI_bresp[1:0];
  assign m01_regslice_to_m01_couplers_BVALID = M_AXI_bvalid;
  assign m01_regslice_to_m01_couplers_RDATA = M_AXI_rdata[511:0];
  assign m01_regslice_to_m01_couplers_RID = M_AXI_rid[5:0];
  assign m01_regslice_to_m01_couplers_RLAST = M_AXI_rlast;
  assign m01_regslice_to_m01_couplers_RRESP = M_AXI_rresp[1:0];
  assign m01_regslice_to_m01_couplers_RVALID = M_AXI_rvalid;
  assign m01_regslice_to_m01_couplers_WREADY = M_AXI_wready;
  cl_xbar_m01_regslice_2 m01_regslice
       (.aclk(M_ACLK_1),
        .aresetn(M_ARESETN_1),
        .m_axi_araddr(m01_regslice_to_m01_couplers_ARADDR),
        .m_axi_arburst(m01_regslice_to_m01_couplers_ARBURST),
        .m_axi_arcache(m01_regslice_to_m01_couplers_ARCACHE),
        .m_axi_arid(m01_regslice_to_m01_couplers_ARID),
        .m_axi_arlen(m01_regslice_to_m01_couplers_ARLEN),
        .m_axi_arlock(m01_regslice_to_m01_couplers_ARLOCK),
        .m_axi_arprot(m01_regslice_to_m01_couplers_ARPROT),
        .m_axi_arqos(m01_regslice_to_m01_couplers_ARQOS),
        .m_axi_arready(m01_regslice_to_m01_couplers_ARREADY),
        .m_axi_arregion(m01_regslice_to_m01_couplers_ARREGION),
        .m_axi_arsize(m01_regslice_to_m01_couplers_ARSIZE),
        .m_axi_arvalid(m01_regslice_to_m01_couplers_ARVALID),
        .m_axi_awaddr(m01_regslice_to_m01_couplers_AWADDR),
        .m_axi_awburst(m01_regslice_to_m01_couplers_AWBURST),
        .m_axi_awcache(m01_regslice_to_m01_couplers_AWCACHE),
        .m_axi_awid(m01_regslice_to_m01_couplers_AWID),
        .m_axi_awlen(m01_regslice_to_m01_couplers_AWLEN),
        .m_axi_awlock(m01_regslice_to_m01_couplers_AWLOCK),
        .m_axi_awprot(m01_regslice_to_m01_couplers_AWPROT),
        .m_axi_awqos(m01_regslice_to_m01_couplers_AWQOS),
        .m_axi_awready(m01_regslice_to_m01_couplers_AWREADY),
        .m_axi_awregion(m01_regslice_to_m01_couplers_AWREGION),
        .m_axi_awsize(m01_regslice_to_m01_couplers_AWSIZE),
        .m_axi_awvalid(m01_regslice_to_m01_couplers_AWVALID),
        .m_axi_bid(m01_regslice_to_m01_couplers_BID),
        .m_axi_bready(m01_regslice_to_m01_couplers_BREADY),
        .m_axi_bresp(m01_regslice_to_m01_couplers_BRESP),
        .m_axi_bvalid(m01_regslice_to_m01_couplers_BVALID),
        .m_axi_rdata(m01_regslice_to_m01_couplers_RDATA),
        .m_axi_rid(m01_regslice_to_m01_couplers_RID),
        .m_axi_rlast(m01_regslice_to_m01_couplers_RLAST),
        .m_axi_rready(m01_regslice_to_m01_couplers_RREADY),
        .m_axi_rresp(m01_regslice_to_m01_couplers_RRESP),
        .m_axi_rvalid(m01_regslice_to_m01_couplers_RVALID),
        .m_axi_wdata(m01_regslice_to_m01_couplers_WDATA),
        .m_axi_wlast(m01_regslice_to_m01_couplers_WLAST),
        .m_axi_wready(m01_regslice_to_m01_couplers_WREADY),
        .m_axi_wstrb(m01_regslice_to_m01_couplers_WSTRB),
        .m_axi_wvalid(m01_regslice_to_m01_couplers_WVALID),
        .s_axi_araddr(m01_couplers_to_m01_regslice_ARADDR),
        .s_axi_arburst(m01_couplers_to_m01_regslice_ARBURST),
        .s_axi_arcache(m01_couplers_to_m01_regslice_ARCACHE),
        .s_axi_arid(m01_couplers_to_m01_regslice_ARID),
        .s_axi_arlen(m01_couplers_to_m01_regslice_ARLEN),
        .s_axi_arlock(m01_couplers_to_m01_regslice_ARLOCK),
        .s_axi_arprot(m01_couplers_to_m01_regslice_ARPROT),
        .s_axi_arqos(m01_couplers_to_m01_regslice_ARQOS),
        .s_axi_arready(m01_couplers_to_m01_regslice_ARREADY),
        .s_axi_arregion(m01_couplers_to_m01_regslice_ARREGION),
        .s_axi_arsize(m01_couplers_to_m01_regslice_ARSIZE),
        .s_axi_arvalid(m01_couplers_to_m01_regslice_ARVALID),
        .s_axi_awaddr(m01_couplers_to_m01_regslice_AWADDR),
        .s_axi_awburst(m01_couplers_to_m01_regslice_AWBURST),
        .s_axi_awcache(m01_couplers_to_m01_regslice_AWCACHE),
        .s_axi_awid(m01_couplers_to_m01_regslice_AWID),
        .s_axi_awlen(m01_couplers_to_m01_regslice_AWLEN),
        .s_axi_awlock(m01_couplers_to_m01_regslice_AWLOCK),
        .s_axi_awprot(m01_couplers_to_m01_regslice_AWPROT),
        .s_axi_awqos(m01_couplers_to_m01_regslice_AWQOS),
        .s_axi_awready(m01_couplers_to_m01_regslice_AWREADY),
        .s_axi_awregion(m01_couplers_to_m01_regslice_AWREGION),
        .s_axi_awsize(m01_couplers_to_m01_regslice_AWSIZE),
        .s_axi_awvalid(m01_couplers_to_m01_regslice_AWVALID),
        .s_axi_bid(m01_couplers_to_m01_regslice_BID),
        .s_axi_bready(m01_couplers_to_m01_regslice_BREADY),
        .s_axi_bresp(m01_couplers_to_m01_regslice_BRESP),
        .s_axi_bvalid(m01_couplers_to_m01_regslice_BVALID),
        .s_axi_rdata(m01_couplers_to_m01_regslice_RDATA),
        .s_axi_rid(m01_couplers_to_m01_regslice_RID),
        .s_axi_rlast(m01_couplers_to_m01_regslice_RLAST),
        .s_axi_rready(m01_couplers_to_m01_regslice_RREADY),
        .s_axi_rresp(m01_couplers_to_m01_regslice_RRESP),
        .s_axi_rvalid(m01_couplers_to_m01_regslice_RVALID),
        .s_axi_wdata(m01_couplers_to_m01_regslice_WDATA),
        .s_axi_wlast(m01_couplers_to_m01_regslice_WLAST),
        .s_axi_wready(m01_couplers_to_m01_regslice_WREADY),
        .s_axi_wstrb(m01_couplers_to_m01_regslice_WSTRB),
        .s_axi_wvalid(m01_couplers_to_m01_regslice_WVALID));
endmodule

module m02_couplers_imp_14A9NST
   (M_ACLK,
    M_ARESETN,
    M_AXI_araddr,
    M_AXI_arburst,
    M_AXI_arcache,
    M_AXI_arid,
    M_AXI_arlen,
    M_AXI_arlock,
    M_AXI_arprot,
    M_AXI_arqos,
    M_AXI_arready,
    M_AXI_arregion,
    M_AXI_arsize,
    M_AXI_arvalid,
    M_AXI_awaddr,
    M_AXI_awburst,
    M_AXI_awcache,
    M_AXI_awid,
    M_AXI_awlen,
    M_AXI_awlock,
    M_AXI_awprot,
    M_AXI_awqos,
    M_AXI_awready,
    M_AXI_awregion,
    M_AXI_awsize,
    M_AXI_awvalid,
    M_AXI_bid,
    M_AXI_bready,
    M_AXI_bresp,
    M_AXI_bvalid,
    M_AXI_rdata,
    M_AXI_rid,
    M_AXI_rlast,
    M_AXI_rready,
    M_AXI_rresp,
    M_AXI_rvalid,
    M_AXI_wdata,
    M_AXI_wlast,
    M_AXI_wready,
    M_AXI_wstrb,
    M_AXI_wvalid,
    S_ACLK,
    S_ARESETN,
    S_AXI_araddr,
    S_AXI_arburst,
    S_AXI_arcache,
    S_AXI_arid,
    S_AXI_arlen,
    S_AXI_arlock,
    S_AXI_arprot,
    S_AXI_arqos,
    S_AXI_arready,
    S_AXI_arregion,
    S_AXI_arsize,
    S_AXI_arvalid,
    S_AXI_awaddr,
    S_AXI_awburst,
    S_AXI_awcache,
    S_AXI_awid,
    S_AXI_awlen,
    S_AXI_awlock,
    S_AXI_awprot,
    S_AXI_awqos,
    S_AXI_awready,
    S_AXI_awregion,
    S_AXI_awsize,
    S_AXI_awvalid,
    S_AXI_bid,
    S_AXI_bready,
    S_AXI_bresp,
    S_AXI_bvalid,
    S_AXI_rdata,
    S_AXI_rid,
    S_AXI_rlast,
    S_AXI_rready,
    S_AXI_rresp,
    S_AXI_rvalid,
    S_AXI_wdata,
    S_AXI_wlast,
    S_AXI_wready,
    S_AXI_wstrb,
    S_AXI_wvalid);
  input M_ACLK;
  input M_ARESETN;
  output [63:0]M_AXI_araddr;
  output [1:0]M_AXI_arburst;
  output [3:0]M_AXI_arcache;
  output [6:0]M_AXI_arid;
  output [7:0]M_AXI_arlen;
  output [0:0]M_AXI_arlock;
  output [2:0]M_AXI_arprot;
  output [3:0]M_AXI_arqos;
  input M_AXI_arready;
  output [3:0]M_AXI_arregion;
  output [2:0]M_AXI_arsize;
  output M_AXI_arvalid;
  output [63:0]M_AXI_awaddr;
  output [1:0]M_AXI_awburst;
  output [3:0]M_AXI_awcache;
  output [6:0]M_AXI_awid;
  output [7:0]M_AXI_awlen;
  output [0:0]M_AXI_awlock;
  output [2:0]M_AXI_awprot;
  output [3:0]M_AXI_awqos;
  input M_AXI_awready;
  output [3:0]M_AXI_awregion;
  output [2:0]M_AXI_awsize;
  output M_AXI_awvalid;
  input [6:0]M_AXI_bid;
  output M_AXI_bready;
  input [1:0]M_AXI_bresp;
  input M_AXI_bvalid;
  input [511:0]M_AXI_rdata;
  input [6:0]M_AXI_rid;
  input M_AXI_rlast;
  output M_AXI_rready;
  input [1:0]M_AXI_rresp;
  input M_AXI_rvalid;
  output [511:0]M_AXI_wdata;
  output M_AXI_wlast;
  input M_AXI_wready;
  output [63:0]M_AXI_wstrb;
  output M_AXI_wvalid;
  input S_ACLK;
  input S_ARESETN;
  input [63:0]S_AXI_araddr;
  input [1:0]S_AXI_arburst;
  input [3:0]S_AXI_arcache;
  input [6:0]S_AXI_arid;
  input [7:0]S_AXI_arlen;
  input [0:0]S_AXI_arlock;
  input [2:0]S_AXI_arprot;
  input [3:0]S_AXI_arqos;
  output S_AXI_arready;
  input [3:0]S_AXI_arregion;
  input [2:0]S_AXI_arsize;
  input S_AXI_arvalid;
  input [63:0]S_AXI_awaddr;
  input [1:0]S_AXI_awburst;
  input [3:0]S_AXI_awcache;
  input [6:0]S_AXI_awid;
  input [7:0]S_AXI_awlen;
  input [0:0]S_AXI_awlock;
  input [2:0]S_AXI_awprot;
  input [3:0]S_AXI_awqos;
  output S_AXI_awready;
  input [3:0]S_AXI_awregion;
  input [2:0]S_AXI_awsize;
  input S_AXI_awvalid;
  output [6:0]S_AXI_bid;
  input S_AXI_bready;
  output [1:0]S_AXI_bresp;
  output S_AXI_bvalid;
  output [511:0]S_AXI_rdata;
  output [6:0]S_AXI_rid;
  output S_AXI_rlast;
  input S_AXI_rready;
  output [1:0]S_AXI_rresp;
  output S_AXI_rvalid;
  input [511:0]S_AXI_wdata;
  input S_AXI_wlast;
  output S_AXI_wready;
  input [63:0]S_AXI_wstrb;
  input S_AXI_wvalid;

  wire M_ACLK_1;
  wire M_ARESETN_1;
  wire [63:0]m02_couplers_to_m02_regslice_ARADDR;
  wire [1:0]m02_couplers_to_m02_regslice_ARBURST;
  wire [3:0]m02_couplers_to_m02_regslice_ARCACHE;
  wire [6:0]m02_couplers_to_m02_regslice_ARID;
  wire [7:0]m02_couplers_to_m02_regslice_ARLEN;
  wire [0:0]m02_couplers_to_m02_regslice_ARLOCK;
  wire [2:0]m02_couplers_to_m02_regslice_ARPROT;
  wire [3:0]m02_couplers_to_m02_regslice_ARQOS;
  wire m02_couplers_to_m02_regslice_ARREADY;
  wire [3:0]m02_couplers_to_m02_regslice_ARREGION;
  wire [2:0]m02_couplers_to_m02_regslice_ARSIZE;
  wire m02_couplers_to_m02_regslice_ARVALID;
  wire [63:0]m02_couplers_to_m02_regslice_AWADDR;
  wire [1:0]m02_couplers_to_m02_regslice_AWBURST;
  wire [3:0]m02_couplers_to_m02_regslice_AWCACHE;
  wire [6:0]m02_couplers_to_m02_regslice_AWID;
  wire [7:0]m02_couplers_to_m02_regslice_AWLEN;
  wire [0:0]m02_couplers_to_m02_regslice_AWLOCK;
  wire [2:0]m02_couplers_to_m02_regslice_AWPROT;
  wire [3:0]m02_couplers_to_m02_regslice_AWQOS;
  wire m02_couplers_to_m02_regslice_AWREADY;
  wire [3:0]m02_couplers_to_m02_regslice_AWREGION;
  wire [2:0]m02_couplers_to_m02_regslice_AWSIZE;
  wire m02_couplers_to_m02_regslice_AWVALID;
  wire [6:0]m02_couplers_to_m02_regslice_BID;
  wire m02_couplers_to_m02_regslice_BREADY;
  wire [1:0]m02_couplers_to_m02_regslice_BRESP;
  wire m02_couplers_to_m02_regslice_BVALID;
  wire [511:0]m02_couplers_to_m02_regslice_RDATA;
  wire [6:0]m02_couplers_to_m02_regslice_RID;
  wire m02_couplers_to_m02_regslice_RLAST;
  wire m02_couplers_to_m02_regslice_RREADY;
  wire [1:0]m02_couplers_to_m02_regslice_RRESP;
  wire m02_couplers_to_m02_regslice_RVALID;
  wire [511:0]m02_couplers_to_m02_regslice_WDATA;
  wire m02_couplers_to_m02_regslice_WLAST;
  wire m02_couplers_to_m02_regslice_WREADY;
  wire [63:0]m02_couplers_to_m02_regslice_WSTRB;
  wire m02_couplers_to_m02_regslice_WVALID;
  wire [63:0]m02_regslice_to_m02_couplers_ARADDR;
  wire [1:0]m02_regslice_to_m02_couplers_ARBURST;
  wire [3:0]m02_regslice_to_m02_couplers_ARCACHE;
  wire [6:0]m02_regslice_to_m02_couplers_ARID;
  wire [7:0]m02_regslice_to_m02_couplers_ARLEN;
  wire [0:0]m02_regslice_to_m02_couplers_ARLOCK;
  wire [2:0]m02_regslice_to_m02_couplers_ARPROT;
  wire [3:0]m02_regslice_to_m02_couplers_ARQOS;
  wire m02_regslice_to_m02_couplers_ARREADY;
  wire [3:0]m02_regslice_to_m02_couplers_ARREGION;
  wire [2:0]m02_regslice_to_m02_couplers_ARSIZE;
  wire m02_regslice_to_m02_couplers_ARVALID;
  wire [63:0]m02_regslice_to_m02_couplers_AWADDR;
  wire [1:0]m02_regslice_to_m02_couplers_AWBURST;
  wire [3:0]m02_regslice_to_m02_couplers_AWCACHE;
  wire [6:0]m02_regslice_to_m02_couplers_AWID;
  wire [7:0]m02_regslice_to_m02_couplers_AWLEN;
  wire [0:0]m02_regslice_to_m02_couplers_AWLOCK;
  wire [2:0]m02_regslice_to_m02_couplers_AWPROT;
  wire [3:0]m02_regslice_to_m02_couplers_AWQOS;
  wire m02_regslice_to_m02_couplers_AWREADY;
  wire [3:0]m02_regslice_to_m02_couplers_AWREGION;
  wire [2:0]m02_regslice_to_m02_couplers_AWSIZE;
  wire m02_regslice_to_m02_couplers_AWVALID;
  wire [6:0]m02_regslice_to_m02_couplers_BID;
  wire m02_regslice_to_m02_couplers_BREADY;
  wire [1:0]m02_regslice_to_m02_couplers_BRESP;
  wire m02_regslice_to_m02_couplers_BVALID;
  wire [511:0]m02_regslice_to_m02_couplers_RDATA;
  wire [6:0]m02_regslice_to_m02_couplers_RID;
  wire m02_regslice_to_m02_couplers_RLAST;
  wire m02_regslice_to_m02_couplers_RREADY;
  wire [1:0]m02_regslice_to_m02_couplers_RRESP;
  wire m02_regslice_to_m02_couplers_RVALID;
  wire [511:0]m02_regslice_to_m02_couplers_WDATA;
  wire m02_regslice_to_m02_couplers_WLAST;
  wire m02_regslice_to_m02_couplers_WREADY;
  wire [63:0]m02_regslice_to_m02_couplers_WSTRB;
  wire m02_regslice_to_m02_couplers_WVALID;

  assign M_ACLK_1 = M_ACLK;
  assign M_ARESETN_1 = M_ARESETN;
  assign M_AXI_araddr[63:0] = m02_regslice_to_m02_couplers_ARADDR;
  assign M_AXI_arburst[1:0] = m02_regslice_to_m02_couplers_ARBURST;
  assign M_AXI_arcache[3:0] = m02_regslice_to_m02_couplers_ARCACHE;
  assign M_AXI_arid[6:0] = m02_regslice_to_m02_couplers_ARID;
  assign M_AXI_arlen[7:0] = m02_regslice_to_m02_couplers_ARLEN;
  assign M_AXI_arlock[0] = m02_regslice_to_m02_couplers_ARLOCK;
  assign M_AXI_arprot[2:0] = m02_regslice_to_m02_couplers_ARPROT;
  assign M_AXI_arqos[3:0] = m02_regslice_to_m02_couplers_ARQOS;
  assign M_AXI_arregion[3:0] = m02_regslice_to_m02_couplers_ARREGION;
  assign M_AXI_arsize[2:0] = m02_regslice_to_m02_couplers_ARSIZE;
  assign M_AXI_arvalid = m02_regslice_to_m02_couplers_ARVALID;
  assign M_AXI_awaddr[63:0] = m02_regslice_to_m02_couplers_AWADDR;
  assign M_AXI_awburst[1:0] = m02_regslice_to_m02_couplers_AWBURST;
  assign M_AXI_awcache[3:0] = m02_regslice_to_m02_couplers_AWCACHE;
  assign M_AXI_awid[6:0] = m02_regslice_to_m02_couplers_AWID;
  assign M_AXI_awlen[7:0] = m02_regslice_to_m02_couplers_AWLEN;
  assign M_AXI_awlock[0] = m02_regslice_to_m02_couplers_AWLOCK;
  assign M_AXI_awprot[2:0] = m02_regslice_to_m02_couplers_AWPROT;
  assign M_AXI_awqos[3:0] = m02_regslice_to_m02_couplers_AWQOS;
  assign M_AXI_awregion[3:0] = m02_regslice_to_m02_couplers_AWREGION;
  assign M_AXI_awsize[2:0] = m02_regslice_to_m02_couplers_AWSIZE;
  assign M_AXI_awvalid = m02_regslice_to_m02_couplers_AWVALID;
  assign M_AXI_bready = m02_regslice_to_m02_couplers_BREADY;
  assign M_AXI_rready = m02_regslice_to_m02_couplers_RREADY;
  assign M_AXI_wdata[511:0] = m02_regslice_to_m02_couplers_WDATA;
  assign M_AXI_wlast = m02_regslice_to_m02_couplers_WLAST;
  assign M_AXI_wstrb[63:0] = m02_regslice_to_m02_couplers_WSTRB;
  assign M_AXI_wvalid = m02_regslice_to_m02_couplers_WVALID;
  assign S_AXI_arready = m02_couplers_to_m02_regslice_ARREADY;
  assign S_AXI_awready = m02_couplers_to_m02_regslice_AWREADY;
  assign S_AXI_bid[6:0] = m02_couplers_to_m02_regslice_BID;
  assign S_AXI_bresp[1:0] = m02_couplers_to_m02_regslice_BRESP;
  assign S_AXI_bvalid = m02_couplers_to_m02_regslice_BVALID;
  assign S_AXI_rdata[511:0] = m02_couplers_to_m02_regslice_RDATA;
  assign S_AXI_rid[6:0] = m02_couplers_to_m02_regslice_RID;
  assign S_AXI_rlast = m02_couplers_to_m02_regslice_RLAST;
  assign S_AXI_rresp[1:0] = m02_couplers_to_m02_regslice_RRESP;
  assign S_AXI_rvalid = m02_couplers_to_m02_regslice_RVALID;
  assign S_AXI_wready = m02_couplers_to_m02_regslice_WREADY;
  assign m02_couplers_to_m02_regslice_ARADDR = S_AXI_araddr[63:0];
  assign m02_couplers_to_m02_regslice_ARBURST = S_AXI_arburst[1:0];
  assign m02_couplers_to_m02_regslice_ARCACHE = S_AXI_arcache[3:0];
  assign m02_couplers_to_m02_regslice_ARID = S_AXI_arid[6:0];
  assign m02_couplers_to_m02_regslice_ARLEN = S_AXI_arlen[7:0];
  assign m02_couplers_to_m02_regslice_ARLOCK = S_AXI_arlock[0];
  assign m02_couplers_to_m02_regslice_ARPROT = S_AXI_arprot[2:0];
  assign m02_couplers_to_m02_regslice_ARQOS = S_AXI_arqos[3:0];
  assign m02_couplers_to_m02_regslice_ARREGION = S_AXI_arregion[3:0];
  assign m02_couplers_to_m02_regslice_ARSIZE = S_AXI_arsize[2:0];
  assign m02_couplers_to_m02_regslice_ARVALID = S_AXI_arvalid;
  assign m02_couplers_to_m02_regslice_AWADDR = S_AXI_awaddr[63:0];
  assign m02_couplers_to_m02_regslice_AWBURST = S_AXI_awburst[1:0];
  assign m02_couplers_to_m02_regslice_AWCACHE = S_AXI_awcache[3:0];
  assign m02_couplers_to_m02_regslice_AWID = S_AXI_awid[6:0];
  assign m02_couplers_to_m02_regslice_AWLEN = S_AXI_awlen[7:0];
  assign m02_couplers_to_m02_regslice_AWLOCK = S_AXI_awlock[0];
  assign m02_couplers_to_m02_regslice_AWPROT = S_AXI_awprot[2:0];
  assign m02_couplers_to_m02_regslice_AWQOS = S_AXI_awqos[3:0];
  assign m02_couplers_to_m02_regslice_AWREGION = S_AXI_awregion[3:0];
  assign m02_couplers_to_m02_regslice_AWSIZE = S_AXI_awsize[2:0];
  assign m02_couplers_to_m02_regslice_AWVALID = S_AXI_awvalid;
  assign m02_couplers_to_m02_regslice_BREADY = S_AXI_bready;
  assign m02_couplers_to_m02_regslice_RREADY = S_AXI_rready;
  assign m02_couplers_to_m02_regslice_WDATA = S_AXI_wdata[511:0];
  assign m02_couplers_to_m02_regslice_WLAST = S_AXI_wlast;
  assign m02_couplers_to_m02_regslice_WSTRB = S_AXI_wstrb[63:0];
  assign m02_couplers_to_m02_regslice_WVALID = S_AXI_wvalid;
  assign m02_regslice_to_m02_couplers_ARREADY = M_AXI_arready;
  assign m02_regslice_to_m02_couplers_AWREADY = M_AXI_awready;
  assign m02_regslice_to_m02_couplers_BID = M_AXI_bid[6:0];
  assign m02_regslice_to_m02_couplers_BRESP = M_AXI_bresp[1:0];
  assign m02_regslice_to_m02_couplers_BVALID = M_AXI_bvalid;
  assign m02_regslice_to_m02_couplers_RDATA = M_AXI_rdata[511:0];
  assign m02_regslice_to_m02_couplers_RID = M_AXI_rid[6:0];
  assign m02_regslice_to_m02_couplers_RLAST = M_AXI_rlast;
  assign m02_regslice_to_m02_couplers_RRESP = M_AXI_rresp[1:0];
  assign m02_regslice_to_m02_couplers_RVALID = M_AXI_rvalid;
  assign m02_regslice_to_m02_couplers_WREADY = M_AXI_wready;
  cl_xbar_m02_regslice_0 m02_regslice
       (.aclk(M_ACLK_1),
        .aresetn(M_ARESETN_1),
        .m_axi_araddr(m02_regslice_to_m02_couplers_ARADDR),
        .m_axi_arburst(m02_regslice_to_m02_couplers_ARBURST),
        .m_axi_arcache(m02_regslice_to_m02_couplers_ARCACHE),
        .m_axi_arid(m02_regslice_to_m02_couplers_ARID),
        .m_axi_arlen(m02_regslice_to_m02_couplers_ARLEN),
        .m_axi_arlock(m02_regslice_to_m02_couplers_ARLOCK),
        .m_axi_arprot(m02_regslice_to_m02_couplers_ARPROT),
        .m_axi_arqos(m02_regslice_to_m02_couplers_ARQOS),
        .m_axi_arready(m02_regslice_to_m02_couplers_ARREADY),
        .m_axi_arregion(m02_regslice_to_m02_couplers_ARREGION),
        .m_axi_arsize(m02_regslice_to_m02_couplers_ARSIZE),
        .m_axi_arvalid(m02_regslice_to_m02_couplers_ARVALID),
        .m_axi_awaddr(m02_regslice_to_m02_couplers_AWADDR),
        .m_axi_awburst(m02_regslice_to_m02_couplers_AWBURST),
        .m_axi_awcache(m02_regslice_to_m02_couplers_AWCACHE),
        .m_axi_awid(m02_regslice_to_m02_couplers_AWID),
        .m_axi_awlen(m02_regslice_to_m02_couplers_AWLEN),
        .m_axi_awlock(m02_regslice_to_m02_couplers_AWLOCK),
        .m_axi_awprot(m02_regslice_to_m02_couplers_AWPROT),
        .m_axi_awqos(m02_regslice_to_m02_couplers_AWQOS),
        .m_axi_awready(m02_regslice_to_m02_couplers_AWREADY),
        .m_axi_awregion(m02_regslice_to_m02_couplers_AWREGION),
        .m_axi_awsize(m02_regslice_to_m02_couplers_AWSIZE),
        .m_axi_awvalid(m02_regslice_to_m02_couplers_AWVALID),
        .m_axi_bid(m02_regslice_to_m02_couplers_BID),
        .m_axi_bready(m02_regslice_to_m02_couplers_BREADY),
        .m_axi_bresp(m02_regslice_to_m02_couplers_BRESP),
        .m_axi_bvalid(m02_regslice_to_m02_couplers_BVALID),
        .m_axi_rdata(m02_regslice_to_m02_couplers_RDATA),
        .m_axi_rid(m02_regslice_to_m02_couplers_RID),
        .m_axi_rlast(m02_regslice_to_m02_couplers_RLAST),
        .m_axi_rready(m02_regslice_to_m02_couplers_RREADY),
        .m_axi_rresp(m02_regslice_to_m02_couplers_RRESP),
        .m_axi_rvalid(m02_regslice_to_m02_couplers_RVALID),
        .m_axi_wdata(m02_regslice_to_m02_couplers_WDATA),
        .m_axi_wlast(m02_regslice_to_m02_couplers_WLAST),
        .m_axi_wready(m02_regslice_to_m02_couplers_WREADY),
        .m_axi_wstrb(m02_regslice_to_m02_couplers_WSTRB),
        .m_axi_wvalid(m02_regslice_to_m02_couplers_WVALID),
        .s_axi_araddr(m02_couplers_to_m02_regslice_ARADDR),
        .s_axi_arburst(m02_couplers_to_m02_regslice_ARBURST),
        .s_axi_arcache(m02_couplers_to_m02_regslice_ARCACHE),
        .s_axi_arid(m02_couplers_to_m02_regslice_ARID),
        .s_axi_arlen(m02_couplers_to_m02_regslice_ARLEN),
        .s_axi_arlock(m02_couplers_to_m02_regslice_ARLOCK),
        .s_axi_arprot(m02_couplers_to_m02_regslice_ARPROT),
        .s_axi_arqos(m02_couplers_to_m02_regslice_ARQOS),
        .s_axi_arready(m02_couplers_to_m02_regslice_ARREADY),
        .s_axi_arregion(m02_couplers_to_m02_regslice_ARREGION),
        .s_axi_arsize(m02_couplers_to_m02_regslice_ARSIZE),
        .s_axi_arvalid(m02_couplers_to_m02_regslice_ARVALID),
        .s_axi_awaddr(m02_couplers_to_m02_regslice_AWADDR),
        .s_axi_awburst(m02_couplers_to_m02_regslice_AWBURST),
        .s_axi_awcache(m02_couplers_to_m02_regslice_AWCACHE),
        .s_axi_awid(m02_couplers_to_m02_regslice_AWID),
        .s_axi_awlen(m02_couplers_to_m02_regslice_AWLEN),
        .s_axi_awlock(m02_couplers_to_m02_regslice_AWLOCK),
        .s_axi_awprot(m02_couplers_to_m02_regslice_AWPROT),
        .s_axi_awqos(m02_couplers_to_m02_regslice_AWQOS),
        .s_axi_awready(m02_couplers_to_m02_regslice_AWREADY),
        .s_axi_awregion(m02_couplers_to_m02_regslice_AWREGION),
        .s_axi_awsize(m02_couplers_to_m02_regslice_AWSIZE),
        .s_axi_awvalid(m02_couplers_to_m02_regslice_AWVALID),
        .s_axi_bid(m02_couplers_to_m02_regslice_BID),
        .s_axi_bready(m02_couplers_to_m02_regslice_BREADY),
        .s_axi_bresp(m02_couplers_to_m02_regslice_BRESP),
        .s_axi_bvalid(m02_couplers_to_m02_regslice_BVALID),
        .s_axi_rdata(m02_couplers_to_m02_regslice_RDATA),
        .s_axi_rid(m02_couplers_to_m02_regslice_RID),
        .s_axi_rlast(m02_couplers_to_m02_regslice_RLAST),
        .s_axi_rready(m02_couplers_to_m02_regslice_RREADY),
        .s_axi_rresp(m02_couplers_to_m02_regslice_RRESP),
        .s_axi_rvalid(m02_couplers_to_m02_regslice_RVALID),
        .s_axi_wdata(m02_couplers_to_m02_regslice_WDATA),
        .s_axi_wlast(m02_couplers_to_m02_regslice_WLAST),
        .s_axi_wready(m02_couplers_to_m02_regslice_WREADY),
        .s_axi_wstrb(m02_couplers_to_m02_regslice_WSTRB),
        .s_axi_wvalid(m02_couplers_to_m02_regslice_WVALID));
endmodule

module m03_couplers_imp_69LRZY
   (M_ACLK,
    M_ARESETN,
    M_AXI_araddr,
    M_AXI_arburst,
    M_AXI_arcache,
    M_AXI_arid,
    M_AXI_arlen,
    M_AXI_arlock,
    M_AXI_arprot,
    M_AXI_arqos,
    M_AXI_arready,
    M_AXI_arregion,
    M_AXI_arsize,
    M_AXI_arvalid,
    M_AXI_awaddr,
    M_AXI_awburst,
    M_AXI_awcache,
    M_AXI_awid,
    M_AXI_awlen,
    M_AXI_awlock,
    M_AXI_awprot,
    M_AXI_awqos,
    M_AXI_awready,
    M_AXI_awregion,
    M_AXI_awsize,
    M_AXI_awvalid,
    M_AXI_bid,
    M_AXI_bready,
    M_AXI_bresp,
    M_AXI_bvalid,
    M_AXI_rdata,
    M_AXI_rid,
    M_AXI_rlast,
    M_AXI_rready,
    M_AXI_rresp,
    M_AXI_rvalid,
    M_AXI_wdata,
    M_AXI_wlast,
    M_AXI_wready,
    M_AXI_wstrb,
    M_AXI_wvalid,
    S_ACLK,
    S_ARESETN,
    S_AXI_araddr,
    S_AXI_arburst,
    S_AXI_arcache,
    S_AXI_arid,
    S_AXI_arlen,
    S_AXI_arlock,
    S_AXI_arprot,
    S_AXI_arqos,
    S_AXI_arready,
    S_AXI_arregion,
    S_AXI_arsize,
    S_AXI_arvalid,
    S_AXI_awaddr,
    S_AXI_awburst,
    S_AXI_awcache,
    S_AXI_awid,
    S_AXI_awlen,
    S_AXI_awlock,
    S_AXI_awprot,
    S_AXI_awqos,
    S_AXI_awready,
    S_AXI_awregion,
    S_AXI_awsize,
    S_AXI_awvalid,
    S_AXI_bid,
    S_AXI_bready,
    S_AXI_bresp,
    S_AXI_bvalid,
    S_AXI_rdata,
    S_AXI_rid,
    S_AXI_rlast,
    S_AXI_rready,
    S_AXI_rresp,
    S_AXI_rvalid,
    S_AXI_wdata,
    S_AXI_wlast,
    S_AXI_wready,
    S_AXI_wstrb,
    S_AXI_wvalid);
  input M_ACLK;
  input M_ARESETN;
  output [63:0]M_AXI_araddr;
  output [1:0]M_AXI_arburst;
  output [3:0]M_AXI_arcache;
  output [6:0]M_AXI_arid;
  output [7:0]M_AXI_arlen;
  output [0:0]M_AXI_arlock;
  output [2:0]M_AXI_arprot;
  output [3:0]M_AXI_arqos;
  input M_AXI_arready;
  output [3:0]M_AXI_arregion;
  output [2:0]M_AXI_arsize;
  output M_AXI_arvalid;
  output [63:0]M_AXI_awaddr;
  output [1:0]M_AXI_awburst;
  output [3:0]M_AXI_awcache;
  output [6:0]M_AXI_awid;
  output [7:0]M_AXI_awlen;
  output [0:0]M_AXI_awlock;
  output [2:0]M_AXI_awprot;
  output [3:0]M_AXI_awqos;
  input M_AXI_awready;
  output [3:0]M_AXI_awregion;
  output [2:0]M_AXI_awsize;
  output M_AXI_awvalid;
  input [6:0]M_AXI_bid;
  output M_AXI_bready;
  input [1:0]M_AXI_bresp;
  input M_AXI_bvalid;
  input [511:0]M_AXI_rdata;
  input [6:0]M_AXI_rid;
  input M_AXI_rlast;
  output M_AXI_rready;
  input [1:0]M_AXI_rresp;
  input M_AXI_rvalid;
  output [511:0]M_AXI_wdata;
  output M_AXI_wlast;
  input M_AXI_wready;
  output [63:0]M_AXI_wstrb;
  output M_AXI_wvalid;
  input S_ACLK;
  input S_ARESETN;
  input [63:0]S_AXI_araddr;
  input [1:0]S_AXI_arburst;
  input [3:0]S_AXI_arcache;
  input [6:0]S_AXI_arid;
  input [7:0]S_AXI_arlen;
  input [0:0]S_AXI_arlock;
  input [2:0]S_AXI_arprot;
  input [3:0]S_AXI_arqos;
  output S_AXI_arready;
  input [3:0]S_AXI_arregion;
  input [2:0]S_AXI_arsize;
  input S_AXI_arvalid;
  input [63:0]S_AXI_awaddr;
  input [1:0]S_AXI_awburst;
  input [3:0]S_AXI_awcache;
  input [6:0]S_AXI_awid;
  input [7:0]S_AXI_awlen;
  input [0:0]S_AXI_awlock;
  input [2:0]S_AXI_awprot;
  input [3:0]S_AXI_awqos;
  output S_AXI_awready;
  input [3:0]S_AXI_awregion;
  input [2:0]S_AXI_awsize;
  input S_AXI_awvalid;
  output [6:0]S_AXI_bid;
  input S_AXI_bready;
  output [1:0]S_AXI_bresp;
  output S_AXI_bvalid;
  output [511:0]S_AXI_rdata;
  output [6:0]S_AXI_rid;
  output S_AXI_rlast;
  input S_AXI_rready;
  output [1:0]S_AXI_rresp;
  output S_AXI_rvalid;
  input [511:0]S_AXI_wdata;
  input S_AXI_wlast;
  output S_AXI_wready;
  input [63:0]S_AXI_wstrb;
  input S_AXI_wvalid;

  wire M_ACLK_1;
  wire M_ARESETN_1;
  wire [63:0]m03_couplers_to_m03_regslice_ARADDR;
  wire [1:0]m03_couplers_to_m03_regslice_ARBURST;
  wire [3:0]m03_couplers_to_m03_regslice_ARCACHE;
  wire [6:0]m03_couplers_to_m03_regslice_ARID;
  wire [7:0]m03_couplers_to_m03_regslice_ARLEN;
  wire [0:0]m03_couplers_to_m03_regslice_ARLOCK;
  wire [2:0]m03_couplers_to_m03_regslice_ARPROT;
  wire [3:0]m03_couplers_to_m03_regslice_ARQOS;
  wire m03_couplers_to_m03_regslice_ARREADY;
  wire [3:0]m03_couplers_to_m03_regslice_ARREGION;
  wire [2:0]m03_couplers_to_m03_regslice_ARSIZE;
  wire m03_couplers_to_m03_regslice_ARVALID;
  wire [63:0]m03_couplers_to_m03_regslice_AWADDR;
  wire [1:0]m03_couplers_to_m03_regslice_AWBURST;
  wire [3:0]m03_couplers_to_m03_regslice_AWCACHE;
  wire [6:0]m03_couplers_to_m03_regslice_AWID;
  wire [7:0]m03_couplers_to_m03_regslice_AWLEN;
  wire [0:0]m03_couplers_to_m03_regslice_AWLOCK;
  wire [2:0]m03_couplers_to_m03_regslice_AWPROT;
  wire [3:0]m03_couplers_to_m03_regslice_AWQOS;
  wire m03_couplers_to_m03_regslice_AWREADY;
  wire [3:0]m03_couplers_to_m03_regslice_AWREGION;
  wire [2:0]m03_couplers_to_m03_regslice_AWSIZE;
  wire m03_couplers_to_m03_regslice_AWVALID;
  wire [6:0]m03_couplers_to_m03_regslice_BID;
  wire m03_couplers_to_m03_regslice_BREADY;
  wire [1:0]m03_couplers_to_m03_regslice_BRESP;
  wire m03_couplers_to_m03_regslice_BVALID;
  wire [511:0]m03_couplers_to_m03_regslice_RDATA;
  wire [6:0]m03_couplers_to_m03_regslice_RID;
  wire m03_couplers_to_m03_regslice_RLAST;
  wire m03_couplers_to_m03_regslice_RREADY;
  wire [1:0]m03_couplers_to_m03_regslice_RRESP;
  wire m03_couplers_to_m03_regslice_RVALID;
  wire [511:0]m03_couplers_to_m03_regslice_WDATA;
  wire m03_couplers_to_m03_regslice_WLAST;
  wire m03_couplers_to_m03_regslice_WREADY;
  wire [63:0]m03_couplers_to_m03_regslice_WSTRB;
  wire m03_couplers_to_m03_regslice_WVALID;
  wire [63:0]m03_regslice_to_m03_couplers_ARADDR;
  wire [1:0]m03_regslice_to_m03_couplers_ARBURST;
  wire [3:0]m03_regslice_to_m03_couplers_ARCACHE;
  wire [6:0]m03_regslice_to_m03_couplers_ARID;
  wire [7:0]m03_regslice_to_m03_couplers_ARLEN;
  wire [0:0]m03_regslice_to_m03_couplers_ARLOCK;
  wire [2:0]m03_regslice_to_m03_couplers_ARPROT;
  wire [3:0]m03_regslice_to_m03_couplers_ARQOS;
  wire m03_regslice_to_m03_couplers_ARREADY;
  wire [3:0]m03_regslice_to_m03_couplers_ARREGION;
  wire [2:0]m03_regslice_to_m03_couplers_ARSIZE;
  wire m03_regslice_to_m03_couplers_ARVALID;
  wire [63:0]m03_regslice_to_m03_couplers_AWADDR;
  wire [1:0]m03_regslice_to_m03_couplers_AWBURST;
  wire [3:0]m03_regslice_to_m03_couplers_AWCACHE;
  wire [6:0]m03_regslice_to_m03_couplers_AWID;
  wire [7:0]m03_regslice_to_m03_couplers_AWLEN;
  wire [0:0]m03_regslice_to_m03_couplers_AWLOCK;
  wire [2:0]m03_regslice_to_m03_couplers_AWPROT;
  wire [3:0]m03_regslice_to_m03_couplers_AWQOS;
  wire m03_regslice_to_m03_couplers_AWREADY;
  wire [3:0]m03_regslice_to_m03_couplers_AWREGION;
  wire [2:0]m03_regslice_to_m03_couplers_AWSIZE;
  wire m03_regslice_to_m03_couplers_AWVALID;
  wire [6:0]m03_regslice_to_m03_couplers_BID;
  wire m03_regslice_to_m03_couplers_BREADY;
  wire [1:0]m03_regslice_to_m03_couplers_BRESP;
  wire m03_regslice_to_m03_couplers_BVALID;
  wire [511:0]m03_regslice_to_m03_couplers_RDATA;
  wire [6:0]m03_regslice_to_m03_couplers_RID;
  wire m03_regslice_to_m03_couplers_RLAST;
  wire m03_regslice_to_m03_couplers_RREADY;
  wire [1:0]m03_regslice_to_m03_couplers_RRESP;
  wire m03_regslice_to_m03_couplers_RVALID;
  wire [511:0]m03_regslice_to_m03_couplers_WDATA;
  wire m03_regslice_to_m03_couplers_WLAST;
  wire m03_regslice_to_m03_couplers_WREADY;
  wire [63:0]m03_regslice_to_m03_couplers_WSTRB;
  wire m03_regslice_to_m03_couplers_WVALID;

  assign M_ACLK_1 = M_ACLK;
  assign M_ARESETN_1 = M_ARESETN;
  assign M_AXI_araddr[63:0] = m03_regslice_to_m03_couplers_ARADDR;
  assign M_AXI_arburst[1:0] = m03_regslice_to_m03_couplers_ARBURST;
  assign M_AXI_arcache[3:0] = m03_regslice_to_m03_couplers_ARCACHE;
  assign M_AXI_arid[6:0] = m03_regslice_to_m03_couplers_ARID;
  assign M_AXI_arlen[7:0] = m03_regslice_to_m03_couplers_ARLEN;
  assign M_AXI_arlock[0] = m03_regslice_to_m03_couplers_ARLOCK;
  assign M_AXI_arprot[2:0] = m03_regslice_to_m03_couplers_ARPROT;
  assign M_AXI_arqos[3:0] = m03_regslice_to_m03_couplers_ARQOS;
  assign M_AXI_arregion[3:0] = m03_regslice_to_m03_couplers_ARREGION;
  assign M_AXI_arsize[2:0] = m03_regslice_to_m03_couplers_ARSIZE;
  assign M_AXI_arvalid = m03_regslice_to_m03_couplers_ARVALID;
  assign M_AXI_awaddr[63:0] = m03_regslice_to_m03_couplers_AWADDR;
  assign M_AXI_awburst[1:0] = m03_regslice_to_m03_couplers_AWBURST;
  assign M_AXI_awcache[3:0] = m03_regslice_to_m03_couplers_AWCACHE;
  assign M_AXI_awid[6:0] = m03_regslice_to_m03_couplers_AWID;
  assign M_AXI_awlen[7:0] = m03_regslice_to_m03_couplers_AWLEN;
  assign M_AXI_awlock[0] = m03_regslice_to_m03_couplers_AWLOCK;
  assign M_AXI_awprot[2:0] = m03_regslice_to_m03_couplers_AWPROT;
  assign M_AXI_awqos[3:0] = m03_regslice_to_m03_couplers_AWQOS;
  assign M_AXI_awregion[3:0] = m03_regslice_to_m03_couplers_AWREGION;
  assign M_AXI_awsize[2:0] = m03_regslice_to_m03_couplers_AWSIZE;
  assign M_AXI_awvalid = m03_regslice_to_m03_couplers_AWVALID;
  assign M_AXI_bready = m03_regslice_to_m03_couplers_BREADY;
  assign M_AXI_rready = m03_regslice_to_m03_couplers_RREADY;
  assign M_AXI_wdata[511:0] = m03_regslice_to_m03_couplers_WDATA;
  assign M_AXI_wlast = m03_regslice_to_m03_couplers_WLAST;
  assign M_AXI_wstrb[63:0] = m03_regslice_to_m03_couplers_WSTRB;
  assign M_AXI_wvalid = m03_regslice_to_m03_couplers_WVALID;
  assign S_AXI_arready = m03_couplers_to_m03_regslice_ARREADY;
  assign S_AXI_awready = m03_couplers_to_m03_regslice_AWREADY;
  assign S_AXI_bid[6:0] = m03_couplers_to_m03_regslice_BID;
  assign S_AXI_bresp[1:0] = m03_couplers_to_m03_regslice_BRESP;
  assign S_AXI_bvalid = m03_couplers_to_m03_regslice_BVALID;
  assign S_AXI_rdata[511:0] = m03_couplers_to_m03_regslice_RDATA;
  assign S_AXI_rid[6:0] = m03_couplers_to_m03_regslice_RID;
  assign S_AXI_rlast = m03_couplers_to_m03_regslice_RLAST;
  assign S_AXI_rresp[1:0] = m03_couplers_to_m03_regslice_RRESP;
  assign S_AXI_rvalid = m03_couplers_to_m03_regslice_RVALID;
  assign S_AXI_wready = m03_couplers_to_m03_regslice_WREADY;
  assign m03_couplers_to_m03_regslice_ARADDR = S_AXI_araddr[63:0];
  assign m03_couplers_to_m03_regslice_ARBURST = S_AXI_arburst[1:0];
  assign m03_couplers_to_m03_regslice_ARCACHE = S_AXI_arcache[3:0];
  assign m03_couplers_to_m03_regslice_ARID = S_AXI_arid[6:0];
  assign m03_couplers_to_m03_regslice_ARLEN = S_AXI_arlen[7:0];
  assign m03_couplers_to_m03_regslice_ARLOCK = S_AXI_arlock[0];
  assign m03_couplers_to_m03_regslice_ARPROT = S_AXI_arprot[2:0];
  assign m03_couplers_to_m03_regslice_ARQOS = S_AXI_arqos[3:0];
  assign m03_couplers_to_m03_regslice_ARREGION = S_AXI_arregion[3:0];
  assign m03_couplers_to_m03_regslice_ARSIZE = S_AXI_arsize[2:0];
  assign m03_couplers_to_m03_regslice_ARVALID = S_AXI_arvalid;
  assign m03_couplers_to_m03_regslice_AWADDR = S_AXI_awaddr[63:0];
  assign m03_couplers_to_m03_regslice_AWBURST = S_AXI_awburst[1:0];
  assign m03_couplers_to_m03_regslice_AWCACHE = S_AXI_awcache[3:0];
  assign m03_couplers_to_m03_regslice_AWID = S_AXI_awid[6:0];
  assign m03_couplers_to_m03_regslice_AWLEN = S_AXI_awlen[7:0];
  assign m03_couplers_to_m03_regslice_AWLOCK = S_AXI_awlock[0];
  assign m03_couplers_to_m03_regslice_AWPROT = S_AXI_awprot[2:0];
  assign m03_couplers_to_m03_regslice_AWQOS = S_AXI_awqos[3:0];
  assign m03_couplers_to_m03_regslice_AWREGION = S_AXI_awregion[3:0];
  assign m03_couplers_to_m03_regslice_AWSIZE = S_AXI_awsize[2:0];
  assign m03_couplers_to_m03_regslice_AWVALID = S_AXI_awvalid;
  assign m03_couplers_to_m03_regslice_BREADY = S_AXI_bready;
  assign m03_couplers_to_m03_regslice_RREADY = S_AXI_rready;
  assign m03_couplers_to_m03_regslice_WDATA = S_AXI_wdata[511:0];
  assign m03_couplers_to_m03_regslice_WLAST = S_AXI_wlast;
  assign m03_couplers_to_m03_regslice_WSTRB = S_AXI_wstrb[63:0];
  assign m03_couplers_to_m03_regslice_WVALID = S_AXI_wvalid;
  assign m03_regslice_to_m03_couplers_ARREADY = M_AXI_arready;
  assign m03_regslice_to_m03_couplers_AWREADY = M_AXI_awready;
  assign m03_regslice_to_m03_couplers_BID = M_AXI_bid[6:0];
  assign m03_regslice_to_m03_couplers_BRESP = M_AXI_bresp[1:0];
  assign m03_regslice_to_m03_couplers_BVALID = M_AXI_bvalid;
  assign m03_regslice_to_m03_couplers_RDATA = M_AXI_rdata[511:0];
  assign m03_regslice_to_m03_couplers_RID = M_AXI_rid[6:0];
  assign m03_regslice_to_m03_couplers_RLAST = M_AXI_rlast;
  assign m03_regslice_to_m03_couplers_RRESP = M_AXI_rresp[1:0];
  assign m03_regslice_to_m03_couplers_RVALID = M_AXI_rvalid;
  assign m03_regslice_to_m03_couplers_WREADY = M_AXI_wready;
  cl_xbar_m03_regslice_0 m03_regslice
       (.aclk(M_ACLK_1),
        .aresetn(M_ARESETN_1),
        .m_axi_araddr(m03_regslice_to_m03_couplers_ARADDR),
        .m_axi_arburst(m03_regslice_to_m03_couplers_ARBURST),
        .m_axi_arcache(m03_regslice_to_m03_couplers_ARCACHE),
        .m_axi_arid(m03_regslice_to_m03_couplers_ARID),
        .m_axi_arlen(m03_regslice_to_m03_couplers_ARLEN),
        .m_axi_arlock(m03_regslice_to_m03_couplers_ARLOCK),
        .m_axi_arprot(m03_regslice_to_m03_couplers_ARPROT),
        .m_axi_arqos(m03_regslice_to_m03_couplers_ARQOS),
        .m_axi_arready(m03_regslice_to_m03_couplers_ARREADY),
        .m_axi_arregion(m03_regslice_to_m03_couplers_ARREGION),
        .m_axi_arsize(m03_regslice_to_m03_couplers_ARSIZE),
        .m_axi_arvalid(m03_regslice_to_m03_couplers_ARVALID),
        .m_axi_awaddr(m03_regslice_to_m03_couplers_AWADDR),
        .m_axi_awburst(m03_regslice_to_m03_couplers_AWBURST),
        .m_axi_awcache(m03_regslice_to_m03_couplers_AWCACHE),
        .m_axi_awid(m03_regslice_to_m03_couplers_AWID),
        .m_axi_awlen(m03_regslice_to_m03_couplers_AWLEN),
        .m_axi_awlock(m03_regslice_to_m03_couplers_AWLOCK),
        .m_axi_awprot(m03_regslice_to_m03_couplers_AWPROT),
        .m_axi_awqos(m03_regslice_to_m03_couplers_AWQOS),
        .m_axi_awready(m03_regslice_to_m03_couplers_AWREADY),
        .m_axi_awregion(m03_regslice_to_m03_couplers_AWREGION),
        .m_axi_awsize(m03_regslice_to_m03_couplers_AWSIZE),
        .m_axi_awvalid(m03_regslice_to_m03_couplers_AWVALID),
        .m_axi_bid(m03_regslice_to_m03_couplers_BID),
        .m_axi_bready(m03_regslice_to_m03_couplers_BREADY),
        .m_axi_bresp(m03_regslice_to_m03_couplers_BRESP),
        .m_axi_bvalid(m03_regslice_to_m03_couplers_BVALID),
        .m_axi_rdata(m03_regslice_to_m03_couplers_RDATA),
        .m_axi_rid(m03_regslice_to_m03_couplers_RID),
        .m_axi_rlast(m03_regslice_to_m03_couplers_RLAST),
        .m_axi_rready(m03_regslice_to_m03_couplers_RREADY),
        .m_axi_rresp(m03_regslice_to_m03_couplers_RRESP),
        .m_axi_rvalid(m03_regslice_to_m03_couplers_RVALID),
        .m_axi_wdata(m03_regslice_to_m03_couplers_WDATA),
        .m_axi_wlast(m03_regslice_to_m03_couplers_WLAST),
        .m_axi_wready(m03_regslice_to_m03_couplers_WREADY),
        .m_axi_wstrb(m03_regslice_to_m03_couplers_WSTRB),
        .m_axi_wvalid(m03_regslice_to_m03_couplers_WVALID),
        .s_axi_araddr(m03_couplers_to_m03_regslice_ARADDR),
        .s_axi_arburst(m03_couplers_to_m03_regslice_ARBURST),
        .s_axi_arcache(m03_couplers_to_m03_regslice_ARCACHE),
        .s_axi_arid(m03_couplers_to_m03_regslice_ARID),
        .s_axi_arlen(m03_couplers_to_m03_regslice_ARLEN),
        .s_axi_arlock(m03_couplers_to_m03_regslice_ARLOCK),
        .s_axi_arprot(m03_couplers_to_m03_regslice_ARPROT),
        .s_axi_arqos(m03_couplers_to_m03_regslice_ARQOS),
        .s_axi_arready(m03_couplers_to_m03_regslice_ARREADY),
        .s_axi_arregion(m03_couplers_to_m03_regslice_ARREGION),
        .s_axi_arsize(m03_couplers_to_m03_regslice_ARSIZE),
        .s_axi_arvalid(m03_couplers_to_m03_regslice_ARVALID),
        .s_axi_awaddr(m03_couplers_to_m03_regslice_AWADDR),
        .s_axi_awburst(m03_couplers_to_m03_regslice_AWBURST),
        .s_axi_awcache(m03_couplers_to_m03_regslice_AWCACHE),
        .s_axi_awid(m03_couplers_to_m03_regslice_AWID),
        .s_axi_awlen(m03_couplers_to_m03_regslice_AWLEN),
        .s_axi_awlock(m03_couplers_to_m03_regslice_AWLOCK),
        .s_axi_awprot(m03_couplers_to_m03_regslice_AWPROT),
        .s_axi_awqos(m03_couplers_to_m03_regslice_AWQOS),
        .s_axi_awready(m03_couplers_to_m03_regslice_AWREADY),
        .s_axi_awregion(m03_couplers_to_m03_regslice_AWREGION),
        .s_axi_awsize(m03_couplers_to_m03_regslice_AWSIZE),
        .s_axi_awvalid(m03_couplers_to_m03_regslice_AWVALID),
        .s_axi_bid(m03_couplers_to_m03_regslice_BID),
        .s_axi_bready(m03_couplers_to_m03_regslice_BREADY),
        .s_axi_bresp(m03_couplers_to_m03_regslice_BRESP),
        .s_axi_bvalid(m03_couplers_to_m03_regslice_BVALID),
        .s_axi_rdata(m03_couplers_to_m03_regslice_RDATA),
        .s_axi_rid(m03_couplers_to_m03_regslice_RID),
        .s_axi_rlast(m03_couplers_to_m03_regslice_RLAST),
        .s_axi_rready(m03_couplers_to_m03_regslice_RREADY),
        .s_axi_rresp(m03_couplers_to_m03_regslice_RRESP),
        .s_axi_rvalid(m03_couplers_to_m03_regslice_RVALID),
        .s_axi_wdata(m03_couplers_to_m03_regslice_WDATA),
        .s_axi_wlast(m03_couplers_to_m03_regslice_WLAST),
        .s_axi_wready(m03_couplers_to_m03_regslice_WREADY),
        .s_axi_wstrb(m03_couplers_to_m03_regslice_WSTRB),
        .s_axi_wvalid(m03_couplers_to_m03_regslice_WVALID));
endmodule

module s00_couplers_imp_1HVTIW6
   (M_ACLK,
    M_ARESETN,
    M_AXI_araddr,
    M_AXI_arburst,
    M_AXI_arcache,
    M_AXI_arid,
    M_AXI_arlen,
    M_AXI_arlock,
    M_AXI_arprot,
    M_AXI_arqos,
    M_AXI_arready,
    M_AXI_arsize,
    M_AXI_arvalid,
    M_AXI_awaddr,
    M_AXI_awburst,
    M_AXI_awcache,
    M_AXI_awid,
    M_AXI_awlen,
    M_AXI_awlock,
    M_AXI_awprot,
    M_AXI_awqos,
    M_AXI_awready,
    M_AXI_awsize,
    M_AXI_awvalid,
    M_AXI_bid,
    M_AXI_bready,
    M_AXI_bresp,
    M_AXI_bvalid,
    M_AXI_rdata,
    M_AXI_rid,
    M_AXI_rlast,
    M_AXI_rready,
    M_AXI_rresp,
    M_AXI_rvalid,
    M_AXI_wdata,
    M_AXI_wlast,
    M_AXI_wready,
    M_AXI_wstrb,
    M_AXI_wvalid,
    S_ACLK,
    S_ARESETN,
    S_AXI_araddr,
    S_AXI_arburst,
    S_AXI_arcache,
    S_AXI_arid,
    S_AXI_arlen,
    S_AXI_arlock,
    S_AXI_arprot,
    S_AXI_arqos,
    S_AXI_arready,
    S_AXI_arregion,
    S_AXI_arsize,
    S_AXI_arvalid,
    S_AXI_awaddr,
    S_AXI_awburst,
    S_AXI_awcache,
    S_AXI_awid,
    S_AXI_awlen,
    S_AXI_awlock,
    S_AXI_awprot,
    S_AXI_awqos,
    S_AXI_awready,
    S_AXI_awregion,
    S_AXI_awsize,
    S_AXI_awvalid,
    S_AXI_bid,
    S_AXI_bready,
    S_AXI_bresp,
    S_AXI_bvalid,
    S_AXI_rdata,
    S_AXI_rid,
    S_AXI_rlast,
    S_AXI_rready,
    S_AXI_rresp,
    S_AXI_rvalid,
    S_AXI_wdata,
    S_AXI_wlast,
    S_AXI_wready,
    S_AXI_wstrb,
    S_AXI_wvalid);
  input M_ACLK;
  input M_ARESETN;
  output [63:0]M_AXI_araddr;
  output [1:0]M_AXI_arburst;
  output [3:0]M_AXI_arcache;
  output [5:0]M_AXI_arid;
  output [7:0]M_AXI_arlen;
  output [0:0]M_AXI_arlock;
  output [2:0]M_AXI_arprot;
  output [3:0]M_AXI_arqos;
  input M_AXI_arready;
  output [2:0]M_AXI_arsize;
  output M_AXI_arvalid;
  output [63:0]M_AXI_awaddr;
  output [1:0]M_AXI_awburst;
  output [3:0]M_AXI_awcache;
  output [5:0]M_AXI_awid;
  output [7:0]M_AXI_awlen;
  output [0:0]M_AXI_awlock;
  output [2:0]M_AXI_awprot;
  output [3:0]M_AXI_awqos;
  input M_AXI_awready;
  output [2:0]M_AXI_awsize;
  output M_AXI_awvalid;
  input [6:0]M_AXI_bid;
  output M_AXI_bready;
  input [1:0]M_AXI_bresp;
  input M_AXI_bvalid;
  input [511:0]M_AXI_rdata;
  input [6:0]M_AXI_rid;
  input M_AXI_rlast;
  output M_AXI_rready;
  input [1:0]M_AXI_rresp;
  input M_AXI_rvalid;
  output [511:0]M_AXI_wdata;
  output M_AXI_wlast;
  input M_AXI_wready;
  output [63:0]M_AXI_wstrb;
  output M_AXI_wvalid;
  input S_ACLK;
  input S_ARESETN;
  input [63:0]S_AXI_araddr;
  input [1:0]S_AXI_arburst;
  input [3:0]S_AXI_arcache;
  input [5:0]S_AXI_arid;
  input [7:0]S_AXI_arlen;
  input [0:0]S_AXI_arlock;
  input [2:0]S_AXI_arprot;
  input [3:0]S_AXI_arqos;
  output S_AXI_arready;
  input [3:0]S_AXI_arregion;
  input [2:0]S_AXI_arsize;
  input S_AXI_arvalid;
  input [63:0]S_AXI_awaddr;
  input [1:0]S_AXI_awburst;
  input [3:0]S_AXI_awcache;
  input [5:0]S_AXI_awid;
  input [7:0]S_AXI_awlen;
  input [0:0]S_AXI_awlock;
  input [2:0]S_AXI_awprot;
  input [3:0]S_AXI_awqos;
  output S_AXI_awready;
  input [3:0]S_AXI_awregion;
  input [2:0]S_AXI_awsize;
  input S_AXI_awvalid;
  output [5:0]S_AXI_bid;
  input S_AXI_bready;
  output [1:0]S_AXI_bresp;
  output S_AXI_bvalid;
  output [511:0]S_AXI_rdata;
  output [5:0]S_AXI_rid;
  output S_AXI_rlast;
  input S_AXI_rready;
  output [1:0]S_AXI_rresp;
  output S_AXI_rvalid;
  input [511:0]S_AXI_wdata;
  input S_AXI_wlast;
  output S_AXI_wready;
  input [63:0]S_AXI_wstrb;
  input S_AXI_wvalid;

  wire S_ACLK_1;
  wire S_ARESETN_1;
  wire [63:0]s00_couplers_to_s00_regslice_ARADDR;
  wire [1:0]s00_couplers_to_s00_regslice_ARBURST;
  wire [3:0]s00_couplers_to_s00_regslice_ARCACHE;
  wire [5:0]s00_couplers_to_s00_regslice_ARID;
  wire [7:0]s00_couplers_to_s00_regslice_ARLEN;
  wire [0:0]s00_couplers_to_s00_regslice_ARLOCK;
  wire [2:0]s00_couplers_to_s00_regslice_ARPROT;
  wire [3:0]s00_couplers_to_s00_regslice_ARQOS;
  wire s00_couplers_to_s00_regslice_ARREADY;
  wire [3:0]s00_couplers_to_s00_regslice_ARREGION;
  wire [2:0]s00_couplers_to_s00_regslice_ARSIZE;
  wire s00_couplers_to_s00_regslice_ARVALID;
  wire [63:0]s00_couplers_to_s00_regslice_AWADDR;
  wire [1:0]s00_couplers_to_s00_regslice_AWBURST;
  wire [3:0]s00_couplers_to_s00_regslice_AWCACHE;
  wire [5:0]s00_couplers_to_s00_regslice_AWID;
  wire [7:0]s00_couplers_to_s00_regslice_AWLEN;
  wire [0:0]s00_couplers_to_s00_regslice_AWLOCK;
  wire [2:0]s00_couplers_to_s00_regslice_AWPROT;
  wire [3:0]s00_couplers_to_s00_regslice_AWQOS;
  wire s00_couplers_to_s00_regslice_AWREADY;
  wire [3:0]s00_couplers_to_s00_regslice_AWREGION;
  wire [2:0]s00_couplers_to_s00_regslice_AWSIZE;
  wire s00_couplers_to_s00_regslice_AWVALID;
  wire [5:0]s00_couplers_to_s00_regslice_BID;
  wire s00_couplers_to_s00_regslice_BREADY;
  wire [1:0]s00_couplers_to_s00_regslice_BRESP;
  wire s00_couplers_to_s00_regslice_BVALID;
  wire [511:0]s00_couplers_to_s00_regslice_RDATA;
  wire [5:0]s00_couplers_to_s00_regslice_RID;
  wire s00_couplers_to_s00_regslice_RLAST;
  wire s00_couplers_to_s00_regslice_RREADY;
  wire [1:0]s00_couplers_to_s00_regslice_RRESP;
  wire s00_couplers_to_s00_regslice_RVALID;
  wire [511:0]s00_couplers_to_s00_regslice_WDATA;
  wire s00_couplers_to_s00_regslice_WLAST;
  wire s00_couplers_to_s00_regslice_WREADY;
  wire [63:0]s00_couplers_to_s00_regslice_WSTRB;
  wire s00_couplers_to_s00_regslice_WVALID;
  wire [63:0]s00_regslice_to_s00_couplers_ARADDR;
  wire [1:0]s00_regslice_to_s00_couplers_ARBURST;
  wire [3:0]s00_regslice_to_s00_couplers_ARCACHE;
  wire [5:0]s00_regslice_to_s00_couplers_ARID;
  wire [7:0]s00_regslice_to_s00_couplers_ARLEN;
  wire [0:0]s00_regslice_to_s00_couplers_ARLOCK;
  wire [2:0]s00_regslice_to_s00_couplers_ARPROT;
  wire [3:0]s00_regslice_to_s00_couplers_ARQOS;
  wire s00_regslice_to_s00_couplers_ARREADY;
  wire [2:0]s00_regslice_to_s00_couplers_ARSIZE;
  wire s00_regslice_to_s00_couplers_ARVALID;
  wire [63:0]s00_regslice_to_s00_couplers_AWADDR;
  wire [1:0]s00_regslice_to_s00_couplers_AWBURST;
  wire [3:0]s00_regslice_to_s00_couplers_AWCACHE;
  wire [5:0]s00_regslice_to_s00_couplers_AWID;
  wire [7:0]s00_regslice_to_s00_couplers_AWLEN;
  wire [0:0]s00_regslice_to_s00_couplers_AWLOCK;
  wire [2:0]s00_regslice_to_s00_couplers_AWPROT;
  wire [3:0]s00_regslice_to_s00_couplers_AWQOS;
  wire s00_regslice_to_s00_couplers_AWREADY;
  wire [2:0]s00_regslice_to_s00_couplers_AWSIZE;
  wire s00_regslice_to_s00_couplers_AWVALID;
  wire [6:0]s00_regslice_to_s00_couplers_BID;
  wire s00_regslice_to_s00_couplers_BREADY;
  wire [1:0]s00_regslice_to_s00_couplers_BRESP;
  wire s00_regslice_to_s00_couplers_BVALID;
  wire [511:0]s00_regslice_to_s00_couplers_RDATA;
  wire [6:0]s00_regslice_to_s00_couplers_RID;
  wire s00_regslice_to_s00_couplers_RLAST;
  wire s00_regslice_to_s00_couplers_RREADY;
  wire [1:0]s00_regslice_to_s00_couplers_RRESP;
  wire s00_regslice_to_s00_couplers_RVALID;
  wire [511:0]s00_regslice_to_s00_couplers_WDATA;
  wire s00_regslice_to_s00_couplers_WLAST;
  wire s00_regslice_to_s00_couplers_WREADY;
  wire [63:0]s00_regslice_to_s00_couplers_WSTRB;
  wire s00_regslice_to_s00_couplers_WVALID;

  assign M_AXI_araddr[63:0] = s00_regslice_to_s00_couplers_ARADDR;
  assign M_AXI_arburst[1:0] = s00_regslice_to_s00_couplers_ARBURST;
  assign M_AXI_arcache[3:0] = s00_regslice_to_s00_couplers_ARCACHE;
  assign M_AXI_arid[5:0] = s00_regslice_to_s00_couplers_ARID;
  assign M_AXI_arlen[7:0] = s00_regslice_to_s00_couplers_ARLEN;
  assign M_AXI_arlock[0] = s00_regslice_to_s00_couplers_ARLOCK;
  assign M_AXI_arprot[2:0] = s00_regslice_to_s00_couplers_ARPROT;
  assign M_AXI_arqos[3:0] = s00_regslice_to_s00_couplers_ARQOS;
  assign M_AXI_arsize[2:0] = s00_regslice_to_s00_couplers_ARSIZE;
  assign M_AXI_arvalid = s00_regslice_to_s00_couplers_ARVALID;
  assign M_AXI_awaddr[63:0] = s00_regslice_to_s00_couplers_AWADDR;
  assign M_AXI_awburst[1:0] = s00_regslice_to_s00_couplers_AWBURST;
  assign M_AXI_awcache[3:0] = s00_regslice_to_s00_couplers_AWCACHE;
  assign M_AXI_awid[5:0] = s00_regslice_to_s00_couplers_AWID;
  assign M_AXI_awlen[7:0] = s00_regslice_to_s00_couplers_AWLEN;
  assign M_AXI_awlock[0] = s00_regslice_to_s00_couplers_AWLOCK;
  assign M_AXI_awprot[2:0] = s00_regslice_to_s00_couplers_AWPROT;
  assign M_AXI_awqos[3:0] = s00_regslice_to_s00_couplers_AWQOS;
  assign M_AXI_awsize[2:0] = s00_regslice_to_s00_couplers_AWSIZE;
  assign M_AXI_awvalid = s00_regslice_to_s00_couplers_AWVALID;
  assign M_AXI_bready = s00_regslice_to_s00_couplers_BREADY;
  assign M_AXI_rready = s00_regslice_to_s00_couplers_RREADY;
  assign M_AXI_wdata[511:0] = s00_regslice_to_s00_couplers_WDATA;
  assign M_AXI_wlast = s00_regslice_to_s00_couplers_WLAST;
  assign M_AXI_wstrb[63:0] = s00_regslice_to_s00_couplers_WSTRB;
  assign M_AXI_wvalid = s00_regslice_to_s00_couplers_WVALID;
  assign S_ACLK_1 = S_ACLK;
  assign S_ARESETN_1 = S_ARESETN;
  assign S_AXI_arready = s00_couplers_to_s00_regslice_ARREADY;
  assign S_AXI_awready = s00_couplers_to_s00_regslice_AWREADY;
  assign S_AXI_bid[5:0] = s00_couplers_to_s00_regslice_BID;
  assign S_AXI_bresp[1:0] = s00_couplers_to_s00_regslice_BRESP;
  assign S_AXI_bvalid = s00_couplers_to_s00_regslice_BVALID;
  assign S_AXI_rdata[511:0] = s00_couplers_to_s00_regslice_RDATA;
  assign S_AXI_rid[5:0] = s00_couplers_to_s00_regslice_RID;
  assign S_AXI_rlast = s00_couplers_to_s00_regslice_RLAST;
  assign S_AXI_rresp[1:0] = s00_couplers_to_s00_regslice_RRESP;
  assign S_AXI_rvalid = s00_couplers_to_s00_regslice_RVALID;
  assign S_AXI_wready = s00_couplers_to_s00_regslice_WREADY;
  assign s00_couplers_to_s00_regslice_ARADDR = S_AXI_araddr[63:0];
  assign s00_couplers_to_s00_regslice_ARBURST = S_AXI_arburst[1:0];
  assign s00_couplers_to_s00_regslice_ARCACHE = S_AXI_arcache[3:0];
  assign s00_couplers_to_s00_regslice_ARID = S_AXI_arid[5:0];
  assign s00_couplers_to_s00_regslice_ARLEN = S_AXI_arlen[7:0];
  assign s00_couplers_to_s00_regslice_ARLOCK = S_AXI_arlock[0];
  assign s00_couplers_to_s00_regslice_ARPROT = S_AXI_arprot[2:0];
  assign s00_couplers_to_s00_regslice_ARQOS = S_AXI_arqos[3:0];
  assign s00_couplers_to_s00_regslice_ARREGION = S_AXI_arregion[3:0];
  assign s00_couplers_to_s00_regslice_ARSIZE = S_AXI_arsize[2:0];
  assign s00_couplers_to_s00_regslice_ARVALID = S_AXI_arvalid;
  assign s00_couplers_to_s00_regslice_AWADDR = S_AXI_awaddr[63:0];
  assign s00_couplers_to_s00_regslice_AWBURST = S_AXI_awburst[1:0];
  assign s00_couplers_to_s00_regslice_AWCACHE = S_AXI_awcache[3:0];
  assign s00_couplers_to_s00_regslice_AWID = S_AXI_awid[5:0];
  assign s00_couplers_to_s00_regslice_AWLEN = S_AXI_awlen[7:0];
  assign s00_couplers_to_s00_regslice_AWLOCK = S_AXI_awlock[0];
  assign s00_couplers_to_s00_regslice_AWPROT = S_AXI_awprot[2:0];
  assign s00_couplers_to_s00_regslice_AWQOS = S_AXI_awqos[3:0];
  assign s00_couplers_to_s00_regslice_AWREGION = S_AXI_awregion[3:0];
  assign s00_couplers_to_s00_regslice_AWSIZE = S_AXI_awsize[2:0];
  assign s00_couplers_to_s00_regslice_AWVALID = S_AXI_awvalid;
  assign s00_couplers_to_s00_regslice_BREADY = S_AXI_bready;
  assign s00_couplers_to_s00_regslice_RREADY = S_AXI_rready;
  assign s00_couplers_to_s00_regslice_WDATA = S_AXI_wdata[511:0];
  assign s00_couplers_to_s00_regslice_WLAST = S_AXI_wlast;
  assign s00_couplers_to_s00_regslice_WSTRB = S_AXI_wstrb[63:0];
  assign s00_couplers_to_s00_regslice_WVALID = S_AXI_wvalid;
  assign s00_regslice_to_s00_couplers_ARREADY = M_AXI_arready;
  assign s00_regslice_to_s00_couplers_AWREADY = M_AXI_awready;
  assign s00_regslice_to_s00_couplers_BID = M_AXI_bid[6:0];
  assign s00_regslice_to_s00_couplers_BRESP = M_AXI_bresp[1:0];
  assign s00_regslice_to_s00_couplers_BVALID = M_AXI_bvalid;
  assign s00_regslice_to_s00_couplers_RDATA = M_AXI_rdata[511:0];
  assign s00_regslice_to_s00_couplers_RID = M_AXI_rid[6:0];
  assign s00_regslice_to_s00_couplers_RLAST = M_AXI_rlast;
  assign s00_regslice_to_s00_couplers_RRESP = M_AXI_rresp[1:0];
  assign s00_regslice_to_s00_couplers_RVALID = M_AXI_rvalid;
  assign s00_regslice_to_s00_couplers_WREADY = M_AXI_wready;
  cl_xbar_s00_regslice_3 s00_regslice
       (.aclk(S_ACLK_1),
        .aresetn(S_ARESETN_1),
        .m_axi_araddr(s00_regslice_to_s00_couplers_ARADDR),
        .m_axi_arburst(s00_regslice_to_s00_couplers_ARBURST),
        .m_axi_arcache(s00_regslice_to_s00_couplers_ARCACHE),
        .m_axi_arid(s00_regslice_to_s00_couplers_ARID),
        .m_axi_arlen(s00_regslice_to_s00_couplers_ARLEN),
        .m_axi_arlock(s00_regslice_to_s00_couplers_ARLOCK),
        .m_axi_arprot(s00_regslice_to_s00_couplers_ARPROT),
        .m_axi_arqos(s00_regslice_to_s00_couplers_ARQOS),
        .m_axi_arready(s00_regslice_to_s00_couplers_ARREADY),
        .m_axi_arsize(s00_regslice_to_s00_couplers_ARSIZE),
        .m_axi_arvalid(s00_regslice_to_s00_couplers_ARVALID),
        .m_axi_awaddr(s00_regslice_to_s00_couplers_AWADDR),
        .m_axi_awburst(s00_regslice_to_s00_couplers_AWBURST),
        .m_axi_awcache(s00_regslice_to_s00_couplers_AWCACHE),
        .m_axi_awid(s00_regslice_to_s00_couplers_AWID),
        .m_axi_awlen(s00_regslice_to_s00_couplers_AWLEN),
        .m_axi_awlock(s00_regslice_to_s00_couplers_AWLOCK),
        .m_axi_awprot(s00_regslice_to_s00_couplers_AWPROT),
        .m_axi_awqos(s00_regslice_to_s00_couplers_AWQOS),
        .m_axi_awready(s00_regslice_to_s00_couplers_AWREADY),
        .m_axi_awsize(s00_regslice_to_s00_couplers_AWSIZE),
        .m_axi_awvalid(s00_regslice_to_s00_couplers_AWVALID),
        .m_axi_bid(s00_regslice_to_s00_couplers_BID[5:0]),
        .m_axi_bready(s00_regslice_to_s00_couplers_BREADY),
        .m_axi_bresp(s00_regslice_to_s00_couplers_BRESP),
        .m_axi_bvalid(s00_regslice_to_s00_couplers_BVALID),
        .m_axi_rdata(s00_regslice_to_s00_couplers_RDATA),
        .m_axi_rid(s00_regslice_to_s00_couplers_RID[5:0]),
        .m_axi_rlast(s00_regslice_to_s00_couplers_RLAST),
        .m_axi_rready(s00_regslice_to_s00_couplers_RREADY),
        .m_axi_rresp(s00_regslice_to_s00_couplers_RRESP),
        .m_axi_rvalid(s00_regslice_to_s00_couplers_RVALID),
        .m_axi_wdata(s00_regslice_to_s00_couplers_WDATA),
        .m_axi_wlast(s00_regslice_to_s00_couplers_WLAST),
        .m_axi_wready(s00_regslice_to_s00_couplers_WREADY),
        .m_axi_wstrb(s00_regslice_to_s00_couplers_WSTRB),
        .m_axi_wvalid(s00_regslice_to_s00_couplers_WVALID),
        .s_axi_araddr(s00_couplers_to_s00_regslice_ARADDR),
        .s_axi_arburst(s00_couplers_to_s00_regslice_ARBURST),
        .s_axi_arcache(s00_couplers_to_s00_regslice_ARCACHE),
        .s_axi_arid(s00_couplers_to_s00_regslice_ARID),
        .s_axi_arlen(s00_couplers_to_s00_regslice_ARLEN),
        .s_axi_arlock(s00_couplers_to_s00_regslice_ARLOCK),
        .s_axi_arprot(s00_couplers_to_s00_regslice_ARPROT),
        .s_axi_arqos(s00_couplers_to_s00_regslice_ARQOS),
        .s_axi_arready(s00_couplers_to_s00_regslice_ARREADY),
        .s_axi_arregion(s00_couplers_to_s00_regslice_ARREGION),
        .s_axi_arsize(s00_couplers_to_s00_regslice_ARSIZE),
        .s_axi_arvalid(s00_couplers_to_s00_regslice_ARVALID),
        .s_axi_awaddr(s00_couplers_to_s00_regslice_AWADDR),
        .s_axi_awburst(s00_couplers_to_s00_regslice_AWBURST),
        .s_axi_awcache(s00_couplers_to_s00_regslice_AWCACHE),
        .s_axi_awid(s00_couplers_to_s00_regslice_AWID),
        .s_axi_awlen(s00_couplers_to_s00_regslice_AWLEN),
        .s_axi_awlock(s00_couplers_to_s00_regslice_AWLOCK),
        .s_axi_awprot(s00_couplers_to_s00_regslice_AWPROT),
        .s_axi_awqos(s00_couplers_to_s00_regslice_AWQOS),
        .s_axi_awready(s00_couplers_to_s00_regslice_AWREADY),
        .s_axi_awregion(s00_couplers_to_s00_regslice_AWREGION),
        .s_axi_awsize(s00_couplers_to_s00_regslice_AWSIZE),
        .s_axi_awvalid(s00_couplers_to_s00_regslice_AWVALID),
        .s_axi_bid(s00_couplers_to_s00_regslice_BID),
        .s_axi_bready(s00_couplers_to_s00_regslice_BREADY),
        .s_axi_bresp(s00_couplers_to_s00_regslice_BRESP),
        .s_axi_bvalid(s00_couplers_to_s00_regslice_BVALID),
        .s_axi_rdata(s00_couplers_to_s00_regslice_RDATA),
        .s_axi_rid(s00_couplers_to_s00_regslice_RID),
        .s_axi_rlast(s00_couplers_to_s00_regslice_RLAST),
        .s_axi_rready(s00_couplers_to_s00_regslice_RREADY),
        .s_axi_rresp(s00_couplers_to_s00_regslice_RRESP),
        .s_axi_rvalid(s00_couplers_to_s00_regslice_RVALID),
        .s_axi_wdata(s00_couplers_to_s00_regslice_WDATA),
        .s_axi_wlast(s00_couplers_to_s00_regslice_WLAST),
        .s_axi_wready(s00_couplers_to_s00_regslice_WREADY),
        .s_axi_wstrb(s00_couplers_to_s00_regslice_WSTRB),
        .s_axi_wvalid(s00_couplers_to_s00_regslice_WVALID));
endmodule

module s00_couplers_imp_35C8W0
   (M_ACLK,
    M_ARESETN,
    M_AXI_araddr,
    M_AXI_arburst,
    M_AXI_arcache,
    M_AXI_arid,
    M_AXI_arlen,
    M_AXI_arlock,
    M_AXI_arprot,
    M_AXI_arqos,
    M_AXI_arready,
    M_AXI_arsize,
    M_AXI_arvalid,
    M_AXI_awaddr,
    M_AXI_awburst,
    M_AXI_awcache,
    M_AXI_awid,
    M_AXI_awlen,
    M_AXI_awlock,
    M_AXI_awprot,
    M_AXI_awqos,
    M_AXI_awready,
    M_AXI_awsize,
    M_AXI_awvalid,
    M_AXI_bid,
    M_AXI_bready,
    M_AXI_bresp,
    M_AXI_bvalid,
    M_AXI_rdata,
    M_AXI_rid,
    M_AXI_rlast,
    M_AXI_rready,
    M_AXI_rresp,
    M_AXI_rvalid,
    M_AXI_wdata,
    M_AXI_wlast,
    M_AXI_wready,
    M_AXI_wstrb,
    M_AXI_wvalid,
    S_ACLK,
    S_ARESETN,
    S_AXI_araddr,
    S_AXI_arburst,
    S_AXI_arcache,
    S_AXI_arid,
    S_AXI_arlen,
    S_AXI_arlock,
    S_AXI_arprot,
    S_AXI_arqos,
    S_AXI_arready,
    S_AXI_arregion,
    S_AXI_arsize,
    S_AXI_arvalid,
    S_AXI_awaddr,
    S_AXI_awburst,
    S_AXI_awcache,
    S_AXI_awid,
    S_AXI_awlen,
    S_AXI_awlock,
    S_AXI_awprot,
    S_AXI_awqos,
    S_AXI_awready,
    S_AXI_awregion,
    S_AXI_awsize,
    S_AXI_awvalid,
    S_AXI_bid,
    S_AXI_bready,
    S_AXI_bresp,
    S_AXI_bvalid,
    S_AXI_rdata,
    S_AXI_rid,
    S_AXI_rlast,
    S_AXI_rready,
    S_AXI_rresp,
    S_AXI_rvalid,
    S_AXI_wdata,
    S_AXI_wlast,
    S_AXI_wready,
    S_AXI_wstrb,
    S_AXI_wvalid);
  input M_ACLK;
  input M_ARESETN;
  output [63:0]M_AXI_araddr;
  output [1:0]M_AXI_arburst;
  output [3:0]M_AXI_arcache;
  output [5:0]M_AXI_arid;
  output [7:0]M_AXI_arlen;
  output [0:0]M_AXI_arlock;
  output [2:0]M_AXI_arprot;
  output [3:0]M_AXI_arqos;
  input M_AXI_arready;
  output [2:0]M_AXI_arsize;
  output M_AXI_arvalid;
  output [63:0]M_AXI_awaddr;
  output [1:0]M_AXI_awburst;
  output [3:0]M_AXI_awcache;
  output [5:0]M_AXI_awid;
  output [7:0]M_AXI_awlen;
  output [0:0]M_AXI_awlock;
  output [2:0]M_AXI_awprot;
  output [3:0]M_AXI_awqos;
  input M_AXI_awready;
  output [2:0]M_AXI_awsize;
  output M_AXI_awvalid;
  input [5:0]M_AXI_bid;
  output M_AXI_bready;
  input [1:0]M_AXI_bresp;
  input M_AXI_bvalid;
  input [511:0]M_AXI_rdata;
  input [5:0]M_AXI_rid;
  input M_AXI_rlast;
  output M_AXI_rready;
  input [1:0]M_AXI_rresp;
  input M_AXI_rvalid;
  output [511:0]M_AXI_wdata;
  output M_AXI_wlast;
  input M_AXI_wready;
  output [63:0]M_AXI_wstrb;
  output M_AXI_wvalid;
  input S_ACLK;
  input S_ARESETN;
  input [63:0]S_AXI_araddr;
  input [1:0]S_AXI_arburst;
  input [3:0]S_AXI_arcache;
  input [5:0]S_AXI_arid;
  input [7:0]S_AXI_arlen;
  input [0:0]S_AXI_arlock;
  input [2:0]S_AXI_arprot;
  input [3:0]S_AXI_arqos;
  output S_AXI_arready;
  input [3:0]S_AXI_arregion;
  input [2:0]S_AXI_arsize;
  input S_AXI_arvalid;
  input [63:0]S_AXI_awaddr;
  input [1:0]S_AXI_awburst;
  input [3:0]S_AXI_awcache;
  input [5:0]S_AXI_awid;
  input [7:0]S_AXI_awlen;
  input [0:0]S_AXI_awlock;
  input [2:0]S_AXI_awprot;
  input [3:0]S_AXI_awqos;
  output S_AXI_awready;
  input [3:0]S_AXI_awregion;
  input [2:0]S_AXI_awsize;
  input S_AXI_awvalid;
  output [5:0]S_AXI_bid;
  input S_AXI_bready;
  output [1:0]S_AXI_bresp;
  output S_AXI_bvalid;
  output [511:0]S_AXI_rdata;
  output [5:0]S_AXI_rid;
  output S_AXI_rlast;
  input S_AXI_rready;
  output [1:0]S_AXI_rresp;
  output S_AXI_rvalid;
  input [511:0]S_AXI_wdata;
  input S_AXI_wlast;
  output S_AXI_wready;
  input [63:0]S_AXI_wstrb;
  input S_AXI_wvalid;

  wire S_ACLK_1;
  wire S_ARESETN_1;
  wire [63:0]s00_couplers_to_s00_regslice_ARADDR;
  wire [1:0]s00_couplers_to_s00_regslice_ARBURST;
  wire [3:0]s00_couplers_to_s00_regslice_ARCACHE;
  wire [5:0]s00_couplers_to_s00_regslice_ARID;
  wire [7:0]s00_couplers_to_s00_regslice_ARLEN;
  wire [0:0]s00_couplers_to_s00_regslice_ARLOCK;
  wire [2:0]s00_couplers_to_s00_regslice_ARPROT;
  wire [3:0]s00_couplers_to_s00_regslice_ARQOS;
  wire s00_couplers_to_s00_regslice_ARREADY;
  wire [3:0]s00_couplers_to_s00_regslice_ARREGION;
  wire [2:0]s00_couplers_to_s00_regslice_ARSIZE;
  wire s00_couplers_to_s00_regslice_ARVALID;
  wire [63:0]s00_couplers_to_s00_regslice_AWADDR;
  wire [1:0]s00_couplers_to_s00_regslice_AWBURST;
  wire [3:0]s00_couplers_to_s00_regslice_AWCACHE;
  wire [5:0]s00_couplers_to_s00_regslice_AWID;
  wire [7:0]s00_couplers_to_s00_regslice_AWLEN;
  wire [0:0]s00_couplers_to_s00_regslice_AWLOCK;
  wire [2:0]s00_couplers_to_s00_regslice_AWPROT;
  wire [3:0]s00_couplers_to_s00_regslice_AWQOS;
  wire s00_couplers_to_s00_regslice_AWREADY;
  wire [3:0]s00_couplers_to_s00_regslice_AWREGION;
  wire [2:0]s00_couplers_to_s00_regslice_AWSIZE;
  wire s00_couplers_to_s00_regslice_AWVALID;
  wire [5:0]s00_couplers_to_s00_regslice_BID;
  wire s00_couplers_to_s00_regslice_BREADY;
  wire [1:0]s00_couplers_to_s00_regslice_BRESP;
  wire s00_couplers_to_s00_regslice_BVALID;
  wire [511:0]s00_couplers_to_s00_regslice_RDATA;
  wire [5:0]s00_couplers_to_s00_regslice_RID;
  wire s00_couplers_to_s00_regslice_RLAST;
  wire s00_couplers_to_s00_regslice_RREADY;
  wire [1:0]s00_couplers_to_s00_regslice_RRESP;
  wire s00_couplers_to_s00_regslice_RVALID;
  wire [511:0]s00_couplers_to_s00_regslice_WDATA;
  wire s00_couplers_to_s00_regslice_WLAST;
  wire s00_couplers_to_s00_regslice_WREADY;
  wire [63:0]s00_couplers_to_s00_regslice_WSTRB;
  wire s00_couplers_to_s00_regslice_WVALID;
  wire [63:0]s00_regslice_to_s00_couplers_ARADDR;
  wire [1:0]s00_regslice_to_s00_couplers_ARBURST;
  wire [3:0]s00_regslice_to_s00_couplers_ARCACHE;
  wire [5:0]s00_regslice_to_s00_couplers_ARID;
  wire [7:0]s00_regslice_to_s00_couplers_ARLEN;
  wire [0:0]s00_regslice_to_s00_couplers_ARLOCK;
  wire [2:0]s00_regslice_to_s00_couplers_ARPROT;
  wire [3:0]s00_regslice_to_s00_couplers_ARQOS;
  wire s00_regslice_to_s00_couplers_ARREADY;
  wire [2:0]s00_regslice_to_s00_couplers_ARSIZE;
  wire s00_regslice_to_s00_couplers_ARVALID;
  wire [63:0]s00_regslice_to_s00_couplers_AWADDR;
  wire [1:0]s00_regslice_to_s00_couplers_AWBURST;
  wire [3:0]s00_regslice_to_s00_couplers_AWCACHE;
  wire [5:0]s00_regslice_to_s00_couplers_AWID;
  wire [7:0]s00_regslice_to_s00_couplers_AWLEN;
  wire [0:0]s00_regslice_to_s00_couplers_AWLOCK;
  wire [2:0]s00_regslice_to_s00_couplers_AWPROT;
  wire [3:0]s00_regslice_to_s00_couplers_AWQOS;
  wire s00_regslice_to_s00_couplers_AWREADY;
  wire [2:0]s00_regslice_to_s00_couplers_AWSIZE;
  wire s00_regslice_to_s00_couplers_AWVALID;
  wire [5:0]s00_regslice_to_s00_couplers_BID;
  wire s00_regslice_to_s00_couplers_BREADY;
  wire [1:0]s00_regslice_to_s00_couplers_BRESP;
  wire s00_regslice_to_s00_couplers_BVALID;
  wire [511:0]s00_regslice_to_s00_couplers_RDATA;
  wire [5:0]s00_regslice_to_s00_couplers_RID;
  wire s00_regslice_to_s00_couplers_RLAST;
  wire s00_regslice_to_s00_couplers_RREADY;
  wire [1:0]s00_regslice_to_s00_couplers_RRESP;
  wire s00_regslice_to_s00_couplers_RVALID;
  wire [511:0]s00_regslice_to_s00_couplers_WDATA;
  wire s00_regslice_to_s00_couplers_WLAST;
  wire s00_regslice_to_s00_couplers_WREADY;
  wire [63:0]s00_regslice_to_s00_couplers_WSTRB;
  wire s00_regslice_to_s00_couplers_WVALID;

  assign M_AXI_araddr[63:0] = s00_regslice_to_s00_couplers_ARADDR;
  assign M_AXI_arburst[1:0] = s00_regslice_to_s00_couplers_ARBURST;
  assign M_AXI_arcache[3:0] = s00_regslice_to_s00_couplers_ARCACHE;
  assign M_AXI_arid[5:0] = s00_regslice_to_s00_couplers_ARID;
  assign M_AXI_arlen[7:0] = s00_regslice_to_s00_couplers_ARLEN;
  assign M_AXI_arlock[0] = s00_regslice_to_s00_couplers_ARLOCK;
  assign M_AXI_arprot[2:0] = s00_regslice_to_s00_couplers_ARPROT;
  assign M_AXI_arqos[3:0] = s00_regslice_to_s00_couplers_ARQOS;
  assign M_AXI_arsize[2:0] = s00_regslice_to_s00_couplers_ARSIZE;
  assign M_AXI_arvalid = s00_regslice_to_s00_couplers_ARVALID;
  assign M_AXI_awaddr[63:0] = s00_regslice_to_s00_couplers_AWADDR;
  assign M_AXI_awburst[1:0] = s00_regslice_to_s00_couplers_AWBURST;
  assign M_AXI_awcache[3:0] = s00_regslice_to_s00_couplers_AWCACHE;
  assign M_AXI_awid[5:0] = s00_regslice_to_s00_couplers_AWID;
  assign M_AXI_awlen[7:0] = s00_regslice_to_s00_couplers_AWLEN;
  assign M_AXI_awlock[0] = s00_regslice_to_s00_couplers_AWLOCK;
  assign M_AXI_awprot[2:0] = s00_regslice_to_s00_couplers_AWPROT;
  assign M_AXI_awqos[3:0] = s00_regslice_to_s00_couplers_AWQOS;
  assign M_AXI_awsize[2:0] = s00_regslice_to_s00_couplers_AWSIZE;
  assign M_AXI_awvalid = s00_regslice_to_s00_couplers_AWVALID;
  assign M_AXI_bready = s00_regslice_to_s00_couplers_BREADY;
  assign M_AXI_rready = s00_regslice_to_s00_couplers_RREADY;
  assign M_AXI_wdata[511:0] = s00_regslice_to_s00_couplers_WDATA;
  assign M_AXI_wlast = s00_regslice_to_s00_couplers_WLAST;
  assign M_AXI_wstrb[63:0] = s00_regslice_to_s00_couplers_WSTRB;
  assign M_AXI_wvalid = s00_regslice_to_s00_couplers_WVALID;
  assign S_ACLK_1 = S_ACLK;
  assign S_ARESETN_1 = S_ARESETN;
  assign S_AXI_arready = s00_couplers_to_s00_regslice_ARREADY;
  assign S_AXI_awready = s00_couplers_to_s00_regslice_AWREADY;
  assign S_AXI_bid[5:0] = s00_couplers_to_s00_regslice_BID;
  assign S_AXI_bresp[1:0] = s00_couplers_to_s00_regslice_BRESP;
  assign S_AXI_bvalid = s00_couplers_to_s00_regslice_BVALID;
  assign S_AXI_rdata[511:0] = s00_couplers_to_s00_regslice_RDATA;
  assign S_AXI_rid[5:0] = s00_couplers_to_s00_regslice_RID;
  assign S_AXI_rlast = s00_couplers_to_s00_regslice_RLAST;
  assign S_AXI_rresp[1:0] = s00_couplers_to_s00_regslice_RRESP;
  assign S_AXI_rvalid = s00_couplers_to_s00_regslice_RVALID;
  assign S_AXI_wready = s00_couplers_to_s00_regslice_WREADY;
  assign s00_couplers_to_s00_regslice_ARADDR = S_AXI_araddr[63:0];
  assign s00_couplers_to_s00_regslice_ARBURST = S_AXI_arburst[1:0];
  assign s00_couplers_to_s00_regslice_ARCACHE = S_AXI_arcache[3:0];
  assign s00_couplers_to_s00_regslice_ARID = S_AXI_arid[5:0];
  assign s00_couplers_to_s00_regslice_ARLEN = S_AXI_arlen[7:0];
  assign s00_couplers_to_s00_regslice_ARLOCK = S_AXI_arlock[0];
  assign s00_couplers_to_s00_regslice_ARPROT = S_AXI_arprot[2:0];
  assign s00_couplers_to_s00_regslice_ARQOS = S_AXI_arqos[3:0];
  assign s00_couplers_to_s00_regslice_ARREGION = S_AXI_arregion[3:0];
  assign s00_couplers_to_s00_regslice_ARSIZE = S_AXI_arsize[2:0];
  assign s00_couplers_to_s00_regslice_ARVALID = S_AXI_arvalid;
  assign s00_couplers_to_s00_regslice_AWADDR = S_AXI_awaddr[63:0];
  assign s00_couplers_to_s00_regslice_AWBURST = S_AXI_awburst[1:0];
  assign s00_couplers_to_s00_regslice_AWCACHE = S_AXI_awcache[3:0];
  assign s00_couplers_to_s00_regslice_AWID = S_AXI_awid[5:0];
  assign s00_couplers_to_s00_regslice_AWLEN = S_AXI_awlen[7:0];
  assign s00_couplers_to_s00_regslice_AWLOCK = S_AXI_awlock[0];
  assign s00_couplers_to_s00_regslice_AWPROT = S_AXI_awprot[2:0];
  assign s00_couplers_to_s00_regslice_AWQOS = S_AXI_awqos[3:0];
  assign s00_couplers_to_s00_regslice_AWREGION = S_AXI_awregion[3:0];
  assign s00_couplers_to_s00_regslice_AWSIZE = S_AXI_awsize[2:0];
  assign s00_couplers_to_s00_regslice_AWVALID = S_AXI_awvalid;
  assign s00_couplers_to_s00_regslice_BREADY = S_AXI_bready;
  assign s00_couplers_to_s00_regslice_RREADY = S_AXI_rready;
  assign s00_couplers_to_s00_regslice_WDATA = S_AXI_wdata[511:0];
  assign s00_couplers_to_s00_regslice_WLAST = S_AXI_wlast;
  assign s00_couplers_to_s00_regslice_WSTRB = S_AXI_wstrb[63:0];
  assign s00_couplers_to_s00_regslice_WVALID = S_AXI_wvalid;
  assign s00_regslice_to_s00_couplers_ARREADY = M_AXI_arready;
  assign s00_regslice_to_s00_couplers_AWREADY = M_AXI_awready;
  assign s00_regslice_to_s00_couplers_BID = M_AXI_bid[5:0];
  assign s00_regslice_to_s00_couplers_BRESP = M_AXI_bresp[1:0];
  assign s00_regslice_to_s00_couplers_BVALID = M_AXI_bvalid;
  assign s00_regslice_to_s00_couplers_RDATA = M_AXI_rdata[511:0];
  assign s00_regslice_to_s00_couplers_RID = M_AXI_rid[5:0];
  assign s00_regslice_to_s00_couplers_RLAST = M_AXI_rlast;
  assign s00_regslice_to_s00_couplers_RRESP = M_AXI_rresp[1:0];
  assign s00_regslice_to_s00_couplers_RVALID = M_AXI_rvalid;
  assign s00_regslice_to_s00_couplers_WREADY = M_AXI_wready;
  cl_xbar_s00_regslice_2 s00_regslice
       (.aclk(S_ACLK_1),
        .aresetn(S_ARESETN_1),
        .m_axi_araddr(s00_regslice_to_s00_couplers_ARADDR),
        .m_axi_arburst(s00_regslice_to_s00_couplers_ARBURST),
        .m_axi_arcache(s00_regslice_to_s00_couplers_ARCACHE),
        .m_axi_arid(s00_regslice_to_s00_couplers_ARID),
        .m_axi_arlen(s00_regslice_to_s00_couplers_ARLEN),
        .m_axi_arlock(s00_regslice_to_s00_couplers_ARLOCK),
        .m_axi_arprot(s00_regslice_to_s00_couplers_ARPROT),
        .m_axi_arqos(s00_regslice_to_s00_couplers_ARQOS),
        .m_axi_arready(s00_regslice_to_s00_couplers_ARREADY),
        .m_axi_arsize(s00_regslice_to_s00_couplers_ARSIZE),
        .m_axi_arvalid(s00_regslice_to_s00_couplers_ARVALID),
        .m_axi_awaddr(s00_regslice_to_s00_couplers_AWADDR),
        .m_axi_awburst(s00_regslice_to_s00_couplers_AWBURST),
        .m_axi_awcache(s00_regslice_to_s00_couplers_AWCACHE),
        .m_axi_awid(s00_regslice_to_s00_couplers_AWID),
        .m_axi_awlen(s00_regslice_to_s00_couplers_AWLEN),
        .m_axi_awlock(s00_regslice_to_s00_couplers_AWLOCK),
        .m_axi_awprot(s00_regslice_to_s00_couplers_AWPROT),
        .m_axi_awqos(s00_regslice_to_s00_couplers_AWQOS),
        .m_axi_awready(s00_regslice_to_s00_couplers_AWREADY),
        .m_axi_awsize(s00_regslice_to_s00_couplers_AWSIZE),
        .m_axi_awvalid(s00_regslice_to_s00_couplers_AWVALID),
        .m_axi_bid(s00_regslice_to_s00_couplers_BID),
        .m_axi_bready(s00_regslice_to_s00_couplers_BREADY),
        .m_axi_bresp(s00_regslice_to_s00_couplers_BRESP),
        .m_axi_bvalid(s00_regslice_to_s00_couplers_BVALID),
        .m_axi_rdata(s00_regslice_to_s00_couplers_RDATA),
        .m_axi_rid(s00_regslice_to_s00_couplers_RID),
        .m_axi_rlast(s00_regslice_to_s00_couplers_RLAST),
        .m_axi_rready(s00_regslice_to_s00_couplers_RREADY),
        .m_axi_rresp(s00_regslice_to_s00_couplers_RRESP),
        .m_axi_rvalid(s00_regslice_to_s00_couplers_RVALID),
        .m_axi_wdata(s00_regslice_to_s00_couplers_WDATA),
        .m_axi_wlast(s00_regslice_to_s00_couplers_WLAST),
        .m_axi_wready(s00_regslice_to_s00_couplers_WREADY),
        .m_axi_wstrb(s00_regslice_to_s00_couplers_WSTRB),
        .m_axi_wvalid(s00_regslice_to_s00_couplers_WVALID),
        .s_axi_araddr(s00_couplers_to_s00_regslice_ARADDR),
        .s_axi_arburst(s00_couplers_to_s00_regslice_ARBURST),
        .s_axi_arcache(s00_couplers_to_s00_regslice_ARCACHE),
        .s_axi_arid(s00_couplers_to_s00_regslice_ARID),
        .s_axi_arlen(s00_couplers_to_s00_regslice_ARLEN),
        .s_axi_arlock(s00_couplers_to_s00_regslice_ARLOCK),
        .s_axi_arprot(s00_couplers_to_s00_regslice_ARPROT),
        .s_axi_arqos(s00_couplers_to_s00_regslice_ARQOS),
        .s_axi_arready(s00_couplers_to_s00_regslice_ARREADY),
        .s_axi_arregion(s00_couplers_to_s00_regslice_ARREGION),
        .s_axi_arsize(s00_couplers_to_s00_regslice_ARSIZE),
        .s_axi_arvalid(s00_couplers_to_s00_regslice_ARVALID),
        .s_axi_awaddr(s00_couplers_to_s00_regslice_AWADDR),
        .s_axi_awburst(s00_couplers_to_s00_regslice_AWBURST),
        .s_axi_awcache(s00_couplers_to_s00_regslice_AWCACHE),
        .s_axi_awid(s00_couplers_to_s00_regslice_AWID),
        .s_axi_awlen(s00_couplers_to_s00_regslice_AWLEN),
        .s_axi_awlock(s00_couplers_to_s00_regslice_AWLOCK),
        .s_axi_awprot(s00_couplers_to_s00_regslice_AWPROT),
        .s_axi_awqos(s00_couplers_to_s00_regslice_AWQOS),
        .s_axi_awready(s00_couplers_to_s00_regslice_AWREADY),
        .s_axi_awregion(s00_couplers_to_s00_regslice_AWREGION),
        .s_axi_awsize(s00_couplers_to_s00_regslice_AWSIZE),
        .s_axi_awvalid(s00_couplers_to_s00_regslice_AWVALID),
        .s_axi_bid(s00_couplers_to_s00_regslice_BID),
        .s_axi_bready(s00_couplers_to_s00_regslice_BREADY),
        .s_axi_bresp(s00_couplers_to_s00_regslice_BRESP),
        .s_axi_bvalid(s00_couplers_to_s00_regslice_BVALID),
        .s_axi_rdata(s00_couplers_to_s00_regslice_RDATA),
        .s_axi_rid(s00_couplers_to_s00_regslice_RID),
        .s_axi_rlast(s00_couplers_to_s00_regslice_RLAST),
        .s_axi_rready(s00_couplers_to_s00_regslice_RREADY),
        .s_axi_rresp(s00_couplers_to_s00_regslice_RRESP),
        .s_axi_rvalid(s00_couplers_to_s00_regslice_RVALID),
        .s_axi_wdata(s00_couplers_to_s00_regslice_WDATA),
        .s_axi_wlast(s00_couplers_to_s00_regslice_WLAST),
        .s_axi_wready(s00_couplers_to_s00_regslice_WREADY),
        .s_axi_wstrb(s00_couplers_to_s00_regslice_WSTRB),
        .s_axi_wvalid(s00_couplers_to_s00_regslice_WVALID));
endmodule

module s01_couplers_imp_JB2BAD
   (M_ACLK,
    M_ARESETN,
    M_AXI_araddr,
    M_AXI_arburst,
    M_AXI_arcache,
    M_AXI_arid,
    M_AXI_arlen,
    M_AXI_arlock,
    M_AXI_arprot,
    M_AXI_arqos,
    M_AXI_arready,
    M_AXI_arsize,
    M_AXI_arvalid,
    M_AXI_awaddr,
    M_AXI_awburst,
    M_AXI_awcache,
    M_AXI_awid,
    M_AXI_awlen,
    M_AXI_awlock,
    M_AXI_awprot,
    M_AXI_awqos,
    M_AXI_awready,
    M_AXI_awsize,
    M_AXI_awvalid,
    M_AXI_bid,
    M_AXI_bready,
    M_AXI_bresp,
    M_AXI_bvalid,
    M_AXI_rdata,
    M_AXI_rid,
    M_AXI_rlast,
    M_AXI_rready,
    M_AXI_rresp,
    M_AXI_rvalid,
    M_AXI_wdata,
    M_AXI_wlast,
    M_AXI_wready,
    M_AXI_wstrb,
    M_AXI_wvalid,
    S_ACLK,
    S_ARESETN,
    S_AXI_araddr,
    S_AXI_arburst,
    S_AXI_arcache,
    S_AXI_arid,
    S_AXI_arlen,
    S_AXI_arlock,
    S_AXI_arprot,
    S_AXI_arqos,
    S_AXI_arready,
    S_AXI_arregion,
    S_AXI_arsize,
    S_AXI_arvalid,
    S_AXI_awaddr,
    S_AXI_awburst,
    S_AXI_awcache,
    S_AXI_awid,
    S_AXI_awlen,
    S_AXI_awlock,
    S_AXI_awprot,
    S_AXI_awqos,
    S_AXI_awready,
    S_AXI_awregion,
    S_AXI_awsize,
    S_AXI_awvalid,
    S_AXI_bid,
    S_AXI_bready,
    S_AXI_bresp,
    S_AXI_bvalid,
    S_AXI_rdata,
    S_AXI_rid,
    S_AXI_rlast,
    S_AXI_rready,
    S_AXI_rresp,
    S_AXI_rvalid,
    S_AXI_wdata,
    S_AXI_wlast,
    S_AXI_wready,
    S_AXI_wstrb,
    S_AXI_wvalid);
  input M_ACLK;
  input M_ARESETN;
  output [63:0]M_AXI_araddr;
  output [1:0]M_AXI_arburst;
  output [3:0]M_AXI_arcache;
  output [5:0]M_AXI_arid;
  output [7:0]M_AXI_arlen;
  output [0:0]M_AXI_arlock;
  output [2:0]M_AXI_arprot;
  output [3:0]M_AXI_arqos;
  input M_AXI_arready;
  output [2:0]M_AXI_arsize;
  output M_AXI_arvalid;
  output [63:0]M_AXI_awaddr;
  output [1:0]M_AXI_awburst;
  output [3:0]M_AXI_awcache;
  output [5:0]M_AXI_awid;
  output [7:0]M_AXI_awlen;
  output [0:0]M_AXI_awlock;
  output [2:0]M_AXI_awprot;
  output [3:0]M_AXI_awqos;
  input M_AXI_awready;
  output [2:0]M_AXI_awsize;
  output M_AXI_awvalid;
  input [6:0]M_AXI_bid;
  output M_AXI_bready;
  input [1:0]M_AXI_bresp;
  input M_AXI_bvalid;
  input [511:0]M_AXI_rdata;
  input [6:0]M_AXI_rid;
  input M_AXI_rlast;
  output M_AXI_rready;
  input [1:0]M_AXI_rresp;
  input M_AXI_rvalid;
  output [511:0]M_AXI_wdata;
  output M_AXI_wlast;
  input M_AXI_wready;
  output [63:0]M_AXI_wstrb;
  output M_AXI_wvalid;
  input S_ACLK;
  input S_ARESETN;
  input [63:0]S_AXI_araddr;
  input [1:0]S_AXI_arburst;
  input [3:0]S_AXI_arcache;
  input [5:0]S_AXI_arid;
  input [7:0]S_AXI_arlen;
  input [0:0]S_AXI_arlock;
  input [2:0]S_AXI_arprot;
  input [3:0]S_AXI_arqos;
  output S_AXI_arready;
  input [3:0]S_AXI_arregion;
  input [2:0]S_AXI_arsize;
  input S_AXI_arvalid;
  input [63:0]S_AXI_awaddr;
  input [1:0]S_AXI_awburst;
  input [3:0]S_AXI_awcache;
  input [5:0]S_AXI_awid;
  input [7:0]S_AXI_awlen;
  input [0:0]S_AXI_awlock;
  input [2:0]S_AXI_awprot;
  input [3:0]S_AXI_awqos;
  output S_AXI_awready;
  input [3:0]S_AXI_awregion;
  input [2:0]S_AXI_awsize;
  input S_AXI_awvalid;
  output [5:0]S_AXI_bid;
  input S_AXI_bready;
  output [1:0]S_AXI_bresp;
  output S_AXI_bvalid;
  output [511:0]S_AXI_rdata;
  output [5:0]S_AXI_rid;
  output S_AXI_rlast;
  input S_AXI_rready;
  output [1:0]S_AXI_rresp;
  output S_AXI_rvalid;
  input [511:0]S_AXI_wdata;
  input S_AXI_wlast;
  output S_AXI_wready;
  input [63:0]S_AXI_wstrb;
  input S_AXI_wvalid;

  wire S_ACLK_1;
  wire S_ARESETN_1;
  wire [63:0]s01_couplers_to_s01_regslice_ARADDR;
  wire [1:0]s01_couplers_to_s01_regslice_ARBURST;
  wire [3:0]s01_couplers_to_s01_regslice_ARCACHE;
  wire [5:0]s01_couplers_to_s01_regslice_ARID;
  wire [7:0]s01_couplers_to_s01_regslice_ARLEN;
  wire [0:0]s01_couplers_to_s01_regslice_ARLOCK;
  wire [2:0]s01_couplers_to_s01_regslice_ARPROT;
  wire [3:0]s01_couplers_to_s01_regslice_ARQOS;
  wire s01_couplers_to_s01_regslice_ARREADY;
  wire [3:0]s01_couplers_to_s01_regslice_ARREGION;
  wire [2:0]s01_couplers_to_s01_regslice_ARSIZE;
  wire s01_couplers_to_s01_regslice_ARVALID;
  wire [63:0]s01_couplers_to_s01_regslice_AWADDR;
  wire [1:0]s01_couplers_to_s01_regslice_AWBURST;
  wire [3:0]s01_couplers_to_s01_regslice_AWCACHE;
  wire [5:0]s01_couplers_to_s01_regslice_AWID;
  wire [7:0]s01_couplers_to_s01_regslice_AWLEN;
  wire [0:0]s01_couplers_to_s01_regslice_AWLOCK;
  wire [2:0]s01_couplers_to_s01_regslice_AWPROT;
  wire [3:0]s01_couplers_to_s01_regslice_AWQOS;
  wire s01_couplers_to_s01_regslice_AWREADY;
  wire [3:0]s01_couplers_to_s01_regslice_AWREGION;
  wire [2:0]s01_couplers_to_s01_regslice_AWSIZE;
  wire s01_couplers_to_s01_regslice_AWVALID;
  wire [5:0]s01_couplers_to_s01_regslice_BID;
  wire s01_couplers_to_s01_regslice_BREADY;
  wire [1:0]s01_couplers_to_s01_regslice_BRESP;
  wire s01_couplers_to_s01_regslice_BVALID;
  wire [511:0]s01_couplers_to_s01_regslice_RDATA;
  wire [5:0]s01_couplers_to_s01_regslice_RID;
  wire s01_couplers_to_s01_regslice_RLAST;
  wire s01_couplers_to_s01_regslice_RREADY;
  wire [1:0]s01_couplers_to_s01_regslice_RRESP;
  wire s01_couplers_to_s01_regslice_RVALID;
  wire [511:0]s01_couplers_to_s01_regslice_WDATA;
  wire s01_couplers_to_s01_regslice_WLAST;
  wire s01_couplers_to_s01_regslice_WREADY;
  wire [63:0]s01_couplers_to_s01_regslice_WSTRB;
  wire s01_couplers_to_s01_regslice_WVALID;
  wire [63:0]s01_regslice_to_s01_couplers_ARADDR;
  wire [1:0]s01_regslice_to_s01_couplers_ARBURST;
  wire [3:0]s01_regslice_to_s01_couplers_ARCACHE;
  wire [5:0]s01_regslice_to_s01_couplers_ARID;
  wire [7:0]s01_regslice_to_s01_couplers_ARLEN;
  wire [0:0]s01_regslice_to_s01_couplers_ARLOCK;
  wire [2:0]s01_regslice_to_s01_couplers_ARPROT;
  wire [3:0]s01_regslice_to_s01_couplers_ARQOS;
  wire s01_regslice_to_s01_couplers_ARREADY;
  wire [2:0]s01_regslice_to_s01_couplers_ARSIZE;
  wire s01_regslice_to_s01_couplers_ARVALID;
  wire [63:0]s01_regslice_to_s01_couplers_AWADDR;
  wire [1:0]s01_regslice_to_s01_couplers_AWBURST;
  wire [3:0]s01_regslice_to_s01_couplers_AWCACHE;
  wire [5:0]s01_regslice_to_s01_couplers_AWID;
  wire [7:0]s01_regslice_to_s01_couplers_AWLEN;
  wire [0:0]s01_regslice_to_s01_couplers_AWLOCK;
  wire [2:0]s01_regslice_to_s01_couplers_AWPROT;
  wire [3:0]s01_regslice_to_s01_couplers_AWQOS;
  wire s01_regslice_to_s01_couplers_AWREADY;
  wire [2:0]s01_regslice_to_s01_couplers_AWSIZE;
  wire s01_regslice_to_s01_couplers_AWVALID;
  wire [6:0]s01_regslice_to_s01_couplers_BID;
  wire s01_regslice_to_s01_couplers_BREADY;
  wire [1:0]s01_regslice_to_s01_couplers_BRESP;
  wire s01_regslice_to_s01_couplers_BVALID;
  wire [511:0]s01_regslice_to_s01_couplers_RDATA;
  wire [6:0]s01_regslice_to_s01_couplers_RID;
  wire s01_regslice_to_s01_couplers_RLAST;
  wire s01_regslice_to_s01_couplers_RREADY;
  wire [1:0]s01_regslice_to_s01_couplers_RRESP;
  wire s01_regslice_to_s01_couplers_RVALID;
  wire [511:0]s01_regslice_to_s01_couplers_WDATA;
  wire s01_regslice_to_s01_couplers_WLAST;
  wire s01_regslice_to_s01_couplers_WREADY;
  wire [63:0]s01_regslice_to_s01_couplers_WSTRB;
  wire s01_regslice_to_s01_couplers_WVALID;

  assign M_AXI_araddr[63:0] = s01_regslice_to_s01_couplers_ARADDR;
  assign M_AXI_arburst[1:0] = s01_regslice_to_s01_couplers_ARBURST;
  assign M_AXI_arcache[3:0] = s01_regslice_to_s01_couplers_ARCACHE;
  assign M_AXI_arid[5:0] = s01_regslice_to_s01_couplers_ARID;
  assign M_AXI_arlen[7:0] = s01_regslice_to_s01_couplers_ARLEN;
  assign M_AXI_arlock[0] = s01_regslice_to_s01_couplers_ARLOCK;
  assign M_AXI_arprot[2:0] = s01_regslice_to_s01_couplers_ARPROT;
  assign M_AXI_arqos[3:0] = s01_regslice_to_s01_couplers_ARQOS;
  assign M_AXI_arsize[2:0] = s01_regslice_to_s01_couplers_ARSIZE;
  assign M_AXI_arvalid = s01_regslice_to_s01_couplers_ARVALID;
  assign M_AXI_awaddr[63:0] = s01_regslice_to_s01_couplers_AWADDR;
  assign M_AXI_awburst[1:0] = s01_regslice_to_s01_couplers_AWBURST;
  assign M_AXI_awcache[3:0] = s01_regslice_to_s01_couplers_AWCACHE;
  assign M_AXI_awid[5:0] = s01_regslice_to_s01_couplers_AWID;
  assign M_AXI_awlen[7:0] = s01_regslice_to_s01_couplers_AWLEN;
  assign M_AXI_awlock[0] = s01_regslice_to_s01_couplers_AWLOCK;
  assign M_AXI_awprot[2:0] = s01_regslice_to_s01_couplers_AWPROT;
  assign M_AXI_awqos[3:0] = s01_regslice_to_s01_couplers_AWQOS;
  assign M_AXI_awsize[2:0] = s01_regslice_to_s01_couplers_AWSIZE;
  assign M_AXI_awvalid = s01_regslice_to_s01_couplers_AWVALID;
  assign M_AXI_bready = s01_regslice_to_s01_couplers_BREADY;
  assign M_AXI_rready = s01_regslice_to_s01_couplers_RREADY;
  assign M_AXI_wdata[511:0] = s01_regslice_to_s01_couplers_WDATA;
  assign M_AXI_wlast = s01_regslice_to_s01_couplers_WLAST;
  assign M_AXI_wstrb[63:0] = s01_regslice_to_s01_couplers_WSTRB;
  assign M_AXI_wvalid = s01_regslice_to_s01_couplers_WVALID;
  assign S_ACLK_1 = S_ACLK;
  assign S_ARESETN_1 = S_ARESETN;
  assign S_AXI_arready = s01_couplers_to_s01_regslice_ARREADY;
  assign S_AXI_awready = s01_couplers_to_s01_regslice_AWREADY;
  assign S_AXI_bid[5:0] = s01_couplers_to_s01_regslice_BID;
  assign S_AXI_bresp[1:0] = s01_couplers_to_s01_regslice_BRESP;
  assign S_AXI_bvalid = s01_couplers_to_s01_regslice_BVALID;
  assign S_AXI_rdata[511:0] = s01_couplers_to_s01_regslice_RDATA;
  assign S_AXI_rid[5:0] = s01_couplers_to_s01_regslice_RID;
  assign S_AXI_rlast = s01_couplers_to_s01_regslice_RLAST;
  assign S_AXI_rresp[1:0] = s01_couplers_to_s01_regslice_RRESP;
  assign S_AXI_rvalid = s01_couplers_to_s01_regslice_RVALID;
  assign S_AXI_wready = s01_couplers_to_s01_regslice_WREADY;
  assign s01_couplers_to_s01_regslice_ARADDR = S_AXI_araddr[63:0];
  assign s01_couplers_to_s01_regslice_ARBURST = S_AXI_arburst[1:0];
  assign s01_couplers_to_s01_regslice_ARCACHE = S_AXI_arcache[3:0];
  assign s01_couplers_to_s01_regslice_ARID = S_AXI_arid[5:0];
  assign s01_couplers_to_s01_regslice_ARLEN = S_AXI_arlen[7:0];
  assign s01_couplers_to_s01_regslice_ARLOCK = S_AXI_arlock[0];
  assign s01_couplers_to_s01_regslice_ARPROT = S_AXI_arprot[2:0];
  assign s01_couplers_to_s01_regslice_ARQOS = S_AXI_arqos[3:0];
  assign s01_couplers_to_s01_regslice_ARREGION = S_AXI_arregion[3:0];
  assign s01_couplers_to_s01_regslice_ARSIZE = S_AXI_arsize[2:0];
  assign s01_couplers_to_s01_regslice_ARVALID = S_AXI_arvalid;
  assign s01_couplers_to_s01_regslice_AWADDR = S_AXI_awaddr[63:0];
  assign s01_couplers_to_s01_regslice_AWBURST = S_AXI_awburst[1:0];
  assign s01_couplers_to_s01_regslice_AWCACHE = S_AXI_awcache[3:0];
  assign s01_couplers_to_s01_regslice_AWID = S_AXI_awid[5:0];
  assign s01_couplers_to_s01_regslice_AWLEN = S_AXI_awlen[7:0];
  assign s01_couplers_to_s01_regslice_AWLOCK = S_AXI_awlock[0];
  assign s01_couplers_to_s01_regslice_AWPROT = S_AXI_awprot[2:0];
  assign s01_couplers_to_s01_regslice_AWQOS = S_AXI_awqos[3:0];
  assign s01_couplers_to_s01_regslice_AWREGION = S_AXI_awregion[3:0];
  assign s01_couplers_to_s01_regslice_AWSIZE = S_AXI_awsize[2:0];
  assign s01_couplers_to_s01_regslice_AWVALID = S_AXI_awvalid;
  assign s01_couplers_to_s01_regslice_BREADY = S_AXI_bready;
  assign s01_couplers_to_s01_regslice_RREADY = S_AXI_rready;
  assign s01_couplers_to_s01_regslice_WDATA = S_AXI_wdata[511:0];
  assign s01_couplers_to_s01_regslice_WLAST = S_AXI_wlast;
  assign s01_couplers_to_s01_regslice_WSTRB = S_AXI_wstrb[63:0];
  assign s01_couplers_to_s01_regslice_WVALID = S_AXI_wvalid;
  assign s01_regslice_to_s01_couplers_ARREADY = M_AXI_arready;
  assign s01_regslice_to_s01_couplers_AWREADY = M_AXI_awready;
  assign s01_regslice_to_s01_couplers_BID = M_AXI_bid[6:0];
  assign s01_regslice_to_s01_couplers_BRESP = M_AXI_bresp[1:0];
  assign s01_regslice_to_s01_couplers_BVALID = M_AXI_bvalid;
  assign s01_regslice_to_s01_couplers_RDATA = M_AXI_rdata[511:0];
  assign s01_regslice_to_s01_couplers_RID = M_AXI_rid[6:0];
  assign s01_regslice_to_s01_couplers_RLAST = M_AXI_rlast;
  assign s01_regslice_to_s01_couplers_RRESP = M_AXI_rresp[1:0];
  assign s01_regslice_to_s01_couplers_RVALID = M_AXI_rvalid;
  assign s01_regslice_to_s01_couplers_WREADY = M_AXI_wready;
  cl_xbar_s01_regslice_0 s01_regslice
       (.aclk(S_ACLK_1),
        .aresetn(S_ARESETN_1),
        .m_axi_araddr(s01_regslice_to_s01_couplers_ARADDR),
        .m_axi_arburst(s01_regslice_to_s01_couplers_ARBURST),
        .m_axi_arcache(s01_regslice_to_s01_couplers_ARCACHE),
        .m_axi_arid(s01_regslice_to_s01_couplers_ARID),
        .m_axi_arlen(s01_regslice_to_s01_couplers_ARLEN),
        .m_axi_arlock(s01_regslice_to_s01_couplers_ARLOCK),
        .m_axi_arprot(s01_regslice_to_s01_couplers_ARPROT),
        .m_axi_arqos(s01_regslice_to_s01_couplers_ARQOS),
        .m_axi_arready(s01_regslice_to_s01_couplers_ARREADY),
        .m_axi_arsize(s01_regslice_to_s01_couplers_ARSIZE),
        .m_axi_arvalid(s01_regslice_to_s01_couplers_ARVALID),
        .m_axi_awaddr(s01_regslice_to_s01_couplers_AWADDR),
        .m_axi_awburst(s01_regslice_to_s01_couplers_AWBURST),
        .m_axi_awcache(s01_regslice_to_s01_couplers_AWCACHE),
        .m_axi_awid(s01_regslice_to_s01_couplers_AWID),
        .m_axi_awlen(s01_regslice_to_s01_couplers_AWLEN),
        .m_axi_awlock(s01_regslice_to_s01_couplers_AWLOCK),
        .m_axi_awprot(s01_regslice_to_s01_couplers_AWPROT),
        .m_axi_awqos(s01_regslice_to_s01_couplers_AWQOS),
        .m_axi_awready(s01_regslice_to_s01_couplers_AWREADY),
        .m_axi_awsize(s01_regslice_to_s01_couplers_AWSIZE),
        .m_axi_awvalid(s01_regslice_to_s01_couplers_AWVALID),
        .m_axi_bid(s01_regslice_to_s01_couplers_BID[5:0]),
        .m_axi_bready(s01_regslice_to_s01_couplers_BREADY),
        .m_axi_bresp(s01_regslice_to_s01_couplers_BRESP),
        .m_axi_bvalid(s01_regslice_to_s01_couplers_BVALID),
        .m_axi_rdata(s01_regslice_to_s01_couplers_RDATA),
        .m_axi_rid(s01_regslice_to_s01_couplers_RID[5:0]),
        .m_axi_rlast(s01_regslice_to_s01_couplers_RLAST),
        .m_axi_rready(s01_regslice_to_s01_couplers_RREADY),
        .m_axi_rresp(s01_regslice_to_s01_couplers_RRESP),
        .m_axi_rvalid(s01_regslice_to_s01_couplers_RVALID),
        .m_axi_wdata(s01_regslice_to_s01_couplers_WDATA),
        .m_axi_wlast(s01_regslice_to_s01_couplers_WLAST),
        .m_axi_wready(s01_regslice_to_s01_couplers_WREADY),
        .m_axi_wstrb(s01_regslice_to_s01_couplers_WSTRB),
        .m_axi_wvalid(s01_regslice_to_s01_couplers_WVALID),
        .s_axi_araddr(s01_couplers_to_s01_regslice_ARADDR),
        .s_axi_arburst(s01_couplers_to_s01_regslice_ARBURST),
        .s_axi_arcache(s01_couplers_to_s01_regslice_ARCACHE),
        .s_axi_arid(s01_couplers_to_s01_regslice_ARID),
        .s_axi_arlen(s01_couplers_to_s01_regslice_ARLEN),
        .s_axi_arlock(s01_couplers_to_s01_regslice_ARLOCK),
        .s_axi_arprot(s01_couplers_to_s01_regslice_ARPROT),
        .s_axi_arqos(s01_couplers_to_s01_regslice_ARQOS),
        .s_axi_arready(s01_couplers_to_s01_regslice_ARREADY),
        .s_axi_arregion(s01_couplers_to_s01_regslice_ARREGION),
        .s_axi_arsize(s01_couplers_to_s01_regslice_ARSIZE),
        .s_axi_arvalid(s01_couplers_to_s01_regslice_ARVALID),
        .s_axi_awaddr(s01_couplers_to_s01_regslice_AWADDR),
        .s_axi_awburst(s01_couplers_to_s01_regslice_AWBURST),
        .s_axi_awcache(s01_couplers_to_s01_regslice_AWCACHE),
        .s_axi_awid(s01_couplers_to_s01_regslice_AWID),
        .s_axi_awlen(s01_couplers_to_s01_regslice_AWLEN),
        .s_axi_awlock(s01_couplers_to_s01_regslice_AWLOCK),
        .s_axi_awprot(s01_couplers_to_s01_regslice_AWPROT),
        .s_axi_awqos(s01_couplers_to_s01_regslice_AWQOS),
        .s_axi_awready(s01_couplers_to_s01_regslice_AWREADY),
        .s_axi_awregion(s01_couplers_to_s01_regslice_AWREGION),
        .s_axi_awsize(s01_couplers_to_s01_regslice_AWSIZE),
        .s_axi_awvalid(s01_couplers_to_s01_regslice_AWVALID),
        .s_axi_bid(s01_couplers_to_s01_regslice_BID),
        .s_axi_bready(s01_couplers_to_s01_regslice_BREADY),
        .s_axi_bresp(s01_couplers_to_s01_regslice_BRESP),
        .s_axi_bvalid(s01_couplers_to_s01_regslice_BVALID),
        .s_axi_rdata(s01_couplers_to_s01_regslice_RDATA),
        .s_axi_rid(s01_couplers_to_s01_regslice_RID),
        .s_axi_rlast(s01_couplers_to_s01_regslice_RLAST),
        .s_axi_rready(s01_couplers_to_s01_regslice_RREADY),
        .s_axi_rresp(s01_couplers_to_s01_regslice_RRESP),
        .s_axi_rvalid(s01_couplers_to_s01_regslice_RVALID),
        .s_axi_wdata(s01_couplers_to_s01_regslice_WDATA),
        .s_axi_wlast(s01_couplers_to_s01_regslice_WLAST),
        .s_axi_wready(s01_couplers_to_s01_regslice_WREADY),
        .s_axi_wstrb(s01_couplers_to_s01_regslice_WSTRB),
        .s_axi_wvalid(s01_couplers_to_s01_regslice_WVALID));
endmodule
