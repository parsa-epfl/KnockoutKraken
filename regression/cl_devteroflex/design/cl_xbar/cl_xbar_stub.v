// Copyright 1986-2021 Xilinx, Inc. All Rights Reserved.
// --------------------------------------------------------------------------------
// Tool Version: Vivado v.2021.1 (lin64) Build 3247384 Thu Jun 10 19:36:07 MDT 2021
// Date        : Mon Apr 11 00:21:59 2022
// Host        : ulises-OptiPlex-7060 running 64-bit Ubuntu 20.04.4 LTS
// Command     : write_verilog -force -mode synth_stub
//               /home/eda/VivadoProjects/IP_generator_2021.1/managed_ip_project/managed_ip_project.gen/sources_1/bd/cl_xbar/cl_xbar_stub.v
// Design      : cl_xbar
// Purpose     : Stub declaration of top-level module interface
// Device      : xcvu9p-flgb2104-2-i
// --------------------------------------------------------------------------------

// This empty module with port declaration file causes synthesis tools to infer a black box for IP.
// The synthesis directives are for Synopsys Synplify support to prevent IO buffer insertion.
// Please paste the declaration into a Verilog source file or add the file as an additional source.
module cl_xbar(ACLK, ARESETN, M_00G_16G_AXI_araddr, 
  M_00G_16G_AXI_arburst, M_00G_16G_AXI_arcache, M_00G_16G_AXI_arid, M_00G_16G_AXI_arlen, 
  M_00G_16G_AXI_arlock, M_00G_16G_AXI_arprot, M_00G_16G_AXI_arqos, M_00G_16G_AXI_arready, 
  M_00G_16G_AXI_arregion, M_00G_16G_AXI_arsize, M_00G_16G_AXI_arvalid, 
  M_00G_16G_AXI_awaddr, M_00G_16G_AXI_awburst, M_00G_16G_AXI_awcache, M_00G_16G_AXI_awid, 
  M_00G_16G_AXI_awlen, M_00G_16G_AXI_awlock, M_00G_16G_AXI_awprot, M_00G_16G_AXI_awqos, 
  M_00G_16G_AXI_awready, M_00G_16G_AXI_awregion, M_00G_16G_AXI_awsize, 
  M_00G_16G_AXI_awvalid, M_00G_16G_AXI_bid, M_00G_16G_AXI_bready, M_00G_16G_AXI_bresp, 
  M_00G_16G_AXI_bvalid, M_00G_16G_AXI_rdata, M_00G_16G_AXI_rid, M_00G_16G_AXI_rlast, 
  M_00G_16G_AXI_rready, M_00G_16G_AXI_rresp, M_00G_16G_AXI_rvalid, M_00G_16G_AXI_wdata, 
  M_00G_16G_AXI_wlast, M_00G_16G_AXI_wready, M_00G_16G_AXI_wstrb, M_00G_16G_AXI_wvalid, 
  M_16G_32G_AXI_araddr, M_16G_32G_AXI_arburst, M_16G_32G_AXI_arcache, M_16G_32G_AXI_arid, 
  M_16G_32G_AXI_arlen, M_16G_32G_AXI_arlock, M_16G_32G_AXI_arprot, M_16G_32G_AXI_arqos, 
  M_16G_32G_AXI_arready, M_16G_32G_AXI_arregion, M_16G_32G_AXI_arsize, 
  M_16G_32G_AXI_arvalid, M_16G_32G_AXI_awaddr, M_16G_32G_AXI_awburst, 
  M_16G_32G_AXI_awcache, M_16G_32G_AXI_awid, M_16G_32G_AXI_awlen, M_16G_32G_AXI_awlock, 
  M_16G_32G_AXI_awprot, M_16G_32G_AXI_awqos, M_16G_32G_AXI_awready, 
  M_16G_32G_AXI_awregion, M_16G_32G_AXI_awsize, M_16G_32G_AXI_awvalid, M_16G_32G_AXI_bid, 
  M_16G_32G_AXI_bready, M_16G_32G_AXI_bresp, M_16G_32G_AXI_bvalid, M_16G_32G_AXI_rdata, 
  M_16G_32G_AXI_rid, M_16G_32G_AXI_rlast, M_16G_32G_AXI_rready, M_16G_32G_AXI_rresp, 
  M_16G_32G_AXI_rvalid, M_16G_32G_AXI_wdata, M_16G_32G_AXI_wlast, M_16G_32G_AXI_wready, 
  M_16G_32G_AXI_wstrb, M_16G_32G_AXI_wvalid, M_32G_48G_AXI_araddr, M_32G_48G_AXI_arburst, 
  M_32G_48G_AXI_arcache, M_32G_48G_AXI_arid, M_32G_48G_AXI_arlen, M_32G_48G_AXI_arlock, 
  M_32G_48G_AXI_arprot, M_32G_48G_AXI_arqos, M_32G_48G_AXI_arready, 
  M_32G_48G_AXI_arregion, M_32G_48G_AXI_arsize, M_32G_48G_AXI_arvalid, 
  M_32G_48G_AXI_awaddr, M_32G_48G_AXI_awburst, M_32G_48G_AXI_awcache, M_32G_48G_AXI_awid, 
  M_32G_48G_AXI_awlen, M_32G_48G_AXI_awlock, M_32G_48G_AXI_awprot, M_32G_48G_AXI_awqos, 
  M_32G_48G_AXI_awready, M_32G_48G_AXI_awregion, M_32G_48G_AXI_awsize, 
  M_32G_48G_AXI_awvalid, M_32G_48G_AXI_bid, M_32G_48G_AXI_bready, M_32G_48G_AXI_bresp, 
  M_32G_48G_AXI_bvalid, M_32G_48G_AXI_rdata, M_32G_48G_AXI_rid, M_32G_48G_AXI_rlast, 
  M_32G_48G_AXI_rready, M_32G_48G_AXI_rresp, M_32G_48G_AXI_rvalid, M_32G_48G_AXI_wdata, 
  M_32G_48G_AXI_wlast, M_32G_48G_AXI_wready, M_32G_48G_AXI_wstrb, M_32G_48G_AXI_wvalid, 
  M_48G_64G_AXI_araddr, M_48G_64G_AXI_arburst, M_48G_64G_AXI_arcache, M_48G_64G_AXI_arid, 
  M_48G_64G_AXI_arlen, M_48G_64G_AXI_arlock, M_48G_64G_AXI_arprot, M_48G_64G_AXI_arqos, 
  M_48G_64G_AXI_arready, M_48G_64G_AXI_arregion, M_48G_64G_AXI_arsize, 
  M_48G_64G_AXI_arvalid, M_48G_64G_AXI_awaddr, M_48G_64G_AXI_awburst, 
  M_48G_64G_AXI_awcache, M_48G_64G_AXI_awid, M_48G_64G_AXI_awlen, M_48G_64G_AXI_awlock, 
  M_48G_64G_AXI_awprot, M_48G_64G_AXI_awqos, M_48G_64G_AXI_awready, 
  M_48G_64G_AXI_awregion, M_48G_64G_AXI_awsize, M_48G_64G_AXI_awvalid, M_48G_64G_AXI_bid, 
  M_48G_64G_AXI_bready, M_48G_64G_AXI_bresp, M_48G_64G_AXI_bvalid, M_48G_64G_AXI_rdata, 
  M_48G_64G_AXI_rid, M_48G_64G_AXI_rlast, M_48G_64G_AXI_rready, M_48G_64G_AXI_rresp, 
  M_48G_64G_AXI_rvalid, M_48G_64G_AXI_wdata, M_48G_64G_AXI_wlast, M_48G_64G_AXI_wready, 
  M_48G_64G_AXI_wstrb, M_48G_64G_AXI_wvalid, M_AXI_RTL_araddr, M_AXI_RTL_arburst, 
  M_AXI_RTL_arcache, M_AXI_RTL_arid, M_AXI_RTL_arlen, M_AXI_RTL_arlock, M_AXI_RTL_arprot, 
  M_AXI_RTL_arqos, M_AXI_RTL_arready, M_AXI_RTL_arregion, M_AXI_RTL_arsize, 
  M_AXI_RTL_arvalid, M_AXI_RTL_awaddr, M_AXI_RTL_awburst, M_AXI_RTL_awcache, 
  M_AXI_RTL_awid, M_AXI_RTL_awlen, M_AXI_RTL_awlock, M_AXI_RTL_awprot, M_AXI_RTL_awqos, 
  M_AXI_RTL_awready, M_AXI_RTL_awregion, M_AXI_RTL_awsize, M_AXI_RTL_awvalid, 
  M_AXI_RTL_bid, M_AXI_RTL_bready, M_AXI_RTL_bresp, M_AXI_RTL_bvalid, M_AXI_RTL_rdata, 
  M_AXI_RTL_rid, M_AXI_RTL_rlast, M_AXI_RTL_rready, M_AXI_RTL_rresp, M_AXI_RTL_rvalid, 
  M_AXI_RTL_wdata, M_AXI_RTL_wlast, M_AXI_RTL_wready, M_AXI_RTL_wstrb, M_AXI_RTL_wvalid, 
  S_AXI_PCIS_araddr, S_AXI_PCIS_arburst, S_AXI_PCIS_arcache, S_AXI_PCIS_arid, 
  S_AXI_PCIS_arlen, S_AXI_PCIS_arlock, S_AXI_PCIS_arprot, S_AXI_PCIS_arqos, 
  S_AXI_PCIS_arready, S_AXI_PCIS_arregion, S_AXI_PCIS_arsize, S_AXI_PCIS_arvalid, 
  S_AXI_PCIS_awaddr, S_AXI_PCIS_awburst, S_AXI_PCIS_awcache, S_AXI_PCIS_awid, 
  S_AXI_PCIS_awlen, S_AXI_PCIS_awlock, S_AXI_PCIS_awprot, S_AXI_PCIS_awqos, 
  S_AXI_PCIS_awready, S_AXI_PCIS_awregion, S_AXI_PCIS_awsize, S_AXI_PCIS_awvalid, 
  S_AXI_PCIS_bid, S_AXI_PCIS_bready, S_AXI_PCIS_bresp, S_AXI_PCIS_bvalid, S_AXI_PCIS_rdata, 
  S_AXI_PCIS_rid, S_AXI_PCIS_rlast, S_AXI_PCIS_rready, S_AXI_PCIS_rresp, S_AXI_PCIS_rvalid, 
  S_AXI_PCIS_wdata, S_AXI_PCIS_wlast, S_AXI_PCIS_wready, S_AXI_PCIS_wstrb, 
  S_AXI_PCIS_wvalid, S_RTL_DRAM_AXI_araddr, S_RTL_DRAM_AXI_arburst, 
  S_RTL_DRAM_AXI_arcache, S_RTL_DRAM_AXI_arid, S_RTL_DRAM_AXI_arlen, 
  S_RTL_DRAM_AXI_arlock, S_RTL_DRAM_AXI_arprot, S_RTL_DRAM_AXI_arqos, 
  S_RTL_DRAM_AXI_arready, S_RTL_DRAM_AXI_arregion, S_RTL_DRAM_AXI_arsize, 
  S_RTL_DRAM_AXI_arvalid, S_RTL_DRAM_AXI_awaddr, S_RTL_DRAM_AXI_awburst, 
  S_RTL_DRAM_AXI_awcache, S_RTL_DRAM_AXI_awid, S_RTL_DRAM_AXI_awlen, 
  S_RTL_DRAM_AXI_awlock, S_RTL_DRAM_AXI_awprot, S_RTL_DRAM_AXI_awqos, 
  S_RTL_DRAM_AXI_awready, S_RTL_DRAM_AXI_awregion, S_RTL_DRAM_AXI_awsize, 
  S_RTL_DRAM_AXI_awvalid, S_RTL_DRAM_AXI_bid, S_RTL_DRAM_AXI_bready, 
  S_RTL_DRAM_AXI_bresp, S_RTL_DRAM_AXI_bvalid, S_RTL_DRAM_AXI_rdata, S_RTL_DRAM_AXI_rid, 
  S_RTL_DRAM_AXI_rlast, S_RTL_DRAM_AXI_rready, S_RTL_DRAM_AXI_rresp, 
  S_RTL_DRAM_AXI_rvalid, S_RTL_DRAM_AXI_wdata, S_RTL_DRAM_AXI_wlast, 
  S_RTL_DRAM_AXI_wready, S_RTL_DRAM_AXI_wstrb, S_RTL_DRAM_AXI_wvalid)
/* synthesis syn_black_box black_box_pad_pin="ACLK,ARESETN,M_00G_16G_AXI_araddr[63:0],M_00G_16G_AXI_arburst[1:0],M_00G_16G_AXI_arcache[3:0],M_00G_16G_AXI_arid[6:0],M_00G_16G_AXI_arlen[7:0],M_00G_16G_AXI_arlock[0:0],M_00G_16G_AXI_arprot[2:0],M_00G_16G_AXI_arqos[3:0],M_00G_16G_AXI_arready,M_00G_16G_AXI_arregion[3:0],M_00G_16G_AXI_arsize[2:0],M_00G_16G_AXI_arvalid,M_00G_16G_AXI_awaddr[63:0],M_00G_16G_AXI_awburst[1:0],M_00G_16G_AXI_awcache[3:0],M_00G_16G_AXI_awid[6:0],M_00G_16G_AXI_awlen[7:0],M_00G_16G_AXI_awlock[0:0],M_00G_16G_AXI_awprot[2:0],M_00G_16G_AXI_awqos[3:0],M_00G_16G_AXI_awready,M_00G_16G_AXI_awregion[3:0],M_00G_16G_AXI_awsize[2:0],M_00G_16G_AXI_awvalid,M_00G_16G_AXI_bid[6:0],M_00G_16G_AXI_bready,M_00G_16G_AXI_bresp[1:0],M_00G_16G_AXI_bvalid,M_00G_16G_AXI_rdata[511:0],M_00G_16G_AXI_rid[6:0],M_00G_16G_AXI_rlast,M_00G_16G_AXI_rready,M_00G_16G_AXI_rresp[1:0],M_00G_16G_AXI_rvalid,M_00G_16G_AXI_wdata[511:0],M_00G_16G_AXI_wlast,M_00G_16G_AXI_wready,M_00G_16G_AXI_wstrb[63:0],M_00G_16G_AXI_wvalid,M_16G_32G_AXI_araddr[63:0],M_16G_32G_AXI_arburst[1:0],M_16G_32G_AXI_arcache[3:0],M_16G_32G_AXI_arid[6:0],M_16G_32G_AXI_arlen[7:0],M_16G_32G_AXI_arlock[0:0],M_16G_32G_AXI_arprot[2:0],M_16G_32G_AXI_arqos[3:0],M_16G_32G_AXI_arready,M_16G_32G_AXI_arregion[3:0],M_16G_32G_AXI_arsize[2:0],M_16G_32G_AXI_arvalid,M_16G_32G_AXI_awaddr[63:0],M_16G_32G_AXI_awburst[1:0],M_16G_32G_AXI_awcache[3:0],M_16G_32G_AXI_awid[6:0],M_16G_32G_AXI_awlen[7:0],M_16G_32G_AXI_awlock[0:0],M_16G_32G_AXI_awprot[2:0],M_16G_32G_AXI_awqos[3:0],M_16G_32G_AXI_awready,M_16G_32G_AXI_awregion[3:0],M_16G_32G_AXI_awsize[2:0],M_16G_32G_AXI_awvalid,M_16G_32G_AXI_bid[6:0],M_16G_32G_AXI_bready,M_16G_32G_AXI_bresp[1:0],M_16G_32G_AXI_bvalid,M_16G_32G_AXI_rdata[511:0],M_16G_32G_AXI_rid[6:0],M_16G_32G_AXI_rlast,M_16G_32G_AXI_rready,M_16G_32G_AXI_rresp[1:0],M_16G_32G_AXI_rvalid,M_16G_32G_AXI_wdata[511:0],M_16G_32G_AXI_wlast,M_16G_32G_AXI_wready,M_16G_32G_AXI_wstrb[63:0],M_16G_32G_AXI_wvalid,M_32G_48G_AXI_araddr[63:0],M_32G_48G_AXI_arburst[1:0],M_32G_48G_AXI_arcache[3:0],M_32G_48G_AXI_arid[6:0],M_32G_48G_AXI_arlen[7:0],M_32G_48G_AXI_arlock[0:0],M_32G_48G_AXI_arprot[2:0],M_32G_48G_AXI_arqos[3:0],M_32G_48G_AXI_arready,M_32G_48G_AXI_arregion[3:0],M_32G_48G_AXI_arsize[2:0],M_32G_48G_AXI_arvalid,M_32G_48G_AXI_awaddr[63:0],M_32G_48G_AXI_awburst[1:0],M_32G_48G_AXI_awcache[3:0],M_32G_48G_AXI_awid[6:0],M_32G_48G_AXI_awlen[7:0],M_32G_48G_AXI_awlock[0:0],M_32G_48G_AXI_awprot[2:0],M_32G_48G_AXI_awqos[3:0],M_32G_48G_AXI_awready,M_32G_48G_AXI_awregion[3:0],M_32G_48G_AXI_awsize[2:0],M_32G_48G_AXI_awvalid,M_32G_48G_AXI_bid[6:0],M_32G_48G_AXI_bready,M_32G_48G_AXI_bresp[1:0],M_32G_48G_AXI_bvalid,M_32G_48G_AXI_rdata[511:0],M_32G_48G_AXI_rid[6:0],M_32G_48G_AXI_rlast,M_32G_48G_AXI_rready,M_32G_48G_AXI_rresp[1:0],M_32G_48G_AXI_rvalid,M_32G_48G_AXI_wdata[511:0],M_32G_48G_AXI_wlast,M_32G_48G_AXI_wready,M_32G_48G_AXI_wstrb[63:0],M_32G_48G_AXI_wvalid,M_48G_64G_AXI_araddr[63:0],M_48G_64G_AXI_arburst[1:0],M_48G_64G_AXI_arcache[3:0],M_48G_64G_AXI_arid[6:0],M_48G_64G_AXI_arlen[7:0],M_48G_64G_AXI_arlock[0:0],M_48G_64G_AXI_arprot[2:0],M_48G_64G_AXI_arqos[3:0],M_48G_64G_AXI_arready,M_48G_64G_AXI_arregion[3:0],M_48G_64G_AXI_arsize[2:0],M_48G_64G_AXI_arvalid,M_48G_64G_AXI_awaddr[63:0],M_48G_64G_AXI_awburst[1:0],M_48G_64G_AXI_awcache[3:0],M_48G_64G_AXI_awid[6:0],M_48G_64G_AXI_awlen[7:0],M_48G_64G_AXI_awlock[0:0],M_48G_64G_AXI_awprot[2:0],M_48G_64G_AXI_awqos[3:0],M_48G_64G_AXI_awready,M_48G_64G_AXI_awregion[3:0],M_48G_64G_AXI_awsize[2:0],M_48G_64G_AXI_awvalid,M_48G_64G_AXI_bid[6:0],M_48G_64G_AXI_bready,M_48G_64G_AXI_bresp[1:0],M_48G_64G_AXI_bvalid,M_48G_64G_AXI_rdata[511:0],M_48G_64G_AXI_rid[6:0],M_48G_64G_AXI_rlast,M_48G_64G_AXI_rready,M_48G_64G_AXI_rresp[1:0],M_48G_64G_AXI_rvalid,M_48G_64G_AXI_wdata[511:0],M_48G_64G_AXI_wlast,M_48G_64G_AXI_wready,M_48G_64G_AXI_wstrb[63:0],M_48G_64G_AXI_wvalid,M_AXI_RTL_araddr[63:0],M_AXI_RTL_arburst[1:0],M_AXI_RTL_arcache[3:0],M_AXI_RTL_arid[5:0],M_AXI_RTL_arlen[7:0],M_AXI_RTL_arlock[0:0],M_AXI_RTL_arprot[2:0],M_AXI_RTL_arqos[3:0],M_AXI_RTL_arready,M_AXI_RTL_arregion[3:0],M_AXI_RTL_arsize[2:0],M_AXI_RTL_arvalid,M_AXI_RTL_awaddr[63:0],M_AXI_RTL_awburst[1:0],M_AXI_RTL_awcache[3:0],M_AXI_RTL_awid[5:0],M_AXI_RTL_awlen[7:0],M_AXI_RTL_awlock[0:0],M_AXI_RTL_awprot[2:0],M_AXI_RTL_awqos[3:0],M_AXI_RTL_awready,M_AXI_RTL_awregion[3:0],M_AXI_RTL_awsize[2:0],M_AXI_RTL_awvalid,M_AXI_RTL_bid[5:0],M_AXI_RTL_bready,M_AXI_RTL_bresp[1:0],M_AXI_RTL_bvalid,M_AXI_RTL_rdata[511:0],M_AXI_RTL_rid[5:0],M_AXI_RTL_rlast,M_AXI_RTL_rready,M_AXI_RTL_rresp[1:0],M_AXI_RTL_rvalid,M_AXI_RTL_wdata[511:0],M_AXI_RTL_wlast,M_AXI_RTL_wready,M_AXI_RTL_wstrb[63:0],M_AXI_RTL_wvalid,S_AXI_PCIS_araddr[63:0],S_AXI_PCIS_arburst[1:0],S_AXI_PCIS_arcache[3:0],S_AXI_PCIS_arid[5:0],S_AXI_PCIS_arlen[7:0],S_AXI_PCIS_arlock[0:0],S_AXI_PCIS_arprot[2:0],S_AXI_PCIS_arqos[3:0],S_AXI_PCIS_arready,S_AXI_PCIS_arregion[3:0],S_AXI_PCIS_arsize[2:0],S_AXI_PCIS_arvalid,S_AXI_PCIS_awaddr[63:0],S_AXI_PCIS_awburst[1:0],S_AXI_PCIS_awcache[3:0],S_AXI_PCIS_awid[5:0],S_AXI_PCIS_awlen[7:0],S_AXI_PCIS_awlock[0:0],S_AXI_PCIS_awprot[2:0],S_AXI_PCIS_awqos[3:0],S_AXI_PCIS_awready,S_AXI_PCIS_awregion[3:0],S_AXI_PCIS_awsize[2:0],S_AXI_PCIS_awvalid,S_AXI_PCIS_bid[5:0],S_AXI_PCIS_bready,S_AXI_PCIS_bresp[1:0],S_AXI_PCIS_bvalid,S_AXI_PCIS_rdata[511:0],S_AXI_PCIS_rid[5:0],S_AXI_PCIS_rlast,S_AXI_PCIS_rready,S_AXI_PCIS_rresp[1:0],S_AXI_PCIS_rvalid,S_AXI_PCIS_wdata[511:0],S_AXI_PCIS_wlast,S_AXI_PCIS_wready,S_AXI_PCIS_wstrb[63:0],S_AXI_PCIS_wvalid,S_RTL_DRAM_AXI_araddr[63:0],S_RTL_DRAM_AXI_arburst[1:0],S_RTL_DRAM_AXI_arcache[3:0],S_RTL_DRAM_AXI_arid[5:0],S_RTL_DRAM_AXI_arlen[7:0],S_RTL_DRAM_AXI_arlock[0:0],S_RTL_DRAM_AXI_arprot[2:0],S_RTL_DRAM_AXI_arqos[3:0],S_RTL_DRAM_AXI_arready,S_RTL_DRAM_AXI_arregion[3:0],S_RTL_DRAM_AXI_arsize[2:0],S_RTL_DRAM_AXI_arvalid,S_RTL_DRAM_AXI_awaddr[63:0],S_RTL_DRAM_AXI_awburst[1:0],S_RTL_DRAM_AXI_awcache[3:0],S_RTL_DRAM_AXI_awid[5:0],S_RTL_DRAM_AXI_awlen[7:0],S_RTL_DRAM_AXI_awlock[0:0],S_RTL_DRAM_AXI_awprot[2:0],S_RTL_DRAM_AXI_awqos[3:0],S_RTL_DRAM_AXI_awready,S_RTL_DRAM_AXI_awregion[3:0],S_RTL_DRAM_AXI_awsize[2:0],S_RTL_DRAM_AXI_awvalid,S_RTL_DRAM_AXI_bid[5:0],S_RTL_DRAM_AXI_bready,S_RTL_DRAM_AXI_bresp[1:0],S_RTL_DRAM_AXI_bvalid,S_RTL_DRAM_AXI_rdata[511:0],S_RTL_DRAM_AXI_rid[5:0],S_RTL_DRAM_AXI_rlast,S_RTL_DRAM_AXI_rready,S_RTL_DRAM_AXI_rresp[1:0],S_RTL_DRAM_AXI_rvalid,S_RTL_DRAM_AXI_wdata[511:0],S_RTL_DRAM_AXI_wlast,S_RTL_DRAM_AXI_wready,S_RTL_DRAM_AXI_wstrb[63:0],S_RTL_DRAM_AXI_wvalid" */;
  input ACLK;
  input ARESETN;
  output [63:0]M_00G_16G_AXI_araddr;
  output [1:0]M_00G_16G_AXI_arburst;
  output [3:0]M_00G_16G_AXI_arcache;
  output [6:0]M_00G_16G_AXI_arid;
  output [7:0]M_00G_16G_AXI_arlen;
  output [0:0]M_00G_16G_AXI_arlock;
  output [2:0]M_00G_16G_AXI_arprot;
  output [3:0]M_00G_16G_AXI_arqos;
  input M_00G_16G_AXI_arready;
  output [3:0]M_00G_16G_AXI_arregion;
  output [2:0]M_00G_16G_AXI_arsize;
  output M_00G_16G_AXI_arvalid;
  output [63:0]M_00G_16G_AXI_awaddr;
  output [1:0]M_00G_16G_AXI_awburst;
  output [3:0]M_00G_16G_AXI_awcache;
  output [6:0]M_00G_16G_AXI_awid;
  output [7:0]M_00G_16G_AXI_awlen;
  output [0:0]M_00G_16G_AXI_awlock;
  output [2:0]M_00G_16G_AXI_awprot;
  output [3:0]M_00G_16G_AXI_awqos;
  input M_00G_16G_AXI_awready;
  output [3:0]M_00G_16G_AXI_awregion;
  output [2:0]M_00G_16G_AXI_awsize;
  output M_00G_16G_AXI_awvalid;
  input [6:0]M_00G_16G_AXI_bid;
  output M_00G_16G_AXI_bready;
  input [1:0]M_00G_16G_AXI_bresp;
  input M_00G_16G_AXI_bvalid;
  input [511:0]M_00G_16G_AXI_rdata;
  input [6:0]M_00G_16G_AXI_rid;
  input M_00G_16G_AXI_rlast;
  output M_00G_16G_AXI_rready;
  input [1:0]M_00G_16G_AXI_rresp;
  input M_00G_16G_AXI_rvalid;
  output [511:0]M_00G_16G_AXI_wdata;
  output M_00G_16G_AXI_wlast;
  input M_00G_16G_AXI_wready;
  output [63:0]M_00G_16G_AXI_wstrb;
  output M_00G_16G_AXI_wvalid;
  output [63:0]M_16G_32G_AXI_araddr;
  output [1:0]M_16G_32G_AXI_arburst;
  output [3:0]M_16G_32G_AXI_arcache;
  output [6:0]M_16G_32G_AXI_arid;
  output [7:0]M_16G_32G_AXI_arlen;
  output [0:0]M_16G_32G_AXI_arlock;
  output [2:0]M_16G_32G_AXI_arprot;
  output [3:0]M_16G_32G_AXI_arqos;
  input M_16G_32G_AXI_arready;
  output [3:0]M_16G_32G_AXI_arregion;
  output [2:0]M_16G_32G_AXI_arsize;
  output M_16G_32G_AXI_arvalid;
  output [63:0]M_16G_32G_AXI_awaddr;
  output [1:0]M_16G_32G_AXI_awburst;
  output [3:0]M_16G_32G_AXI_awcache;
  output [6:0]M_16G_32G_AXI_awid;
  output [7:0]M_16G_32G_AXI_awlen;
  output [0:0]M_16G_32G_AXI_awlock;
  output [2:0]M_16G_32G_AXI_awprot;
  output [3:0]M_16G_32G_AXI_awqos;
  input M_16G_32G_AXI_awready;
  output [3:0]M_16G_32G_AXI_awregion;
  output [2:0]M_16G_32G_AXI_awsize;
  output M_16G_32G_AXI_awvalid;
  input [6:0]M_16G_32G_AXI_bid;
  output M_16G_32G_AXI_bready;
  input [1:0]M_16G_32G_AXI_bresp;
  input M_16G_32G_AXI_bvalid;
  input [511:0]M_16G_32G_AXI_rdata;
  input [6:0]M_16G_32G_AXI_rid;
  input M_16G_32G_AXI_rlast;
  output M_16G_32G_AXI_rready;
  input [1:0]M_16G_32G_AXI_rresp;
  input M_16G_32G_AXI_rvalid;
  output [511:0]M_16G_32G_AXI_wdata;
  output M_16G_32G_AXI_wlast;
  input M_16G_32G_AXI_wready;
  output [63:0]M_16G_32G_AXI_wstrb;
  output M_16G_32G_AXI_wvalid;
  output [63:0]M_32G_48G_AXI_araddr;
  output [1:0]M_32G_48G_AXI_arburst;
  output [3:0]M_32G_48G_AXI_arcache;
  output [6:0]M_32G_48G_AXI_arid;
  output [7:0]M_32G_48G_AXI_arlen;
  output [0:0]M_32G_48G_AXI_arlock;
  output [2:0]M_32G_48G_AXI_arprot;
  output [3:0]M_32G_48G_AXI_arqos;
  input M_32G_48G_AXI_arready;
  output [3:0]M_32G_48G_AXI_arregion;
  output [2:0]M_32G_48G_AXI_arsize;
  output M_32G_48G_AXI_arvalid;
  output [63:0]M_32G_48G_AXI_awaddr;
  output [1:0]M_32G_48G_AXI_awburst;
  output [3:0]M_32G_48G_AXI_awcache;
  output [6:0]M_32G_48G_AXI_awid;
  output [7:0]M_32G_48G_AXI_awlen;
  output [0:0]M_32G_48G_AXI_awlock;
  output [2:0]M_32G_48G_AXI_awprot;
  output [3:0]M_32G_48G_AXI_awqos;
  input M_32G_48G_AXI_awready;
  output [3:0]M_32G_48G_AXI_awregion;
  output [2:0]M_32G_48G_AXI_awsize;
  output M_32G_48G_AXI_awvalid;
  input [6:0]M_32G_48G_AXI_bid;
  output M_32G_48G_AXI_bready;
  input [1:0]M_32G_48G_AXI_bresp;
  input M_32G_48G_AXI_bvalid;
  input [511:0]M_32G_48G_AXI_rdata;
  input [6:0]M_32G_48G_AXI_rid;
  input M_32G_48G_AXI_rlast;
  output M_32G_48G_AXI_rready;
  input [1:0]M_32G_48G_AXI_rresp;
  input M_32G_48G_AXI_rvalid;
  output [511:0]M_32G_48G_AXI_wdata;
  output M_32G_48G_AXI_wlast;
  input M_32G_48G_AXI_wready;
  output [63:0]M_32G_48G_AXI_wstrb;
  output M_32G_48G_AXI_wvalid;
  output [63:0]M_48G_64G_AXI_araddr;
  output [1:0]M_48G_64G_AXI_arburst;
  output [3:0]M_48G_64G_AXI_arcache;
  output [6:0]M_48G_64G_AXI_arid;
  output [7:0]M_48G_64G_AXI_arlen;
  output [0:0]M_48G_64G_AXI_arlock;
  output [2:0]M_48G_64G_AXI_arprot;
  output [3:0]M_48G_64G_AXI_arqos;
  input M_48G_64G_AXI_arready;
  output [3:0]M_48G_64G_AXI_arregion;
  output [2:0]M_48G_64G_AXI_arsize;
  output M_48G_64G_AXI_arvalid;
  output [63:0]M_48G_64G_AXI_awaddr;
  output [1:0]M_48G_64G_AXI_awburst;
  output [3:0]M_48G_64G_AXI_awcache;
  output [6:0]M_48G_64G_AXI_awid;
  output [7:0]M_48G_64G_AXI_awlen;
  output [0:0]M_48G_64G_AXI_awlock;
  output [2:0]M_48G_64G_AXI_awprot;
  output [3:0]M_48G_64G_AXI_awqos;
  input M_48G_64G_AXI_awready;
  output [3:0]M_48G_64G_AXI_awregion;
  output [2:0]M_48G_64G_AXI_awsize;
  output M_48G_64G_AXI_awvalid;
  input [6:0]M_48G_64G_AXI_bid;
  output M_48G_64G_AXI_bready;
  input [1:0]M_48G_64G_AXI_bresp;
  input M_48G_64G_AXI_bvalid;
  input [511:0]M_48G_64G_AXI_rdata;
  input [6:0]M_48G_64G_AXI_rid;
  input M_48G_64G_AXI_rlast;
  output M_48G_64G_AXI_rready;
  input [1:0]M_48G_64G_AXI_rresp;
  input M_48G_64G_AXI_rvalid;
  output [511:0]M_48G_64G_AXI_wdata;
  output M_48G_64G_AXI_wlast;
  input M_48G_64G_AXI_wready;
  output [63:0]M_48G_64G_AXI_wstrb;
  output M_48G_64G_AXI_wvalid;
  output [63:0]M_AXI_RTL_araddr;
  output [1:0]M_AXI_RTL_arburst;
  output [3:0]M_AXI_RTL_arcache;
  output [5:0]M_AXI_RTL_arid;
  output [7:0]M_AXI_RTL_arlen;
  output [0:0]M_AXI_RTL_arlock;
  output [2:0]M_AXI_RTL_arprot;
  output [3:0]M_AXI_RTL_arqos;
  input M_AXI_RTL_arready;
  output [3:0]M_AXI_RTL_arregion;
  output [2:0]M_AXI_RTL_arsize;
  output M_AXI_RTL_arvalid;
  output [63:0]M_AXI_RTL_awaddr;
  output [1:0]M_AXI_RTL_awburst;
  output [3:0]M_AXI_RTL_awcache;
  output [5:0]M_AXI_RTL_awid;
  output [7:0]M_AXI_RTL_awlen;
  output [0:0]M_AXI_RTL_awlock;
  output [2:0]M_AXI_RTL_awprot;
  output [3:0]M_AXI_RTL_awqos;
  input M_AXI_RTL_awready;
  output [3:0]M_AXI_RTL_awregion;
  output [2:0]M_AXI_RTL_awsize;
  output M_AXI_RTL_awvalid;
  input [5:0]M_AXI_RTL_bid;
  output M_AXI_RTL_bready;
  input [1:0]M_AXI_RTL_bresp;
  input M_AXI_RTL_bvalid;
  input [511:0]M_AXI_RTL_rdata;
  input [5:0]M_AXI_RTL_rid;
  input M_AXI_RTL_rlast;
  output M_AXI_RTL_rready;
  input [1:0]M_AXI_RTL_rresp;
  input M_AXI_RTL_rvalid;
  output [511:0]M_AXI_RTL_wdata;
  output M_AXI_RTL_wlast;
  input M_AXI_RTL_wready;
  output [63:0]M_AXI_RTL_wstrb;
  output M_AXI_RTL_wvalid;
  input [63:0]S_AXI_PCIS_araddr;
  input [1:0]S_AXI_PCIS_arburst;
  input [3:0]S_AXI_PCIS_arcache;
  input [5:0]S_AXI_PCIS_arid;
  input [7:0]S_AXI_PCIS_arlen;
  input [0:0]S_AXI_PCIS_arlock;
  input [2:0]S_AXI_PCIS_arprot;
  input [3:0]S_AXI_PCIS_arqos;
  output S_AXI_PCIS_arready;
  input [3:0]S_AXI_PCIS_arregion;
  input [2:0]S_AXI_PCIS_arsize;
  input S_AXI_PCIS_arvalid;
  input [63:0]S_AXI_PCIS_awaddr;
  input [1:0]S_AXI_PCIS_awburst;
  input [3:0]S_AXI_PCIS_awcache;
  input [5:0]S_AXI_PCIS_awid;
  input [7:0]S_AXI_PCIS_awlen;
  input [0:0]S_AXI_PCIS_awlock;
  input [2:0]S_AXI_PCIS_awprot;
  input [3:0]S_AXI_PCIS_awqos;
  output S_AXI_PCIS_awready;
  input [3:0]S_AXI_PCIS_awregion;
  input [2:0]S_AXI_PCIS_awsize;
  input S_AXI_PCIS_awvalid;
  output [5:0]S_AXI_PCIS_bid;
  input S_AXI_PCIS_bready;
  output [1:0]S_AXI_PCIS_bresp;
  output S_AXI_PCIS_bvalid;
  output [511:0]S_AXI_PCIS_rdata;
  output [5:0]S_AXI_PCIS_rid;
  output S_AXI_PCIS_rlast;
  input S_AXI_PCIS_rready;
  output [1:0]S_AXI_PCIS_rresp;
  output S_AXI_PCIS_rvalid;
  input [511:0]S_AXI_PCIS_wdata;
  input S_AXI_PCIS_wlast;
  output S_AXI_PCIS_wready;
  input [63:0]S_AXI_PCIS_wstrb;
  input S_AXI_PCIS_wvalid;
  input [63:0]S_RTL_DRAM_AXI_araddr;
  input [1:0]S_RTL_DRAM_AXI_arburst;
  input [3:0]S_RTL_DRAM_AXI_arcache;
  input [5:0]S_RTL_DRAM_AXI_arid;
  input [7:0]S_RTL_DRAM_AXI_arlen;
  input [0:0]S_RTL_DRAM_AXI_arlock;
  input [2:0]S_RTL_DRAM_AXI_arprot;
  input [3:0]S_RTL_DRAM_AXI_arqos;
  output S_RTL_DRAM_AXI_arready;
  input [3:0]S_RTL_DRAM_AXI_arregion;
  input [2:0]S_RTL_DRAM_AXI_arsize;
  input S_RTL_DRAM_AXI_arvalid;
  input [63:0]S_RTL_DRAM_AXI_awaddr;
  input [1:0]S_RTL_DRAM_AXI_awburst;
  input [3:0]S_RTL_DRAM_AXI_awcache;
  input [5:0]S_RTL_DRAM_AXI_awid;
  input [7:0]S_RTL_DRAM_AXI_awlen;
  input [0:0]S_RTL_DRAM_AXI_awlock;
  input [2:0]S_RTL_DRAM_AXI_awprot;
  input [3:0]S_RTL_DRAM_AXI_awqos;
  output S_RTL_DRAM_AXI_awready;
  input [3:0]S_RTL_DRAM_AXI_awregion;
  input [2:0]S_RTL_DRAM_AXI_awsize;
  input S_RTL_DRAM_AXI_awvalid;
  output [5:0]S_RTL_DRAM_AXI_bid;
  input S_RTL_DRAM_AXI_bready;
  output [1:0]S_RTL_DRAM_AXI_bresp;
  output S_RTL_DRAM_AXI_bvalid;
  output [511:0]S_RTL_DRAM_AXI_rdata;
  output [5:0]S_RTL_DRAM_AXI_rid;
  output S_RTL_DRAM_AXI_rlast;
  input S_RTL_DRAM_AXI_rready;
  output [1:0]S_RTL_DRAM_AXI_rresp;
  output S_RTL_DRAM_AXI_rvalid;
  input [511:0]S_RTL_DRAM_AXI_wdata;
  input S_RTL_DRAM_AXI_wlast;
  output S_RTL_DRAM_AXI_wready;
  input [63:0]S_RTL_DRAM_AXI_wstrb;
  input S_RTL_DRAM_AXI_wvalid;
endmodule
