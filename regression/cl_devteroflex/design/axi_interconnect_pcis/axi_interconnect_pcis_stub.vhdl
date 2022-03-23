-- Copyright 1986-2019 Xilinx, Inc. All Rights Reserved.
-- --------------------------------------------------------------------------------
-- Tool Version: Vivado v.2019.2 (lin64) Build 2708876 Wed Nov  6 21:39:14 MST 2019
-- Date        : Thu Mar 17 14:24:22 2022
-- Host        : ulises-OptiPlex-7060 running 64-bit Ubuntu 20.04.2 LTS
-- Command     : write_vhdl -force -mode synth_stub
--               /home/eda/VivadoProjects/IP_generator_2019.2/managed_ip_project/managed_ip_project.srcs/sources_1/bd/axi_interconnect_pcis/axi_interconnect_pcis_stub.vhdl
-- Design      : axi_interconnect_pcis
-- Purpose     : Stub declaration of top-level module interface
-- Device      : xcvu9p-flgb2104-2-i
-- --------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity axi_interconnect_pcis is
  Port ( 
    ACLK : in STD_LOGIC;
    ARESETN : in STD_LOGIC;
    M_AXI_DRAM_araddr : out STD_LOGIC_VECTOR ( 63 downto 0 );
    M_AXI_DRAM_arburst : out STD_LOGIC_VECTOR ( 1 downto 0 );
    M_AXI_DRAM_arcache : out STD_LOGIC_VECTOR ( 3 downto 0 );
    M_AXI_DRAM_arid : out STD_LOGIC_VECTOR ( 5 downto 0 );
    M_AXI_DRAM_arlen : out STD_LOGIC_VECTOR ( 7 downto 0 );
    M_AXI_DRAM_arlock : out STD_LOGIC_VECTOR ( 0 to 0 );
    M_AXI_DRAM_arprot : out STD_LOGIC_VECTOR ( 2 downto 0 );
    M_AXI_DRAM_arqos : out STD_LOGIC_VECTOR ( 3 downto 0 );
    M_AXI_DRAM_arready : in STD_LOGIC;
    M_AXI_DRAM_arregion : out STD_LOGIC_VECTOR ( 3 downto 0 );
    M_AXI_DRAM_arsize : out STD_LOGIC_VECTOR ( 2 downto 0 );
    M_AXI_DRAM_arvalid : out STD_LOGIC;
    M_AXI_DRAM_awaddr : out STD_LOGIC_VECTOR ( 63 downto 0 );
    M_AXI_DRAM_awburst : out STD_LOGIC_VECTOR ( 1 downto 0 );
    M_AXI_DRAM_awcache : out STD_LOGIC_VECTOR ( 3 downto 0 );
    M_AXI_DRAM_awid : out STD_LOGIC_VECTOR ( 5 downto 0 );
    M_AXI_DRAM_awlen : out STD_LOGIC_VECTOR ( 7 downto 0 );
    M_AXI_DRAM_awlock : out STD_LOGIC_VECTOR ( 0 to 0 );
    M_AXI_DRAM_awprot : out STD_LOGIC_VECTOR ( 2 downto 0 );
    M_AXI_DRAM_awqos : out STD_LOGIC_VECTOR ( 3 downto 0 );
    M_AXI_DRAM_awready : in STD_LOGIC;
    M_AXI_DRAM_awregion : out STD_LOGIC_VECTOR ( 3 downto 0 );
    M_AXI_DRAM_awsize : out STD_LOGIC_VECTOR ( 2 downto 0 );
    M_AXI_DRAM_awvalid : out STD_LOGIC;
    M_AXI_DRAM_bid : in STD_LOGIC_VECTOR ( 5 downto 0 );
    M_AXI_DRAM_bready : out STD_LOGIC;
    M_AXI_DRAM_bresp : in STD_LOGIC_VECTOR ( 1 downto 0 );
    M_AXI_DRAM_bvalid : in STD_LOGIC;
    M_AXI_DRAM_rdata : in STD_LOGIC_VECTOR ( 511 downto 0 );
    M_AXI_DRAM_rid : in STD_LOGIC_VECTOR ( 5 downto 0 );
    M_AXI_DRAM_rlast : in STD_LOGIC;
    M_AXI_DRAM_rready : out STD_LOGIC;
    M_AXI_DRAM_rresp : in STD_LOGIC_VECTOR ( 1 downto 0 );
    M_AXI_DRAM_rvalid : in STD_LOGIC;
    M_AXI_DRAM_wdata : out STD_LOGIC_VECTOR ( 511 downto 0 );
    M_AXI_DRAM_wlast : out STD_LOGIC;
    M_AXI_DRAM_wready : in STD_LOGIC;
    M_AXI_DRAM_wstrb : out STD_LOGIC_VECTOR ( 63 downto 0 );
    M_AXI_DRAM_wvalid : out STD_LOGIC;
    M_AXI_RTL_araddr : out STD_LOGIC_VECTOR ( 63 downto 0 );
    M_AXI_RTL_arburst : out STD_LOGIC_VECTOR ( 1 downto 0 );
    M_AXI_RTL_arcache : out STD_LOGIC_VECTOR ( 3 downto 0 );
    M_AXI_RTL_arid : out STD_LOGIC_VECTOR ( 5 downto 0 );
    M_AXI_RTL_arlen : out STD_LOGIC_VECTOR ( 7 downto 0 );
    M_AXI_RTL_arlock : out STD_LOGIC_VECTOR ( 0 to 0 );
    M_AXI_RTL_arprot : out STD_LOGIC_VECTOR ( 2 downto 0 );
    M_AXI_RTL_arqos : out STD_LOGIC_VECTOR ( 3 downto 0 );
    M_AXI_RTL_arready : in STD_LOGIC;
    M_AXI_RTL_arregion : out STD_LOGIC_VECTOR ( 3 downto 0 );
    M_AXI_RTL_arsize : out STD_LOGIC_VECTOR ( 2 downto 0 );
    M_AXI_RTL_arvalid : out STD_LOGIC;
    M_AXI_RTL_awaddr : out STD_LOGIC_VECTOR ( 63 downto 0 );
    M_AXI_RTL_awburst : out STD_LOGIC_VECTOR ( 1 downto 0 );
    M_AXI_RTL_awcache : out STD_LOGIC_VECTOR ( 3 downto 0 );
    M_AXI_RTL_awid : out STD_LOGIC_VECTOR ( 5 downto 0 );
    M_AXI_RTL_awlen : out STD_LOGIC_VECTOR ( 7 downto 0 );
    M_AXI_RTL_awlock : out STD_LOGIC_VECTOR ( 0 to 0 );
    M_AXI_RTL_awprot : out STD_LOGIC_VECTOR ( 2 downto 0 );
    M_AXI_RTL_awqos : out STD_LOGIC_VECTOR ( 3 downto 0 );
    M_AXI_RTL_awready : in STD_LOGIC;
    M_AXI_RTL_awregion : out STD_LOGIC_VECTOR ( 3 downto 0 );
    M_AXI_RTL_awsize : out STD_LOGIC_VECTOR ( 2 downto 0 );
    M_AXI_RTL_awvalid : out STD_LOGIC;
    M_AXI_RTL_bid : in STD_LOGIC_VECTOR ( 5 downto 0 );
    M_AXI_RTL_bready : out STD_LOGIC;
    M_AXI_RTL_bresp : in STD_LOGIC_VECTOR ( 1 downto 0 );
    M_AXI_RTL_bvalid : in STD_LOGIC;
    M_AXI_RTL_rdata : in STD_LOGIC_VECTOR ( 511 downto 0 );
    M_AXI_RTL_rid : in STD_LOGIC_VECTOR ( 5 downto 0 );
    M_AXI_RTL_rlast : in STD_LOGIC;
    M_AXI_RTL_rready : out STD_LOGIC;
    M_AXI_RTL_rresp : in STD_LOGIC_VECTOR ( 1 downto 0 );
    M_AXI_RTL_rvalid : in STD_LOGIC;
    M_AXI_RTL_wdata : out STD_LOGIC_VECTOR ( 511 downto 0 );
    M_AXI_RTL_wlast : out STD_LOGIC;
    M_AXI_RTL_wready : in STD_LOGIC;
    M_AXI_RTL_wstrb : out STD_LOGIC_VECTOR ( 63 downto 0 );
    M_AXI_RTL_wvalid : out STD_LOGIC;
    S_AXI_PCIS_araddr : in STD_LOGIC_VECTOR ( 63 downto 0 );
    S_AXI_PCIS_arburst : in STD_LOGIC_VECTOR ( 1 downto 0 );
    S_AXI_PCIS_arcache : in STD_LOGIC_VECTOR ( 3 downto 0 );
    S_AXI_PCIS_arid : in STD_LOGIC_VECTOR ( 5 downto 0 );
    S_AXI_PCIS_arlen : in STD_LOGIC_VECTOR ( 7 downto 0 );
    S_AXI_PCIS_arlock : in STD_LOGIC_VECTOR ( 0 to 0 );
    S_AXI_PCIS_arprot : in STD_LOGIC_VECTOR ( 2 downto 0 );
    S_AXI_PCIS_arqos : in STD_LOGIC_VECTOR ( 3 downto 0 );
    S_AXI_PCIS_arready : out STD_LOGIC;
    S_AXI_PCIS_arregion : in STD_LOGIC_VECTOR ( 3 downto 0 );
    S_AXI_PCIS_arsize : in STD_LOGIC_VECTOR ( 2 downto 0 );
    S_AXI_PCIS_arvalid : in STD_LOGIC;
    S_AXI_PCIS_awaddr : in STD_LOGIC_VECTOR ( 63 downto 0 );
    S_AXI_PCIS_awburst : in STD_LOGIC_VECTOR ( 1 downto 0 );
    S_AXI_PCIS_awcache : in STD_LOGIC_VECTOR ( 3 downto 0 );
    S_AXI_PCIS_awid : in STD_LOGIC_VECTOR ( 5 downto 0 );
    S_AXI_PCIS_awlen : in STD_LOGIC_VECTOR ( 7 downto 0 );
    S_AXI_PCIS_awlock : in STD_LOGIC_VECTOR ( 0 to 0 );
    S_AXI_PCIS_awprot : in STD_LOGIC_VECTOR ( 2 downto 0 );
    S_AXI_PCIS_awqos : in STD_LOGIC_VECTOR ( 3 downto 0 );
    S_AXI_PCIS_awready : out STD_LOGIC;
    S_AXI_PCIS_awregion : in STD_LOGIC_VECTOR ( 3 downto 0 );
    S_AXI_PCIS_awsize : in STD_LOGIC_VECTOR ( 2 downto 0 );
    S_AXI_PCIS_awvalid : in STD_LOGIC;
    S_AXI_PCIS_bid : out STD_LOGIC_VECTOR ( 5 downto 0 );
    S_AXI_PCIS_bready : in STD_LOGIC;
    S_AXI_PCIS_bresp : out STD_LOGIC_VECTOR ( 1 downto 0 );
    S_AXI_PCIS_bvalid : out STD_LOGIC;
    S_AXI_PCIS_rdata : out STD_LOGIC_VECTOR ( 511 downto 0 );
    S_AXI_PCIS_rid : out STD_LOGIC_VECTOR ( 5 downto 0 );
    S_AXI_PCIS_rlast : out STD_LOGIC;
    S_AXI_PCIS_rready : in STD_LOGIC;
    S_AXI_PCIS_rresp : out STD_LOGIC_VECTOR ( 1 downto 0 );
    S_AXI_PCIS_rvalid : out STD_LOGIC;
    S_AXI_PCIS_wdata : in STD_LOGIC_VECTOR ( 511 downto 0 );
    S_AXI_PCIS_wlast : in STD_LOGIC;
    S_AXI_PCIS_wready : out STD_LOGIC;
    S_AXI_PCIS_wstrb : in STD_LOGIC_VECTOR ( 63 downto 0 );
    S_AXI_PCIS_wvalid : in STD_LOGIC
  );

end axi_interconnect_pcis;

architecture stub of axi_interconnect_pcis is
attribute syn_black_box : boolean;
attribute black_box_pad_pin : string;
attribute syn_black_box of stub : architecture is true;
attribute black_box_pad_pin of stub : architecture is "ACLK,ARESETN,M_AXI_DRAM_araddr[63:0],M_AXI_DRAM_arburst[1:0],M_AXI_DRAM_arcache[3:0],M_AXI_DRAM_arid[5:0],M_AXI_DRAM_arlen[7:0],M_AXI_DRAM_arlock[0:0],M_AXI_DRAM_arprot[2:0],M_AXI_DRAM_arqos[3:0],M_AXI_DRAM_arready,M_AXI_DRAM_arregion[3:0],M_AXI_DRAM_arsize[2:0],M_AXI_DRAM_arvalid,M_AXI_DRAM_awaddr[63:0],M_AXI_DRAM_awburst[1:0],M_AXI_DRAM_awcache[3:0],M_AXI_DRAM_awid[5:0],M_AXI_DRAM_awlen[7:0],M_AXI_DRAM_awlock[0:0],M_AXI_DRAM_awprot[2:0],M_AXI_DRAM_awqos[3:0],M_AXI_DRAM_awready,M_AXI_DRAM_awregion[3:0],M_AXI_DRAM_awsize[2:0],M_AXI_DRAM_awvalid,M_AXI_DRAM_bid[5:0],M_AXI_DRAM_bready,M_AXI_DRAM_bresp[1:0],M_AXI_DRAM_bvalid,M_AXI_DRAM_rdata[511:0],M_AXI_DRAM_rid[5:0],M_AXI_DRAM_rlast,M_AXI_DRAM_rready,M_AXI_DRAM_rresp[1:0],M_AXI_DRAM_rvalid,M_AXI_DRAM_wdata[511:0],M_AXI_DRAM_wlast,M_AXI_DRAM_wready,M_AXI_DRAM_wstrb[63:0],M_AXI_DRAM_wvalid,M_AXI_RTL_araddr[63:0],M_AXI_RTL_arburst[1:0],M_AXI_RTL_arcache[3:0],M_AXI_RTL_arid[5:0],M_AXI_RTL_arlen[7:0],M_AXI_RTL_arlock[0:0],M_AXI_RTL_arprot[2:0],M_AXI_RTL_arqos[3:0],M_AXI_RTL_arready,M_AXI_RTL_arregion[3:0],M_AXI_RTL_arsize[2:0],M_AXI_RTL_arvalid,M_AXI_RTL_awaddr[63:0],M_AXI_RTL_awburst[1:0],M_AXI_RTL_awcache[3:0],M_AXI_RTL_awid[5:0],M_AXI_RTL_awlen[7:0],M_AXI_RTL_awlock[0:0],M_AXI_RTL_awprot[2:0],M_AXI_RTL_awqos[3:0],M_AXI_RTL_awready,M_AXI_RTL_awregion[3:0],M_AXI_RTL_awsize[2:0],M_AXI_RTL_awvalid,M_AXI_RTL_bid[5:0],M_AXI_RTL_bready,M_AXI_RTL_bresp[1:0],M_AXI_RTL_bvalid,M_AXI_RTL_rdata[511:0],M_AXI_RTL_rid[5:0],M_AXI_RTL_rlast,M_AXI_RTL_rready,M_AXI_RTL_rresp[1:0],M_AXI_RTL_rvalid,M_AXI_RTL_wdata[511:0],M_AXI_RTL_wlast,M_AXI_RTL_wready,M_AXI_RTL_wstrb[63:0],M_AXI_RTL_wvalid,S_AXI_PCIS_araddr[63:0],S_AXI_PCIS_arburst[1:0],S_AXI_PCIS_arcache[3:0],S_AXI_PCIS_arid[5:0],S_AXI_PCIS_arlen[7:0],S_AXI_PCIS_arlock[0:0],S_AXI_PCIS_arprot[2:0],S_AXI_PCIS_arqos[3:0],S_AXI_PCIS_arready,S_AXI_PCIS_arregion[3:0],S_AXI_PCIS_arsize[2:0],S_AXI_PCIS_arvalid,S_AXI_PCIS_awaddr[63:0],S_AXI_PCIS_awburst[1:0],S_AXI_PCIS_awcache[3:0],S_AXI_PCIS_awid[5:0],S_AXI_PCIS_awlen[7:0],S_AXI_PCIS_awlock[0:0],S_AXI_PCIS_awprot[2:0],S_AXI_PCIS_awqos[3:0],S_AXI_PCIS_awready,S_AXI_PCIS_awregion[3:0],S_AXI_PCIS_awsize[2:0],S_AXI_PCIS_awvalid,S_AXI_PCIS_bid[5:0],S_AXI_PCIS_bready,S_AXI_PCIS_bresp[1:0],S_AXI_PCIS_bvalid,S_AXI_PCIS_rdata[511:0],S_AXI_PCIS_rid[5:0],S_AXI_PCIS_rlast,S_AXI_PCIS_rready,S_AXI_PCIS_rresp[1:0],S_AXI_PCIS_rvalid,S_AXI_PCIS_wdata[511:0],S_AXI_PCIS_wlast,S_AXI_PCIS_wready,S_AXI_PCIS_wstrb[63:0],S_AXI_PCIS_wvalid";
begin
end;
