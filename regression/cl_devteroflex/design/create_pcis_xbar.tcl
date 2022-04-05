#create_bd_design sh_pcis_xbar

# Create ports
#set ACLK [ create_bd_port -dir I -type clk ACLK -freq_hz 250000000]
#set ARESETN [ create_bd_port -dir I -type rst ARESETN ]

# Create interface ports
set M_AXI_DRAM [ create_bd_intf_port -mode Master -vlnv xilinx.com:interface:aximm_rtl:1.0 M_AXI_DRAM ]
set_property -dict [ list \
 CONFIG.ADDR_WIDTH {64} \
 CONFIG.DATA_WIDTH {512} \
 CONFIG.NUM_READ_OUTSTANDING {16} \
 CONFIG.NUM_WRITE_OUTSTANDING {16} \
 CONFIG.PROTOCOL {AXI4} \
 ] $M_AXI_DRAM
set M_AXI_RTL [ create_bd_intf_port -mode Master -vlnv xilinx.com:interface:aximm_rtl:1.0 M_AXI_RTL]
set_property -dict [ list \
 CONFIG.ADDR_WIDTH {64} \
 CONFIG.DATA_WIDTH {512} \
 CONFIG.NUM_READ_OUTSTANDING {16} \
 CONFIG.NUM_WRITE_OUTSTANDING {16} \
 CONFIG.PROTOCOL {AXI4} \
 ] $M_AXI_RTL
set S_AXI_PCIS [ create_bd_intf_port -mode Slave -vlnv xilinx.com:interface:aximm_rtl:1.0 S_AXI_PCIS ]
set_property -dict [ list \
 CONFIG.ADDR_WIDTH {64} \
 CONFIG.ARUSER_WIDTH {0} \
 CONFIG.AWUSER_WIDTH {0} \
 CONFIG.BUSER_WIDTH {0} \
 CONFIG.DATA_WIDTH {512} \
 CONFIG.HAS_BRESP {1} \
 CONFIG.HAS_BURST {1} \
 CONFIG.HAS_CACHE {1} \
 CONFIG.HAS_LOCK {1} \
 CONFIG.HAS_PROT {1} \
 CONFIG.HAS_QOS {1} \
 CONFIG.HAS_REGION {1} \
 CONFIG.HAS_RRESP {1} \
 CONFIG.HAS_WSTRB {1} \
 CONFIG.ID_WIDTH {6} \
 CONFIG.MAX_BURST_LENGTH {256} \
 CONFIG.NUM_READ_OUTSTANDING {32} \
 CONFIG.NUM_READ_THREADS {16} \
 CONFIG.NUM_WRITE_OUTSTANDING {32} \
 CONFIG.NUM_WRITE_THREADS {16} \
 CONFIG.PROTOCOL {AXI4} \
 CONFIG.READ_WRITE_MODE {READ_WRITE} \
 CONFIG.RUSER_BITS_PER_BYTE {0} \
 CONFIG.RUSER_WIDTH {0} \
 CONFIG.SUPPORTS_NARROW_BURST {1} \
 CONFIG.WUSER_BITS_PER_BYTE {0} \
 CONFIG.WUSER_WIDTH {0} \
 ] $S_AXI_PCIS

set_property CONFIG.ASSOCIATED_BUSIF {M_AXI_DRAM} [get_bd_ports /ACLK]
set_property CONFIG.ASSOCIATED_BUSIF {M_AXI_RTL}  [get_bd_ports /ACLK]
set_property CONFIG.ASSOCIATED_BUSIF {S_AXI_PCIS} [get_bd_ports /ACLK]

# Create instance: sh_pcis_xbar, and set properties
set axi_interconnect_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_interconnect axi_interconnect_0 ]
set_property -dict [ list \
  CONFIG.ENABLE_ADVANCED_OPTIONS {1} \
  CONFIG.NUM_MI {2} \
  CONFIG.NUM_SI {1} \
  CONFIG.M00_HAS_REGSLICE {4} \
  CONFIG.M01_HAS_REGSLICE {4} \
  CONFIG.S00_HAS_REGSLICE {4} \
  CONFIG.SYNCHRONIZATION_STAGES {2} \
  CONFIG.XBAR_DATA_WIDTH {512} \
] $axi_interconnect_0

# Create interface connections
connect_bd_intf_net -intf_net S_AXI_PCIS_IO [get_bd_intf_ports S_AXI_PCIS] [get_bd_intf_pins axi_interconnect_0/S00_AXI]
connect_bd_intf_net -intf_net M_AXI_DRAM_IO [get_bd_intf_ports M_AXI_DRAM] [get_bd_intf_pins axi_interconnect_0/M00_AXI]
connect_bd_intf_net -intf_net M_AXI_RTL_IO   [get_bd_intf_ports M_AXI_RTL]   [get_bd_intf_pins axi_interconnect_0/M01_AXI]

# Create port connections
connect_bd_net -net ACLK_WIRE [get_bd_ports ACLK] \
  [get_bd_pins axi_interconnect_0/ACLK]     \
  [get_bd_pins axi_interconnect_0/M00_ACLK] \
  [get_bd_pins axi_interconnect_0/M01_ACLK] \
  [get_bd_pins axi_interconnect_0/S00_ACLK]
  
connect_bd_net -net ARESETN_WIRE [get_bd_ports ARESETN] \
  [get_bd_pins axi_interconnect_0/ARESETN]     \
  [get_bd_pins axi_interconnect_0/M00_ARESETN] \
  [get_bd_pins axi_interconnect_0/M01_ARESETN] \
  [get_bd_pins axi_interconnect_0/S00_ARESETN]

# Create address segments
create_bd_addr_seg -range 0x001000000000 -offset 0x000000000000 [get_bd_addr_spaces S_AXI_PCIS] [get_bd_addr_segs M_AXI_DRAM/Reg] SEG_M00_AXI_Reg
create_bd_addr_seg -range 0x000100000000 -offset 0x001000000000 [get_bd_addr_spaces S_AXI_PCIS] [get_bd_addr_segs M_AXI_RTL/Reg]  SEG_M01_AXI_Reg
