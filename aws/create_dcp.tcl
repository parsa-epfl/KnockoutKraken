#################################################
## Initialize the project (set the locatioin of Verilog files)
#################################################
set armflex_verilog_dir "../Verilog"
create_project -name armflex
aws::make_ipi
# enable PCI auxilary
set_property -dict [list CONFIG.PCIS_PRESENT {1} CONFIG.BAR1_PRESENT {1} CONFIG.AUX_PRESENT {1}] [get_bd_cells f1_inst]
# set clock to 125
set_property -dict [list CONFIG.CLOCK_A_RECIPE {0} CONFIG.CLOCK_A0_FREQ {125000000} CONFIG.CLOCK_A1_FREQ {62500000} CONFIG.CLOCK_A2_FREQ {187500000} CONFIG.CLOCK_A3_FREQ {250000000}] [get_bd_cells f1_inst]
# import verilog files to design
import_files -force $armflex_verilog_dir
create_bd_cell -type module -reference ProcAxiWrap ProcAxiWrap_0
apply_bd_automation -rule xilinx.com:bd_rule:axi4 -config { Clk_master {/f1_inst/clk_main_a0_out (250 MHz)} Clk_slave {Auto} Clk_xbar {Auto} Master {/f1_inst/M_AXI_BAR1} Slave {/ProcAxiWrap_0/io_axiLite} intc_ip {New AXI Interconnect} master_apm {0}}  [get_bd_intf_pins ProcAxiWrap_0/io_axiLite]

# add bram controllers
create_bd_cell -type ip -vlnv xilinx.com:ip:axi_bram_ctrl:4.1 axi_bram_ctrl_0
set_property -dict [list CONFIG.SINGLE_PORT_BRAM {1}] [get_bd_cells axi_bram_ctrl_0]
set_property -dict [list CONFIG.DATA_WIDTH {64} CONFIG.ECC_TYPE {0}] [get_bd_cells axi_bram_ctrl_0]

create_bd_cell -type ip -vlnv xilinx.com:ip:axi_bram_ctrl:4.1 axi_bram_ctrl_1
set_property -dict [list CONFIG.SINGLE_PORT_BRAM {1}] [get_bd_cells axi_bram_ctrl_1]
set_property -dict [list CONFIG.DATA_WIDTH {64} CONFIG.ECC_TYPE {0}] [get_bd_cells axi_bram_ctrl_1]

connect_bd_intf_net [get_bd_intf_pins axi_bram_ctrl_0/BRAM_PORTA] [get_bd_intf_pins ProcAxiWrap_0/stateBRAM]
connect_bd_intf_net [get_bd_intf_pins axi_bram_ctrl_1/BRAM_PORTA] [get_bd_intf_pins ProcAxiWrap_0/memoryBRAM]

apply_bd_automation -rule xilinx.com:bd_rule:axi4 -config { Clk_master {/f1_inst/clk_main_a0_out (125 MHz)} Clk_slave {Auto} Clk_xbar {Auto} Master {/f1_inst/M_AXI_PCIS} Slave {/axi_bram_ctrl_0/S_AXI} intc_ip {Auto} master_apm {0}}  [get_bd_intf_pins axi_bram_ctrl_0/S_AXI]
apply_bd_automation -rule xilinx.com:bd_rule:axi4 -config { Clk_master {/f1_inst/clk_main_a0_out (125 MHz)} Clk_slave {Auto} Clk_xbar {Auto} Master {/f1_inst/M_AXI_PCIS} Slave {/axi_bram_ctrl_1/S_AXI} intc_ip {Auto} master_apm {0}}  [get_bd_intf_pins axi_bram_ctrl_1/S_AXI]

# invert the reset pin of ProcAxiWrap
disconnect_bd_net /f1_inst_rst_main_n_out [get_bd_pins ProcAxiWrap_0/reset]
create_bd_cell -type ip -vlnv xilinx.com:ip:util_vector_logic:2.0 util_vector_logic_0
set_property -dict [list CONFIG.C_SIZE {1} CONFIG.C_OPERATION {not} CONFIG.LOGO_FILE {data/sym_notgate.png}] [get_bd_cells util_vector_logic_0]
connect_bd_net [get_bd_pins util_vector_logic_0/Res] [get_bd_pins ProcAxiWrap_0/reset]
connect_bd_net [get_bd_pins util_vector_logic_0/Op1] [get_bd_pins f1_inst/rst_main_n_out]

# run implementation
save_bd_design
launch_runs synth_1 -jobs 12
launch_runs impl_1 -jobs 12

