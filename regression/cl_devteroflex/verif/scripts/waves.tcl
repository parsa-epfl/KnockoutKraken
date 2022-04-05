# Amazon FPGA Hardware Development Kit
#
# Copyright 2016 Amazon.com, Inc. or its affiliates. All Rights Reserved.
#
# Licensed under the Amazon Software License (the "License"). You may not use
# this file except in compliance with the License. A copy of the License is
# located at
#
#    http://aws.amazon.com/asl/
#
# or in the "license" file accompanying this file. This file is distributed on
# an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, express or
# implied. See the License for the specific language governing permissions and
# limitations under the License.

set curr_wave [current_wave_config]
if { [string length $curr_wave] == 0 } {
  if { [llength [get_objects]] > 0} {
    add_wave /
    set_property needs_save false [current_wave_config]
  } else {
     send_msg_id Add_Wave-1 WARNING "No top level signals found. Simulator will start without a wave window. If you want to open a wave window go to 'File->New Waveform Configuration' or type 'create_wave_config' in the TCL console."
  }
}


add_wave \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_awaddr}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_awsize}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_awvalid}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_awready}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_wdata}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_wlast}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_wvalid}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_wready}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_bvalid}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_bready}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_araddr}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_arlen}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_arsize}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_arvalid}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_arready}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_rdata}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_rresp}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_rlast}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_rvalid}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/M_AXI_rready}} 

add_wave \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_awaddr}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_awlen}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_awsize}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_awvalid}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_awready}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_wdata}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_wlast}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_wvalid}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_wready}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_bresp}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_bvalid}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_bready}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_araddr}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_arlen}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_arsize}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_arvalid}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_arready}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_rdata}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_rresp}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_rlast}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_rvalid}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXI_rready}} 

add_wave \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_awaddr}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_awvalid}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_awready}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_wdata}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_wvalid}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_wready}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_bresp}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_bvalid}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_bready}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_araddr}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_arvalid}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_arready}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_rdata}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_rresp}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_rvalid}} \
	{{/tb/card/fpga/CL/DEVTEROFLEX_TOP/S_AXIL_rready}} 

add_wave \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_arvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_arvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_arvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_arready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_arready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_arready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_araddr}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_araddr}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_araddr}} \
	\
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_wdata}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_wdata}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_wdata}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_wlast}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_wlast}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_wlast}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_wvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_wvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_wvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_wready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_wready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_wready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_bresp}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_bresp}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_bresp}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_bvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_bvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_bvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_bready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_bready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_bready}} \
	\
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_awaddr}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_awaddr}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_awaddr}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_awvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_awvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_awvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_awready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_awready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_awready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_rdata}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_rdata}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_rdata}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_rvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_rvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_rvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_rready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_rready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_rready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_rlast}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_rlast}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_rlast}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_AXI_PCIS_rresp}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/S_RTL_DRAM_AXI_rresp}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/AXI_CROSSBAR/M_AXI_RTL_rresp}}
	

add_wave {{/tb/card/fpga/CL/clk_main_a0}} 
#add_wave \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_arvalid}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_arvalid}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_arvalid}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_arready}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_arready}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_arready}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_araddr}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_araddr}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_araddr}} \
#	\
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_wdata}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_wdata}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_wdata}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_wlast}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_wlast}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_wlast}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_wvalid}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_wvalid}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_wvalid}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_wready}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_wready}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_wready}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_bresp}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_bresp}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_bresp}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_bvalid}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_bvalid}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_bvalid}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_bready}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_bready}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_bready}} \
#	\
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_awaddr}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_awaddr}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_awaddr}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_awvalid}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_awvalid}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_awvalid}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_awready}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_awready}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_awready}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_rdata}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_rdata}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_rdata}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_rvalid}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_rvalid}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_rvalid}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_rready}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_rready}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_rready}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_rlast}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_rlast}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_rlast}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/S00_AXI_rresp}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M00_AXI_rresp}} \
#	{{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/AXI_CROSSBAR/M02_AXI_rresp}}

add_wave {{/tb/card/fpga/CL/clk_main_a0}} 
add_wave \
	{{/tb/card/fpga/CL/cl_sh_dma_pcis_awready}} \
	{{/tb/card/fpga/CL/sh_cl_dma_pcis_awvalid}} \
	{{/tb/card/fpga/CL/sh_cl_dma_pcis_awaddr}} \
	{{/tb/card/fpga/CL/cl_sh_dma_pcis_wready}} \
	{{/tb/card/fpga/CL/sh_cl_dma_pcis_wvalid}} \
	{{/tb/card/fpga/CL/sh_cl_dma_pcis_wdata}} \
	{{/tb/card/fpga/CL/cl_sh_dma_pcis_arready}} \
	{{/tb/card/fpga/CL/sh_cl_dma_pcis_arvalid}} \
	{{/tb/card/fpga/CL/sh_cl_dma_pcis_araddr}} \
	{{/tb/card/fpga/CL/sh_cl_dma_pcis_rready}} \
	{{/tb/card/fpga/CL/cl_sh_dma_pcis_rvalid}} \
	{{/tb/card/fpga/CL/cl_sh_dma_pcis_rresp}} \
	{{/tb/card/fpga/CL/cl_sh_dma_pcis_rdata}} \
	{{/tb/card/fpga/CL/sh_cl_dma_pcis_bready}} \
	{{/tb/card/fpga/CL/cl_sh_dma_pcis_bvalid}} \
	{{/tb/card/fpga/CL/cl_sh_dma_pcis_bresp}}


add_wave \
	{{/tb/card/fpga/cl_sh_ddr_awaddr}} \
	{{/tb/card/fpga/cl_sh_ddr_awvalid}} \
	{{/tb/card/fpga/sh_cl_ddr_awready}} \
	{{/tb/card/fpga/cl_sh_ddr_wdata}} \
	{{/tb/card/fpga/cl_sh_ddr_wvalid}} \
	{{/tb/card/fpga/sh_cl_ddr_wready}} \
	{{/tb/card/fpga/sh_cl_ddr_bresp}} \
	{{/tb/card/fpga/sh_cl_ddr_bvalid}} \
	{{/tb/card/fpga/cl_sh_ddr_bready}} \
	{{/tb/card/fpga/cl_sh_ddr_araddr}} \
	{{/tb/card/fpga/cl_sh_ddr_arvalid}} \
	{{/tb/card/fpga/sh_cl_ddr_arready}} \
	{{/tb/card/fpga/sh_cl_ddr_rdata}} \
	{{/tb/card/fpga/sh_cl_ddr_rresp}} \
	{{/tb/card/fpga/sh_cl_ddr_rlast}} \
	{{/tb/card/fpga/sh_cl_ddr_rvalid}} \
	{{/tb/card/fpga/cl_sh_ddr_rready}} 

add_wave \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/awaddr}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/awaddr}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/awaddr}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/awaddr}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/awvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/awvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/awvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/awvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/awready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/awready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/awready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/awready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/wdata}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/wdata}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/wdata}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/wdata}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/wvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/wvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/wvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/wvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/wready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/wready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/wready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/wready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/bresp}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/bresp}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/bresp}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/bresp}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/bvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/bvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/bvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/bvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/bready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/bready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/bready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/bready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/araddr}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/araddr}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/araddr}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/araddr}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/arvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/arvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/arvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/arvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/arready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/arready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/arready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/arready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/rdata}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/rdata}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/rdata}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/rdata}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/rresp}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/rresp}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/rresp}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/rresp}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/rlast}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/rlast}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/rlast}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/rlast}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/rvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/rvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/rvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/rvalid}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddra/rready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrb/rready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrc/rready}} \
	{{/tb/card/fpga/CL/CL_XBAR_GENERAL/lcl_cl_sh_ddrd/rready}} 

run 200 us 
quit