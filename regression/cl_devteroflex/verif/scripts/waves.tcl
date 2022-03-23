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
add_wave {{/tb/card/fpga/CL/sh_cl_dma_pcis_wvalid}} {{/tb/card/fpga/CL/sh_cl_dma_pcis_wstrb}} {{/tb/card/fpga/CL/sh_cl_dma_pcis_wlast}} {{/tb/card/fpga/CL/sh_cl_dma_pcis_wdata}} {{/tb/card/fpga/CL/sh_cl_dma_pcis_rready}} {{/tb/card/fpga/CL/sh_cl_dma_pcis_bready}} {{/tb/card/fpga/CL/sh_cl_dma_pcis_awvalid}} {{/tb/card/fpga/CL/sh_cl_dma_pcis_awsize}} {{/tb/card/fpga/CL/sh_cl_dma_pcis_awlen}} {{/tb/card/fpga/CL/sh_cl_dma_pcis_awid}} {{/tb/card/fpga/CL/sh_cl_dma_pcis_awaddr}} {{/tb/card/fpga/CL/sh_cl_dma_pcis_arvalid}} {{/tb/card/fpga/CL/sh_cl_dma_pcis_arsize}} {{/tb/card/fpga/CL/sh_cl_dma_pcis_arlen}} {{/tb/card/fpga/CL/sh_cl_dma_pcis_arid}} {{/tb/card/fpga/CL/sh_cl_dma_pcis_araddr}} {{/tb/card/fpga/CL/dma_pcis_slv_sync_rst_n}} {{/tb/card/fpga/CL/cl_sh_dma_pcis_wready}} {{/tb/card/fpga/CL/cl_sh_dma_pcis_rvalid}} {{/tb/card/fpga/CL/cl_sh_dma_pcis_rresp}} {{/tb/card/fpga/CL/cl_sh_dma_pcis_rlast}} {{/tb/card/fpga/CL/cl_sh_dma_pcis_rid}} {{/tb/card/fpga/CL/cl_sh_dma_pcis_rdata}} {{/tb/card/fpga/CL/cl_sh_dma_pcis_bvalid}} {{/tb/card/fpga/CL/cl_sh_dma_pcis_bresp}} {{/tb/card/fpga/CL/cl_sh_dma_pcis_bid}} {{/tb/card/fpga/CL/cl_sh_dma_pcis_awready}} {{/tb/card/fpga/CL/cl_sh_dma_pcis_arready}} 

add_wave {{/tb/card/fpga/CL/CL_DMA_PCIS_SLV/aclk}} 

run 200 us 
quit
