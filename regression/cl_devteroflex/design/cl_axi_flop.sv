// Amazon FPGA Hardware Development Kit
//
// Copyright 2016 Amazon.com, Inc. or its affiliates. All Rights Reserved.
//
// Licensed under the Amazon Software License (the "License"). You may not use
// this file except in compliance with the License. A copy of the License is
// located at
//
//    http://aws.amazon.com/asl/
//
// or in the "license" file accompanying this file. This file is distributed on
// an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, express or
// implied. See the License for the specific language governing permissions and
// limitations under the License.

module cl_axi_flop (
   
   input clk,
   input sync_rst_n,

   axi_bus_t.master s_axi_bus,

   axi_bus_t.slave m_axi_bus
);

   axi_register_slice AXI_REG_SLC (
       .aclk          (clk),
       .aresetn       (sync_rst_n),
       .s_axi_awid    (s_axi_bus.awid),
       .s_axi_awaddr  (s_axi_bus.awaddr),
       .s_axi_awlen   (s_axi_bus.awlen),
       .s_axi_awvalid (s_axi_bus.awvalid),
       .s_axi_awsize  (s_axi_bus.awsize),
       .s_axi_awready (s_axi_bus.awready),
       .s_axi_wdata   (s_axi_bus.wdata),
       .s_axi_wstrb   (s_axi_bus.wstrb),
       .s_axi_wlast   (s_axi_bus.wlast),
       .s_axi_wvalid  (s_axi_bus.wvalid),
       .s_axi_wready  (s_axi_bus.wready),
       .s_axi_bid     (s_axi_bus.bid),
       .s_axi_bresp   (s_axi_bus.bresp),
       .s_axi_bvalid  (s_axi_bus.bvalid),
       .s_axi_bready  (s_axi_bus.bready),
       .s_axi_arid    (s_axi_bus.arid),
       .s_axi_araddr  (s_axi_bus.araddr),
       .s_axi_arlen   (s_axi_bus.arlen),
       .s_axi_arvalid (s_axi_bus.arvalid),
       .s_axi_arsize  (s_axi_bus.arsize),
       .s_axi_arready (s_axi_bus.arready),
       .s_axi_rid     (s_axi_bus.rid),
       .s_axi_rdata   (s_axi_bus.rdata),
       .s_axi_rresp   (s_axi_bus.rresp),
       .s_axi_rlast   (s_axi_bus.rlast),
       .s_axi_rvalid  (s_axi_bus.rvalid),
       .s_axi_rready  (s_axi_bus.rready),

       .m_axi_awid    (m_axi_bus.awid),
       .m_axi_awaddr  (m_axi_bus.awaddr),
       .m_axi_awlen   (m_axi_bus.awlen),
       .m_axi_awvalid (m_axi_bus.awvalid),
       .m_axi_awsize  (m_axi_bus.awsize),
       .m_axi_awready (m_axi_bus.awready),
       .m_axi_wdata   (m_axi_bus.wdata),
       .m_axi_wstrb   (m_axi_bus.wstrb),
       .m_axi_wvalid  (m_axi_bus.wvalid),
       .m_axi_wlast   (m_axi_bus.wlast),
       .m_axi_wready  (m_axi_bus.wready),
       .m_axi_bresp   (m_axi_bus.bresp),
       .m_axi_bvalid  (m_axi_bus.bvalid),
       .m_axi_bid     (m_axi_bus.bid),
       .m_axi_bready  (m_axi_bus.bready),
       .m_axi_arid    (m_axi_bus.arid),
       .m_axi_araddr  (m_axi_bus.araddr),
       .m_axi_arlen   (m_axi_bus.arlen),
       .m_axi_arsize  (m_axi_bus.arsize),
       .m_axi_arvalid (m_axi_bus.arvalid),
       .m_axi_arready (m_axi_bus.arready),
       .m_axi_rid     (m_axi_bus.rid),
       .m_axi_rdata   (m_axi_bus.rdata),
       .m_axi_rresp   (m_axi_bus.rresp),
       .m_axi_rlast   (m_axi_bus.rlast),
       .m_axi_rvalid  (m_axi_bus.rvalid),
       .m_axi_rready  (m_axi_bus.rready)
   );

 
endmodule
