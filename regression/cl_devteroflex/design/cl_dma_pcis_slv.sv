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

module cl_dma_pcis_slv

(
    input aclk,
    input aresetn,

    axi_bus_t.master cl_pcis_dram,
    axi_bus_t.master cl_rtl_dram,

    axi_bus_t.slave lcl_cl_sh_ddra,
    axi_bus_t.slave lcl_cl_sh_ddrb,
    axi_bus_t.slave lcl_cl_sh_ddrc,
    axi_bus_t.slave lcl_cl_sh_ddrd
);

localparam NUM_CFG_STGS_CL_DDR_ATG = 4;
localparam NUM_CFG_STGS_SH_DDR_ATG = 4;

//----------------------------
// Internal signals
//----------------------------
axi_bus_t axi_m_00G_16G_q();
axi_bus_t axi_m_16G_32G_q();
axi_bus_t axi_m_48G_64G_q();
axi_bus_t axi_m_32G_48G_q();

//----------------------------
// End Internal signals
//----------------------------

//reset synchronizers
(* dont_touch = "true" *) logic slr0_sync_aresetn;
lib_pipe #(.WIDTH(1), .STAGES(4)) SLR0_PIPE_RST_N (.clk(aclk), .rst_n(1'b1), .in_bus(aresetn), .out_bus(slr0_sync_aresetn));

//-----------------------------------------------------
//TIE-OFF unused signals to prevent critical warnings
//-----------------------------------------------------
   assign cl_pcis_dram.rid[15:6] = 10'b0 ;
   assign cl_pcis_dram.bid[15:6] = 10'b0 ;

//----------------------------
// axi interconnect for DDR address decodes
//----------------------------

(* dont_touch = "true" *) cl_dram_xbar AXI_CROSSBAR (
   .ACLK(aclk),
   .ARESETN(slr1_sync_aresetn),
   
   .M_00G_16G_AXI_araddr       (axi_m_00G_16G_q.araddr),
   .M_00G_16G_AXI_arburst      (),
   .M_00G_16G_AXI_arcache      (),
   .M_00G_16G_AXI_arid         (axi_m_00G_16G_q.arid),
   .M_00G_16G_AXI_arlen        (axi_m_00G_16G_q.arlen),
   .M_00G_16G_AXI_arlock       (),
   .M_00G_16G_AXI_arprot       (),
   .M_00G_16G_AXI_arqos        (),
   .M_00G_16G_AXI_arready      (axi_m_00G_16G_q.arready),
   .M_00G_16G_AXI_arregion     (),
   .M_00G_16G_AXI_arsize       (axi_m_00G_16G_q.arsize),
   .M_00G_16G_AXI_arvalid      (axi_m_00G_16G_q.arvalid),
   .M_00G_16G_AXI_awaddr       (axi_m_00G_16G_q.awaddr),
   .M_00G_16G_AXI_awburst      (),
   .M_00G_16G_AXI_awcache      (),
   .M_00G_16G_AXI_awid         (axi_m_00G_16G_q.awid),
   .M_00G_16G_AXI_awlen        (axi_m_00G_16G_q.awlen),
   .M_00G_16G_AXI_awlock       (),
   .M_00G_16G_AXI_awprot       (),
   .M_00G_16G_AXI_awqos        (),
   .M_00G_16G_AXI_awready      (axi_m_00G_16G_q.awready),
   .M_00G_16G_AXI_awregion     (),
   .M_00G_16G_AXI_awsize       (axi_m_00G_16G_q.awsize),
   .M_00G_16G_AXI_awvalid      (axi_m_00G_16G_q.awvalid),
   .M_00G_16G_AXI_bid          (axi_m_00G_16G_q.bid),
   .M_00G_16G_AXI_bready       (axi_m_00G_16G_q.bready),
   .M_00G_16G_AXI_bresp        (axi_m_00G_16G_q.bresp),
   .M_00G_16G_AXI_bvalid       (axi_m_00G_16G_q.bvalid),
   .M_00G_16G_AXI_rdata        (axi_m_00G_16G_q.rdata),
   .M_00G_16G_AXI_rid          (axi_m_00G_16G_q.rid),
   .M_00G_16G_AXI_rlast        (axi_m_00G_16G_q.rlast),
   .M_00G_16G_AXI_rready       (axi_m_00G_16G_q.rready),
   .M_00G_16G_AXI_rresp        (axi_m_00G_16G_q.rresp),
   .M_00G_16G_AXI_rvalid       (axi_m_00G_16G_q.rvalid),
   .M_00G_16G_AXI_wdata        (axi_m_00G_16G_q.wdata),
   .M_00G_16G_AXI_wlast        (axi_m_00G_16G_q.wlast),
   .M_00G_16G_AXI_wready       (axi_m_00G_16G_q.wready),
   .M_00G_16G_AXI_wstrb        (axi_m_00G_16G_q.wstrb),
   .M_00G_16G_AXI_wvalid       (axi_m_00G_16G_q.wvalid),
   
   .M_16G_32G_AXI_araddr       (axi_m_16G_32G_q.araddr),
   .M_16G_32G_AXI_arburst      (),
   .M_16G_32G_AXI_arcache      (),
   .M_16G_32G_AXI_arid         (axi_m_16G_32G_q.arid),
   .M_16G_32G_AXI_arlen        (axi_m_16G_32G_q.arlen),
   .M_16G_32G_AXI_arlock       (),
   .M_16G_32G_AXI_arprot       (),
   .M_16G_32G_AXI_arqos        (),
   .M_16G_32G_AXI_arready      (axi_m_16G_32G_q.arready),
   .M_16G_32G_AXI_arregion     (),
   .M_16G_32G_AXI_arsize       (axi_m_16G_32G_q.arsize),
   .M_16G_32G_AXI_arvalid      (axi_m_16G_32G_q.arvalid),
   .M_16G_32G_AXI_awaddr       (axi_m_16G_32G_q.awaddr),
   .M_16G_32G_AXI_awburst      (),
   .M_16G_32G_AXI_awcache      (),
   .M_16G_32G_AXI_awid         (axi_m_16G_32G_q.awid),
   .M_16G_32G_AXI_awlen        (axi_m_16G_32G_q.awlen),
   .M_16G_32G_AXI_awlock       (),
   .M_16G_32G_AXI_awprot       (),
   .M_16G_32G_AXI_awqos        (),
   .M_16G_32G_AXI_awready      (axi_m_16G_32G_q.awready),
   .M_16G_32G_AXI_awregion     (),
   .M_16G_32G_AXI_awsize       (axi_m_16G_32G_q.awsize),
   .M_16G_32G_AXI_awvalid      (axi_m_16G_32G_q.awvalid),
   .M_16G_32G_AXI_bid          (axi_m_16G_32G_q.bid),
   .M_16G_32G_AXI_bready       (axi_m_16G_32G_q.bready),
   .M_16G_32G_AXI_bresp        (axi_m_16G_32G_q.bresp),
   .M_16G_32G_AXI_bvalid       (axi_m_16G_32G_q.bvalid),
   .M_16G_32G_AXI_rdata        (axi_m_16G_32G_q.rdata),
   .M_16G_32G_AXI_rid          (axi_m_16G_32G_q.rid),
   .M_16G_32G_AXI_rlast        (axi_m_16G_32G_q.rlast),
   .M_16G_32G_AXI_rready       (axi_m_16G_32G_q.rready),
   .M_16G_32G_AXI_rresp        (axi_m_16G_32G_q.rresp),
   .M_16G_32G_AXI_rvalid       (axi_m_16G_32G_q.rvalid),
   .M_16G_32G_AXI_wdata        (axi_m_16G_32G_q.wdata),
   .M_16G_32G_AXI_wlast        (axi_m_16G_32G_q.wlast),
   .M_16G_32G_AXI_wready       (axi_m_16G_32G_q.wready),
   .M_16G_32G_AXI_wstrb        (axi_m_16G_32G_q.wstrb),
   .M_16G_32G_AXI_wvalid       (axi_m_16G_32G_q.wvalid),
   
   
   .M_32G_48G_AXI_araddr       (axi_m_32G_48G_q.araddr),
   .M_32G_48G_AXI_arburst      (),
   .M_32G_48G_AXI_arcache      (),
   .M_32G_48G_AXI_arid         (axi_m_32G_48G_q.arid),
   .M_32G_48G_AXI_arlen        (axi_m_32G_48G_q.arlen),
   .M_32G_48G_AXI_arlock       (),
   .M_32G_48G_AXI_arprot       (),
   .M_32G_48G_AXI_arqos        (),
   .M_32G_48G_AXI_arready      (axi_m_32G_48G_q.arready),
   .M_32G_48G_AXI_arregion     (),
   .M_32G_48G_AXI_arsize       (axi_m_32G_48G_q.arsize),
   .M_32G_48G_AXI_arvalid      (axi_m_32G_48G_q.arvalid),
   .M_32G_48G_AXI_awaddr       (axi_m_32G_48G_q.awaddr),
   .M_32G_48G_AXI_awburst      (),
   .M_32G_48G_AXI_awcache      (),
   .M_32G_48G_AXI_awid         (axi_m_32G_48G_q.awid),
   .M_32G_48G_AXI_awlen        (axi_m_32G_48G_q.awlen),
   .M_32G_48G_AXI_awlock       (),
   .M_32G_48G_AXI_awprot       (),
   .M_32G_48G_AXI_awqos        (),
   .M_32G_48G_AXI_awready      (axi_m_32G_48G_q.awready),
   .M_32G_48G_AXI_awregion     (),
   .M_32G_48G_AXI_awsize       (axi_m_32G_48G_q.awsize),
   .M_32G_48G_AXI_awvalid      (axi_m_32G_48G_q.awvalid),
   .M_32G_48G_AXI_bid          (axi_m_32G_48G_q.bid),
   .M_32G_48G_AXI_bready       (axi_m_32G_48G_q.bready),
   .M_32G_48G_AXI_bresp        (axi_m_32G_48G_q.bresp),
   .M_32G_48G_AXI_bvalid       (axi_m_32G_48G_q.bvalid),
   .M_32G_48G_AXI_rdata        (axi_m_32G_48G_q.rdata),
   .M_32G_48G_AXI_rid          (axi_m_32G_48G_q.rid),
   .M_32G_48G_AXI_rlast        (axi_m_32G_48G_q.rlast),
   .M_32G_48G_AXI_rready       (axi_m_32G_48G_q.rready),
   .M_32G_48G_AXI_rresp        (axi_m_32G_48G_q.rresp),
   .M_32G_48G_AXI_rvalid       (axi_m_32G_48G_q.rvalid),
   .M_32G_48G_AXI_wdata        (axi_m_32G_48G_q.wdata),
   .M_32G_48G_AXI_wlast        (axi_m_32G_48G_q.wlast),
   .M_32G_48G_AXI_wready       (axi_m_32G_48G_q.wready),
   .M_32G_48G_AXI_wstrb        (axi_m_32G_48G_q.wstrb),
   .M_32G_48G_AXI_wvalid       (axi_m_32G_48G_q.wvalid),
   
   .M_48G_64G_AXI_araddr       (axi_m_48G_64G_q.araddr),
   .M_48G_64G_AXI_arburst      (),
   .M_48G_64G_AXI_arcache      (),
   .M_48G_64G_AXI_arid         (axi_m_48G_64G_q.arid),
   .M_48G_64G_AXI_arlen        (axi_m_48G_64G_q.arlen),
   .M_48G_64G_AXI_arlock       (),
   .M_48G_64G_AXI_arprot       (),
   .M_48G_64G_AXI_arqos        (),
   .M_48G_64G_AXI_arready      (axi_m_48G_64G_q.arready),
   .M_48G_64G_AXI_arregion     (),
   .M_48G_64G_AXI_arsize       (axi_m_48G_64G_q.arsize),
   .M_48G_64G_AXI_arvalid      (axi_m_48G_64G_q.arvalid),
   .M_48G_64G_AXI_awaddr       (axi_m_48G_64G_q.awaddr),
   .M_48G_64G_AXI_awburst      (),
   .M_48G_64G_AXI_awcache      (),
   .M_48G_64G_AXI_awid         (axi_m_48G_64G_q.awid),
   .M_48G_64G_AXI_awlen        (axi_m_48G_64G_q.awlen),
   .M_48G_64G_AXI_awlock       (),
   .M_48G_64G_AXI_awprot       (),
   .M_48G_64G_AXI_awqos        (),
   .M_48G_64G_AXI_awready      (axi_m_48G_64G_q.awready),
   .M_48G_64G_AXI_awregion     (),
   .M_48G_64G_AXI_awsize       (axi_m_48G_64G_q.awsize),
   .M_48G_64G_AXI_awvalid      (axi_m_48G_64G_q.awvalid),
   .M_48G_64G_AXI_bid          (axi_m_48G_64G_q.bid),
   .M_48G_64G_AXI_bready       (axi_m_48G_64G_q.bready),
   .M_48G_64G_AXI_bresp        (axi_m_48G_64G_q.bresp),
   .M_48G_64G_AXI_bvalid       (axi_m_48G_64G_q.bvalid),
   .M_48G_64G_AXI_rdata        (axi_m_48G_64G_q.rdata),
   .M_48G_64G_AXI_rid          (axi_m_48G_64G_q.rid),
   .M_48G_64G_AXI_rlast        (axi_m_48G_64G_q.rlast),
   .M_48G_64G_AXI_rready       (axi_m_48G_64G_q.rready),
   .M_48G_64G_AXI_rresp        (axi_m_48G_64G_q.rresp),
   .M_48G_64G_AXI_rvalid       (axi_m_48G_64G_q.rvalid),
   .M_48G_64G_AXI_wdata        (axi_m_48G_64G_q.wdata),
   .M_48G_64G_AXI_wlast        (axi_m_48G_64G_q.wlast),
   .M_48G_64G_AXI_wready       (axi_m_48G_64G_q.wready),
   .M_48G_64G_AXI_wstrb        (axi_m_48G_64G_q.wstrb),
   .M_48G_64G_AXI_wvalid       (axi_m_48G_64G_q.wvalid),
   
   .S_PCIS_DRAM_AXI_araddr       (cl_pcis_dram.araddr),
   .S_PCIS_DRAM_AXI_arburst      (2'b1),
   .S_PCIS_DRAM_AXI_arcache      (4'b11),
   .S_PCIS_DRAM_AXI_arid         (cl_pcis_dram.arid),
   .S_PCIS_DRAM_AXI_arlen        (cl_pcis_dram.arlen),
   .S_PCIS_DRAM_AXI_arlock       (1'b0),
   .S_PCIS_DRAM_AXI_arprot       (3'b10),
   .S_PCIS_DRAM_AXI_arqos        (4'b0),
   .S_PCIS_DRAM_AXI_arready      (cl_pcis_dram.arready),
   .S_PCIS_DRAM_AXI_arregion     (4'b0),
   .S_PCIS_DRAM_AXI_arsize       (cl_pcis_dram.arsize),
   .S_PCIS_DRAM_AXI_arvalid      (cl_pcis_dram.arvalid),
   .S_PCIS_DRAM_AXI_awaddr       (cl_pcis_dram.awaddr),
   .S_PCIS_DRAM_AXI_awburst      (2'b1),
   .S_PCIS_DRAM_AXI_awcache      (4'b11),
   .S_PCIS_DRAM_AXI_awid         (cl_pcis_dram.awid),
   .S_PCIS_DRAM_AXI_awlen        (cl_pcis_dram.awlen),
   .S_PCIS_DRAM_AXI_awlock       (1'b0),
   .S_PCIS_DRAM_AXI_awprot       (3'b10),
   .S_PCIS_DRAM_AXI_awqos        (4'b0),
   .S_PCIS_DRAM_AXI_awready      (cl_pcis_dram.awready),
   .S_PCIS_DRAM_AXI_awregion     (4'b0),
   .S_PCIS_DRAM_AXI_awsize       (cl_pcis_dram.awsize),
   .S_PCIS_DRAM_AXI_awvalid      (cl_pcis_dram.awvalid),
   .S_PCIS_DRAM_AXI_bid          (cl_pcis_dram.bid),
   .S_PCIS_DRAM_AXI_bready       (cl_pcis_dram.bready),
   .S_PCIS_DRAM_AXI_bresp        (cl_pcis_dram.bresp),
   .S_PCIS_DRAM_AXI_bvalid       (cl_pcis_dram.bvalid),
   .S_PCIS_DRAM_AXI_rdata        (cl_pcis_dram.rdata),
   .S_PCIS_DRAM_AXI_rid          (cl_pcis_dram.rid),
   .S_PCIS_DRAM_AXI_rlast        (cl_pcis_dram.rlast),
   .S_PCIS_DRAM_AXI_rready       (cl_pcis_dram.rready),
   .S_PCIS_DRAM_AXI_rresp        (cl_pcis_dram.rresp),
   .S_PCIS_DRAM_AXI_rvalid       (cl_pcis_dram.rvalid),
   .S_PCIS_DRAM_AXI_wdata        (cl_pcis_dram.wdata),
   .S_PCIS_DRAM_AXI_wlast        (cl_pcis_dram.wlast),
   .S_PCIS_DRAM_AXI_wready       (cl_pcis_dram.wready),
   .S_PCIS_DRAM_AXI_wstrb        (cl_pcis_dram.wstrb),
   .S_PCIS_DRAM_AXI_wvalid       (cl_pcis_dram.wvalid),
   
   .S_RTL_DRAM_AXI_araddr       (cl_rtl_dram.araddr),
   .S_RTL_DRAM_AXI_arburst      (2'b1),
   .S_RTL_DRAM_AXI_arcache      (4'b11),
   .S_RTL_DRAM_AXI_arid         (cl_rtl_dram.arid),
   .S_RTL_DRAM_AXI_arlen        (cl_rtl_dram.arlen),
   .S_RTL_DRAM_AXI_arlock       (1'b0),
   .S_RTL_DRAM_AXI_arprot       (3'b10),
   .S_RTL_DRAM_AXI_arqos        (4'b0),
   .S_RTL_DRAM_AXI_arready      (cl_rtl_dram.arready),
   .S_RTL_DRAM_AXI_arregion     (4'b0),
   .S_RTL_DRAM_AXI_arsize       (cl_rtl_dram.arsize),
   .S_RTL_DRAM_AXI_arvalid      (cl_rtl_dram.arvalid),
   .S_RTL_DRAM_AXI_awaddr       (cl_rtl_dram.awaddr),
   .S_RTL_DRAM_AXI_awburst      (2'b1),
   .S_RTL_DRAM_AXI_awcache      (4'b11),
   .S_RTL_DRAM_AXI_awid         (cl_rtl_dram.awid),
   .S_RTL_DRAM_AXI_awlen        (cl_rtl_dram.awlen),
   .S_RTL_DRAM_AXI_awlock       (1'b0),
   .S_RTL_DRAM_AXI_awprot       (3'b10),
   .S_RTL_DRAM_AXI_awqos        (4'b0),
   .S_RTL_DRAM_AXI_awready      (cl_rtl_dram.awready),
   .S_RTL_DRAM_AXI_awregion     (4'b0),
   .S_RTL_DRAM_AXI_awsize       (cl_rtl_dram.awsize),
   .S_RTL_DRAM_AXI_awvalid      (cl_rtl_dram.awvalid),
   .S_RTL_DRAM_AXI_bid          (cl_rtl_dram.bid),
   .S_RTL_DRAM_AXI_bready       (cl_rtl_dram.bready),
   .S_RTL_DRAM_AXI_bresp        (cl_rtl_dram.bresp),
   .S_RTL_DRAM_AXI_bvalid       (cl_rtl_dram.bvalid),
   .S_RTL_DRAM_AXI_rdata        (cl_rtl_dram.rdata),
   .S_RTL_DRAM_AXI_rid          (cl_rtl_dram.rid),
   .S_RTL_DRAM_AXI_rlast        (cl_rtl_dram.rlast),
   .S_RTL_DRAM_AXI_rready       (cl_rtl_dram.rready),
   .S_RTL_DRAM_AXI_rresp        (cl_rtl_dram.rresp),
   .S_RTL_DRAM_AXI_rvalid       (cl_rtl_dram.rvalid),
   .S_RTL_DRAM_AXI_wdata        (cl_rtl_dram.wdata),
   .S_RTL_DRAM_AXI_wlast        (cl_rtl_dram.wlast),
   .S_RTL_DRAM_AXI_wready       (cl_rtl_dram.wready),
   .S_RTL_DRAM_AXI_wstrb        (cl_rtl_dram.wstrb),
   .S_RTL_DRAM_AXI_wvalid       (cl_rtl_dram.wvalid)
);

cl_axi_rename AXI_00G_16G_BUS ( .s_axi_bus (axi_m_00G_16G_q), .m_axi_bus (lcl_cl_sh_ddrc) );
cl_axi_rename AXI_16G_32G_BUS ( .s_axi_bus (axi_m_16G_32G_q), .m_axi_bus (lcl_cl_sh_ddrb) );
cl_axi_rename AXI_32G_48G_BUS ( .s_axi_bus (axi_m_32G_48G_q), .m_axi_bus (lcl_cl_sh_ddra) );
cl_axi_rename AXI_48G_64G_BUS ( .s_axi_bus (axi_m_48G_64G_q), .m_axi_bus (lcl_cl_sh_ddrd) );
 
assign lcl_cl_sh_ddra.awid[15:9] = 7'b0;
assign lcl_cl_sh_ddra.wid[15:9] = 7'b0;
assign lcl_cl_sh_ddra.arid[15:9] = 7'b0;
 
assign lcl_cl_sh_ddrb.awid[15:9] = 7'b0;
assign lcl_cl_sh_ddrb.wid[15:9] = 7'b0;
assign lcl_cl_sh_ddrb.arid[15:9] = 7'b0;

assign lcl_cl_sh_ddrd.awid[15:9] = 7'b0;
assign lcl_cl_sh_ddrd.wid[15:9] = 7'b0;
assign lcl_cl_sh_ddrd.arid[15:9] = 7'b0;

endmodule

