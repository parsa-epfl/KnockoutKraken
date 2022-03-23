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

module cl_pcis_interconnect

(
    input aclk,
    input aresetn,

    axi_bus_t.master sh_pcis_bus,

    axi_bus_t.slave cl_pcis_dram,
    axi_bus_t.slave cl_pcis_rtl
);

//----------------------------
// Internal signals
//----------------------------
axi_bus_t sh_pcis_bus_q();
axi_bus_t cl_pcis_rtl_q();
axi_bus_t cl_pcis_dram_q();

//----------------------------
// End Internal signals
//----------------------------

//reset synchronizers
(* dont_touch = "true" *) logic slr0_sync_aresetn;
(* dont_touch = "true" *) logic slr1_sync_aresetn;
(* dont_touch = "true" *) logic slr2_sync_aresetn;
lib_pipe #(.WIDTH(1), .STAGES(4)) SLR0_PIPE_RST_N (.clk(aclk), .rst_n(1'b1), .in_bus(aresetn), .out_bus(slr0_sync_aresetn));
lib_pipe #(.WIDTH(1), .STAGES(4)) SLR1_PIPE_RST_N (.clk(aclk), .rst_n(1'b1), .in_bus(aresetn), .out_bus(slr1_sync_aresetn));
lib_pipe #(.WIDTH(1), .STAGES(4)) SLR2_PIPE_RST_N (.clk(aclk), .rst_n(1'b1), .in_bus(aresetn), .out_bus(slr2_sync_aresetn));

//----------------------------
// flop the sh_pcis interface input of CL
//----------------------------

   // AXI4 Register Slice for sh_pcis interface
   axi_register_slice PCI_AXL_REG_SLC (
       .aclk          (aclk),
       .aresetn       (slr0_sync_aresetn),
       .s_axi_awid    (sh_pcis_bus.awid),
       .s_axi_awaddr  (sh_pcis_bus.awaddr),
       .s_axi_awlen   (sh_pcis_bus.awlen),
       .s_axi_awvalid (sh_pcis_bus.awvalid),
       .s_axi_awsize  (sh_pcis_bus.awsize),
       .s_axi_awready (sh_pcis_bus.awready),
       .s_axi_wdata   (sh_pcis_bus.wdata),
       .s_axi_wstrb   (sh_pcis_bus.wstrb),
       .s_axi_wlast   (sh_pcis_bus.wlast),
       .s_axi_wvalid  (sh_pcis_bus.wvalid),
       .s_axi_wready  (sh_pcis_bus.wready),
       .s_axi_bid     (sh_pcis_bus.bid),
       .s_axi_bresp   (sh_pcis_bus.bresp),
       .s_axi_bvalid  (sh_pcis_bus.bvalid),
       .s_axi_bready  (sh_pcis_bus.bready),
       .s_axi_arid    (sh_pcis_bus.arid),
       .s_axi_araddr  (sh_pcis_bus.araddr),
       .s_axi_arlen   (sh_pcis_bus.arlen),
       .s_axi_arvalid (sh_pcis_bus.arvalid),
       .s_axi_arsize  (sh_pcis_bus.arsize),
       .s_axi_arready (sh_pcis_bus.arready),
       .s_axi_rid     (sh_pcis_bus.rid),
       .s_axi_rdata   (sh_pcis_bus.rdata),
       .s_axi_rresp   (sh_pcis_bus.rresp),
       .s_axi_rlast   (sh_pcis_bus.rlast),
       .s_axi_rvalid  (sh_pcis_bus.rvalid),
       .s_axi_rready  (sh_pcis_bus.rready),

       .m_axi_awid    (sh_pcis_bus_q.awid),
       .m_axi_awaddr  (sh_pcis_bus_q.awaddr),
       .m_axi_awlen   (sh_pcis_bus_q.awlen),
       .m_axi_awvalid (sh_pcis_bus_q.awvalid),
       .m_axi_awsize  (sh_pcis_bus_q.awsize),
       .m_axi_awready (sh_pcis_bus_q.awready),
       .m_axi_wdata   (sh_pcis_bus_q.wdata),
       .m_axi_wstrb   (sh_pcis_bus_q.wstrb),
       .m_axi_wvalid  (sh_pcis_bus_q.wvalid),
       .m_axi_wlast   (sh_pcis_bus_q.wlast),
       .m_axi_wready  (sh_pcis_bus_q.wready),
       .m_axi_bresp   (sh_pcis_bus_q.bresp),
       .m_axi_bvalid  (sh_pcis_bus_q.bvalid),
       .m_axi_bid     (sh_pcis_bus_q.bid),
       .m_axi_bready  (sh_pcis_bus_q.bready),
       .m_axi_arid    (sh_pcis_bus_q.arid),
       .m_axi_araddr  (sh_pcis_bus_q.araddr),
       .m_axi_arlen   (sh_pcis_bus_q.arlen),
       .m_axi_arsize  (sh_pcis_bus_q.arsize),
       .m_axi_arvalid (sh_pcis_bus_q.arvalid),
       .m_axi_arready (sh_pcis_bus_q.arready),
       .m_axi_rid     (sh_pcis_bus_q.rid),
       .m_axi_rdata   (sh_pcis_bus_q.rdata),
       .m_axi_rresp   (sh_pcis_bus_q.rresp),
       .m_axi_rlast   (sh_pcis_bus_q.rlast),
       .m_axi_rvalid  (sh_pcis_bus_q.rvalid),
       .m_axi_rready  (sh_pcis_bus_q.rready)
   );

//-----------------------------------------------------
//TIE-OFF unused signals to prevent critical warnings
//-----------------------------------------------------
   assign sh_pcis_bus_q.rid[15:6] = 10'b0 ;
   assign sh_pcis_bus_q.bid[15:6] = 10'b0 ;

(* dont_touch = "true" *) axi_interconnect_pcis AXI_CROSSBAR
       (.ACLK(aclk),
        .ARESETN(slr1_sync_aresetn),

        .M_AXI_RTL_araddr(cl_pcis_rtl_q.araddr),
        .M_AXI_RTL_arburst(),
        .M_AXI_RTL_arcache(),
        .M_AXI_RTL_arid(cl_pcis_rtl_q.arid[6:0]),
        .M_AXI_RTL_arlen(cl_pcis_rtl_q.arlen),
        .M_AXI_RTL_arlock(),
        .M_AXI_RTL_arprot(),
        .M_AXI_RTL_arqos(),
        .M_AXI_RTL_arready(cl_pcis_rtl_q.arready),
        .M_AXI_RTL_arregion(),
        .M_AXI_RTL_arsize(cl_pcis_rtl_q.arsize),
        .M_AXI_RTL_arvalid(cl_pcis_rtl_q.arvalid),
        .M_AXI_RTL_awaddr(cl_pcis_rtl_q.awaddr),
        .M_AXI_RTL_awburst(),
        .M_AXI_RTL_awcache(),
        .M_AXI_RTL_awid(cl_pcis_rtl_q.awid[6:0]),
        .M_AXI_RTL_awlen(cl_pcis_rtl_q.awlen),
        .M_AXI_RTL_awlock(),
        .M_AXI_RTL_awprot(),
        .M_AXI_RTL_awqos(),
        .M_AXI_RTL_awready(cl_pcis_rtl_q.awready),
        .M_AXI_RTL_awregion(),
        .M_AXI_RTL_awsize(cl_pcis_rtl_q.awsize),
        .M_AXI_RTL_awvalid(cl_pcis_rtl_q.awvalid),
        .M_AXI_RTL_bid(cl_pcis_rtl_q.bid[6:0]),
        .M_AXI_RTL_bready(cl_pcis_rtl_q.bready),
        .M_AXI_RTL_bresp(cl_pcis_rtl_q.bresp),
        .M_AXI_RTL_bvalid(cl_pcis_rtl_q.bvalid),
        .M_AXI_RTL_rdata(cl_pcis_rtl_q.rdata),
        .M_AXI_RTL_rid(cl_pcis_rtl_q.rid[6:0]),
        .M_AXI_RTL_rlast(cl_pcis_rtl_q.rlast),
        .M_AXI_RTL_rready(cl_pcis_rtl_q.rready),
        .M_AXI_RTL_rresp(cl_pcis_rtl_q.rresp),
        .M_AXI_RTL_rvalid(cl_pcis_rtl_q.rvalid),
        .M_AXI_RTL_wdata(cl_pcis_rtl_q.wdata),
        .M_AXI_RTL_wlast(cl_pcis_rtl_q.wlast),
        .M_AXI_RTL_wready(cl_pcis_rtl_q.wready),
        .M_AXI_RTL_wstrb(cl_pcis_rtl_q.wstrb),
        .M_AXI_RTL_wvalid(cl_pcis_rtl_q.wvalid),

        .M_AXI_DRAM_araddr(cl_pcis_dram_q.araddr),
        .M_AXI_DRAM_arburst(),
        .M_AXI_DRAM_arcache(),
        .M_AXI_DRAM_arid(cl_pcis_dram_q.arid[6:0]),
        .M_AXI_DRAM_arlen(cl_pcis_dram_q.arlen),
        .M_AXI_DRAM_arlock(),
        .M_AXI_DRAM_arprot(),
        .M_AXI_DRAM_arqos(),
        .M_AXI_DRAM_arready(cl_pcis_dram_q.arready),
        .M_AXI_DRAM_arregion(),
        .M_AXI_DRAM_arsize(cl_pcis_dram_q.arsize),
        .M_AXI_DRAM_arvalid(cl_pcis_dram_q.arvalid),
        .M_AXI_DRAM_awaddr(cl_pcis_dram_q.awaddr),
        .M_AXI_DRAM_awburst(),
        .M_AXI_DRAM_awcache(),
        .M_AXI_DRAM_awid(cl_pcis_dram_q.awid[6:0]),
        .M_AXI_DRAM_awlen(cl_pcis_dram_q.awlen),
        .M_AXI_DRAM_awlock(),
        .M_AXI_DRAM_awprot(),
        .M_AXI_DRAM_awqos(),
        .M_AXI_DRAM_awready(cl_pcis_dram_q.awready),
        .M_AXI_DRAM_awregion(),
        .M_AXI_DRAM_awsize(cl_pcis_dram_q.awsize),
        .M_AXI_DRAM_awvalid(cl_pcis_dram_q.awvalid),
        .M_AXI_DRAM_bid(cl_pcis_dram_q.bid[6:0]),
        .M_AXI_DRAM_bready(cl_pcis_dram_q.bready),
        .M_AXI_DRAM_bresp(cl_pcis_dram_q.bresp),
        .M_AXI_DRAM_bvalid(cl_pcis_dram_q.bvalid),
        .M_AXI_DRAM_rdata(cl_pcis_dram_q.rdata),
        .M_AXI_DRAM_rid(cl_pcis_dram_q.rid[6:0]),
        .M_AXI_DRAM_rlast(cl_pcis_dram_q.rlast),
        .M_AXI_DRAM_rready(cl_pcis_dram_q.rready),
        .M_AXI_DRAM_rresp(cl_pcis_dram_q.rresp),
        .M_AXI_DRAM_rvalid(cl_pcis_dram_q.rvalid),
        .M_AXI_DRAM_wdata(cl_pcis_dram_q.wdata),
        .M_AXI_DRAM_wlast(cl_pcis_dram_q.wlast),
        .M_AXI_DRAM_wready(cl_pcis_dram_q.wready),
        .M_AXI_DRAM_wstrb(cl_pcis_dram_q.wstrb),
        .M_AXI_DRAM_wvalid(cl_pcis_dram_q.wvalid),

        .S_AXI_PCIS_araddr({sh_pcis_bus_q.araddr[63:37], 1'b0, sh_pcis_bus_q.araddr[35:0]}),
        .S_AXI_PCIS_arburst(2'b1),
        .S_AXI_PCIS_arcache(4'b11),
        .S_AXI_PCIS_arid(sh_pcis_bus_q.arid[5:0]),
        .S_AXI_PCIS_arlen(sh_pcis_bus_q.arlen),
        .S_AXI_PCIS_arlock(1'b0),
        .S_AXI_PCIS_arprot(3'b10),
        .S_AXI_PCIS_arqos(4'b0),
        .S_AXI_PCIS_arready(sh_pcis_bus_q.arready),
        .S_AXI_PCIS_arregion(4'b0),
        .S_AXI_PCIS_arsize(sh_pcis_bus_q.arsize),
        .S_AXI_PCIS_arvalid(sh_pcis_bus_q.arvalid),
        .S_AXI_PCIS_awaddr({sh_pcis_bus_q.awaddr[63:37], 1'b0, sh_pcis_bus_q.awaddr[35:0]}),
        .S_AXI_PCIS_awburst(2'b1),
        .S_AXI_PCIS_awcache(4'b11),
        .S_AXI_PCIS_awid(sh_pcis_bus_q.awid[5:0]),
        .S_AXI_PCIS_awlen(sh_pcis_bus_q.awlen),
        .S_AXI_PCIS_awlock(1'b0),
        .S_AXI_PCIS_awprot(3'b10),
        .S_AXI_PCIS_awqos(4'b0),
        .S_AXI_PCIS_awready(sh_pcis_bus_q.awready),
        .S_AXI_PCIS_awregion(4'b0),
        .S_AXI_PCIS_awsize(sh_pcis_bus_q.awsize),
        .S_AXI_PCIS_awvalid(sh_pcis_bus_q.awvalid),
        .S_AXI_PCIS_bid(sh_pcis_bus_q.bid[5:0]),
        .S_AXI_PCIS_bready(sh_pcis_bus_q.bready),
        .S_AXI_PCIS_bresp(sh_pcis_bus_q.bresp),
        .S_AXI_PCIS_bvalid(sh_pcis_bus_q.bvalid),
        .S_AXI_PCIS_rdata(sh_pcis_bus_q.rdata),
        .S_AXI_PCIS_rid(sh_pcis_bus_q.rid[5:0]),
        .S_AXI_PCIS_rlast(sh_pcis_bus_q.rlast),
        .S_AXI_PCIS_rready(sh_pcis_bus_q.rready),
        .S_AXI_PCIS_rresp(sh_pcis_bus_q.rresp),
        .S_AXI_PCIS_rvalid(sh_pcis_bus_q.rvalid),
        .S_AXI_PCIS_wdata(sh_pcis_bus_q.wdata),
        .S_AXI_PCIS_wlast(sh_pcis_bus_q.wlast),
        .S_AXI_PCIS_wready(sh_pcis_bus_q.wready),
        .S_AXI_PCIS_wstrb(sh_pcis_bus_q.wstrb),
        .S_AXI_PCIS_wvalid(sh_pcis_bus_q.wvalid)
      );

//----------------------------
// flop the output of interconnect for DDRA
// back to back for SLR crossing
//----------------------------
   //back to back register slices for SLR crossing
   src_register_slice DRAM_AXI4_REG_SLC_1 (
       .aclk           (aclk),
       .aresetn        (slr1_sync_aresetn),
       .s_axi_awid     (cl_pcis_rtl_q.awid),
       .s_axi_awaddr   ({cl_pcis_rtl_q.awaddr[63:36], 2'b0, cl_pcis_rtl_q.awaddr[33:0]}),
       .s_axi_awlen    (cl_pcis_rtl_q.awlen),
       .s_axi_awsize   (cl_pcis_rtl_q.awsize),
       .s_axi_awburst  (2'b1),
       .s_axi_awlock   (1'b0),
       .s_axi_awcache  (4'b11),
       .s_axi_awprot   (3'b10),
       .s_axi_awregion (4'b0),
       .s_axi_awqos    (4'b0),
       .s_axi_awvalid  (cl_pcis_rtl_q.awvalid),
       .s_axi_awready  (cl_pcis_rtl_q.awready),
       .s_axi_wdata    (cl_pcis_rtl_q.wdata),
       .s_axi_wstrb    (cl_pcis_rtl_q.wstrb),
       .s_axi_wlast    (cl_pcis_rtl_q.wlast),
       .s_axi_wvalid   (cl_pcis_rtl_q.wvalid),
       .s_axi_wready   (cl_pcis_rtl_q.wready),
       .s_axi_bid      (cl_pcis_rtl_q.bid),
       .s_axi_bresp    (cl_pcis_rtl_q.bresp),
       .s_axi_bvalid   (cl_pcis_rtl_q.bvalid),
       .s_axi_bready   (cl_pcis_rtl_q.bready),
       .s_axi_arid     (cl_pcis_rtl_q.arid),
       .s_axi_araddr   ({cl_pcis_rtl_q.araddr[63:36], 2'b0, cl_pcis_rtl_q.araddr[33:0]}),
       .s_axi_arlen    (cl_pcis_rtl_q.arlen),
       .s_axi_arsize   (cl_pcis_rtl_q.arsize),
       .s_axi_arburst  (2'b1),
       .s_axi_arlock   (1'b0),
       .s_axi_arcache  (4'b11),
       .s_axi_arprot   (3'b10),
       .s_axi_arregion (4'b0),
       .s_axi_arqos    (4'b0),
       .s_axi_arvalid  (cl_pcis_rtl_q.arvalid),
       .s_axi_arready  (cl_pcis_rtl_q.arready),
       .s_axi_rid      (cl_pcis_rtl_q.rid),
       .s_axi_rdata    (cl_pcis_rtl_q.rdata),
       .s_axi_rresp    (cl_pcis_rtl_q.rresp),
       .s_axi_rlast    (cl_pcis_rtl_q.rlast),
       .s_axi_rvalid   (cl_pcis_rtl_q.rvalid),
       .s_axi_rready   (cl_pcis_rtl_q.rready),
       .m_axi_awid     (cl_pcis_rtl.awid),
       .m_axi_awaddr   (cl_pcis_rtl.awaddr),
       .m_axi_awlen    (cl_pcis_rtl.awlen),
       .m_axi_awsize   (cl_pcis_rtl.awsize),
       .m_axi_awburst  (),
       .m_axi_awlock   (),
       .m_axi_awcache  (),
       .m_axi_awprot   (),
       .m_axi_awregion (),
       .m_axi_awqos    (),
       .m_axi_awvalid  (cl_pcis_rtl.awvalid),
       .m_axi_awready  (cl_pcis_rtl.awready),
       .m_axi_wdata    (cl_pcis_rtl.wdata),
       .m_axi_wstrb    (cl_pcis_rtl.wstrb),
       .m_axi_wlast    (cl_pcis_rtl.wlast),
       .m_axi_wvalid   (cl_pcis_rtl.wvalid),
       .m_axi_wready   (cl_pcis_rtl.wready),
       .m_axi_bid      (cl_pcis_rtl.bid),
       .m_axi_bresp    (cl_pcis_rtl.bresp),
       .m_axi_bvalid   (cl_pcis_rtl.bvalid),
       .m_axi_bready   (cl_pcis_rtl.bready),
       .m_axi_arid     (cl_pcis_rtl.arid),
       .m_axi_araddr   (cl_pcis_rtl.araddr),
       .m_axi_arlen    (cl_pcis_rtl.arlen),
       .m_axi_arsize   (cl_pcis_rtl.arsize),
       .m_axi_arburst  (),
       .m_axi_arlock   (),
       .m_axi_arcache  (),
       .m_axi_arprot   (),
       .m_axi_arregion (),
       .m_axi_arqos    (),
       .m_axi_arvalid  (cl_pcis_rtl.arvalid),
       .m_axi_arready  (cl_pcis_rtl.arready),
       .m_axi_rid      (cl_pcis_rtl.rid),
       .m_axi_rdata    (cl_pcis_rtl.rdata),
       .m_axi_rresp    (cl_pcis_rtl.rresp),
       .m_axi_rlast    (cl_pcis_rtl.rlast),
       .m_axi_rvalid   (cl_pcis_rtl.rvalid),
       .m_axi_rready   (cl_pcis_rtl.rready)
       );

//----------------------------
// flop the output of interconnect for DDRB
// back to back for SLR crossing
//----------------------------

  //back to back register slices for SLR crossing
   src_register_slice RTL_TST_AXI4_REG_SLC_1 (
       .aclk           (aclk),
       .aresetn        (slr1_sync_aresetn),
       .s_axi_awid     (cl_pcis_dram_q.awid),
       .s_axi_awaddr   ({cl_pcis_dram_q.awaddr[63:36], 2'b0, cl_pcis_dram_q.awaddr[33:0]}),
       .s_axi_awlen    (cl_pcis_dram_q.awlen),
       .s_axi_awsize   (cl_pcis_dram_q.awsize),
       .s_axi_awburst  (2'b1),
       .s_axi_awlock   (1'b0),
       .s_axi_awcache  (4'b11),
       .s_axi_awprot   (3'b10),
       .s_axi_awregion (4'b0),
       .s_axi_awqos    (4'b0),
       .s_axi_awvalid  (cl_pcis_dram_q.awvalid),
       .s_axi_awready  (cl_pcis_dram_q.awready),
       .s_axi_wdata    (cl_pcis_dram_q.wdata),
       .s_axi_wstrb    (cl_pcis_dram_q.wstrb),
       .s_axi_wlast    (cl_pcis_dram_q.wlast),
       .s_axi_wvalid   (cl_pcis_dram_q.wvalid),
       .s_axi_wready   (cl_pcis_dram_q.wready),
       .s_axi_bid      (cl_pcis_dram_q.bid),
       .s_axi_bresp    (cl_pcis_dram_q.bresp),
       .s_axi_bvalid   (cl_pcis_dram_q.bvalid),
       .s_axi_bready   (cl_pcis_dram_q.bready),
       .s_axi_arid     (cl_pcis_dram_q.arid),
       .s_axi_araddr   ({cl_pcis_dram_q.araddr[63:36], 2'b0, cl_pcis_dram_q.araddr[33:0]}),
       .s_axi_arlen    (cl_pcis_dram_q.arlen),
       .s_axi_arsize   (cl_pcis_dram_q.arsize),
       .s_axi_arburst  (2'b1),
       .s_axi_arlock   (1'b0),
       .s_axi_arcache  (4'b11),
       .s_axi_arprot   (3'b10),
       .s_axi_arregion (4'b0),
       .s_axi_arqos    (4'b0),
       .s_axi_arvalid  (cl_pcis_dram_q.arvalid),
       .s_axi_arready  (cl_pcis_dram_q.arready),
       .s_axi_rid      (cl_pcis_dram_q.rid),
       .s_axi_rdata    (cl_pcis_dram_q.rdata),
       .s_axi_rresp    (cl_pcis_dram_q.rresp),
       .s_axi_rlast    (cl_pcis_dram_q.rlast),
       .s_axi_rvalid   (cl_pcis_dram_q.rvalid),
       .s_axi_rready   (cl_pcis_dram_q.rready),
       .m_axi_awid     (cl_pcis_dram.awid),
       .m_axi_awaddr   (cl_pcis_dram.awaddr),
       .m_axi_awlen    (cl_pcis_dram.awlen),
       .m_axi_awsize   (cl_pcis_dram.awsize),
       .m_axi_awburst  (),
       .m_axi_awlock   (),
       .m_axi_awcache  (),
       .m_axi_awprot   (),
       .m_axi_awregion (),
       .m_axi_awqos    (),
       .m_axi_awvalid  (cl_pcis_dram.awvalid),
       .m_axi_awready  (cl_pcis_dram.awready),
       .m_axi_wdata    (cl_pcis_dram.wdata),
       .m_axi_wstrb    (cl_pcis_dram.wstrb),
       .m_axi_wlast    (cl_pcis_dram.wlast),
       .m_axi_wvalid   (cl_pcis_dram.wvalid),
       .m_axi_wready   (cl_pcis_dram.wready),
       .m_axi_bid      (cl_pcis_dram.bid),
       .m_axi_bresp    (cl_pcis_dram.bresp),
       .m_axi_bvalid   (cl_pcis_dram.bvalid),
       .m_axi_bready   (cl_pcis_dram.bready),
       .m_axi_arid     (cl_pcis_dram.arid),
       .m_axi_araddr   (cl_pcis_dram.araddr),
       .m_axi_arlen    (cl_pcis_dram.arlen),
       .m_axi_arsize   (cl_pcis_dram.arsize),
       .m_axi_arburst  (),
       .m_axi_arlock   (),
       .m_axi_arcache  (),
       .m_axi_arprot   (),
       .m_axi_arregion (),
       .m_axi_arqos    (),
       .m_axi_arvalid  (cl_pcis_dram.arvalid),
       .m_axi_arready  (cl_pcis_dram.arready),
       .m_axi_rid      (cl_pcis_dram.rid),
       .m_axi_rdata    (cl_pcis_dram.rdata),
       .m_axi_rresp    (cl_pcis_dram.rresp),
       .m_axi_rlast    (cl_pcis_dram.rlast),
       .m_axi_rvalid   (cl_pcis_dram.rvalid),
       .m_axi_rready   (cl_pcis_dram.rready)
       );

endmodule

