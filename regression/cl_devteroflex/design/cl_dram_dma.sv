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

module cl_dram_dma #(parameter NUM_DDR=4) 

(
   `include "cl_ports.vh"

);

`include "cl_common_defines.vh"      // CL Defines for all examples
`include "cl_id_defines.vh"          // Defines for ID0 and ID1 (PCI ID's)
`include "cl_dram_dma_defines.vh"

// TIE OFF ALL UNUSED INTERFACES
// Including all the unused interface to tie off
// This list is put in the top of the fie to remind
// developers to remve the specific interfaces
// that the CL will use


//`include "unused_flr_template.inc"
//`include "unused_ddr_a_b_d_template.inc"
//`include "unused_ddr_c_template.inc"
`include "unused_pcim_template.inc"
//`include "unused_dma_pcis_template.inc"
//`include "unused_cl_sda_template.inc"
`include "unused_sh_ocl_template.inc"
//`include "unused_sh_bar1_template.inc"
`include "unused_apppf_irq_template.inc"

// Define the addition pipeline stag
// needed to close timing for the various
// place where ATG (Automatic Test Generator)
// is defined
   
   localparam NUM_CFG_STGS_CL_DDR_ATG = 8;
   localparam NUM_CFG_STGS_SH_DDR_ATG = 4;
   localparam NUM_CFG_STGS_PCIE_ATG = 4;

//---------------------------- 
// Internal signals
//---------------------------- 
axi_bus_t lcl_cl_sh_ddra();
axi_bus_t lcl_cl_sh_ddrb();
axi_bus_t lcl_cl_sh_ddrc();
axi_bus_t lcl_cl_sh_ddrd();
axi_bus_t axi_bus_tied();

axi_bus_t sh_pcis_bus();
axi_bus_t sh_cl_dma_pcis_q();

axi_bus_t cl_rtl_ctrl();
axi_bus_t cl_rtl_dram();
axi_bus_t cl_pcis_rtl();

axi_bus_t cl_sh_pcim_bus();

axi_bus_t sda_cl_bus();
axi_bus_t sh_ocl_bus();
axi_bus_t sh_bar1_bus();

cfg_bus_t pcim_tst_cfg_bus();
cfg_bus_t ddra_tst_cfg_bus();
cfg_bus_t ddrb_tst_cfg_bus();
cfg_bus_t ddrc_tst_cfg_bus();
cfg_bus_t ddrd_tst_cfg_bus();
cfg_bus_t axi_mstr_cfg_bus();
cfg_bus_t int_tst_cfg_bus();

logic clk;
(* dont_touch = "true" *) logic pipe_rst_n;
logic pre_sync_rst_n;
(* dont_touch = "true" *) logic sync_rst_n;
logic sh_cl_flr_assert_q;

logic [3:0] all_ddr_is_ready;
logic [2:0] lcl_sh_cl_ddr_is_ready;

//---------------------------- 
// End Internal signals
//----------------------------

// Unused 'full' signals
assign cl_sh_dma_rd_full  = 1'b0;
assign cl_sh_dma_wr_full  = 1'b0;

// Unused *burst signals
assign cl_sh_ddr_arburst[1:0] = 2'h0;
assign cl_sh_ddr_awburst[1:0] = 2'h0;


assign clk = clk_main_a0;

//reset synchronizer
lib_pipe #(.WIDTH(1), .STAGES(4)) PIPE_RST_N (.clk(clk), .rst_n(1'b1), .in_bus(rst_main_n), .out_bus(pipe_rst_n));
   
always_ff @(negedge pipe_rst_n or posedge clk)
   if (!pipe_rst_n)
   begin
      pre_sync_rst_n <= 0;
      sync_rst_n <= 0;
   end
   else
   begin
      pre_sync_rst_n <= 1;
      sync_rst_n <= pre_sync_rst_n;
   end

//FLR response 
always_ff @(negedge sync_rst_n or posedge clk)
   if (!sync_rst_n)
   begin
      sh_cl_flr_assert_q <= 0;
      cl_sh_flr_done <= 0;
   end
   else
   begin
      sh_cl_flr_assert_q <= sh_cl_flr_assert;
      cl_sh_flr_done <= sh_cl_flr_assert_q && !cl_sh_flr_done;
   end

///////////////////////////////////////////////////////////////////////
///////////////// DMA PCIS SLAVE module ///////////////////////////////
///////////////////////////////////////////////////////////////////////
 
assign sh_pcis_bus.awvalid    = sh_cl_dma_pcis_awvalid;
assign sh_pcis_bus.awaddr     = sh_cl_dma_pcis_awaddr;
assign sh_pcis_bus.awid[5:0]  = sh_cl_dma_pcis_awid;
assign sh_pcis_bus.awlen      = sh_cl_dma_pcis_awlen;
assign sh_pcis_bus.awsize     = sh_cl_dma_pcis_awsize;
assign cl_sh_dma_pcis_awready = sh_pcis_bus.awready;
assign sh_pcis_bus.wvalid     = sh_cl_dma_pcis_wvalid;
assign sh_pcis_bus.wdata      = sh_cl_dma_pcis_wdata;
assign sh_pcis_bus.wstrb      = sh_cl_dma_pcis_wstrb;
assign sh_pcis_bus.wlast      = sh_cl_dma_pcis_wlast;
assign cl_sh_dma_pcis_wready  = sh_pcis_bus.wready;
assign cl_sh_dma_pcis_bvalid  = sh_pcis_bus.bvalid;
assign cl_sh_dma_pcis_bresp   = sh_pcis_bus.bresp;
assign sh_pcis_bus.bready     = sh_cl_dma_pcis_bready;
assign cl_sh_dma_pcis_bid     = sh_pcis_bus.bid[5:0];
assign sh_pcis_bus.arvalid    = sh_cl_dma_pcis_arvalid;
assign sh_pcis_bus.araddr     = sh_cl_dma_pcis_araddr;
assign sh_pcis_bus.arid[5:0]  = sh_cl_dma_pcis_arid;
assign sh_pcis_bus.arlen      = sh_cl_dma_pcis_arlen;
assign sh_pcis_bus.arsize     = sh_cl_dma_pcis_arsize;
assign cl_sh_dma_pcis_arready = sh_pcis_bus.arready;
assign cl_sh_dma_pcis_rvalid  = sh_pcis_bus.rvalid;
assign cl_sh_dma_pcis_rid     = sh_pcis_bus.rid[5:0];
assign cl_sh_dma_pcis_rlast   = sh_pcis_bus.rlast;
assign cl_sh_dma_pcis_rresp   = sh_pcis_bus.rresp;
assign cl_sh_dma_pcis_rdata   = sh_pcis_bus.rdata;
assign sh_pcis_bus.rready     = sh_cl_dma_pcis_rready;

assign cl_sh_ddr_awid         = lcl_cl_sh_ddrc.awid;
assign cl_sh_ddr_awaddr       = lcl_cl_sh_ddrc.awaddr;
assign cl_sh_ddr_awlen        = lcl_cl_sh_ddrc.awlen;
assign cl_sh_ddr_awsize       = lcl_cl_sh_ddrc.awsize;
assign cl_sh_ddr_awvalid      = lcl_cl_sh_ddrc.awvalid;
assign lcl_cl_sh_ddrc.awready = sh_cl_ddr_awready;
assign cl_sh_ddr_wid          = 16'b0;
assign cl_sh_ddr_wdata        = lcl_cl_sh_ddrc.wdata;
assign cl_sh_ddr_wstrb        = lcl_cl_sh_ddrc.wstrb;
assign cl_sh_ddr_wlast        = lcl_cl_sh_ddrc.wlast;
assign cl_sh_ddr_wvalid       = lcl_cl_sh_ddrc.wvalid;
assign lcl_cl_sh_ddrc.wready  = sh_cl_ddr_wready;
assign lcl_cl_sh_ddrc.bid     = sh_cl_ddr_bid;
assign lcl_cl_sh_ddrc.bresp   = sh_cl_ddr_bresp;
assign lcl_cl_sh_ddrc.bvalid  = sh_cl_ddr_bvalid;
assign cl_sh_ddr_bready       = lcl_cl_sh_ddrc.bready;
assign cl_sh_ddr_arid         = lcl_cl_sh_ddrc.arid;
assign cl_sh_ddr_araddr       = lcl_cl_sh_ddrc.araddr;
assign cl_sh_ddr_arlen        = lcl_cl_sh_ddrc.arlen;
assign cl_sh_ddr_arsize       = lcl_cl_sh_ddrc.arsize;
assign cl_sh_ddr_arvalid      = lcl_cl_sh_ddrc.arvalid;
assign lcl_cl_sh_ddrc.arready = sh_cl_ddr_arready;
assign lcl_cl_sh_ddrc.rid     = sh_cl_ddr_rid;
assign lcl_cl_sh_ddrc.rresp   = sh_cl_ddr_rresp;
assign lcl_cl_sh_ddrc.rvalid  = sh_cl_ddr_rvalid;
assign lcl_cl_sh_ddrc.rdata   = sh_cl_ddr_rdata;
assign lcl_cl_sh_ddrc.rlast   = sh_cl_ddr_rlast;
assign cl_sh_ddr_rready       = lcl_cl_sh_ddrc.rready;

(* dont_touch = "true" *) logic cl_xbar_general_sync_rst_n;
lib_pipe #(.WIDTH(1), .STAGES(4)) CL_XBAR_GENERAL_RST_N (.clk(clk), .rst_n(1'b1), .in_bus(sync_rst_n), .out_bus(cl_xbar_general_sync_rst_n));
cl_xbar_general CL_XBAR_GENERAL (
   .aclk(clk),
   .aresetn(cl_xbar_general_sync_rst_n),

   .sh_pcis_bus(sh_pcis_bus),

   .cl_pcis_rtl(cl_pcis_rtl),
   .cl_rtl_dram(cl_rtl_dram),

   .lcl_cl_sh_ddra(lcl_cl_sh_ddra),
   .lcl_cl_sh_ddrb(lcl_cl_sh_ddrb),
   .lcl_cl_sh_ddrc(lcl_cl_sh_ddrc),
   .lcl_cl_sh_ddrd(lcl_cl_sh_ddrd)
);

///////////////////////////////////////////////////////////////////////
///////////////// DMA PCIS SLAVE module ///////////////////////////////
///////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////
/////////////////////////// BAR1 BUS //////////////////////////////////
///////////////////////////////////////////////////////////////////////


assign sh_bar1_bus.awvalid       = sh_bar1_awvalid;
assign sh_bar1_bus.awaddr[31:0]  = sh_bar1_awaddr;
assign bar1_sh_awready           = sh_bar1_bus.awready;
assign sh_bar1_bus.wvalid        = sh_bar1_wvalid;
assign sh_bar1_bus.wdata[31:0]   = sh_bar1_wdata;
assign sh_bar1_bus.wstrb[3:0]    = sh_bar1_wstrb;
assign bar1_sh_wready            = sh_bar1_bus.wready;
assign bar1_sh_bvalid            = sh_bar1_bus.bvalid;
assign bar1_sh_bresp             = sh_bar1_bus.bresp;
assign sh_bar1_bus.bready        = sh_bar1_bready;
assign sh_bar1_bus.arvalid       = sh_bar1_arvalid;
assign sh_bar1_bus.araddr[31:0]  = sh_bar1_araddr;
assign bar1_sh_arready           = sh_bar1_bus.arready;
assign bar1_sh_rvalid            = sh_bar1_bus.rvalid;
assign bar1_sh_rresp             = sh_bar1_bus.rresp;
assign bar1_sh_rdata             = sh_bar1_bus.rdata[31:0];
assign sh_bar1_bus.rready        = sh_bar1_rready;

cl_axi_rename BAR1_RTL_AXIL_BUS ( .s_axi_bus (sh_bar1_bus), .m_axi_bus (cl_rtl_ctrl) );

///////////////////////////////////////////////////////////////////////
/////////////////////////// BAR1 BUS //////////////////////////////////
///////////////////////////////////////////////////////////////////////



///////////////////////////////////////////////////////////////////////
////////////////////////////// RTL ////////////////////////////////////
///////////////////////////////////////////////////////////////////////

(* dont_touch = "true" *) logic devteroflex_sync_rst_n;
lib_pipe #(.WIDTH(1), .STAGES(4)) DEVTEROFLEX_RST_N (.clk(clk), .rst_n(1'b1), .in_bus(sync_rst_n), .out_bus(devteroflex_sync_rst_n));
DevteroFlexTopLevel DEVTEROFLEX_TOP (
   .clock(clk),
   .reset(!sync_rst_n),

   .S_AXI_awid    (cl_pcis_rtl.awid   ), 
   .S_AXI_awaddr  (cl_pcis_rtl.awaddr ), 
   .S_AXI_awlen   (cl_pcis_rtl.awlen  ), 
   .S_AXI_awsize  (cl_pcis_rtl.awsize ), 
   .S_AXI_awburst (2'b1),
   .S_AXI_awlock  (1'b0),
   .S_AXI_awcache (4'b11),
   .S_AXI_awprot  (3'b10),
   .S_AXI_awqos   (4'b0),
   .S_AXI_awvalid (cl_pcis_rtl.awvalid), 
   .S_AXI_awready (cl_pcis_rtl.awready), 
   .S_AXI_wdata   (cl_pcis_rtl.wdata  ), 
   .S_AXI_wstrb   (cl_pcis_rtl.wstrb  ), 
   .S_AXI_wlast   (cl_pcis_rtl.wlast  ), 
   .S_AXI_wvalid  (cl_pcis_rtl.wvalid ), 
   .S_AXI_wready  (cl_pcis_rtl.wready ), 
   .S_AXI_bid     (cl_pcis_rtl.bid    ), 
   .S_AXI_bresp   (cl_pcis_rtl.bresp  ), 
   .S_AXI_bvalid  (cl_pcis_rtl.bvalid ), 
   .S_AXI_bready  (cl_pcis_rtl.bready ), 
   .S_AXI_arid    (cl_pcis_rtl.arid   ), 
   .S_AXI_araddr  (cl_pcis_rtl.araddr ), 
   .S_AXI_arlen   (cl_pcis_rtl.arlen  ), 
   .S_AXI_arsize  (cl_pcis_rtl.arsize ), 
   .S_AXI_arburst (2'b1),
   .S_AXI_arlock  (1'b0),
   .S_AXI_arcache (4'b11),
   .S_AXI_arprot  (3'b10),
   .S_AXI_arqos   (4'b0),
   .S_AXI_arvalid (cl_pcis_rtl.arvalid), 
   .S_AXI_arready (cl_pcis_rtl.arready), 
   .S_AXI_rid     (cl_pcis_rtl.rid    ), 
   .S_AXI_rdata   (cl_pcis_rtl.rdata  ), 
   .S_AXI_rresp   (cl_pcis_rtl.rresp  ), 
   .S_AXI_rlast   (cl_pcis_rtl.rlast  ), 
   .S_AXI_rvalid  (cl_pcis_rtl.rvalid ), 
   .S_AXI_rready  (cl_pcis_rtl.rready ), 

   .S_AXIL_awaddr  (cl_rtl_ctrl.awaddr ),
   .S_AXIL_awprot  (3'b10),
   .S_AXIL_awvalid (cl_rtl_ctrl.awvalid),
   .S_AXIL_awready (cl_rtl_ctrl.awready),
   .S_AXIL_wdata   (cl_rtl_ctrl.wdata  ),
   .S_AXIL_wstrb   (cl_rtl_ctrl.wstrb  ),
   .S_AXIL_wvalid  (cl_rtl_ctrl.wvalid ),
   .S_AXIL_wready  (cl_rtl_ctrl.wready ),
   .S_AXIL_bresp   (cl_rtl_ctrl.bresp  ),
   .S_AXIL_bvalid  (cl_rtl_ctrl.bvalid ),
   .S_AXIL_bready  (cl_rtl_ctrl.bready ),
   .S_AXIL_araddr  (cl_rtl_ctrl.araddr ),
   .S_AXIL_arprot  (3'b10),
   .S_AXIL_arvalid (cl_rtl_ctrl.arvalid),
   .S_AXIL_arready (cl_rtl_ctrl.arready),
   .S_AXIL_rdata   (cl_rtl_ctrl.rdata  ),
   .S_AXIL_rresp   (cl_rtl_ctrl.rresp  ),
   .S_AXIL_rvalid  (cl_rtl_ctrl.rvalid ),
   .S_AXIL_rready  (cl_rtl_ctrl.rready ),

   .M_AXI_awid    (cl_rtl_dram.awid   ),
   .M_AXI_awaddr  (cl_rtl_dram.awaddr ),
   .M_AXI_awlen   (cl_rtl_dram.awlen  ),
   .M_AXI_awsize  (cl_rtl_dram.awsize ),
   .M_AXI_awvalid (cl_rtl_dram.awvalid),
   .M_AXI_awready (cl_rtl_dram.awready),
   .M_AXI_wdata   (cl_rtl_dram.wdata  ),
   .M_AXI_wstrb   (cl_rtl_dram.wstrb  ),
   .M_AXI_wlast   (cl_rtl_dram.wlast  ),
   .M_AXI_wvalid  (cl_rtl_dram.wvalid ),
   .M_AXI_wready  (cl_rtl_dram.wready ),
   .M_AXI_bid     (cl_rtl_dram.bid    ),
   .M_AXI_bresp   (cl_rtl_dram.bresp  ),
   .M_AXI_bvalid  (cl_rtl_dram.bvalid ),
   .M_AXI_bready  (cl_rtl_dram.bready ),
   .M_AXI_arid    (cl_rtl_dram.arid   ),
   .M_AXI_araddr  (cl_rtl_dram.araddr ),
   .M_AXI_arlen   (cl_rtl_dram.arlen  ),
   .M_AXI_arsize  (cl_rtl_dram.arsize ),
   .M_AXI_arvalid (cl_rtl_dram.arvalid),
   .M_AXI_arready (cl_rtl_dram.arready),
   .M_AXI_rid     (cl_rtl_dram.rid    ),
   .M_AXI_rdata   (cl_rtl_dram.rdata  ),
   .M_AXI_rresp   (cl_rtl_dram.rresp  ),
   .M_AXI_rlast   (cl_rtl_dram.rlast  ),
   .M_AXI_rvalid  (cl_rtl_dram.rvalid ),
   .M_AXI_rready  (cl_rtl_dram.rready )
);



///////////////////////////////////////////////////////////////////////
////////////////////////////// RTL ////////////////////////////////////
///////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////
///////////////// PCIM MSTR module ////////////////////////////////////
///////////////////////////////////////////////////////////////////////

/*
assign cl_sh_pcim_awid        = cl_sh_pcim_bus.awid;
assign cl_sh_pcim_awaddr      = cl_sh_pcim_bus.awaddr;
assign cl_sh_pcim_awlen       = cl_sh_pcim_bus.awlen;
assign cl_sh_pcim_awvalid     = cl_sh_pcim_bus.awvalid;
assign cl_sh_pcim_awsize      = cl_sh_pcim_bus.awsize;
assign cl_sh_pcim_bus.awready = sh_cl_pcim_awready;
assign cl_sh_pcim_wdata       = cl_sh_pcim_bus.wdata;
assign cl_sh_pcim_wstrb       = cl_sh_pcim_bus.wstrb;
assign cl_sh_pcim_wlast       = cl_sh_pcim_bus.wlast;
assign cl_sh_pcim_wvalid      = cl_sh_pcim_bus.wvalid;
assign cl_sh_pcim_bus.wready  = sh_cl_pcim_wready;
assign cl_sh_pcim_bus.bid     = sh_cl_pcim_bid;
assign cl_sh_pcim_bus.bresp   = sh_cl_pcim_bresp;
assign cl_sh_pcim_bus.bvalid  = sh_cl_pcim_bvalid;
assign cl_sh_pcim_bready      = cl_sh_pcim_bus.bready;
assign cl_sh_pcim_arid        = cl_sh_pcim_bus.arid;
assign cl_sh_pcim_araddr      = cl_sh_pcim_bus.araddr;
assign cl_sh_pcim_arlen       = cl_sh_pcim_bus.arlen;
assign cl_sh_pcim_arvalid     = cl_sh_pcim_bus.arvalid;
assign cl_sh_pcim_bus.arready = sh_cl_pcim_arready;
assign cl_sh_pcim_arsize      = cl_sh_pcim_bus.arsize;
assign cl_sh_pcim_bus.rid     = sh_cl_pcim_rid;
assign cl_sh_pcim_bus.rresp   = sh_cl_pcim_rresp;
assign cl_sh_pcim_bus.rvalid  = sh_cl_pcim_rvalid;
assign cl_sh_pcim_bus.rdata   = sh_cl_pcim_rdata;
assign cl_sh_pcim_bus.rlast   = sh_cl_pcim_rlast;
assign cl_sh_pcim_rready      = cl_sh_pcim_bus.rready;

// note: cl_sh_pcim_aruser/awuser are ignored by the shell
// and the axi4 size is set fixed for 64-bytes
//  cl_sh_pcim_arsize/awsize = 3'b6;

(* dont_touch = "true" *) logic pcim_mstr_sync_rst_n;
lib_pipe #(.WIDTH(1), .STAGES(4)) PCIM_MSTR_SLC_RST_N (.clk(clk), .rst_n(1'b1), .in_bus(sync_rst_n), .out_bus(pcim_mstr_sync_rst_n));
cl_pcim_mstr CL_PCIM_MSTR (

   .aclk(clk),
   .aresetn(pcim_mstr_sync_rst_n),

   .cfg_bus(pcim_tst_cfg_bus),

   .cl_sh_pcim_bus     (cl_sh_pcim_bus)
);
// */

///////////////////////////////////////////////////////////////////////
///////////////// OCL SLAVE module ////////////////////////////////////
///////////////////////////////////////////////////////////////////////

/*
assign sh_ocl_bus.awvalid      = sh_ocl_awvalid;
assign sh_ocl_bus.awaddr[31:0] = sh_ocl_awaddr;
assign ocl_sh_awready          = sh_ocl_bus.awready;
assign sh_ocl_bus.wvalid       = sh_ocl_wvalid;
assign sh_ocl_bus.wdata[31:0]  = sh_ocl_wdata;
assign sh_ocl_bus.wstrb[3:0]   = sh_ocl_wstrb;
assign ocl_sh_wready           = sh_ocl_bus.wready;
assign ocl_sh_bvalid           = sh_ocl_bus.bvalid;
assign ocl_sh_bresp            = sh_ocl_bus.bresp;
assign sh_ocl_bus.bready       = sh_ocl_bready;
assign sh_ocl_bus.arvalid      = sh_ocl_arvalid;
assign sh_ocl_bus.araddr[31:0] = sh_ocl_araddr;
assign ocl_sh_arready          = sh_ocl_bus.arready;
assign ocl_sh_rvalid           = sh_ocl_bus.rvalid;
assign ocl_sh_rresp            = sh_ocl_bus.rresp;
assign ocl_sh_rdata            = sh_ocl_bus.rdata[31:0];
assign sh_ocl_bus.rready       = sh_ocl_rready;

(* dont_touch = "true" *) logic ocl_slv_sync_rst_n;
lib_pipe #(.WIDTH(1), .STAGES(4)) OCL_SLV_SLC_RST_N (.clk(clk), .rst_n(1'b1), .in_bus(sync_rst_n), .out_bus(ocl_slv_sync_rst_n));
cl_ocl_slv CL_OCL_SLV (

   .clk(clk),
   .sync_rst_n(ocl_slv_sync_rst_n),

   .sh_cl_flr_assert_q(sh_cl_flr_assert_q),

   .sh_ocl_bus  (sh_ocl_bus),

   .pcim_tst_cfg_bus(pcim_tst_cfg_bus),
   .ddra_tst_cfg_bus(ddra_tst_cfg_bus),
   .ddrb_tst_cfg_bus(ddrb_tst_cfg_bus),
   .ddrc_tst_cfg_bus(ddrc_tst_cfg_bus),
   .ddrd_tst_cfg_bus(ddrd_tst_cfg_bus),
   .axi_mstr_cfg_bus(axi_mstr_cfg_bus),
   .int_tst_cfg_bus(int_tst_cfg_bus)
);

// */

///////////////////////////////////////////////////////////////////////
///////////////// OCL SLAVE module ////////////////////////////////////
///////////////////////////////////////////////////////////////////////

//----------------------------------------- 
// DDR controller instantiation   
//-----------------------------------------
logic [7:0] sh_ddr_stat_addr_q[2:0];
logic[2:0] sh_ddr_stat_wr_q;
logic[2:0] sh_ddr_stat_rd_q; 
logic[31:0] sh_ddr_stat_wdata_q[2:0];
logic[2:0] ddr_sh_stat_ack_q;
logic[31:0] ddr_sh_stat_rdata_q[2:0];
logic[7:0] ddr_sh_stat_int_q[2:0];


lib_pipe #(.WIDTH(1+1+8+32), .STAGES(NUM_CFG_STGS_CL_DDR_ATG)) PIPE_DDR_STAT0 (.clk(clk), .rst_n(sync_rst_n),
                                               .in_bus({sh_ddr_stat_wr0, sh_ddr_stat_rd0, sh_ddr_stat_addr0, sh_ddr_stat_wdata0}),
                                               .out_bus({sh_ddr_stat_wr_q[0], sh_ddr_stat_rd_q[0], sh_ddr_stat_addr_q[0], sh_ddr_stat_wdata_q[0]})
                                               );


lib_pipe #(.WIDTH(1+8+32), .STAGES(NUM_CFG_STGS_CL_DDR_ATG)) PIPE_DDR_STAT_ACK0 (.clk(clk), .rst_n(sync_rst_n),
                                               .in_bus({ddr_sh_stat_ack_q[0], ddr_sh_stat_int_q[0], ddr_sh_stat_rdata_q[0]}),
                                               .out_bus({ddr_sh_stat_ack0, ddr_sh_stat_int0, ddr_sh_stat_rdata0})
                                               );


lib_pipe #(.WIDTH(1+1+8+32), .STAGES(NUM_CFG_STGS_CL_DDR_ATG)) PIPE_DDR_STAT1 (.clk(clk), .rst_n(sync_rst_n),
                                               .in_bus({sh_ddr_stat_wr1, sh_ddr_stat_rd1, sh_ddr_stat_addr1, sh_ddr_stat_wdata1}),
                                               .out_bus({sh_ddr_stat_wr_q[1], sh_ddr_stat_rd_q[1], sh_ddr_stat_addr_q[1], sh_ddr_stat_wdata_q[1]})
                                               );


lib_pipe #(.WIDTH(1+8+32), .STAGES(NUM_CFG_STGS_CL_DDR_ATG)) PIPE_DDR_STAT_ACK1 (.clk(clk), .rst_n(sync_rst_n),
                                               .in_bus({ddr_sh_stat_ack_q[1], ddr_sh_stat_int_q[1], ddr_sh_stat_rdata_q[1]}),
                                               .out_bus({ddr_sh_stat_ack1, ddr_sh_stat_int1, ddr_sh_stat_rdata1})
                                               );

lib_pipe #(.WIDTH(1+1+8+32), .STAGES(NUM_CFG_STGS_CL_DDR_ATG)) PIPE_DDR_STAT2 (.clk(clk), .rst_n(sync_rst_n),
                                               .in_bus({sh_ddr_stat_wr2, sh_ddr_stat_rd2, sh_ddr_stat_addr2, sh_ddr_stat_wdata2}),
                                               .out_bus({sh_ddr_stat_wr_q[2], sh_ddr_stat_rd_q[2], sh_ddr_stat_addr_q[2], sh_ddr_stat_wdata_q[2]})
                                               );


lib_pipe #(.WIDTH(1+8+32), .STAGES(NUM_CFG_STGS_CL_DDR_ATG)) PIPE_DDR_STAT_ACK2 (.clk(clk), .rst_n(sync_rst_n),
                                               .in_bus({ddr_sh_stat_ack_q[2], ddr_sh_stat_int_q[2], ddr_sh_stat_rdata_q[2]}),
                                               .out_bus({ddr_sh_stat_ack2, ddr_sh_stat_int2, ddr_sh_stat_rdata2})
                                               ); 

//convert to 2D 
logic[15:0] cl_sh_ddr_awid_2d[2:0];
logic[63:0] cl_sh_ddr_awaddr_2d[2:0];
logic[7:0]  cl_sh_ddr_awlen_2d[2:0];
logic[2:0]  cl_sh_ddr_awsize_2d[2:0];
logic[1:0]  cl_sh_ddr_awburst_2d[2:0];
logic       cl_sh_ddr_awvalid_2d [2:0];
logic[2:0]  sh_cl_ddr_awready_2d;

logic[15:0]  cl_sh_ddr_wid_2d[2:0];
logic[511:0] cl_sh_ddr_wdata_2d[2:0];
logic[63:0]  cl_sh_ddr_wstrb_2d[2:0];
logic[2:0]   cl_sh_ddr_wlast_2d;
logic[2:0]   cl_sh_ddr_wvalid_2d;
logic[2:0]   sh_cl_ddr_wready_2d;

logic[15:0] sh_cl_ddr_bid_2d[2:0];
logic[1:0]  sh_cl_ddr_bresp_2d[2:0];
logic[2:0]  sh_cl_ddr_bvalid_2d;
logic[2:0]  cl_sh_ddr_bready_2d;

logic[15:0] cl_sh_ddr_arid_2d[2:0];
logic[63:0] cl_sh_ddr_araddr_2d[2:0];
logic[7:0]  cl_sh_ddr_arlen_2d[2:0];
logic[2:0]  cl_sh_ddr_arsize_2d[2:0];
logic[1:0]  cl_sh_ddr_arburst_2d[2:0];
logic[2:0]  cl_sh_ddr_arvalid_2d;
logic[2:0]  sh_cl_ddr_arready_2d;

logic[15:0]  sh_cl_ddr_rid_2d[2:0];
logic[511:0] sh_cl_ddr_rdata_2d[2:0];
logic[1:0]   sh_cl_ddr_rresp_2d[2:0];
logic[2:0]   sh_cl_ddr_rlast_2d;
logic[2:0]   sh_cl_ddr_rvalid_2d;
logic[2:0]   cl_sh_ddr_rready_2d;

assign cl_sh_ddr_awid_2d    = '{lcl_cl_sh_ddrd.awid,    lcl_cl_sh_ddrb.awid,    lcl_cl_sh_ddra.awid};
assign cl_sh_ddr_awaddr_2d  = '{lcl_cl_sh_ddrd.awaddr,  lcl_cl_sh_ddrb.awaddr,  lcl_cl_sh_ddra.awaddr};
assign cl_sh_ddr_awlen_2d   = '{lcl_cl_sh_ddrd.awlen,   lcl_cl_sh_ddrb.awlen,   lcl_cl_sh_ddra.awlen};
assign cl_sh_ddr_awsize_2d  = '{lcl_cl_sh_ddrd.awsize,  lcl_cl_sh_ddrb.awsize,  lcl_cl_sh_ddra.awsize};
assign cl_sh_ddr_awvalid_2d = '{lcl_cl_sh_ddrd.awvalid, lcl_cl_sh_ddrb.awvalid, lcl_cl_sh_ddra.awvalid};
assign cl_sh_ddr_awburst_2d = {2'b01, 2'b01, 2'b01};
assign {lcl_cl_sh_ddrd.awready, lcl_cl_sh_ddrb.awready, lcl_cl_sh_ddra.awready} = sh_cl_ddr_awready_2d;

assign cl_sh_ddr_wid_2d    = '{lcl_cl_sh_ddrd.wid,    lcl_cl_sh_ddrb.wid,    lcl_cl_sh_ddra.wid};
assign cl_sh_ddr_wdata_2d  = '{lcl_cl_sh_ddrd.wdata,  lcl_cl_sh_ddrb.wdata,  lcl_cl_sh_ddra.wdata};
assign cl_sh_ddr_wstrb_2d  = '{lcl_cl_sh_ddrd.wstrb,  lcl_cl_sh_ddrb.wstrb,  lcl_cl_sh_ddra.wstrb};
assign cl_sh_ddr_wlast_2d  =  {lcl_cl_sh_ddrd.wlast,  lcl_cl_sh_ddrb.wlast,  lcl_cl_sh_ddra.wlast};
assign cl_sh_ddr_wvalid_2d =  {lcl_cl_sh_ddrd.wvalid, lcl_cl_sh_ddrb.wvalid, lcl_cl_sh_ddra.wvalid};
assign {lcl_cl_sh_ddrd.wready, lcl_cl_sh_ddrb.wready, lcl_cl_sh_ddra.wready} = sh_cl_ddr_wready_2d;

assign {lcl_cl_sh_ddrd.bid,    lcl_cl_sh_ddrb.bid,    lcl_cl_sh_ddra.bid}    = {sh_cl_ddr_bid_2d[2],   sh_cl_ddr_bid_2d[1],   sh_cl_ddr_bid_2d[0]};
assign {lcl_cl_sh_ddrd.bresp,  lcl_cl_sh_ddrb.bresp,  lcl_cl_sh_ddra.bresp}  = {sh_cl_ddr_bresp_2d[2], sh_cl_ddr_bresp_2d[1], sh_cl_ddr_bresp_2d[0]};
assign {lcl_cl_sh_ddrd.bvalid, lcl_cl_sh_ddrb.bvalid, lcl_cl_sh_ddra.bvalid} =  sh_cl_ddr_bvalid_2d;
assign cl_sh_ddr_bready_2d =  {lcl_cl_sh_ddrd.bready, lcl_cl_sh_ddrb.bready, lcl_cl_sh_ddra.bready};

assign cl_sh_ddr_arid_2d   = '{lcl_cl_sh_ddrd.arid,    lcl_cl_sh_ddrb.arid,    lcl_cl_sh_ddra.arid};
assign cl_sh_ddr_araddr_2d = '{lcl_cl_sh_ddrd.araddr,  lcl_cl_sh_ddrb.araddr,  lcl_cl_sh_ddra.araddr};
assign cl_sh_ddr_arlen_2d  = '{lcl_cl_sh_ddrd.arlen,   lcl_cl_sh_ddrb.arlen,   lcl_cl_sh_ddra.arlen};
assign cl_sh_ddr_arsize_2d = '{lcl_cl_sh_ddrd.arsize,  lcl_cl_sh_ddrb.arsize,  lcl_cl_sh_ddra.arsize};
assign cl_sh_ddr_arvalid_2d = {lcl_cl_sh_ddrd.arvalid, lcl_cl_sh_ddrb.arvalid, lcl_cl_sh_ddra.arvalid};
assign cl_sh_ddr_arburst_2d = {2'b01, 2'b01, 2'b01};
assign {lcl_cl_sh_ddrd.arready, lcl_cl_sh_ddrb.arready, lcl_cl_sh_ddra.arready} = sh_cl_ddr_arready_2d;

assign {lcl_cl_sh_ddrd.rid,    lcl_cl_sh_ddrb.rid,    lcl_cl_sh_ddra.rid}   = {sh_cl_ddr_rid_2d[2],   sh_cl_ddr_rid_2d[1],   sh_cl_ddr_rid_2d[0]};
assign {lcl_cl_sh_ddrd.rresp,  lcl_cl_sh_ddrb.rresp,  lcl_cl_sh_ddra.rresp} = {sh_cl_ddr_rresp_2d[2], sh_cl_ddr_rresp_2d[1], sh_cl_ddr_rresp_2d[0]};
assign {lcl_cl_sh_ddrd.rdata,  lcl_cl_sh_ddrb.rdata,  lcl_cl_sh_ddra.rdata} = {sh_cl_ddr_rdata_2d[2], sh_cl_ddr_rdata_2d[1], sh_cl_ddr_rdata_2d[0]};
assign {lcl_cl_sh_ddrd.rlast,  lcl_cl_sh_ddrb.rlast,  lcl_cl_sh_ddra.rlast} =  sh_cl_ddr_rlast_2d;
assign {lcl_cl_sh_ddrd.rvalid, lcl_cl_sh_ddrb.rvalid, lcl_cl_sh_ddra.rvalid} = sh_cl_ddr_rvalid_2d;
assign cl_sh_ddr_rready_2d = {lcl_cl_sh_ddrd.rready, lcl_cl_sh_ddrb.rready, lcl_cl_sh_ddra.rready};

(* dont_touch = "true" *) logic sh_ddr_sync_rst_n;
lib_pipe #(.WIDTH(1), .STAGES(4)) SH_DDR_SLC_RST_N (.clk(clk), .rst_n(1'b1), .in_bus(sync_rst_n), .out_bus(sh_ddr_sync_rst_n));
sh_ddr #(
         .DDR_A_PRESENT(`DDR_A_PRESENT),
         .DDR_B_PRESENT(`DDR_B_PRESENT),
         .DDR_D_PRESENT(`DDR_D_PRESENT)
   ) SH_DDR
   (
   .clk(clk),
   .rst_n(sh_ddr_sync_rst_n),

   .stat_clk(clk),
   .stat_rst_n(sh_ddr_sync_rst_n),


   .CLK_300M_DIMM0_DP(CLK_300M_DIMM0_DP),
   .CLK_300M_DIMM0_DN(CLK_300M_DIMM0_DN),
   .M_A_ACT_N     (M_A_ACT_N),
   .M_A_MA        (M_A_MA),
   .M_A_BA        (M_A_BA),
   .M_A_BG        (M_A_BG),
   .M_A_CKE       (M_A_CKE),
   .M_A_ODT       (M_A_ODT),
   .M_A_CS_N      (M_A_CS_N),
   .M_A_CLK_DN    (M_A_CLK_DN),
   .M_A_CLK_DP    (M_A_CLK_DP),
   .M_A_PAR       (M_A_PAR),
   .M_A_DQ        (M_A_DQ),
   .M_A_ECC       (M_A_ECC),
   .M_A_DQS_DP    (M_A_DQS_DP),
   .M_A_DQS_DN    (M_A_DQS_DN),
   .cl_RST_DIMM_A_N(cl_RST_DIMM_A_N),
   
   
   .CLK_300M_DIMM1_DP(CLK_300M_DIMM1_DP),
   .CLK_300M_DIMM1_DN(CLK_300M_DIMM1_DN),
   .M_B_ACT_N     (M_B_ACT_N),
   .M_B_MA        (M_B_MA),
   .M_B_BA        (M_B_BA),
   .M_B_BG        (M_B_BG),
   .M_B_CKE       (M_B_CKE),
   .M_B_ODT       (M_B_ODT),
   .M_B_CS_N      (M_B_CS_N),
   .M_B_CLK_DN    (M_B_CLK_DN),
   .M_B_CLK_DP    (M_B_CLK_DP),
   .M_B_PAR       (M_B_PAR),
   .M_B_DQ        (M_B_DQ),
   .M_B_ECC       (M_B_ECC),
   .M_B_DQS_DP    (M_B_DQS_DP),
   .M_B_DQS_DN    (M_B_DQS_DN),
   .cl_RST_DIMM_B_N(cl_RST_DIMM_B_N),

   .CLK_300M_DIMM3_DP(CLK_300M_DIMM3_DP),
   .CLK_300M_DIMM3_DN(CLK_300M_DIMM3_DN),
   .M_D_ACT_N     (M_D_ACT_N),
   .M_D_MA        (M_D_MA),
   .M_D_BA        (M_D_BA),
   .M_D_BG        (M_D_BG),
   .M_D_CKE       (M_D_CKE),
   .M_D_ODT       (M_D_ODT),
   .M_D_CS_N      (M_D_CS_N),
   .M_D_CLK_DN    (M_D_CLK_DN),
   .M_D_CLK_DP    (M_D_CLK_DP),
   .M_D_PAR       (M_D_PAR),
   .M_D_DQ        (M_D_DQ),
   .M_D_ECC       (M_D_ECC),
   .M_D_DQS_DP    (M_D_DQS_DP),
   .M_D_DQS_DN    (M_D_DQS_DN),
   .cl_RST_DIMM_D_N(cl_RST_DIMM_D_N),

   //------------------------------------------------------
   // DDR-4 Interface from CL (AXI-4)
   //------------------------------------------------------
   .cl_sh_ddr_awid      (cl_sh_ddr_awid_2d),
   .cl_sh_ddr_awaddr    (cl_sh_ddr_awaddr_2d),
   .cl_sh_ddr_awlen     (cl_sh_ddr_awlen_2d),
   .cl_sh_ddr_awsize    (cl_sh_ddr_awsize_2d),
   .cl_sh_ddr_awvalid   (cl_sh_ddr_awvalid_2d),
   .cl_sh_ddr_awburst   (cl_sh_ddr_awburst_2d),
   .sh_cl_ddr_awready   (sh_cl_ddr_awready_2d),

   .cl_sh_ddr_wid       (cl_sh_ddr_wid_2d),
   .cl_sh_ddr_wdata     (cl_sh_ddr_wdata_2d),
   .cl_sh_ddr_wstrb     (cl_sh_ddr_wstrb_2d),
   .cl_sh_ddr_wlast     (cl_sh_ddr_wlast_2d),
   .cl_sh_ddr_wvalid    (cl_sh_ddr_wvalid_2d),
   .sh_cl_ddr_wready    (sh_cl_ddr_wready_2d),

   .sh_cl_ddr_bid       (sh_cl_ddr_bid_2d),
   .sh_cl_ddr_bresp     (sh_cl_ddr_bresp_2d),
   .sh_cl_ddr_bvalid    (sh_cl_ddr_bvalid_2d),
   .cl_sh_ddr_bready    (cl_sh_ddr_bready_2d),

   .cl_sh_ddr_arid      (cl_sh_ddr_arid_2d),
   .cl_sh_ddr_araddr    (cl_sh_ddr_araddr_2d),
   .cl_sh_ddr_arlen     (cl_sh_ddr_arlen_2d),
   .cl_sh_ddr_arsize    (cl_sh_ddr_arsize_2d),
   .cl_sh_ddr_arvalid   (cl_sh_ddr_arvalid_2d),
   .cl_sh_ddr_arburst   (cl_sh_ddr_arburst_2d),
   .sh_cl_ddr_arready   (sh_cl_ddr_arready_2d),

   .sh_cl_ddr_rid       (sh_cl_ddr_rid_2d),
   .sh_cl_ddr_rdata     (sh_cl_ddr_rdata_2d),
   .sh_cl_ddr_rresp     (sh_cl_ddr_rresp_2d),
   .sh_cl_ddr_rlast     (sh_cl_ddr_rlast_2d),
   .sh_cl_ddr_rvalid    (sh_cl_ddr_rvalid_2d),
   .cl_sh_ddr_rready    (cl_sh_ddr_rready_2d),

   .sh_cl_ddr_is_ready  (lcl_sh_cl_ddr_is_ready),

   .sh_ddr_stat_addr0  (sh_ddr_stat_addr_q[0]),
   .sh_ddr_stat_wr0    (sh_ddr_stat_wr_q[0]), 
   .sh_ddr_stat_rd0    (sh_ddr_stat_rd_q[0]), 
   .sh_ddr_stat_wdata0 (sh_ddr_stat_wdata_q[0]), 
   .ddr_sh_stat_ack0   (ddr_sh_stat_ack_q[0]),
   .ddr_sh_stat_rdata0 (ddr_sh_stat_rdata_q[0]),
   .ddr_sh_stat_int0   (ddr_sh_stat_int_q[0]),

   .sh_ddr_stat_addr1  (sh_ddr_stat_addr_q[1]),
   .sh_ddr_stat_wr1    (sh_ddr_stat_wr_q[1]), 
   .sh_ddr_stat_rd1    (sh_ddr_stat_rd_q[1]), 
   .sh_ddr_stat_wdata1 (sh_ddr_stat_wdata_q[1]), 
   .ddr_sh_stat_ack1   (ddr_sh_stat_ack_q[1]),
   .ddr_sh_stat_rdata1 (ddr_sh_stat_rdata_q[1]),
   .ddr_sh_stat_int1   (ddr_sh_stat_int_q[1]),

   .sh_ddr_stat_addr2  (sh_ddr_stat_addr_q[2]),
   .sh_ddr_stat_wr2    (sh_ddr_stat_wr_q[2]), 
   .sh_ddr_stat_rd2    (sh_ddr_stat_rd_q[2]), 
   .sh_ddr_stat_wdata2 (sh_ddr_stat_wdata_q[2]), 
   .ddr_sh_stat_ack2   (ddr_sh_stat_ack_q[2]),
   .ddr_sh_stat_rdata2 (ddr_sh_stat_rdata_q[2]),
   .ddr_sh_stat_int2   (ddr_sh_stat_int_q[2]) 
   );

//----------------------------------------- 
// DDR controller instantiation   
//-----------------------------------------


//----------------------------------------- 
// Interrrupt example  
//-----------------------------------------

/*
(* dont_touch = "true" *) logic int_slv_sync_rst_n;
lib_pipe #(.WIDTH(1), .STAGES(4)) INT_SLV_SLC_RST_N (.clk(clk), .rst_n(1'b1), .in_bus(sync_rst_n), .out_bus(int_slv_sync_rst_n));
cl_int_slv CL_INT_TST 
(
  .clk                 (clk),
  .rst_n               (int_slv_sync_rst_n),

  .cfg_bus             (int_tst_cfg_bus),

  .cl_sh_apppf_irq_req (cl_sh_apppf_irq_req),
  .sh_cl_apppf_irq_ack (sh_cl_apppf_irq_ack)
       
);
// */

//----------------------------------------- 
// Interrrupt example  
//-----------------------------------------

//----------------------------------------- 
// SDA SLAVE module 
//-----------------------------------------

assign sda_cl_bus.awvalid       = sda_cl_awvalid;
assign sda_cl_bus.awaddr[31:0]  = sda_cl_awaddr;
assign cl_sda_awready           = sda_cl_bus.awready;
assign sda_cl_bus.wvalid        = sda_cl_wvalid;
assign sda_cl_bus.wdata[31:0]   = sda_cl_wdata;
assign sda_cl_bus.wstrb[3:0]    = sda_cl_wstrb;
assign cl_sda_wready            = sda_cl_bus.wready;
assign cl_sda_bvalid            = sda_cl_bus.bvalid;
assign cl_sda_bresp             = sda_cl_bus.bresp;
assign sda_cl_bus.bready        = sda_cl_bready;
assign sda_cl_bus.arvalid       = sda_cl_arvalid;
assign sda_cl_bus.araddr[31:0]  = sda_cl_araddr;
assign cl_sda_arready           = sda_cl_bus.arready;
assign cl_sda_rvalid            = sda_cl_bus.rvalid;
assign cl_sda_rresp             = sda_cl_bus.rresp;
assign cl_sda_rdata             = sda_cl_bus.rdata[31:0];
assign sda_cl_bus.rready        = sda_cl_rready;

(* dont_touch = "true" *) logic sda_slv_sync_rst_n;
lib_pipe #(.WIDTH(1), .STAGES(4)) SDA_SLV_SLC_RST_N (.clk(clk), .rst_n(1'b1), .in_bus(sync_rst_n), .out_bus(sda_slv_sync_rst_n));
cl_sda_slv CL_SDA_SLV (

  .aclk(clk),
  .aresetn(sda_slv_sync_rst_n),
  
  .sda_cl_bus(sda_cl_bus)
);

//----------------------------------------- 
// SDA SLAVE module 
//-----------------------------------------


//----------------------------------------- 
// Virtual JTAG ILA Debug core example 
//-----------------------------------------


`ifndef DISABLE_VJTAG_DEBUG

   cl_ila #(.DDR_A_PRESENT(`DDR_A_PRESENT) ) CL_ILA   (

   .aclk(clk),
   .drck(drck),
   .shift(shift),
   .tdi(tdi),
   .update(update),
   .sel(sel),
   .tdo(tdo),
   .tms(tms),
   .tck(tck),
   .runtest(runtest),
   .reset(reset),
   .capture(capture),
   .bscanid_en(bscanid_en),
   .sh_cl_dma_pcis_q(sh_cl_dma_pcis_q),
`ifndef DDR_A_ABSENT   
   .lcl_cl_sh_ddra(lcl_cl_sh_ddra)
`else
   .lcl_cl_sh_ddra(axi_bus_tied)
`endif
);

cl_vio CL_VIO (

   .clk_extra_a1(clk_extra_a1)

);

`endif //  `ifndef DISABLE_VJTAG_DEBUG

//----------------------------------------- 
// Virtual JATG ILA Debug core example 
//-----------------------------------------
// tie off for ILA port when probing block not present
assign axi_bus_tied.awvalid  = 1'b0 ;
assign axi_bus_tied.awaddr   = 64'b0 ;
assign axi_bus_tied.awready  = 1'b0 ;
assign axi_bus_tied.wvalid   = 1'b0 ;
assign axi_bus_tied.wstrb    = 64'b0 ;
assign axi_bus_tied.wlast    = 1'b0 ;
assign axi_bus_tied.wready   = 1'b0 ;
assign axi_bus_tied.wdata    = 512'b0 ;
assign axi_bus_tied.arready  = 1'b0 ;
assign axi_bus_tied.rdata    = 512'b0 ;
assign axi_bus_tied.araddr   = 64'b0 ;
assign axi_bus_tied.arvalid  = 1'b0 ;
assign axi_bus_tied.awid     = 16'b0 ;
assign axi_bus_tied.arid     = 16'b0 ;
assign axi_bus_tied.awlen    = 8'b0 ;
assign axi_bus_tied.rlast    = 1'b0 ;
assign axi_bus_tied.rresp    = 2'b0 ;
assign axi_bus_tied.rid      = 16'b0 ;
assign axi_bus_tied.rvalid   = 1'b0 ;
assign axi_bus_tied.arlen    = 8'b0 ;
assign axi_bus_tied.bresp    = 2'b0 ;
assign axi_bus_tied.rready   = 1'b0 ;
assign axi_bus_tied.bvalid   = 1'b0 ;
assign axi_bus_tied.bid      = 16'b0 ;
assign axi_bus_tied.bready   = 1'b0 ;

// Temporal workaround until these signals removed from the shell
assign cl_sh_pcim_awuser = 18'h0;
assign cl_sh_pcim_aruser = 18'h0;


endmodule   
