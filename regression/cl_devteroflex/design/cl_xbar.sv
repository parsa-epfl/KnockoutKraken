module cl_xbar_general
(
    input aclk,
    input aresetn,

    axi_bus_t.master sh_pcis_bus,
    axi_bus_t.slave cl_pcis_rtl,

    axi_bus_t.master cl_rtl_dram,

    axi_bus_t.slave lcl_cl_sh_ddra,
    axi_bus_t.slave lcl_cl_sh_ddrb,
    axi_bus_t.slave lcl_cl_sh_ddrc,
    axi_bus_t.slave lcl_cl_sh_ddrd
);

//----------------------------
// Internal signals
//----------------------------
axi_bus_t m_00G_16G_axi();
axi_bus_t m_16G_32G_axi();
axi_bus_t m_48G_64G_axi();
axi_bus_t m_32G_48G_axi();

(* dont_touch = "true" *) logic sync_aresetn;
lib_pipe #(.WIDTH(1), .STAGES(4)) PIPE_RST_N (.clk(aclk), .rst_n(1'b1), .in_bus(aresetn), .out_bus(sync_aresetn));

(* dont_touch = "true" *) 
cl_xbar AXI_CROSSBAR (
    .ACLK(aclk),
    .ARESETN(sync_aresetn),
    .M_00G_16G_AXI_araddr           (m_00G_16G_axi.araddr),
    .M_00G_16G_AXI_arburst          (),
    .M_00G_16G_AXI_arcache          (),
    .M_00G_16G_AXI_arid             (m_00G_16G_axi.arid),
    .M_00G_16G_AXI_arlen            (m_00G_16G_axi.arlen),
    .M_00G_16G_AXI_arlock           (),
    .M_00G_16G_AXI_arprot           (),
    .M_00G_16G_AXI_arqos            (),
    .M_00G_16G_AXI_arready          (m_00G_16G_axi.arready),
    .M_00G_16G_AXI_arregion         (),
    .M_00G_16G_AXI_arsize           (m_00G_16G_axi.arsize),
    .M_00G_16G_AXI_arvalid          (m_00G_16G_axi.arvalid),
    .M_00G_16G_AXI_awaddr           (m_00G_16G_axi.awaddr),
    .M_00G_16G_AXI_awburst          (),
    .M_00G_16G_AXI_awcache          (),
    .M_00G_16G_AXI_awid             (m_00G_16G_axi.awid),
    .M_00G_16G_AXI_awlen            (m_00G_16G_axi.awlen),
    .M_00G_16G_AXI_awlock           (),
    .M_00G_16G_AXI_awprot           (),
    .M_00G_16G_AXI_awqos            (),
    .M_00G_16G_AXI_awready          (m_00G_16G_axi.awready),
    .M_00G_16G_AXI_awregion         (),
    .M_00G_16G_AXI_awsize           (m_00G_16G_axi.awsize),
    .M_00G_16G_AXI_awvalid          (m_00G_16G_axi.awvalid),
    .M_00G_16G_AXI_bid              (m_00G_16G_axi.bid),
    .M_00G_16G_AXI_bready           (m_00G_16G_axi.bready),
    .M_00G_16G_AXI_bresp            (m_00G_16G_axi.bresp),
    .M_00G_16G_AXI_bvalid           (m_00G_16G_axi.bvalid),
    .M_00G_16G_AXI_rdata            (m_00G_16G_axi.rdata),
    .M_00G_16G_AXI_rid              (m_00G_16G_axi.rid),
    .M_00G_16G_AXI_rlast            (m_00G_16G_axi.rlast),
    .M_00G_16G_AXI_rready           (m_00G_16G_axi.rready),
    .M_00G_16G_AXI_rresp            (m_00G_16G_axi.rresp),
    .M_00G_16G_AXI_rvalid           (m_00G_16G_axi.rvalid),
    .M_00G_16G_AXI_wdata            (m_00G_16G_axi.wdata),
    .M_00G_16G_AXI_wlast            (m_00G_16G_axi.wlast),
    .M_00G_16G_AXI_wready           (m_00G_16G_axi.wready),
    .M_00G_16G_AXI_wstrb            (m_00G_16G_axi.wstrb),
    .M_00G_16G_AXI_wvalid           (m_00G_16G_axi.wvalid),

    .M_16G_32G_AXI_araddr           (m_16G_32G_axi.araddr),
    .M_16G_32G_AXI_arburst          (),
    .M_16G_32G_AXI_arcache          (),
    .M_16G_32G_AXI_arid             (m_16G_32G_axi.arid),
    .M_16G_32G_AXI_arlen            (m_16G_32G_axi.arlen),
    .M_16G_32G_AXI_arlock           (),
    .M_16G_32G_AXI_arprot           (),
    .M_16G_32G_AXI_arqos            (),
    .M_16G_32G_AXI_arready          (m_16G_32G_axi.arready),
    .M_16G_32G_AXI_arregion         (),
    .M_16G_32G_AXI_arsize           (m_16G_32G_axi.arsize),
    .M_16G_32G_AXI_arvalid          (m_16G_32G_axi.arvalid),
    .M_16G_32G_AXI_awaddr           (m_16G_32G_axi.awaddr),
    .M_16G_32G_AXI_awburst          (),
    .M_16G_32G_AXI_awcache          (),
    .M_16G_32G_AXI_awid             (m_16G_32G_axi.awid),
    .M_16G_32G_AXI_awlen            (m_16G_32G_axi.awlen),
    .M_16G_32G_AXI_awlock           (),
    .M_16G_32G_AXI_awprot           (),
    .M_16G_32G_AXI_awqos            (),
    .M_16G_32G_AXI_awready          (m_16G_32G_axi.awready),
    .M_16G_32G_AXI_awregion         (),
    .M_16G_32G_AXI_awsize           (m_16G_32G_axi.awsize),
    .M_16G_32G_AXI_awvalid          (m_16G_32G_axi.awvalid),
    .M_16G_32G_AXI_bid              (m_16G_32G_axi.bid),
    .M_16G_32G_AXI_bready           (m_16G_32G_axi.bready),
    .M_16G_32G_AXI_bresp            (m_16G_32G_axi.bresp),
    .M_16G_32G_AXI_bvalid           (m_16G_32G_axi.bvalid),
    .M_16G_32G_AXI_rdata            (m_16G_32G_axi.rdata),
    .M_16G_32G_AXI_rid              (m_16G_32G_axi.rid),
    .M_16G_32G_AXI_rlast            (m_16G_32G_axi.rlast),
    .M_16G_32G_AXI_rready           (m_16G_32G_axi.rready),
    .M_16G_32G_AXI_rresp            (m_16G_32G_axi.rresp),
    .M_16G_32G_AXI_rvalid           (m_16G_32G_axi.rvalid),
    .M_16G_32G_AXI_wdata            (m_16G_32G_axi.wdata),
    .M_16G_32G_AXI_wlast            (m_16G_32G_axi.wlast),
    .M_16G_32G_AXI_wready           (m_16G_32G_axi.wready),
    .M_16G_32G_AXI_wstrb            (m_16G_32G_axi.wstrb),
    .M_16G_32G_AXI_wvalid           (m_16G_32G_axi.wvalid),

    .M_32G_48G_AXI_araddr           (m_32G_48G_axi.araddr),
    .M_32G_48G_AXI_arburst          (),
    .M_32G_48G_AXI_arcache          (),
    .M_32G_48G_AXI_arid             (m_32G_48G_axi.arid),
    .M_32G_48G_AXI_arlen            (m_32G_48G_axi.arlen),
    .M_32G_48G_AXI_arlock           (),
    .M_32G_48G_AXI_arprot           (),
    .M_32G_48G_AXI_arqos            (),
    .M_32G_48G_AXI_arready          (m_32G_48G_axi.arready),
    .M_32G_48G_AXI_arregion         (),
    .M_32G_48G_AXI_arsize           (m_32G_48G_axi.arsize),
    .M_32G_48G_AXI_arvalid          (m_32G_48G_axi.arvalid),
    .M_32G_48G_AXI_awaddr           (m_32G_48G_axi.awaddr),
    .M_32G_48G_AXI_awburst          (),
    .M_32G_48G_AXI_awcache          (),
    .M_32G_48G_AXI_awid             (m_32G_48G_axi.awid),
    .M_32G_48G_AXI_awlen            (m_32G_48G_axi.awlen),
    .M_32G_48G_AXI_awlock           (),
    .M_32G_48G_AXI_awprot           (),
    .M_32G_48G_AXI_awqos            (),
    .M_32G_48G_AXI_awready          (m_32G_48G_axi.awready),
    .M_32G_48G_AXI_awregion         (),
    .M_32G_48G_AXI_awsize           (m_32G_48G_axi.awsize),
    .M_32G_48G_AXI_awvalid          (m_32G_48G_axi.awvalid),
    .M_32G_48G_AXI_bid              (m_32G_48G_axi.bid),
    .M_32G_48G_AXI_bready           (m_32G_48G_axi.bready),
    .M_32G_48G_AXI_bresp            (m_32G_48G_axi.bresp),
    .M_32G_48G_AXI_bvalid           (m_32G_48G_axi.bvalid),
    .M_32G_48G_AXI_rdata            (m_32G_48G_axi.rdata),
    .M_32G_48G_AXI_rid              (m_32G_48G_axi.rid),
    .M_32G_48G_AXI_rlast            (m_32G_48G_axi.rlast),
    .M_32G_48G_AXI_rready           (m_32G_48G_axi.rready),
    .M_32G_48G_AXI_rresp            (m_32G_48G_axi.rresp),
    .M_32G_48G_AXI_rvalid           (m_32G_48G_axi.rvalid),
    .M_32G_48G_AXI_wdata            (m_32G_48G_axi.wdata),
    .M_32G_48G_AXI_wlast            (m_32G_48G_axi.wlast),
    .M_32G_48G_AXI_wready           (m_32G_48G_axi.wready),
    .M_32G_48G_AXI_wstrb            (m_32G_48G_axi.wstrb),
    .M_32G_48G_AXI_wvalid           (m_32G_48G_axi.wvalid),

    .M_48G_64G_AXI_araddr           (m_48G_64G_axi.araddr),
    .M_48G_64G_AXI_arburst          (),
    .M_48G_64G_AXI_arcache          (),
    .M_48G_64G_AXI_arid             (m_48G_64G_axi.arid),
    .M_48G_64G_AXI_arlen            (m_48G_64G_axi.arlen),
    .M_48G_64G_AXI_arlock           (),
    .M_48G_64G_AXI_arprot           (),
    .M_48G_64G_AXI_arqos            (),
    .M_48G_64G_AXI_arready          (m_48G_64G_axi.arready),
    .M_48G_64G_AXI_arregion         (),
    .M_48G_64G_AXI_arsize           (m_48G_64G_axi.arsize),
    .M_48G_64G_AXI_arvalid          (m_48G_64G_axi.arvalid),
    .M_48G_64G_AXI_awaddr           (m_48G_64G_axi.awaddr),
    .M_48G_64G_AXI_awburst          (),
    .M_48G_64G_AXI_awcache          (),
    .M_48G_64G_AXI_awid             (m_48G_64G_axi.awid),
    .M_48G_64G_AXI_awlen            (m_48G_64G_axi.awlen),
    .M_48G_64G_AXI_awlock           (),
    .M_48G_64G_AXI_awprot           (),
    .M_48G_64G_AXI_awqos            (),
    .M_48G_64G_AXI_awready          (m_48G_64G_axi.awready),
    .M_48G_64G_AXI_awregion         (),
    .M_48G_64G_AXI_awsize           (m_48G_64G_axi.awsize),
    .M_48G_64G_AXI_awvalid          (m_48G_64G_axi.awvalid),
    .M_48G_64G_AXI_bid              (m_48G_64G_axi.bid),
    .M_48G_64G_AXI_bready           (m_48G_64G_axi.bready),
    .M_48G_64G_AXI_bresp            (m_48G_64G_axi.bresp),
    .M_48G_64G_AXI_bvalid           (m_48G_64G_axi.bvalid),
    .M_48G_64G_AXI_rdata            (m_48G_64G_axi.rdata),
    .M_48G_64G_AXI_rid              (m_48G_64G_axi.rid),
    .M_48G_64G_AXI_rlast            (m_48G_64G_axi.rlast),
    .M_48G_64G_AXI_rready           (m_48G_64G_axi.rready),
    .M_48G_64G_AXI_rresp            (m_48G_64G_axi.rresp),
    .M_48G_64G_AXI_rvalid           (m_48G_64G_axi.rvalid),
    .M_48G_64G_AXI_wdata            (m_48G_64G_axi.wdata),
    .M_48G_64G_AXI_wlast            (m_48G_64G_axi.wlast),
    .M_48G_64G_AXI_wready           (m_48G_64G_axi.wready),
    .M_48G_64G_AXI_wstrb            (m_48G_64G_axi.wstrb),
    .M_48G_64G_AXI_wvalid           (m_48G_64G_axi.wvalid),

    .M_AXI_RTL_araddr               (cl_pcis_rtl.araddr),
    .M_AXI_RTL_arburst              (),
    .M_AXI_RTL_arcache              (),
    .M_AXI_RTL_arid                 (cl_pcis_rtl.arid),
    .M_AXI_RTL_arlen                (cl_pcis_rtl.arlen),
    .M_AXI_RTL_arlock               (),
    .M_AXI_RTL_arprot               (),
    .M_AXI_RTL_arqos                (),
    .M_AXI_RTL_arready              (cl_pcis_rtl.arready),
    .M_AXI_RTL_arregion             (),
    .M_AXI_RTL_arsize               (cl_pcis_rtl.arsize),
    .M_AXI_RTL_arvalid              (cl_pcis_rtl.arvalid),
    .M_AXI_RTL_awaddr               (cl_pcis_rtl.awaddr),
    .M_AXI_RTL_awburst              (),
    .M_AXI_RTL_awcache              (),
    .M_AXI_RTL_awid                 (cl_pcis_rtl.awid),
    .M_AXI_RTL_awlen                (cl_pcis_rtl.awlen),
    .M_AXI_RTL_awlock               (),
    .M_AXI_RTL_awprot               (),
    .M_AXI_RTL_awqos                (),
    .M_AXI_RTL_awready              (cl_pcis_rtl.awready),
    .M_AXI_RTL_awregion             (),
    .M_AXI_RTL_awsize               (cl_pcis_rtl.awsize),
    .M_AXI_RTL_awvalid              (cl_pcis_rtl.awvalid),
    .M_AXI_RTL_bid                  (cl_pcis_rtl.bid),
    .M_AXI_RTL_bready               (cl_pcis_rtl.bready),
    .M_AXI_RTL_bresp                (cl_pcis_rtl.bresp),
    .M_AXI_RTL_bvalid               (cl_pcis_rtl.bvalid),
    .M_AXI_RTL_rdata                (cl_pcis_rtl.rdata),
    .M_AXI_RTL_rid                  (cl_pcis_rtl.rid),
    .M_AXI_RTL_rlast                (cl_pcis_rtl.rlast),
    .M_AXI_RTL_rready               (cl_pcis_rtl.rready),
    .M_AXI_RTL_rresp                (cl_pcis_rtl.rresp),
    .M_AXI_RTL_rvalid               (cl_pcis_rtl.rvalid),
    .M_AXI_RTL_wdata                (cl_pcis_rtl.wdata),
    .M_AXI_RTL_wlast                (cl_pcis_rtl.wlast),
    .M_AXI_RTL_wready               (cl_pcis_rtl.wready),
    .M_AXI_RTL_wstrb                (cl_pcis_rtl.wstrb),
    .M_AXI_RTL_wvalid               (cl_pcis_rtl.wvalid),

    .S_AXI_PCIS_araddr              (sh_pcis_bus.araddr),
    .S_AXI_PCIS_arburst             (2'b1),
    .S_AXI_PCIS_arcache             (4'b11),
    .S_AXI_PCIS_arid                (sh_pcis_bus.arid),
    .S_AXI_PCIS_arlen               (sh_pcis_bus.arlen),
    .S_AXI_PCIS_arlock              (1'b0),
    .S_AXI_PCIS_arprot              (3'b10),
    .S_AXI_PCIS_arqos               (4'b0),
    .S_AXI_PCIS_arready             (sh_pcis_bus.arready),
    .S_AXI_PCIS_arregion            (4'b0),
    .S_AXI_PCIS_arsize              (sh_pcis_bus.arsize),
    .S_AXI_PCIS_arvalid             (sh_pcis_bus.arvalid),
    .S_AXI_PCIS_awaddr              (sh_pcis_bus.awaddr),
    .S_AXI_PCIS_awburst             (2'b1),
    .S_AXI_PCIS_awcache             (4'b11),
    .S_AXI_PCIS_awid                (sh_pcis_bus.awid),
    .S_AXI_PCIS_awlen               (sh_pcis_bus.awlen),
    .S_AXI_PCIS_awlock              (1'b0),
    .S_AXI_PCIS_awprot              (3'b10),
    .S_AXI_PCIS_awqos               (4'b0),
    .S_AXI_PCIS_awready             (sh_pcis_bus.awready),
    .S_AXI_PCIS_awregion            (4'b0),
    .S_AXI_PCIS_awsize              (sh_pcis_bus.awsize),
    .S_AXI_PCIS_awvalid             (sh_pcis_bus.awvalid),
    .S_AXI_PCIS_bid                 (sh_pcis_bus.bid),
    .S_AXI_PCIS_bready              (sh_pcis_bus.bready),
    .S_AXI_PCIS_bresp               (sh_pcis_bus.bresp),
    .S_AXI_PCIS_bvalid              (sh_pcis_bus.bvalid),
    .S_AXI_PCIS_rdata               (sh_pcis_bus.rdata),
    .S_AXI_PCIS_rid                 (sh_pcis_bus.rid),
    .S_AXI_PCIS_rlast               (sh_pcis_bus.rlast),
    .S_AXI_PCIS_rready              (sh_pcis_bus.rready),
    .S_AXI_PCIS_rresp               (sh_pcis_bus.rresp),
    .S_AXI_PCIS_rvalid              (sh_pcis_bus.rvalid),
    .S_AXI_PCIS_wdata               (sh_pcis_bus.wdata),
    .S_AXI_PCIS_wlast               (sh_pcis_bus.wlast),
    .S_AXI_PCIS_wready              (sh_pcis_bus.wready),
    .S_AXI_PCIS_wstrb               (sh_pcis_bus.wstrb),
    .S_AXI_PCIS_wvalid              (sh_pcis_bus.wvalid),

    .S_RTL_DRAM_AXI_araddr          (cl_rtl_dram.araddr),
    .S_RTL_DRAM_AXI_arburst         (2'b1),
    .S_RTL_DRAM_AXI_arcache         (4'b11),
    .S_RTL_DRAM_AXI_arid            (cl_rtl_dram.arid),
    .S_RTL_DRAM_AXI_arlen           (cl_rtl_dram.arlen),
    .S_RTL_DRAM_AXI_arlock          (1'b0),
    .S_RTL_DRAM_AXI_arprot          (3'b10),
    .S_RTL_DRAM_AXI_arqos           (4'b0),
    .S_RTL_DRAM_AXI_arready         (cl_rtl_dram.arready),
    .S_RTL_DRAM_AXI_arregion        (4'b0),
    .S_RTL_DRAM_AXI_arsize          (cl_rtl_dram.arsize),
    .S_RTL_DRAM_AXI_arvalid         (cl_rtl_dram.arvalid),
    .S_RTL_DRAM_AXI_awaddr          (cl_rtl_dram.awaddr),
    .S_RTL_DRAM_AXI_awburst         (2'b1),
    .S_RTL_DRAM_AXI_awcache         (4'b11),
    .S_RTL_DRAM_AXI_awid            (cl_rtl_dram.awid),
    .S_RTL_DRAM_AXI_awlen           (cl_rtl_dram.awlen),
    .S_RTL_DRAM_AXI_awlock          (1'b0),
    .S_RTL_DRAM_AXI_awprot          (3'b10),
    .S_RTL_DRAM_AXI_awqos           (4'b0),
    .S_RTL_DRAM_AXI_awready         (cl_rtl_dram.awready),
    .S_RTL_DRAM_AXI_awregion        (4'b0),
    .S_RTL_DRAM_AXI_awsize          (cl_rtl_dram.awsize),
    .S_RTL_DRAM_AXI_awvalid         (cl_rtl_dram.awvalid),
    .S_RTL_DRAM_AXI_bid             (cl_rtl_dram.bid),
    .S_RTL_DRAM_AXI_bready          (cl_rtl_dram.bready),
    .S_RTL_DRAM_AXI_bresp           (cl_rtl_dram.bresp),
    .S_RTL_DRAM_AXI_bvalid          (cl_rtl_dram.bvalid),
    .S_RTL_DRAM_AXI_rdata           (cl_rtl_dram.rdata),
    .S_RTL_DRAM_AXI_rid             (cl_rtl_dram.rid),
    .S_RTL_DRAM_AXI_rlast           (cl_rtl_dram.rlast),
    .S_RTL_DRAM_AXI_rready          (cl_rtl_dram.rready),
    .S_RTL_DRAM_AXI_rresp           (cl_rtl_dram.rresp),
    .S_RTL_DRAM_AXI_rvalid          (cl_rtl_dram.rvalid),
    .S_RTL_DRAM_AXI_wdata           (cl_rtl_dram.wdata),
    .S_RTL_DRAM_AXI_wlast           (cl_rtl_dram.wlast),
    .S_RTL_DRAM_AXI_wready          (cl_rtl_dram.wready),
    .S_RTL_DRAM_AXI_wstrb           (cl_rtl_dram.wstrb),
    .S_RTL_DRAM_AXI_wvalid          (cl_rtl_dram.wvalid)
);

cl_axi_rename AXI_00G_16G_BUS ( .s_axi_bus (m_00G_16G_axi), .m_axi_bus (lcl_cl_sh_ddrc) );
cl_axi_rename AXI_16G_32G_BUS ( .s_axi_bus (m_16G_32G_axi), .m_axi_bus (lcl_cl_sh_ddrb) );
cl_axi_rename AXI_32G_48G_BUS ( .s_axi_bus (m_32G_48G_axi), .m_axi_bus (lcl_cl_sh_ddra) );
cl_axi_rename AXI_48G_64G_BUS ( .s_axi_bus (m_48G_64G_axi), .m_axi_bus (lcl_cl_sh_ddrd) );


axi_bus_t axi_bus_tied();
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

`ifdef DDR_A_ABSENT
cl_axi_rename AXI_32G_48G_BUS_TIED ( .s_axi_bus (m_32G_48G_axi), .m_axi_bus (axi_bus_tied) );
`endif

`ifdef DDR_B_ABSENT
cl_axi_rename AXI_16G_32G_BUS_TIED ( .s_axi_bus (m_16G_32G_axi), .m_axi_bus (axi_bus_tied) );
`endif

`ifdef DDR_D_ABSENT
cl_axi_rename AXI_48G_64G_BUS_TIED ( .s_axi_bus (m_48G_64G_axi), .m_axi_bus (axi_bus_tied) );
`endif



endmodule