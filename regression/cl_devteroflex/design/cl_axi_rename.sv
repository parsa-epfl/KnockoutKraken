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

module cl_axi_rename (
   axi_bus_t.slave s_axi_bus,
   axi_bus_t.master m_axi_bus
);

assign m_axi_bus.awvalid = s_axi_bus.awvalid;
assign m_axi_bus.awaddr = s_axi_bus.awaddr;
assign m_axi_bus.awid[5:0] = s_axi_bus.awid;
assign m_axi_bus.awlen = s_axi_bus.awlen;
assign m_axi_bus.awsize = s_axi_bus.awsize;
assign s_axi_bus.awready = m_axi_bus.awready;
assign m_axi_bus.wvalid = s_axi_bus.wvalid;
assign m_axi_bus.wdata = s_axi_bus.wdata;
assign m_axi_bus.wstrb = s_axi_bus.wstrb;
assign m_axi_bus.wlast = s_axi_bus.wlast;
assign s_axi_bus.wready = m_axi_bus.wready;
assign s_axi_bus.bvalid = m_axi_bus.bvalid;
assign s_axi_bus.bresp = m_axi_bus.bresp;
assign m_axi_bus.bready = s_axi_bus.bready;
assign s_axi_bus.bid = m_axi_bus.bid[5:0];
assign m_axi_bus.arvalid = s_axi_bus.arvalid;
assign m_axi_bus.araddr = s_axi_bus.araddr;
assign m_axi_bus.arid[5:0] = s_axi_bus.arid;
assign m_axi_bus.arlen = s_axi_bus.arlen;
assign m_axi_bus.arsize = s_axi_bus.arsize;
assign s_axi_bus.arready = m_axi_bus.arready;
assign s_axi_bus.rvalid = m_axi_bus.rvalid;
assign s_axi_bus.rid = m_axi_bus.rid[5:0];
assign s_axi_bus.rlast = m_axi_bus.rlast;
assign s_axi_bus.rresp = m_axi_bus.rresp;
assign s_axi_bus.rdata = m_axi_bus.rdata;
assign m_axi_bus.rready = s_axi_bus.rready;

endmodule

