package armflex_mmu

import antmicro.Bus._
import armflex.util._
import armflex_cache.TLBParameter
import armflex_mmu.peripheral.PageTableSetPacket
import chisel3._
import chiseltest._

object PageDemanderTestHelper {


/**
 * This helper will convert a pageset packet to three 512bits raw data.
 * 
 */
class PageSetConverter(param: TLBParameter) extends MultiIOModule {
  import peripheral.PageTableSetPacket
  val pageset_packet_i = IO(Input(new PageTableSetPacket(param)))
  val raw_o = IO(Output(Vec(3, UInt(512.W))))
  raw_o := VecInit(pageset_packet_i.asUInt().asBools().grouped(512).map(VecInit(_).asUInt()).toSeq)
  val raw_i = IO(Input(Vec(3, UInt(512.W))))
  val pageset_packet_o = IO(Output(new PageTableSetPacket(param)))
  pageset_packet_o := raw_i.asUInt().asTypeOf(new PageTableSetPacket(param))
}


}

/**
 * This is just a simple wrapper of the module PageDemander. 
 * It also contains some utility functions that simplify the verification process.
 * 
 * @param param Parameter of the memory system.
 * 
 * @note find a easy way to automatically generate the ports since it's dirty.
 * 
 * TODO: 
 * 
 */ 
class MMUDUT(
  param: MMUParameter
) extends MultiIOModule {
  val u_page_demander = Module(new MMU(param, 2))
  // AXI Bus for thread table
  // val S_AXIL_TT = IO(Flipped(u_page_demander.S_AXIL_TT.cloneType))
  // S_AXIL_TT <> u_page_demander.S_AXIL_TT
  // AXI Bus for Page table set
  // val M_AXI_PTSet = IO(u_page_demander.M_AXI_PTSet.cloneType)
  // M_AXI_PTSet <> u_page_demander.M_AXI_PTSet

  // Load Store Unit: Page deleting acknowledgement
  u_page_demander.lsu_page_delete_request_o.ready := true.B
  u_page_demander.lsu_complete_notify_o.ready := true.B

  // TLB backend replies
  val itlb_backend_reply_o = IO(u_page_demander.itlb_backend_reply_o.cloneType)
  itlb_backend_reply_o <> u_page_demander.itlb_backend_reply_o
  val dtlb_backend_reply_o = IO(u_page_demander.dtlb_backend_reply_o.cloneType)
  dtlb_backend_reply_o <> u_page_demander.dtlb_backend_reply_o

  // TLB Flush requests
  val itlb_flush_request_o = IO(u_page_demander.itlb_flush_request_o.cloneType)
  itlb_flush_request_o <> u_page_demander.itlb_flush_request_o
  val dtlb_flush_request_o = IO(u_page_demander.dtlb_flush_request_o.cloneType)
  dtlb_flush_request_o <> u_page_demander.dtlb_flush_request_o

  // TLB Flush replies
  val itlb_flush_reply_i = IO(Input(u_page_demander.itlb_flush_reply_i.cloneType))
  itlb_flush_reply_i <> u_page_demander.itlb_flush_reply_i
  val dtlb_flush_reply_i = IO(Input(u_page_demander.dtlb_flush_reply_i.cloneType))
  dtlb_flush_reply_i <> u_page_demander.dtlb_flush_reply_i

  // D Cache flush request
  val dcache_flush_request_o = IO(u_page_demander.dcache_flush_request_o.cloneType)
  dcache_flush_request_o <> u_page_demander.dcache_flush_request_o
  // D Cache flush complete
  val dcache_wb_queue_empty_i = IO(Input(u_page_demander.dcache_wb_queue_empty_i.cloneType))
  dcache_wb_queue_empty_i <> u_page_demander.dcache_wb_queue_empty_i
  // I Cache flush request
  val icache_flush_request_o = IO(u_page_demander.icache_flush_request_o.cloneType)
  icache_flush_request_o <> u_page_demander.icache_flush_request_o
  // I Cache flush complete
  val icache_wb_queue_empty_i = IO(Input(u_page_demander.icache_wb_queue_empty_i.cloneType))
  icache_wb_queue_empty_i <> u_page_demander.icache_wb_queue_empty_i
  // AXI slave of the page buffer
  val S_AXI = IO(Flipped(u_page_demander.S_AXI.cloneType))
  S_AXI <> u_page_demander.S_AXI
  // AXI Master for pushing message to QEMU
  // I TLB backend request
  val itlb_backend_request_i = IO(Flipped(u_page_demander.itlb_backend_request_i.cloneType))
  itlb_backend_request_i <> u_page_demander.itlb_backend_request_i
  // D TLB backend request
  val dtlb_backend_request_i = IO(Flipped(u_page_demander.dtlb_backend_request_i.cloneType))
  dtlb_backend_request_i <> u_page_demander.dtlb_backend_request_i
  // AXI Slave for receiving message to QEMU
  // val S_AXIL_QEMU_MQ = IO(Flipped(u_page_demander.S_AXIL_QEMU_MQ.cloneType))
  // S_AXIL_QEMU_MQ <> u_page_demander.S_AXIL_QEMU_MQ

  val M_AXI = IO(new AXI4(
    param.dramAddrWidth,
    param.dramDataWidth
  ))

  val u_axi_read = Module(new AXIReadMultiplexer(
    param.dramAddrWidth,
    param.dramDataWidth,
    4
  ))

  for(i <- 0 until 4) u_axi_read.S_IF(i) <> u_page_demander.M_DMA_R(i)

  M_AXI.ar <> u_axi_read.M_AXI.ar
  M_AXI.r <> u_axi_read.M_AXI.r
  u_axi_read.M_AXI.aw <> AXI4AW.stub(param.dramAddrWidth)
  u_axi_read.M_AXI.w <> AXI4W.stub(param.dramDataWidth)
  u_axi_read.M_AXI.b <> AXI4B.stub()

  val u_axi_write = Module(new AXIWriteMultiplexer(
    param.dramAddrWidth,
    param.dramDataWidth,
    3
  ))

  for(i <- 0 until 3) u_axi_write.S_IF(i) <> u_page_demander.M_DMA_W(i)

  M_AXI.aw <> u_axi_write.M_AXI.aw
  M_AXI.w <> u_axi_write.M_AXI.w
  M_AXI.b <> u_axi_write.M_AXI.b

  u_axi_write.M_AXI.ar <> AXI4AR.stub(param.dramAddrWidth)
  u_axi_write.M_AXI.r <> AXI4R.stub(param.dramDataWidth)

  val u_axil_inter = Module(new AXILInterconnector(
    Seq(0x8000), Seq(0xC000), 32, 32
  ))

  val S_AXIL = IO(Flipped(u_axil_inter.S_AXIL.cloneType))
  S_AXIL <> u_axil_inter.S_AXIL

  // u_axil_inter.M_AXIL(0) <> u_page_demander.S_AXIL_TT
  u_axil_inter.M_AXIL(0) <> u_page_demander.S_AXIL_QEMU_MQ

  // Helper 1: the page set converter
  val u_helper_page_set_converter = Module(new PageDemanderTestHelper.PageSetConverter(param.mem.toTLBParameter))
  val pageset_packet_i = IO(Input(u_helper_page_set_converter.pageset_packet_i.cloneType))
  pageset_packet_i <> u_helper_page_set_converter.pageset_packet_i
  val pageset_converter_raw_o = IO(Output(u_helper_page_set_converter.raw_o.cloneType))
  pageset_converter_raw_o <> u_helper_page_set_converter.raw_o

  val pageset_packet_o = IO(Output(new PageTableSetPacket(param.mem.toTLBParameter)))
  pageset_packet_o <> u_helper_page_set_converter.pageset_packet_o
  val pageset_converter_raw_i = IO(Input(Vec(3, UInt(512.W))))
  pageset_converter_raw_i <> u_helper_page_set_converter.raw_i

}


object PageDemanderDriver {

implicit class PageDemanderDriver(target: MMUDUT){
  def tk(step: Int = 1) = target.clock.step(step)

  /**
   * Wait the handshake port (boolean type) equal to the value.
   * 
   * @param port the source port
   * @param value the target value, usually and default true
   * 
   * @return the number of cycles (interval) it waits for.
   * 
   */ 
  def waitForSignalToBe(port: Bool, value: Boolean = true): Int = {    
    println(s"wait ${port.pathName} to be $value.")
    var interval = 0
    while(port.peek.litToBoolean != value){  
      target.tk()
      interval += 1
    }
    return interval
  }

  def sendQEMUMessage(message_type: BigInt, rawMessage: Seq[BigInt]) = timescope {
    target.S_AXI.aw.awaddr.poke(0.U)
    target.S_AXI.aw.awready.expect(true.B)
    target.S_AXI.aw.awvalid.poke(true.B)
    target.S_AXI.aw.awburst.poke(1.U)
    target.S_AXI.aw.awlen.poke(0.U)
    target.S_AXI.aw.awsize.poke(6.U)
    tk()
    target.S_AXI.aw.awvalid.poke(false.B)
    val raw_res = (message_type +: rawMessage).reverse.reduce { (last: BigInt, current: BigInt) =>
        (last << 32) | (current & BigInt("4294967295"))
    }
    target.S_AXI.w.wdata.poke(raw_res.asUInt())
    target.S_AXI.w.wlast.poke(true.B)
    target.S_AXI.w.wstrb.poke(((BigInt(1) << 64) - 1).U)
    timescope {
      waitForSignalToBe(target.S_AXI.w.wready)
      target.S_AXI.w.wvalid.poke(true.B)
      tk()
    }
    waitForSignalToBe(target.S_AXI.b.bvalid)
    target.S_AXI.b.bready.poke(true.B)
    tk()
  }

  def sendPageFaultResponse(vpn: BigInt, tid: Int, asid: Int, permission: Int, ppn: Int) = {
    sendQEMUMessage(
      2, Seq(
        vpn & 0xFFFFFFFF,
        vpn >> 32,
        asid,
        permission,
        tid,
        ppn
      )
    )
  }

  def expectQEMUMessage(message_type: BigInt, rawMessage: Seq[BigInt]) = timescope {
    var message_available = false
    // message request
    do{
      println("Access S_AXIL_QEMU_MQ by 0x4")
      timescope {
        target.S_AXIL.ar.araddr.poke((0x4 + 0x8000).U)
        target.S_AXIL.ar.arvalid.poke(true.B)
        waitForSignalToBe(target.S_AXIL.ar.arready)
        tk()
      }
      target.S_AXIL.r.rvalid.expect(true.B)
      val result = target.S_AXIL.r.rdata.peek.litValue()
      if(result != 0){
        message_available = true
      }
      timescope {
        target.S_AXIL.r.rready.poke(true.B)
        tk()
      }
    } while(!message_available);
    // read the message
    target.S_AXI.ar.araddr.poke(0.U)
    waitForSignalToBe(target.S_AXI.ar.arready)
    timescope {
      target.S_AXI.ar.arvalid.poke(true.B)
      tk()
    }
    waitForSignalToBe(target.S_AXI.r.rvalid)
    val raw_res = (message_type +: rawMessage).reverse.reduce { (last: BigInt, current: BigInt) =>
        (last << 32) | current
    }
    target.S_AXI.r.rdata.expect(raw_res.U)
    timescope {
      target.S_AXI.r.rready.poke(true.B)
      tk()
    }
  }

  def receivePageTableSet(master_bus: AXI4, expectedAddr: UInt) = {
    waitForSignalToBe(master_bus.aw.awvalid)
    master_bus.aw.awaddr.expect(expectedAddr)
    master_bus.aw.awlen.expect(2.U)
    timescope {
      master_bus.aw.awready.poke(true.B)
      tk()
    }
    for(i <- 0 until 3){
      waitForSignalToBe(master_bus.w.wvalid)
      target.pageset_converter_raw_i(i).poke(
        master_bus.w.wdata.peek()
      )
      timescope {
        master_bus.w.wready.poke(true.B)
        tk()
      }
    }
    timescope {
      waitForSignalToBe(master_bus.b.bready)
      master_bus.b.bvalid.poke(true.B)
      tk()
    }
  } 

  def sendPageTableSet(master_bus: AXI4, expectedAddr: UInt) = {
    waitForSignalToBe(master_bus.ar.arvalid)
    master_bus.ar.araddr.expect(expectedAddr)
    master_bus.ar.arlen.expect(2.U)
    timescope {
      master_bus.ar.arready.poke(true.B)
      tk()
    }
    timescope {
      for(i <- 0 until 3){
        master_bus.r.rdata.poke(target.pageset_converter_raw_o(i).peek)
        master_bus.r.rid.poke(0.U)
        master_bus.r.rlast.poke((i == 2).B)
        master_bus.r.rresp.poke(0.U)
        waitForSignalToBe(master_bus.r.rready)
        master_bus.r.rvalid.poke(true.B)
        tk()
      }
    }
  }
}


}

