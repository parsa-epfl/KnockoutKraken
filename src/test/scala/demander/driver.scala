package armflex.demander

import chisel3._
import chisel3.util._

import chiseltest._
import chiseltest.experimental._
import armflex.cache.MemorySystemParameter

import DMAController.Bus._
import armflex.demander.software_bundle.ParameterConstants
import armflex.demander.peripheral.PageTableSetPacket

object PageDemanderTestHelper {


/**
 * This helper will convert a pageset packet to three 512bits raw data.
 * 
 */
class PageSetConverter extends MultiIOModule {
  import peripheral.PageTableSetPacket
  val pageset_packet_i = IO(Input(new PageTableSetPacket()))
  val raw_o = IO(Output(Vec(3, UInt(512.W))))
  raw_o := VecInit(pageset_packet_i.asUInt().asBools().grouped(512).map(VecInit(_).asUInt()).toSeq)
  val raw_i = IO(Input(Vec(3, UInt(512.W))))
  val pageset_packet_o = IO(Output(new PageTableSetPacket()))
  pageset_packet_o := raw_i.asUInt().asTypeOf(new PageTableSetPacket())
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
class PageDemanderDUT(
  param: MemorySystemParameter
) extends MultiIOModule {
  val u_page_demander = Module(new PageDemander(param, 2, false))
  // AXI Bus for thread table
  val S_AXI_TT = IO(u_page_demander.S_AXI_TT.cloneType)
  S_AXI_TT <> u_page_demander.S_AXI_TT
  // AXI Bus for Page table set
  // val M_AXI_PTSet = IO(u_page_demander.M_AXI_PTSet.cloneType)
  // M_AXI_PTSet <> u_page_demander.M_AXI_PTSet

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

  // AXI Bus for Free PA List
  val M_AXI_PAPOOL = IO(u_page_demander.M_AXI_PAPOOL.cloneType)
  M_AXI_PAPOOL <> u_page_demander.M_AXI_PAPOOL

  // AXI Bus for Page Walker
  val M_AXI_PW = IO(u_page_demander.M_AXI_PW.cloneType)
  M_AXI_PW <> u_page_demander.M_AXI_PW

  // AXI Bus for Page Fault resolution
  val M_AXI_QEMU_MISS = IO(u_page_demander.M_AXI_QEMU_MISS.cloneType)
  M_AXI_QEMU_MISS <> u_page_demander.M_AXI_QEMU_MISS

  // AXI Bus for Page Eviction
  val M_AXI_QEMU_PAGE_EVICT = IO(u_page_demander.M_AXI_QEMU_PAGE_EVICT.cloneType)
  M_AXI_QEMU_PAGE_EVICT <> u_page_demander.M_AXI_QEMU_PAGE_EVICT

  // AXI Bus for TLB Writeback
  val M_AXI_TLBWB = IO(u_page_demander.M_AXI_TLBWB.cloneType)
  M_AXI_TLBWB <> u_page_demander.M_AXI_TLBWB

  // AXI Bus for fetching / pushing page to DRAM
  val M_AXI_PAGE = IO(u_page_demander.M_AXI_PAGE.cloneType)
  M_AXI_PAGE <> u_page_demander.M_AXI_PAGE
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
  val S_AXI_PAGE = IO(Flipped(u_page_demander.S_AXI_PAGE.cloneType))
  S_AXI_PAGE <> u_page_demander.S_AXI_PAGE
  // AXI Master for pushing message to QEMU
  val M_AXI_QEMUTX = IO(u_page_demander.M_AXI_QEMUTX.cloneType)
  M_AXI_QEMUTX <> u_page_demander.M_AXI_QEMUTX
  // I TLB backend request
  val itlb_backend_request_i = IO(Flipped(u_page_demander.itlb_backend_request_i.cloneType))
  itlb_backend_request_i <> u_page_demander.itlb_backend_request_i
  // D TLB backend request
  val dtlb_backend_request_i = IO(Flipped(u_page_demander.dtlb_backend_request_i.cloneType))
  dtlb_backend_request_i <> u_page_demander.dtlb_backend_request_i
  // AXI Slave for receiving message to QEMU
  val S_AXI_QEMU_RX = IO(Flipped(u_page_demander.S_AXI_QEMU_RX.cloneType))
  S_AXI_QEMU_RX <> u_page_demander.S_AXI_QEMU_RX

  // DRAM Resetter
  u_page_demander.M_AXI_RESET.ar <> AXI4AR.stub(ParameterConstants.dram_addr_width)
  u_page_demander.M_AXI_RESET.aw <> AXI4AW.stub(ParameterConstants.dram_addr_width)
  u_page_demander.M_AXI_RESET.b <> AXI4B.stub()
  u_page_demander.M_AXI_RESET.r <> AXI4R.stub(ParameterConstants.dram_data_width) 
  u_page_demander.M_AXI_RESET.w <> AXI4W.stub(ParameterConstants.dram_data_width)

  // Helper 1: the page set converter
  val u_helper_page_set_converter = Module(new PageDemanderTestHelper.PageSetConverter)
  val pageset_packet_i = IO(Input(u_helper_page_set_converter.pageset_packet_i.cloneType))
  pageset_packet_i <> u_helper_page_set_converter.pageset_packet_i
  val pageset_converter_raw_o = IO(Output(u_helper_page_set_converter.raw_o.cloneType))
  pageset_converter_raw_o <> u_helper_page_set_converter.raw_o

  val pageset_packet_o = IO(Output(new PageTableSetPacket()))
  pageset_packet_o <> u_helper_page_set_converter.pageset_packet_o
  val pageset_converter_raw_i = IO(Input(Vec(3, UInt(512.W))))
  pageset_converter_raw_i <> u_helper_page_set_converter.raw_i

}


object PageDemanderDriver {

implicit class PageDemanderDriver(target: PageDemanderDUT){
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
  
  def registerThreadTable(thread_id: UInt, process_id: UInt) = timescope {
    // Send write request
    target.S_AXI_TT.awaddr.poke(thread_id)
    target.S_AXI_TT.awvalid.poke(true.B)
    target.S_AXI_TT.awready.expect(true.B)
    target.tk()
    target.S_AXI_TT.awvalid.poke(false.B)
    // Send write data
    target.S_AXI_TT.wdata.poke(process_id)
    target.S_AXI_TT.wvalid.poke(true.B)
    // Wait for it ready.
    target.S_AXI_TT.wready.expect(true.B)
    target.tk()
    // Wait for write reply
    target.S_AXI_TT.wvalid.poke(false.B)
    target.S_AXI_TT.bvalid.expect(true.B)
    target.S_AXI_TT.bready.poke(true.B)
    target.tk()
  }

  def sendQEMUMessage(message_type: BigInt, rawMessage: Seq[BigInt]) = timescope {
    target.S_AXI_QEMU_RX.aw.awready.expect(true.B)
    target.S_AXI_QEMU_RX.aw.awvalid.poke(true.B)
    target.S_AXI_QEMU_RX.aw.awburst.poke(1.U)
    target.S_AXI_QEMU_RX.aw.awlen.poke(0.U)
    target.S_AXI_QEMU_RX.aw.awsize.poke(6.U)
    tk()
    target.S_AXI_QEMU_RX.aw.awvalid.poke(false.B)
    val raw_res = (message_type +: rawMessage).reverse.reduce { (last: BigInt, current: BigInt) =>
        (last << 32) | current
    }
    target.S_AXI_QEMU_RX.w.wdata.poke(raw_res.U)
    target.S_AXI_QEMU_RX.w.wlast.poke(true.B)
    target.S_AXI_QEMU_RX.w.wstrb.poke(((BigInt(1) << 64) - 1).U)
    timescope {
      waitForSignalToBe(target.S_AXI_QEMU_RX.w.wready)
      target.S_AXI_QEMU_RX.w.wvalid.poke(true.B)
      tk()
    }
    waitForSignalToBe(target.S_AXI_QEMU_RX.b.bvalid)
    target.S_AXI_QEMU_RX.b.bready.poke(true.B)
    tk()
  }

  def sendPageFaultResponse(vpn: BigInt, pid: Int, permission: Int, synonym: Boolean, s_vpn: BigInt, s_pid: Int) = {
    sendQEMUMessage(
      2, Seq(
        vpn & 0xFFFFFFFF,
        vpn >> 32,
        pid,
        permission, 
        if(synonym) 1 else 0,
        s_vpn & 0xFFFFFFFF,
        s_vpn >> 32,
        s_pid
      )
    )
  }

  def movePageIn(duplicatedLine: BigInt) = timescope {
    target.S_AXI_PAGE.aw.awready.expect(true.B)
    target.S_AXI_PAGE.aw.awvalid.poke(true.B)
    target.S_AXI_PAGE.aw.awaddr.poke(4096.U)
    target.S_AXI_PAGE.aw.awburst.poke(1.U)
    target.S_AXI_PAGE.aw.awlen.poke(63.U)
    target.S_AXI_PAGE.aw.awsize.poke(6.U)
    target.S_AXI_PAGE.aw.awvalid.poke(true.B)
    tk()
    target.S_AXI_PAGE.aw.awvalid.poke(false.B)
    // transfer data
    for (i <- 0 until 64){
      target.S_AXI_PAGE.w.wdata.poke(duplicatedLine.U)
      target.S_AXI_PAGE.w.wstrb.poke(((BigInt(1) << 64) - 1).U)
      target.S_AXI_PAGE.w.wlast.poke((i == 63).B)
      waitForSignalToBe(target.S_AXI_PAGE.w.wready)
      target.S_AXI_PAGE.w.wvalid.poke(true.B)
      tk()
      target.S_AXI_PAGE.w.wvalid.poke(false.B)
    } 
    // wait for reply
    waitForSignalToBe(target.S_AXI_PAGE.b.bvalid)
    target.S_AXI_PAGE.b.bready.poke(true.B)
    tk()
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

