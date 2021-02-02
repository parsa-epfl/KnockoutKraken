package armflex.demander

import chisel3._
import chisel3.util._

import chiseltest._
import chiseltest.experimental._
import armflex.cache.MemorySystemParameter

object PageDemanderTestHelper {


/**
 * This helper will convert a pageset packet to three 512bits raw data.
 * 
 */
class PageSetConverter extends MultiIOModule {
  import peripheral.PTSetPacket
  val pageset_packet_i = IO(Input(new PTSetPacket()))
  val raw_o = IO(Output(Vec(3, UInt(512.W))))
  raw_o := VecInit(pageset_packet_i.asUInt().asBools().grouped(512).map(VecInit(_).asUInt()).toSeq)
}


}


/**
 * This is just a simple wrapper of the module PageDemander. 
 * It also contains some utility functions that simplify the verification process.
 * 
 * @param param Parameter of the memory system.
 * @param instructionFile the path to the memory file that fills the instruction memory.
 * 
 * @note find a easy way to automatically generate the ports since it's dirty.
 * 
 */ 
class PageDemanderDUT(
  param: MemorySystemParameter,
  instructionFile: String = ""
) extends MultiIOModule {
  val u_page_demander = Module(new PageDemander(param, instructionFile))
  // AXI Bus for thread table
  val S_AXI_Threadtable = IO(u_page_demander.S_AXI_Threadtable.cloneType)
  S_AXI_Threadtable <> u_page_demander.S_AXI_Threadtable
  // AXI Bus for Page table set
  val M_AXI_PTSet = IO(u_page_demander.M_AXI_PTSet.cloneType)
  M_AXI_PTSet <> u_page_demander.M_AXI_PTSet
  // TLB backend replies
  val tlb_backend_reply_o = IO(u_page_demander.tlb_backend_reply_o.cloneType)
  tlb_backend_reply_o <> u_page_demander.tlb_backend_reply_o
  // TLB Flush requests
  val tlb_flush_request_o = IO(u_page_demander.tlb_flush_request_o.cloneType)
  tlb_flush_request_o <> u_page_demander.tlb_flush_request_o
  // TLB Frontend replies
  val tlb_frontend_reply_i = IO(u_page_demander.tlb_frontend_reply_i.cloneType)
  tlb_frontend_reply_i <> u_page_demander.tlb_frontend_reply_i
  // AXI Bus for Free PA List
  val M_AXI_FL = IO(u_page_demander.M_AXI_FL.cloneType)
  M_AXI_FL <> u_page_demander.M_AXI_FL
  // Free PA List empty signal
  val freelist_empty_vo = IO(Output(u_page_demander.freelist_empty_vo.cloneType))
  freelist_empty_vo <> u_page_demander.freelist_empty_vo
  // Free PA List full signal
  val freelist_full_vo = IO(Output(u_page_demander.freelist_full_vo.cloneType))
  freelist_full_vo <> u_page_demander.freelist_full_vo  
  // AXI Bus for fetching / pushing page to DRAM
  val M_AXI_Page = IO(u_page_demander.M_AXI_Page.cloneType)
  M_AXI_Page <> u_page_demander.M_AXI_Page
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
  val S_AXI_PageBuffer = IO(Flipped(u_page_demander.S_AXI_PageBuffer.cloneType))
  S_AXI_PageBuffer <> u_page_demander.S_AXI_PageBuffer
  // AXI Master for pushing message to QEMU
  val M_AXI_QEMU_Message = IO(u_page_demander.M_AXI_QEMU_Message.cloneType)
  M_AXI_QEMU_Message <> u_page_demander.M_AXI_QEMU_Message
  // I TLB backend request
  val itlb_backend_request_i = IO(Flipped(u_page_demander.itlb_backend_request_i.cloneType))
  itlb_backend_request_i <> u_page_demander.itlb_backend_request_i
  // D TLB backend request
  val dtlb_backend_request_i = IO(Flipped(u_page_demander.dtlb_backend_request_i.cloneType))
  dtlb_backend_request_i <> u_page_demander.dtlb_backend_request_i
  // AXI Slave for receiving message to QEMU
  val S_AXI_QEMU_Message = IO(Flipped(u_page_demander.S_AXI_QEMU_Message.cloneType))
  S_AXI_QEMU_Message <> u_page_demander.S_AXI_QEMU_Message

  // Helper 1: the page set converter
  val u_helper_page_set_converter = Module(new PageDemanderTestHelper.PageSetConverter)
  val pageset_packet_i = IO(Input(u_helper_page_set_converter.pageset_packet_i.cloneType))
  pageset_packet_i <> u_helper_page_set_converter.pageset_packet_i
  val pageset_converter_raw_o = IO(Output(u_helper_page_set_converter.raw_o.cloneType))
  pageset_converter_raw_o <> u_helper_page_set_converter.raw_o
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
    var interval = 0
    while(port.peek.litToBoolean != value){  
      target.tk()
      interval += 1
    }
    return interval
  }
  
  def registerThreadTable(thread_id: UInt, process_id: UInt) = timescope {
    // Send write request
    target.S_AXI_Threadtable.awaddr.poke(thread_id)
    target.S_AXI_Threadtable.awvalid.poke(true.B)
    target.S_AXI_Threadtable.awready.expect(true.B)
    target.tk()
    target.S_AXI_Threadtable.awvalid.poke(false.B)
    // Send write data
    target.S_AXI_Threadtable.wdata.poke(process_id)
    target.S_AXI_Threadtable.wvalid.poke(true.B)
    // Wait for it ready.
    target.S_AXI_Threadtable.wready.expect(true.B)
    target.tk()
    // Wait for write reply
    target.S_AXI_Threadtable.wvalid.poke(false.B)
    target.S_AXI_Threadtable.bvalid.expect(true.B)
    target.S_AXI_Threadtable.bready.poke(true.B)
    target.tk()
  }
}


}

