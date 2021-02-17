package armflex.demander

import chisel3._
import chisel3.util._

import armflex.cache._

class PageDemanderWithTLBandCache(
  param: MemorySystemParameter
) extends MultiIOModule {
  val u_inst = Module(new InstructionMemorySystem(param))
  // ports of instruction path
  // u_inst.cache_backend_reply_i
  // u_inst.cache_backend_request_o
  // u_inst.cache_flush_request_i
  val icache_packet_arrive_o = IO(u_inst.cache_packet_arrive_o.cloneType)
  u_inst.cache_packet_arrive_o <> icache_packet_arrive_o
  val ifrontend_reply_o = IO(u_inst.frontend_reply_o.cloneType)
  u_inst.frontend_reply_o <> ifrontend_reply_o
  val ifrontend_request_i = IO(Flipped(u_inst.frontend_request_i.cloneType))
  u_inst.frontend_request_i <> ifrontend_request_i
  // u_inst.stall_request_i
  // u_inst.tlb_backend_reply_i
  // u_inst.tlb_backend_request_o
  // u_inst.tlb_flush_request_i
  val itlb_packet_arrive_o = IO(u_inst.tlb_packet_arrive_o.cloneType)
  u_inst.tlb_packet_arrive_o <> itlb_packet_arrive_o
  val itlb_violation_o = IO(u_inst.tlb_violation_o.cloneType)
  u_inst.tlb_violation_o <> itlb_violation_o

  val u_inst_axi = Module(new CacheBackendToAXIInterface.CacheBackendAXIAdaptors(param))
  val M_AXI_IPATH = IO(u_inst_axi.M_AXI.cloneType)
  u_inst_axi.M_AXI <> M_AXI_IPATH
  // u_inst_axi.pending_queue_empty_o
  u_inst_axi.cache_backend_reply_o <> u_inst.cache_backend_reply_i
  u_inst_axi.cache_backend_request_i <> u_inst.cache_backend_request_o // TODO: Add FIFO here to buffer the backend request.

  val u_data = Module(new DataMemorySystem(param))
  // ports of the data path
  // u_data.cache_backend_reply_i
  // u_data.cache_backend_request_o
  // u_data.cache_flush_request_i
  val dcache_packet_arrive_o = IO(u_data.cache_packet_arrive_o.cloneType)
  u_data.cache_packet_arrive_o <> dcache_packet_arrive_o
  val dfrontend_reply_o = IO(u_data.frontend_reply_o.cloneType)
  u_data.frontend_reply_o <> dfrontend_reply_o
  val dfrontend_request_i = IO(Flipped(u_data.frontend_request_i.cloneType))
  u_data.frontend_request_i <> dfrontend_request_i
  // u_data.stall_request_i
  // u_data.tlb_backend_reply_i
  // u_data.tlb_backend_request_o
  // u_data.tlb_flush_request_i
  val dtlb_packet_arrive_o = IO(u_data.tlb_packet_arrive_o.cloneType)
  u_data.tlb_packet_arrive_o <> dtlb_packet_arrive_o
  val dtlb_violation_o = IO(u_data.tlb_violation_o.cloneType)
  u_data.tlb_violation_o <> dtlb_violation_o

  val u_data_axi = Module(new CacheBackendToAXIInterface.CacheBackendAXIAdaptors(param))
  val M_AXI_DPATH = IO(u_data_axi.M_AXI.cloneType)
  u_data_axi.M_AXI <> M_AXI_DPATH
  // u_data_axi.pending_queue_empty_o
  u_data_axi.cache_backend_reply_o <> u_data.cache_backend_reply_i
  u_data_axi.cache_backend_request_i <> u_data.cache_backend_request_o


  val u_pd = Module(new PageDemander(param, 2, false))
  // ports of the page demander
  val M_AXI_PAGE = IO(u_pd.M_AXI_PAGE.cloneType)
  u_pd.M_AXI_PAGE <> M_AXI_PAGE
  val M_AXI_PAPOOL = IO(u_pd.M_AXI_PAPOOL.cloneType)
  u_pd.M_AXI_PAPOOL <> M_AXI_PAPOOL
  val M_AXI_PW = IO(u_pd.M_AXI_PW.cloneType)
  u_pd.M_AXI_PW <> M_AXI_PW
  val S_AXI_QEMU_MQ = IO(u_pd.S_AXI_QEMU_MQ.cloneType)
  u_pd.S_AXI_QEMU_MQ <> S_AXI_QEMU_MQ
  val S_AXIL_QEMU_MQ = IO(u_pd.S_AXIL_QEMU_MQ.cloneType)
  u_pd.S_AXIL_QEMU_MQ <> S_AXIL_QEMU_MQ
  val M_AXI_QEMU_MISS = IO(u_pd.M_AXI_QEMU_MISS.cloneType)
  u_pd.M_AXI_QEMU_MISS <> M_AXI_QEMU_MISS
  val M_AXI_QEMU_PAGE_EVICT = IO(u_pd.M_AXI_QEMU_PAGE_EVICT.cloneType)
  u_pd.M_AXI_QEMU_PAGE_EVICT <> M_AXI_QEMU_PAGE_EVICT
  val M_AXI_RESET = IO(u_pd.M_AXI_RESET.cloneType)
  u_pd.M_AXI_RESET <> M_AXI_RESET
  val M_AXI_TLBWB = IO(u_pd.M_AXI_TLBWB.cloneType)
  u_pd.M_AXI_TLBWB <> M_AXI_TLBWB
  val S_AXI_PAGE = IO(Flipped(u_pd.S_AXI_PAGE.cloneType))
  u_pd.S_AXI_PAGE <> S_AXI_PAGE
  val S_AXI_TT = IO(u_pd.S_AXI_TT.cloneType)
  u_pd.S_AXI_TT <> S_AXI_TT

  // u_pd.dcache_flush_request_o
  u_pd.dcache_flush_request_o <> u_data.cache_flush_request_i
  // u_pd.dcache_stall_request_vo
  u_pd.dcache_stall_request_vo <> u_data.stall_request_i
  // u_pd.dcache_wb_queue_empty_i
  u_pd.dcache_wb_queue_empty_i <> u_data_axi.pending_queue_empty_o

  // u_pd.dtlb_backend_reply_o
  u_pd.dtlb_backend_reply_o <> u_data.tlb_backend_reply_i
  // u_pd.dtlb_backend_request_i
  u_pd.dtlb_backend_request_i <> u_data.tlb_backend_request_o
  // u_pd.dtlb_flush_reply_i
  u_pd.dtlb_flush_reply_i := u_data.tlb_flush_reply_o
  // u_pd.dtlb_flush_request_o
  u_pd.dtlb_flush_request_o <> u_data.tlb_flush_request_i

  // u_pd.icache_flush_request_o
  u_pd.icache_flush_request_o <> u_inst.cache_flush_request_i
  // u_pd.icache_stall_request_vo
  u_pd.icache_stall_request_vo <> u_inst.stall_request_i
  // u_pd.icache_wb_queue_empty_i
  u_pd.icache_wb_queue_empty_i <> u_inst_axi.pending_queue_empty_o

  // u_pd.itlb_backend_reply_o
  u_pd.itlb_backend_reply_o <> u_inst.tlb_backend_reply_i
  // u_pd.itlb_backend_request_i
  u_pd.itlb_backend_request_i <> u_inst.tlb_backend_request_o
  // u_pd.itlb_flush_reply_i
  u_pd.itlb_flush_reply_i <> u_inst.tlb_flush_reply_o
  // u_pd.itlb_flush_request_o
  u_pd.itlb_flush_request_o <> u_inst.tlb_flush_request_i
}

object WithTLBandCacheVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new PageDemanderWithTLBandCache(new MemorySystemParameter)))
}