package armflex

import chisel3._
import chisel3.util._
import armflex.cache.TLBPlusCache
import armflex.cache.MemorySystemParameter
import armflex.cache.MatrixLRUCore
import armflex.cache.PseudoTreeLRUCore
import armflex.cache.CacheBackendToAXIInterface
import armflex.demander.PageDemander

class ARMFlexTop extends MultiIOModule {
  implicit val pipelineCfg = new ProcConfig()
  val memoryParameter = new MemorySystemParameter()

  val u_pipeline = Module(new PipelineAxi)

  val transplantIO = IO(u_pipeline.transplantIO.cloneType)
  transplantIO <> u_pipeline.transplantIO

  val u_inst_path = Module(new TLBPlusCache(
    memoryParameter,
    () => new MatrixLRUCore(memoryParameter.cacheWayNumber),
    () => new PseudoTreeLRUCore(memoryParameter.tlbWayNumber)
  ))

  u_pipeline.mem.inst.req <> u_inst_path.frontend_request_i
  u_pipeline.mem.inst.resp <> u_inst_path.frontend_reply_o

  u_pipeline.mem.wake(0).tag := u_inst_path.tlb_packet_arrive_o.bits
  u_pipeline.mem.wake(0).valid := u_inst_path.tlb_packet_arrive_o.valid

  u_pipeline.mem.wake(1).tag := u_inst_path.cache_packet_arrive_o.bits.thread_id
  u_pipeline.mem.wake(1).valid := u_inst_path.cache_packet_arrive_o.valid

  u_pipeline.mem.instFault.tag := u_inst_path.tlb_violation_o.bits
  u_pipeline.mem.instFault.valid := u_inst_path.tlb_violation_o.valid

  val u_inst_axi = Module(new CacheBackendToAXIInterface.CacheBackendAXIAdaptors(memoryParameter))
  val M_AXI_IPATH = IO(u_inst_axi.M_AXI.cloneType)
  u_inst_axi.M_AXI <> M_AXI_IPATH
  // u_inst_axi.pending_queue_empty_o
  u_inst_axi.cache_backend_reply_o <> u_inst_path.cache_backend_reply_i
  u_inst_axi.cache_backend_request_i <> u_inst_path.cache_backend_request_o // TODO: Add FIFO here to buffer the backend request if timing is not good then.

  val u_data_path = Module(new TLBPlusCache(
    memoryParameter,
    () => new MatrixLRUCore(memoryParameter.cacheWayNumber),
    () => new PseudoTreeLRUCore(memoryParameter.tlbWayNumber)
  ))

  u_pipeline.mem.data.req <> u_data_path.frontend_request_i
  u_pipeline.mem.data.resp <> u_data_path.frontend_reply_o

  u_pipeline.mem.wake(2).tag := u_data_path.tlb_packet_arrive_o.bits
  u_pipeline.mem.wake(2).valid := u_data_path.tlb_packet_arrive_o.valid

  u_pipeline.mem.wake(3).tag := u_data_path.cache_packet_arrive_o.bits.thread_id
  u_pipeline.mem.wake(3).valid := u_data_path.cache_packet_arrive_o.valid

  u_pipeline.mem.dataFault.tag := u_data_path.tlb_violation_o.bits
  u_pipeline.mem.dataFault.valid := u_data_path.tlb_violation_o.valid

  val u_data_axi = Module(new CacheBackendToAXIInterface.CacheBackendAXIAdaptors(memoryParameter))
  val M_AXI_DPATH = IO(u_data_axi.M_AXI.cloneType)
  u_data_axi.M_AXI <> M_AXI_DPATH
  // u_data_axi.pending_queue_empty_o
  u_data_axi.cache_backend_reply_o <> u_data_path.cache_backend_reply_i
  u_data_axi.cache_backend_request_i <> u_data_path.cache_backend_request_o

  val u_pd = Module(new PageDemander(memoryParameter, 2, false))
  // ports of the page demander
  val M_AXI_PAGE = IO(u_pd.M_AXI_PAGE.cloneType)
  u_pd.M_AXI_PAGE <> M_AXI_PAGE
  val M_AXI_PAPOOL = IO(u_pd.M_AXI_PAPOOL.cloneType)
  u_pd.M_AXI_PAPOOL <> M_AXI_PAPOOL
  val M_AXI_PW = IO(u_pd.M_AXI_PW.cloneType)
  u_pd.M_AXI_PW <> M_AXI_PW
  val S_AXI_QEMU_MQ = IO(Flipped(u_pd.S_AXI_QEMU_MQ.cloneType))
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
  u_pd.dcache_flush_request_o <> u_data_path.cache_flush_request_i
  // u_pd.dcache_stall_request_vo
  u_pd.dcache_stall_request_vo <> u_data_path.stall_request_i
  // u_pd.dcache_wb_queue_empty_i
  u_pd.dcache_wb_queue_empty_i <> u_data_axi.pending_queue_empty_o

  // u_pd.dtlb_backend_reply_o
  u_pd.dtlb_backend_reply_o <> u_data_path.tlb_backend_reply_i
  // u_pd.dtlb_backend_request_i
  u_pd.dtlb_backend_request_i <> u_data_path.tlb_backend_request_o
  // u_pd.dtlb_flush_reply_i
  u_pd.dtlb_flush_reply_i := u_data_path.tlb_flush_reply_o
  // u_pd.dtlb_flush_request_o
  u_pd.dtlb_flush_request_o <> u_data_path.tlb_flush_request_i

  // u_pd.icache_flush_request_o
  u_pd.icache_flush_request_o <> u_inst_path.cache_flush_request_i
  // u_pd.icache_stall_request_vo
  u_pd.icache_stall_request_vo <> u_inst_path.stall_request_i
  // u_pd.icache_wb_queue_empty_i
  u_pd.icache_wb_queue_empty_i <> u_inst_axi.pending_queue_empty_o

  // u_pd.itlb_backend_reply_o
  u_pd.itlb_backend_reply_o <> u_inst_path.tlb_backend_reply_i
  // u_pd.itlb_backend_request_i
  u_pd.itlb_backend_request_i <> u_inst_path.tlb_backend_request_o
  // u_pd.itlb_flush_reply_i
  u_pd.itlb_flush_reply_i <> u_inst_path.tlb_flush_reply_o
  // u_pd.itlb_flush_request_o
  u_pd.itlb_flush_request_o <> u_inst_path.tlb_flush_request_i
}

object ARMFlexTopVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  import java.io._
  val fr = new FileWriter(new File("ARMFlexTop.v"))
  fr.write(c.emitVerilog(new ARMFlexTop))
  fr.close()
}
