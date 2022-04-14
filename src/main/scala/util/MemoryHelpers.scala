package armflex.util

import chisel3._
import chisel3.util._

import armflex.util.ExtraUtils._

class MemReqBurst(
    val addrW: Int,
    val dataW: Int,
    val burstS: Int = 8
) extends Bundle {
    val addr = UInt(addrW.W)
    val w_en = UInt((dataW/8).W)
    val burst = UInt(burstS.W)
}

class ReadPort(
    val addrW: Int,
    val dataW: Int,
    val burstS: Int = 8
) extends Bundle {
    val req = Flipped(Decoupled(new MemReqBurst(addrW, dataW, burstS)))
    val data = Decoupled(UInt(dataW.W))
}

class WritePort(
    val addrW: Int,
    val dataW: Int,
    val burstS: Int = 8
) extends Bundle {
    val req = Flipped(Decoupled(new MemReqBurst(addrW, dataW, burstS)))
    val data = Flipped(Decoupled(UInt(dataW.W)))
}

class DRAMPortParams(
  val pAddrW: Int = 12,
  val pDataW: Int = 512
)
class DRAMWrapperWrite(val params: DRAMPortParams) extends MultiIOModule {
  assert(params.pDataW == 512, "Assumes that we match AWS F1 DRAM width.")
  val M_AXI_W = IO(new AXIWriteMasterIF(params.pAddrW, params.pDataW))
  // The write port allows for burst data writes
  val write = IO(new WritePort(params.pAddrW, params.pDataW))

  M_AXI_W.req.bits.address := write.req.bits.addr
  M_AXI_W.req.bits.length := write.req.bits.burst
  M_AXI_W.req.handshake(write.req)
  M_AXI_W.data <> write.data
}

class DRAMWrapperRead(val params: DRAMPortParams) extends MultiIOModule {
  assert(params.pDataW == 512, "Assumes that we match AWS F1 DRAM width.")
  val M_AXI_R = IO(new AXIReadMasterIF(params.pAddrW, params.pDataW))

  // The read port allows for burst data writes
  val read = IO(new ReadPort(params.pAddrW, params.pDataW))
  M_AXI_R.req.bits.address := read.req.bits.addr
  M_AXI_R.req.bits.length := read.req.bits.burst
  M_AXI_R.req.handshake(read.req)
  read.data <> M_AXI_R.data
}