package armflex.util

import chisel3._
import chisel3.util._

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
