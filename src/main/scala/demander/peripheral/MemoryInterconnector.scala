package armflex.demander.peripheral

import chisel3._
import chisel3.util._

class MemoryInterconnector(
  val addresses: Seq[Int],
  val masks: Seq[Int],
  val addressWidth: Int = 32,
  val dataWidth: Int = 32
) extends MultiIOModule {
  assert(addresses.length == masks.length)
  val slave_requests_o = IO(Vec(addresses.length, Valid(
    new MemoryRequestPacket(addressWidth, dataWidth)
  )))

  val slave_replies_i = IO(Input(Vec(addresses.length,UInt(dataWidth.W))))

  val master_request_i = IO(Flipped(Valid(new MemoryRequestPacket(addressWidth, dataWidth))))

  val master_reply_o = IO(Valid(UInt(dataWidth.W)))

  for (i <- 0 until masks.length){
    slave_requests_o(i).bits := master_request_i.bits
    slave_requests_o(i).valid := master_request_i.valid && (master_request_i.bits.addr.asSInt() & masks(i).S) === addresses(i).S
  }
  
  val encodings_r = RegNext(OHToUInt(slave_requests_o.map(_.valid)))

  master_reply_o.bits := MuxLookup(encodings_r, 0.U, slave_replies_i.zipWithIndex.map{ case (port, i) =>
    i.U -> port
  })
  master_reply_o.valid := true.B
}

