package armflex.util

import chisel3._
import chisel3.util._

import antmicro.Bus._
import antmicro.Frontend._

/**
 * A DMA with two AXI Master ports. The destination port writes what the source ports reads.
 * 
 * @param sourceAddressWidth the width of address line in the source AXI master.
 * @param destAddressWidth the width of address line in the dest AXI master.
 */ 
class AXIDualMasterDMA(
  sourceAddressWidth: Int,
  destAddressWidth: Int
) extends MultiIOModule {
  class request_t extends Bundle {
    val src_addr = UInt(sourceAddressWidth.W)
    val dst_addr = UInt(destAddressWidth.W) 
  }
  val move_request_i = IO(Flipped(Decoupled(new request_t)))
  val done_o = IO(Output(Bool()))
  val SRC_AXI_M = IO(new AXI4(sourceAddressWidth, 512))
  val DST_AXI_M = IO(new AXI4(destAddressWidth, 512))

  val u_src_to_stream = Module(new AXI4Reader(sourceAddressWidth, 512))
  val u_stream_to_dst = Module(new AXI4Writer(destAddressWidth, 512))

  u_src_to_stream.io.dataOut <> u_stream_to_dst.io.dataIn
  u_src_to_stream.io.bus <> SRC_AXI_M
  u_stream_to_dst.io.bus <> DST_AXI_M

  u_src_to_stream.io.xfer.address := move_request_i.bits.src_addr
  u_src_to_stream.io.xfer.length := 64.U
  u_src_to_stream.io.xfer.valid := move_request_i.valid

  u_stream_to_dst.io.xfer.address := move_request_i.bits.dst_addr
  u_stream_to_dst.io.xfer.length := 64.U
  u_stream_to_dst.io.xfer.valid := move_request_i.valid

  // The ready order of two movers is:
  //  1. u_src_to_stream
  //  2. u_stream_to_dst
  val first_ready_r = RegInit(false.B)
  val ready_r = RegInit(true.B)

  when(move_request_i.fire()){
    first_ready_r := false.B
    ready_r := false.B
  }.elsewhen(u_src_to_stream.io.xfer.done){
    first_ready_r := true.B
  }.elsewhen(first_ready_r && u_stream_to_dst.io.xfer.done){
    ready_r := true.B
    first_ready_r := false.B
  }

  move_request_i.ready := ready_r
  done_o := first_ready_r && u_stream_to_dst.io.xfer.done

}
