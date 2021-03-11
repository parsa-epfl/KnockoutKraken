package armflex.util

import chisel3._
import chisel3.util._

import antmicro.Bus._
import antmicro.Frontend._

class AXIDMARequestPacket(
  val addressWidth: Int
) extends Bundle {
  val address = UInt(addressWidth.W)
  val length = UInt(addressWidth.W)
}

/**
 * Master Interface for a module to communicate with a AXI Read DMA.
 * 
 * @param addressWidth the width of the address
 * @param dataWidth the width of the data
 * 
 * 
 * @note use Flipped(this) to create a slave interface.
 * 
 */ 
class AXIReadMasterIF(addrWidth: Int, dataWidth: Int) extends Bundle {
  val req = Decoupled(new AXIDMARequestPacket(addrWidth))
  val data = Flipped(Decoupled(UInt(dataWidth.W)))
  val done = Input(Bool())

  override def cloneType: this.type = new AXIReadMasterIF(addrWidth, dataWidth).asInstanceOf[this.type]
}

/**
 * DMA Worker to perform AXI read, with more than one master support.
 */ 
class AXIReadMultiplexer(
  addressWidth: Int = 64,
  dataWidth: Int = 512,
  wayNumber: Int = 1
) extends MultiIOModule {
  val S_IF = IO(Vec(wayNumber, Flipped(new AXIReadMasterIF(addressWidth, dataWidth))))

  //val req_i = IO(Vec(wayNumber,Flipped(Decoupled(new AXIDMARequestPacket(addressWidth)))))
  val u_arb = Module(new RRArbiter(new AXIDMARequestPacket(addressWidth), wayNumber))
  for(i <- 0 until wayNumber) u_arb.io.in(i) <> S_IF(i).req
  val selected_req = u_arb.io.out
  val index_r = RegEnable(u_arb.io.chosen, 0.U, selected_req.fire())
  val index_vr = RegInit(false.B)

  val u_axi_reader = Module(new AXI4Reader(addressWidth, dataWidth))

  u_axi_reader.io.xfer.address := selected_req.bits.address
  u_axi_reader.io.xfer.length := selected_req.bits.length

  u_axi_reader.io.xfer.valid := selected_req.valid
  selected_req.ready := u_axi_reader.io.xfer.ready

  for(i <- 0 until wayNumber){
    S_IF(i).data.bits := u_axi_reader.io.dataOut.bits
    S_IF(i).data.valid := u_axi_reader.io.dataOut.valid && index_r === i.U
    S_IF(i).done := u_axi_reader.io.xfer.done && index_r === i.U
  }
  
  u_axi_reader.io.dataOut.ready := S_IF(index_r).data.ready

  val M_AXI = IO(u_axi_reader.io.bus.cloneType)
  M_AXI <> u_axi_reader.io.bus

  when(u_axi_reader.io.xfer.done){
    index_vr := false.B
  }.elsewhen(selected_req.fire()){
    index_vr := true.B
  }

}

/**
 * Master Interface for a module to communicate with a AXI Write DMA.
 * 
 * @param addressWidth the width of the address
 * @param dataWidth the width of the data
 * 
 * @note use Flipped(this) to create a slave interface.
 * 
 */ 
class AXIWriteMasterIF(addrWidth: Int, dataWidth: Int) extends Bundle {
  val req = Decoupled(new AXIDMARequestPacket(addrWidth))
  val data = Decoupled(UInt(dataWidth.W))
  val done = Input(Bool())

  override def cloneType: this.type = new AXIWriteMasterIF(addrWidth, dataWidth).asInstanceOf[this.type]
}

class AXIWriteMultiplexer(
  addressWidth: Int = 64,
  dataWidth: Int = 512,
  wayNumber: Int = 1
) extends MultiIOModule {
  val S_IF = IO(Vec(wayNumber, Flipped(new AXIWriteMasterIF(addressWidth, dataWidth))))
  //val req_i = IO(Vec(wayNumber,Flipped(Decoupled(new AXIDMARequestPacket(addressWidth)))))
  val u_arb = Module(new RRArbiter(new AXIDMARequestPacket(addressWidth), wayNumber))
  for(i <- 0 until wayNumber) u_arb.io.in(i) <> S_IF(i).req

  val selected_req = u_arb.io.out
  val index_r = RegEnable(u_arb.io.chosen, 0.U, selected_req.fire())
  val index_vr = RegInit(false.B)

  val u_axi_writer = Module(new AXI4Writer(addressWidth, dataWidth))
  u_axi_writer.io.xfer.address := selected_req.bits.address
  u_axi_writer.io.xfer.length := selected_req.bits.length

  u_axi_writer.io.xfer.valid := selected_req.valid
  selected_req.ready := u_axi_writer.io.xfer.ready

  u_axi_writer.io.dataIn.bits := S_IF(index_r).data.bits
  u_axi_writer.io.dataIn.valid := S_IF(index_r).data.valid


  for(i <- 0 until wayNumber){
    S_IF(i).data.ready := u_axi_writer.io.dataIn.ready && index_r === i.U
    S_IF(i).done := u_axi_writer.io.xfer.done && index_r === i.U
  }

  val M_AXI = IO(u_axi_writer.io.bus.cloneType)
  M_AXI <> u_axi_writer.io.bus

  when(u_axi_writer.io.xfer.done){
    index_vr := false.B
  }.elsewhen(selected_req.fire()){
    index_vr := true.B
  }
}

object AXIDMAMultiplexerVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  c.emitVerilog(new AXIReadMultiplexer(64, 512, 8))
  c.emitVerilog(new AXIWriteMultiplexer(64, 512, 8))
}