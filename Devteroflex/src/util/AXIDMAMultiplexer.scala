package armflex.util

import chisel3._
import chisel3.util._

import antmicro.Bus._
import antmicro.Frontend._

class AXIDMARequestPacket(
  val addrW: Int
) extends Bundle {
  val address = UInt(addrW.W)
  val length = UInt(addrW.W)
}

/**
 * Master Interface for a module to communicate with a AXI Read DMA.
 * 
 * @params addrW the width of the address
 * @params dataW the width of the data
 * 
 * 
 * @note use Flipped(this) to create a slave interface.
 * 
 */ 
class AXIReadMasterIF(addrW: Int, dataW: Int) extends Bundle {
  val req = Decoupled(new AXIDMARequestPacket(addrW))
  val data = Flipped(Decoupled(UInt(dataW.W)))
  val done = Input(Bool())

}

/**
 * DMA Worker to perform AXI read, with more than one master support.
 */ 
class AXIReadMultiplexer(
  addrW: Int = 64,
  dataW: Int = 512,
  wayNumber: Int = 1
) extends Module {
  val S_IF = IO(Vec(wayNumber, Flipped(new AXIReadMasterIF(addrW, dataW))))

  //val req_i = IO(Vec(wayNumber,Flipped(Decoupled(new AXIDMARequestPacket(addrW)))))
  val u_arb = Module(new RRArbiter(new AXIDMARequestPacket(addrW), wayNumber))
  for(i <- 0 until wayNumber) u_arb.io.in(i) <> S_IF(i).req
  val selected_req = u_arb.io.out
  val index_r = RegEnable(u_arb.io.chosen, 0.U, selected_req.fire)
  val index_vr = RegInit(false.B)

  val axiMulti_R = Module(new AXI4Reader(addrW, dataW))

  axiMulti_R.io.xfer.address := selected_req.bits.address
  axiMulti_R.io.xfer.length := selected_req.bits.length

  axiMulti_R.io.xfer.valid := selected_req.valid
  selected_req.ready := axiMulti_R.io.xfer.ready

  for(i <- 0 until wayNumber){
    S_IF(i).data.bits := axiMulti_R.io.dataOut.bits
    S_IF(i).data.valid := axiMulti_R.io.dataOut.valid && index_r === i.U
    S_IF(i).done := axiMulti_R.io.xfer.done && index_r === i.U
  }
  
  axiMulti_R.io.dataOut.ready := S_IF(index_r).data.ready

  val M_AXI = IO(axiMulti_R.io.bus.cloneType)
  M_AXI <> axiMulti_R.io.bus

  when(axiMulti_R.io.xfer.done){
    index_vr := false.B
  }.elsewhen(selected_req.fire){
    index_vr := true.B
  }

}

/**
 * Master Interface for a module to communicate with a AXI Write DMA.
 * 
 * @params addrW the width of the address
 * @params dataW the width of the data
 * 
 * @note use Flipped(this) to create a slave interface.
 * 
 */ 
class AXIWriteMasterIF(addrW: Int, dataW: Int) extends Bundle {
  val req = Decoupled(new AXIDMARequestPacket(addrW))
  val data = Decoupled(UInt(dataW.W))
  val done = Input(Bool())

}

class AXIWriteMultiplexer(
  addrW: Int = 64,
  dataW: Int = 512,
  wayNumber: Int = 1
) extends Module {
  val S_IF = IO(Vec(wayNumber, Flipped(new AXIWriteMasterIF(addrW, dataW))))
  //val req_i = IO(Vec(wayNumber,Flipped(Decoupled(new AXIDMARequestPacket(addrW)))))
  val u_arb = Module(new RRArbiter(new AXIDMARequestPacket(addrW), wayNumber))
  for(i <- 0 until wayNumber) u_arb.io.in(i) <> S_IF(i).req

  val selected_req = u_arb.io.out
  val index_r = RegEnable(u_arb.io.chosen, 0.U, selected_req.fire)
  val index_vr = RegInit(false.B)

  val axiMulti_Wr = Module(new AXI4Writer(addrW, dataW))
  axiMulti_Wr.io.xfer.address := selected_req.bits.address
  axiMulti_Wr.io.xfer.length := selected_req.bits.length

  axiMulti_Wr.io.xfer.valid := selected_req.valid
  selected_req.ready := axiMulti_Wr.io.xfer.ready

  axiMulti_Wr.io.dataIn.bits := S_IF(index_r).data.bits
  axiMulti_Wr.io.dataIn.valid := S_IF(index_r).data.valid


  for(i <- 0 until wayNumber){
    S_IF(i).data.ready := axiMulti_Wr.io.dataIn.ready && index_r === i.U
    S_IF(i).done := axiMulti_Wr.io.xfer.done && index_r === i.U
  }

  val M_AXI = IO(axiMulti_Wr.io.bus.cloneType)
  M_AXI <> axiMulti_Wr.io.bus

  when(axiMulti_Wr.io.xfer.done){
    index_vr := false.B
  }.elsewhen(selected_req.fire){
    index_vr := true.B
  }
}

object AXIDMAMultiplexerVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  c.emitVerilog(new AXIReadMultiplexer(64, 512, 8))
  c.emitVerilog(new AXIWriteMultiplexer(64, 512, 8))
}
