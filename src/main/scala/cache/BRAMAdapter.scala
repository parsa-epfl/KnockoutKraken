package armflex.cache

import chisel3._
import chisel3.util._

import chisel3.stage.ChiselStage

import armflex.util.Diverter
import armflex.util.BRAMConfig
import armflex.util.BRAMPort
import armflex.util.BRAM
import armflex.util.FlushQueue
//import firrtl.PrimOps.Mul

import scala.collection.mutable
import treadle.executable.DataType
import scala.xml.dtd.impl.Base

class BRAMorRegister(implementedWithRegister: Boolean = true)(implicit cfg: BRAMConfig) extends MultiIOModule{
  val portA = IO(new BRAMPort)
  val portB = IO(new BRAMPort)

  if(implementedWithRegister){
    val pAdo = Wire(Vec(cfg.NB_COL, UInt(cfg.COL_WIDTH.W)))
    val pBdo = Wire(Vec(cfg.NB_COL, UInt(cfg.COL_WIDTH.W)))
    for(col <- 0 until cfg.NB_COL){
      val regBank_r = RegInit(VecInit(Seq.fill(cfg.NB_ELE)(0.U(cfg.COL_WIDTH.W))))
      when(portA.EN && portA.WE(col)){
        regBank_r(portA.ADDR) := portA.DI((col+1)*cfg.COL_WIDTH-1, col*cfg.COL_WIDTH)
      }
      pAdo(col) := regBank_r(portA.ADDR)

      when(portB.EN && portB.WE(col)){
        regBank_r(portA.ADDR) := portB.DI((col+1)*cfg.COL_WIDTH-1, col*cfg.COL_WIDTH)
      }
      pBdo(col) := regBank_r(portB.ADDR)
    }
    portA.DO := pAdo.asUInt()
    portB.DO := pBdo.asUInt()

  } else {
    val bram = Module(new BRAM())
    bram.portA <> portA
    bram.portB <> portB
  }
}

class BankWriteRequestPacket(
  t: Entry,
  param: CacheParameter
) extends Bundle{
  val addr = UInt(param.setWidth().W)
  val which = UInt(param.wayWidth().W)
  val data = t.cloneType

  override def cloneType: this.type = new BankWriteRequestPacket(t, param).asInstanceOf[this.type]
}

class BRAMPortAdapter(
  t: Entry,
  param: CacheParameter,
) extends MultiIOModule{
  val set_t = Vec(param.associativity, new CacheEntry(param))

  val frontendReadRequest_i = IO(Flipped(Decoupled(UInt(param.setWidth().W))))
  val frontendReadReplyData_o = IO(Decoupled(set_t.cloneType))

  implicit val bramCfg = new BRAMConfig(
    param.associativity, new CacheEntry(param).getWidth, param.setNumber
  )

  val bramPortA = IO(Flipped(new BRAMPort()))
  bramPortA.ADDR := frontendReadRequest_i.bits
  bramPortA.EN := true.B
  bramPortA.WE := 0.U
  bramPortA.DI := 0.U
  frontendReadReplyData_o.bits := bramPortA.DO.asTypeOf(set_t.cloneType)
  if(param.implementedWithRegister){
    frontendReadReplyData_o.valid := frontendReadRequest_i.valid
    frontendReadRequest_i.ready := true.B
  } else {
    frontendReadReplyData_o.valid := RegNext(frontendReadRequest_i.valid)
    frontendReadRequest_i.ready := true.B
  }

  when(frontendReadRequest_i.valid){
    printf(p"BRAM: Accept Read Transaction: Address: ${frontendReadRequest_i.bits}\n")
  }
  when(frontendReadReplyData_o.valid){
    printf(p"BRAM: Reply Read Transaction: Data: ${frontendReadReplyData_o.bits}\n")
  }

  val frontendWriteRequest_i = IO(Flipped(Decoupled(new BankWriteRequestPacket(t, param))))
  val bramPortB = IO(Flipped(new BRAMPort()))
  bramPortB.ADDR := frontendWriteRequest_i.bits.addr

  val writeValue = Wire(set_t.cloneType)
  writeValue := 0.U.asTypeOf(set_t.cloneType)
  writeValue(frontendWriteRequest_i.bits.which) := frontendWriteRequest_i.bits.data

  bramPortB.DI := writeValue.asUInt()
  bramPortB.EN := frontendWriteRequest_i.valid
  bramPortB.WE := UIntToOH(frontendWriteRequest_i.bits.which) & Fill(param.associativity, frontendWriteRequest_i.valid)
  frontendWriteRequest_i.ready := true.B

  when(frontendWriteRequest_i.valid){
    printf(p"BRAM: Accept Write Transaction: Address: ${frontendWriteRequest_i.bits.addr}, Entry: ${frontendWriteRequest_i.bits.which}, Data: ${writeValue}\n")
  }

}