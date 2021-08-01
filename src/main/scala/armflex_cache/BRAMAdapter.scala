package armflex_cache

import chisel3._
import chisel3.util._

import chisel3.stage.ChiselStage

import armflex.util.Diverter
import armflex.util.BRAMParams
import armflex.util.BRAMPort
import armflex.util.BRAM
import armflex.util.FlushQueue
//import firrtl.PrimOps.Mul

import scala.collection.mutable
//import treadle.executable.DataType
//import scala.xml.dtd.impl.Base

class BRAMorRegister(implementedWithRegister: Boolean = true)(implicit cfg: BRAMParams) extends MultiIOModule{
  val portA = IO(new BRAMPort)
  val portB = IO(new BRAMPort)

  if(implementedWithRegister){
    val pAdo = Wire(Vec(cfg.NB_COL, UInt(cfg.COL_WIDTH.W)))
    val pBdo = Wire(Vec(cfg.NB_COL, UInt(cfg.COL_WIDTH.W)))
    for(col <- 0 until cfg.NB_COL){
      val reg_bank_r = RegInit(VecInit(Seq.fill(cfg.NB_ELE)(0.U(cfg.COL_WIDTH.W))))
      when(portA.EN && portA.WE(col)){
        reg_bank_r(portA.ADDR) := portA.DI((col+1)*cfg.COL_WIDTH-1, col*cfg.COL_WIDTH)
      }
      pAdo(col) := reg_bank_r(portA.ADDR)

      when(portB.EN && portB.WE(col)){
        reg_bank_r(portB.ADDR) := portB.DI((col+1)*cfg.COL_WIDTH-1, col*cfg.COL_WIDTH)
      }
      pBdo(col) := reg_bank_r(portB.ADDR)
    }
    portA.DO := pAdo.asUInt
    portB.DO := pBdo.asUInt

  } else {
    val bram = Module(new BRAM())
    bram.portA <> portA
    bram.portB <> portB
  }
}

class BankWriteRequestPacket(
  params: DatabankParams
) extends Bundle{
  val addr = UInt(params.setWidth().W)
  val which = UInt(params.wayWidth().W)
  val data = new CacheEntry(params)

  override def cloneType: this.type = new BankWriteRequestPacket(params).asInstanceOf[this.type]
}

class BRAMPortAdapter(
  params: DatabankParams,
) extends MultiIOModule{
  val set_t = Vec(params.associativity, new CacheEntry(params))

  val frontend_read_request_i = IO(Flipped(Decoupled(UInt(params.setWidth().W))))
  val frontend_read_reply_data_o = IO(Decoupled(set_t.cloneType))

  implicit val bramParams = new BRAMParams(
    params.associativity, new CacheEntry(params).getWidth, params.setNumber, implementedWithRegister = params.implementedWithRegister
    )

  val bram_ports = IO(Flipped(Vec(2, new BRAMPort())))

  bram_ports(0).ADDR := frontend_read_request_i.bits
  bram_ports(0).EN := true.B
  bram_ports(0).WE := 0.U
  bram_ports(0).DI := 0.U
  frontend_read_reply_data_o.bits := bram_ports(0).DO.asTypeOf(set_t.cloneType)
  if(params.implementedWithRegister){
    frontend_read_reply_data_o.valid := frontend_read_request_i.valid
    frontend_read_request_i.ready := true.B
  } else {
    frontend_read_reply_data_o.valid := RegNext(frontend_read_request_i.valid)
    frontend_read_request_i.ready := true.B
  }

  when(frontend_read_request_i.valid){
    printf(p"BRAM: Accept Read Transaction: Address: ${frontend_read_request_i.bits}\n")
  }
  when(frontend_read_reply_data_o.valid){
    printf(p"BRAM: Reply Read Transaction: Data: ${frontend_read_reply_data_o.bits}\n")
  }

  val frontend_write_request_i = IO(Flipped(Decoupled(new BankWriteRequestPacket(params))))
  bram_ports(1).ADDR := frontend_write_request_i.bits.addr

  val writeValue = Wire(set_t.cloneType)
  writeValue := 0.U.asTypeOf(set_t.cloneType)
  writeValue(frontend_write_request_i.bits.which) := frontend_write_request_i.bits.data

  bram_ports(1).DI := writeValue.asUInt
  bram_ports(1).EN := frontend_write_request_i.valid
  bram_ports(1).WE := UIntToOH(frontend_write_request_i.bits.which) & Fill(params.associativity, frontend_write_request_i.valid)
  frontend_write_request_i.ready := true.B

  when(frontend_write_request_i.valid){
    printf(p"BRAM: Accept Write Transaction: Address: ${frontend_write_request_i.bits.addr}, Entry: ${frontend_write_request_i.bits.which}, Data: ${writeValue}\n")
  }

}