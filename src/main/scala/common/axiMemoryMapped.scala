/** Credits to my friend Laurier Loiselle
  *  source: https://github.com/laurierloi/fpgabits/
  */

package common

import scala.language.reflectiveCalls
import chisel3._
import chisel3.util._
import chisel3.experimental._

case class AxiMemoryMappedConfig(
  val nbrReg: Int,
  val readOnly: Seq[Boolean],
  val pulseMode: Seq[Boolean]
){
  // TODO: add assert check on pulseMode and readOnly regs for conflicts
  val addrWidth = log2Up(nbrReg) + 2 // 2 extra bits for the byte level adressing
  val axiLiteConfig = new AxiLiteConfig(addrWidth)
  val dataWidth = axiLiteConfig.dataWidth
  val Seq(rOKAY, rEXOKAY, rSLVERR, rDECERR) = 0 until 4
  val regMapConfig = new RegisterMapConfig(nbrReg, dataWidth, readOnly, pulseMode)
}

class AxiMemoryMappedIO (implicit val c: AxiMemoryMappedConfig) extends Bundle{
  val axiLite = AxiLiteSlave(c.axiLiteConfig)

  /* registerMap */
  val regPort = Flipped(new RegisterMapPort()(c.regMapConfig))
}

class AxiMemoryMapped(implicit c: AxiMemoryMappedConfig) extends Module{
  val io = IO(new AxiMemoryMappedIO())


  val writeAddressEnq  = io.axiLite.getAddressWriteEnq
  val writeDataEnq     = io.axiLite.getWriteEnq
  val writeResponseDeq = io.axiLite.getWriteResponseDeq
  val readAddressEnq   = io.axiLite.getAddressReadEnq
  val readDataDeq      = io.axiLite.getReadDataDeq
  // TODO: enq ready management

  /* Write Address Control */
  val writeAddr = RegEnable(writeAddressEnq.bits, 0.U, writeAddressEnq.fire())
  writeAddressEnq.ready := true.B

  // Don't care about prot...
  val writeProt = io.axiLite.awprot//RegEnable(io.axiLite.awprot,    0.U, writeAddressEnq.fire())

  /* Write Response */
  // Must track writeAddressEnq.fire() and writeAddressEnq.fire()
  // Once we sse a valid address, we keep store this info until the end of the transaction
  // This is done because the write and address can come in any order in Axi4
  // (See AMBA AXI specification p.A3-42)
  val validWriteAddressEnable = Wire(Bool())
  val validWriteAddress: Bool = RegEnable(writeAddressEnq.fire(), false.B, validWriteAddressEnable)
  validWriteAddressEnable := (~validWriteAddress) | writeResponseDeq.fire()
  val validWriteEnable = Wire(Bool())
  val validWrite: Bool = RegEnable(writeDataEnq.fire(), false.B, validWriteEnable)
  validWriteEnable := (~validWrite) | writeResponseDeq.fire()


  /* Write Data */
  val writeValid  = RegNext(writeDataEnq.fire())
  val writeData   = RegEnable(writeDataEnq.bits, 0.U, writeDataEnq.fire())
  val writeStrobe = RegEnable(io.axiLite.wstrb, 0.U, writeDataEnq.fire())
  writeDataEnq.ready := validWriteAddress // Only set ready once a valid address has been set


  io.regPort.writeValid  := writeValid & validWriteAddress
  io.regPort.writeAddr   := writeAddr >> 2 // regPort works with 32bit addressing
  io.regPort.writeData   := writeData
  io.regPort.writeStrobe := writeStrobe

  // Response
  val validWriteResp = RegNext(validWriteAddress & validWrite)
  writeResponseDeq.valid := validWriteResp
  when(io.regPort.writeError){
    writeResponseDeq.bits := c.rSLVERR.asUInt
  }.otherwise{
    writeResponseDeq.bits := c.rOKAY.asUInt
  }


  /* Read Address Control */
  val readAddrFire = readAddressEnq.fire()
  val readAddr = RegEnable(readAddressEnq.bits, 0.U, readAddrFire)
  io.regPort.readAddr  := readAddr >> 2 // regPort works with 32bit based adressing, not 8bit like readAddr
  readAddressEnq.ready := true.B
  // Don't care about prot...
  val readProt = io.axiLite.awprot //RegEnable(io.axiLite.awprot,     0.U, readAddressEnq.fire())

  /* Read Data */
  val readValidNext = Wire(Bool())
  val readValid = RegNext(readValidNext)
  when(readValid & ~readDataDeq.ready){
    readValidNext := readValid
  }.otherwise {
    readValidNext := readAddressEnq.fire()}

  val readData  = io.regPort.readData
  readDataDeq.valid := readValid
  readDataDeq.bits  := readData

  when(io.regPort.readError){
    io.axiLite.rresp  := c.rSLVERR.asUInt
  }.otherwise{
    io.axiLite.rresp  := c.rOKAY.asUInt
  }

  /*
  printf(p"${io.axiLite}")
  printf(p"WriteEnq: ${writeDataEnq.valid}, ${writeDataEnq.ready}, ${Hexadecimal(writeDataEnq.bits)}\n")
  printf(p"regPort Write: ${writeValid}, ${Binary(writeAddr)}" +
          p" , ${Hexadecimal(writeData)}, ${Binary(writeStrobe)}\n")
  printf(p"validWriteAddress: $validWriteAddress, validWrite: $validWrite, validWriteResp: $validWriteResp\n")
  // */
}

class AxiMemoryMappedWithRegs(implicit val c: AxiMemoryMappedConfig) extends Module{
  val io = IO(new Bundle{
                val axiLite      = AxiLiteSlave(c.axiLiteConfig)
                val moduleInputs = Input(Vec(c.nbrReg,  UInt(c.dataWidth.W)))
                val regsValues   = Output(Vec(c.nbrReg, UInt(c.dataWidth.W)))
              })
    val axiMemoryMapped = Module(new AxiMemoryMapped())
    val registerMap     = Module(new RegisterMap()(c.regMapConfig))

    io.axiLite          <> axiMemoryMapped.io.axiLite
    registerMap.io.port <> axiMemoryMapped.io.regPort
    io.moduleInputs     <> registerMap.io.moduleInputs
    io.regsValues       <> registerMap.io.regsValues
}
