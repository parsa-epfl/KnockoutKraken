package common

import scala.language.reflectiveCalls
import chisel3._
import chisel3.util._

case class RegisterMapConfig(
  val nbrReg:      Int,
  val regWidth:    Int,
  val readOnly:  Seq[Boolean],
  val pulseMode: Seq[Boolean]
){
  val byteWidth   = 8
  val addrWidth   = log2Up(nbrReg)
  val strobeWidth = regWidth/byteWidth
}

class RegisterMapPort (implicit val c: RegisterMapConfig) extends Bundle{
  val writeValid  = Input(Bool())
  val writeAddr   = Input(UInt(c.addrWidth.W))
  val writeData   = Input(UInt(c.regWidth.W))
  val writeStrobe = Input(UInt(c.strobeWidth.W))
  val writeError  = Output(Bool())

  val readAddr  = Input(UInt(c.addrWidth.W))
  val readData  = Output(UInt(c.regWidth.W))
  val readError = Output(Bool())
}

class RegisterMapIO (implicit val c: RegisterMapConfig) extends Bundle{
  val port = new RegisterMapPort()

  val moduleInputs = Input(Vec(c.nbrReg, UInt(c.regWidth.W)))
  val regsValues  = Output(Vec(c.nbrReg, UInt(c.regWidth.W)))
}

class RegisterMap(implicit c: RegisterMapConfig) extends Module{
  val io = IO(new RegisterMapIO())

  /* Registers */
  val regsNext = Wire(Vec(c.nbrReg, UInt(c.regWidth.W)))
  val regs     = RegNext(regsNext)

  io.regsValues := regs

  val readOnlyVec = Wire(Vec(c.nbrReg, Bool()))
  for (i <- 0 until c.nbrReg) readOnlyVec(i) := c.readOnly(i).B
  val writeValid = io.port.writeValid && (io.port.writeStrobe =/= 0.U)

  /* Write Port */
  // Check for error
  val validWriteAddr   = io.port.writeAddr <= (c.nbrReg - 1).asUInt(c.addrWidth.W)
  val writeRegReadOnly = readOnlyVec(io.port.writeAddr) === 1.U
  val writeErrorNext   = writeValid & ((~validWriteAddr) | writeRegReadOnly)
  val writeErrorReg    = RegEnable(writeErrorNext, io.port.writeValid)
  io.port.writeError := writeErrorReg

  /* Read Port */
  val validReadAddr = io.port.readAddr  <= (c.nbrReg - 1).asUInt(c.addrWidth.W)
  io.port.readError := ~validReadAddr
  io.port.readData  := regs(io.port.readAddr)

  /* Register Write Management */

  // One Hot write address encodinge
  val writeAddressOH = Wire(UInt(c.nbrReg.W))
  when(writeErrorNext === true.B | writeValid === false.B){
    writeAddressOH := 0.U
  }.otherwise{
    writeAddressOH := UIntToOH(io.port.writeAddr, c.nbrReg)
  }

  // Strobe
  val strobedWriteDataBytes = Wire(Vec(c.strobeWidth, UInt(c.byteWidth.W)))
  def getByte(bits: UInt, idx: Int): UInt = bits((idx+1)*c.byteWidth-1, idx*c.byteWidth)
  for (i <- 0 until c.strobeWidth){
    strobedWriteDataBytes(i) := Mux(io.port.writeStrobe(i),
                                  getByte(io.port.writeData, i),
                                  getByte(regs(io.port.writeAddr),i))
  }
  val strobedWriteData = strobedWriteDataBytes.asUInt

  // Data assignment
  for ( i <- 0 until c.nbrReg ){
    // When read only, data in the register comes from the module
    if (c.readOnly(i)){
      regsNext(i) := io.moduleInputs(i)
    }
    else {
      regsNext(i) := MuxCase(
        regs(i), Array(
          (writeAddressOH(i) === true.B) -> strobedWriteData,
          (c.pulseMode(i).B) -> 0.U))
    }
  }
  /*
  // TODO: find a way to have a clean methodology for the printf
  printf(p"RegisterMap: ")
  printf(p"\n  Params: ")
  printf(p"pulseMode:"          + p"${Binary(pulseMode)}")
  printf(p", readOnly:"         + p"${Binary(readOnly)}")
  printf(p"\n  Write: ")
  printf(p"WriteValid:"         + p"${Binary(writeValid)}")
  printf(p", WriteAddressOH:"   + p"${Binary(writeAddressOH)}")
  printf(p", WriteData:"        + p"0x${Hexadecimal(io.port.writeData)}")
  printf(p", strobe:"           + p"${Binary(io.port.writeStrobe)}")
  printf(p", strobedWriteData:" + p"${Hexadecimal(strobedWriteData)}")
  printf(p", WriteErrorReg:"      + p"${Binary(writeErrorReg)}")
  printf(p"\n  Read: ")
  printf(p"readAddr:"           + p"${Binary(io.port.readAddr)}")
  printf(p", readData:"         + p"0x${Hexadecimal(io.port.readData)}")
  printf(p", readError:"        + p"${Binary(io.port.readError)}")
  printf(p"\n  Regs: ")
  printf(p"regsValues : "       + p"${io.regsValues}")
  printf(p", moduleInputs : "   + p"${io.moduleInputs}")
  printf("\n")
  // */
}
