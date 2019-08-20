package common

import scala.language.reflectiveCalls
import chisel3._
import chisel3.iotesters._
import scala.collection.mutable._

trait LowLevelAxiLiteTestInterface extends protoflex.ArmflexBasePeekPokeTests {
  val axiLiteList : List[AxiLiteSignals]
  val maxIte : Int = 10

  //val Seq(PortWriteAddress, PortWriteData, PortWriteResponse,
  //        PortReadAddress,  PortReadData) = (0 to 4)
  val PortWriteAddress  = AxiLiteConfig.PortWriteAddress
  val PortWriteData     = AxiLiteConfig.PortWriteData
  val PortWriteResponse = AxiLiteConfig.PortWriteResponse
  val PortReadAddress   = AxiLiteConfig.PortReadAddress
  val PortReadData      = AxiLiteConfig.PortReadData

  var axiIdx: Int = 0

  var dataWidth = 0

  /* Low Level Interface */
  def setValid(port: Int, valid: Boolean): Unit =
    port match {
      case PortWriteAddress  => poke(axiLiteList(axiIdx).awvalid, valid)
      case PortWriteData     => poke(axiLiteList(axiIdx).wvalid,  valid)
      case PortWriteResponse => poke(axiLiteList(axiIdx).bvalid,  valid)
      case PortReadAddress   => poke(axiLiteList(axiIdx).arvalid, valid)
      case _                 => poke(axiLiteList(axiIdx).rvalid,  valid)
    }
  def setReady(port: Int, ready: Boolean): Unit =
    port match {
      case PortWriteAddress  => poke(axiLiteList(axiIdx).awready, ready)
      case PortWriteData     => poke(axiLiteList(axiIdx).wready,  ready)
      case PortWriteResponse => poke(axiLiteList(axiIdx).bready,  ready)
      case PortReadAddress   => poke(axiLiteList(axiIdx).arready, ready)
      case _                 => poke(axiLiteList(axiIdx).rready,  ready)
   }
  def setData(port: Int, data: BigInt): Unit =
    port match {
      case PortWriteAddress  => poke(axiLiteList(axiIdx).awaddr, data)
      case PortWriteData     => poke(axiLiteList(axiIdx).wdata,  data)
      case PortWriteResponse => poke(axiLiteList(axiIdx).bresp,  data)
      case PortReadAddress   => poke(axiLiteList(axiIdx).araddr, data)
      case _                 => poke(axiLiteList(axiIdx).rdata,  data)
    }
  def setExtra(port: Int, extra: BigInt): Unit =
    port match {
      case PortWriteAddress  => poke(axiLiteList(axiIdx).awprot, extra)
      case PortWriteData     => poke(axiLiteList(axiIdx).wstrb,  extra)
      case PortWriteResponse => poke(axiLiteList(axiIdx).bresp,  extra)
      case PortReadAddress   => poke(axiLiteList(axiIdx).arprot, extra)
      case _                 => poke(axiLiteList(axiIdx).rresp,  extra)
    }

  // Returns the ready/valid pair of a port
  def getValid(port: Int): Boolean = {
    val valid = port match {
      case PortWriteAddress  => peek(axiLiteList(axiIdx).awvalid)
      case PortWriteData     => peek(axiLiteList(axiIdx).wvalid)
      case PortWriteResponse => peek(axiLiteList(axiIdx).bvalid)
      case PortReadAddress   => peek(axiLiteList(axiIdx).arvalid)
      case _                 => peek(axiLiteList(axiIdx).rvalid)
    }
    valid > 0 }
  def getReady(port: Int): Boolean = {
    val ready = port match {
      case PortWriteAddress  => peek(axiLiteList(axiIdx).awready)
      case PortWriteData     => peek(axiLiteList(axiIdx).wready)
      case PortWriteResponse => peek(axiLiteList(axiIdx).bready)
      case PortReadAddress   => peek(axiLiteList(axiIdx).arready)
      case _                 => peek(axiLiteList(axiIdx).rready)
    }
    ready > 0 }
  def getHandshake(port: Int): (Boolean, Boolean) =
    (getReady(port), getValid(port))
  def getData(port: Int): BigInt = {
    val data = port match {
      case PortWriteAddress  => peek(axiLiteList(axiIdx).awaddr)
      case PortWriteData     => peek(axiLiteList(axiIdx).wdata)
      case PortWriteResponse => peek(axiLiteList(axiIdx).bresp)
      case PortReadAddress   => peek(axiLiteList(axiIdx).araddr)
      case _                 => peek(axiLiteList(axiIdx).rdata)
    }
    data
  }
  def getExtra(port: Int): BigInt = {
    val extra = port match {
      case PortWriteAddress  => peek(axiLiteList(axiIdx).awprot)
      case PortWriteData     => peek(axiLiteList(axiIdx).wstrb)
      case PortWriteResponse => peek(axiLiteList(axiIdx).bresp)
      case PortReadAddress   => peek(axiLiteList(axiIdx).arprot)
      case _                 => peek(axiLiteList(axiIdx).rresp)
    }
    extra
  }

  def expectValid(port: Int, valid: Boolean): Unit =
    port match {
      case PortWriteAddress  => expect(axiLiteList(axiIdx).awvalid, valid)
      case PortWriteData     => expect(axiLiteList(axiIdx).wvalid,  valid)
      case PortWriteResponse => expect(axiLiteList(axiIdx).bvalid,  valid)
      case PortReadAddress   => expect(axiLiteList(axiIdx).arvalid, valid)
      case _                 => expect(axiLiteList(axiIdx).rvalid,  valid)
    }
  def expectReady(port: Int, ready: Boolean): Unit =
    port match {
      case PortWriteAddress  => expect(axiLiteList(axiIdx).awready, ready)
      case PortWriteData     => expect(axiLiteList(axiIdx).wready,  ready)
      case PortWriteResponse => expect(axiLiteList(axiIdx).bready,  ready)
      case PortReadAddress   => expect(axiLiteList(axiIdx).arready, ready)
      case _                 => expect(axiLiteList(axiIdx).rready,  ready)
    }
  def expectData(port: Int, data: BigInt): Unit =
    port match {
      case PortWriteAddress  => expect(axiLiteList(axiIdx).awaddr, data)
      case PortWriteData     => expect(axiLiteList(axiIdx).wdata,  data)
      case PortWriteResponse => expect(axiLiteList(axiIdx).bresp,  data)
      case PortReadAddress   => expect(axiLiteList(axiIdx).araddr, data)
      case _                 => expect(axiLiteList(axiIdx).rdata,  data)
    }
  def expectExtra(port: Int, extra: BigInt): Unit =
    port match {
      case PortWriteAddress  => expect(axiLiteList(axiIdx).awprot, extra)
      case PortWriteData     => expect(axiLiteList(axiIdx).wstrb,  extra)
      case PortWriteResponse => expect(axiLiteList(axiIdx).bresp,  extra)
      case PortReadAddress   => expect(axiLiteList(axiIdx).arprot, extra)
      case _                 => expect(axiLiteList(axiIdx).rresp,  extra)
    }

  def fireControl(ready: Boolean, valid: Boolean): Boolean =
    ready & valid
  def fireControl(control: (Boolean, Boolean)): Boolean =
    fireControl(control._1, control._2)

  def holdControl(ready: Boolean, valid: Boolean): Boolean =
    (!ready) & valid
  def holdControl(control: (Boolean, Boolean)): Boolean =
    holdControl(control._1, control._2)

  def clearValidAtFire(port: Int): Unit =
    if (fireControl(getHandshake(port))) setValid(port, false)

}

trait HighLevelAxiLiteTestInterface extends LowLevelAxiLiteTestInterface{
  // More complex behaviour
  // Note: those should occult the port numbers
  def setWriteAddress(addr: BigInt, valid: Boolean = true, prot: BigInt = 0): Unit = {
    setData  (PortWriteAddress, addr)
    setValid (PortWriteAddress, valid)
    setExtra (PortWriteAddress, prot)
  }
  def setWriteAddressValid(valid: Boolean){
    setValid (PortWriteAddress, valid)
  }

  def setWriteData(data: BigInt, valid: Boolean = true, strobe: String = "1111"): Unit = {
    setData  (PortWriteData, data)
    setValid (PortWriteData, valid)
    setExtra (PortWriteData, BigInt(strobe, 2))
  }
  def setWriteDataValid(valid: Boolean): Unit =
    setValid (PortWriteData, valid)

  def setReadAddress(addr: BigInt, valid: Boolean = true, prot: BigInt = 0): Unit = {
    setData  (PortReadAddress, addr)
    setValid (PortReadAddress, valid)
    setExtra (PortReadAddress, prot)
  }
  def setReadAddressValid(valid: Boolean): Unit =
    setValid (PortReadAddress, valid)

  def setWriteResponseReady(ready: Boolean): Unit =
    setReady(PortWriteResponse, ready)

  def setReadDataReady(ready: Boolean): Unit =
    setReady(PortReadData, ready)

  def getWriteAddressFire: Boolean =
    fireControl(getHandshake(PortWriteAddress))

  def getWriteDataFire: Boolean =
    fireControl(getHandshake(PortWriteData))

  def getWriteResponseFire: Boolean =
    fireControl(getHandshake(PortWriteResponse))
  def getWriteResponse: BigInt =
     getData(PortWriteResponse)

  def getReadAddressFire: Boolean =
    fireControl(getHandshake(PortReadAddress))

  def getReadDataFire: Boolean =
    fireControl(getHandshake(PortReadData))
  def getReadData: (BigInt, BigInt) =
    (getData(PortReadData),
     getExtra(PortReadData))

  def expectWriteResponse(response: BigInt): Unit =
    expectData(PortWriteResponse, response)

  def expectReadData(data: BigInt): Unit =
    expectData(PortReadData, data)

  def expectReadDataResponse(response: BigInt): Unit =
    expectExtra(PortReadData, response)

  def setReadAddressAndGetData(addr:BigInt): (BigInt, BigInt) = {
    setReadAddress(addr)
    var readDataAsFired = false
    while ( !readDataAsFired ){
      step(1)
      setReadDataReady(true)
      if(getReadDataFire){
        readDataAsFired = true
      }
    }
    getReadData
  }

  def writeAddrSelectiveFire(doAddress: Boolean, doData: Boolean, maxIte: Int = 10): Boolean = {
    var writeAddressFire = !doAddress
    var writeDataFire    = !doData
    var ite = 0
    do {
      if (getWriteAddressFire) { writeAddressFire = true }
      if (getWriteDataFire)    { writeDataFire = true }
      step(1)
      ite += 1
      if (writeAddressFire) { setWriteAddressValid(false) }
      if (writeDataFire)    { setWriteDataValid(false)}
    } while(!(writeAddressFire & writeDataFire) & (ite < maxIte))
        ite < maxIte
  }

  def writeDataAtAddress(addr: BigInt, data: BigInt, strobe: String = "1111"): Boolean = {
    setWriteAddress(addr, true)
    setWriteData(data, true)
    writeAddrSelectiveFire(true, true)
  }

  def writeDatatNBit(addr: BigInt, data: BigInt, byteWidth: Int): Boolean = {
    val byteShift = addr.toInt % 4
    val byteMask  = Integer.parseInt(("1"*byteWidth), 2)
    val strobe    = (byteMask << byteShift).toBinaryString
    val shiftedData = data << (byteShift * 8)
    assert((byteShift+byteWidth) <= (dataWidth/8),
           s"${byteWidth*8}Bit Write: Address need to be aligned, byteShift: ${byteShift}, addr: ${addr}")
    writeDataAtAddress(addr, shiftedData, strobe)
  }

  def writeData32Bit(addr: BigInt, data: BigInt): Boolean = {
    writeDatatNBit(addr, data, 4)
  }

  def writeData16Bit(addr: BigInt, data: BigInt): Boolean = {
    writeDatatNBit(addr, data, 2)
  }

  def writeData8Bit(addr: BigInt, data:BigInt): Boolean = {
    writeDatatNBit(addr, data, 1)
  }
}
