/*
package common

import scala.language.reflectiveCalls
import chisel3._
import chisel3.iotesters._
import scala.collection.mutable._

import AxiLite.ports._

trait LowLevelAxiLiteTestInterface extends protoflex.ArmflexBasePeekPokeTests {
  //val Seq(portWA, AxiLite.portWD, portRA,
  //        portRA,  portRD) = (0 to 4)

  /* Low Level Interface */
  def setValid(port: Int, valid: Boolean)(implicit axiLite: AxiLiteSignals): Unit =
    port match {
      case PortWrAddr => poke(axiLite.awvalid, valid)
      case PortWrData => poke(axiLite.wvalid,  valid)
      case PortWrResp => poke(axiLite.bvalid,  valid)
      case PortRdAddr => poke(axiLite.arvalid, valid)
      case PortRdData => poke(axiLite.rvalid,  valid)
    }

  def setReady(port: Int, ready: Boolean)(implicit axiLite: AxiLiteSignals): Unit =
    port match {
      case PortWrAddr => poke(axiLite.awready, ready)
      case PortWrData => poke(axiLite.wready,  ready)
      case PortWrResp => poke(axiLite.bready,  ready)
      case PortRdAddr => poke(axiLite.arready, ready)
      case PortRdData => poke(axiLite.rready,  ready)
    }

  def setData(port: Int, data: BigInt)(implicit axiLite: AxiLiteSignals): Unit =
    port match {
      case PortWrAddr => poke(axiLite.awaddr, data)
      case PortWrData => poke(axiLite.wdata,  data)
      case PortWrResp => poke(axiLite.bresp,  data)
      case PortRdAddr => poke(axiLite.araddr, data)
      case PortRdData => poke(axiLite.rdata,  data)
    }

  def setExtra(port: Int, extra: BigInt)(implicit axiLite: AxiLiteSignals): Unit =
    port match {
      case PortWrAddr => poke(axiLite.awprot, extra)
      case PortWrData => poke(axiLite.wstrb,  extra)
      case PortWrResp => poke(axiLite.bresp,  extra)
      case PortRdAddr => poke(axiLite.arprot, extra)
      case PortRdData => poke(axiLite.rresp,  extra)
    }

  // Returns the ready/valid pair of a port
  def getValid(port: Int)(implicit axiLite: AxiLiteSignals): Boolean = {
    val valid = port match {
      case PortWrAddr => peek(axiLite.awvalid)
      case PortWrData => peek(axiLite.wvalid)
      case PortWrResp => peek(axiLite.bvalid)
      case PortRdAddr => peek(axiLite.arvalid)
      case PortRdData => peek(axiLite.rvalid)
    }
    valid > 0
  }

  def getReady(port: Int)(implicit axiLite: AxiLiteSignals): Boolean = {
    val ready = port match {
      case PortWrAddr => peek(axiLite.awready)
      case PortWrData => peek(axiLite.wready)
      case PortWrResp => peek(axiLite.bready)
      case PortRdAddr => peek(axiLite.arready)
      case PortRdData => peek(axiLite.rready)
    }
    ready > 0
  }

  def getHandshake(port: Int)(implicit axiLite: AxiLiteSignals): (Boolean, Boolean) = (getReady(port), getValid(port))

  def getData(port: Int)(implicit axiLite: AxiLiteSignals): BigInt = {
    val data = port match {
      case PortWrAddr => peek(axiLite.awaddr)
      case PortWrData => peek(axiLite.wdata)
      case PortWrResp => peek(axiLite.bresp)
      case PortRdAddr => peek(axiLite.araddr)
      case PortRdData => peek(axiLite.rdata)
    }
    data
  }

  def getExtra(port: Int)(implicit axiLite: AxiLiteSignals): BigInt = {
    val extra = port match {
      case PortWrAddr => peek(axiLite.awprot)
      case PortWrData => peek(axiLite.wstrb)
      case PortWrResp => peek(axiLite.bresp)
      case PortRdAddr => peek(axiLite.arprot)
      case PortRdData => peek(axiLite.rresp)
    }
    extra
  }

  def expectValid(port: Int, valid: Boolean)(implicit axiLite: AxiLiteSignals): Unit =
    port match {
      case PortWrAddr => expect(axiLite.awvalid, valid)
      case PortWrData => expect(axiLite.wvalid,  valid)
      case PortWrResp => expect(axiLite.bvalid,  valid)
      case PortRdAddr => expect(axiLite.arvalid, valid)
      case PortRdData => expect(axiLite.rvalid,  valid)
    }

  def expectReady(port: Int, ready: Boolean)(implicit axiLite: AxiLiteSignals): Unit =
    port match {
      case PortWrAddr => expect(axiLite.awready, ready)
      case PortWrData => expect(axiLite.wready,  ready)
      case PortWrResp => expect(axiLite.bready,  ready)
      case PortRdAddr => expect(axiLite.arready, ready)
      case PortRdData => expect(axiLite.rready,  ready)
    }

  def expectData(port: Int, data: BigInt)(implicit axiLite: AxiLiteSignals): Unit =
    port match {
      case PortWrAddr => expect(axiLite.awaddr, data)
      case PortWrData => expect(axiLite.wdata,  data)
      case PortWrResp => expect(axiLite.bresp,  data)
      case PortRdAddr => expect(axiLite.araddr, data)
      case PortRdData => expect(axiLite.rdata,  data)
    }

  def expectExtra(port: Int, extra: BigInt)(implicit axiLite: AxiLiteSignals): Unit =
    port match {
      case PortWrAddr => expect(axiLite.awprot, extra)
      case PortWrData => expect(axiLite.wstrb,  extra)
      case PortWrResp => expect(axiLite.bresp,  extra)
      case PortRdAddr => expect(axiLite.arprot, extra)
      case PortRdData => expect(axiLite.rresp,  extra)
    }

  def fireControl(ready: Boolean, valid: Boolean): Boolean = ready & valid

  def fireControl(control: (Boolean, Boolean)): Boolean = fireControl(control._1, control._2)

  def holdControl(ready: Boolean, valid: Boolean): Boolean = (!ready) & valid

  def holdControl(control: (Boolean, Boolean)): Boolean = holdControl(control._1, control._2)

  def clearValidAtFire(port: Int)(implicit axiLite: AxiLiteSignals): Unit =
    if (fireControl(getHandshake(port))) setValid(port, false)

}

trait HighLevelAxiLiteTestInterface extends LowLevelAxiLiteTestInterface {
  // More complex behaviour
  // Note: those should occult the port numbers
  def setWriteAddress(addr: BigInt, valid: Boolean = true, prot: BigInt = 0)(implicit axiLite: AxiLiteSignals): Unit = {
    setData (PortWrAddr, addr)
    setValid(PortWrAddr, valid)
    setExtra(PortWrAddr, prot)
  }
  def setWriteAddressValid(valid: Boolean)(implicit axiLite: AxiLiteSignals){
    setValid(PortWrAddr, valid)
  }

  def setWriteData(data: BigInt, valid: Boolean = true, strobe: String = "1111")(implicit axiLite: AxiLiteSignals): Unit = {
    setData (PortWrData, data)
    setValid(PortWrData, valid)
    setExtra(PortWrData, BigInt(strobe, 2))
  }
  def setWriteDataValid(valid: Boolean)(implicit axiLite: AxiLiteSignals): Unit =
    setValid(PortWrData, valid)

  def setReadAddress(addr: BigInt, valid: Boolean = true, prot: BigInt = 0)(implicit axiLite: AxiLiteSignals): Unit = {
    setData (PortRdAddr, addr)
    setValid(PortRdAddr, valid)
    setExtra(PortRdAddr, prot)
  }

  def setReadAddressValid(valid: Boolean)(implicit axiLite: AxiLiteSignals): Unit = setValid(PortRdAddr, valid)

  def setWriteResponseReady(ready: Boolean)(implicit axiLite: AxiLiteSignals): Unit = setReady(PortRdAddr, ready)

  def setReadDataReady(ready: Boolean)(implicit axiLite: AxiLiteSignals): Unit = setReady(PortRdData, ready)

  def getWriteAddressFire(implicit axiLite: AxiLiteSignals): Boolean = fireControl(getHandshake(PortWrAddr))

  def getWriteDataFire(implicit axiLite: AxiLiteSignals): Boolean = fireControl(getHandshake(PortWrData))

  def getWriteResponseFire(implicit axiLite: AxiLiteSignals): Boolean = fireControl(getHandshake(PortRdAddr))

  def getWriteResponse(implicit axiLite: AxiLiteSignals): BigInt = getData(PortRdAddr)

  def getReadAddressFire(implicit axiLite: AxiLiteSignals): Boolean = fireControl(getHandshake(PortRdAddr))

  def getReadDataFire(implicit axiLite: AxiLiteSignals): Boolean = fireControl(getHandshake(PortRdData))

  def getReadData(implicit axiLite: AxiLiteSignals): (BigInt, BigInt) = (getData(PortRdData), getExtra(PortRdData))

  def expectWriteResponse(response: BigInt)(implicit axiLite: AxiLiteSignals): Unit = expectData(PortRdAddr, response)

  def expectReadData(data: BigInt)(implicit axiLite: AxiLiteSignals): Unit = expectData(PortRdData, data)

  def expectReadDataResponse(response: BigInt)(implicit axiLite: AxiLiteSignals): Unit = expectExtra(PortRdData, response)

  def setReadAddressAndGetData(addr:BigInt)(implicit axiLite: AxiLiteSignals): (BigInt, BigInt) = {
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

  def writeAddrSelectiveFire(doAddress: Boolean, doData: Boolean, maxIte: Int = 10)(implicit axiLite: AxiLiteSignals): Boolean = {
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

  def writeDataAtAddress(addr: BigInt, data: BigInt, strobe: String = "1111")(implicit axiLite: AxiLiteSignals): Boolean = {
    setWriteAddress(addr, true)
    setWriteData(data, true)
    writeAddrSelectiveFire(true, true)
  }

  def writeDatatNBit(addr: BigInt, data: BigInt, byteWidth: Int)(implicit axiLite: AxiLiteSignals): Boolean = {
    val byteShift = addr.toInt % 4
    val byteMask  = Integer.parseInt(("1"*byteWidth), 2)
    val strobe    = (byteMask << byteShift).toBinaryString
    val shiftedData = data << (byteShift * 8)
    assert((byteShift+byteWidth) <= (AxiLite.dataWidth/8),
           s"${byteWidth*8}Bit Write: Address need to be aligned, byteShift: ${byteShift}, addr: ${addr}")
    writeDataAtAddress(addr, shiftedData, strobe)
  }

  def writeData32Bit(addr: BigInt, data: BigInt)(implicit axiLite: AxiLiteSignals): Boolean = writeDatatNBit(addr, data, 4)

  def writeData16Bit(addr: BigInt, data: BigInt)(implicit axiLite: AxiLiteSignals): Boolean = writeDatatNBit(addr, data, 2)

  def writeData8Bit(addr: BigInt, data:BigInt)(implicit axiLite: AxiLiteSignals): Boolean = writeDatatNBit(addr, data, 1)
}
 */
