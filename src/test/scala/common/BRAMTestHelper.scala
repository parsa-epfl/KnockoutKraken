/** Credits to my friend Laurier Loiselle
  *  source: https://github.com/laurierloi/fpgabits/
  *  Base Bram testing helper function
  */
package common

import java.nio.ByteBuffer

import scala.language.reflectiveCalls
import scala.math.pow
import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester, PeekPokeTests}

import scala.collection.mutable
import scala.collection.mutable._

/** Note: This BRAM port is expected to be 32 bit wide for data
  *       And address of 1024 words
  */
trait BRAMPortHelper extends PeekPokeTests {
  def wrBRAM32b(port: BRAMPort, bits:Int, offst: Int) = {
    poke(port.en, 1)
    poke(port.writeEn.get, 1)
    poke(port.addr, offst)
    poke(port.dataIn.get, bits)
    step(1)
    poke(port.en, 0)
    poke(port.writeEn.get, 0)
    poke(port.addr, 0)
    poke(port.dataIn.get, 0)
  }

  def wrBRAM64b(port: BRAMPort, lw: Long, offst: Int) = {
    val bytes = Array.fill(8)(0.toByte)
    for( i <- 0 to 7 ) bytes(i) = ((lw >> ((7-i) * 8)) & 0xFF).toByte
    val msb = ByteBuffer.wrap(bytes.slice(0, 4)).getInt
    val lsb = ByteBuffer.wrap(bytes.slice(4, 8)).getInt
    wrBRAM32b(port, msb, offst)
    wrBRAM32b(port, lsb, offst+1)
  }

  def rdBRAM32b(port: BRAMPort, offst:Int):Int = {
    poke(port.en, 1)
    poke(port.addr, offst)
    step(1)
    poke(port.en, 0)
    poke(port.addr, 0)
    val uint32 = peek(port.dataOut.get)
    return uint32.toInt
  }

  def rdBRAM64b(port:BRAMPort, offst:Int):Long = {
    val msb = rdBRAM32b(port, offst)
    val lsb = rdBRAM32b(port, offst+1)
    val byte_msb = Array.fill(4)(0.toByte)
    val byte_lsb = Array.fill(4)(0.toByte)
    for (i <- 0 to 3) byte_msb(i) = ((msb >> ((3-i) * 8)) & 0xFF).toByte
    for (i <- 0 to 3) byte_lsb(i) = ((lsb >> ((3-i) * 8)) & 0xFF).toByte
    val uint64 = ByteBuffer.wrap((byte_msb ++ byte_lsb)).getLong
    return uint64
  }
}


trait BRAMTestHelper extends PeekPokeTests {
  def int(x: Int): BigInt = { BigInt(x) }
  def int(x: Long): BigInt = { BigInt(x) }

  // These peek's are missingin PeekPokeTests trait,
  // but are present in PeekPokeTester
  def peek(signal: Aggregate): Seq[BigInt]
  def peek(signal: Bundle): mutable.LinkedHashMap[String, BigInt]


  val b:BRAMPort
  val configs = b.c

  // need function to:
  //   1) set random address/data
  //   2) set sequential address/data
  //   3) needs option to enable write
  //   4) needs option to disable write (for read)
  def getPort(portNbr: Int): BRAMPort = b

  def enablePort(portNbr: Int): Unit =
    poke(getPort(portNbr).en, true)

  def disablePort(portNbr: Int): Unit =
    poke(getPort(portNbr).en, false)

  def setAddr(portNbr: Int, addr: BigInt): Unit =
    poke(getPort(portNbr).addr, addr)

  def getReadPort(portNbr: Int): Int = {
    if (configs.isTrueDualPort){ portNbr }
    else { configs.sdpReadPortIdx } }

  def getWritePort(portNbr: Int): Int = {
    if (configs.isTrueDualPort){ portNbr }
    else { configs.sdpWritePortIdx } }

  def limitWidth(x: BigInt, w: Int, signed: Boolean = false): BigInt = {
    x % BigDecimal(pow(2, w)).toBigInt}

  def limitWidth(portNbr: Int, data: BigInt): BigInt = {
    limitWidth(data, configs.dataWidthVec(portNbr))
  }

  def disableWrite(portNbr: Int): Unit = {
    val port = getWritePort(portNbr)
    poke(getPort(port).writeEn.get, false)}

  def enableWrite(portNbr: Int): Unit = {
    val port = getWritePort(portNbr)
    poke(getPort(port).writeEn.get, true)}

  def writeData(portNbr: Int, data: BigInt) : Unit = {
    val port = getWritePort(portNbr)
    poke(getPort(port).dataIn.get, limitWidth(port, data))}

  def readData(portNbr: Int) : BigInt = {
    val port = getReadPort(portNbr)
    peek(getPort(port).dataOut.get)}

  def readAtAddr(portNbr: Int, addr: BigInt) : BigInt = {
    val port = getReadPort(portNbr)
    setAddr(port, addr)
    enablePort(port)
    step(1)
    disablePort(port)
    readData(port) }

  def writeAtAddr(portNbr: Int, addr: BigInt, data: BigInt): Unit = {
    val port = getWritePort(portNbr)
    setAddr(port, addr)
    enablePort(port)
    enableWrite(port)
    writeData(port, data)
    step(1)
    disableWrite(port)
  }

  def getWritePortWidth(portNbr: Int): Int = {
    configs.dataWidthVec(getWritePort(portNbr)) }
  def getReadPortWidth(portNbr: Int): Int = {
    configs.dataWidthVec(getReadPort(portNbr)) }

  def seqWrite(portNbr: Int, dataQ: Queue[BigInt], offset: Int = 10){
    val port = getWritePort(portNbr)
    val localDataQ = dataQ.clone()
    for ( addr <- 0 until configs.sizeVec(port) ) {
      val writeAddr = addr + offset
      val data = localDataQ.dequeue()
      writeAtAddr(portNbr, writeAddr, data) } }

  def randomWrite(portNbr: Int, addrQ: Queue[BigInt], dataQ: Queue[BigInt]){
    val localAddrQ = addrQ.clone()
    val localDataQ = dataQ.clone()
    while(!localAddrQ.isEmpty){
      writeAtAddr(portNbr, localAddrQ.dequeue, localDataQ.dequeue) } }

  def seqRead(portNbr: Int, offset: Int = 10): Queue[BigInt] = {
    // Make sure write is not enabled
    val port = getReadPort(portNbr)
    val readQ = new Queue[BigInt]
    for ( addr <- 0 until configs.sizeVec(port) ) {
      val readAddr = addr + offset
      readQ.enqueue(readAtAddr(port, readAddr)) }
    readQ }

  def randomRead(portNbr: Int, addrQ: Queue[BigInt]) : Queue[BigInt] = {
    val localAddrQ = addrQ.clone()
    val readQ = new Queue[BigInt]
    while(!localAddrQ.isEmpty){
      readQ.enqueue(readAtAddr(portNbr, addrQ.dequeue)) }
    readQ
  }

  def mergeReadToSizeOfWrite(portNbr: Int, readQ: Queue[BigInt]): Queue[BigInt] = {
    val writePortWidth = getWritePortWidth(portNbr)
    val readPortWidth  = getReadPortWidth(portNbr)
    val widthRatio = writePortWidth / readPortWidth
    val binaryReadQ = readQ.map(_.toString(2).reverse.padTo(readPortWidth,  '0').reverse)
    val mergedReadQ = Queue[BigInt]()
    var baseStr = ""
    for(i <- 0 until configs.sizeVec(configs.sdpWritePortIdx))  {
      baseStr = ""
      for(j <- 0 until widthRatio){
        val dataRead = binaryReadQ(i*widthRatio + j)
        baseStr = dataRead + baseStr
      }
      mergedReadQ.enqueue(BigInt(baseStr, 2))
    }
    mergedReadQ
  }
}
