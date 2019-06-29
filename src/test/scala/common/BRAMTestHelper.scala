/** Credits to my friend Laurier Loiselle
  *  source: https://github.com/laurierloi/fpgabits/
  *  Base Bram testing helper function
  */
package common

import scala.language.reflectiveCalls
import scala.math.pow
import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester, PeekPokeTests}

import scala.collection.mutable
import scala.collection.mutable._

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