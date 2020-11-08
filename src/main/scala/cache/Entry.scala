package armflex.cache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage}

/**
 * Base class of data entry.
 */ 
sealed abstract class DataBankEntry extends Bundle{
  val v = Bool() // valid bit
  val threadID: UInt
  /**
   * Build a new entry from given parameters.
   * This method is called when a entry in the bank is selected to be replaced with a new one.
   * @return a new packet.
  */ 
  def buildFrom(address:UInt, threadID: UInt, data: UInt): DataBankEntry

  /**
   * @brief @return a copy of this entry with its data updated by @param data and the @param mask . 
   */ 
  def updateData( data:UInt, mask:UInt): DataBankEntry

  /**
   * Check whether this entry matches the given address and threadID
   * @return true if match.
   */ 
  def checkHit(address: UInt, threadID: UInt): Bool

  /**
   * @return the data out from the entry.
   */ 
  def read(): UInt

  /**
   * @return whether this operation is valid
   */
  def valid(isWrite: Bool): Bool

  /**
   * Invalid this term according to the selected thread.
   * @return the flushed term
   */
  def flush(threadMask: UInt): DataBankEntry = {
    val res = WireInit(this)
    res.v := !threadMask(threadID)
    res
  }
  /**
   * @return bits to write back to the high-level cache / DRAM accepted by the backend
   */
  def writeBits(): UInt 
}

object DataBankEntry{
  def getTagFromAddress(address: UInt, tagWidth: Int): UInt = {
    address(address.getWidth - 1, address.getWidth - tagWidth)
  }
}


/**
 * Data bank entry for a simple cache.
 */ 
case class CacheEntry(param: CacheParameter) extends DataBankEntry{
  val tag = UInt(param.tagWidth().W)
  val data = UInt(param.blockBit.W)
  val threadID = UInt(param.threadIDWidth().W)
  def buildFrom(address: UInt, threadID: UInt, data: UInt): DataBankEntry  = {
    val res = Wire(new CacheEntry(param))
    res.v := true.B
    res.threadID := threadID
    res.tag := DataBankEntry.getTagFromAddress(address, param.tagWidth())
    res.data := data
    res
  }

  def updateData(data: UInt, mask: UInt): DataBankEntry = {
    val res = Wire(new CacheEntry(param))
    res.v := true.B
    res.threadID := this.threadID
    res.tag := this.tag
    val newdata = VecInit(0.U(param.blockBit.W).asBools())
    for(i <- 0 until param.blockBit){
      newdata(i) := Mux(mask(i), data(i), this.data(i))
    }
    res.data := newdata.asUInt()

    res
  }

  def checkHit(address: UInt, threadID: UInt): Bool = this.tag === DataBankEntry.getTagFromAddress(address, param.tagWidth()) 

  def read(): UInt = data

  def valid(isWrite: Bool): Bool = true.B

  def writeBits(): UInt = data
}

/**
 * Data bank entry for a TLB. 
 */ 

case class TLBEntry(param: CacheParameter) extends DataBankEntry{
  // Note that the data of TLBEntry is physical address, so data block Size is the size of physical address.
  val phyAddr = UInt(param.blockBit.W) 
  val tag = UInt(param.tagWidth().W)
  val threadID = UInt(param.threadIDWidth().W)
  val protection = UInt(2.W) // how to update the permission?

  def buildFrom(address: UInt, threadID: UInt, data: UInt): DataBankEntry = {
    val res = Wire(new TLBEntry(param))
    res.tag := DataBankEntry.getTagFromAddress(address, param.tagWidth())
    res.threadID := threadID
    class TLBDataParser extends Bundle{
      val phyAddr = UInt(param.blockBit.W)
      val permission = UInt(2.W)
    }
    val converted = data.asTypeOf(new TLBDataParser())
    res.phyAddr := converted.phyAddr
    res.v := true.B
    //! permission?
    res.protection := converted.permission
    res
  }

  def updateData(data: UInt, mask: UInt): DataBankEntry = {
    val res = Wire(new TLBEntry(param))
    res.v := true.B
    res.threadID := this.threadID
    res.tag := this.tag
    res.protection := this.protection
    val newdata = WireInit(this.phyAddr).asBools()
    for(i <- 0 until param.blockBit){
      newdata(i) := Mux(mask(i), data(i), this.phyAddr(i))
    }
    res.phyAddr := VecInit(newdata).asUInt()
    
    res
  }

  def checkHit(address: UInt, threadID: UInt): Bool = {
    this.tag === DataBankEntry.getTagFromAddress(address, param.tagWidth()) && this.threadID === threadID
  }

  def read(): UInt = {
    phyAddr
  }

  def valid(isWrite: Bool): Bool = {
    isWrite === protection
  }

  def writeBits(): UInt = {
    assert(false, "At present we don't support writing back in TLB.")
    0.U
  }
}