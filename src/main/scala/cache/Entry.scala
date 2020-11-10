package armflex.cache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage}

/**
 * Base class of data entry.
 */ 
sealed abstract class DataBankEntry extends Bundle{
  val v = Bool() // valid bit
  val p = Bool() // pending bit. Cache is currently handle this entry.
  /**
   * Build a pending entry from given @param address and @param threadID 
   * The pending flag is raised.
   * @return a new packet.
   */ 
  def buildFrom(address:UInt, threadID: UInt): DataBankEntry

  /**
   * Build a new entry from given parameters.
   * This method is called when a entry in the bank is selected to be replaced with a new one.
   * @return a new packet.
  */ 
  def buildFrom(address:UInt, threadID: UInt, data: UInt): DataBankEntry

  /**
   * @brief @return a copy of this entry with its data updated by @param data and the @param mask . 
   */ 
  def updateData(data:UInt, mask:UInt): DataBankEntry

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


  override def buildFrom(address: UInt, threadID: UInt): DataBankEntry = {
    val res = Wire(new CacheEntry(param))
    res.v := true.B
    res.p := true.B
    res.tag := DataBankEntry.getTagFromAddress(address, param.tagWidth())
    res.data := 0.B
    res
  }

  override def buildFrom(address: UInt, threadID: UInt, data: UInt): DataBankEntry  = {
    val res = Wire(new CacheEntry(param))
    res.v := true.B
    res.p := false.B
    res.tag := DataBankEntry.getTagFromAddress(address, param.tagWidth())
    res.data := data
    res
  }

  override def updateData(data: UInt, mask: UInt): DataBankEntry = {
    val res = Wire(new CacheEntry(param))
    res.v := true.B
    res.tag := this.tag
    res.p := false.B
    val newdata = VecInit(0.U(param.blockBit.W).asBools())
    for(i <- 0 until param.blockBit){
      newdata(i) := Mux(mask(i), data(i), this.data(i))
    }
    res.data := newdata.asUInt()

    res
  }

  override def checkHit(address: UInt, threadID: UInt): Bool = this.tag === DataBankEntry.getTagFromAddress(address, param.tagWidth()) 

  override def read(): UInt = data

  override def valid(isWrite: Bool): Bool = true.B
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

  override def buildFrom(address: UInt, threadID: UInt): DataBankEntry = {
    val res = Wire(new TLBEntry(param))
    res.tag := DataBankEntry.getTagFromAddress(address, param.tagWidth())
    res.threadID := threadID
    res.v := true.B
    res.p := true.B
    res.protection := false.B
    res.phyAddr := 0.U
    res
  }

  def buildFrom(address: UInt, threadID: UInt, data: UInt): DataBankEntry = {
    val res = Wire(new TLBEntry(param))
    res.tag := DataBankEntry.getTagFromAddress(address, param.tagWidth())
    res.threadID := threadID
    res.p := false.B
    class TLBDataParser extends Bundle{
      val phyAddr = UInt(param.blockBit.W)
      val permission = UInt(2.W)
    }
    val converted = data.asTypeOf(new TLBDataParser())
    res.phyAddr := converted.phyAddr
    res.v := true.B
    res.protection := converted.permission
    res
  }

  def updateData(data: UInt, mask: UInt): DataBankEntry = {
    val res = Wire(new TLBEntry(param))
    res.v := true.B
    res.p := false.B
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
}