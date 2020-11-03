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
    res.threadID := this.threadID
    res.tag := DataBankEntry.getTagFromAddress(address, param.tagWidth())
    res.data := data
    res
  }

  def checkHit(address: UInt, threadID: UInt): Bool = this.tag === DataBankEntry.getTagFromAddress(address, param.tagWidth()) 

  def read(): UInt = data

  def valid(isWrite: Bool): Bool = true.B

}

/**
 * Data bank entry for a TLB. 
 */ 

case class TLBEntry(param: CacheParameter, physicalAddressWidth: Int) extends DataBankEntry{
  // Note that the data of TLBEntry is physical address.
  val phyAddr = UInt(physicalAddressWidth.W)
  val tag = UInt(param.tagWidth().W)
  val threadID = UInt(param.threadIDWidth().W)
  val permission = if(param.permissionIsChecked) Some(Bool()) else None // how to update the permission?

  def buildFrom(address: UInt, threadID: UInt, data: UInt): DataBankEntry = {
    val res = Wire(new TLBEntry(param, physicalAddressWidth))
    res.tag := DataBankEntry.getTagFromAddress(address, param.tagWidth())
    res.threadID := threadID
    res.phyAddr := data
    res.v := true.B
    //! permission?
    if(param.permissionIsChecked)
      res.permission.get := this.permission.get
    res
  }

  def checkHit(address: UInt, threadID: UInt): Bool = {
    this.tag === DataBankEntry.getTagFromAddress(address, param.tagWidth()) && this.threadID === threadID
  }

  def read(): UInt = {
    phyAddr
  }

  def valid(isWrite: Bool): Bool = {
    if(param.permissionIsChecked){
      isWrite === permission.get
    }
    false.B 
  }
}