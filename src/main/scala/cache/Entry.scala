package armflex.cache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage}

/**
 * Base class of data entry.
 */ 
abstract class DataBankEntry extends Bundle{
  val v = Bool() // valid bit

  /**
   * Build a new entry from given parameters.
   * This method is called when a entry in the bank is selected to be replaced with a new one.
  */ 
  def buildFrom(address:UInt, threadID: UInt, data: UInt)

  /**
   * Check whether this entry matches the given address and threadID
   */ 
  def checkHit(address: UInt, threadID: UInt): Bool

  /**
   * Read data out from the entry.
   */ 
  def read(): UInt

  /**
   * Whether this operation is valid
   */
  def valid(is_write: Bool): Bool
}

object DataBankEntry{
  def getTagFromAddress(address: UInt, tagWidth: Int): UInt = {
    address(address.getWidth - 1, address.getWidth - tagWidth)
  }
}


/**
 * Data bank entry for a simple cache.
 */ 
class CacheEntry(param: CacheParameter) extends DataBankEntry{
  val tag = UInt(param.tagWidth().W)
  val data = UInt(param.blockBit.W)
  def buildFrom(address: UInt, threadID: UInt, data: UInt) = {
    this.tag := DataBankEntry.getTagFromAddress(address, param.tagWidth())
    this.data := data
  }

  def checkHit(address: UInt, threadID: UInt): Bool = this.tag === DataBankEntry.getTagFromAddress(address, param.tagWidth()) 

  def read(): UInt = data

  def valid(is_write: Bool): Bool = true.B
}

/**
 * Data bank entry for a TLB. 
 */ 

class TLBEntry(param: CacheParameter, physicalAddressWidth: Int) extends DataBankEntry{
  // Note that the data of TLBEntry is physical address.
  val phy_addr = UInt(physicalAddressWidth.W)
  val tag = UInt(param.tagWidth().W)
  val thread_id = UInt(param.threadIDWidth().W)
  val writable = if(param.writable) Some(Bool()) else None // how to update the writable?

  def buildFrom(address: UInt, threadID: UInt, data: UInt): Unit = {
    tag := DataBankEntry.getTagFromAddress(address, param.tagWidth())
    thread_id := threadID
    phy_addr := data
    //! writable?
  }

  def checkHit(address: UInt, threadID: UInt): Bool = {
    this.tag === DataBankEntry.getTagFromAddress(address, param.tagWidth()) && this.thread_id === threadID
  }

  def read(): UInt = {
    phy_addr
  }

  def valid(is_write: Bool): Bool = {
    if(param.writable){
      is_write === writable.get
    }
    false.B 
  }
}