package armflex.cache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage}

/**
 * Base class of data entry.
 */ 
abstract class Entry extends Bundle{
  val v = Bool() // valid bit
  val d = Bool() // dirty bit.
  /**
   * Build a new entry from given parameters. (Expected to be called when refilling)
   * This method is called when a entry in the bank is selected to be replaced with a new one.
   * @return a new packet.
  */ 
  def refill(address:UInt, threadID: UInt, data: UInt): Entry

  /**
   * @brief @return a copy of this entry with its data updated by @param data and the @param mask . 
   * Expected to be called when writing.
   */ 
  def write(data:UInt, mask:UInt): Entry

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
   * @return the actual address according to the @param setNumber
   */ 
  def address(setNumber: UInt): UInt

  /**
   * @return the thread id for this entry.
   */
  def threadID(): UInt 

  /**
   * @return the tag according to the @param address and @param tagWidth
   * It will slice the 
   */ 
  protected def getTagFromAddress(address: UInt, tagWidth: Int): UInt = {
    address(address.getWidth - 1, address.getWidth - tagWidth)
  }
}


/**
 * Data bank entry for a simple cache.
 * TODO: remove all parameter entry since we only have one type of entry now.
 */ 
case class CacheEntry(param: CacheParameter) extends Entry{
  val tag = UInt(param.tagWidth().W)
  val data = UInt(param.blockBit.W)

  override def refill(address: UInt, threadID: UInt, data: UInt): Entry  = {
    val res = Wire(new CacheEntry(param))
    res.v := true.B
    res.tag := getTagFromAddress(address, param.tagWidth())
    res.d := false.B 
    res.data := data
    res
  }

  override def write(data: UInt, mask: UInt): Entry = {
    val res = Wire(new CacheEntry(param))
    res.v := true.B
    res.tag := this.tag
    res.d := true.B
    val newdata = VecInit(0.U(param.blockBit.W).asBools())
    for(i <- 0 until param.blockBit){
      newdata(i) := Mux(mask(i), data(i), this.data(i))
    }
    res.data := newdata.asUInt()

    res
  }

  override def checkHit(address: UInt, threadID: UInt): Bool = this.tag === getTagFromAddress(address, param.tagWidth()) 

  override def read(): UInt = data

  override def valid(isWrite: Bool): Bool = true.B

  def address(setNumber: UInt): UInt = {
    Cat(this.tag, setNumber)
  }

  def threadID(): UInt = {
    0.U(param.threadIDWidth().W)
  }
}
