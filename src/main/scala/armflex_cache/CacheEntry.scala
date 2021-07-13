package armflex_cache

import chisel3._
import chisel3.util._

/**
 * Data bank entry for a simple cache.
 */ 
class CacheEntry(param: DatabankParameter) extends Bundle {
  val tag = UInt(param.tagWidth().W)
  val data = UInt(param.blockBit.W)
  val v = Bool() // valid bit
  val d = Bool() // dirty bit.

  /**
   * Build the entry from given parameters **in place**. (Expected to be called when refilling)
   * This method is called when a entry in the bank is selected to be replaced with a new one.
  */ 
  def refill(address: UInt, asid: UInt, data: UInt, valid: Bool = true.B): Unit  = {
    this.v := valid
    this.tag := getTagFromAddress(address, param.tagWidth())
    this.d := false.B 
    this.data := data
  }

  /**
   * @brief @return a new entry with its data updated by @param data and the @param mask . 
   * Expected to be called when writing.
   */ 
  def write(data: UInt, mask: UInt, refill: Bool, valid: Bool = true.B): CacheEntry = {
    when(refill){
      assert(mask.andR(), "Refill is 1 means mask is full 1!");
    }
    val res = Wire(new CacheEntry(param))
    res.d := 
      !refill && // refilling should not cause the dirty
      mask.orR() =/= 0.U // no mask 
    res.tag := this.tag
    res.v := valid
    val newdata = VecInit(0.U(param.blockBit.W).asBools())
    for(i <- 0 until param.blockBit){
      newdata(i) := Mux(mask(i), data(i), this.data(i))
    }
    res.data := newdata.asUInt()
    res
  }

  /**
   * Check whether this entry matches the given address and asid
   * @return true if match.
   */ 
  def checkHit(address: UInt, asid: UInt): Bool = this.tag === getTagFromAddress(address, param.tagWidth()) 

  /**
   * @return the data out from the entry.
   */ 
  def read(): UInt = data

  /**
   * @return whether this operation is valid
   */
  def valid(isWrite: Bool): Bool = true.B

  /**
   * @return the actual address according to the @param setNumber
   */ 
  def address(setNumber: UInt): UInt = {
    Cat(this.tag, setNumber)
  }

  /**
   * @return the thread id for this entry.
   */
  def asid(): UInt = {
    0.U(param.asidWidth.W)
  }

  def getTagFromAddress(address: UInt, tagWidth: Int): UInt = {
    address(address.getWidth - 1, address.getWidth - tagWidth)
  }

  override def cloneType: this.type = new CacheEntry(param).asInstanceOf[this.type]
}
