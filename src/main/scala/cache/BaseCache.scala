package armflex.cache

import chisel3._
import chisel3.util._

case class CacheParameter(
  // Cache properties
  setNumber: Int = 1024,
  associativity: Int = 4,
  blockBit: Int = 512,  
  // Address
  addressWidth: Int = 55, // address to access the whole block instead of one byte
  // Thread id
  threadNumber: Int = 4,
  // Permission recordings. A D cache needs one bit to record the permission of the data.
  writable: Boolean = false,
  // operation port width
  operandWidth: Int = 32, // 32 for ICache, 128 usually for DCache?
){
  assert (isPow2(setNumber))
  assert (isPow2(associativity))

  def getTagSize(): Int = {
    addressWidth - log2Ceil(setNumber)
  }

  def threadIDWidth(): Int = {
    if(threadNumber > 0)
      log2Ceil(threadNumber)
    else
      0
  }

  def wayWidth(): Int = {
    log2Ceil(associativity)
  }

}

/**
 * The generator for BRAM-based cache.
 * 
 * The size of all operands is `param.blockBit`, which is typically 512bit
 * for a cache. If you hope to access the cache by 32bit, please partition 
 * the block manually. 
 */ 
class BaseCache[T_ENTRY <: DataBankEntry, T_LRU_CORE <: LRUCore](
  param: CacheParameter,
  entry_t: T_ENTRY,
  lru_core_t: LRUCore,
) extends Module{
  val data_bank = Module(new DataBank(param, entry_t))
  val lru = Module(new LRU(param, lru_core_t, false))
  val io = IO(new Bundle{
    val frontend = data_bank.io.frontend.cloneType
    val backend = data_bank.io.backend.cloneType
    val notify_wakeup_o = data_bank.io.notify_wakeup_o.cloneType
    val notify_sleep_o = data_bank.io.notify_sleep_o.cloneType
  })

  io.frontend <> data_bank.io.frontend
  io.backend <> data_bank.io.backend
  io.notify_wakeup_o <> data_bank.io.notify_wakeup_o
  io.notify_sleep_o <> data_bank.io.notify_sleep_o

  data_bank.io.lru <> lru.io
}