package utils

import chisel3._

object TypeConverters {

  def ByteArray2UBigInt(bytes: Array[Byte]): BigInt = BigInt(Array(0.toByte) ++ bytes)
  def ByteArray2UInt(bytes: Array[Byte]): UInt = ByteArray2UBigInt(bytes).U

}
