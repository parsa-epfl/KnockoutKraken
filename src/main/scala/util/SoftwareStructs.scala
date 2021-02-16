package armflex.util

import chisel3._

import java.nio.ByteBuffer

object SoftwareStructs {
  //import scala.language.implicitConversions

  //implicit def uint2bigint(bits: UInt): BigInt = { bits.litValue }
  //implicit def bool2bigint(bits: Bool): BigInt = { bits.litValue }
  //implicit def bool2boolean(bits: Bool): Boolean = {bits.litToBoolean }

  //implicit def bigint2short(bigint: BigInt): Short = { bigint.toShort }
  //implicit def bigint2int(bigint: BigInt):   Int   = { bigint.toInt }
  //implicit def bigint2long(bigint: BigInt):  Long  = { bigint.toLong }

  //implicit def uint2short(bits: UInt): Short = { bigint2short(uint2bigint(bits)) }
  //implicit def uint2int(bits: UInt):   Int   = { bigint2int(uint2bigint(bits)) }
  //implicit def uint2long(bits: UInt):  Long  = { bigint2long(uint2bigint(bits)) }

  //implicit def bool2short(bits: Bool): Short = { bigint2short(bool2bigint(bits)) }
  //implicit def bool2int(bits: Bool):   Int   = { bigint2int(bool2bigint(bits)) }
  //implicit def bool2long(bits: Bool):  Long  = { bigint2long(bool2bigint(bits)) }

  def asU(unsigned: Long): BigInt =
    BigInt(0.toByte +: ByteBuffer.allocate(8).putLong(unsigned).array())
  def asU(unsigned: Int): BigInt = 
    BigInt(0.toByte +: ByteBuffer.allocate(4).putInt(unsigned).array())

  def toLong(unsignedLong: BigInt): Long =
    (((unsignedLong >> 1).toLong << 1) + (unsignedLong & 1).toLong)
  def toInt(unsignedLong: BigInt): Int =
    (((unsignedLong >> 1).toInt << 1) + (unsignedLong & 1).toInt)

  case class PState(
    val xregs: List[BigInt],
    val pc:    BigInt,
    val sp:    BigInt,
    val nzcv:  Int) {
    override def toString() = {
      val name = s"PROC STATE:"
      val regs = xregs.zipWithIndex.map { case (reg, i) =>
        s"X${i}:0x${"%016x".format(reg)}"
      }.mkString("\n")
      val str = Seq(
        name,
        regs,
        "PC :" + "%016x".format(pc),
        "SP :" + "%016x".format(sp),
        "EL :" + 0,
        "NZCV" + ":" + nzcv.toBinaryString
      ).mkString("\n")
      str + "\n"
    }

    def matches(other: PState): Option[String] = {
      var str: String = ""
      val diffXRegs = (this.xregs zip other.xregs).zipWithIndex.filter { case ((t, o), i) =>
        t != o
      }
      if (!diffXRegs.isEmpty || this.pc != other.pc || this.nzcv != other.nzcv) {
        str = str ++ "PState didn't match, differences FPGA - QEMU:\n"
        diffXRegs foreach { case ((t, o), i) =>
          str = str ++ s"X${i}:0x${"%016x".format(t)} != 0x${"%016x".format(o)}\n"
        }
        if (this.pc != other.pc)
          str = str ++ s"PC:0x${"%016x".format(this.pc)} != 0x${"%016x".format(other.pc)}\n"
        if (this.sp != other.sp)
          str = str ++ s"SP:0x${"%016x".format(this.sp)} != 0x${"%016x".format(other.sp)}\n"
        if (this.nzcv != other.nzcv)
          str = str ++ s"NZCV:${this.nzcv.toBinaryString} != ${other.nzcv.toBinaryString}\n"

        return Some(str)
      } else {
        str = str ++ "PState matched.\n"
        return None
      }
    }
  }

  case class CommitTrace(
    val state:    PState,
    val inst:     BigInt,
    val mem_addr: List[BigInt],
    val mem_data: List[BigInt])
}
