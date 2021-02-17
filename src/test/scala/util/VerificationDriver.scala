package armflex.util

import scala.io._
import java.nio._
import java.io._

import scala.collection.mutable._
import ArmflexProtoBuf._
import SoftwareStructs._
import armflex.CommitInst
import armflex_p.armflex_p.ArmflexCommitTrace_p

class VerificationDriver(val filename: String) {
  private lazy val classLoader: ClassLoader = getClass().getClassLoader()
  private lazy val reader: BufferedInputStream = new BufferedInputStream(
    classLoader.getResourceAsStream("test_workloads/" + filename)
  )
  private lazy val sizeBytes: Array[Byte] = new Array[Byte](4)

  private def getNextTrace: Option[CommitTrace] = {
    try {
      if (reader.available > 0 && reader.read(sizeBytes) == 4) {
        val size = ByteBuffer.wrap(sizeBytes).order(ByteOrder.LITTLE_ENDIAN).getInt
        val protoBuf: Array[Byte] = new Array[Byte](size)
        reader.read(protoBuf)
        val trace = proto2commitTrace(protoBuf)
        Some(trace)
      } else {
        reader.close
        None
      }
    } catch {
      case e: IOException => None
    }
  }

  private lazy val it: Iterator[(CommitTrace, Option[CommitTrace])] = new Iterator[(CommitTrace, Option[CommitTrace])] {
    var inst = getNextTrace
    def hasNext = inst.isDefined
    def next(): (CommitTrace, Option[CommitTrace]) = {
      val commited = getNextTrace
      val ret = (inst.get, commited)
      inst = commited
      ret
    }
  }

  def writeInstsToFile(iterator: Iterator[BigInt]): Unit = {
    val file = new BufferedOutputStream(new FileOutputStream("/dev/shm/transplantsInsts"))

    println("Failed instructions (Transplants), objdump '/dev/shm/transplantInsts' for mode details:")
    iterator.foreach {
      case bigint => {
        println("INST:" + bigint.toString(16))
        val byteArray = ByteBuffer.allocate(4).putInt(bigint.toInt).array().reverse
        file.write(byteArray)
      }
    }
    file.close()
  }

  def hasNext = it.hasNext
  def next = it.next
}

object TraceAssemblyGenerator extends App {
  val filename = if (args.length == 1) args(0) else "binary10_0"
  val traceDrv = new VerificationDriver(filename)
  val file = new BufferedOutputStream(new FileOutputStream("/dev/shm/insts"))
  println("Start")
  Stream
    .continually(traceDrv.next._1.inst)
    .takeWhile(_ => traceDrv.hasNext)
    .foreach(bigint => file.write(ByteBuffer.allocate(4).putInt(bigint.toInt).array().reverse))
  file.close()
  println("Done")
}

object CheckInsts extends App {
  val filename = if (args.length == 1) args(0) else "binary10_1"
  val traceDrv = new VerificationDriver(filename)
  val file = new PrintWriter(new BufferedWriter(new FileWriter("/dev/shm/hex", true)), true)
  println("Start")
  Stream
    .continually(traceDrv.next._1)
    .takeWhile(_ => traceDrv.hasNext)
    .foreach(trace => println(trace.state.pc.toString(16) + ":" + trace.inst.toString(16)))
  file.close()
  println("Done")
}
