package armflex

import armflex_cache._
import chisel3._
import chisel3.util._

object QEMUMessagesType {
  val encodingW = 3
  val sPageFaultNotify = 4.U(encodingW.W)
  val sEvictNotify = 5.U(encodingW.W)
  val sEvictDone = 6.U(encodingW.W)

  val sPageEvict = 7.U(encodingW.W)
  val sMissReply = 2.U(encodingW.W)
  val sEvictReply = 3.U(encodingW.W)
}

trait VectorSerializable {
  /**
   * Width of vector elements
   */
  val vecSerialiazedWidth: Int = 32 // Only support 32 bits for now

  /**
   * Takes the current bundle and serializes it in a vector with subelements of `vecWidth` lenght
   * @return Bundle serialized in a vector with elements of size 32
   */
  def asVec: Vec[UInt]

  /**
   *
   * @params vec
   * @return A vector from the pre-serialized elements of size `width`
   */
  def parseFromVec(vec: Vec[UInt]): this.type
}

/**
 * This file records all the hardware structures defined in the demander.cc
 */
class PTTagPacket(params: PageTableParams) extends Bundle
  with VectorSerializable {
  val vpn = UInt(params.vPageW.W)
  val asid = UInt(params.asidW.W)

  def asVec: Vec[UInt] = VecInit(Seq(asid.pad(32), vpn(31, 0), vpn(params.vPageW-1, 32).pad(32) ))

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.asid := f(0)
    res.vpn := Cat(f(2), f(1))
    res.asInstanceOf[this.type]
  }

  override def toPrintable: Printable = p"PTTagPacket(asid[${Hexadecimal(asid)}]:vpn[${Hexadecimal(vpn)}]\n"
}

class PTEntryPacket(params: PageTableParams) extends Bundle
  with VectorSerializable {
  val ppn = UInt(params.pPageW.W) // 24
  val perm = UInt(params.permW.W) // 2
  val modified = Bool() // 1

  def permValid(targetPerm: UInt): Bool = MuxLookup(targetPerm, false.B, Seq(
    0.U -> (perm === 0.U || perm === 1.U),
    1.U -> (perm === 1.U),
    2.U -> (perm === 2.U)
    ))

  def asVec: Vec[UInt] = VecInit(Seq(ppn, perm, modified))

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.ppn := f(0)
    res.perm := f(1)
    res.modified := f(2)
    res.asInstanceOf[this.type]
  }

}

class PageTableItem(params: PageTableParams) extends Bundle
  with VectorSerializable {
  val tag = new PTTagPacket(params)     // 3
  val entry = new PTEntryPacket(params) // 3

  def asVec: Vec[UInt] = VecInit(tag.asVec ++ entry.asVec)

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.tag := res.tag.parseFromVec(VecInit(f.slice(0, 3)))
    res.entry := res.entry.parseFromVec(VecInit(f.slice(3, 6)))
    res.asInstanceOf[this.type]
  }

}


class TLBMissRequestMessage(params: PageTableParams) extends Bundle {
  val tag = new PTTagPacket(params)
  val perm = UInt(params.permW.W)
  val thid = UInt(log2Ceil(params.thidN).W)

}

class TLBEvictionMessage(param: PageTableParams) extends Bundle {
  val tag = new PTTagPacket(param)
  val entry = new PTEntryPacket(param)

}

trait RawMessage extends Bundle
  with VectorSerializable {
  val message_type: UInt
  val data: Vec[UInt]
}

class RxMessage extends RawMessage
  with VectorSerializable {
  val message_type = UInt(QEMUMessagesType.encodingW.W)
  val data = Vec(8, UInt(32.W))

  def asVec: Vec[UInt] = VecInit(Seq(message_type) ++ data)

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.message_type := f(0)
    res.data := f.slice(1, 9)
    res.asInstanceOf[this.type]
  }
}

//TODO: Mark the parseFromVec to a static member
class TxMessage extends RawMessage
  with VectorSerializable {
  val message_type = UInt(QEMUMessagesType.encodingW.W)
  val data = Vec(15, UInt(32.W)) // Maximum is 512bit

  def asVec: Vec[UInt] = VecInit(Seq(message_type) ++ data)

  def parseFromVec(f: Vec[UInt]): this.type = {
    assert(f.size <= 16 && f.size > 1)
    val res = Wire(this.cloneType)
    res.message_type := f(0)
    res.data := VecInit(f.slice(1, f.size))
    res.asInstanceOf[this.type]
  }
}

abstract class SerializableToRaw[T <: RawMessage](msg: T) extends Bundle
  with VectorSerializable {

  def getMessageType: UInt

  def getRawMessage: T = {
    val res = WireInit(0.U.asTypeOf(msg))
    val rawVec = this.asVec
    assert(rawVec.length < res.data.length)
    res.message_type := this.getMessageType
    for(i <- 0 until rawVec.length)
      res.data(i) := rawVec(i)

    res
  }
}

class QEMUPageEvictRequest(params: PageTableParams) extends SerializableToRaw(new RxMessage){
  val tag = new PTTagPacket(params)

  def asVec: Vec[UInt] = tag.asVec

  def parseFromVec(vec: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.tag := res.tag.parseFromVec(vec)
    res.asInstanceOf[this.type]
  }

  def getMessageType: UInt = QEMUMessagesType.sPageEvict

}


class QEMUMissReply(params: PageTableParams) extends SerializableToRaw(new RxMessage) {
  val tag = new PTTagPacket(params)
  val perm = UInt(params.permW.W)
  val thid = UInt(log2Ceil(params.thidN).W)
  val thid_v = Bool()
  val ppn = UInt(params.pPageW.W)

  def asVec: Vec[UInt] = VecInit(tag.asVec ++ Seq(perm, ppn))

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.tag := res.tag.parseFromVec(VecInit(f.slice(0, 3)))
    res.perm := f(3)
    res.thid := f(4)
    res.thid_v := f(4) =/= -1.S(32.W).asUInt
    res.ppn := f(5)
    res.asInstanceOf[this.type]
  }

  def getMessageType: UInt = QEMUMessagesType.sMissReply

}

class QEMUEvictReply(params: PageTableParams) extends SerializableToRaw(new RxMessage) {
  val tag = new PTTagPacket(params)
  val old_ppn = UInt(params.pPageW.W)
  // val synonym_v = Bool()

  def asVec: Vec[UInt] = VecInit(tag.asVec ++ Seq(old_ppn /*, synonym_v */))

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.tag := res.tag.parseFromVec(VecInit(f.slice(0, 3)))
    res.old_ppn := f(3)
    res.asInstanceOf[this.type]
  }

  def getMessageType: UInt = QEMUMessagesType.sEvictReply

}


class PageFaultNotification(params: PageTableParams) extends SerializableToRaw(new TxMessage) {
  val tag = new PTTagPacket(params)
  val perm = UInt(params.permW.W)
  val thid = UInt(log2Ceil(params.thidN).W)

  def asVec: Vec[UInt] = VecInit(tag.asVec ++ Seq(perm, thid))

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.tag := tag.parseFromVec(VecInit(f.slice(0, 3)))
    res.perm := f(3)
    res.thid := f(4)
    res.asInstanceOf[this.type]
  }

  def getMessageType: UInt = QEMUMessagesType.sPageFaultNotify

}

class PageEvictNotification(message_type: UInt, params: PageTableParams) extends SerializableToRaw(new TxMessage) {
  val item = new PageTableItem(params)

  def asVec: Vec[UInt] = item.asVec

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.item := item.parseFromVec(f)
    res.asInstanceOf[this.type]
  }

  def getMessageType: UInt = message_type

}
