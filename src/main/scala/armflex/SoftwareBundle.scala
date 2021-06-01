package armflex

import armflex_cache._
import chisel3._
import chisel3.util._

abstract class SoftwareControlledBundle extends Bundle {
  def asVec(width: Int): Vec[UInt]
  def parseFromVec(vec: Vec[UInt]): this.type
}

/**
 * This file records all the hardware structures defined in the demander.cc 
 */

class PTTagPacket(param: TLBParameter) extends SoftwareControlledBundle {
  val vpn = UInt(param.vPageWidth.W) // 52
  val asid = UInt(param.asidWidth.W) // 16

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(Seq(
      vpn(31, 0),
      vpn(param.vPageWidth-1, 32),
      asid
    ))
  }

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.vpn := Cat(f(1), f(0))
    res.asid := f(2)
    return res.asInstanceOf[this.type]
  }

  override def cloneType: this.type = {
    return new PTTagPacket(param).asInstanceOf[this.type]
  }
}

class PTEntryPacket(param: TLBParameter) extends SoftwareControlledBundle {
  val ppn = UInt(param.pPageWidth.W) // 24
  val permission = UInt(param.permissionWidth.W) // 2
  val modified = Bool() // 1

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(Seq(
      ppn,
      permission,
      modified
    ))
  }

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.ppn := f(0)
    res.permission := f(1)
    res.modified := f(2)
    return res.asInstanceOf[this.type]
  }

  override def cloneType: this.type = {
    return new PTEntryPacket(param).asInstanceOf[this.type]
  }

  def permissionValid(targetPermission: UInt): Bool = MuxLookup(
    targetPermission,
    false.B,
    Seq(
      (0.U) -> (permission === 0.U || permission === 1.U),
      (1.U) -> (permission === 1.U),
      (2.U) -> (permission === 2.U)
    )
  )
}

class PageTableItem(param: TLBParameter) extends SoftwareControlledBundle {
  val tag = new PTTagPacket(param)     // 3
  val entry = new PTEntryPacket(param) // 3

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(tag.asVec(width).toSeq ++ entry.asVec(width).toSeq)
  }

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.tag := res.tag.parseFromVec(VecInit(f.slice(0, 3)))
    res.entry := res.entry.parseFromVec(VecInit(f.slice(3, 6)))
    return res.asInstanceOf[this.type]
  }

  override def cloneType: this.type = {
    return new PageTableItem(param).asInstanceOf[this.type]
  }
}


class TLBMissRequestMessage(param: TLBParameter) extends Bundle {
  val tag = new PTTagPacket(param)
  val permission = UInt(param.permissionWidth.W)
  val tid = UInt(log2Ceil(param.threadNumber).W)

  override def cloneType: this.type = {
    return new TLBMissRequestMessage(param).asInstanceOf[this.type]
  }
}

class TLBEvictionMessage(param: TLBParameter) extends Bundle {
  val tag = new PTTagPacket(param)
  val entry = new PTEntryPacket(param)

  override def cloneType: this.type = {
    return new TLBEvictionMessage(param).asInstanceOf[this.type]
  }
}


abstract class RawMessage extends SoftwareControlledBundle {
  val message_type: UInt
  val data: Vec[UInt]
}

object QEMUMessagesType {
  val sPageFaultNotify = 4.U(3.W)
  val sEvictNotify = 5.U(3.W)
  val sEvictDone = 6.U(3.W)
  
  val sPageEvict = 7.U(3.W)
  val sMissReply = 2.U(3.W)
  val sEvictReply = 3.U(3.W)
}

class QEMURxMessage extends RawMessage {
  val message_type = UInt(2.W)
  val data = Vec(8, UInt(32.W))
  def asVec(width: Int): Vec[UInt] = {
    return VecInit(Seq(message_type) ++ data.toSeq)
  }

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.message_type := f(0)
    res.data := f.slice(1, 9)
    return res.asInstanceOf[this.type]
  }
}

//TODO: Mark the parseFromVec to a static member 
class QEMUTxMessage extends RawMessage {
  val message_type = UInt(3.W)
  val data = Vec(15, UInt(32.W)) // Maximum is 512bit

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(Seq(message_type) ++ data.toSeq)
  }
  
  def parseFromVec(f: Vec[UInt]): this.type = {
    assert(f.size <= 16 && f.size > 1)
    val res = Wire(this.cloneType)
    res.message_type := f(0)
    res.data := VecInit(f.slice(1, f.size))
    return res.asInstanceOf[this.type]
  }
}

abstract class MessageUnionSubtype[T <: RawMessage](mess: T) extends SoftwareControlledBundle {
  def messageType: UInt
  def getRawMessage: T = {
    val res = Wire(mess.cloneType)
    res.message_type := messageType
    val rawVec = asVec(32)
    for(i <- 0 until res.data.length){
      if(i >= rawVec.length)
        res.data(i) := 0.U
      else
        res.data(i) := rawVec(i)
    }
    return res
  }
}

class QEMUPageEvictRequest(param: TLBParameter) extends MessageUnionSubtype(new QEMURxMessage){
  val tag = new PTTagPacket(param)
  def asVec(width: Int): Vec[UInt] = {
    return tag.asVec(width)
  }
  def parseFromVec(vec: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.tag := res.tag.parseFromVec(vec)
    return res.asInstanceOf[this.type]
  }

  def messageType: UInt = QEMUMessagesType.sPageEvict

  override def cloneType: this.type = {
    return new QEMUPageEvictRequest(param).asInstanceOf[this.type]
  }
}


class QEMUMissReply(param: TLBParameter) extends MessageUnionSubtype(new QEMURxMessage) {
  val tag = new PTTagPacket(param)
  val permission = UInt(param.permissionWidth.W)

  val tid = UInt(log2Ceil(param.threadNumber).W)
  val tid_v = Bool()

  val synonym_v = Bool()
  val synonym_tag = new PTTagPacket(param)

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(
      tag.asVec(width) ++
      Seq(
        permission,
        Mux(tid_v, tid, -1.S(32.W).asUInt()),
        synonym_v
      ) ++ 
      synonym_tag.asVec(width)
    )
  }

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.tag := res.tag.parseFromVec(VecInit(f.slice(0, 3)))
    res.permission := f(3)
    res.tid := f(4)
    res.tid_v := f(4) =/= -1.S(32.W).asUInt()
    res.synonym_v := f(5)
    res.synonym_tag := res.synonym_tag.parseFromVec(VecInit(f.slice(6, 9)))
    return res.asInstanceOf[this.type]
  }

  def messageType: UInt = QEMUMessagesType.sMissReply

  override def cloneType: this.type = {
    return new QEMUMissReply(param).asInstanceOf[this.type]
  }

}

class QEMUEvictReply(param: TLBParameter) extends MessageUnionSubtype(new QEMURxMessage) {
  val tag = new PTTagPacket(param)
  val old_ppn = UInt(24.W)
  val synonym_v = Bool()
  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(
      tag.asVec(width).toSeq ++
      Seq(
        old_ppn,
        synonym_v
      )
    )
  }
  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    //res.vpn := Cat(f(1), f(0))
    //res.process_id := f(2)
    res.tag := res.tag.parseFromVec(VecInit(f.slice(0, 3)))
    res.old_ppn := f(3)
    res.synonym_v := f(4)
    return res.asInstanceOf[this.type]
  }

  override def cloneType: this.type = {
    return new QEMUEvictReply(param).asInstanceOf[this.type]
  }

  def messageType: UInt = QEMUMessagesType.sEvictReply
}


class PageFaultNotification(param: TLBParameter) extends MessageUnionSubtype(new QEMUTxMessage) {
  val tag = new PTTagPacket(param)
  val permission = UInt(param.permissionWidth.W)
  val tid = UInt(log2Ceil(param.threadNumber).W)

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(tag.asVec(width).toSeq ++ Seq(permission, tid))
  }

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.tag := tag.parseFromVec(VecInit(f.slice(0, 3)))
    res.permission := f(3)
    res.tid := f(4)
    return res.asInstanceOf[this.type]
  }

  def messageType: UInt = QEMUMessagesType.sPageFaultNotify

  override def cloneType: this.type = {
    return new PageFaultNotification(param).asInstanceOf[this.type]
  }
}

class PageEvictNotification(message_type: UInt, param: TLBParameter) extends MessageUnionSubtype(new QEMUTxMessage) {
  val item = new PageTableItem(param)

  def asVec(width: Int): Vec[UInt] = item.asVec(width)

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.item := item.parseFromVec(f)
    return res.asInstanceOf[this.type]
  }

  override def cloneType: this.type = {
    return new PageEvictNotification(message_type, param).asInstanceOf[this.type]
  }

  def messageType: UInt = message_type
}
