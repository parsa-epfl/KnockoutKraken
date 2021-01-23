package armflex.demander.software_bundle

import chisel3._
import chisel3.util._
import armflex.demander.peripheral.SoftwareControlledBundle


/**
 * This file records all the hardware structures defined in the demander.cc 
 */ 

object ParameterConstants {
  val vpn_width = 52
  val ppn_width = 24
  val thread_id_width = 2
  val process_id_width = 16
  val permission_bit_width = 2
}

class TLBTag extends SoftwareControlledBundle {
  val vpn = UInt(ParameterConstants.vpn_width.W)
  val thread_id = UInt(ParameterConstants.thread_id_width.W)

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(Seq(
      vpn(31, 0),
      vpn(ParameterConstants.vpn_width-1, 32),
      thread_id
    ))
  }

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.vpn := Cat(f(1), f(0))
    res.thread_id := f(2)
    return res.asInstanceOf[this.type]
  }
}

class PTTag extends SoftwareControlledBundle {
  val vpn = UInt(ParameterConstants.vpn_width.W)
  val process_id = UInt(ParameterConstants.process_id_width.W)

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(Seq(
      vpn(31, 0),
      vpn(ParameterConstants.vpn_width-1, 32),
      process_id
    ))
  }

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.vpn := Cat(f(1), f(0))
    res.process_id := f(2)
    return res.asInstanceOf[this.type]
  }
}

class PTEntry extends SoftwareControlledBundle {
  val ppn = UInt(ParameterConstants.ppn_width.W)
  val permission = UInt(ParameterConstants.permission_bit_width.W)
  val modified = Bool()

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
}

class PageTableItem extends SoftwareControlledBundle {
  val tag = new PTTag     // 3
  val entry = new PTEntry // 3

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
}

abstract class RawMessage extends SoftwareControlledBundle {
  val message_type: UInt
  val data: Vec[UInt]
}

object PageDemanderMessageType {
  val sTLBMissRequest = 0.U(2.W)
  val sTLBEvict = 1.U(2.W)
  val sQEMUMissReply = 2.U(2.W)
  val sQEMUEvictReply = 3.U(2.W)
}

class PageDemanderMessage extends RawMessage {
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

object QEMUMessagesType {
  val sMissRequest = 4.U(3.W)
  val sEvictNotify = 5.U(3.W)
  val sEvictDone = 6.U(3.W)

  val sMissReply = 2.U(3.W)
  val sEvictReply = 3.U(3.W)
}

class QEMUMessage extends RawMessage {
  val message_type = UInt(3.W)
  val data = Vec(8, UInt(32.W))

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(Seq(message_type) ++ data.toSeq)
  }
  
  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.message_type := f(0)
    res.message_type := VecInit(f.slice(1, 9))
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

class TLBMissRequestMessage extends MessageUnionSubtype(new PageDemanderMessage) {
  val tag = new TLBTag
  val permission = UInt(ParameterConstants.permission_bit_width.W)

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(Seq(
      tag.vpn(31, 0),
      tag.vpn(tag.vpn.getWidth-1, 32),
      tag.thread_id,
      permission
    ))
  }

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.tag := res.tag.parseFromVec(VecInit(f.slice(0,3)))
    res.permission := f(3)
    return res.asInstanceOf[this.type]
  }

  def messageType = PageDemanderMessageType.sTLBMissRequest
}

class TLBEvictionMessage extends MessageUnionSubtype(new PageDemanderMessage) {

  val tag = new TLBTag
  val entry = new PTEntry

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(
      tag.asVec(width).toSeq ++
      entry.asVec(width).toSeq
    )
  }
  
  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.tag := tag.parseFromVec(VecInit(f.slice(0, 3)))
    res.entry := entry.parseFromVec(VecInit(f.slice(3, 5)))
    return res.asInstanceOf[this.type]
  }

  def messageType: UInt = PageDemanderMessageType.sTLBEvict
}


class QEMUMissReply extends MessageUnionSubtype(new PageDemanderMessage) {

  val tag = new PTTag
  val permission = UInt(ParameterConstants.permission_bit_width.W)

  val synonym_v = Bool()
  val synonym_tag = new PTTag

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(
      tag.asVec(width).toSeq ++ 
      Seq(
        permission,
        synonym_v
      ) ++ 
      synonym_tag.asVec(width).toSeq
    )
  }

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.tag := res.tag.parseFromVec(VecInit(f.slice(0, 3)))
    res.permission := f(3)
    res.synonym_v := f(4)
    res.synonym_tag := res.synonym_tag.parseFromVec(VecInit(f.slice(5, 8)))
    return res.asInstanceOf[this.type]
  }

  def messageType: UInt = PageDemanderMessageType.sQEMUMissReply

}

class QEMUEvictReply extends MessageUnionSubtype(new PageDemanderMessage) {

  val tag = new PTTag
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

  def messageType: UInt = PageDemanderMessageType.sQEMUEvictReply
}


class PageFaultRequest extends MessageUnionSubtype(new QEMUMessage) {
  val tag = new PTTag
  val permission = Bool()

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(tag.asVec(width).toSeq ++ Seq(permission))
  }

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.tag := tag.parseFromVec(VecInit(f.slice(0, 3)))
    res.permission := f(3)
    return res.asInstanceOf[this.type]
  }

  def messageType: UInt = QEMUMessagesType.sMissRequest
}

class PageEvictRequest(message_type: UInt) extends MessageUnionSubtype(new QEMUMessage) {
  val pte = new PTEntry

  def asVec(width: Int): Vec[UInt] = pte.asVec(width)

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.pte := pte.parseFromVec(f)
    return res.asInstanceOf[this.type]
  }

  override def cloneType: this.type = {
    return new PageEvictRequest(message_type).asInstanceOf[this.type]
  }

  def messageType: UInt = message_type
}

