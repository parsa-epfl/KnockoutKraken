package armflex.demander.software_bundle

import chisel3._
import chisel3.util._
import armflex.cache._
import armflex.demander.peripheral.TLBMessageConverter

abstract class SoftwareControlledBundle extends Bundle {
  def asVec(width: Int): Vec[UInt]
  def parseFromVec(vec: Vec[UInt]): this.type
}

/**
 * This file records all the hardware structures defined in the demander.cc 
 */ 

object ParameterConstants {
  val vpn_width = 52
  val ppn_width = 24
  val process_id_width = 15
  val permission_bit_width = 2

  val dram_addr_width = 36
  val dram_data_width = 512

  def getPageTableAddressByVPN(vpn: UInt) = {
    // val res = Wire()
    val pageset_number = vpn(23, 4)
    Cat(pageset_number * 3.U(2.W), 0.U(6.W))
  }
}

class TLBTag(param: TLBParameter) extends TLBTagPacket(param) {
  
}

class PTTag extends SoftwareControlledBundle {
  val vpn = UInt(ParameterConstants.vpn_width.W) // 52
  val process_id = UInt(ParameterConstants.process_id_width.W) // 16

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

// FIXME: Make it compatible with TLBEntryPacket
class PTEntry extends SoftwareControlledBundle {
  val ppn = UInt(ParameterConstants.ppn_width.W) // 24
  val permission = UInt(ParameterConstants.permission_bit_width.W) // 2
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


class TLBMissRequestMessage(param: TLBParameter) extends Bundle {
  val tag = new TLBTag(param)
  val permission = UInt(ParameterConstants.permission_bit_width.W)

  override def cloneType: this.type = {
    return new TLBMissRequestMessage(param).asInstanceOf[this.type]
  }
}

class TLBEvictionMessage(param: TLBParameter) extends Bundle {
  val tag = new TLBTag(param)
  val entry = new PTEntry

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
  val data = Vec(8, UInt(32.W))

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(Seq(message_type) ++ data.toSeq)
  }
  
  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.message_type := f(0)
    res.data := VecInit(f.slice(1, 9))
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

class QEMUPageEvictRequest extends MessageUnionSubtype(new QEMURxMessage){
  val tag = new PTTag
  def asVec(width: Int): Vec[UInt] = {
    return tag.asVec(width)
  }
  def parseFromVec(vec: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.tag := res.tag.parseFromVec(vec)
    return res.asInstanceOf[this.type]
  }

  def messageType: UInt = QEMUMessagesType.sPageEvict
}


class QEMUMissReply extends MessageUnionSubtype(new QEMURxMessage) {
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

  def messageType: UInt = QEMUMessagesType.sMissReply

}

class QEMUEvictReply extends MessageUnionSubtype(new QEMURxMessage) {
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

  def messageType: UInt = QEMUMessagesType.sEvictReply
}


class PageFaultNotification extends MessageUnionSubtype(new QEMUTxMessage) {
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

  def messageType: UInt = QEMUMessagesType.sPageFaultNotify
}

class PageEvictNotification(message_type: UInt) extends MessageUnionSubtype(new QEMUTxMessage) {
  val item = new PageTableItem

  def asVec(width: Int): Vec[UInt] = item.asVec(width)

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.item := item.parseFromVec(f)
    return res.asInstanceOf[this.type]
  }

  override def cloneType: this.type = {
    return new PageEvictNotification(message_type).asInstanceOf[this.type]
  }

  def messageType: UInt = message_type
}
