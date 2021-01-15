package armflex.demander.peripheral.SoftwareBundle

import chisel3._
import chisel3.util._
import armflex.demander.peripheral.SoftwareControlledBundle
import chisel3.experimental.Param


/**
 * This file records all the hardware structures defined in the demander.cc 
 */ 

object ParameterConstants {
  val vpn_width = 52
  val ppn_width = 24
  val thread_id_width = 2
  val process_id = 16
}

class PTEntry extends SoftwareControlledBundle {
  val ppn = UInt(ParameterConstants.ppn_width.W)
  val permission = Bool()
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
  val vpn = UInt(ParameterConstants.vpn_width.W)
  val p_id = UInt(ParameterConstants.process_id.W)
  val entry = (new PTEntry)

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(Seq(
      vpn(31, 0),
      vpn(ParameterConstants.vpn_width-1, 32),
      p_id
    ) ++ entry.asVec(width).toSeq)
  }

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.vpn := Cat(f(1), f(0))
    res.p_id := f(2)
    res.entry := res.entry.parseFromVec(VecInit(f.slice(3, f.length)))
    return res.asInstanceOf[this.type]
  }
}

class PageRequestMessage extends SoftwareControlledBundle {
  val vpn = UInt(ParameterConstants.vpn_width.W)
  val t_id = UInt(ParameterConstants.thread_id_width.W)
  val permission = Bool()

  def asVec(width: Int): Vec[UInt] = {
    assert(width == 32)
    return VecInit(Seq(
      vpn(31, 0),
      vpn(vpn.getWidth-1, 32),
      t_id,
      permission
    ))
  }

  def parseFromVec(f: Vec[UInt]): this.type = {
    val res = Wire(this.cloneType)
    res.vpn := Cat(f(1), f(0))
    res.t_id := f(2)
    res.permission := f(3)
    return res.asInstanceOf[this.type]
  }
}
