package utils

import scala.collection.mutable.LinkedHashMap

import chisel3._

import common.DEC_LITS._
import utils.PrintingTools._

object PrintingTools {
  def get_itype(itype : BigInt) : String = {
    val str = itype.toInt match {
      case I_X     => "I_X    "
      case I_BImm  => "I_BImm "
      case I_BCImm => "I_BCImm"
      case I_LogSR => "I_LogSR"
      case I_LSImm => "I_LSImm"
      case I_ASImm => "I_ASImm"
    }
    "itype".padTo(8, ' ') + ": " + str
  }
  def get_op(itype : BigInt, op : BigInt ) : String = {
    val str = itype.toInt match {
      case I_X     =>  "X"
      case I_BImm | I_BCImm =>  op.toInt match {
        case OP_B => "B"
        case OP_BCOND => "B.cond"
      }
      case I_LogSR => op.toInt match {
        case OP_AND => "AND"
        case OP_BIC => "BIC"
        case OP_ORR => "ORR"
        case OP_ORN => "ORN"
        case OP_EOR => "EOR"
        case OP_EON => "EON"
        case OP_ADD => "ADD"
        case OP_SUB => "SUB"
      }

      case I_LSImm => op.toInt match {
        case OP_LDR => "LDR"
      }

      case I_ASImm => op.toInt match {
        case OP_ADD => "ADD"
        case OP_SUB => "SUB"
      }
    }
    "op".padTo(8, ' ') + ": " + str
  }

  def getReg(r : BigInt) : String = {
    val str = "X" + r.toInt.toString
    str.padTo(3, ' ')
  }

  def get_imm(imm : BigInt, imm_en: BigInt) : String = {
    imm_en.toInt match{
      case Y => "IMM".padTo(8, ' ') + ": "+imm.toInt.toString
      case N => "IMM".padTo(8, ' ') + ": "+"XXX"
    }
  }

  def getReg_op(rd : BigInt, rs1 : BigInt, rs2 : BigInt, rd_en : BigInt, rs1_en : BigInt, rs2_en : BigInt) : String = {
    (rd_en.toInt, rs1_en.toInt, rs2_en.toInt) match {
      case (0,0,0) => "XXX" + " <- " + "XXX" + "," + "XXX"
      case (1,0,0) => getReg(rd) + " <- "
      case (0,1,0) => "XXX" + " <- " + getReg(rs1)
      case (1,1,0) => getReg(rd) + " <- " + getReg(rs1)
      case (1,1,1) => getReg(rd) + " <- " + getReg(rs1) + "," + getReg(rs2)
      case _ => "? <-   ?,   ?"
    }
  }

  def get_rd_res(rd : BigInt, res : BigInt, rd_en : BigInt) : String = {
    rd_en.toInt match {
      case Y => getReg(rd) + " <- " + res.toInt
      case N => "" // "XXX" + " <- " + "XXX"
    }
  }

  def get_nzcv(nzcv : BigInt, nzcv_en : BigInt) : String = {
    nzcv_en.toInt match {
      case Y => "nzcv" + " <- " + nzcv.toInt
      case N => "nzcv" + " <- " + "XXX"
    }
  }

  def get_v(b : BigInt) : String = {
    val str = b.toInt match {
      case Y => "Y"
      case N => "N"
    }
    str
  }

  def get_shift(shift : BigInt, shift_en : BigInt, shift_val : BigInt) : String = {
    val str = shift.toInt match {
      case LSL => "LSL"
      case LSR => "LSR"
      case ASR => "ASR"
      case ROR => "ROR"
    }
    shift_en.toInt match {
      case Y => "shift".padTo(8, ' ') + ": " + str + "#" + shift_val.toString
      case N => "shift".padTo(8, ' ') + ": " + "XXX"
    }
  }

  def get_cond(cond : BigInt, cond_en : BigInt) : String = {
    val str = cond.toInt match {
      case EQ => "EQ = 000 0"
      case NE => "NE = 000 1"
      case CS => "CS = 001 0"
      case HS => "HS = 001 0"
      case CC => "CC = 001 1"
      case LO => "LO = 001 1"
      case MI => "MI = 010 0"
      case PL => "PL = 010 1"
      case VS => "VS = 011 0"
      case VC => "VC = 011 1"
      case HI => "HI = 100 0"
      case LS => "LS = 100 1"
      case GE => "GE = 101 0"
      case LT => "LT = 101 1"
      case GT => "GT = 110 0"
      case LE => "LE = 110 1"
      case AL => "AL = 111 0"
      case NV => "NV = 111 1"
    }

    cond_en.toInt match {
      case Y => "cond".padTo(8, ' ') + ": " + str
      case N => "cond".padTo(8, ' ') + ": " + "XXX"
    }
  }
  def get_nzcv_is_update(nzcv_en : BigInt) : String = {
    nzcv_en.toInt match {
      case Y => "nzcv".padTo(8, ' ') + ": " + "Y"
      case N => "nzcv".padTo(8, ' ') + ": " + "N"
    }
  }
}


object SoftwareStructs {
  import scala.language.implicitConversions

  implicit def uint2bigint(bits: UInt): BigInt = { bits.litValue }
  implicit def bool2bigint(bits: Bool): BigInt = { bits.litValue }
  implicit def bool2boolean(bits: Bool): Boolean = {bits.litToBoolean }

  implicit def bigint2short(bigint: BigInt): Short = { bigint.toShort }
  implicit def bigint2int(bigint: BigInt):   Int   = { bigint.toInt }
  implicit def bigint2long(bigint: BigInt):  Long  = { bigint.toLong }

  implicit def uint2short(bits: UInt): Short = { bigint2short(uint2bigint(bits)) }
  implicit def uint2int(bits: UInt):   Int   = { bigint2int(uint2bigint(bits)) }
  implicit def uint2long(bits: UInt):  Long  = { bigint2long(uint2bigint(bits)) }

  implicit def bool2short(bits: Bool): Short = { bigint2short(bool2bigint(bits)) }
  implicit def bool2int(bits: Bool):   Int   = { bigint2int(bool2bigint(bits)) }
  implicit def bool2long(bits: Bool):  Long  = { bigint2long(bool2bigint(bits)) }

  case class PState (
    val xregs : List[Long],
    val pc : Long,
    val nzcv : Int
  ) {
    override def toString() = {
      val name = s"PROC STATE:"
      val regs = xregs.zipWithIndex.map {case (reg, i) => s"${PrintingTools.getReg(i)}:${"%016x".format(reg)}"}.mkString("\n")
      val str = Seq(
        name,
        regs,
        "PC :" + "%016x".format(pc),
        "SP :" + 0,
        "EL :" + 0,
        "NZCV" + ":" + nzcv.toBinaryString
      ).mkString("\n")
      str + "\n"
    }

    def matches(other: PState): (Boolean, String) = {
      var str: String = ""
      val diffXRegs = (this.xregs zip other.xregs).zipWithIndex.filter {
        case ((t,o), i) => t != o
      }
      if(!diffXRegs.isEmpty || this.pc != other.pc || this.nzcv != other.nzcv) {
        str = str ++ "PState didn't match, differences:\n"
        diffXRegs foreach {
          case ((t, o), i) =>
            str = str ++ s"${PrintingTools.getReg(i)}:${t} != ${o}\n"
        }
        if(this.pc != other.pc)
          str = str ++ s"PC:${"%016x".format(this.pc)} != ${"%016x".format(other.pc)}\n"
        if(this.nzcv != other.nzcv)
          str = str ++ s"NZCV:${this.nzcv.toBinaryString} != ${other.nzcv.toBinaryString}\n"

        return (false, str)
      } else {

        str = str ++ "PState matched.\n"
        return (true, str)
      }
    }
  }

  case class FInst (
    val pc : Long,
    val tag : Int,
    val inst : Int
  ) {
    override def toString() = {
      val str = Seq(
        "FInst",
        Seq(
          "pc:   " + pc.toString,
          "tag:  " + tag.toString,
          "insn: " + inst
        )
      ).mkString("\n")
      str + "\n"
    }
  }

  case class DInst (
     val tag          : BigInt,
     val itype        : BigInt,
     val op           : BigInt,
     val rd           : BigInt,
     val rs1          : BigInt,
     val rs2          : BigInt,
     val imm          : BigInt,
     val shift_val    : BigInt,
     val shift_type   : BigInt,
     val cond         : BigInt,
     val rd_en        : BigInt,
     val rs1_en       : BigInt,
     val rs2_en       : BigInt,
     val imm_en       : BigInt,
     val shift_en     : BigInt,
     val cond_en      : BigInt,
     val nzcv_en      : BigInt,
     val inst_en      : BigInt,
     val pc : BigInt = 0
  ) {
    override def toString() = {
      val str = Seq(
        "DInst",
        Seq(
          "tag: " + tag.toString,
          get_itype(itype: BigInt),
          get_op(itype: BigInt, op: BigInt),
          getReg_op(rd: BigInt, rs1: BigInt, rs2: BigInt, rd_en: BigInt, rs1_en: BigInt, rs2_en: BigInt),
          get_imm(imm: BigInt, imm_en: BigInt),
          get_shift(shift_type, shift_en, shift_val),
          get_cond(cond: BigInt, cond_en : BigInt),
          get_nzcv_is_update(nzcv_en),
          ).map(s => " |-- " + s).mkString("\n")
      ).mkString("\n")
      str+"\n"
    }
  }

  case class EInst (
    val res     : BigInt,
    val rd      : BigInt,
    val rd_en   : BigInt,
    val nzcv    : BigInt,
    val nzcv_en : BigInt
  ) {
    override def toString() = {
      val str = Seq(
        "DInst",
        Seq(
          get_rd_res(rd: BigInt, res : BigInt, rd_en : BigInt),
          get_nzcv(nzcv: BigInt, nzcv_en: BigInt)
        ).map(s => " |-- " + s).mkString("\n")
      ).mkString("\n")
      str
    }
  }

  case class BInst (
    val offset : BigInt
  ) {
    override def toString() = {
      val str = Seq(
        "DInst",
        Seq(
          "offset : "+ offset.toString()
        ).map(s => " |-- " + s).mkString("\n")
      ).mkString("\n")
      str
    }
  }

  def finst(bundle : protoflex.FInst) : FInst = {
    new FInst (
      bundle.pc : Long,
      bundle.tag : Int,
      bundle.inst : Int
      )
  }

  def dinst(bundle: protoflex.DInst) : DInst = {
    new DInst(
      bundle.tag : BigInt,
      bundle.itype : BigInt,
      bundle.op : BigInt,
      bundle.rd.bits : BigInt,
      bundle.rs1.bits : BigInt,
      bundle.rs2.bits : BigInt,
      bundle.imm.bits : BigInt,
      bundle.shift_val.bits : BigInt,
      bundle.shift_type : BigInt,
      bundle.cond.bits : BigInt,
      bundle.rd.valid : BigInt,
      bundle.rs1.valid : BigInt,
      bundle.rs2.valid : BigInt,
      bundle.imm.valid : BigInt,
      bundle.shift_val.valid : BigInt,
      bundle.cond.valid : BigInt,
      bundle.nzcv.valid : BigInt,
      bundle.inst32.bits : BigInt,
      bundle.pc : BigInt)
  }

  def einst(bundle: protoflex.EInst) : EInst = {
    new EInst (
      bundle.res     : BigInt,
      bundle.rd.bits : BigInt,
      bundle.rd.valid : BigInt,
      bundle.nzcv.bits : BigInt,
      bundle.nzcv.valid : BigInt
    )
  }

  def binst(bundle : protoflex.BInst) : BInst = {
    new BInst (
      bundle.offset.litValue  : BigInt,
      )
  }
}
