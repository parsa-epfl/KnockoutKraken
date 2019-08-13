package utils

import scala.collection.mutable.LinkedHashMap

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

  case class PState (
    val xregs : List[Long],
    val pc : Long,
    val nzcv : Int
  )

  case class FInst (
    val tag : Int,
    val inst : Int
  ) {
    override def toString() = {
      val str = Seq(
        "FInst",
        Seq(
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
     val inst_en      : BigInt
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
    val tag     : BigInt,
    val nzcv    : BigInt,
    val nzcv_en : BigInt
  ) {
    override def toString() = {
      val str = Seq(
        "DInst",
        Seq(
          "tag: " + tag.toString,
          get_rd_res(rd: BigInt, res : BigInt, rd_en : BigInt),
          get_nzcv(nzcv: BigInt, nzcv_en: BigInt)
        ).map(s => " |-- " + s).mkString("\n")
      ).mkString("\n")
      str
    }
  }

  case class BInst (
    val tag    : BigInt,
    val offset : BigInt
  ) {
    override def toString() = {
      val str = Seq(
        "DInst",
        Seq(
          "tag    : " + tag.toString,
          "offset : "+ offset.toString()
        ).map(s => " |-- " + s).mkString("\n")
      ).mkString("\n")
      str
    }
  }

  def dinst(map : LinkedHashMap[String, BigInt]) : DInst = {
    val rd          = map("rd")
    val rs1         = map("rs1")
    val rs2         = map("rs2")
    val imm         = map("imm")
    val shift_val   = map("shift_val")
    val shift_type  = map("shift_type")
    val cond        = map("cond")
    val itype       = map("itype")
    val op          = map("op")
    val rd_en       = map("rd_en")
    val rs1_en      = map("rs1_en")
    val rs2_en      = map("rs2_en")
    val imm_en      = map("imm_en")
    val shift_en    = map("shift_en")
    val cond_en     = map("cond_en")
    val nzcv_en     = map("nzcv_en")
    val inst_en     = map("inst_en")
    val tag         = map("tag")
    new DInst(
      tag : BigInt,
      itype: BigInt,
      op: BigInt,
      rd: BigInt,
      rs1: BigInt,
      rs2: BigInt,
      imm: BigInt,
      shift_val: BigInt,
      shift_type: BigInt,
      cond: BigInt,
      rd_en: BigInt,
      rs1_en: BigInt,
      rs2_en: BigInt,
      imm_en: BigInt,
      shift_en: BigInt,
      cond_en: BigInt,
      nzcv_en: BigInt,
      inst_en: BigInt)
  }

  def einst(map : LinkedHashMap[String, BigInt]) : EInst = {
    val res       = map("res")
    val rd        = map("rd")
    val rd_en     = map("rd_en")
    val tag       = map("tag")
    val nzcv      = map("nzcv")
    val nzcv_en   = map("nzcv_en")
    new EInst (
      res     : BigInt,
      rd      : BigInt,
      rd_en   : BigInt,
      tag     : BigInt,
      nzcv    : BigInt,
      nzcv_en : BigInt
    )
  }

  def binst(map : LinkedHashMap[String, BigInt]) : BInst = {
    val tag       = map("tag")
    val offset    = map("offset")
    new BInst (
      tag     : BigInt,
      offset  : BigInt,
      )
  }
}
