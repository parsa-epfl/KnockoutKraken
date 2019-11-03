package utils

import scala.io.Source
import scala.util.matching.Regex

import common.DEC_LITS._

case class AssemblyInstruction
  (
    val line        : String,
    val itype       : Int,
    val op          : Int,
    val rd          : Int,
    val rs1         : Int,
    val rs2         : Int,
    val imm         : Int,
    val shift_type  : Int,
    val cond        : Int,
    val rd_en       : Int,
    val rs1_en      : Int,
    val rs2_en      : Int,
    val imm_en      : Int,
    val shift_en    : Int,
    val cond_en     : Int,
    val nzcv_en     : Int,
    val inst_en     : Int,
    val bitPat      : Int
  )
{
  val io = Seq(
    itype,
    op,
    rd,
    rs1,
    rs2,
    imm,
    shift_type,
    cond,
    rd_en,
    rs1_en,
    rs2_en,
    imm_en,
    shift_en,
    cond_en,
    nzcv_en,
    inst_en
  )
}

/** object AssemblyInstruction
  * Creates an decoded instruction from the AssemblyParser information
  *
  */
object AssemblyInstruction
{
  // Instruction types
  def LogSR = Map(
    "AND" -> OP_AND,
    "BIC" -> OP_BIC,
    "ORR" -> OP_ORR,
    "ORN" -> OP_ORN,
    "EOR" -> OP_EOR,
    "EON" -> OP_EON,
    "ANDS" -> OP_AND,
    "BICS" -> OP_BIC
  )

  def AddSubImm = Map (
    "ADD" -> OP_ADD,
    "ADDS" -> OP_ADD,
    "SUB" -> OP_SUB,
    "SUBS" -> OP_SUB
  )

  def Branch = Map (
    "B" -> OP_BCOND
  )

  def LoadStore = Map(
    "LDR" -> OP_LDR
  )

  def ShiftTypes = Map(
  "LSL" -> LSL,
  "LSR" -> LSR,
  "ASR" -> ASR,
  "ROR" -> ROR
  )

  def CondTypes = Map (
    "EQ" -> EQ,
    "NE" -> NE,
    "CS" -> CS,
    "HS" -> HS,
    "CC" -> CC,
    "LO" -> LO,
    "MI" -> MI,
    "PL" -> PL,
    "VS" -> VS,
    "VC" -> VC,
    "HI" -> HI,
    "LS" -> LS,
    "GE" -> GE,
    "LT" -> LT,
    "GT" -> GT,
    "LE" -> LE,
    "AL" -> AL,
    "NV" -> NV
  )

  def setCond = Seq("ANDS", "BICS")

  /**
    * Creates a simple instruction with registers enabled
    */
  def apply(rd:Int, rs1:Int, rs2 : Int) : AssemblyInstruction = {
    AssemblyInstruction(
      "ands": String,
      Some(rd): Option[Int],
      Some(rs1): Option[Int],
      Some(rs2): Option[Int],
      None: Option[Int],
      None: Option[String],
      None: Option[String],
      "00000000": String,
      "empty": String)
  }

  def apply( i_op     : String,
             i_rd     : Option[Int],
             i_rs1    : Option[Int],
             i_rs2    : Option[Int],
             i_imm    : Option[Int],
             i_shift  : Option[String],
             i_cond   : Option[String],
             i_bitPat : String,
             i_line   : String ) : AssemblyInstruction = {


    val line = i_line
    val i_bitPad_l = i_bitPat.grouped(2).toList.reverse.mkString // little endien
    val bitPat = Integer.parseInt(i_bitPad_l, 16)

    var itype = I_X
    var op    = OP_ALU_X
    var rd    = REG_X
    var rs1   = REG_X
    var rs2   = REG_X
    var imm   = IMM_X
    var shift_type = SHIFT_TYPE_X
    var cond  = COND_X

    (i_rd, i_rs1, i_rs2, i_imm, i_shift, i_cond) match {
      // Logical (shifted register)
      case (Some(d), Some(s1), Some(s2), _, _, _) if LogSR.get(i_op.toUpperCase).isDefined => {
        itype = I_LogSR

        op = LogSR.getOrElse(i_op.toUpperCase, OP_ALU_X)

        rd  = d
        rs1 = s1
        rs2 = s2
        imm = i_imm match { case Some(imm) => imm; case _ => IMM_X }

        // (shift_type)
        shift_type = i_shift match {
          case Some(s) => ShiftTypes.getOrElse(s.toUpperCase, SHIFT_TYPE_X)
          case None    => SHIFT_TYPE_X
        }
      }

      // Compare & branch (immediate)
      case (_, _, _, Some(i), _, Some(c)) if Branch.get(i_op.toUpperCase).isDefined => {
        itype = I_BCImm

        op = OP_BCOND
        rd  = i_rd match { case Some(d) => d; case _ => REG_X }
        imm = i
        cond = CondTypes.getOrElse(c.toUpperCase, COND_X)
      }

      // Branch ( immediate )
      case (_, _, _, Some(i), _, _) if Branch.get(i_op.toUpperCase).isDefined => {
        itype = I_BImm
        op = OP_B
        rd  = i_rd match { case Some(d) => d; case _ => REG_X }
        imm = i
      }
      // add/subdract ( immediate )
      case (Some(d), Some(s1), _,Some(i), _, _) if AddSubImm.get(i_op.toUpperCase).isDefined => {
        itype = I_ASImm
        op = AddSubImm.getOrElse(i_op.toUpperCase, OP_ALU_X)
        rd  = d
        rs1 = s1
        imm = i
        // (shift_type)
        shift_type = i_shift match {
          case Some(s) => ShiftTypes.getOrElse(s.toUpperCase, SHIFT_TYPE_X)
          case None    => SHIFT_TYPE_X
        }
      }

      // load store ( immediate )
      case (Some(d), _, _,Some(i), _, _) if LoadStore.get(i_op.toUpperCase).isDefined => {
        itype = I_LSImm
        op = OP_LDR
        rd  = d
        imm = i
      }

      case _ =>
    }

    val ctrl = decode_table(itype.toInt)
    val rd_en    = ctrl(0)
    val rs1_en   = ctrl(1)
    val rs2_en   = ctrl(2)
    val imm_en   = ctrl(3)
    val shift_en = ctrl(4)
    val cond_en  = ctrl(5)
    val nzcv_en  = if(setCond.contains(i_op.toUpperCase)) Y else ctrl(6)
    val inst_en  = ctrl(7)

    new AssemblyInstruction(
      line,
      itype,
      op,
      rd,
      rs1,
      rs2,
      imm,
      shift_type,
      cond,
      rd_en,
      rs1_en,
      rs2_en,
      imm_en,
      shift_en,
      cond_en,
      nzcv_en,
      inst_en,
      bitPat
    )
  }
}

object AssemblyParser
{
  /*
   Regex 3 (immediate ops) :
   Group information :
   group:      1    2        3   8   12  14   16
    b:  52800000    mov     w0, #40
   */

  /*
   Regex 1 (multireg ops) :
   [a,b,c,d,e0-9]{1,}[?:]\s*([0-9a-z]{8,8})\s*([a-z]*)\s*[wrx]([0-9]*)(,\s*\[|,\s*|.*)((sp|pc)|#([0-9]*)|[wrx]([0-9]*))(,\s*|.*)(#([0-9]*)|[wrx]([0-9]*))(,\s*|.*)(lsl|lsr|asr|.*)(\s*#|)([0-9]*|)
   Group information :
   group:      1    2        3   8   12  14   16
   20:  0a803c20    and     w0, w1, w0,  asr #15
   */

  val grouVals_1 = Map(
    "bitPat"  -> Seq(1),
    "op"      -> Seq(2),
    "rd"      -> Seq(3),
    "sp"      -> Seq(6), // Or PC
    "imm"     -> Seq(7,11,16),
    "rs1"     -> Seq(8),
    "rs2"     -> Seq(12),
    "cond"    -> Seq(),
    "shift"   -> Seq(14)
  )

  val regex_str_1 = "[0-9a-fA-F]{1,}[?:]\\s*([0-9A-Fa-f]{8,8})\\s*([a-zA-Z]*)\\s*[wrxWRX]([0-9]*)(,\\s*\\[|,\\s*|.*)((sp|pc)|#([a-fA-F0-9]*)|[wrxWRX]([0-9]*)|.*)(,\\s*|.*)(#([a-fA-F0-9]*)|[wrxWRX]([0-9]*))(,\\s*|.*)(?i)(lsl|lsr|asr|.*)(\\s*#|)([a-fA-F0-9]*)"
  val regex_1     = new Regex(regex_str_1)
  /*

   Regex 2 (branches) :
   [a,b,c,d,e0-9]{1,}[?:]\s*([0-9a-z]{8,8})\s*([a-z]*)\s*([.]|\s*)(le|eq|ne|cs|cc|mi|pl|vs|vc|hi|ls|ge|lt|gt|al|nv|)\s*([a-z0-9]*).*<.*>.*
   Group information :
   group       1  2  5
   10:	14000013 	b	5c <main+0x5c>

   group       1  2  4   5
   64:	54fffd8d 	b.le	14 <main+0x14>

   */

  val regex_str_2 =
    "[a-fA-F0-9]{1,}[?:]\\s*([0-9a-zA-F]{8,8})\\s*([a-zA-Z]*)\\s*([.]|\\s*)(le|eq|ne|cs|cc|mi|pl|vs|vc|hi|ls|ge|lt|gt|al|nv|)\\s*([a-z0-9]*).*<.*>.*"

  val regex_2     = new Regex(regex_str_2)
  val grouVals_2 = Map(
    "bitPat"  -> Seq(1),
    "op"      -> Seq(2),
    "rd"      -> Seq(),
    "sp"      -> Seq(), // Or PC
    "imm"     -> Seq(5),
    "rs1"     -> Seq(),
    "rs2"     -> Seq(),
    "cond"    -> Seq(4),
    "shift"   -> Seq()
  )

  def parse(filename: String): Seq[AssemblyInstruction] = {

    val path = getClass.getResource("/" + filename).getPath
    val file = Source.fromFile(path)

    var insts : List[AssemblyInstruction] = List()

    for ( line <- file.getLines ) {
      regex_1.findAllMatchIn(line) foreach {
        matches =>
        val op     = grouVals_1("op")     map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => s; case _ => ""}
        val rd     = grouVals_1("rd")     map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
        val rs1    = grouVals_1("rs1")    map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
        val rs2    = grouVals_1("rs2")    map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
        val imm    = grouVals_1("imm")    map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
        val cond   = grouVals_1("cond")   map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s); case _ => None}
        val shift  = grouVals_1("shift")  map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s); case _ => None}
        val bitPat = grouVals_1("bitPat") map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => s; case _ => ""}

        insts = insts :+ AssemblyInstruction(op, rd, rs1, rs2, imm, shift, cond, bitPat, line)
      }
      //          case e : Exception => throw new Exception("Regex multi register op : matched wrong, non integer where expected integer (pos:" + file.pos + ") in line :\n\t" + line)
      regex_2.findAllMatchIn(line) foreach {
        matches =>
        val op = grouVals_2("op")     map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => s; case _ => ""}
        val rd = grouVals_2("rd")     map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
        val rs1 = None
        val rs2 = None
        val imm  = grouVals_2("imm") map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(BigInt(s,16).toInt); case _ => None}
        val cond = grouVals_2("cond") map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s); case _ => None}
        val shift  = None
        val bitPat = grouVals_2("bitPat") map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => s; case _ => ""}
        insts = insts :+ AssemblyInstruction(op.toUpperCase, rd, rs1, rs2, imm, shift, cond, bitPat, line)
      }
    }
    insts
  }
}
