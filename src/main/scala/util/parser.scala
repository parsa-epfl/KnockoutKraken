package utils

import scala.io.Source
import scala.util.matching.Regex

import common.DECODE_INTEGER_LITERALS._

class AssemblyInstruction
  (
    val line       : String,
    val itype      : BigInt,
    val aluOp      : BigInt,
    val rd         : BigInt,
    val rs1        : BigInt,
    val rs2        : BigInt,
    val imm        : BigInt,
    val shift      : BigInt,
    val rd_en      : BigInt,
    val rs1_en     : BigInt,
    val rs2_en     : BigInt,
    val imm_en     : BigInt,
    val shift_en   : BigInt,
    val inst_en    : BigInt,
    val bitPat     : BigInt
  )
{
  val io = Seq(
    itype,
    aluOp,
    rd,
    rs1,
    rs2,
    imm,
    shift,
    rd_en,
    rs1_en,
    rs2_en,
    imm_en,
    shift_en,
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
    "EON" -> OP_EON
  )

  def ShiftTypes = Map(
    "LSL" -> LSL,
    "LSR" -> LSR,
    "ASR" -> ASR,
    "ROR" -> ROR
  )

  /**
    * Creates a simple instruction with registers enabled
    */
  def apply(rd:Int, rs1:Int, rs2 : Int) : AssemblyInstruction = {
    AssemblyInstruction(
      "and": String,
      Some(rd): Option[Int],
      Some(rs1): Option[Int],
      Some(rs2): Option[Int],
      None: Option[Int],
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
             i_bitPat : String,
             i_line   : String ) : AssemblyInstruction = {


    val line = i_line
    val bitPat = BigInt(i_bitPat, 16)

    var itype = I_X
    var aluOp = OP_ALU_X
    var rd    = REG_X
    var rs1   = REG_X
    var rs2   = REG_X
    var imm   = IMM_X
    var shift = SHIFT_X

    var ctrl = decode_table(I_X)
    (i_rd, i_rs1, i_rs2) match {
      case (Some(d), Some(s1), Some(s2)) if LogSR.get(i_op.toUpperCase).isDefined =>
        itype = I_LogSR

        // (alu_op)
        aluOp = LogSR.getOrElse(i_op.toUpperCase, OP_ALU_X)

        // (reg_addr | imm_val)
        rd  = d
        rs1 = s1
        rs2 = s2
        imm = i_imm match { case Some(i) => i; case _ => 0 }


        // (shift_type)
        i_shift match {
          case Some(s) => shift = ShiftTypes.getOrElse(s.toUpperCase, SHIFT_X)
          case None    => shift = SHIFT_X
        }

        ctrl = decode_table(itype.toInt)
      case _ =>

    }
    val rd_en    = ctrl(0)
    val rs1_en   = ctrl(1)
    val rs2_en   = ctrl(2)
    val imm_en   = ctrl(3)
    val shift_en = ctrl(4)
    val inst_en  = ctrl(5)

    new AssemblyInstruction(
      line,
      itype,
      aluOp,
      rd,
      rs1,
      rs2,
      imm,
      shift,
      rd_en,
      rs1_en,
      rs2_en,
      imm_en,
      shift_en,
      inst_en,
      bitPat
    )

  }
}

object AssemblyParser
{
  /*
   Regex :
   [a,b,c,d,e0-9]{1,}[?:]\s*([0-9a-z]{8,8})\s*([a-z]*)\s*[wrx]([0-9]*)(,\s*\[|,\s*|.*)((sp|pc)|#([0-9]*)|[wrx]([0-9]*))(,\s*|.*)(#([0-9]*)|[wrx]([0-9]*))(,\s*|.*)(lsl|lsr|asr|.*)(\s*#|)([0-9]*|)
   Group information :

   group:      1    2        3   6    11
   0:   b90013e0    str     w0, [sp, #16]

   group:      1    2        3   7
   b:   52800000    mov     w1, #40

   group:      1    2        3   8   12  14   16
   20:  0a803c20    and     w0, w1, w0,  asr #15

   */
  val grouVals = Map(
    "bitPat"  -> Seq(1),
    "op"      -> Seq(2),
    "rd"      -> Seq(3),
    "sp"      -> Seq(6), // Or PC
    "imm"     -> Seq(7,11,16),
    "rs1"     -> Seq(8),
    "rs2"     -> Seq(12),
    "shift"   -> Seq(14)
  )
  val regex_str = "[a,b,c,d,e0-9]{1,}[?:]\\s*([0-9a-z]{8,8})\\s*([a-z]*)\\s*[wrx]([0-9]*)(,\\s*\\[|,\\s*|.*)((sp|pc)|#([0-9]*)|[wrx]([0-9]*))(,\\s*|.*)(#([0-9]*)|[wrx]([0-9]*))(,\\s*|.*)(lsl|lsr|asr|.*)(\\s*#|)([0-9]*|)"
  val regex     = new Regex(regex_str)

  def parse(filename: String): Seq[AssemblyInstruction] = {

    val path = getClass.getResource("/" + filename).getPath
    val file = Source.fromFile(path)

    var insts : List[AssemblyInstruction] = List()

    for ( line <- file.getLines() ) {
      regex.findAllMatchIn(line) foreach {
        matches =>
        try {
          val op     = grouVals("op")     map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => s; case _ => ""}
          val rd     = grouVals("rd")     map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
          val rs1    = grouVals("rs1")    map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
          val rs2    = grouVals("rs2")    map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
          val imm    = grouVals("imm")    map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
          val shift  = grouVals("shift")  map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s); case _ => None}
          val bitPat = grouVals("bitPat") map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => s; case _ => ""}

          insts = insts :+ AssemblyInstruction(op.toUpperCase, rd, rs1, rs2, imm, shift, bitPat, line)
        } catch {
          case e : Exception => throw new Exception("Regex matched wrong, non integer where expected integer (pos:" + file.pos + ") in line :\n\t" + line)
        }
      }
    }
    insts
  }
}
