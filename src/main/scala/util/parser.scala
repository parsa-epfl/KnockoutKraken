package utils

import scala.io.Source
import scala.util.matching.Regex

class AssemblyInstruction
  (
  val op     : String,
  val rd     : Option[Int],
  val rs1    : Option[Int],
  val rs2    : Option[Int],
  val imm    : Option[Int],
  val shift  : Option[String],
  val bitPat : String,
  val line   : String
  )
{
 val dinstvals : Seq[BigInt] =
  {
    val bitpat = BigInt(bitPat, 16)
    val vals = (op, rd, rs1, rs2, imm, shift) match {
      case (_, Some(s1), Some(s2), Some(d), Some(i), Some(shift)) =>
        // (reg_addr | imm_val)
        val i_rd   = Seq(d)
        val i_rs1  = Seq(s1)
        val i_rs2  = Seq(s2)
        val i_imm  = Seq(i)

        // (shift_type)
        val i_shift = shift.toUpperCase match {
          case "LSL" => Seq(0)
          case "LSR" => Seq(1)
          case "ASR" => Seq(2)
          case "ROR" => Seq(3)
          case  _    => Seq(0)
        }

        // ( inst_type, alu_op)
        val i_op = op.toUpperCase match {
          case "AND" => Seq(1, 0)
          case "BIC" => Seq(1, 1)
          case "ORR" => Seq(1, 2)
          case "ORN" => Seq(1, 3)
          case "EOR" => Seq(1, 4)
          case "EON" => Seq(1, 5)
          case "ADD" => Seq(2, 6)
          case "SUB" => Seq(2, 7)
          case _     => Seq(0, 0)
        }
        // (rd_en, rs1_en, rs2_en, imm_en, shift_en, inst_en)
        val control = Seq(1, 1, 1, 0, 1, 1)
        Seq(i_op, i_rd, i_rs1, i_rs2, i_imm, i_shift, control).flatten map (i => BigInt(i))

      case _ =>
        (0 to 12) map ( i => BigInt(0))
    }
    bitpat +: vals
  }
  val getSig : Map[String, BigInt] = Map(
    "bitpat"   -> dinstvals(0),
    "inst_type"-> dinstvals(1),
    "aluOp"    -> dinstvals(2),
    "rd"       -> dinstvals(3),
    "rs1"      -> dinstvals(4),
    "rs2"      -> dinstvals(5),
    "imm"      -> dinstvals(6),
    "shift"    -> dinstvals(7),
    "rd_en"    -> dinstvals(8),
    "rs1_en"   -> dinstvals(9),
    "rs2_en"   -> dinstvals(10),
    "imm_en"   -> dinstvals(11),
    "shift_en" -> dinstvals(12),
    "inst_en" -> dinstvals(13)
  )

}

object AssemblyParser
{
  /*
   [a,b,c,d,e0-9]{1,}[?:]\s*([0-9a-z]{8,8})\s*([a-z]*)\s*[wr]([0-9]*)(,\s*\[|,\s*|.*)((sp|pc)|#([0-9]*)|[wr]([0-9]*))(,\s*|.*)(#([0-9]*)|[wr]([0-9]*))(,\s*|.*)(lsl|lsr|asr|.*)(\s*#|)([0-9]*|)
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
  val regex_str = "[a,b,c,d,e0-9]{1,}[?:]\\s*([0-9a-z]{8,8})\\s*([a-z]*)\\s*[wr]([0-9]*)(,\\s*\\[|,\\s*|.*)((sp|pc)|#([0-9]*)|[wr]([0-9]*))(,\\s*|.*)(#([0-9]*)|[wr]([0-9]*))(,\\s*|.*)(lsl|lsr|asr|.*)(\\s*#|)([0-9]*|)"
  val regex     = new Regex(regex_str)

  def parse(filename: String): Seq[AssemblyInstruction] = {

    val path = getClass.getResource("/" + filename).getPath
    val file = Source.fromFile(path)

    var insts : List[AssemblyInstruction] = List()

    for ( line <- file.getLines() ) {
      regex.findAllMatchIn(line) foreach {
        matches =>
        try {
          val     op = grouVals("op")     map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => s; case _ => ""}
          val     rd = grouVals("rd")     map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
          val    rs1 = grouVals("rs1")    map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
          val    rs2 = grouVals("rs2")    map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
          val    imm = grouVals("imm")    map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
          val  shift = grouVals("shift")  map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s); case _ => None}
          val bitPat = grouVals("bitPat") map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => s; case _ => ""}

          insts = (new AssemblyInstruction(op, rd, rs1, rs2, imm, shift, bitPat, line)) :: insts
        } catch {
          case e : Exception => throw new Exception("Pattern matched wrong, non integer where expected integer (pos:" + file.pos + ") in line :\n\t" + line)
        }
      }
    }
    insts
  }
}
