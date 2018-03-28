package utils

import scala.io.Source
import scala.util.matching.Regex

import common.DECODE_INTEGER_LITERALS._

class AssemblyInstruction
{
  var line   = "": String

  var inst_type = I_X: BigInt
  var aluOp     = OP_ALU_X: BigInt
  var rd        = 0: BigInt
  var rs1       = 0: BigInt
  var rs2       = 0: BigInt
  var imm       = 0: BigInt
  var shift     = 0: BigInt
  var rd_en     = N: BigInt
  var rs1_en    = N: BigInt
  var rs2_en    = N: BigInt
  var imm_en    = N: BigInt
  var shift_en  = N: BigInt
  var inst_en   = N: BigInt

  val io = Seq(
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
  def apply( op     : String,
             rd     : Option[Int],
             rs1    : Option[Int],
             rs2    : Option[Int],
             imm    : Option[Int],
             shift  : Option[String],
             bitPat : String,
             line   : String ) : AssemblyInstruction = {

    var inst = new AssemblyInstruction
    val inst_ctrl: List[(BigInt)=>Unit] =
      List(inst.rd_en_=, inst.rs1_en_=, inst.rs2_en_=, inst.imm_en_=, inst.inst_en_=)
    val ctrl = decode_table(I_X)
    (inst_ctrl zip ctrl) map{ case (i_a, v) => i_a(v)}

    inst.line = line

    (op, rd, rs1, rs2, imm, shift) match {
      case (_, Some(s1), Some(s2), Some(d), Some(i), Some(shift)) =>
        // (reg_addr | imm_val)
        inst.rd  = d
        inst.rs1 = s1
        inst.rs2 = s2
        inst.imm = i


        // (shift_type)
        shift.toUpperCase match {
          case "LSL" => inst.shift_en = Y; inst.shift = LSL
          case "LSR" => inst.shift_en = Y; inst.shift = LSR
          case "ASR" => inst.shift_en = Y; inst.shift = ASR
          case "ROR" => inst.shift_en = Y; inst.shift = ROR
          case  _    => inst.shift_en = Y; inst.shift = SHIFT_X
        }

        // ( inst_type, alu_op)
        val i_op = op.toUpperCase match {
          case "AND" => inst.inst_type = I_LogSR; inst.aluOp = OP_AND
          case "BIC" => inst.inst_type = I_LogSR; inst.aluOp = OP_BIC
          case "ORR" => inst.inst_type = I_LogSR; inst.aluOp = OP_ORR
          case "ORN" => inst.inst_type = I_LogSR; inst.aluOp = OP_ORN
          case "EOR" => inst.inst_type = I_LogSR; inst.aluOp = OP_EOR
          case "EON" => inst.inst_type = I_LogSR; inst.aluOp = OP_EON
          case "ADD" => inst.inst_type = 0;       inst.aluOp = OP_ADD
          case "SUB" => inst.inst_type = 0;       inst.aluOp = OP_SUB
          case _     => inst.inst_type = 0;       inst.aluOp = OP_ALU_X
        }

        val ctrl = decode_table(inst.inst_type.toInt)
        (inst_ctrl zip ctrl) map{ case (i_a, v) => i_a(v)}
      case _ =>

    }
    inst
  }
}

object AssemblyParser
{
  /*
   Regex :
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
          val op     = grouVals("op")     map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => s; case _ => ""}
          val rd     = grouVals("rd")     map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
          val rs1    = grouVals("rs1")    map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
          val rs2    = grouVals("rs2")    map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
          val imm    = grouVals("imm")    map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s.toInt); case _ => None}
          val shift  = grouVals("shift")  map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => Some(s); case _ => None}
          val bitPat = grouVals("bitPat") map ( n => matches.group(n)) find( s => s != null && s != "") match { case Some(s) => s; case _ => ""}

          insts = AssemblyInstruction(op, rd, rs1, rs2, imm, shift, bitPat, line) :: insts
        } catch {
          case e : Exception => throw new Exception("Regex matched wrong, non integer where expected integer (pos:" + file.pos + ") in line :\n\t" + line)
        }
      }
    }
    insts
  }
}
