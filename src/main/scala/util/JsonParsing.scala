package utils

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write
import scala.language.implicitConversions

import SoftwareStructs._

object ArmflexJson {
  implicit val formats = DefaultFormats

  private case class JSONCmd(val cmd: String, val addr: String)
  private case class JSONPState(val xregs : List[String], val pc : String, val sp : String, val nzcv : String)

  private def jp2sp(jp: JSONPState): PState = {
    PState(
      jp.xregs.map(BigInt(_,16).toLong),
      BigInt(jp.pc, 16).toLong,
      BigInt(jp.sp, 16).toLong,
      BigInt(jp.nzcv, 16).toInt
    )
  }

  private def sp2jp(sp: PState): JSONPState = {
    JSONPState(
      sp.xregs.map(r => f"${r}%016x"),
      f"${sp.pc}%016x",
      f"${sp.sp}%016x",
      f"${sp.nzcv}%08x"
    )
  }

  def json2state(sjson : String): PState = {
    val json: JValue = parse(sjson)
    val jsonp = json.extract[JSONPState]
    jp2sp(jsonp)
  }

  def state2json(pstate : PState): String = {
    implicit val formats = Serialization.formats(NoTypeHints)
    write(sp2jp(pstate))
  }

  def cmd2json(cmd : (Int, Long)): String = {
    val json = new JSONCmd(cmd._1.toString, f"${cmd._2}%016x")
    write(json)
  }

  def json2cmd(sjson: String): (Int, Long) = {
    val json: JValue = parse(sjson)
    val jsonp = json.extract[JSONCmd]
    val cmd = jsonp.cmd.toInt
    val addr = BigInt(jsonp.addr, 16).toLong
    (cmd, addr)
  }
}
