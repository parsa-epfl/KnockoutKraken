package utils

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write
import scala.language.implicitConversions

object ArmflexJson {
  private case class JSONPState(val xregs : List[String], val pc : String, val nzcv : String)

  private def jp2sp(jp: JSONPState) : SoftwareStructs.PState = {
    SoftwareStructs.PState(
      jp.xregs.map(BigInt(_,16).toLong),
      BigInt(jp.pc, 16).toLong,
      BigInt(jp.nzcv, 16).toInt
    )
  }

  private def sp2jp(sp: SoftwareStructs.PState) : JSONPState = {
    JSONPState(
      sp.xregs.map(_.toHexString.padTo(16, '0')),
      sp.pc.toHexString.padTo(16, '0'),
      sp.nzcv.toHexString.padTo(8, '0')
    )
  }

  def json2state(sjson : String) : SoftwareStructs.PState = {
    implicit val formats = DefaultFormats
    val json: JValue = parse(sjson)
    val jsonp = json.extract[JSONPState]
    jp2sp(jsonp)
  }

  def state2json(pstate : SoftwareStructs.PState):String= {
    implicit val formats = Serialization.formats(NoTypeHints)
    write(sp2jp(pstate))
  }
}
