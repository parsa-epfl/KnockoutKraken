import mill._, scalalib._
import $ivy.`com.lihaoyi::mill-contrib-scalapblib:`
import $ivy.`com.lihaoyi::mill-contrib-bloop:`
import contrib.scalapblib._

object Devteroflex extends ScalaPBModule {
  def scalaVersion = "2.13.8"

  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"edu.berkeley.cs::chisel3:3.5.4"
  )

  override def scalacOptions = Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
  )

  override def scalacPluginIvyDeps = Agg(
    ivy"edu.berkeley.cs:::chisel3-plugin:3.5.4",
  )

  object test extends Tests {
    override def ivyDeps = Agg(
      ivy"edu.berkeley.cs::chiseltest:0.5.4"
    )
    override def testFramework = "org.scalatest.tools.Framework"
  }

  override def scalaPBVersion = "0.11.8"

  def exportVerilog() = T.command {
    println("Generating Verilog...")
    super.runMain("ARMFlexTopDebugVerilogEmitter")
  }

  def exportFPGAVerilog() = T.command {
    printf("Generate Verilog for FPGA...")
    super.runMain("ARMFlexTopVerilogFPGAEmitter")
  }
}
