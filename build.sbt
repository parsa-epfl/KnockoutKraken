// See README.md for license details.  
ThisBuild / scalaVersion     := "2.12.13"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "parsa.epfl.ch"

Compile / PB.targets:= Seq(scalapb.gen() -> (sourceManaged in Compile).value / "scalapb")

lazy val root = (project in file("."))
  .settings(
    name := "armflex",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.4.3",
      "edu.berkeley.cs" %% "chiseltest" % "0.3.3" % "test",
      "com.github.tototoshi" %% "scala-csv" % "1.3.6"
    ),
    scalacOptions ++= Seq(
      "-Xsource:2.11",
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-P:chiselplugin:useBundlePlugin"
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.4.3" cross CrossVersion.full),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

  )



Test / fork := true
testOptions in Test += Tests.Argument("-oF")
javaOptions += "-Xmx32768m"
javaOptions += "-Xss128m"
