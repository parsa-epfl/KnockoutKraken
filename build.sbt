// See README.md for license details.  
ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "parsa.epfl.ch"

Compile / PB.targets:= Seq(scalapb.gen() -> (sourceManaged in Compile).value / "scalapb")

lazy val root = (project in file("."))
  .settings(
    name := "armflex",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.5.0",
      "edu.berkeley.cs" %% "chiseltest" % "0.5.0-RC2" % "test",
      "com.github.tototoshi" %% "scala-csv" % "1.3.10"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit"
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.0" cross CrossVersion.full),
  )



Test / fork := true
testOptions in Test += Tests.Argument("-oF")
javaOptions += "-Xmx32768m"
javaOptions += "-Xss128m"
