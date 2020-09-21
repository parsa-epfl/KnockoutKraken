// See README.md for license details.  
def scalacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // If we're building with Scala > 2.11, enable the compile option
    //  switch to support our anonymous Bundle definitions:
    //  https://github.com/scala/bug/issues/10047
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 => Seq()
      case _ => Seq("-Xsource:2.11")
    }
  }
}

def javacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // Scala 2.12 requires Java 8. We continue to generate
    //  Java 7 compatible code for Scala 2.11
    //  for compatibility with old clients.
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 =>
        Seq("-source", "1.7", "-target", "1.7")
      case _ =>
        Seq("-source", "1.8", "-target", "1.8")
    }
  }
}

organization := "parsa.epfl.ch"

name := "protoflex"

version := "0.1"

scalaVersion := "2.12.10"

crossScalaVersions := Seq("2.11.12", "2.12.10")

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.2-SNAPSHOT"
libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.3-SNAPSHOT"
libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.6"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:reflectiveCalls")
scalacOptions ++= scalacOptionsVersion(scalaVersion.value)
javacOptions ++= javacOptionsVersion(scalaVersion.value)

// Recommendations from http://www.scalatest.org/user_guide/using_scalatest_with_sbt
logBuffered in Test := false


val json4sNative = "org.json4s" %% "json4s-native" % "{latestVersion}"

Test / fork := true
javaOptions += "-Xmx32768m"
javaOptions += "-Xss128m"
