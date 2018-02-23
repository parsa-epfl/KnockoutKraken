ensimeScalaVersion in ThisBuild := "2.11.11"
ensimeLaunchConfigurations := Seq(
  LaunchConfig("server",
               JavaArgs(
                 mainClass = "mypackage.Server",
                 classArgs = Seq("start", "--foreground"),
                 envArgs   = Map("MYSERVER_PORT" -> "8080"),
                 jvmArgs   = Seq("-Xmx2g")
               )
  )
)
