lazy val commonSettings = Seq(
  version := "0.1-SNAPSHOT",
  organization := "kijima",
  scalaVersion := "2.12.8",
  test in assembly := {}
)

lazy val app = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    mainClass in assembly := Some("Server"),
    assemblyJarName in assembly := "server.jar"
    // more settings here ...
  )

/* 

  lazy val client = (project in file(".")).
  settings(
    mainClass in(Compile, run) := Some("Client")
  )
  
 */

