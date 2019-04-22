lazy val commonSettings = Seq(
  version := "0.1-SNAPSHOT",
  organization := "kijima",
  scalaVersion := "2.12.8",
  test in assembly := {}
)

lazy val server = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    mainClass in assembly := Some("Server"),
    assemblyJarName in assembly := "server.jar"
  )

lazy val client = (project in file("Client")).settings(
  mainClass in (Compile, run) := Some("Client")
)
