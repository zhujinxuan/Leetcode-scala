val dottyVersion = "0.24.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test",
  )
