val scala3Version = "3.9.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "AoC2016",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
  )
