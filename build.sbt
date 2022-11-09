ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "Text_statistics"
  )

val argparse = "0.16.2"

libraryDependencies ++= Seq(
  "io.crashbox" %% "argparse" % argparse,
)