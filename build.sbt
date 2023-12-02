import sbt.Def

organization := "guycastle"
name         := "aoc2023"
scalaVersion := "3.3.1"

lazy val aoc = (project in file("."))
  .settings(settings)
  .settings(libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % "3.2.17",
    "org.scalatest" %% "scalatest" % "3.2.17" % "test"
  ))

lazy val settings: Seq[Setting[?]] = Seq(
  // Dynamic versioning
  ThisBuild / dynverVTagPrefix := false,
  ThisBuild / dynverSeparator := "-",
  // Scalafix
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
)
