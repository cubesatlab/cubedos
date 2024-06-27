
enablePlugins(Antlr4Plugin)

ThisBuild / organization  := "edu.vermontstate"
ThisBuild / version       := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion  := "2.13.12"
ThisBuild / scalacOptions :=
  Seq("-encoding", "UTF-8", // Encoding of the source files.
      "-feature",
      "-deprecation",    // Tell us about deprecated things.
      "-unchecked",
      "-Wunused:nowarn", // Warn if the nowarn annotation doesn't actually suppress a warning.
      "-Xsource:3",      // Help migrate to Scala 3 by forbidding some things and allowing others.
      "-Ywarn-dead-code",
      "-Ywarn-value-discard")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.17"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test"

Test / logBuffered := false

lazy val merc = (project in file("."))
  .settings(
    name := "Merc",

    Antlr4 / antlr4Version     := "4.13.1",
    Antlr4 / antlr4PackageName := Some("edu.vermontstate.merc"),
    Antlr4 / antlr4GenListener := true,
    Antlr4 / antlr4GenVisitor  := true
  )
