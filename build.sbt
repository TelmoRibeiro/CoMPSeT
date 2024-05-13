ThisBuild / scalaVersion := "3.4.1"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "ConsoleMPST",
    Compile / mainClass  := Some("src.main.scala.mpst.frontend.Main"),
    // Compile / fastLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "lib" / "caos" / "tool" / "js" / "gen",
    libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.3.0",
    libraryDependencies += "org.scala-js"           %%% "scalajs-dom"              % "2.4.0",
    scalaJSUseMainModuleInitializer := true,
  ).enablePlugins(ScalaJSPlugin)