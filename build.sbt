//ThisBuild / scalaVersion := "3.4.1"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val caos = project.in(file("lib/caos"))
  .enablePlugins(ScalaJSPlugin)
  .settings(scalaVersion := "3.1.1")

lazy val root = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "codeMPST",
    scalaVersion := "3.1.1",
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass  := Some("mpst.frontend.Main"),
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "lib" / "caos" / "tool" / "js" / "gen",
    Compile / fullLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "lib" / "caos" / "tool" / "js" / "gen",
    libraryDependencies ++= Seq(
        "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.0",
        ("org.scala-js" %%% "scalajs-dom" % "1.2.0").cross(CrossVersion.for3Use2_13),
    ),
  ).dependsOn(caos)