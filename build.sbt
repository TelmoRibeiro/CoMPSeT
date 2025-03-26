val scala3Version = "3.3.5"
// val scalaCheckVersion = "1.18.1"
val scalaParserCombinatorsVersion = "2.1.0"
val scalaJSVersion = "1.2.0"

val toolName = "CoMPSeT"
val toolVersion = "1.4"

lazy val caos = project.in(file("lib/caos"))
  .enablePlugins(ScalaJSPlugin)
  .settings(scalaVersion := scala3Version)

lazy val root = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := toolName,
    version := toolVersion,
    scalaVersion := scala3Version,
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass  := Some("mpst.frontend.Main"),
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "lib" / "caos" / "tool" / "js" / "gen",
    Compile / fullLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "lib" / "caos" / "tool" / "js" / "gen",
    libraryDependencies ++= Seq(
        "org.scala-lang.modules" %%% "scala-parser-combinators" % scalaParserCombinatorsVersion,
        ("org.scala-js" %%% "scalajs-dom" % scalaJSVersion).cross(CrossVersion.for3Use2_13),
    ),
  ).dependsOn(caos)