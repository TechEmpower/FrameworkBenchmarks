name := "play2-scala-reactivemongo"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.3"

lazy val root =
  (project in file(".")).
  settings(routesGenerator := InjectedRoutesGenerator).
  enablePlugins(PlayScala)

libraryDependencies ++= Seq(
  "org.reactivemongo" %% "play2-reactivemongo" % "0.12.7-play26",
  "org.reactivemongo" %% "reactivemongo-play-json" % "0.12.7-play26",
  "com.softwaremill.macwire" %% "macros" % "2.3.0",
  "com.softwaremill.macwire" %% "util" % "2.3.0"
)
