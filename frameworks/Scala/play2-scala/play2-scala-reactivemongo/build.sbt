name := "play2-scala-reactivemongo"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, PlayNettyServer)

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.reactivemongo" %% "play2-reactivemongo" % "0.16.0-play26",
  "org.reactivemongo" %% "reactivemongo-play-json" % "0.16.0-play26",
  "com.softwaremill.macwire" %% "macros" % "2.3.0",
  "com.softwaremill.macwire" %% "util" % "2.3.0"
)
