name := "play2-scala-reactivemongo"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, PlayNettyServer).disablePlugins(PlayFilters)

scalaVersion := "2.13.16"

libraryDependencies ++= Seq(
  "org.reactivemongo" %% "play2-reactivemongo" % "1.1.0-play29.RC15",
  "org.reactivemongo" %% "reactivemongo-play-json-compat" % "1.1.0-play29.RC15",
  "com.softwaremill.macwire" %% "macros" % "2.3.3",
  "com.softwaremill.macwire" %% "util" % "2.3.3"
)
