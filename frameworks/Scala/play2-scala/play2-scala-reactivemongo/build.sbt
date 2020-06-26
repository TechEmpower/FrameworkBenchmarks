name := "play2-scala-reactivemongo"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, PlayNettyServer).disablePlugins(PlayFilters)

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.reactivemongo" %% "play2-reactivemongo" % "0.20.3-play28",
  "org.reactivemongo" %% "reactivemongo-play-json" % "0.20.3-play28",
  "com.softwaremill.macwire" %% "macros" % "2.3.3",
  "com.softwaremill.macwire" %% "util" % "2.3.3"
)
