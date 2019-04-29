name := "play2-scala-reactivemongo"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, PlayNettyServer).disablePlugins(PlayFilters)

scalaVersion := "2.12.8"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.reactivemongo" %% "play2-reactivemongo" % "0.17.0-play27-SNAPSHOT",
  "org.reactivemongo" %% "reactivemongo-play-json" % "0.17.0-play27-SNAPSHOT",
  "com.softwaremill.macwire" %% "macros" % "2.3.0",
  "com.softwaremill.macwire" %% "util" % "2.3.0"
)
