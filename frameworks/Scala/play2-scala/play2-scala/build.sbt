name := "play2-scala"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, PlayNettyServer)

scalaVersion := "2.12.5"

libraryDependencies ++= Seq(
  guice,
  "com.typesafe.play" %% "play-json" % "2.6.7"
)