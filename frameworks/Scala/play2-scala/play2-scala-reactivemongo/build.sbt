name := "play2-scala-reactivemongo"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

libraryDependencies ++= Seq(
  "org.reactivemongo" %% "play2-reactivemongo" % "0.11.9",
  "org.reactivemongo" %% "reactivemongo-play-json" % "0.11.9"
)

