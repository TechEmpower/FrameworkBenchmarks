name := "play2-scala-slick"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.3"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-slick" % "3.0.2",
  "mysql" % "mysql-connector-java" % "5.1.44",
  filters
)
