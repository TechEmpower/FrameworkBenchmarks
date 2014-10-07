name := "play2-scala-slick"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.2"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-slick" % "0.8.0",
  "mysql" % "mysql-connector-java" % "5.1.32")
