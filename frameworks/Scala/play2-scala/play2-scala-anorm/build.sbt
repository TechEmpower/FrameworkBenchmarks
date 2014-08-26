name := "play2-scala-anorm"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.2"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  "mysql" % "mysql-connector-java" % "5.1.32")
