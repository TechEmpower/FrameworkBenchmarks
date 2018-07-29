name := "play2-scala"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, PlayNettyServer)

scalaVersion := "2.12.6"

libraryDependencies += guice