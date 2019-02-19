name := "play2-java"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayMinimalJava, PlayNettyServer)

scalaVersion := "2.12.8"

libraryDependencies += guice
