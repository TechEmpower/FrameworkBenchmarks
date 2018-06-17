name := "play2-java"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava, PlayNettyServer)

scalaVersion := "2.12.6"

libraryDependencies += guice
