name := "play2-java"
version := "1.0-SNAPSHOT"
scalaVersion := "2.12.4"

libraryDependencies += guice

routesGenerator := InjectedRoutesGenerator

lazy val root = (project in file(".")).enablePlugins(PlayJava)

