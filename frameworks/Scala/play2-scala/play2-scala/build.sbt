name := "play2-scala"

version := "2.6.0"

scalaVersion := "2.11.11"

val root = (project in file(".")).enablePlugins(PlayScala)

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.0"
libraryDependencies += guice

