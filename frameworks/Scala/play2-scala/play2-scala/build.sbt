name := "play2-scala"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.4"

val root =
  (project in file(".")).
  enablePlugins(PlayScala)

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.6"
libraryDependencies += guice
