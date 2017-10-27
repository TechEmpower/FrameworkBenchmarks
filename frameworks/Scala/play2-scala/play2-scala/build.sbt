name := "play2-scala"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.3"

val root =
  (project in file(".")).
  enablePlugins(PlayScala, PlayNettyServer).
  disablePlugins(PlayAkkaHttpServer)

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.6"
libraryDependencies += guice
