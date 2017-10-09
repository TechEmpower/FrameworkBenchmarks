name := "play2-scala"

version := "2.6.3"

scalaVersion := "2.12.3"

val root =
  (project in file(".")).
  enablePlugins(PlayScala, PlayNettyServer).
  disablePlugins(PlayAkkaHttpServer)

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.3"
libraryDependencies += guice
