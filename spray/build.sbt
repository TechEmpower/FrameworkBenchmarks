import spray.revolver.RevolverPlugin.Revolver

name := "spray-benchmark"

organization := "io.spray"

scalaVersion := "2.10.3"

version := "1.0.1"

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io/",
  "spray nightly repo" at "http://nightlies.spray.io/"
)

libraryDependencies ++= Seq(
  "io.spray" %% "spray-json" % "1.2.5",
  "io.spray" % "spray-can" % "1.2.0",
  "com.typesafe.akka" %%  "akka-actor" % "2.2.3",
  "com.typesafe.akka" %%  "akka-slf4j" % "2.2.3",
  "ch.qos.logback"% "logback-classic" % "1.0.13" % "runtime"
)

Revolver.settings

sbtassembly.Plugin.assemblySettings
