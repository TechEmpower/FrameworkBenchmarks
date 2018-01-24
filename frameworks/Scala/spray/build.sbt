name := "spray-benchmark"

organization := "io.spray"

scalaVersion := "2.11.12"

version := "1.0"

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io/"
)

libraryDependencies ++= Seq(
  "io.spray" %% "spray-json" % "1.3.1",
  "io.spray" %% "spray-can" % "1.3.1",
  "com.typesafe.akka" %%  "akka-actor" % "2.4.7",
  "com.typesafe.akka" %%  "akka-slf4j" % "2.4.7",
  "ch.qos.logback"% "logback-classic" % "1.0.12" % "runtime"
)
