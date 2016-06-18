<<<<<<< HEAD
=======
import spray.revolver.RevolverPlugin

>>>>>>> upstream/master
name := "spray-benchmark"

organization := "io.spray"

<<<<<<< HEAD
scalaVersion := "2.11.8"
=======
scalaVersion := "2.11.7"
>>>>>>> upstream/master

version := "1.0"

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io/"
)

libraryDependencies ++= Seq(
<<<<<<< HEAD
  "io.spray" %% "spray-json" % "1.3.1",
  "io.spray" %% "spray-can" % "1.3.1",
  "com.typesafe.akka" %%  "akka-actor" % "2.4.7",
  "com.typesafe.akka" %%  "akka-slf4j" % "2.4.7",
  "ch.qos.logback"% "logback-classic" % "1.0.12" % "runtime"
)
=======
  "io.spray" %% "spray-json" % "1.3.2",
  "io.spray" % "spray-can" % "1.1-20130619",
  "com.typesafe.akka" %%  "akka-actor" % "2.4.1",
  "com.typesafe.akka" %%  "akka-slf4j" % "2.4.1",
  "ch.qos.logback"% "logback-classic" % "1.0.12" % "runtime",
  "org.scala-lang" % "scala-xml" % "2.11.0-M4"
)

RevolverPlugin.settings

sbtassembly.AssemblyPlugin.assemblySettings
>>>>>>> upstream/master
