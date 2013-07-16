import sbt._
import spray.revolver.RevolverPlugin.Revolver
import sbtassembly.Plugin._
import AssemblyKeys._

name := "plain-benchmark"

organization := "com.ibm"

scalaVersion := "2.10.2"

version := "1.0"

test in Compile := {}

mainClass in Compile := Some("com.ibm.plain.bootstrap.Main")

scalacOptions in Compile ++= Seq(
	"-g:none",
	"-encoding", "UTF-8", 
	"-target:jvm-1.7", 
	"-deprecation", 
	"-feature", 
	"-optimise"
)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.10.2",
  "org.reflections" % "reflections" % "0.9.8",
  "com.typesafe" % "config" % "1.0.1",
  "ch.qos.logback" % "logback-classic" % "1.0.12",
  "org.codehaus.janino" % "janino" % "2.6.1",
  "com.lmax" % "disruptor" % "3.1.0",
  "com.typesafe.akka" %% "akka-actor" % "2.1.4",
  "com.typesafe.akka" %% "akka-slf4j" % "2.1.4",
  "org.apache.commons" % "commons-lang3" % "3.1",
  "org.apache.commons" % "commons-compress" % "1.4.1",
  "commons-io" % "commons-io" % "2.4",
  "commons-net" % "commons-net" % "3.2",
  "commons-codec" % "commons-codec" % "1.7",
  "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.3.2",
  "net.jpountz.lz4" % "lz4" % "1.1.2",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.2.2",
  "com.sun.jersey" % "jersey-json" % "1.17.1",
  "org.jvnet.mimepull" % "mimepull" % "1.9.2",
  "javax.servlet" % "servlet-api" % "2.5"
)

Revolver.settings

sbtassembly.Plugin.assemblySettings
