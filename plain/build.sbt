import sbt._
import spray.revolver.RevolverPlugin.Revolver
import sbtassembly.Plugin._
import AssemblyKeys._

name := "plain-benchmark"

organization := "com.ibm"

scalaVersion := "2.10.3"

version := "1.0.1"

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
  "org.scala-lang" % "scala-reflect" % "2.10.3",
  "org.reflections" % "reflections" % "0.9.8",
  "com.typesafe" % "config" % "1.0.2",
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  "org.codehaus.janino" % "janino" % "2.6.1",
  "com.lmax" % "disruptor" % "3.2.0",
  "com.typesafe.akka" %% "akka-actor" % "2.2.1",
  "com.typesafe.akka" %% "akka-slf4j" % "2.2.1",
  "org.apache.commons" % "commons-lang3" % "3.1",
  "org.apache.commons" % "commons-compress" % "1.5",
  "commons-io" % "commons-io" % "2.4",
  "commons-net" % "commons-net" % "3.3",
  "commons-codec" % "commons-codec" % "1.8",
  "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.3.2",
  "net.jpountz.lz4" % "lz4" % "1.2.0",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.2.3",
  "com.sun.jersey" % "jersey-json" % "1.17.1",
  "org.jvnet.mimepull" % "mimepull" % "1.9.3",
  "org.apache.derby" % "derby" % "10.10.1.1",
  "org.apache.derby" % "derbyclient" % "10.10.1.1",
  "com.h2database" % "h2" % "1.3.172",
  "mysql" % "mysql-connector-java" % "5.1.26",
  "javax.servlet" % "javax.servlet-api" % "3.1.0"
)

Revolver.settings

sbtassembly.Plugin.assemblySettings

scalariformSettings
