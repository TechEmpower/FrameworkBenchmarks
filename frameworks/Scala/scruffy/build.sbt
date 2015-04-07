name := "scruffy-benchmark"

organization := "com.sksamuel.scruffy"

scalaVersion := "2.11.6"

version := "10.1" // version 10 for round 10

libraryDependencies ++= Seq(
  "com.sksamuel.scruffy" %% "scruffy-undertow" % "1.9.0",
  "org.mongodb" %% "casbah-core" % "2.7.1"
)

sbtassembly.Plugin.assemblySettings
