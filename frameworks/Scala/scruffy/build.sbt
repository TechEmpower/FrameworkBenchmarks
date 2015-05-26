name := "scruffy-benchmark"

organization := "com.sksamuel.scruffy"

scalaVersion := "2.11.6"

version := "11.0" // version 0 for round 11

libraryDependencies ++= Seq(
  "com.sksamuel.scruffy" %% "scruffy-undertow" % "1.13.0",
  "com.sksamuel.scruffy" %% "scruffy-jackson" % "1.13.0",
  "com.sksamuel.scruffy" %% "scruffy-client" % "1.13.0" % "test",
  "org.mongodb" %% "casbah-core" % "2.8.0"
)

sbtassembly.Plugin.assemblySettings
