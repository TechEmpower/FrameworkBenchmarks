name := "scruffy-benchmark"

organization := "com.sksamuel.scruffy"

scalaVersion := "2.11.1"

version := "1.0.1"

libraryDependencies ++= Seq(
  "com.sksamuel.scruffy" %% "scruffy-server" % "1.4.15",
  "org.mongodb" %% "casbah-core" % "2.7.1"
)

sbtassembly.Plugin.assemblySettings
