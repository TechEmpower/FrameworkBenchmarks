name := "scruffy-benchmark"

organization := "com.sksamuel.scruffy"

scalaVersion := "2.11.0"

version := "1.0.1"

libraryDependencies ++= Seq(
    "com.sksamuel.scruffy" %% "scruffy-server" % "1.4.14",
    "org.mongodb" %% "casbah-core" % "2.7.1"
)

sbtassembly.Plugin.assemblySettings
