name := "finagle"

scalaVersion := "2.11.12"

version := "18.3.0"

com.github.retronym.SbtOneJar.oneJarSettings

libraryDependencies ++= Seq(
  "com.twitter" %% "finagle-http" % "18.3.0",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.8.4"
)
