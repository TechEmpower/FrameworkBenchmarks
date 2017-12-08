name := "finagle"

scalaVersion := "2.11.12"

version := "17.11.0"

com.github.retronym.SbtOneJar.oneJarSettings

libraryDependencies ++= Seq(
  "com.twitter" %% "finagle-http" % "17.11.0",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.8.4"
)
