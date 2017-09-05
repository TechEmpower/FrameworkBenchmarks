name := "finagle"

scalaVersion := "2.11.11"

version := "7.0.0"

com.github.retronym.SbtOneJar.oneJarSettings

libraryDependencies ++= Seq(
  "com.twitter" %% "finagle-http" % "7.0.0",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.8.4"
)
