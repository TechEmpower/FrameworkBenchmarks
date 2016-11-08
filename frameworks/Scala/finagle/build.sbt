name := "finagle"

scalaVersion := "2.11.8"

version := "1.0.2"

com.github.retronym.SbtOneJar.oneJarSettings

libraryDependencies ++= Seq(
  "com.twitter" %% "finagle-http" % "6.36.0",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.5.3"
)
