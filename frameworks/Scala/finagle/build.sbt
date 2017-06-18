name := "finagle"

scalaVersion := "2.11.11"

version := "6.45.0"

com.github.retronym.SbtOneJar.oneJarSettings

libraryDependencies ++= Seq(
  "com.twitter" %% "finagle-http" % "6.45.0",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.5.3"
)
