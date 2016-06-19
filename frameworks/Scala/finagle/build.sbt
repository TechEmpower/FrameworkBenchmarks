name := "finagle"

scalaVersion := "2.11.8"

version := "1.0.1"

libraryDependencies ++= Seq(
  "com.twitter" %% "finagle-http" % "6.34.0",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.5.3"
)
