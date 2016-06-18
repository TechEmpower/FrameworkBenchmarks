name := "finagle"

scalaVersion := "2.11.7"

version := "1.0"

libraryDependencies ++= Seq(
  "com.twitter" %% "finagle-http" % "6.33.0",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.5.3"
)
