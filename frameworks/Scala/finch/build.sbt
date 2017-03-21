name := """techempower-benchmarks-finch"""

version := "0.0.5"

scalaVersion := "2.11.8"

com.github.retronym.SbtOneJar.oneJarSettings

libraryDependencies ++= Seq(
  "com.github.finagle" %% "finch-core" % "0.13.0",
  "com.github.finagle" %% "finch-circe" % "0.13.0"
)
