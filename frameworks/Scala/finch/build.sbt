name := """techempower-benchmarks-finch"""

version := "0.18.0"

scalaVersion := "2.11.12"

com.github.retronym.SbtOneJar.oneJarSettings

libraryDependencies ++= Seq(
  "com.github.finagle" %% "finch-core" % "0.18.0",
  "com.github.finagle" %% "finch-circe" % "0.18.0"
)
