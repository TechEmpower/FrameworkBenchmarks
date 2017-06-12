name := """techempower-benchmarks-finch"""

version := "0.15.0"

scalaVersion := "2.11.11"

com.github.retronym.SbtOneJar.oneJarSettings

libraryDependencies ++= Seq(
  "com.github.finagle" %% "finch-core" % "0.15.0",
  "com.github.finagle" %% "finch-circe" % "0.15.0"
)
