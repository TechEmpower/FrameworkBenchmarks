name := """techempower-benchmarks-finch"""

version := "0.14.1"

scalaVersion := "2.11.11"

com.github.retronym.SbtOneJar.oneJarSettings

libraryDependencies ++= Seq(
  "com.github.finagle" %% "finch-core" % "0.14.1",
  "com.github.finagle" %% "finch-circe" % "0.14.1"
)
