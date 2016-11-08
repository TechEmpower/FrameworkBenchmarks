name := """techempower-benchmarks-finch"""

version := "0.0.4"

scalaVersion := "2.11.8"

com.github.retronym.SbtOneJar.oneJarSettings

libraryDependencies ++= Seq(
  "com.twitter" %% "finagle-http" % "6.36.0",
  "com.github.finagle" %% "finch-core" % "0.11.0-M2",
  "com.github.finagle" %% "finch-circe" % "0.11.0-M2",
  "io.circe" %% "circe-core" % "0.5.0-M2",
  "io.circe" %% "circe-generic" % "0.5.0-M2",
  "io.circe" %% "circe-jawn" % "0.5.0-M2"
)

resolvers += Resolver.sonatypeRepo("snapshots")
