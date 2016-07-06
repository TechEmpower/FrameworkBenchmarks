name := """techempower-benchmarks-finch"""

version := "0.0.3"

scalaVersion := "2.11.8"

com.github.retronym.SbtOneJar.oneJarSettings

mainClass in (Compile, run) := Some("WebServer")

libraryDependencies ++= Seq(
  "com.twitter" %% "finagle-http" % "6.34.0",
  "com.github.finagle" %% "finch-core" % "0.10.0",
  "com.github.finagle" %% "finch-circe" % "0.10.0",
  "io.circe" %% "circe-core" % "0.4.1",
  "io.circe" %% "circe-generic" % "0.4.1",
  "io.circe" %% "circe-jawn" % "0.4.1"
)

resolvers += Resolver.sonatypeRepo("snapshots")
