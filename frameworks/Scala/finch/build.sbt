name := """techempower-benchmarks-finch"""

version := "0.0.2"

scalaVersion := "2.11.7"

com.github.retronym.SbtOneJar.oneJarSettings

mainClass in (Compile, run) := Some("WebServer")

libraryDependencies ++= Seq(
  "com.github.finagle" %% "finch-core" % "0.10.0",
  "com.github.finagle" %% "finch-circe" % "0.10.0"  ,
  "io.circe" %% "circe-core" % "0.3.0",
  "io.circe" %% "circe-generic" % "0.3.0",
  "io.circe" %% "circe-jawn" % "0.3.0"
)

resolvers += Resolver.sonatypeRepo("snapshots")
