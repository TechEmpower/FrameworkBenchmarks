name := """techempower-benchmarks-fintrospect"""

version := "0.0.1"

scalaVersion := "2.11.7"

resolvers += "JCenter" at "https://jcenter.bintray.com"

com.github.retronym.SbtOneJar.oneJarSettings

mainClass in(Compile, run) := Some("FintrospectBenchmarkServer")

libraryDependencies ++= Seq(
  "com.twitter" %% "finagle-http" % "6.34.0",
  "io.github.daviddenton" %% "fintrospect" % "12.8.0",
  "io.circe" %% "circe-core" % "0.3.0",
  "io.circe" %% "circe-parser" % "0.3.0",
  "io.circe" %% "circe-generic" % "0.3.0"
)

resolvers += Resolver.sonatypeRepo("snapshots")
