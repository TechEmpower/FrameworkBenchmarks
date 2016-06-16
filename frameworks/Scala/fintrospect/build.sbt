name := """techempower-benchmarks-fintrospect"""

version := "0.0.2"

scalaVersion := "2.11.8"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

resolvers += "JCenter" at "https://jcenter.bintray.com"

com.github.retronym.SbtOneJar.oneJarSettings

mainClass in(Compile, run) := Some("FintrospectBenchmarkServer")

libraryDependencies ++= Seq(
  "com.twitter" %% "finagle-http" % "6.35.0",
  "io.github.daviddenton" %% "fintrospect" % "12.20.1",
  "io.circe" %% "circe-core" % "0.4.1",
  "io.circe" %% "circe-parser" % "0.4.1",
  "io.circe" %% "circe-generic" % "0.4.1"
)

resolvers += Resolver.sonatypeRepo("snapshots")
