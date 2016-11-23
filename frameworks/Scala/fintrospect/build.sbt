name := """techempower-benchmarks-fintrospect"""

version := "0.0.7"

scalaVersion := "2.11.8"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

resolvers += "JCenter" at "https://jcenter.bintray.com"

com.github.retronym.SbtOneJar.oneJarSettings

mainClass in(Compile, run) := Some("FintrospectBenchmarkServer")

libraryDependencies ++= Seq(
  "io.fintrospect" %% "fintrospect-core" % "13.13.0",
  "io.fintrospect" %% "fintrospect-json4s" % "13.13.0",
  "io.fintrospect" %% "fintrospect-mustache" % "13.13.0",
  "com.twitter" %% "finagle-mysql" % "6.40.0"
)

resolvers += Resolver.sonatypeRepo("snapshots")