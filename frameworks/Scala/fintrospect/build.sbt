name := """techempower-benchmarks-fintrospect"""

version := "0.0.3"

scalaVersion := "2.11.8"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

resolvers += "JCenter" at "https://jcenter.bintray.com"

com.github.retronym.SbtOneJar.oneJarSettings

mainClass in(Compile, run) := Some("FintrospectBenchmarkServer")

libraryDependencies ++= Seq(
  "io.fintrospect" %% "fintrospect-core" % "13.0.0",
  "io.fintrospect" %% "fintrospect-circe" % "13.0.0"
)

resolvers += Resolver.sonatypeRepo("snapshots")
