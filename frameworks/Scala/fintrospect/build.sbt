name := """techempower-benchmarks-fintrospect"""

version := "0.0.9"

scalaVersion := "2.11.8"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

resolvers += "JCenter" at "https://jcenter.bintray.com"

com.github.retronym.SbtOneJar.oneJarSettings

mainClass in(Compile, run) := Some("FintrospectBenchmarkServer")

libraryDependencies ++= Seq(
  "io.fintrospect" %% "fintrospect-core" % "14.0.0",
  "io.fintrospect" %% "fintrospect-jackson" % "14.0.0",
  "io.fintrospect" %% "fintrospect-mustache" % "14.0.0",
  "com.twitter" %% "finagle-mysql" % "6.40.0"
)

resolvers += Resolver.sonatypeRepo("snapshots")