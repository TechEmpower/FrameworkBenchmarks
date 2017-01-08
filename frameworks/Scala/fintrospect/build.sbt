name := """techempower-benchmarks-fintrospect"""

version := "0.2.0"

scalaVersion := "2.11.8"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

resolvers += "JCenter" at "https://jcenter.bintray.com"

resolvers += Resolver.sonatypeRepo("snapshots")

com.github.retronym.SbtOneJar.oneJarSettings

mainClass in(Compile, run) := Some("FintrospectBenchmarkServer")

libraryDependencies ++= Seq(
  "io.fintrospect" %% "fintrospect-core" % "14.1.0",
  "io.fintrospect" %% "fintrospect-jackson" % "14.1.0",
  "io.fintrospect" %% "fintrospect-mustache" % "14.1.0",
  "com.twitter" %% "finagle-mysql" % "6.41.0"
)
