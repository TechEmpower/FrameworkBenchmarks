name := "techempower-benchmarks-finatra"
organization := "com.twitter"
version := "18.3.0"

scalaVersion := "2.12.5"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases")
)

assemblyJarName in assembly := "finatra-benchmark.jar"
assemblyMergeStrategy in assembly := {
  case "BUILD" => MergeStrategy.discard
  case PathList("META-INF", "io.netty.versions.properties") => MergeStrategy.discard
  case other => MergeStrategy.defaultMergeStrategy(other)
}

libraryDependencies ++= Seq(
  "com.twitter" %% "finatra-http" % "18.3.0",
  "org.slf4j" % "slf4j-nop" % "1.7.25",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.8.4",
  "javax.activation" % "activation" % "1.1.1"
)
