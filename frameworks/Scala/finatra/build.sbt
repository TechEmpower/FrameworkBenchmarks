lazy val finatraVersion = "20.1.0"

name := "techempower-benchmarks-finatra"
organization := "com.twitter"
version := finatraVersion

scalaVersion := "2.12.8"

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
  ("com.twitter" %% "finatra-http" % finatraVersion).
    exclude("com.sun.activation", "javax.activation"),
  "org.slf4j" % "slf4j-nop" % "1.7.25",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.10",
)

excludeDependencies ++= Seq(
  // commons-logging is replaced by jcl-over-slf4j
  ExclusionRule("org.slf4j", "slf4j-simple"),
  ExclusionRule("commons-logging", "commons-logging")
)
