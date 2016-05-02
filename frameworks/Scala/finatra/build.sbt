name := "techempower-benchmarks-finatra"
organization := "com.twitter"
version := "0.0.1"

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("snapshots")

assemblyJarName in assembly := "finatra-benchmark.jar"
assemblyMergeStrategy in assembly := {
  case "BUILD" => MergeStrategy.discard
  case other => MergeStrategy.defaultMergeStrategy(other)
}

libraryDependencies ++= Seq(
  "com.twitter.finatra" %% "finatra-http" % "2.1.6",
  "org.slf4j" % "slf4j-nop" % "1.7.7"
)
