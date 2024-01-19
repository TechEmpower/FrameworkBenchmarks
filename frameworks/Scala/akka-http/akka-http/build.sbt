enablePlugins(JavaAppPackaging)

organization := "com.typesafe.akka"

name := "akka-http-benchmark"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.8"

val akkaV = "2.6.19"
val akkaHttpV = "10.2.9"

// to get latest versions
resolvers += "akka-http-snapshot-repository" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % akkaHttpV,
  "com.typesafe.akka" %% "akka-stream" % akkaV,
  "de.heikoseeberger" %% "akka-http-jsoniter-scala" % "1.34.0",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.6.0",
  "mysql" % "mysql-connector-java" % "8.0.21",
  "com.zaxxer" % "HikariCP" % "3.4.5",
  "org.scalatra.scalate" %% "scalate-core" % "1.9.6",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)

mainClass in Compile := Some("com.typesafe.akka.http.benchmark.Main")
