enablePlugins(JavaAppPackaging)

organization := "org.apache.pekko"

name := "pekko-http-benchmark"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.8"

val pekkoV = "1.0.1"
val pekkoHttpV = "1.0.0"

libraryDependencies ++= Seq(
  "org.apache.pekko" %% "pekko-http" % pekkoHttpV,
  "org.apache.pekko" %% "pekko-stream" % pekkoV,
  "com.github.pjfanning" %% "pekko-http-jsoniter-scala" % "2.1.0",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.6.0",
  "mysql" % "mysql-connector-java" % "8.0.21",
  "com.zaxxer" % "HikariCP" % "3.4.5",
  "org.scalatra.scalate" %% "scalate-core" % "1.9.6",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)

Compile / mainClass := Some("pekko.http.benchmark.Main")
